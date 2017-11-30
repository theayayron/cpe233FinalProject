;---------------------------------------------------------------------
; Conway's Game of Life 
;
; Author: Caitlin Settles and Aaron Kawahara
;---------------------------------------------------------------------

.CSEG
.ORG 0x10

.EQU VGA_HADD  = 0x90
.EQU VGA_LADD  = 0x91
.EQU VGA_COLOR = 0x92

.EQU VGA_READ  = 0x93

.EQU SSEG      = 0x81
.EQU LEDS      = 0x40

.EQU BG_COLOR  = 0x00             ; Background:  black

;---------------------------------------------------------------------

.DEF CUR_COLOR = r0               ; pixel color read from ram
.DEF COLOR     = r6               ; color of pixel to write to address

.DEF ROW       = r7               ; y coordinate
.DEF COL       = r8               ; x coordinate
.DEF TEMP_ROW  = r4
.DEF TEMP_COL  = r5

; Two rows must be recorded at a time until the third row is reached, 
; so that changes made to past pixels do not mess with the behavior
; of future pixels

; These 5 8-bit registers combine to represent 40 pixels in a row,
; with each pixel representing a single bit. The LSB represents the 
; left-most pixel, and the MSB represents the right-most pixel.
.DEF ROWA_08    = r21       
.DEF ROWA_16    = r22
.DEF ROWA_24    = r23
.DEF ROWA_32    = r24
.DEF ROWA_40    = r25
    
.DEF ROWB_08    = r26
.DEF ROWB_16    = r27
.DEF ROWB_24    = r28
.DEF ROWB_32    = r29
.DEF ROWB_40    = r30
    
.DEF CURR_PIX   = r31            ; loop counter for row manipulations
.DEF TEMP_Y     = r20

.DEF BIT_MASK   = r11
.DEF NUM_NEIGH  = r12

.DEF TOP_ROW    = r13
.DEF CUR_ROW    = r14

.DEF SET_PIX    = r15

;r6 is used for color
;r7 is used for Y
;r8 is used for X
;r0 is used for pixelValue

;---------------------------------------------------------------------
init:   SEI

        MOV    ROWA_08, 0x00     
        MOV    ROWA_16, 0x00  
        MOV    ROWA_24, 0x00  
        MOV    ROWA_32, 0x00  
        MOV    ROWA_40, 0x00  
            
        MOV    ROWB_08, 0x00  
        MOV    ROWB_16, 0x00  
        MOV    ROWB_24, 0x00  
        MOV    ROWB_32, 0x00  
        MOV    ROWB_40, 0x00  

        CALL   draw_background
start_loop:
        MOV    ROW, 0x00
        MOV    COL, 0x00
loop_row:
        MOV    ROW, 0x00                ; restart x coordinates
loop_col:
        CALL   start_game
        ADD    COL, 0x01
        CMP    COL, 0x28               ; check column is still under 40 (0x28)
        BRNE   loop_col                ; if it is under 40, keep looping

        CALL   transfer_BtoA

        CMP    ROW, 0x01
        BRCS   loop_cont               ; if we are on the first row, don't save row A to RAM
        CALL   out_row                 ; save row A to RAM
loop_cont:
        ADD    ROW, 0x01
        CMP    ROW, 0x1E               ; check row is still under 30 (0x1E)
        BRNE   loop_row                ; branch to draw more rows
                                       ; we have looped throught the entire monitor
        CALL   transfer_BtoA           ; we still have to write the bottom row to RAM
        CALL   out_row

        CALL   delay                   ; wait to start the next loop
        BRN    start_loop              ; game plays indefinitely

;---------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: start_game
;-
;-  Implements Conway's Game of Life in our RAT processor.
;- 
;- Tweaked registers: TEMP_ROW, TEMP_COL
;--------------------------------------------------------------------

start_game:
        MOV     TEMP_ROW, ROW           ; these two lines are necessary bc read_pixel only reads pixel value from these coordinates
        MOV     TEMP_COL, COL
        CALL    read_pixel              ; get current pixel color

        CALL    calc_neighbors

        AND     CUR_COLOR, 0xFF         ; if white, r0 will be 0xFF, otherwise it will be 0x00
        BREQ    pixel_is_dead           ; pixel was black ("dead")
        BRN     pixel_is_alive           ; pixel was white ("live")
pixel_is_dead:
        CMP     NUM_NEIGH, 0x03          ; if a dead pixel has 3 neighbors, it will come to life
        BREQ    create_pixel
        BRN     kill_pixel
pixel_is_alive:
        CMP     NUM_NEIGH, 0x02         ; if a live pixel has less than 2 neighbors, it dies
        BRCS    kill_pixel
        CMP     NUM_NEIGH, 0x04         ; if a live pixel has more than 3 neighbors, it dies
        BRCC    kill_pixel
        BRN     create_pixel
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: draw_white_or_black
;-
;-  Helper subroutine for out_row. Writes a specific pixel to the RAM.
;-  (X, Y) = r8 (ROW), r7 (COL)
;- 
;- Tweaked registers: TEMP_ROW, TEMP_COL
;--------------------------------------------------------------------


draw_white_or_black:
        BRCS   draw_white
        BRCC   draw_black
draw_white:
        MOV    COLOR, 0xFF
        CALL   draw_dot
        RET
draw_black:
        MOV    COLOR, 0x00
        CALL   draw_dot
        RET
;--------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: kill_pixel
;- 
;- Clears the COLth "pixel" in a row while leaving all other bits
;- unchanged.
;- 
;- Tweaked registers: SET_PIX, BIT_MASK, CURR_PIX
;---------------------------------------------------------------------

kill_pixel:
        MOV     SET_PIX, 0x00
        CALl    setup
        SEC
        LSL     BIT_MASK              ; shift left
        SUB     CURR_PIX, 0x01
        BRNE    kill_pixel
        EXOR    BIT_MASK, 0xFF        ; invert the mask
        CALL    apply_mask
        RET
;--------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: create_pixel
;- 
;- Sets the COLth "pixel" in a row while leaving all other bits
;- unchanged.
;- 
;- Tweaked registers: SET_PIX, BIT_MASK, CURR_PIX
;---------------------------------------------------------------------
create_pixel:
        MOV     SET_PIX, 0x01
        CALl    setup
        SEC
        LSL     BIT_MASK              ; shift left
        SUB     CURR_PIX, 0x01
        BRNE    create_pixel
        CALL    apply_mask
        RET
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: setup
;-
;-  Normalizes the loop counter for a specific register. Say we have
;-  COL = 0x23. This is the fourth bit of the ROWB_40 register. There-
;-  fore, we want our counter to shift over a bit 3 times from
;-  its initial position at the LSB in order to mask the fourth bit.
;-  We must subtract 32, which is the "starting" position of ROWB_40,
;-  from 35 (our 0x23) in order to get the proper loop counter value.
;- 
;- Tweaked registers: BIT_MASK, CURR_PIX
;--------------------------------------------------------------------

setup:
        MOV     BIT_MASK, 0x00
        CMP     COL, 0x08
        BRCS    setup_08
        CMP     COL, 0x10
        BRCS    setup_16
        CMP     COL, 0x18
        BRCS    setup_24
        CMP     COL, 0x20
        BRCS    setup_32
        BRN     setup_40

setup_08:
        MOV     CURR_PIX, COL
        RET
setup_16:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x08
        RET
setup_24:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x10
        RET
setup_32:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x18
        RET
setup_40:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x20
        RET
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: apply_mask
;-
;-  Applies the mask to either clear or set a bit in the appropriate
;-  register in ROW B
;- 
;- Tweaked registers: ROWB_08, ROWB_16, ROWB_24, ROWB_32, ROWB_40
;--------------------------------------------------------------------

apply_mask:
        CMP     COL, 0x08
        BRCS    apply_mask_08
        CMP     COL, 0x10
        BRCS    apply_mask_16
        CMP     COL, 0x18
        BRCS    apply_mask_24
        CMP     COL, 0x20
        BRCS    apply_mask_32
        BRN     apply_mask_40

apply_mask_08:
        CMP     SET_PIX, 0x01
        BREQ    set_08
        AND     ROWB_08, BIT_MASK     ; clear the COLth bit and leave all others unchanged
        RET
set_08:
        OR      ROWB_08, BIT_MASK     ; set the COLth bit and leave all others unchanged
        RET
apply_mask_16:
        CMP     SET_PIX, 0x01
        BREQ    set_16
        AND     ROWB_16, BIT_MASK
        RET
set_16:
        OR      ROWB_16, BIT_MASK
        RET
apply_mask_24:
        CMP     SET_PIX, 0x01
        BREQ    set_24
        AND     ROWB_24, BIT_MASK
        RET
set_24:
        OR      ROWB_24, BIT_MASK
        RET
apply_mask_32:
        CMP     SET_PIX, 0x01
        BREQ    set_32
        AND     ROWB_32, BIT_MASK
        RET
set_32:
        OR      ROWB_32, BIT_MASK
        RET
apply_mask_40:
        CMP     SET_PIX, 0x01
        BREQ    set_40
        AND     ROWB_40, BIT_MASK
        RET
set_40:
        OR      ROWB_40, BIT_MASK
        RET

;--------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: out_row
;- 
;- This subroutine writes out the new pixel colors for rowA, 
;- then transfer the data in rowB to rowA and clear rowB
;- 
;- Tweaked registers: ROWA & ROWB (r21-r30)
;---------------------------------------------------------------------
out_row:
        MOV     TEMP_Y, ROW          ; save y coordinate before going back
        SUB     ROW, 0x02            ; go back two rows
        MOV     CURR_PIX, 0x08       ; a register can only hold 8 bits
        CALL    out_ROWA_08
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_16
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_24
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_32
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_40
        MOV     ROW, TEMP_Y          ; change r8 to value before subroutine
        MOV     COL, 0x00            ; reset x-coordinate
        RET
out_ROWA_08:
        CLC
        LSR     ROWA_08
        CALL    draw_white_or_black

        ADD     COL, 0x01            ; increment x-coordinate
        SUB     CURR_PIX, 0x01
        BRNE    out_ROWA_08
        RET
out_ROWA_16:
        CLC
        LSR     ROWA_16
        CALL    draw_white_or_black

        ADD     COL, 0x01            ; increment x-coordinate
        SUB     CURR_PIX, 0x01       ; decrement register counter
        BRNE    out_ROWA_16
        RET
out_ROWA_24:
        CLC
        LSR     ROWA_24
        CALL    draw_white_or_black

        ADD     COL, 0x01            ; increment x-coordinate
        SUB     CURR_PIX, 0x01
        BRNE    out_ROWA_24
        RET
out_ROWA_32:
        CLC
        LSR     ROWA_32
        CALL    draw_white_or_black

        ADD     COL, 0x01            ; increment x-coordinate
        SUB     CURR_PIX, 0x01
        BRNE    out_ROWA_32
        RET
out_ROWA_40:
        CLC
        LSR     ROWA_40
        CALL    draw_white_or_black

        ADD     COL, 0x01            ; increment x-coordinate
        SUB     CURR_PIX, 0x01
        BRNE    out_ROWA_40
        RET
;---------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: transfer_BtoA
;- 
;- When calculating whether a pixel should live or die for the next
;- iteration, we store the row above the current and the current
;- calculated row in the ROW registers so that we do not make
;- calculations off of changed data in RAM. Once the current row is
;- finished being calculated, we write the top row to RAM and shift
;- ROW B to ROW A, and then store the next row in ROW A and resume
;- calculations.
;- 
;- Tweaked registers: ROWA & ROWB (r21-r30)
;---------------------------------------------------------------------
        
transfer_BtoA:
        MOV     ROWA_08, ROWB_08
        MOV     ROWA_16, ROWB_16
        MOV     ROWA_24, ROWB_24
        MOV     ROWA_32, ROWB_32
        MOV     ROWA_40, ROWB_40
        RET
;---------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: calc_neighbors
;- 
;- Does the heavy lifting for the game. Calculates the number of
;- living neighbors a pixel has, signified by a white (0xFF)
;- pixel color. This only looks at the current pixel values in RAM.
;- 
;- Tweaked registers: r4 (TEMP_ROW), r5 (TEMP_COL), r11 (BIT_MASK),
;-                    r12 (NUM_NEIGH)
;---------------------------------------------------------------------

calc_neighbors:
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL
        MOV     BIT_MASK, 0x00        ; say we are trying to mask the third bit (r11 = 0x02)
create_mask_above:
        SUB     TEMP_ROW, 0x01
        BRCS    create_mask_right     ; if overflowed, top row is out of bounds
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_ald
        ADD     NUM_NEIGH, 0x01 
create_mask_ald:                      ; above left diagonal neighbor
        SUB     TEMP_COL, 0x01
        BRCS    create_mask_ard       ; bounds check
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_ard
        ADD     NUM_NEIGH, 0x01
create_mask_ard:                      ; above right diagonal neighbor
        ADD     TEMP_COL, 0x02
        CMP     TEMP_COL, 0x28        ; check if it's gone over 40
        BRCS    create_mask_left
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_right
        ADD     NUM_NEIGH, 0x01
create_mask_right:
        MOV     TEMP_ROW, ROW
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_left
        ADD     NUM_NEIGH, 0x01
create_mask_left:
        SUB     TEMP_COL, 0x02
        BRCS    create_mask_bottom
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_bottom
        ADD     NUM_NEIGH, 0x01
create_mask_bottom:
        MOV     TEMP_COL, COL
        ADD     TEMP_ROW, 0x01
        CMP     TEMP_ROW, 0x1E
        BRCS    end
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_bld
        ADD     NUM_NEIGH, 0x01
create_mask_bld:
        SUB    TEMP_COL, 0x01
        BRCS   create_mask_brd
        CALL   read_pixel
        CMP    CUR_COLOR, 0xFF
        BRNE   create_mask_brd
        ADD    NUM_NEIGH, 0x01
create_mask_brd:
        ADD    TEMP_COL, 0x02
        CMP    TEMP_COL, 0x28
        BRCS   end
        CALL   read_pixel
        CMP    CUR_COLOR, 0xFF
        BRNE   end
        ADD    NUM_NEIGH, 0x01 
end:
        RET
;---------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: delay
;-
;-  Delays the CPU by a set amount of clock cycles.
;- 
;- Tweaked registers: r1,r2,r3
;--------------------------------------------------------------------

delay:
        MOV r1, 0xFF
d1:
        MOV r2, 0xFF
d2:
        MOV r3, 0xFF
d3:
        SUB r3, 0x01
        BRNE d3
        SUB r2, 0x01
        BRNE d2
        SUB r1, 0x01
        BRNE d1
        RET

;--------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: draw_horizontal_line
;-  Author: SHANE KENT
;-
;-  Draws a horizontal line from (r8,r7) to (r9,r7) using color in r6
;-
;-  Parameters:
;-   r8  = starting x-coordinate
;-   r7  = y-coordinate
;-   r9  = ending x-coordinate
;-   r6  = color used for line
;- 
;- Tweaked registers: r8,r9
;--------------------------------------------------------------------
draw_horizontal_line:
        ADD    r9, 0x01          ; go from r8 to r15 inclusive

draw_horiz1:
        CALL   draw_dot
        ADD    COL, 0x01
        CMP    COL, r9
        BRNE   draw_horiz1
        RET
;--------------------------------------------------------------------

;---------------------------------------------------------------------
;-  Subroutine: draw_background
;-  Author: SHANE KENT
;-
;-  Fills the 30x40 grid with one color using successive calls to 
;-  draw_horizontal_line subroutine. 
;- 
;-  Tweaked registers: r13,r7,r8,r9
;----------------------------------------------------------------------
draw_background: 
        MOV   COLOR, BG_COLOR              ; use default color
        MOV   r13, 0x00                 ; r13 keeps track of rows
start:  MOV   ROW, r13                 ; load current row count 
        MOV   COL, 0x00                  ; restart x coordinates
        MOV   r9, 0x27 
 
        CALL  draw_horizontal_line
        ADD   r13, 0x01                 ; increment row count
        CMP   r13, 0x1E                 ; see if more rows to draw
        BRNE  start                    ; branch to draw more rows
        RET
;---------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: draw_dot
;- Author: SHANE KENT
;- 
;- This subroutine draws a dot on the display the given coordinates: 
;- 
;- (X,Y) = (r5,r4)  with a color stored in r6  
;- 
;- Tweaked registers: r4,r5
;---------------------------------------------------------------------
draw_dot: 
        MOV   r4, ROW       ; copy Y coordinate
        MOV   r5, COL       ; copy X coordinate

        AND   r5,0x3F        ; make sure top 2 bits cleared
        AND   r4,0x1F        ; make sure top 3 bits cleared
        LSR   r4             ; need to get the bot 2 bits of r4 into sA
        BRCS  dd_add40
t1:     LSR   r4
        BRCS  dd_add80

dd_out: OUT   r5, VGA_LADD   ; write bot 8 address bits to register
        OUT   r4, VGA_HADD   ; write top 3 address bits to register
        OUT   r6, VGA_COLOR  ; write data to frame buffer
        RET

dd_add40:
        OR    r5, 0x40       ; set bit if needed
        CLC                  ; freshen bit
        BRN   t1             

dd_add80: 
        OR    r5, 0x80       ; set bit if needed
        BRN   dd_out
; --------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: read_pixel
;- 
;- This subroutine will read a current pixel color from the 2k RAM: 
;- 
;- (X,Y) = (r8,r7)
;- 
;- Tweaked registers: r4,r5
;---------------------------------------------------------------------
read_pixel: 
        ;MOV   r4, ROW             ; copy Y coordinate
        ;MOV   r5, COL             ; copy X coordinate

        AND   r5, 0x3F       ; make sure top 2 bits cleared
        AND   r4, 0x1F       ; make sure top 3 bits cleared
        LSR   r4             ; need to get the bot 2 bits of r4 into sA
        BRCS  ll_add40      
                            
l1:     LSR   r4            
        BRCS  ll_add80

ll_out: OUT   r5, VGA_LADD   ; write bot 8 address bits to register
        OUT   r4, VGA_HADD   ; write top 3 address bits to register
        IN    r6, VGA_READ
        RET

ll_add40:
        OR    r5, 0x40       ; set bit if needed
        CLC                        ; freshen bit
        BRN   l1             

ll_add80:
        OR    r5, 0x80        ; set bit if needed
        BRN   ll_out
; --------------------------------------------------------------------