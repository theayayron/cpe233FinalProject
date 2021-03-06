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
.EQU PAUSE_PLAY= 0x50             ; port for pause/play button

.EQU BG_COLOR  = 0x00             ; Background:  black

;---------------------------------------------------------------------

.DEF CUR_COLOR = r0               ; pixel color read from RAM
.DEF COLOR     = r6               ; color of pixel to write to address

.DEF ROW       = r7               ; y coordinate
.DEF COL       = r8               ; x coordinate
.DEF TEMP_ROW  = r4               ; temporary y coordinate
.DEF TEMP_COL  = r5               ; temporary x coordinate

; Two rows must be recorded at a time until the third row is reached, 
; so that changes made to past pixels do not mess with the behavior
; of future pixels

; These 5 8-bit registers combine to represent 40 pixels (one row of the VGA),
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

.DEF SET_PIX    = r15            ; stores whether pixel should be killed or created

.DEF START_STOP = r10            ; 0x00 for start, 0xFF for pausing

;---------------------------------------------------------------------
init:   
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

        ; begin drawing exploder pattern

        MOV     COLOR, 0xFF

        MOV     ROW, 0x0A
        MOV     r9, 0x0C
        MOV     COL, 0x12
        CALL    draw_vertical_line

        MOV     ROW, 0x08
        MOV     COL, 0x0F
        MOV     r9, 0x11
        CALL    draw_horizontal_line

        MOV     COL, 0x0D
        MOV     ROW, 0x0A
        MOV     r9, 0x0C
        CALL   draw_vertical_line

        MOV    ROW, 0x0D
        MOV    COL, 0x0F
        MOV    r9, 0x11
        CALL   draw_horizontal_line

        ;------------------

        MOV     ROW, 0x0A
        MOV     r9, 0x0C
        MOV     COL, 0x14
        CALL    draw_vertical_line

        MOV     ROW, 0x08
        MOV     COL, 0x15
        MOV     r9, 0x17
        CALL    draw_horizontal_line

        MOV     COL, 0x19
        MOV     ROW, 0x0A
        MOV     r9, 0x0C
        CALL   draw_vertical_line

        MOV    ROW, 0x0D
        MOV    COL, 0x15
        MOV    r9, 0x17
        CALL   draw_horizontal_line

        ;--------------------

        MOV     ROW, 0x0F
        MOV     COL, 0x0F
        MOV     r9, 0x11
        CALL    draw_horizontal_line

        MOV     ROW, 0x10
        MOV     r9, 0x12
        MOV     COL, 0x12
        CALL    draw_vertical_line

        MOV     ROW, 0x14
        MOV     COL, 0x0F
        MOV     r9, 0x11
        CALL    draw_horizontal_line

        MOV     COL, 0x0D
        MOV     ROW, 0x10
        MOV     r9, 0x12
        CALL    draw_vertical_line

        ;--------------------

        MOV     ROW, 0x0F
        MOV     COL, 0x15
        MOV     r9, 0x17
        CALL    draw_horizontal_line

        MOV     ROW, 0x10
        MOV     r9, 0x12
        MOV     COL, 0x14
        CALL    draw_vertical_line

        MOV     ROW, 0x14
        MOV     COL, 0x15
        MOV     r9, 0x17
        CALL    draw_horizontal_line

        MOV     COL, 0x19
        MOV     ROW, 0x10
        MOV     r9, 0x12
        CALL    draw_vertical_line

        ; end drawing exploder pattern

start_loop:
        MOV    ROW, 0x00
loop_row:
        MOV    COL, 0x00               ; restart x coordinates
loop_col:
        CALL   start_game
        ADD    COL, 0x01
        CMP    COL, 0x28               ; check column is still under 40 (0x28)
        BRNE   loop_col                ; if it is under 40, keep looping

        CMP    ROW, 0x01
        BRCS   loop_cont               ; if we are on the first row, don't save row A to RAM
        CALL   out_row                 ; save row A to RAM
loop_cont:
        CALL   transfer_BtoA
        ADD    ROW, 0x01
        CMP    ROW, 0x1E               ; check row is still under 30 (0x1E)
        BRNE   loop_row                ; branch to draw more rows
                                       ; we have looped throught the entire monitor
        CALL   out_row
        CALL   transfer_BtoA

        CALL   delay                   ; wait to start the next loop
        IN     r10, PAUSE_PLAY         ; check to see if the game should be paused after the frame has been fully rendered
        OR     START_STOP, 0x00        ; check the user wants to keep playing
        BRNE   pause_loop           

        BRN    start_loop              ; game plays if START_STOP = 0x00
        
;---------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: pause_loop
;-
;-  Indefinitely polls PAUSE_PLAY input port for a change in r10
;-  and branches back to start_loop when r10 = 0x00 again
;-
;-  Tweaked registers: r10 (START_STOP)
;--------------------------------------------------------------------
pause_loop:
        IN      r10, PAUSE_PLAY     ; check to see if game should be resumed
        OR      START_STOP, 0x00
        BREQ    start_loop
        CALL    delay
        BRN     pause_loop

;---------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: start_game
;-
;-  Implements Conway's Game of Life in our RAT processor.
;- 
;- Tweaked registers: r4 (TEMP_ROW), r5 (TEMP_COL), r0 (CUR_COLOR)
;-                    r15 (SET_PIX)
;--------------------------------------------------------------------

start_game:
        MOV     TEMP_ROW, ROW           ; these two lines are necessary bc read_pixel only reads pixel value from these coordinates
        MOV     TEMP_COL, COL
        CALL    calc_neighbors          ; this will change values of temp x&y...

        MOV     TEMP_ROW, ROW           ; ... so reset them before calling read_pixel
        MOV     TEMP_COL, COL
        CALL    read_pixel              ; get current pixel color

        AND     CUR_COLOR, 0xFF         ; if white, r0 will be 0xFF, otherwise it will be 0x00
        BREQ    pixel_is_dead           ; pixel was black ("dead")
        BRN     pixel_is_alive          ; pixel was white ("live")
pixel_is_dead:
        CMP     NUM_NEIGH, 0x03         ; if a dead pixel has 3 neighbors, it will come to life
        BREQ    create_pixel
        BRN     kill_pixel
pixel_is_alive:
        CMP     NUM_NEIGH, 0x02         ; if a live pixel has less than 2 neighbors, it dies
        BRCS    kill_pixel
        CMP     NUM_NEIGH, 0x04         ; if a live pixel has more than 3 neighbors, it dies
        BRCC    kill_pixel
        BRN     create_pixel

kill_pixel:
        MOV     SET_PIX, 0x00           ; pixel should be turned black
        BRN     create_mask_setup
create_pixel:
        MOV     SET_PIX, 0x01           ; pixel should be turned white
        BRN     create_mask_setup
create_mask_setup:                      ; now we create a bit mask to affect the desired pixel in the registers
        CALL    setup
        MOV     BIT_MASK, 0x00
        SEC
create_mask:
        LSL     BIT_MASK                ; LSB stores left-most pixel (0), so we start from there and shift left
        CMP     CURR_PIX, 0x01
        BRCS    end                     ; if loop counter was 0, stop shifting
        SUB     CURR_PIX, 0x01
        CLC                             ; clear so that only one bit is "on" in our mask
        BRNE    create_mask
        CALL    apply_mask
        RET
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: draw_white_or_black
;-
;-  Helper subroutine for out_row. Writes a specific pixel to the RAM.
;- 
;- Tweaked registers: r6 (COLOR)
;--------------------------------------------------------------------


draw_white_or_black:
        BRCS   draw_white
        BRN    draw_black
draw_white:
        MOV    COLOR, 0xFF
        CALL   draw_dot
        RET
draw_black:
        MOV    COLOR, 0x00
        CALL   draw_dot
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
;- Tweaked registers: r11 (BIT_MASK), r31 (CURR_PIX)
;--------------------------------------------------------------------

setup:
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
        ADD     CURR_PIX, 0x01
        RET
setup_16:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x08
        ADD     CURR_PIX, 0x01
        RET
setup_24:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x10
        ADD     CURR_PIX, 0x01
        RET
setup_32:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x18
        ADD     CURR_PIX, 0x01
        RET
setup_40:
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x20
        ADD     CURR_PIX, 0x01
        RET
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;-  Subroutine: apply_mask
;-
;-  Applies the mask to either clear or set a bit in the appropriate
;-  register in ROW B
;- 
;- Tweaked registers: r26 (ROWB_08), r27 (ROWB_16), r28 (ROWB_24),
;-                    r29 (ROWB_32), r30 (ROWB_40)
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
        BREQ    set_08                  ; if we want to clear the bit...
        EXOR    BIT_MASK, 0xFF          ; invert the mask
        AND     ROWB_08, BIT_MASK       ; clear the COLth bit and leave all others unchanged
        RET
set_08:
        OR      ROWB_08, BIT_MASK       ; set the COLth bit and leave all others unchanged
        RET
apply_mask_16:
        CMP     SET_PIX, 0x01
        BREQ    set_16                  ; if we want to clear the bit...
        EXOR    BIT_MASK, 0xFF          ; invert the mask
        AND     ROWB_16, BIT_MASK
        RET
set_16:
        OR      ROWB_16, BIT_MASK
        RET
apply_mask_24:
        CMP     SET_PIX, 0x01
        BREQ    set_24                  ; if we want to clear the bit...
        EXOR    BIT_MASK, 0xFF          ; invert the mask
        AND     ROWB_24, BIT_MASK
        RET
set_24:
        OR      ROWB_24, BIT_MASK
        RET
apply_mask_32:
        CMP     SET_PIX, 0x01
        BREQ    set_32                  ; if we want to clear the bit...
        EXOR    BIT_MASK, 0xFF          ; invert the mask
        AND     ROWB_32, BIT_MASK
        RET
set_32:
        OR      ROWB_32, BIT_MASK
        RET
apply_mask_40:
        CMP     SET_PIX, 0x01
        BREQ    set_40                  ; if we want to clear the bit...
        EXOR    BIT_MASK, 0xFF          ; invert the mask
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
;- then transfer the data in rowB to rowA and clears rowB
;- 
;- Tweaked registers: r1-r25 (ROWA), r26-r30 (ROWB)
;---------------------------------------------------------------------
out_row:
        MOV     TEMP_Y, ROW             ; save y coordinate before going back
        MOV     COL, 0x00
        SUB     ROW, 0x01               ; go back two rows
        MOV     r3, 0x08                ; a register can only hold 8 bits

        CALL    out_ROWA_08
        MOV     r3, 0x08
        CALL    out_ROWA_16
        MOV     r3, 0x08
        CALL    out_ROWA_24
        MOV     r3, 0x08
        CALL    out_ROWA_32
        MOV     r3, 0x08
        CALL    out_ROWA_40
        MOV     ROW, TEMP_Y              ; change r8 to value before subroutine
        MOV     COL, 0x00                ; reset x-coordinate
        RET
out_ROWA_08:
        CLC
        LSR     ROWA_08
        CALL    draw_white_or_black
  
        ADD     COL, 0x01                ; increment x-coordinate
        SUB     r3, 0x01
        BRNE    out_ROWA_08
        RET
out_ROWA_16:
        CLC
        LSR     ROWA_16
        CALL    draw_white_or_black

        ADD     COL, 0x01                ; increment x-coordinate
        SUB     r3, 0x01                 ; decrement register counter
        BRNE    out_ROWA_16
        RET
out_ROWA_24:
        CLC
        LSR     ROWA_24
        CALL    draw_white_or_black

        ADD     COL, 0x01                ; increment x-coordinate
        SUB     r3, 0x01
        BRNE    out_ROWA_24
        RET
out_ROWA_32:
        CLC
        LSR     ROWA_32
        CALL    draw_white_or_black

        ADD     COL, 0x01                ; increment x-coordinate
        SUB     r3, 0x01
        BRNE    out_ROWA_32
        RET
out_ROWA_40:
        CLC
        LSR     ROWA_40
        CALL    draw_white_or_black

        ADD     COL, 0x01                ; increment x-coordinate
        SUB     r3, 0x01
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
;- Tweaked registers: r1-r25 (ROWA), r26-r30 (ROWB)
;---------------------------------------------------------------------
        
transfer_BtoA:
        MOV     ROWA_08, ROWB_08
        MOV     ROWA_16, ROWB_16
        MOV     ROWA_24, ROWB_24
        MOV     ROWA_32, ROWB_32
        MOV     ROWA_40, ROWB_40

        MOV     ROWB_08, 0x00            ; clear "current" row before doing any calculations
        MOV     ROWB_16, 0x00
        MOV     ROWB_24, 0x00
        MOV     ROWB_32, 0x00
        MOV     ROWB_40, 0x00
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
        MOV     NUM_NEIGH, 0x00
create_mask_above:
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL
        SUB     TEMP_ROW, 0x01

        BRCS    create_mask_right        ; if overflowed, top row is out of bounds
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_ald
        ADD     NUM_NEIGH, 0x01 
create_mask_ald:                         ; above left diagonal neighbor
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL   
        SUB     TEMP_ROW, 0x01
        SUB     TEMP_COL, 0x01

        BRCS    create_mask_ard          ; bounds check
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_ard
        ADD     NUM_NEIGH, 0x01
create_mask_ard:                          ; above right diagonal neighbor
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL   
        SUB     TEMP_ROW, 0x01
        ADD     TEMP_COL, 0x01

        CMP     TEMP_COL, 0x28            ; check if it's gone over 40
        BRCC    create_mask_left
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_right
        ADD     NUM_NEIGH, 0x01
create_mask_right:                        ; immediate right neighbor
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL   
        ADD     TEMP_COL, 0x01

        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_left
        ADD     NUM_NEIGH, 0x01
create_mask_left:                         ; immediate left neighbor
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL   
        SUB     TEMP_COL, 0x01

        BRCS    create_mask_bottom
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_bottom
        ADD     NUM_NEIGH, 0x01
create_mask_bottom:                       ; immediate bottom neighbor
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL   
        ADD     TEMP_ROW, 0x01

        CMP     TEMP_ROW, 0x1E            ; check the row hasn't gone over 29 (0x1E)
        BRCC    end
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_bld
        ADD     NUM_NEIGH, 0x01
create_mask_bld:                           ; bottom left diagonal neighbor
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL   
        ADD     TEMP_ROW, 0x01
        SUB     TEMP_COL, 0x01

        BRCS    create_mask_brd
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_brd
        ADD     NUM_NEIGH, 0x01
create_mask_brd:                            ; botton right diagonal neighbor
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL   
        ADD     TEMP_ROW, 0x01
        ADD     TEMP_COL, 0x01

        CMP     TEMP_COL, 0x28
        BRCC    end
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    end
        ADD     NUM_NEIGH, 0x01 
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
;- Tweaked registers: r8 (COL), r9
;--------------------------------------------------------------------
draw_horizontal_line:
        ADD    r9, 0x01                  ; go from r8 to r15 inclusive

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
;-  Tweaked registers: r13, r7 (ROW), r8 (COL), r9
;----------------------------------------------------------------------
draw_background: 
        MOV   COLOR, BG_COLOR            ; use default color
        MOV   r13, 0x00                  ; r13 keeps track of rows
start:  MOV   ROW, r13                   ; load current row count 
        MOV   COL, 0x00                  ; restart x coordinates
        MOV   r9, 0x27 

        CALL  draw_horizontal_line
        ADD   r13, 0x01                  ; increment row count
        CMP   r13, 0x1E                  ; see if more rows to draw
        BRNE  start                      ; branch to draw more rows
        RET
;---------------------------------------------------------------------

;---------------------------------------------------------------------
;-  Subroutine: draw_vertical_line
;-
;-  Draws a horizontal line from (r8,r7) to (r8,r9) using color in r6
;-
;-  Parameters:
;-   r8  = x-coordinate
;-   r7  = starting y-coordinate
;-   r9  = ending y-coordinate
;-   r6  = color used for line
;- 
;- Tweaked registers: r7 (ROW), r9
;--------------------------------------------------------------------
draw_vertical_line:
         ADD    r9,0x01

draw_vert1:          
         CALL   draw_dot
         ADD    r7,0x01
         CMP    r7,R9
         BRNE   draw_vert1
         RET
;--------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: draw_dot
;- Author: SHANE KENT
;- 
;- This subroutine draws a dot on the display the given coordinates: 
;- 
;- (X,Y) = (r5,r4)  with a color stored in r6  
;- 
;- Tweaked registers: r1, r2
;---------------------------------------------------------------------
draw_dot: 
        MOV   r1, ROW                    ; copy Y coordinate
        MOV   r2, COL                    ; copy X coordinate

        AND   r2,0x3F                    ; make sure top 2 bits cleared
        AND   r1,0x1F                    ; make sure top 3 bits cleared
        LSR   r1                         ; need to get the bot 2 bits of r4 into sA
        BRCS  dd_add40
t1:     LSR   r1
        BRCS  dd_add80

dd_out: OUT   r2, VGA_LADD               ; write bot 8 address bits to register
        OUT   r1, VGA_HADD               ; write top 3 address bits to register
        OUT   r6, VGA_COLOR              ; write data to frame buffer
        RET

dd_add40:
        OR    r2, 0x40                   ; set bit if needed
        CLC                              ; freshen bit
        BRN   t1             

dd_add80: 
        OR    r2, 0x80                   ; set bit if needed
        BRN   dd_out
; --------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: read_pixel
;- 
;- This subroutine will read a current pixel color from the 2k RAM: 
;- (X,Y) = (r4,r5)
;- 
;- Tweaked registers: r4 (TEMP_ROW) ,r5 (TEMP_COL)
;---------------------------------------------------------------------
read_pixel: 
        AND   r5, 0x3F                   ; make sure top 2 bits cleared
        AND   r4, 0x1F                   ; make sure top 3 bits cleared
        LSR   r4                         ; need to get the bot 2 bits of r4 into sA
        BRCS  ll_add40      

l1:     LSR   r4            
        BRCS  ll_add80

ll_out: OUT   r5, VGA_LADD               ; write bot 8 address bits to register
        OUT   r4, VGA_HADD               ; write top 3 address bits to register
        IN    r0, VGA_READ
        MOV   TEMP_ROW, ROW
        MOV   TEMP_COL, COL
        RET

ll_add40:
        OR    r5, 0x40                   ; set bit if needed
        CLC                              ; freshen bit
        BRN   l1             

ll_add80:
        OR    r5, 0x80                   ; set bit if needed
        BRN   ll_out
; --------------------------------------------------------------------
