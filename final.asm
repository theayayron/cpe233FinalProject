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

.DEF CURR_ROW   = r10
.DEF BIT_MASK   = r11
.DEF NUM_NEIGH  = r12

.DEF TOP_ROW    = r13
.DEF BOT_ROW    = r14

;r6 is used for color
;r7 is used for Y
;r8 is used for X
;r0 is used for pixelValue

;---------------------------------------------------------------------
init:   SEI
        CALL   draw_background         ; draw using default color
start_loop:
        MOV    ROW, 0x00
        MOV    COL, 0x00
loop_row:
        MOV    ROW,0x00                ; restart x coordinates
loop_col:
        CALL   start_game
        ADD    COL, 0x01
        CMP    COL, 0x28               ; check column is still under 40 (0x28)
        BRNE   loop_col                ; if it is under 40, keep looping

        ADD    ROW, 0x01
        CMP    ROW, 0x1E               ; check row is still under 30 (0x1E)
        BRNE   loop_row                ; branch to draw more rows
        CALL   delay
        BRN    start_loop              ; this makes an infinite loop

;--------------------------------------------------------------------
;-  Subroutine: start_game
;-
;-  Flips the color of the monitor from black to white inifinitely.
;- 
;- Tweaked registers: r0,r6
;--------------------------------------------------------------------

start_game:
        CALL   read_pixel              ; get current pixel color
        AND    CUR_COLOR, 0xFF         ; if white, r0 will be 0xFF, otherwise it will be 0x00
        BREQ   draw_white              ; flip the color of the pixel
        BRN    draw_black

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
        ADD    r9,0x01          ; go from r8 to r15 inclusive

draw_horiz1:
        CALL   draw_dot         ; 
        ADD    r8,0x01
        CMP    r8,r9
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
        MOV   r6,BG_COLOR              ; use default color
        MOV   r13,0x00                 ; r13 keeps track of rows
start:  MOV   r7,r13                   ; load current row count 
        MOV   r8,0x00                  ; restart x coordinates
        MOV   r9,0x27 
 
        CALL  draw_horizontal_line
        ADD   r13,0x01                 ; increment row count
        CMP   r13,0x1E                 ; see if more rows to draw
        BRNE  start                    ; branch to draw more rows
        RET
;---------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: draw_dot
;- Author: SHANE KENT
;- 
;- This subroutine draws a dot on the display the given coordinates: 
;- 
;- (X,Y) = (r8,r7)  with a color stored in r6  
;- 
;- Tweaked registers: r4,r5
;---------------------------------------------------------------------
draw_dot: 
        MOV   r4,r7         ; copy Y coordinate
        MOV   r5,r8         ; copy X coordinate

        AND   r5,0x3F       ; make sure top 2 bits cleared
        AND   r4,0x1F       ; make sure top 3 bits cleared
        LSR   r4             ; need to get the bot 2 bits of r4 into sA
        BRCS  dd_add40
t1:     LSR   r4
         
        BRCS  dd_add80

dd_out: OUT   r5,VGA_LADD   ; write bot 8 address bits to register
        OUT   r4,VGA_HADD   ; write top 3 address bits to register
        OUT   r6,VGA_COLOR  ; write data to frame buffer
        RET

dd_add40:  OR    r5,0x40       ; set bit if needed
           CLC                  ; freshen bit
           BRN   t1             

dd_add80:  OR    r5,0x80       ; set bit if needed
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
        ;MOV   r4, ROW        ; copy Y coordinate
        ;MOV   r5, COL        ; copy X coordinate

        AND   TEMP_COL,0x3F       ; make sure top 2 bits cleared
        AND   TEMP_ROW,0x1F       ; make sure top 3 bits cleared
        LSR   TEMP_ROW            ; need to get the bot 2 bits of r4 into sA
        BRCS  ll_add40      
                            
l1:     LSR   TEMP_ROW            
        BRCS  ll_add80

ll_out:	OUT   r5, VGA_LADD   ; write bot 8 address bits to register
        OUT   r4, VGA_HADD   ; write top 3 address bits to register
        IN    r0, VGA_READ
        RET

ll_add40:  OR    TEMP_COL,0x40    ; set bit if needed
           CLC                    ; freshen bit
           BRN   l1             

ll_add80:  OR    TEMP_COL,0x80    ; set bit if needed
           BRN   ll_out

; --------------------------------------------------------------------

;---------------------------------------------------------------------
;- Subrountine: out_row
;- 
;- This subroutine writes out the new pixel colors for rowA, 
;- then transfer the data in rowB to rowA and clear rowB
;- 
;- Tweaked registers: ROWA & ROWB (r21-r30)
;---------------------------------------------------------------------
out_row:
        MOV     TEMP_Y, r8          ;; save y coordinate before going back two rows
        SUB     r8, 0x02            
        MOV     CURR_PIX, 0x08      ;; count variable to iterate through eight bits
        CALL    out_ROWA_08
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_16
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_24
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_32
        MOV     CURR_PIX, 0x08
        CALL    out_ROWA_40
		CALL    transfer_BtoA
        MOV     r8, TEMP_Y          ;; change r8 to value before subroutine
        MOV     r7, 0x00            ;; reset x-coordinate
        RET

out_ROWA_08:
        CLC
        LSR     ROWA_08
        CALL    draw_white_or_black

        ADD     COL, 0x01           ; increment x-coordinate
        SUB     CURR_PIX, 0x01
        BRNE    out_ROWA_08
        RET

out_ROWA_16:
        CLC
        LSR     ROWA_16
        CALL    draw_white_or_black

        ADD     COL, 0x01           ; increment x-coordinate
        SUB     CURR_PIX, 0x01      ; decrement register counter
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
        
transfer_BtoA:
        MOV     ROWA_08, ROWB_08
        MOV     ROWA_16, ROWB_16
        MOV     ROWA_24, ROWB_24
        MOV     ROWA_32, ROWB_32
        MOV     ROWA_40, ROWB_40
        RET

;---------------------------------------------------------------------
;- Subrountine: calc_dot
;- 
;- Calculates whether a pixel will "live" or "die" based on how many
;- living neighbors it has.
;- 
;- Tweaked registers: ROWA & ROWB (r21-r30), r10 (holds masked row),
;- r11 (holds inverted COL coordinate), r12 (holds the count of neighbors)
;---------------------------------------------------------------------

calc_dot:
        CMP     COL, 0x90
        BRCS    setup_08
        CMP     COL, 0x17
        BRCS    setup_16
        CMP     COL, 0x25
        BRCS    setup_24
        CMP     COL, 0x33
        BRCS    setup_32
        BRN     setup_40

setup_08:
        MOV     TOP_ROW, ROWA_08
        MOV     BOT_ROW, ROWB_08
        MOV     CURR_PIX, COL
        BRN     calc_neighbors
setup_16:
        MOV     TOP_ROW, ROWA_16
        MOV     BOT_ROW, ROWB_16
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x10
        BRN     calc_neighbors
setup_24:
        MOV     TOP_ROW, ROWA_24
        MOV     BOT_ROW, ROWB_24
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x18
        BRN     calc_neighbors
setup_32:
        MOV     TOP_ROW, ROWA_32
        MOV     BOT_ROW, ROWB_32
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x20
        BRN     calc_neighbors
setup_40:
        MOV     TOP_ROW, ROWA_40
        MOV     BOT_ROW, ROWB_40
        MOV     CURR_PIX, COL
        SUB     CURR_PIX, 0x28
        BRN     calc_neighbors

calc_neighbors:
        MOV     CURR_ROW, TOP_ROW     ; start with above bit
        MOV     BIT_MASK, 0x00        ; say we are trying to mask the third bit (r11 = 0x02)
        SEC                           ; set carry flag
create_mask_above:
        LSL     BIT_MASK              ; shift left
        SUB     CURR_PIX, 0x01
        BRNE    create_mask_above
        AND     CURR_ROW, BIT_MASK    ; clear all bits except the COlth bit
        CALL    increment_neighbors   ; add it to NUM_NEIGH
create_mask_ald:                   	  ; above left diagonal neighbor
        MOV     CURR_ROW, TOP_ROW
        CLC                           ; clear carry flag so that shift doesn't introduce any unwanted bits
        LSL     BIT_MASK              
        AND     CURR_ROW, BIT_MASK
        CALL    increment_neighbors
create_mask_ard:                   ; above right diagonal neighbor
        MOV     CURR_ROW, TOP_ROW
        CLC
        LSR     BIT_MASK
        CLC
        LSR     BIT_MASK
        AND     CURR_ROW, BIT_MASK
        CALL    increment_neighbors
create_mask_right:
        MOV     CURR_ROW, BOT_ROW
        AND     CURR_ROW, BIT_MASK
        CALL    increment_neighbors
create_mask_left:
        MOV     CURR_ROW, BOT_ROW
        CLC
        LSR     BIT_MASK
        CLC
        LSR     BIT_MASK
        AND     CURR_ROW, BIT_MASK
        CALL    increment_neighbors
create_mask_bottom:
        MOV     TEMP_ROW, ROW
        MOV     TEMP_COL, COL
        ADD     TEMP_ROW, 0x01
        CALL    read_pixel
        CMP     CUR_COLOR, 0xFF
        BRNE    create_mask_bld
        ADD     NUM_NEIGH, 0x01
create_mask_bld:
        SUB    TEMP_COL, 0x01
        CALL   read_pixel
        CMP    CUR_COLOR, 0xFF
        BRNE   create_mask_brd
        ADD    NUM_NEIGH, 0x01
create_mask_brd:
        ADD    TEMP_COL, 0x02
        CALL   read_pixel
        CMP    CUR_COLOR, 0xFF
        BRNE   end
        ADD    NUM_NEIGH, 0x01 
end:
        RET


; loops through every bit in a register and adds it to r12
; we're calling this on a masked register, so there should only be one bit that we care about,
; but the rest are zeroes anyways so it doesn't matter if we add them, and we have no way of
; knowing which location the bit we care about is at.
; editing r14
increment_neighbors: 
        MOV     CURR_PIX, 0x08
        LSR     CURR_ROW
        ADDC    NUM_NEIGH, 0x00       ; add c flag to r12
        SUB     CURR_PIX, 0x01
        BRNE    increment_neighbors
        RET
