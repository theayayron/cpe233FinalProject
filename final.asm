;---------------------------------------------------------------------
; Conway's Game of Life 
;
; Author: Caitlin Settles and Aaron Kawahara
;---------------------------------------------------------------------

.CSEG
.ORG 0x10

.EQU VGA_HADD = 0x90
.EQU VGA_LADD = 0x91
.EQU VGA_COLOR = 0x92

.EQU VGA_READ = 0x93

.EQU SSEG = 0x81
.EQU LEDS = 0x40

.EQU BG_COLOR       = 0x00             ; Background:  black

;r6 is used for color
;r7 is used for Y
;r8 is used for X
;r0 is used for pixelValue

;---------------------------------------------------------------------
init:   SEI
        CALL   draw_background         ; draw using default color
start_loop:
        MOV    r7, 0x00                ; generic Y coordinate
        MOV    r8, 0x00                ; generic X coordinate
loop_row:
        MOV    r8,0x00                 ; restart x coordinates
loop_col:
        CALL   start_game
        ADD    r8,0x01                 ; increment column count
        CMP    r8,0x28                 ; check column is still under 40 (0x28)
        BRNE   loop_col                ; if it is under 40, keep looping

        ADD    r7,0x01                 ; increment row count
        CMP    r7,0x1E                 ; check row is still under 30 (0x1E)
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
        AND    r0, 0xFF                ; if white, r0 will be 0xFF, otherwise it will be 0x00
        BREQ   draw_white              ; flip the color of the pixel
        BRN    draw_black

draw_white:
        MOV r6, 0xFF
        CALL draw_dot
        RET

draw_black:
        MOV r6, 0x00
        CALL draw_dot
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
        MOV   r4,r7         ; copy Y coordinate
        MOV   r5,r8         ; copy X coordinate

        AND   r5,0x3F       ; make sure top 2 bits cleared
        AND   r4,0x1F       ; make sure top 3 bits cleared
        LSR   r4            ; need to get the bot 2 bits of r4 into sA
        BRCS  ll_add40      
                            
l1:     LSR   r4            

        BRCS  ll_add80

ll_out: OUT   r5,VGA_LADD   ; write bot 8 address bits to register
        OUT   r4,VGA_HADD   ; write top 3 address bits to register
        IN    r0,VGA_READ
        RET

ll_add40:  OR    r5,0x40       ; set bit if needed
           CLC                  ; freshen bit
           BRN   l1             

ll_add80:  OR    r5,0x80       ; set bit if needed
           BRN   ll_out
; --------------------------------------------------------------------