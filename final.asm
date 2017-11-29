;---------------------------------------------------------------------
; An expanded "draw_dot" program that includes subrountines to draw
; vertical lines, horizontal lines, and a full background. 
; 
; As written, this programs does the following: 
;   1) draws a the background blue (draws all the tiles)
;   2) draws a red dot
;   3) draws a red horizontal lines
;   4) draws a red vertical line
;
; Author: SHANE KENT
; Modifications: SHANE KENT
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

; Two rows must be recorded at a time until the third row is reached, 
; so that changes made to past pixels do not mess with the behavior
; of future pixels

; These 5 8-bit registers combine to represent 40 pixels in a row,
; with each pixel representing a single bit. The LSB represents the 
; left-most pixel, and the MSB represents the right-most pixel.
.DEF ROWA_08 	= r21		
.DEF ROWA_16	= r22
.DEF ROWA_24    = r23
.DEF ROWA_32    = r24
.DEF ROWA_40	= r25
	
.DEF ROWB_08 	= r26
.DEF ROWB_16	= r27
.DEF ROWB_24    = r28
.DEF ROWB_32    = r29
.DEF ROWB_40	= r30
	
.DEF CURR_PIX 	= r31
.DEF TEMP_Y		= r20


;r6 is used for color
;r7 is used for Y
;r8 is used for X

;---------------------------------------------------------------------
init:	  SEI
        CALL   draw_background         ; draw using default color
start_loop:
        MOV    r7, 0x00                ; generic Y coordinate
        MOV    r8, 0x00                ; generic X coordinate
loop_row:
        MOV    r8,0x00                 ; restart x coordinates
loop_col:
        ADD    r8,0x01                 ; increment column count
        CALL   start_game
        CMP    r8,0x28                 ; check column is still under 40 (0x28)
        BRNE   loop_col                ; if it is under 40, keep looping

        ADD    r7,0x01                 ; increment row count
        CMP    r7,0x1E                 ; check row is still under 30 (0x1E)
        BRNE   loop_row                ; branch to draw more rows
        CALL   delay
        BRN    start_loop              ; this makes an infinite loop

;--------------------------------------------------------------------

start_game:
        CALL   read_pixel
        AND    r0, 0xFF
        BREQ   draw_white
        BRN    draw_black

draw_white_or_black:
		BRCS draw_white
		BRCC draw_black

draw_white:
        MOV r6, 0xFF
        CALL draw_dot
        RET

draw_black:
        MOV r6, 0x00
        CALL draw_dot
        RET

delay:
        MOV r1, 0xFF
d1:
        MOV r2, 0xFF
d2:
        MOV r3, 0xFF
        
        SUB r3, 0x01
        BRNE d2
        SUB r2, 0x01
        BRNE d1
        SUB r1, 0x01
        BRNE delay
        RET


;--------------------------------------------------------------------
;-  Subroutine: draw_horizontal_line
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
;- This subroutine will read a pixel color from the 2k RAM: 
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

;---------------------------------------------------------------------
;- Subrountine: out_row
;- 
;- This subroutine write out the new pixel vals for rowA, 
;- then transfer the data in rowB to rowA and clear rowB
;- 
;- Tweaked registers: ROWA & ROWB (r21-r30)
;---------------------------------------------------------------------
out_row:
		MOV 	TEMP_Y, r8			;; save row before going back
		SUB 	r8, 0x02			;; go back two rows
		MOV 	CURR_PIX, 0x08
		CALL 	out_ROWA_08
		MOV 	CURR_PIX, 0x08
		CALL 	out_ROWA_16
		MOV 	CURR_PIX, 0x08
		CALL 	out_ROWA_24
		MOV 	CURR_PIX, 0x08
		CALL 	out_ROWA_32
		MOV 	CURR_PIX, 0x08
		CALL	out_ROWA_40
		MOV 	r8, TEMP_Y			;; change r8 to value before subroutine
		MOV 	r7, 0x00			;; reset x-coordinate
		RET

out_ROWA_08:
		CLC
		LSR 	ROWA_08
		CALL 	draw_white_or_black

		ADD 	r8, 0x01		; increment x-coordinate
		SUB 	CURR_PIX, 0x01
		BRNE 	out_ROWA_08
		RET

out_ROWA_16:
		CLC
		LSR 	ROWA_16
		CALL 	draw_white_or_black

		ADD 	r8, 0x01		; increment x-coordinate
		SUB 	CURR_PIX, 0x01
		BRNE 	out_ROWA_16
		RET

out_ROWA_24:
		CLC
		LSR 	ROWA_24
		CALL 	draw_white_or_black

		ADD 	r8, 0x01		; increment x-coordinate
		SUB 	CURR_PIX, 0x01
		BRNE 	out_ROWA_24
		RET

out_ROWA_32:
		CLC
		LSR 	ROWA_32
		CALL 	draw_white_or_black

		ADD 	r8, 0x01		; increment x-coordinate
		SUB 	CURR_PIX, 0x01
		BRNE 	out_ROWA_32
		RET

out_ROWA_40:
		CLC
		LSR 	ROWA_40
		CALL 	draw_white_or_black

		ADD 	r8, 0x01		; increment x-coordinate
		SUB 	CURR_PIX, 0x01
		BRNE 	out_ROWA_40
		RET
		
transfer_BtoA:
		MOV 	ROWA_08, ROWB_08
		MOV		ROWA_16, ROWB_16
		MOV		ROWA_24, ROWB_24
		MOV		ROWA_32, ROWB_32
		MOV		ROWA_40, ROWB_40
		RET
