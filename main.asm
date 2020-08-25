cpu 386
use16

UP_ARROW	equ	075h			; UP KEY SCAN CODE
DOWN_ARROW	equ	072h			; DOWN KEY
LEFT_ARROW	equ	06Bh			; LEFT KEY
RIGHT_ARROW	equ	074h			; RIGHT KEY
SLIDE_LEFT	equ	06Ch			; HOME KEY
SLIDE_RIGHT	equ	069h			; END KEY
HORZ_CENTER	equ	160				; center of screen, horizontal
VERT_CENTER	equ 100				; vertical
STACK_SIZE	equ 4096			; 4K of stack (after BSS section) should be enough

main:
	cld							; clear direction flag, as we can't assume anything
	mov ax, 0A000h
	mov es, ax					; set ES to video segment
	xor ax, ax
;	cli							; surrounding CLI/STI would be needed for some early 8088 processors
	mov ss, ax					;	since we targeting 80386 anyway, they're turned off to save space
	mov sp, end_all+STACK_SIZE	;	see https://stackoverflow.com/questions/32701854/boot-loader-doesnt-jump-to-kernel-code/
;	sti
	mov ds, ax					; ES,SS,DS set, CS could be 0x0000 or 0x07c0 or anything else, doesn't matter since we don't use jump tables
	mov ax, 13h
	int 10h						; VGA Mode 13h set
	
	fld dword [plyangle]		; 
	fsincos						; [ cos(plyangle), sin(plyangle) ]
	fld dword [plycoords]		;
	fld dword [plycoords+4]		; [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	jmp projector				; at the very first time, having set up preconditions for projector, jump there straight 
								; 	to have our first screen layout before any key need to be pressed
keyPress:
	xor ah, ah
	int 16h						; AH=0: pause and get keypress
	cmp ah, UP_ARROW
	je keyPress_forward
	cmp ah, DOWN_ARROW
	je keyPress_backward
	cmp ah, LEFT_ARROW
	je keyPress_turnLeftRight
	cmp ah, RIGHT_ARROW
	je keyPress_turnLeftRight
;	cmp ah, SLIDE_LEFT			; SLIDING: switched off feature -- doesn't fit in 512 bytes
;	je keyPress_slideLeft
;	cmp ah, SLIDE_RIGHT
;	je keyPress_slideRight
	jmp keyPress

keyPress_forward:
	fadd st3					; [ ply.y+sin(plyangle), ply.x, cos(plyangle), sin(plyangle) ]
	fstp dword [plycoords+4]	; update ply.y [ ply.x, cos(plyangle), sin(plyangle) ]
	fadd st1					; [ ply.x+cos(plyangle), cos(plyangle), sin(plyangle) ]
	fst dword [plycoords]		; update ply.x [ ply.x+cos(plyangle), cos(plyangle), sin(plyangle) ]
	fld dword [plycoords+4]		; reload ply.y [ ply.y+sin(plyangle), ply.x+cos(plyangle), cos(plyangle), sin(plyangle) ]
								; using updated variables: [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	jmp projector
	
keyPress_backward:
	fsub st3					; [ ply.y-sin(plyangle), ply.x, cos(plyangle), sin(plyangle) ]
	fstp dword [plycoords+4]	; update ply.y [ ply.x, cos(plyangle), sin(plyangle) ]
	fsub st1					; [ ply.x-cos(plyangle), cos(plyangle), sin(plyangle) ]
	fst dword [plycoords]		; update ply.x [ ply.x-cos(plyangle), cos(plyangle), sin(plyangle) ]
	fld dword [plycoords+4]		; reload ply.y [ ply.y-sin(plyangle), ply.x-cos(plyangle), cos(plyangle), sin(plyangle) ]
								; using updated variables: [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	jmp projector
	
;keyPress_slideLeft:			; SLIDING: switched off feature -- doesn't fit in 512 bytes
;	fsub st2					; [ ply.y-cos(plyangle), ply.x, cos(plyangle), sin(plyangle) ]
;	fstp dword [plycoords+4]	; update ply.y [ ply.x, cos(plyangle), sin(plyangle) ]
;	fadd st2					; [ ply.x+sin(plyangle), cos(plyangle), sin(plyangle) ]
;	fst dword [plycoords]		; update ply.x [ ply.x+sin(plyangle), cos(plyangle), sin(plyangle) ]
;	fld dword [plycoords+4]		; reload ply.y [ ply.y-cos(plyangle), ply.x+sin(plyangle), cos(plyangle), sin(plyangle) ]
;								; using updated variables: [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
;	jmp projector
;	
;keyPress_slideRight:
;	fadd st2					; [ ply.y+cos(plyangle), ply.x, cos(plyangle), sin(plyangle) ]
;	fstp dword [plycoords+4]	; update ply.y [ ply.x, cos(plyangle), sin(plyangle) ]
;	fsub st2					; [ ply.x-sin(plyangle), cos(plyangle), sin(plyangle) ]
;	fst dword [plycoords]		; update ply.x [ ply.x-sin(plyangle), cos(plyangle), sin(plyangle) ]
;	fld dword [plycoords+4]		; reload ply.y [ ply.y+cos(plyangle), ply.x-sin(plyangle), cos(plyangle), sin(plyangle) ]
;								; using updated variables: [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
;	jmp projector

keyPress_turnLeftRight:
	finit						; EMPTY []
	fld dword [plyangle]		; [ plyangle ]
	fld dword [plyangleDelta]	; [ plyagnleDelta, plyangle ]
	cmp ah, RIGHT_ARROW			; check if this is positive direction
	je keyPress_turnSkipNeg		; if so, skip negation
	fchs
keyPress_turnSkipNeg:			; [ +-plyagnleDelta, plyangle ]
	fadd						; [ plyangle+-plyagnleDelta ]
	fst dword [plyangle]		; update plyangle [ plyangle+-plyagnleDelta ]
	fsincos						; [ cos(plyangle+-plyagnleDelta), sin(plyangle+-plyagnleDelta) ]
								; using updated variables: [ cos(plyangle), sin(plyangle) ]
	fld dword [plycoords]		;
	fld dword [plycoords+4]		; [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
								
projector:

	mov cx, 320*200/2			; clearScreen, fill whole video memory area w/ zeros (A000:0000-A000:FA00h, stack moved somewhere else)
	xor ax, ax
	xor di, di
	rep stosw

	; FPU STACK:				; [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	lea si, [polygon]			;
	mov cx, POLYGONSIZE
	
calcOneSide:
	lea di, [tx1]				; set to ....1 variableset
	mov [plotxy_color], cl		; set color of current side
	push cx						; save cx for outer loop
	mov cl, 2					; two vertices at a time -- setting CL is enough assuming that POLYGONSIZE will be < 256 at all times
calcOneVertex:	
	fild word [si]				; [ pgn[i].x, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fsub st2					; [ pgn[i].x-ply.x, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fild word [si+2]			; [ pgn[i].y, pgn[i].x-ply.x, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fsub st2					; [ pgn[i].y-ply.y, pgn[i].x-ply.x, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
								;   ------ty------  ------tx------
	fld st1						; [ tx, ty, tx, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fmul st5					; [ tx*cos(angle), ty, tx, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	
	fld st1						; [ ty, tx*cos(angle), ty, tx, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fmul st7					; FULL [ ty*sin(angle), tx*cos(angle), ty, tx, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fadd						; [ tx*cos(angle)+ty*sin(angle), ty, tx, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	
								; di+4 = tz1 or 2
	fstp dword [di+4]			; [ ty, tx, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	
	fmul st4					; [ ty*cos(plyangle), tx, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fxch st1					; [ tx, ty*cos(plyangle), ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fmul st5					; [ tx*sin(plyangle), ty*cos(plyangle), ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fsub						; [ tx*sin(plyangle)-ty*cos(plyangle), ply.y, ply.x, cos(plyangle), sin(plyangle) ]
								; di = tx1/2
	fstp dword [di]				; [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	
	fld dword [di+4]			; di+4 = tz1/2, di = tx1/2
	fild word [zoomTimesAspect]
	fld dword [di]				; [ tx1, zoomTimesAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fchs						; [ -tx1, zoomTimesAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fmul						; [ -tx1*zoomTimesAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fdiv st1					; [ -tx1*zoomTimesAspect/tz1, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
								; di+8 = x1/2
	fistp word [di+8]			; [ tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	
	fild word [zoomWOAspect]	;
	fild word [wallHeight]		; [ wallHeight, zoomWOAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fld st0						; FULL [ wallHeight, wallHeight, zoomWOAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fchs						; FULL [ -wallHeight, wallHeight, zoomWOAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fmul st2					; FULL [ -wallHeight*zoomWOAspect, wallHeight, zoomWOAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fdiv st3					; FULL [ -wallHeight*zoomWOAspect/tz1, wallHeight, zoomWOAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
								; di+10 = y1top/y2top, di+12 = y1bot/y2bot
	fistp word [di+10]			; [ wallHeight, zoomWOAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fmul						; [ wallHeight*zoomWOAspect, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fdiv						; [ wallHeight*zoomWOAspect/tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fistp word [di+12]			; [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
								; FPU stack ready for next iteration

	add si, POLYGONENTRY		; move to next vertex
	add di, TEMPVARSIZE			; move to ...2 variableset
	loop calcOneVertex

	call drawOneSide			; draw side defined by x1,y1top/y1bot,x2,y2top/y2bot

	pop cx
	sub si, POLYGONENTRY		; two steps forward minus one step back = one step fwd

	loop calcOneSide
	
	jmp keyPress				; infinite loop - no return from main
	
drawOneSide:
	mov si, HORZ_CENTER			; preparing to draw side defined by x1,y1top/y1bot,x2,y2top/y2bot
	mov cx, si
	mov di, VERT_CENTER
	mov dx, di

	pusha						; pusha instead of individual push's, stack memory is cheaper than instruction space
								; +1 saves si,di,cx,dx at HCENTER,VCENTER,HCENTER,VCENTER
	
	add si, [x1]
	add cx, [x2]
	pusha						; +2 saves si,di,cx,dx at HCENTER+x1,VCENTER,HCENTER+x2,VCENTER
	add di, [y1top]
	add dx, [y2top]
	call lineDraw				; TOP edge: (HCENTER+x1,VCENTER+y1top)->(HCENTER+x2,VCENTER+y2top)
	popa						; +1 restores HCENTER+x1,VCENTER,HCENTER+x2,VCENTER
	add di, [y1bot]
	add dx, [y2bot]
	call lineDraw				; BOTTOM edge: (HCENTER+x1,VCENTER+y1bot)->(HCENTER+x2,VCENTER+y2bot)
	popa						; 0 restores HCENTER,VCENTER,HCENTER,VCENTER
	
	pusha						; +1 saves HCENTER,VCENTER,HCENTER,VCENTER
	add si, [x1]
	add di, [y1top]
	mov cx, si
	add dx, [y1bot]
	call lineDraw				; LEFT edge: (HCENTER+x1,VCENTER+y1top)->(HCENTER+x1,VCENTER+y1bot)
	popa						; 0 restores HCENTER,VCENTER,HCENTER,VCENTER
	add si, [x2]
	add di, [y2top]
	mov cx, si
	add dx, [y2bot]
	call lineDraw				; RIGHT edge: (HCENTER+x2,VCENTER+y2top)->(HCENTER+x2,VCENTER+y2bot)
	
	ret							; return from drawOneSide
	
	
plotxy:							; puts pixel into (si,di) of color plotxy_color
								; assumes: es=video seg
	push di
	push ax
	shl di, 6					; mul di, 320
	mov ax, di					; ax=di=y * 64
	shl ax, 2					; ax=y * 256
	add di, ax					; di=y*(64+256)=320
	add di, si					; di=y*320+x
	mov al, [plotxy_color]
	stosb
	pop ax
	pop di
	ret							; return from plotxy, all registers preserved

; lineDraw octants, e.g. octant1 is when (fromx < tox, fromy < toy, and the line is more horizontal then vertical, i.e. |tox-fromx| > |toy-fromy|
;      octant2
; \  |  /
;  \ | /
;   \|/ octant1
; ---|---
;   /|\ octant8
;  / | \
; /  |  \

lineDraw:						; draws line from (si,di) to (cx,dx) with color plotxy_color

	; lineDraw_part1: this part here checks whether |tox-fromx|<=>|toy-fromy| and sets dh so (0 iff "horizontal dominant",
	; 1 iff "vertical dominant"), then swaps source and destination points if dominant axis coords are not 
	; in order, (e.g. if |tox < fromx| >= |toy-fromy| (horiz. dom.) and tox < fromx, (cx, dx) <-> (si, di)),
	; futhermore sets whether the other (submissive) axis will be increasing (1) or decreasing (-1) after all that
	; AH: 0 iff horiz dominant, 1 iff vert; AL: +1 or -1 submissive direction inc/dec
	
	mov bp, dx					; BP will be |toy-fromy| later, first step isolated to save space
	
	mov bx, cx
	sub bx, si					; BX = tox-fromx
	jge XinOrder
	neg bx						; BX = |tox-fromx|
	sub bp, di					; was BP=toy already, BP = toy-fromy now
	jge XnotInOrder_YinOrder
	neg bp						; BP = |toy-fromy|
	
	; case 1: X not in order, Y not in order, swapping will be inevitable
	cmp bx, bp
	setc ah						; CPU80386 AH=0 iff dominant direction is horizontal
	mov al, 1					; AL=1: having swapped source<->dest, submissive direction will be increasing
	xchg cx, si					; if X dom, swap source <-> destination
	xchg dx, di
	jmp lineDraw_part2
	
XnotInOrder_YinOrder:

	; case 2: X not in order, but Y is, swapping will be necessary iif X is dominant
	mov ax, 0x01FF				; Y dominant case will be default: AH=1, AL=-1, no src<->dst swapping
	cmp bx, bp
	jc lineDraw_part2			; if Y dominant, leave the defaults
	xchg cx, si					; if X dom, swap source <-> destination
	xchg dx, di
	dec ah						; AH=0 as X is dominant (submissive Y will be in wrong order after swap, so AL should stay -1)
	jmp lineDraw_part2
	
XinOrder:
	sub bp, di					; was BP=toy already, BP = toy-fromy now
	jge XinOrder_YinOrder
	neg bp						; BP = |toy-fromy|
	
	; case 3: X is in order, Y isn't, swapping wil be necessary iif Y is dominant
	mov ax, 0x00FF				; X dom. will be default: AH=0, AL=-1, no src<->dst swapping
	cmp bx, bp
	jnc lineDraw_part2			; X dominant: leave the defaults
	xchg cx, si					; Y dominant: swap source <-> destination
	xchg dx, di
	inc ah						; AH=1 as Y is dominant (submissive X will be in wrong order after swap, so AL should stay -1)
	jmp lineDraw_part2
	
XinOrder_YinOrder:

	; case 4: both X and Y are in order, no swapping will take place
	cmp bx, bp
	setc ah
	mov al, 1
	
lineDraw_part2:
	; lineDraw_part2: this is the actual drawing part; preconditions: 
	; - all input arguments intact yet (CX, DX, SI, DI)
	; - BP = |toy-fromy| =: deltay, BX = |tox-fromx| =: deltax
	; - AH = 0 iif horiz dominant, 1 iff vert dominant
	; - AL = 1 iff submissive direction should increase, -1 iff it should decrease

	test ah, ah
	jnz lineDraw_part2vert
	
	; lineDraw_part2/horizontal_dominant
	movsx dx, al				; CPU80386//DX will be used to hold the step in submissive (Y) direction
	
	shl bp, 1					; BP = 2*deltay
	mov ax, bp
	sub ax, bx					; AX = 2*deltay - deltax
	shl bx, 1					; BX = 2*deltax

lineDrawMainLoop_horzdom:
	call plotxy					; draw (SI,DI), color plotxy_color
	cmp ax,0					; check if error reached threshold
	jle skipYincNow				; if no, line won't step into submissive direction
	add di, dx					; increment/decrement current y coord if needed (delta = DX)
	sub ax, bx					; error -= 2*deltax
skipYincNow:
	add ax, bp					; error += 2*deltay
	inc si						; increment current x coord
	cmp si, cx					; have we reached tox?
	jle lineDrawMainLoop_horzdom
	
lineDraw_part2vert:
	; lineDraw_part2/vertical_dominant
	movsx cx, al				; CPU80386//CX will be used to hold the step in submissive (X) direction
	
	shl bx, 1					; BX = 2*deltax
	mov ax, bx
	sub ax, bp					; AX = 2*deltax - deltay
	shl bp, 1					; BP = 2*deltay
	
	
lineDrawMainLoop_vertdom:
	call plotxy					; draw (SI,DI), color plotxy_color
	cmp ax,0					; check if error reached threshold
	jle skipXincNow				; if no, line won't step into submissive direction
	add si, cx					; increment/decrement current x coord if needed (delta = CX)
	sub ax, bp					; error -= 2*deltay
skipXincNow:
	add ax, bx					; error += 2*deltax
	inc di						; increment current y coord
	cmp di, dx					; have we reached toy?
	jle lineDrawMainLoop_horzdom

	ret							; return from lineDraw, no registers preserved
	
	nop							; end marker

; start of static/const variables section

plyangleDelta:					
	dd 0.01
	
polygon: 
	dw 0, 1
	dw 2, 3
	dw 4, 5
	dw 6, 7
	dw 8, 9
	dw 0, 1
POLYGONENTRY equ (2 * 2)
POLYGONSIZE equ (($-polygon)/POLYGONENTRY) - 1
	
zoomTimesAspect: 
	dw 160						; aspect ratio * zoom value = 320/200 * 100, factor of X coords
zoomWOAspect:
	dw 100						; zoom value (alone), factor of Y coords
wallHeight:
	dw 50

; start of uninitialized variables section (BSS)

plotxy_color:	resb 1			; color of the pixel drawn in plotxy

plyangle:
	resd 1						; TODO should be cleared on-the-fly
plycoords:
	resd 2						; TODO should be cleared on-the-fly

tx1:	resd 1
tz1:	resd 1
x1:		resw 1
y1top:	resw 1
y1bot:	resw 1

TEMPVARSIZE	equ ($-tx1)

tx2:	resd 1
tz2:	resd 1
x2:		resw 1
y2top:	resw 1
y2bot:	resw 1

end_all:						; end of all valuable code/data, stack area will be STACK_SIZE bytes starting from here

; stats (not up-to-date)
; plotxy			00h-15h		(21)
; lineDraw			15h-9eh		(137)
; consts			9eh-c0h		(34)
; main				c0h-e4h		(36)		
; main/keyPress		e4h-144h	(96)
; projector			144h-1bfh	(123)
; drawOneSide		1bfh-210h	(81)
; total							528 bytes + 42 bytes BSS