cpu 386
; use16

; lineDraw octants, e.g. octant1 is when (fromx < tox, fromy < toy, and the line is more horizontal then vertical, i.e. |tox-fromx| > |toy-fromy|
;      octant2
; \  |  /
;  \ | /
;   \|/ octant1
; ---|---
;   /|\ octant8
;  / | \
; /  |  \

plotxy_color:	db 5			; color of the pixel drawn in plotxy

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
	mov [es:di], al
	pop ax
	pop di
	ret

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
	
	ret
	
plyangle:
	dd 0
plycoords:
	dd 0, 0

polygon: 
	dw 0, 1
	dw 2, 3
	dw 4, 5
	dw 6, 7
	dw 8, 9
	dw 0, 1
POLYGONENTRY equ (2 * 2)
POLYGONSIZE equ (($-polygon)/POLYGONENTRY) - 1

	
aspectTimesZoom: 
	dw 160.0						; aspect ratio * zoom value = 320/200 * 100, factor of X coords
zoomWOAspect:
	dw 100.0						; zoom value (alone), factor of Y coords
wallHeight:
	dw 50.0
	
	
tx1:	dd 0
tz1:	dd 0
x1:		dw 0
y1top:	dw 0
y1bot:	dw 0

TEMPVARSIZE	equ ($-tx1)

tx2:	dd 0
tz2:	dd 0
x2:		dw 0
y2top:	dw 0
y2bot:	dw 0

	
projector:
	fld dword [plyangle]		; 
	fsincos						; [ cos(plyangle), sin(plyangle) ]
	fld dword [plycoords]		;
	fld dword [plycoords+4]		; [ ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	lea si, [polygon]			;
	mov cx, POLYGONSIZE
	
calcOneSide:
	lea di, [tx1]				; set to ....1 variableset
	push cx						; save cx for outer loop
	mov cx, 2					; two vertices at a time
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
	fild word [aspectTimesZoom]
	fld dword [di]				; [ tx1, aspectTimesZoom, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fchs						; [ -tx1, aspectTimesZoom, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fmul						; [ -tx1*aspectTimesZoom, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
	fdiv st1					; [ -tx1*aspectTimesZoom/tz1, tz1, ply.y, ply.x, cos(plyangle), sin(plyangle) ]
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
	
	pop cx
	sub si, POLYGONENTRY		; two steps forward minus one step back = one step fwd
	loop calcOneSide
	
main:
	mov ax, 0A000h
	mov es, ax
	mov ss, ax
	mov sp, 320*200
	mov ax, 13h
	int 10h						; VGA Mode 13h set
	mov dx, 250					; toy
	mov cx, 510					; tox
	mov di, 210					; fromy
	mov si, 330					; fromx
	call lineDraw

	nop							; end
	
; stats
; plotxy			01h-18h		(23)
; lineDraw			18h-a1h		(137)
; projector vars	a1h-e7h		(70)
; projector			e7h-15eh	(119)
; main				15eh-17ch	(30)
; total							380 bytes
; total			0h-0bfh		(191)