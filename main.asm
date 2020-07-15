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
; plotxy		01h-18h		(23)
; lineDraw		18h-a1h		(137)
; total			0h-0bfh		(191)