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

plotxy:                         ; puts pixel into (si,di) of color dh
                                ; assumes: es=video seg
    push di
    push ax
    shl di, 6                   ; mul di, 320
    mov ax, di                  ; ax=di=y * 64
    shl ax, 2                   ; ax=y * 256
    add di, ax                  ; di=y*(64+256)=320
    add di, si                  ; di=y*320+x
    mov [es:di], dh
    pop ax
    pop di
    ret                         ; plotxy: 0x1e-0x00 bytes (30)
                                ; changed dx to ax, al to dl: (same length)
				; changed dl to dh to give room for dl in caller (lineDraw)

lineDraw:                       ; draws line from (si,di) to (cx,dx)

	cmp cx, si		; check whether tox > fromx: handle octant3..6 cases by swapping coords from <-> to
	jae noSwapFromTo
	xchg cx, si		; if so, fromx <-> tox
	xchg dx, di		;    and fromy <-> toy
noSwapFromTo:	

    mov bp, dx
    sub bp, di                  ; bp = toy-fromy, could be negative
	lea dx, [1]		; set y coord to increasing by default (not touching flags)
	jnc yIncreasing		; if it was dx >= di case, leave it like that
	mov dx, -1		; if dx < di, set y coord to decreasing and
	neg bp			; negate bp to achieve absolute value
yIncreasing:			; bp = |toy-fromy|
    mov bx, cx
    sub bx, si                  ; bx = |tox-fromx| = tox-fromx, since we assured tox >= fromx

    shl bp, 1                   ; bp = 2*deltay
    mov ax, bp
    sub ax, bx                  ; ax = 2*deltay - deltax
    shl bx, 1                   ; bx = 2*deltax

lineDrawMainLoop:
    mov dh, 5			; set color to dh
    call plotxy			; draw (si,di), color dh
    movsx dx, dl		; reset dx=dl=Yincrement=+1 or -1
    cmp ax,0			; check if error reached threshold
    jle skipYincNow		; if no, line won't step
    add di, dx			; increment/decrement current y coord if needed
    sub ax, bx			; error -= 2*deltax
skipYincNow:
    add ax, bp			; error += 2*deltay
    inc si			; increment current x coord
    cmp si, cx			; have we reached tox?
    jbe lineDrawMainLoop

    ret                         ; lineDraw: 0x55-0x1e bytes (55) 
                                ; using the unused dx for color instead of ax: 0x51-0x1e bytes (51) 
                                ; swapping bp & ax: same length
                                ; bugfix: added inc si: +2 bytes, 0x53-0x1e (53)
main:
    mov ax, 0A000h
    mov es, ax
    mov ss, ax
    mov sp, 320*200
    mov ax, 13h
    int 10h                     ; VGA Mode 13h set
    mov dx, 250     		; toy
    mov cx, 510     		; tox
    mov di, 210     		; fromy
    mov si, 330     		; fromx
    call lineDraw

                                ; total so far 81h bytes
                                ; using dl for color & using dx in linedraw 78h bytes
				; inc si bugfix: 7ah bytes
    nop				; total so far 110 bytes (with 0x66 prefix avoided)
