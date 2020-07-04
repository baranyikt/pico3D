plotxy:                         ; puts pixel into (si,di) of color al
                                ; assumes: es=video seg
    push di
    push dx
    shl di, 6                   ; mul di, 320
    mov dx, di                  ; dx=di=y * 64
    shl dx, 2                   ; dx=y * 256
    add di, dx                  ; di=y*(64+256)=320
    add di, si                  ; di=y*320+x
    mov [es:di], al
    pop dx
    pop di
    ret                         ; plotxy: 0x1e-0x00 bytes (30)

lineDraw:                       ; draws line from (si,di) to (cx,dx)
    mov ax, dx
    sub ax, di                  ; ax = |toy-fromy|
    mov bx, cx
    sub bx, si                  ; bx = |tox-fromx|
    shl ax, 1                   ; ax = 2*deltay
    mov bp, ax
    sub bp, bx                  ; bp = 2*deltay - deltax
    shl bx, 1                   ; bx = 2*deltax

ld2:
    push ax
    mov al, 5
    call plotxy
    pop ax
    cmp bp,0
    jle noyinc
    inc di
    sub bp, bx
noyinc:
    add bp, ax
    cmp si, cx
    jbe ld2

    ret                         ; lineDraw: 0x55-0x1e bytes (55) 
main:
    mov ax, 0A000h
    mov es, ax
    mov ss, ax
    mov sp, 320*200
    mov ax, 13h
    int 10h                     ; VGA Mode 13h set
    mov dx, 250     ; toy
    mov cx, 510     ; tox
    mov di, 210     ; fromy
    mov si, 330     ; fromx
    call lineDraw

