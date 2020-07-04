lineDraw:
    cmp dx, di
    jae goodorder1
    xchg dx, di
goodorder1:
    cmp cx, si
    jae goodorder2
    xchg cx, si
goodorder2:
                                ; ensured that tox >= fromx and toy >= fromy
    push dx
    mov ax, dx
    sub ax, di                  ; ax = |toy-fromy|
    mov bx, cx
    sub bx, si                  ; bx = |tox-fromx|
    cmp ax, bx
    jae steep
    div bx

    ret
main:
    mov dx, 250     ; toy
    mov cx, 510     ; tox
    mov di, 210     ; fromy
    mov si, 330     ; fromx
    call lineDraw
