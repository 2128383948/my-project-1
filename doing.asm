; Random Walk example
; with input (A button resets the state)
; and background (just shows a border)

INCLUDE "hardware.inc"
;INCLUDE "data.inc"
DEF OBJCOUNT EQU 2

SECTION "Header", ROM0[$100]
  jp EntryPoint

  ds $150 - @, 0

EntryPoint:
  call WaitVBlank
  ld a, 0
  ld [rLCDC], a

  ;ld a,%11111100 ; black and white palette
  ld a,%11100100 ;4color
  ld [rOBP0], a
  ld [rBGP],  a

  call   CopyTileDataToVRAM
  call   CopyBGToVRAM
  ld     hl, _OAMRAM
  call   ResetOAM
  ld     hl, ShadowOAM
  call   ResetOAM

  call   InitializeObjects  ;print the objects to the screen

; LCD on, enable object layer (no background)
  ld a, LCDCF_ON | LCDCF_OBJON | LCDCF_BGON | LCDCF_BG8000
  ld [rLCDC], a

MainLoop:;--------------------------------------------------------------------------------
  call readKeys
  call MaybeReset  ;check if A was pressed not yet
  call selectobjchangedirection
  ;call selectobjwaspressed
  call WaitVBlank
  call CopyShadowOAMtoOAM
  jp MainLoop

SECTION "Functions", ROM0;------------------------------------------------------------

selectobjwaspressed:
  ld hl,current
  bit 2, [hl]  ; check if select was pressed
  call nz, .check
  ret
.check:
  ret



selectobjchangedirection:
  ld hl,current
  bit 5, [hl]  ; check if left was pressed
  call nz, GoLeft
  ld hl,current
  bit 4, [hl]  ; check if right was pressed
  call nz, GoRight
  ld hl,current
  bit 6, [hl]  ; check if up was pressed
  call nz, GoUp
  ld hl,current
  bit 7, [hl]  ; check if down was pressed
  call nz, GoDown
  ret

GoLeft:
  ld a,[ShadowOAM];y
  sub 16;fist 16 must sub ,get y in the background
  ld c,a
  ld a,[ShadowOAM+1];x
  sub 8+16;fist 8 must sub ,16 check left wall
  ld b,a
  call GetTileByPixel
  ld a,[hl]
  cp $01;wall
  ret z;if is wall ,return
;not wall
  ld hl,ShadowOAM+1
  ld a,[hl]
  sub 24
  ld [hl],a
  ret
GoUp:
  ld a,[ShadowOAM];y
  sub 16+16;fist 16 must sub ,secend 16 check up wall
  ld c,a
  ld a,[ShadowOAM+1];x
  sub 8;fist 8 must sub
  ld b,a
  call GetTileByPixel
  ld a,[hl]
  cp $01;wall
  ret z;if is wall ,return
;not wall
  ld hl,ShadowOAM
  ld a,[hl]
  sub 24
  ld [hl],a
  ret
GoRight:
  ld a,[ShadowOAM];y
  sub 16;fist 16 must sub ,get y in the background
  ld c,a
  ld a,[ShadowOAM+1];x
  add 8;=sub 8-16
  ld b,a
  call GetTileByPixel
  ld a,[hl]
  cp $01;wall
  ret z;if is wall ,return
;not wall
  ld hl,ShadowOAM+1
  ld a,[hl]
  add 24
  ld [hl],a
  ret
GoDown:
  ld a,[ShadowOAM];y
  ;sub 16-16fist 16 must sub ,secend 16 check up wall
  ld c,a
  ld a,[ShadowOAM+1];x
  sub 8;fist 8 must sub
  ld b,a
  call GetTileByPixel
  ld a,[hl]
  cp $01;wall
  ret z;if is wall ,return
  cp $02;full
  ret z;if is full ,return
;not wall
  ld hl,ShadowOAM
  ld a,[hl]
  add 24
  ld [hl],a
  ret


MaybeReset:
  ld hl,current
  bit 0, [hl]  ; check if A was pressed
  call nz, InitializeObjects 
  ret

;y=16,x=8 is (0,0) in the screen
;y=24,x=16 is (1,1) in the screen
InitializeObjects:
  ld hl,   ShadowOAM   ; hl points to first object entry
  ld a,32
  ld [hl], a
  inc      hl          ; point to first object`s X
  ld a,24
  ld [hl], a
  inc      hl
  ld       [hl], 2   ; smiling face
  inc      hl
  inc      hl
  ; second object
  ld a,32+24*4
  ld [hl], a
  inc      hl          ; point to first object`s X
  ld a,24
  ld [hl], a
  inc      hl
  ld       [hl], 0   ; empty
  inc      hl
  ld       [hl], %10000000 ;under the background
  ret


;maybe not need to change--------------------------------------------


; Convert a pixel position to a tilemap address
; hl = $9800 + X + Y * 32，背景的宽是32个瓷砖，所以Y轴的位置需要乘以32
; @param b: X
; @param c: Y
; @return hl: tile address
GetTileByPixel:;将像素位置转换为瓷砖地图地址
	; First, we need to divide by 8 to convert a pixel position to a tile position.
	; After this we want to multiply the Y position by 32.
	; These operations effectively cancel out so we only need to mask the Y value.
	ld a, c
	and a, %11111000;将Y轴位置的低三位清零相当于向下取整到最近的 8 的倍数。例如，Y = 10 会变为 8。
	ld l, a
	ld h, 0
	; Now we have the position * 8 in hl
	add hl, hl ; position * 16
	add hl, hl ; position * 32
	; Just add the X position and offset to the tilemap, and we're done.
	ld a, b
	srl a ; a / 2
	srl a ; a / 4
	srl a ; a / 8
	add a, l
	ld l, a
	adc a, h
	sub a, l
	ld h, a
	ld bc, $9800
	add hl, bc
	ret



ResetOAM:
; input: HL: location of OAM or Shadow OAM
  ld b,40*4
  ld a,0
.loop:
  ld [hl],a
  inc hl
  dec b
  jr nz,.loop
  ret

CopyShadowOAMtoOAM:
  ld hl, ShadowOAM
  ld de, _OAMRAM
  ld b, OBJCOUNT
.loop:
  ld a,[hl+]
  ld [de],a
  inc e
  ld a,[hl+]
  ld [de],a
  inc e
  ld a,[hl+]
  ld [de],a
  inc e
  ld a,[hl+]
  ld [de],a
  inc e
  dec b
  jr nz, .loop
  ret


;bit:7 down, 6 up, 5 left, 4 right, 3 start, 2 select, 1 B, 0 A
;---------------------------------------------------------------------
readKeys:
;---------------------------------------------------------------------
; Output:
; b : raw state:   pressing key triggers given action continuously
;                  as long as it is pressed
; c : rising edge: pressing key triggers given action only once,
;                  key must be released and pressed again
  ld    a,$20
  ldh   [rP1],a   
  ldh   a,[rP1]
  ldh   a,[rP1]
  cpl
  and   $0f         ; lower nibble has down, up, left, right
  swap	a           ; becomes high nibble
  ld	b,a
  ld    a,$10
  ldh   [rP1],a
  ldh   a,[rP1]
  ldh   a,[rP1]
  ldh   a,[rP1]
  ldh   a,[rP1]
  ldh   a,[rP1]
  ldh   a,[rP1]
  cpl
  and   $0f         ; lower nibble has start, select, B, A
  or    b
  ld    b,a

  ld    a,[previous]  ; load previous state
  xor   b	      ; result will be 0 if it`s the same as current read
  and   b	      ; keep buttons that were pressed during this read only
  ld    [current],a   ; store result in "current" variable and c register
  ld    c,a
  ld    a,b           ; current state will be previous in next read
  ld    [previous],a

  ld    a,$30         ; reset rP1
  ldh   [rP1],a
  ret


WaitVBlank:
  ld a, [rLY]
  cp 144
  jr nz, WaitVBlank
  ret


CopyMemory:
; input:
; de : source
; hl : destination
; bc : how many bytes
.copy:
  ld a,[de]
  inc de
  ld [hl],a
  inc hl
  dec bc
  ld a,b
  or c
  jr nz, .copy
  ret

CopyBGToVRAM:
  ld de, Background
  ld hl, $9800;_SCRN0
  ld bc, BackgroundEnd - Background
  call CopyMemory
  ret

CopyTileDataToVRAM:
  ld de, Tiles
  ld hl, _VRAM
  ld bc, TilesEnd - Tiles
  call CopyMemory
  ret

;not change-----------------------------------------------------------




;not useful//////////////////////////////////////////
UpdateObjects:
  ld hl,ShadowOAM
  ld b, OBJCOUNT
.update:
  push hl
  call RandomByte
  and %00000011
  ;call z, GoLeft
  cp 1
  ;call z, GoUp
  cp 2
  ;call z, GoRight
  cp 3
  ;call z, GoDown
  pop hl
  inc hl
  inc hl
  inc hl
  inc hl
  dec b
  jr nz, .update
  ret

RandomByte:
  ld a,[rDIV]
  xor b
  xor c
  xor d
  xor e
  xor h
  xor l
  xor [hl]
  ret

InitializeObjects1:;pre
  ld hl,   ShadowOAM   ; hl points to first object entry
  ld b,    OBJCOUNT
.init:
  ld a,8
  ld [hl], a
  inc      hl          ; point to first object`s X
  ld a,8
  ld [hl], a
  inc      hl
  ld       [hl], 2   ; smiling face
  inc      hl
  inc      hl
  dec      b
  jr nz, .init
  ret
;not useful function///////////////////////////////////////////////




;will in data.inc
SECTION "TilesData", ROM0
Tiles:
; empty 0
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
; wall 1
  dw `33333333
  dw `33322233
  dw `33322233
  dw `33322233
  dw `33322233
  dw `33322233
  dw `33322233
  dw `33333333
; full 2
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
  dw `33333333
; left up corner 3
  dw `00000000
  dw `00333333
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
; left down corner 4
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00333333
  dw `00000000
; right up corner 5
  dw `00000000
  dw `33333300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
; right down corner 6
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `33333300
  dw `00000000
; left edge 7
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
  dw `00300000
; right edge 8
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
  dw `00000300
; up edge 9
  dw `00000000
  dw `33333333
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
; down edge 10
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `33333333
  dw `00000000
; 11 A
  dw `00000000
  dw `00333300
  dw `03003330
  dw `03003330
  dw `03333330
  dw `03003330
  dw `03003330
  dw `00000000
; 12 B;bing
  dw `00000000
  dw `03333300
  dw `03003330
  dw `03333300
  dw `03003330
  dw `03003330
  dw `03333300
  dw `00000000
; 13 C;caocao
  dw `00000000
  dw `00333300
  dw `03003330
  dw `03000000
  dw `03000000
  dw `03003330
  dw `00333300
  dw `00000000
; 14 D
  dw `00000000
  dw `03333300
  dw `03003330
  dw `03003330
  dw `03003330
  dw `03003330
  dw `03333300
  dw `00000000
; 15 E
  dw `00000000
  dw `03333330
  dw `03000000
  dw `03333300
  dw `03000000
  dw `03000000
  dw `03333330
  dw `00000000
; 16 F;zhangfei
  dw `00000000
  dw `03333330
  dw `03000000
  dw `03000000
  dw `03333300
  dw `03000000
  dw `03000000
  dw `00000000
; 17 G;guanyu
  dw `00000000
  dw `00333300
  dw `03003330
  dw `03000000
  dw `03003330
  dw `03003330
  dw `00333330
  dw `00000000
; 18 H;huangzhong
  dw `00000000
  dw `03000330
  dw `03000330
  dw `03333330
  dw `03000330
  dw `03000330
  dw `03000330
  dw `00000000
; 19 I
  dw `00000000
  dw `00333300
  dw `00033000
  dw `00033000
  dw `00033000
  dw `00033000
  dw `00333300
  dw `00000000
; 20 J
  dw `00000000
  dw `00033330
  dw `00003300
  dw `00003300
  dw `03003300
  dw `03003300
  dw `00333000
  dw `00000000
; 21 K
  dw `00000000
  dw `03003330
  dw `03003300
  dw `03033000
  dw `03033000
  dw `03003300
  dw `03003330
  dw `00000000
; 22 L
  dw `00000000
  dw `03000000
  dw `03000000
  dw `03000000
  dw `03000000
  dw `03000000
  dw `03333330
  dw `00000000
; 23 M
  dw `00000000
  dw `03000330
  dw `03303330
  dw `03333330
  dw `03030330
  dw `03000330
  dw `03000330
  dw `00000000
; 24 N
  dw `00000000
  dw `03000330
  dw `03003330
  dw `03033330
  dw `03033330
  dw `03003330
  dw `03000330
  dw `00000000
; 25 O
  dw `00000000
  dw `00333300
  dw `03003330
  dw `03003330
  dw `03003330
  dw `03003330
  dw `00333300
  dw `00000000
; 26 P
  dw `00000000
  dw `03333300
  dw `03003330
  dw `03003330
  dw `03333300
  dw `03000000
  dw `03000000
  dw `00000000
; 27 Q
  dw `00000000
  dw `00333300
  dw `03000330
  dw `03000330
  dw `03030330
  dw `03003300
  dw `00333030
  dw `00000000
; 28 R
  dw `00000000
  dw `03333300
  dw `03003330
  dw `03003330
  dw `03333300
  dw `03003300
  dw `03003330
  dw `00000000
; 29 S
  dw `00000000
  dw `00333300
  dw `03000000
  dw `00333300
  dw `00003330
  dw `03003330
  dw `00333300
  dw `00000000
; 30 T
  dw `00000000
  dw `03333330
  dw `00033000
  dw `00033000
  dw `00033000
  dw `00033000
  dw `00033000
  dw `00000000
; 31 U
  dw `00000000
  dw `03000330
  dw `03000330
  dw `03000330
  dw `03000330
  dw `03003330
  dw `00333300
  dw `00000000
; 32 V
  dw `00000000
  dw `03000330
  dw `03000330
  dw `03000330
  dw `03000330
  dw `00333000
  dw `00033000
  dw `00000000
; 33 W
  dw `00000000
  dw `03000330
  dw `03000330
  dw `03030330
  dw `03333330
  dw `03303330
  dw `03000330
  dw `00000000
; 34 X
  dw `00000000
  dw `03000330
  dw `00333000
  dw `00033000
  dw `00333000
  dw `03000330
  dw `03000330
  dw `00000000
; 35 Y
  dw `00000000
  dw `03003330
  dw `03003330
  dw `00333300
  dw `00033000
  dw `00033000
  dw `00033000
  dw `00000000
; 36 Z;zhaoyun
  dw `00000000
  dw `03333330
  dw `00003330
  dw `00033300
  dw `00333000
  dw `03330000
  dw `03333330
  dw `00000000
; .
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00330000
  dw `00330000
  dw `00000000
; -
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00333300
  dw `00333300
  dw `00000000
  dw `00000000
  dw `00000000
TilesEnd:


SECTION "Background", ROM0
Background:
DB 01,01,01,01,01,01,01,01,01,01,01,01,01,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,03,09,05,03,09,09,09,09,05,03,09,05,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,16,08,07,13,00,00,13,08,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,00,08,07,00,00,00,00,08,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,00,08,07,00,00,00,00,08,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,16,08,07,13,00,00,13,08,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,04,10,06,04,10,10,10,10,06,04,10,06,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,03,09,05,03,09,09,09,09,05,03,09,05,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,00,08,07,00,00,00,00,08,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,00,08,04,10,10,10,10,06,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,00,08,03,09,05,03,09,05,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,00,08,07,12,08,07,12,08,07,00,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,04,10,06,04,10,06,04,10,06,04,10,06,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,03,09,05,00,00,00,00,00,00,03,09,05,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,07,12,08,00,00,00,00,00,00,07,12,08,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,04,10,06,00,00,00,00,00,00,04,10,06,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 01,01,01,01,02,02,02,02,02,02,01,01,01,01,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
DB 02,02,02,02,02,02,02,02,02,02,02,02,02,02,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
BackgroundEnd:

SECTION "Variables", WRAM0
ShadowOAM: DS 160 
previous: DS 1
current: DS 1
caocao: DS 20
