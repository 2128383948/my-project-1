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

MainLoop:
  call readKeys
  call MaybeReset  ;check if A was pressed not yet
  call Maybechangedirection
  ;call UpdateObjects not useful func
  call WaitVBlank
  call CopyShadowOAMtoOAM
  jp MainLoop

SECTION "Functions", ROM0

Maybechangedirection:
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
  ld hl,ShadowOAM+1
  ld a,[hl]
  sub 8
  ld [hl],a
  ret
GoUp:
  ld hl,ShadowOAM
  ld a,[hl]
  sub 8
  ld [hl],a
  ret
GoRight:
  ld hl,ShadowOAM+1
  ld a,[hl]
  add 8
  ld [hl],a
  ret
GoDown:
  ld hl,ShadowOAM
  ld a,[hl]
  add 8
  ld [hl],a
  ret


MaybeReset:
  ld hl,current
  bit 0, [hl]  ; check if A was pressed
  call nz, InitializeObjects 
  ret

InitializeObjects:
  ld hl,   ShadowOAM   ; hl points to first object entry
  ld a,32
  ld [hl], a
  inc      hl          ; point to first object`s X
  ld a,32
  ld [hl], a
  inc      hl
  ld       [hl], 2   ; smiling face
  inc      hl
  inc      hl
  ; second object
  ld a,24
  ld [hl], a
  inc      hl          ; point to first object`s X
  ld a,16
  ld [hl], a
  inc      hl
  ld       [hl], 2   ; smiling face
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


;maybe not need to change

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
  ld hl, _SCRN0
  ld bc, BackgroundEnd - Background
  call CopyMemory
  ret

CopyTileDataToVRAM:
  ld de, Tiles
  ld hl, _VRAM
  ld bc, TilesEnd - Tiles
  call CopyMemory
  ret

;not change




;not useful//////////////////////////////////////////
UpdateObjects:
  ld hl,ShadowOAM
  ld b, OBJCOUNT
.update:
  push hl
  call RandomByte
  and %00000011
  call z, GoLeft
  cp 1
  call z, GoUp
  cp 2
  call z, GoRight
  cp 3
  call z, GoDown
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
; smiling face 2
  dw `33333333
  dw `30000003
  dw `33333333
  dw `30000003
  dw `30333303
  dw `30300303
  dw `30333303
  dw `33333333
;left up corrner 3
  dw `00000000
  dw `00111111
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
;left down corner 4
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00111111
  dw `00000000
;right up corner 5
  dw `00000000
  dw `11111100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
;right down corner 6
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `11111100
  dw `00000000
;left egde 7
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
  dw `00100000
;right edge 8
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
  dw `00000100
;up edge 9
  dw `00000000
  dw `11111111
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
;down edge 10
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `00000000
  dw `11111111
  dw `00000000
TilesEnd:


SECTION "Background", ROM0
Background:
DB $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$03,$09,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$07,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$07,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$07,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$07,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$04,$0A,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
BackgroundEnd:

SECTION "Variables", WRAM0
ShadowOAM: DS 160 
previous: DS 1
current: DS 1
