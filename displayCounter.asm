; Name:
; ID:

INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]
  jp EntryPoint

  ds $150 - @, 0

EntryPoint:
  call WaitVBlank

  ld a, 0
  ld [rLCDC], a  ; turn off LCD
  ld a,%11111100 ; black and white palette
  ld [rBGP],  a  ; background palette

  call CopyCharacters
  call ResetVariables
  call ResetBG

  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000 
  ld [rLCDC], a

MainLoop:
  call readKeys
  call HandleInput
  call binToDec
  call WaitVBlank
  call copyDigitsRev
  jp MainLoop

SECTION "Functions", ROM0
;Input Handling:
;Key UP: Increment counter by 1, saturating at 255.
;Key DOWN: Decrement counter by 1, saturating at 0.
;Key RIGHT: Add 10 to counter, saturating at 255.
;Key LEFT: Subtract 10 from counter, saturating at 0.
HandleInput:
  ld hl, current
  bit 6, [hl]
  call nz, IncrementCounter
  bit 7, [hl]
  call nz, IncrementCounter
  bit 4, [hl]
  call nz, IncrementCounter
  bit 5, [hl]
  call nz, IncrementCounter
  bit 0, [hl]
  call nz, ResetCounter
  ret
IncrementCounter:
  ld a, [counter]
  cp 255
  jr z, .done
  inc a
  ld [counter], a
.done
  ret

ResetCounter:
  ld a, 0
  ld [counter], a
  ret

;not useful------------------------------------------
DecrementCounter:
  ld a, [counter]
  cp 0
  jr z, .done
  dec a
  ld [counter], a
.done
  ret
AddTen:
  ld a, [counter]
  add a, 10
  ld [counter], a
.done
  ret
SubtractTen:
  ld a, [counter]
  sub 15
  ld [counter], a
.done
  ret
;TODO
;Converts a binary number to decimal and stores it in buffer
;Input: [counter]
;Output: [buffer]
;---------------------------------------------------

binToDec:
  ld hl, buffer
  ld a, [counter]
  ld b, 10
  ld c,0;divresult
  call modulo
  ld [hl], a;first
  inc hl
  ld a,c
  ld c,0;divresult
  ld b, 10
  call modulo
  ld [hl], a;secend
  inc hl
  ld a, c
  ld c,0;divresult
  ld b, 10
  call modulo
  ld [hl], a;third
; TODO
  ret

modulo:;if a < b, then a is the remainder
  cp b
  jr c, .done
  sub b
  inc c
  jr modulo
.done:
  ret

copyDigitsRev:
  ld hl, buffer
  ld de, _SCRN0 + 19

  ld a, [hl+]
  ld [de], a
  dec de
  ld a, [hl+]
  ld [de], a
  dec de
  ld a, [hl+]
  ld [de], a
; TODO
  ret

ResetVariables:
  ld a,0
  ld [previous],a
  ld [current],a
  ld [counter],a
  ret

ResetBG:
  ld  bc,32*32
  ld hl, _SCRN0
.loop
  ld [hl],10 ; ID of blank tile
  inc hl
  dec bc
  ld a,b
  or c
  jr nz, .loop
  ret

WaitVBlank:
  ld a, [rLY]
  cp 144
  jr nz, WaitVBlank
  ret

CopyCharacters:
; from the Tetris source code
  ld hl, Tiles
  ld bc, TilesEnd - Tiles
  ld de, _VRAM
.loop:
  ldi a, [hl]
  ld [de], a
  inc de           ; copy each byte twice into _VRAM
  ld [de], a       ; because characters are stored as only black and white
  inc de           ; but the GB uses two bytes per character to allow for 4 colors
  dec bc
  ld a, b
  or c
  jr nz, .loop
  ret

readKeys:
;bit 7:dowm, 6:up, 5:left, 4:right, 3:start, 2:select, 1:B, 0:A
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
  xor   b	      ; result will be 0 if it's the same as current read
  and   b	      ; keep buttons that were pressed during this read only
  ld    [current],a   ; store result in "current" variable and c register
  ld    c,a
  ld    a,b           ; current state will be previous in next read
  ld    [previous],a

  ld    a,$30         ; reset rP1
  ldh   [rP1],a
  ret



SECTION "TilesData", ROM0
Tiles:
 DB $00,$3C,$66,$66,$66,$66,$3C,$00 ; 0
 DB $00,$18,$38,$18,$18,$18,$3C,$00 ; 1
 DB $00,$3C,$4E,$0E,$3C,$70,$7E,$00
 DB $00,$7C,$0E,$3C,$0E,$0E,$7C,$00
 DB $00,$3C,$6C,$4C,$4E,$7E,$0C,$00
 DB $00,$7C,$60,$7C,$0E,$4E,$3C,$00
 DB $00,$3C,$60,$7C,$66,$66,$3C,$00
 DB $00,$7E,$06,$0C,$18,$38,$38,$00
 DB $00,$3C,$4E,$3C,$4E,$4E,$3C,$00 ; 8
 DB $00,$3C,$4E,$4E,$3E,$0E,$3C,$00 ; 9
 DB 0,0,0,0,0,0,0,0                 ; blank
TilesEnd:

SECTION "Variables", WRAM0
previous: DS 1
current: DS 1
counter: DS 1
buffer: DS 3
