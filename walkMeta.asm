;999019821,Mingsong Liu
INCLUDE "hardware.inc"

DEF OBJCOUNT EQU 40
DEF METAOBJCOUNT EQU 10

SECTION "Header", ROM0[$100]
  jp EntryPoint

  ds $150 - @, 0

EntryPoint:
  call WaitVBlank
  ld a, 0
  ld [rLCDC], a

  ld a,%11111100 ; black and white palette
  ld [rOBP0], a

  call   CopyTileDataToVRAM
  ld     hl, _OAMRAM
  call   ResetOAM
  ld     hl, ShadowOAM
  call   ResetOAM
  ;call   InitializeObjects;in ShadowOAM have 40 objects with random (y,x) coordinates
  call   InitailizeMetaObjects

; LCD on, enable object layer (no background)
  ld a, LCDCF_ON | LCDCF_OBJON
  ld [rLCDC], a

MainLoop:
  ;call UpdateObjects
  call UpdateMetaObjects
  call UpdateShadowOAM
  call WaitVBlank
  call CopyShadowOAMtoOAM
  jp MainLoop

SECTION "Functions", ROM0
InitializeObjects:
  ld hl,   ShadowOAM   ; hl points to first object entry
  ld b,    OBJCOUNT
.init:
  call     RandomByte
  ld [hl], a
  inc      hl          ; point to first object's X
  call     RandomByte
  ld [hl], a
  inc      hl
  inc      hl
  inc      hl
  dec      b
  jr nz, .init
  ret

InitailizeMetaObjects:;generate (y,x) coordinates for objects from 0 to METAOBJCOUNT-1
  ld hl, metaCoord;store in metaCoord
  ld b, METAOBJCOUNT
.init:
  call RandomByte
  ld [hl], a
  inc hl
  call RandomByte
  ld [hl], a
  inc hl
  dec b
  jr nz, .init
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

UpdateShadowOAM:
  ld hl,metaCoord
  ld de,ShadowOAM
  ld b,METAOBJCOUNT
.update:
  ld a,[hl];y0
  ld [de],a
  inc hl
  inc de
  ld a,[hl];x0
  ld [de],a
  inc hl
  inc de
  ld a,1;tile id1
  ld [de],a
  inc de
  dec hl
  ld a,0
  ld [de],a
  inc de
  dec hl
  ld a,[hl];y1
  ld [de],a
  inc hl
  inc de
  ld a,[hl];x1=x0+8
  add a,8
  ld [de],a
  inc hl
  inc de
  ld a,2;tile id2
  ld [de],a
  inc de
  dec hl
  ld a,0
  ld [de],a
  inc de
  dec hl
  ld a,[hl];y2=y0+8
  add a,8
  ld [de],a
  inc hl
  inc de
  ld a,[hl];x2
  ld [de],a
  inc hl
  inc de
  ld a,3;tile id3
  ld [de],a
  inc de
  dec hl
  ld a,0
  ld [de],a
  inc de
  dec hl
  ld a,[hl];y3=y0+8
  add a,8
  ld [de],a
  inc hl
  inc de
  ld a,[hl];x3=x0+8
  add a,8
  ld [de],a
  inc hl
  inc de
  ld a,4;tile id4
  ld [de],a
  inc de
  ld a,0
  ld [de],a
  inc de
  dec b
  jr nz, .update
  ret


UpdateMetaObjects:
  ld hl,metaCoord
  ld b,METAOBJCOUNT
.update:
  push hl
  call RandomByte
  and %00000011
  call z, MGoLeft
  cp 1
  call z, MGoUp
  cp 2
  call z, MGoRight
  cp 3
  call z, MGoDown
  pop hl
  inc hl
  inc hl
  dec b
  jr nz, .update
  ret
MGoLeft:
  inc hl
  dec [hl]
  ret
MGoUp:
  dec [hl]
  ret
MGoRight:
  inc hl
  inc [hl]
  ret
MGoDown:
  inc [hl]
  ret


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

GoLeft:
  inc hl
  dec [hl]
  ret
GoUp:
  dec [hl]
  ret
GoRight:
  inc hl
  inc [hl]
  ret
GoDown:
  inc [hl]
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

WaitVBlank:
  ld a, [rLY]
  cp 144
  jr nz, WaitVBlank
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

CopyTileDataToVRAM:
  ld de, Tiles
  ld hl, _VRAM
  ld bc, TilesEnd - Tiles
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

SECTION "TilesData", ROM0
Tiles:
; Tile ID 0: smiling face
 DB %01111110,%00000000
 DB %10000001,%00000000
 DB %10100101,%00000000
 DB %10000001,%00000000
 DB %10100101,%00000000
 DB %10011001,%00000000
 DB %10000001,%00000000
 DB %01111110,%00000000
; tile id 1; big smiling face NW corner
 DB %01111111,%00000000
 DB %10000000,%00000000
 DB %10000000,%00000000
 DB %10011000,%00000000
 DB %10011000,%00000000
 DB %10000000,%00000000
 DB %10000000,%00000000
 DB %10000000,%00000000
; tile id 2; big smiling face NE corner
 DB %11111110,%00000000
 DB %00000001,%00000000
 DB %00000001,%00000000
 DB %00011001,%00000000
 DB %00011001,%00000000
 DB %00000001,%00000000
 DB %00000001,%00000000
 DB %00000001,%00000000
; tile id 3; big smiling face SE corner
 DB %10000000,%00000000
 DB %10000000,%00000000
 DB %10100000,%00000000
 DB %10010000,%00000000
 DB %10001111,%00000000
 DB %10000000,%00000000
 DB %10000000,%00000000
 DB %01111111,%00000000
; tile id 4; big smiling face SW corner
 DB %00000001,%00000000
 DB %00000001,%00000000
 DB %00000101,%00000000
 DB %00001001,%00000000
 DB %11110001,%00000000
 DB %00000001,%00000000
 DB %00000001,%00000000
 DB %11111110,%00000000
TilesEnd:

SECTION "Variables", WRAM0
metaCoord: DS 20
ShadowOAM: DS 160 
