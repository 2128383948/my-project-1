using "./run *.asm"to compile
(use 

sudo apt-get install dos2unix

dos2unix run.sh

to fix run.sh if you need)

will working in doingFSM.asm!!!!!!!!!!!!!!!!!!!

displayCounter：计步器，0~255,can reset

tutorialbricks:have some functions about collision



  ;hl+32
  ld a,32
  add l
  ld l,a
  adc h
  sub l
  ld h,a




  ld [hl],0;(2,2)
  dec hl
  ld [hl],0;(2,1)
  dec hl
  ld [hl],0;(2,0)


  ;hl-64
  ld a, l        ; 将 L 的值加载到 A
  sub 64         ;-64
  ld l, a        ; 将结果存回 L
  ld a, h        ; 将 H 的值加载到 A
  sbc 0          ; A = H - 借位
  ld h, a 




  ld [hl],0;(0,0)
  inc hl
  ld [hl],0;(0,1)
  inc hl
  ld [hl],0;(0,2)