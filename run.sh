set -e
name=`basename $1 .asm`
rgbasm $1 -o $name.o
rgblink $name.o -o $name.gb
rgbfix -v -p 0xFF $name.gb
