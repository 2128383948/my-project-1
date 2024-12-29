clean:
	rm *.o *.gb
dist:
	zip -r project.zip *.asm Readme.md Makefile
