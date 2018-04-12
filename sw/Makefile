all: test.gb

clean:
	rm -f *.gb

%.gb : %.o
	rgblink -o $@ $<

%.o : %.asm
	rgbasm -o $@ $<
