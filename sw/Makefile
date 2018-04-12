all: test.gb

clean:
	rm -f *.gb

%.gb : %.o
	rgblink -o $@ $<
	rgbfix -t "Test Game" -v $@

%.o : %.asm
	rgbasm -o $@ $<
