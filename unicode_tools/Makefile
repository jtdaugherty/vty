all: wide.h nonspacing.h emoji.h extended_pictographic.h

common.o: common.h

gen_wide: common.o

wide.h: gen_wide data/EastAsianWidth.txt
	./gen_wide | fmt -s > wide.h

gen_nonspacing: common.o

nonspacing.h: gen_nonspacing data/UnicodeData.txt
	./gen_nonspacing | fmt -s > nonspacing.h

gen_emoji: common.o

emoji.h: gen_emoji data/emoji-data.txt
	./gen_emoji | fmt -s > emoji.h

gen_extended_pictographic: common.o

extended_pictographic.h: gen_extended_pictographic data/emoji-data.txt
	./gen_extended_pictographic | fmt -s > extended_pictographic.h

.PHONY: clean
clean:
	rm -f *.o \
		gen_wide wide.h \
		gen_nonspacing nonspacing.h \
		gen_emoji emoji.h \
		gen_extended_pictographic extended_pictographic.h
