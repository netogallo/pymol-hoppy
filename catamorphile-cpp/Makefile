.PHONY: all

OS=$(shell ghc -e ":m +System.Info" -e "putStrLn os")

CC = g++
CFLAGS = -O2 -I/home/neto/code/pymol-open-source/layer0 -I/home/neto/code/pymol-open-source/layer1 -I/home/neto/code/pymol-open-source/layer2 -I/home/neto/code/pymol-open-source/layer3 -I/home/neto/code/pymol-open-source/layer4 -I/home/neto/code/pymol-open-source/layer5 -I/home/neto/code/pymol-open-source/ov/src -I/usr/include/python3.10/ -I/home/neto/code/pymol-open-source/include/ -I/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/include/ -L/home/neto/code/pymol-open-source/contrib/catamorphine -lpymol

.PHONY: clean install

all: cpp/libcatamorphile.so

cpp/libcatamorphile.so: cpp/gen_Catamorphile.o cpp/Catamorphile.o cpp/gen_AtomIterators.o cpp/gen_std.o
ifeq ($(OS),darwin)
	$(CC) $(CFLAGS) -dynamic -shared -fPIC -install_name @rpath/libcatamorphile.so -o $@ $^
else
	$(CC) $(CFLAGS) -dynamic -shared -fPIC -o $@ $^
endif

%.o: %.cpp
	$(CC) $(CFLAGS) -fPIC -c -o $@ $<

clean:
	rm -f cpp/gen_Catamorphile.o cpp/Catamorphile.o cpp/gen_AtomIterators.o cpp/gen_std.o cpp/libcatamorphile.so

install:
ifeq ($(OS),darwin)
	install cpp/libcatamorphile.so "$(libdir)"
else
	install -t "$(libdir)" cpp/libcatamorphile.so
endif

