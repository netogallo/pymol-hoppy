builddir:
	mkdir -p build

build/Makefile: builddir
	cmake -B build/

build/libHsPymolCpp.so: build/Makefile
	make -C build/

all: build/libHsPymolCpp.so

clean:
	rm -f build/

install: all
	install -t "$(libdir)" build/libHsPymolCpp.so
