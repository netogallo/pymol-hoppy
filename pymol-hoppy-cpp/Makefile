build/Makefile:
        mkdir build/
	cmake -B build/
build/libHsPymolCpp.so: build/Makefile
	make -C build/

all: build/libHsPymolCpp.so

clean:
	rm -rf build/

install: all
	install -t "$(libdir)" build/libHsPymolCpp.so
