PREFIX  = /usr/local

major   = 1.2
minor   = 2
version = $(major).$(minor)
url     = https://github.com/pmwkaa/sophia.git

all: sophia/libsophia.so.$(version)

install: $(PREFIX)/lib/libsophia.so.$(version)

$(PREFIX)/lib/libsophia.so.$(version): sophia/libsophia.so.$(version)
	cp -f $< $@
	ln -sf $@ $(PREFIX)/lib/libsophia.so.$(major)
	ln -sf $@ $(PREFIX)/lib/libsophia.so
	ldconfig

sophia/libsophia.so.$(version):
	git clone $(url)
	cd sophia; git checkout -b $(version) version_$(version)
	cd sophia; make
	cd sophia/test; make; ./run

clean:
	rm -rf sophia

.PHONY: all install clean
