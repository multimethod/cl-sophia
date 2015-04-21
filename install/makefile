#
# Copyright (c) 2015 Andrey V. Tikhonov <multimethod@yandex.ru>
# This program is free software. It comes without any warranty, to
# the extent permitted by applicable law. You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want
# To Public License, Version 2, as published by Sam Hocevar. See
# http://www.wtfpl.net/ for more details.
#

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
