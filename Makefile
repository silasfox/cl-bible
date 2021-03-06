LISP=sbcl

run:
	$(LISP) --eval "(ql:quickload :cl-bible)" --eval "(cl-bible:start)"

build:
	$(LISP) --eval "(ql:quickload :cl-bible)" --eval "(cl-bible:build)"

install: build
	sudo cp bible /usr/local/bin/

uninstall:
	rm /usr/local/bin/bible

clean:
	rm bible

all: build
