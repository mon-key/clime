


# ==============================
# :FILE-CREATED <Timestamp: #{2011-07-28T21:34:44-04:00Z}#{11304} - by MON>
# :FILE clime/Makefile
# ==============================
#
# ==============================
# This Makefile fashioned after the following:
#
# :PASTE-NUMBER 123247: 	
# :ASTE-TITLE "quicklisp and app building"
# :PASTE-BY Xach
# :PASTE-DATE 2011-07-13
# :PASTE-URL (URL `http://paste.lisp.org/+2N3J')
#
# Thanks Xach!
# ==============================

QUICKLISP = $(HOME)/quicklisp
# DESTDIR = $(BIN_MON)
DESTDIR = /usr/local/bin
CLIME_DATE := $(shell date +%y-%m-%d)
CLIMEDIR := $(shell pwd)
CLIME_D_P_D = $(CLIMEDIR)/

manifest:
	sbcl --disable-debugger --no-userinit --no-sysinit \
              --load $(QUICKLISP)/setup.lisp \
              --eval '(push (merge-pathnames "lisp/" *default-pathname-defaults*) asdf:*central-registry*)' \
              --eval '(ql:write-asdf-manifest-file "asdf-manifest.txt")' \
              --eval '(sb-ext:quit :unix-status 0)'

all: manifest clime

clime: asdf-manifest.txt
	buildapp --manifest-file asdf-manifest.txt \
                --logfile     "clime-buildapp-log" \
	        --require     asdf \
	        --require     sb-posix \
                --load-system cl-ppcre \
                --load-system cffi \
                --load-system magicffi \
                --load-system osicat \
	        --load-path   $(CLIME_D_P_D) \
	        --load-system clime \
                --eval        '(setq clime:*IS-BUILDAPP-P* t)' \
                --entry       clime:clime-main \
		--output      clime
install:
	install -c -m 555 -S "-${CLIME_DATE}" clime ${DESTDIR}/clime

mostly-clean:
	rm -f clime clime-buildapp-log

clean:
	rm -f clime asdf-manifest.txt clime-buildapp-log





