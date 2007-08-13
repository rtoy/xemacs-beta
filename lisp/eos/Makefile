### Makefile --- The makefile to build EOS

## Copyright (C) 1995 Sun Microsystems, Inc.

## Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
## Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

## Keywords:	SPARCworks EOS Era on SPARCworks make makefile

### Commentary:

## Please send feedback to eduardo.pelegri-llopart@eng.sun.com

### Code:

# what emacs is called on your system
EMACS = ../../src/xemacs

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

# files that contain variables and macros that everything else depends on
CORE = sun-eos-common.el

OBJECTS = \
	sun-eos-browser.elc sun-eos-common.elc sun-eos-debugger-extra.elc  \
	sun-eos-debugger.elc sun-eos-editor.elc sun-eos-init.elc \
	sun-eos-menubar.elc sun-eos-toolbar.elc sun-eos-load.elc

SOURCES = \
	sun-eos-browser.el sun-eos-common.el sun-eos-debugger-extra.el  \
	sun-eos-debugger.el sun-eos-editor.el sun-eos-init.el \
	sun-eos-menubar.el sun-eos-toolbar.el sun-eos-load.el

all:	$(OBJECTS)

clean:
	rm -f $(OBJECTS)

sun-eos-browser.elc: sun-eos-browser.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-browser.el

sun-eos-debugger.elc: sun-eos-debugger.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-debugger.el

sun-eos-debugger-extra.elc: sun-eos-debugger-extra.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-debugger-extra.el

sun-eos-editor.elc: sun-eos-editor.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-editor.el

sun-eos-toolbar.elc: sun-eos-toolbar.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-toolbar.el

sun-eos-menubar.elc: sun-eos-menubar.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-menubar.el

sun-eos-common.elc: sun-eos-common.el
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-common.el

sun-eos-init.elc: sun-eos-init.el
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-init.el

sun-eos-load.elc: sun-eos-load.el
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile sun-eos-load.el

### Makefile ends here
