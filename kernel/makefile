#
# A Makefile for DOSKRNL
# (c) osFree project,
# author, date
#

PROJ      = doskrnl
TRGT      = $(PROJ).com
DESC      = dos emulation kernel
srcfiles  = $(p)kernel$(e) $(p)entry$(e) $(p)io$(e) $(p)console$(e) $(p)serial$(e) $(p)printer$(e) $(p)dsk$(e) &
			$(p)sysclk$(e) $(p)asmsupt$(e) $(p)execrh$(e) $(p)nlssupt$(e) $(p)procsupt$(e) $(p)dosidle$(e) $(p)int2f$(e) &
			$(p)nls_hc$(e) $(p)apisupt$(e) $(p)intr$(e) $(p)irqstack$(e) $(p)chario$(e) $(p)os2systime$(e) &
			$(p)error$(e) $(p)break$(e) $(p)dosfns$(e) $(p)os2config$(e) $(p)os2fcbfns$(e) &
			$(p)inthndlr$(e) $(p)ioctl$(e) $(p)memmgr$(e) $(p)task$(e) $(p)newstuff$(e) $(p)nls$(e) $(p)network$(e) &
			$(p)prf$(e) $(p)misc$(e) $(p)strings$(e) $(p)syspack$(e) $(p)lfnapi$(e) $(p)os2blockio$(e) &
			$(p)main$(e) $(p)initoem$(e) $(p)inithma$(e) $(p)dyninit$(e) $(p)iprf$(e) $(p)initdisk$(e) &
			$(p)initclk$(e) $(p)floppy$(e) $(p)rdpcclk$(e) $(p)wrpcclk$(e) $(p)wratclk$(e) $(p)os2fs$(e) $(p)os2dir$(e)

COM=1
#$(p)cpu$(e) $(p)iasmsupt$(e) $(p)memdisk$(e) 

# FAT support not required for DOSKRNL
# $(p)fatdir$(e) $(p)fatfs$(e) $(p)fattab$(e)

# no CONFIG.SYS parser for DOSKRNL
#$(p)config$(e) 

# FCB functions routed to SVC calls in DOSKRNL
#$(p)fcbfns$(e)

# Buffered IO not supported by DOSKRNL
#$(p)blockio$(e)

# Date  time functions routed to SVC
#$(p)systime$(e)

NOLIBS	= 1
JWASM	= 1
ADD_ASMOPT = -I=$(MYDIR)..$(SEP)hdr -DWIN31SUPPORT -DDEBUG
ADD_COPT = -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntHMA_TEXT -DWIN31SUPPORT

#INITCFLAGS=$(ALLCFLAGS)-ntINIT_TEXT-gTGROUP-ndI

# additions to install target
INSTALL_ADD = 1

!include $(%ROOT)tools/mk/appsdos.mk

.asm: $(MYDIR)..$(SEP)drivers

install_add: $(DEST)$(SEP)$(PROJ)

$(DEST)$(SEP)$(PROJ): $(DEST)$(SEP)$(PROJ).com
 $(verbose)$(SAY) INST     $^. $(LOG)
 $(verbose)$(CP) $< $^@ $(LOG)

$(PATH)os2config.$(O): $(MYDIR)os2config.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)initclk.$(O): $(MYDIR)initclk.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)initoem.$(O): $(MYDIR)initoem.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)main.$(O): $(MYDIR)main.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -dDEBUG -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)inithma.$(O): $(MYDIR)inithma.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)dyninit.$(O): $(MYDIR)dyninit.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)iprf.$(O): $(MYDIR)iprf.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)initdisk.$(O): $(MYDIR)initdisk.c
 $(verbose)$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=$(PATH) -i=$(PATH).. -i=$(MYDIR) -i=$(MYDIR).. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

