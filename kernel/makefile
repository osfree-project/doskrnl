#
# A Makefile for DOSKRNL
# (c) osFree project,
# author, date
#

PROJ      = doskrnl
TRGT      = $(PROJ).exe
DESC      = dos emulation kernel
srcfiles  = $(p)kernel$(e) $(p)entry$(e) $(p)io$(e) $(p)console$(e) $(p)serial$(e) $(p)printer$(e) $(p)dsk$(e) &
			$(p)sysclk$(e) $(p)asmsupt$(e) $(p)execrh$(e) $(p)nlssupt$(e) $(p)procsupt$(e) $(p)dosidle$(e) $(p)int2f$(e) &
			$(p)nls_hc$(e) $(p)apisupt$(e) $(p)intr$(e) $(p)irqstack$(e) $(p)chario$(e) $(p)systime$(e) &
			$(p)error$(e) $(p)break$(e) $(p)dosfns$(e) $(p)os2config$(e) $(p)os2fcbfns$(e) &
			$(p)inthndlr$(e) $(p)ioctl$(e) $(p)memmgr$(e) $(p)task$(e) $(p)newstuff$(e) $(p)nls$(e) $(p)network$(e) &
			$(p)prf$(e) $(p)misc$(e) $(p)strings$(e) $(p)syspack$(e) $(p)lfnapi$(e) $(p)os2blockio$(e) &
			$(p)main$(e) $(p)initoem$(e) $(p)inithma$(e) $(p)dyninit$(e) $(p)iprf$(e) $(p)initdisk$(e) &
			$(p)initclk$(e) $(p)floppy$(e) $(p)rdpcclk$(e) $(p)wrpcclk$(e) $(p)wratclk$(e) $(p)os2fs$(e) $(p)os2dir$(e)

#$(p)cpu$(e) $(p)iasmsupt$(e) $(p)memdisk$(e) 

# FAT support not required for DOSKRNL
# $(p)fatdir$(e) $(p)fatfs$(e) $(p)fattab$(e)

# no CONFIG.SYS parser for DOSKRNL
#$(p)config$(e) 

# FCB functions routed to SVC calls in DOSKRNL
#$(p)fcbfns$(e)

# Buffered IO not supported by DOSKRNL
#$(p)blockio$(e)

NOLIBS	= 1
JWASM	= 1
ADD_ASMOPT = -I=$(MYDIR)..$(SEP)hdr
ADD_COPT = -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntHMA_TEXT

#INITCFLAGS=$(ALLCFLAGS)-ntINIT_TEXT-gTGROUP-ndI

# additions to install target
INSTALL_ADD = 1

!include $(%ROOT)tools/mk/appsdos.mk

.asm: $(MYDIR)..$(SEP)drivers

#$(PATH)doskrnl: $(PATH)doskrnl$(SEP)kernel$(SEP)doskrnl.exe

install_add: $(DEST)$(SEP)$(PROJ)

$(DEST)$(SEP)$(PROJ): $(PATH)$(PROJ).exe
 @$(SAY) INST     $^. $(LOG)
 $(BLD)host$(SEP)$(%HOST)$(SEP)$(RELDIR)..$(SEP)utils$(SEP)exeflat.exe $< $^@ 0x60

$(PATH)os2config.$(O): $(MYDIR)os2config.c
 @$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=D:\osfree\DOS\doskrnl\kernel\ -i=D:\osfree\DOS\doskrnl\kernel\.. -i=D:\osfree\build\DOS\doskrnl\kernel\ -i=D:\osfree\build\DOS\doskrnl\kernel\.. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-gTGROUP-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)initclk.$(O): $(MYDIR)initclk.c
 @$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=D:\osfree\DOS\doskrnl\kernel\ -i=D:\osfree\DOS\doskrnl\kernel\.. -i=D:\osfree\build\DOS\doskrnl\kernel\ -i=D:\osfree\build\DOS\doskrnl\kernel\.. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-gTGROUP-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)initoem.$(O): $(MYDIR)initoem.c
 @$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=D:\osfree\DOS\doskrnl\kernel\ -i=D:\osfree\DOS\doskrnl\kernel\.. -i=D:\osfree\build\DOS\doskrnl\kernel\ -i=D:\osfree\build\DOS\doskrnl\kernel\.. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-gTGROUP-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)main.$(O): $(MYDIR)main.c
 @$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=D:\osfree\DOS\doskrnl\kernel\ -i=D:\osfree\DOS\doskrnl\kernel\.. -i=D:\osfree\build\DOS\doskrnl\kernel\ -i=D:\osfree\build\DOS\doskrnl\kernel\.. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-gTGROUP-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)inithma.$(O): $(MYDIR)inithma.c
 @$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=D:\osfree\DOS\doskrnl\kernel\ -i=D:\osfree\DOS\doskrnl\kernel\.. -i=D:\osfree\build\DOS\doskrnl\kernel\ -i=D:\osfree\build\DOS\doskrnl\kernel\.. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-gTGROUP-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)dyninit.$(O): $(MYDIR)dyninit.c
 @$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=D:\osfree\DOS\doskrnl\kernel\ -i=D:\osfree\DOS\doskrnl\kernel\.. -i=D:\osfree\build\DOS\doskrnl\kernel\ -i=D:\osfree\build\DOS\doskrnl\kernel\.. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-gTGROUP-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)

$(PATH)initdisk.$(O): $(MYDIR)initdisk.c
 @$(SAY) CC       $^. $(LOG)
 $(verbose)$(CC) -zq -q -d__WATCOM__ -d__OSFREE__ -q -i=D:\osfree\DOS\doskrnl\kernel\ -i=D:\osfree\DOS\doskrnl\kernel\.. -i=D:\osfree\build\DOS\doskrnl\kernel\ -i=D:\osfree\build\DOS\doskrnl\kernel\.. -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntINIT_TEXT-gTGROUP-ndI -bt=dos -fr=$^*.err -fo=$^@ $[@ $(LOG)


