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
			$(p)nls_hc$(e) $(p)apisupt$(e) $(p)intr$(e) $(p)irqstack$(e) $(p)blockio$(e) $(p)chario$(e) $(p)systime$(e) &
			$(p)error$(e) $(p)break$(e) $(p)dosfns$(e) $(p)fcbfns$(e) &
			$(p)inthndlr$(e) $(p)ioctl$(e) $(p)memmgr$(e) $(p)task$(e) $(p)newstuff$(e) $(p)nls$(e) $(p)network$(e) &
			$(p)prf$(e) $(p)misc$(e) $(p)strings$(e) $(p)syspack$(e) $(p)lfnapi$(e) &
			$(p)main$(e) $(p)config$(e) $(p)initoem$(e) $(p)inithma$(e) $(p)dyninit$(e) $(p)iprf$(e) $(p)initdisk$(e) &
			$(p)initclk$(e) $(p)floppy$(e) $(p)rdpcclk$(e) $(p)wrpcclk$(e) $(p)wratclk$(e) $(p)os2fs$(e) $(p)os2dir$(e)

#$(p)cpu$(e) $(p)iasmsupt$(e) $(p)memdisk$(e)

# FAT support not required for DOSKRNL
# $(p)fatdir$(e) $(p)fatfs$(e) $(p)fattab$(e)

NOLIBS	= 1
JWASM	= 1
ADD_ASMOPT = -I=$(MYDIR)..$(SEP)hdr
ADD_COPT = -I=$(MYDIR)..$(SEP)hdr -zq-os-s-e5-j-zl-zp1-wx-we-zgf-zff-r-ntHMA_TEXT -bt=DOS

!include $(%ROOT)tools/mk/appsdos.mk


.asm: $(MYDIR)..$(SEP)drivers

