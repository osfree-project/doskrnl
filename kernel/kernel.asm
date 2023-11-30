;
; File:
;                          kernel.asm
; Description:
;                       kernel start-up code
;
;                    Copyright (c) 1995, 1996
;                       Pasquale J. Villani
;                       All Rights Reserved
;
; This file is part of DOS-C.
;
; DOS-C is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version
; 2, or (at your option) any later version.
;
; DOS-C is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
; the GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public
; License along with DOS-C; see the file COPYING.  If not,
; write to the Free Software Foundation, 675 Mass Ave,
; Cambridge, MA 02139, USA.
;
; $Id: kernel.asm 1705 2012-02-07 08:10:33Z perditionc $
;

                include segs.inc
                include stacks.inc
                include ludivmul.inc


PSP	segment 

                extern  _ReqPktPtr : far

STACK_SIZE      equ     384/2           ; stack allocated in words

;************************************************************       
; DOSKRNL BEGINS HERE, i.e. this is byte 0 of DOSKRNL (at 60:0)
;************************************************************       
;
; On entry: 
;	SS:SP initial stack (around 400 bytes)
;	SS:BP contains DOSKRNL init structure
;
;0 	2 	First free segment after DOSKRNL
;2 	2 	Size of memory - first free segment (paragraphs)
;4 	2 	Size of init area (paragraphs)
;6 	2 	Value of BREAK setting
;8 	2 	Value of DOS setting
;10 	4 	Far pointer to list of DOS DEVICE setting
;14 	4 	Far pointer to SHELL (filepath only)
;18 	4 	Far pointer to SHELL (arguments)
;22 	4 	FAR pointer to linked list of VDD
;26 	1 	Current drive (0-A, 1-B,…)
;27 	1 	Boot drive (0-A, 1-B,…)
;???? 	??? 	???? 
;
;************************************************************

initdos	struct
wMemStart	dw	?
wMemSize	dw	?
wInitSize	dw	?
wBreak		dw	?
wDOS		dw	?
pDevices	dd	?
pShell		dd	?
pShellArgs	dd	?
pVDDs		dd	?
bCurrentDrive	db	?
bBootDrive	db	?
initdos	ends

krnlstart:
entry:

;************************************************************       
; moves the INIT part of kernel.sys to high memory (~9000:0)
; then jumps there
; to aid debugging, some '123' messages are output
; this area is discardable and used as temporary PSP for the
; init sequence
;************************************************************       

                .8086                ; (keep initial entry compatible)

public realentry
realentry:                              ; execution continues here
	push cs
	pop ds
	jmp     INIT_TEXT:kernel_start


	db 0C0h - ($ - PSP) dup (090h)	; (nop opcodes) magic offset (used by exeflat)

beyond_entry   db   256-(beyond_entry-entry) dup (0)
                                        ; scratch area for data (DOS_PSP)
_master_env equ $ - 128
public _master_env

PSP	ENDS


INIT_TEXT	segment

                extern  _FreeDOSmain : near
                
                ;
                ; kernel start-up
                ;
kernel_start:

        sti		; FreeDOS kernel disables interupt to configure stack, but DOSKRNL is not.
	cld

	ifdef	DEBUG
                push ax
                push bx
                pushf              
                mov ax, 0e31h           ; '1' Tracecode - kernel entered
                mov bx, 00f0h                                        
                int 010h
                popf
                pop bx
                pop ax
	endif

	assume ds:LGROUP
	mov	bl, [bp].initdos.bBootDrive
	mov     byte ptr [LGROUP:_BootDrive],bl ; tell where we came from
	assume ds:nothing

	; Calc start of new init segment
	mov	ax, [bp].initdos.wMemSize
	sub	ax, [bp].initdos.wInitSize	; Cut init structure
	add	ax, [bp].initdos.wMemStart	; Add start segment

                mov     cl,6
                shl     ax,cl           ; convert kb to para
                mov     dx,15 + INITSIZE
                mov     cl,4
                shr     dx,cl
                sub     ax,dx
                mov     es,ax
                mov     dx,INITTEXTSIZE ; para aligned
                shr     dx,cl
                add     ax,dx

                mov     ax,cs
                mov     dx,__HMATextEnd ; para aligned
                shr     dx,cl
%ifdef WATCOM
                add     ax,dx
%endif
                mov     ds,ax
                mov     si,-2 + INITSIZE; word aligned
                lea     cx,[si+2]
                mov     di,si
                shr     cx,1
                std                     ; if there's overlap only std is safe
                rep     movsw

                                        ; move HMA_TEXT to higher memory
                sub     ax,dx
                mov     ds,ax           ; ds = HMA_TEXT
                mov     ax,es
                sub     ax,dx
                mov     es,ax           ; es = new HMA_TEXT

                mov     si,-2 + __HMATextEnd
                lea     cx,[si+2]
                mov     di,si
                shr     cx,1
                rep     movsw
                
                cld
                push    es
                mov     ax,cont
                push    ax
                retf
		
cont:           ; Now set up call frame
                mov     ds,[cs:_INIT_DGROUP]
                mov     bp,sp           ; and set up stack frame for c

		ifdef DEBUG
                push bx
                pushf              
                mov ax, 0e33h           ; '3' Tracecode - kernel entered
                mov bx, 00f0h                                        
                int 010h
                popf
                pop bx
		endif

;	mov ax, ss			; => init data segment
;	mov ds, ax			; => init data segment

           jmp     _FreeDOSmain


INIT_TEXT	ENDS

INIT_TEXT_END	SEGMENT
INIT_TEXT_END	ENDS


;************************************************************       
; KERNEL CODE AREA END
; the NUL device
;************************************************************       

CONST	segment 

                ;
                ; NUL device strategy
                ;
                public  _nul_strtgy
                extern GenStrategy: near
_nul_strtgy:
                jmp short GenStrategy

                ;
                ; NUL device interrupt
                ;
                public  _nul_intr
_nul_intr:
                push    es
                push    bx
                mov     bx,LGROUP
                mov     es,bx
                les     bx, dword ptr [es:_ReqPktPtr]  ;es:bx--> rqheadr
                cmp     byte ptr [es:bx+2],4    ;if read, set 0 read
                jne     no_nul_read
                mov     word ptr [es:bx+12h],0
no_nul_read:
                or      word ptr [es:bx+3],100h ;set "done" flag
                pop     bx
                pop     es
                retf

CONST		ENDS

_LOWTEXT	segment 

                ; low interrupt vectors 10h,13h,15h,19h,1Bh
                ; these need to be at 0070:0100 (see RBIL memory.lst)
                public _intvec_table
_intvec_table:
		db 10h
                dd 0
                ; used by int13 handler and get/set via int 2f/13h
                public  _BIOSInt13 ; BIOS provided disk handler 
                public  _UserInt13 ; actual disk handler used by kernel
                db 13h
_BIOSInt13:
		dd 0  
                db 15h
                dd 0
                ; used for cleanup on reboot
                public  _BIOSInt19
                db 19h
_BIOSInt19:
		dd 0
                db 1Bh
                dd 0
                ; default to using BIOS provided disk handler
                db 13h
_UserInt13:
		dd 0 

                ; floppy parameter table
                public _int1e_table
_int1e_table:
		db 0eh dup (0)

_LOWTEXT	ENDS

;************************************************************       
; KERNEL FIXED DATA AREA 
;************************************************************       


_FIXED_DATA	segment 

; Because of the following bytes of data, THIS MODULE MUST BE THE FIRST
; IN THE LINK SEQUENCE.  THE BYTE AT DS:0004 determines the SDA format in
; use.  A 0 indicates MS-DOS 3.X style, a 1 indicates MS-DOS 4.0-6.X style.
                public  DATASTART
DATASTART:
                public  _DATASTART
_DATASTART:
dos_data        db      0
                dw      kernel_start
                db      0               ; padding
                dw      1               ; Hardcoded MS-DOS 4.0+ style

                db (0eh - ($ - DATASTART)) dup (0)
                public  _NetBios
_NetBios        dw      0               ; NetBios Number

                db (26h - 0ch - ($ - DATASTART)) dup (0)

; Globally referenced variables - WARNING: DO NOT CHANGE ORDER
; BECAUSE THEY ARE DOCUMENTED AS UNDOCUMENTED (?) AND HAVE
; MANY MULTIPLEX PROGRAMS AND TSRs ACCESSING THEM
                public  _NetRetry
_NetRetry       dw      3               ;-000c network retry count
                public  _NetDelay
_NetDelay       dw      1               ;-000a network delay count
                public  _DskBuffer
_DskBuffer      dd      -1              ;-0008 current dos disk buffer
                public  _inputptr
_inputptr       dw      0               ;-0004 Unread con input
                public  _first_mcb
_first_mcb      dw      0               ;-0002 Start of user memory
                public  _DPBp
                public  MARK0026H
; A reference seems to indicate that this should start at offset 26h.
MARK0026H       equ     $
_DPBp           dd      0               ; 0000 First drive Parameter Block
                public  _sfthead
_sfthead        dd      0               ; 0004 System File Table head
                public  _clock
_clock          dd      0               ; 0008 CLOCK$ device
                public  _syscon
_syscon         dw      LGROUP:_con_dev,LGROUP ; 000c console device
                public  _maxsecsize
_maxsecsize     dw      512             ; 0010 maximum bytes/sector of any block device
                dd      0               ; 0012 pointer to buffers info structure
                public  _CDSp
_CDSp           dd      0               ; 0016 Current Directory Structure
                public  _FCBp
_FCBp           dd      0               ; 001a FCB table pointer
                public  _nprotfcb
_nprotfcb       dw      0               ; 001e number of protected fcbs
                public  _nblkdev
_nblkdev        db      0               ; 0020 number of block devices
                public  _lastdrive
_lastdrive      db      0               ; 0021 value of last drive
                public  _nul_dev
_nul_dev:           ; 0022 device chain root
                extern  _con_dev: near
                dw      LGROUP:_con_dev, LGROUP
                                        ; next is con_dev at init time.  
                dw      8004h           ; attributes = char device, NUL bit set
                dw      _nul_strtgy
                dw      _nul_intr
                db      'NUL     '
                public  _njoined
_njoined        db      0               ; 0034 number of joined devices
                dw      0               ; 0035 DOS 4 near pointer to special names (always zero in DOS 5) [setver precursor]
                public  _setverPtr
_setverPtr      dw      0,0             ; 0037 setver list (far pointer, set by setver driver)
                dw      0               ; 003B cs offset for fix a20
                dw      0               ; 003D psp of last umb exec
                public _LoL_nbuffers
_LoL_nbuffers   dw      1               ; 003F number of buffers
                dw      1               ; 0041 size of pre-read buffer
                public  _BootDrive
_BootDrive:
		db      1               ; 0043 drive we booted from   

                public  _CPULevel
_CPULevel       db      3               ; 0044 cpu type (MSDOS >0 indicates dword moves ok, ie 386+)
                                        ; unless compatibility issues arise FD uses
                                        ; 0=808x, 1=18x, 2=286, 3=386+
                                        ; use >= as may add checks for 486 ...
					; DOSKRNL always 386+

                dw      0               ; 0045 Extended memory in KBytes
buf_info:               
                public  _firstbuf
_firstbuf       dd      0               ; 0047 disk buffer chain
                dw      0               ; 004B Number of dirty buffers
                dd      0               ; 004D pre-read buffer
                dw      0               ; 0051 number of look-ahead buffers
                public  _bufloc
_bufloc         db      0               ; 0053 00=conv 01=HMA
                public  _deblock_buf
_deblock_buf    dd      0               ; 0054 deblock buffer
                db      3 dup (0)            ; 0058 unknown
                dw      0               ; 005B unknown
                db      0, 0FFh, 0      ; 005D unknown
                public _VgaSet
_VgaSet         db      0               ; 0060 unknown
                dw      0               ; 0061 unknown
                public  _uppermem_link
_uppermem_link  db      0               ; 0063 upper memory link flag
_min_pars       dw      0               ; 0064 minimum paragraphs of memory 
                                        ;      required by program being EXECed
                public  _uppermem_root
_uppermem_root  dw      0ffffh          ; 0066 dmd_upper_root (usually 9fff)
_last_para      dw      0               ; 0068 para of last mem search
SysVarEnd:

;; FreeDOS specific entries
;; all variables below this point are subject to relocation.
;; programs should not rely on any values below this point!!!

                public  _os_setver_minor
_os_setver_minor        db      0
                public  _os_setver_major
_os_setver_major        db      5
                public  _os_minor
_os_minor       db      0
                public  _os_major              
_os_major       db      5
_rev_number     db      0
                public  _version_flags         
_version_flags  db      0

                public  os_release
                extern  _os_release: near
os_release      dw      _os_release

%IFDEF WIN31SUPPORT
                public  _winStartupInfo, _winInstanced
_winInstanced    dw 0 ; set to 1 on WinInit broadcast, 0 on WinExit broadcast
_winStartupInfo:
                dw 0 ; structure version (same as windows version)
                dd 0 ; next startup info structure, 0:0h marks end
                dd 0 ; far pointer to name virtual device file or 0:0h
                dd 0 ; far pointer, reference data for virtual device driver
                dw instance_table,seg instance_table ; array of instance data
instance_table: ; should include stacks, Win may auto determine SDA region
                ; we simply include whole DOS data segment
                dw seg _DATASTART, 0 ; [SEG:OFF] address of region's base
                dw markEndInstanceData wrt seg _DATASTART ; size in bytes
                dd 0 ; 0 marks end of table
                dw 0 ; and 0 length for end of instance_table entry
                public  _winPatchTable
_winPatchTable: ; returns offsets to various internal variables
                dw 0x0006      ; DOS version, major# in low byte, eg. 6.00
                dw save_DS     ; where DS stored during int21h dispatch
                dw save_BX     ; where BX stored during int21h dispatch
                dw _InDOS      ; offset of InDOS flag
                dw _MachineId  ; offset to variable containing MachineID
                dw _CritPatch  ; offset of to array of offsets to patch
                               ; NOTE: this points to a null terminated
                               ; array of offsets of critical section bytes
                               ; to patch, for now we can just point this
                               ; to an empty table
                               ; ie we just point to a 0 word to mark end
                dw _uppermem_root ; seg of last arena header in conv memory
                                  ; this matches MS DOS's location, but 
                                  ; do we have the same meaning?
%ENDIF ; WIN31SUPPORT

;;  The first 5 sft entries appear to have to be at DS:00cc
                db (0cch - ($ - DATASTART)) dup (0)
                public _firstsftt
_firstsftt:             
                dd -1                   ; link to next
                dw 5                    ; count 
                db 5*59 dup (0)         ; reserve space for the 5 sft entries
                db 0                    ; pad byte so next value on even boundary        

; Some references seem to indicate that this data should start at 01fbh in
; order to maintain 100% MS-DOS compatibility.
                db (01fbh - ($ - DATASTART)) dup (0)

                public  MARK01FBH
MARK01FBH       equ     $
                public  _local_buffer   ; local_buffer is 256 bytes long
                                        ; so it overflows into kb_buf!!
        ; only when kb_buf is used, local_buffer is limited to 128 bytes.
_local_buffer:
		db 128 dup (0)
                public  _kb_buf
_kb_buf db      128,0                   ; initialise buffer to empty
                db 128+1 dup (0)   ; room for 128 byte readline + LF
;
; Variables that follow are documented as part of the DOS 4.0-6.X swappable
; data area in Ralf Browns Interrupt List #56
;
; this byte is used for ^P support
                public  _PrinterEcho
_PrinterEcho    db      0               ;-34 -  0 = no printer echo, ~0 echo
                public  _verify_ena
_verify_ena     db      0               ; ~0, write with verify

; this byte is used for TABs (shared by all char device writes??)
                public _scr_pos
_scr_pos        db      0               ; Current Cursor Column
                public  _switchar
_switchar       db      '/'             ;-31 - switch char
                public  _mem_access_mode
_mem_access_mode db     0               ;-30 -  memory allocation strategy
                public  sharing_flag
sharing_flag    db      0               ; 00 = sharing module not loaded
                                        ; 01 = sharing module loaded, but
                                        ;      open/close for block devices
                                        ;      disabled
                                        ; FF = sharing module loaded,
                                        ;      open/close for block devices
                                        ;      enabled (not implemented)
                public  _net_set_count
_net_set_count   db      1               ;-28 -  count the name below was set
                public  _net_name
_net_name       db      '               ' ;-27 - 15 Character Network Name
                db      00                ; Terminating 0 byte


;
;       Variables contained the the "STATE_DATA" segment contain
;       information about the STATE of the current DOS Process. These
;       variables must be preserved regardless of the state of the INDOS
;       flag.
;
;       All variables that appear in "STATE_DATA" **MUST** be declared
;       in this file as the offsets from the INTERNAL_DATA variable are
;       critical to the DOS applications that modify this data area.
;
;
                public  _ErrorMode, _InDOS
                public  _CritErrLocus, _CritErrCode
                public  _CritErrAction, _CritErrClass
                public  _CritErrDev, _CritErrDrive
                public  _dta
                public  _cu_psp, _default_drive
                public  _break_ena
                public  _return_code
                public  _internal_data

; ensure offset of critical patch table remains fixed, some programs hard code offset
                db (0315h - ($ - DATASTART)) dup (0)
                public  _CritPatch
_CritPatch      dw      0               ;-11 zero list of patched critical
                dw      0               ;    section variables
                dw      0               ;    DOS puts 0d0ch here but some
                dw      0               ;    progs really write to that addr.
                dw      0               ;-03 - critical patch list terminator
                db      90h             ;-01 - unused, NOP pad byte
_internal_data:              ; <-- Address returned by INT21/5D06
_ErrorMode      db      0               ; 00 - Critical Error Flag
_InDOS          db      0               ; 01 - Indos Flag
_CritErrDrive   db      0               ; 02 - Drive on write protect error
_CritErrLocus   db      0               ; 03 - Error Locus
_CritErrCode    dw      0               ; 04 - DOS format error Code
_CritErrAction  db      0               ; 06 - Error Action Code
_CritErrClass   db      0               ; 07 - Error Class
_CritErrDev     dd      0               ; 08 - Failing Device Address
_dta            dd      0               ; 0C - current DTA
_cu_psp         dw      0               ; 10 - Current PSP
break_sp        dw      0               ; 12 - used in int 23
_return_code    dw      0               ; 14 - return code from process
_default_drive  db      0               ; 16 - Current Drive
_break_ena      db      1               ; 17 - Break Flag (default TRUE)
                db      0               ; 18 - flag, code page switching
                db      0               ; 19 - flag, copy of 18 on int 24h abort

                public  _swap_always, _swap_indos
_swap_always:

                public  _Int21AX
_Int21AX        dw      0               ; 1A - AX from last Int 21

                public  owning_psp, _MachineId
owning_psp      dw      0               ; 1C - owning psp
_MachineId      dw      0               ; 1E - remote machine ID
                dw      0               ; 20 - First usable mcb
                dw      0               ; 22 - Best usable mcb
                dw      0               ; 24 - Last usable mcb
                dw      0               ; 26 - memory size in paragraphs
                dw      0               ; 28 - unknown
                db      0               ; 2A - unknown
                db      0               ; 2B - unknown
                db      0               ; 2C - unknown
                public  _break_flg
_break_flg      db      0               ; 2D - Program aborted by ^C
                db      0               ; 2E - unknown
                db      0               ; 2F - not referenced
                public  _DayOfMonth
_DayOfMonth     db      1               ; 30 - day of month
                public  _Month
_Month          db      1               ; 31 - month
                public  _YearsSince1980
_YearsSince1980 dw      0               ; 32 - year since 1980
daysSince1980   dw      0FFFFh          ; 34 - number of days since epoch
                                        ; force rebuild on first clock read
                public  _DayOfWeek
_DayOfWeek      db      2               ; 36 - day of week
_console_swap   db      0               ; 37 console swapped during read from dev
                public  _dosidle_flag        
_dosidle_flag   db      1               ; 38 - safe to call int28 if nonzero
_abort_progress db      0               ; 39 - abort in progress
                public  _CharReqHdr
_CharReqHdr:
                public  _ClkReqHdr
_ClkReqHdr      db 30 dup (0)      ; 3A - Device driver request header
                dd      0               ; 58 - pointer to driver entry
                public  _MediaReqHdr
_MediaReqHdr    db 22 dup (0)      ; 5C - Device driver request header
                public  _IoReqHdr
_IoReqHdr       db 30 dup (0)      ; 72 - Device driver request header
                db 6 dup (0)       ; 90 - unknown
                public  _ClkRecord
_ClkRecord      db 6 dup (0)       ; 96 - CLOCK$ transfer record
                dw      0               ; 9C - unknown
                public  __PriPathBuffer
__PriPathBuffer db 80h dup (0)     ; 9E - buffer for file name
                public  __SecPathBuffer
__SecPathBuffer db 80h dup (0)     ;11E - buffer for file name
                public  _sda_tmp_dm
_sda_tmp_dm     db 21 dup (0)      ;19E - 21 byte srch state
                public  _SearchDir
_SearchDir      db 32 dup (0)      ;1B3 - 32 byte dir entry
                public  _TempCDS
_TempCDS        db 88 dup (0)      ;1D3 - TemporaryCDS buffer
                public  _DirEntBuffer
_DirEntBuffer   db 32 dup (0)      ;22B - space enough for 1 dir entry
                public  _wAttr
_wAttr          dw      0               ;24B - extended FCB file attribute


                public  _SAttr
_SAttr          db      0           ;24D - Attribute Mask for Dir Search
                public  _OpenMode
_OpenMode       db      0           ;24E - File Open Attribute

                db 3 dup (0)
                public  _Server_Call
_Server_Call    db      0           ;252 - Server call Func 5D sub 0
                db      0
                ; Pad to 05CCh
                db (25ch - ($ - _internal_data)) dup (0)

                public  _tsr            ; used by break and critical error
_tsr            db      0               ;25C -  handlers during termination
                db      0               ;25D - padding
                public  term_psp
term_psp        dw  0                   ;25E - 0??
                public  int24_esbp
int24_esbp      dw 2 dup (0)       ;260 - pointer to criticalerr DPB
                public  _user_r, int21regs_off, int21regs_seg
_user_r:
int21regs_off   dw      0               ;264 - pointer to int21h stack frame
int21regs_seg   dw      0
                public  critical_sp
critical_sp     dw      0               ;268 - critical error internal stack
                public  current_ddsc
current_ddsc    dw 2 dup (0)

                ; Pad to 059ah
                db (27ah - ($ - _internal_data)) dup (0)
                public  current_device
current_device  dw 2 dup (0)       ;27A - 0??
                public  _lpCurSft
_lpCurSft       dw 2 dup(0)       ;27e - Current SFT
                public  _current_ldt
_current_ldt     dw 2 dup (0)       ;282 - Current CDS
                public  _sda_lpFcb
_sda_lpFcb      dw 2 dup (0)       ;286 - pointer to callers FCB
                public  _current_sft_idx
_current_sft_idx    dw      0               ;28A - SFT index for next open
                                        ; used by MS NET

                ; Pad to 05b2h
                db (292h - ($ - _internal_data)) dup (0)
                dw      __PriPathBuffer  ; 292 - "sda_WFP_START" offset in DOS DS of first filename argument
                dw      __SecPathBuffer  ; 294 - "sda_REN_WFP" offset in DOS DS of second filename argument

                ; Pad to 05ceh
                db (2aeh - ($ - _internal_data)) dup (0)
                public  _current_filepos
_current_filepos dw 2 dup (0)       ;2AE - current offset in file

                ; Pad to 05eah
                db (2cah - ($ - _internal_data)) dup (0)
                ;public _save_BX
                ;public _save_DS
save_BX                 dw      0       ;2CA - unused by FreeDOS, for Win3.x
save_DS                 dw      0       ;      compatibility, match MS's positions
                        dw      0
                public  _prev_user_r
                public  prev_int21regs_off
                public  prev_int21regs_seg
_prev_user_r:
prev_int21regs_off      dw      0       ;2D0 - pointer to prev int 21 frame
prev_int21regs_seg      dw      0

                ; Pad to 05fdh
                db (2ddh - ($ - _internal_data)) dup (0)
                public  _ext_open_action
                public  _ext_open_attrib
                public  _ext_open_mode
_ext_open_action dw 0                   ;2DD - extended open action
_ext_open_attrib dw 0                   ;2DF - extended open attrib
_ext_open_mode   dw 0                   ;2E1 - extended open mode

                ; Pad to 0620h
                db (300h - ($ - _internal_data)) dup (0)

                public apistk_bottom
apistk_bottom:
                ; use bottom of error stack as scratch buffer
                ;  - only used during int 21 call
                public  _sda_tmp_dm_ren
_sda_tmp_dm_ren:
		db 21 dup (090h)   ;300 - 21 byte srch state for rename
                public  _SearchDir_ren
_SearchDir_ren: 
		db 32 dup (090h)   ;315 - 32 byte dir entry for rename

                ; stacks are made to initialize to no-ops so that high-water
                ; testing can be performed
                db STACK_SIZE*2-($-apistk_bottom) dup (090h)
                ;300 - Error Processing Stack
                public  _error_tos
_error_tos:
                dw STACK_SIZE dup (09090h) ;480 - Disk Function Stack
                public  _disk_api_tos
_disk_api_tos:
                dw STACK_SIZE dup (09090h) ;600 - Char Function Stack
                public  _char_api_tos
_char_api_tos:
apistk_top:
                db      0               ; 780 ???
_VolChange      db      0               ;781 - volume change
_VirtOpen       db      0               ;782 - virtual open flag

                ; controlled variables end at offset 78Ch so pad to end
                db (78ch - ($ - _internal_data)) dup (0)

_FIXED_DATA	ENDS
;
; end of controlled variables
;

_BSS	segment 
;!!                public  _NumFloppies
;!!_NumFloppies resw    1
;!!intr_dos_stk resw    1
;!!intr_dos_seg resw    1


; mark front and end of bss area to clear
IB_B	segment 
    public __ib_start
__ib_start:
IB_B	ENDS
IB_E	segment 
    public __ib_end
__ib_end:
IB_E	ENDS
        ;; do not clear the other init BSS variables + STACK: too late.
_BSS	ENDS

_DATA	segment 
; kernel startup stack (not used now)
;                public  init_tos
;                dw 512 dup (0)
;init_tos:
; the last paragraph of conventional memory might become an MCB
                db 16 dup (0)
                public __init_end
__init_end:
init_end:        
; blockdev private stack
                public  blk_stk_top
                dw 256 dup (0)
blk_stk_top:

; clockdev private stack
                public  clk_stk_top
                dw 128 dup (0)
clk_stk_top:

; int2fh private stack
                public  int2f_stk_top
                dw 128 dup (0)
int2f_stk_top:
_DATA	ENDS
; Dynamic data:
; member of the DOS DATA GROUP
; and marks definitive end of all used data in kernel data segment
;

_DATAEND	segment 

_swap_indos:
; we don't know precisely what needs to be swapped before this, so set it here.
; this is just after FIXED_DATA+BSS+DATA and before (D)CONST+BSS
; probably, the clock and block stacks and disktransferbuffer should go past
; _swap_indos but only if int2a ah=80/81 (critical section start/end)
; are called upon entry and exit of the device drivers

                dw 96 dup (09090h) ; Process 0 Stack
                public  _p_0_tos
_p_0_tos:
_DATAEND	ENDS

DYN_DATA	segment 

        public _Dyn
_Dyn:
        DynAllocated dw 0

markEndInstanceData:  ; mark end of DOS data seg we say needs instancing
DYN_DATA	ENDS
        
ID_B	segment 
    public __INIT_DATA_START
__INIT_DATA_START:
ID_B	ENDS
ID_E	segment 
    public __INIT_DATA_END
__INIT_DATA_END:
ID_E	ENDS

INIT_TEXT_START	segment 
                public  __InitTextStart
__InitTextStart:                    ; and c version
INIT_TEXT_START	ENDS

INIT_TEXT_END	segment 
                public  __InitTextEnd
__InitTextEnd:                      ; and c version
INIT_TEXT_END	ENDS
;
; start end end of HMA area

HMA_TEXT_START	segment 
                public __HMATextAvailable
__HMATextAvailable:
                public  __HMATextStart
__HMATextStart:   

HMA_TEXT_START	ENDS

; 
; the HMA area is filled with 1eh+3(=sizeof VDISK) = 33 byte dummy data,
; so nothing will ever be below 0xffff:0031
;
HMA_TEXT	segment 
begin_hma:              
                db 10h dup (0)   ; filler [ffff:0..ffff:10]
                db 20h dup (0)
                db 0

; to minimize relocations
                public _DGROUP_
_DGROUP_        dw DGROUP

;%ifdef WATCOM
;               32 bit multiplication + division
public __U4M
__U4M:
                LMULU
public __U4D
__U4D:
                LDIVMODU
;%endif


                db 0d0h - ($-begin_hma) dup (0)
                ; reserve space for far jump to cp/m routine
                db 5 dup (0)

HMA_TEXT	ENDS

;End of HMA segment                
HMA_TEXT_END	segment 
                public  __HMATextEnd
__HMATextEnd:                   ; and c version
HMA_TEXT_END	ENDS


; The default stack (_TEXT:0) will overwrite the data area, so I create a dummy
; stack here to ease debugging. -- ror4

_STACK	segment public 'STACK'
_STACK	ENDS


    

CONST	segment 
        ; dummy interrupt return handlers

                public _int22_handler
                public _int28_handler
                public _int2a_handler
                public _empty_handler
_int22_handler:         
_int28_handler:
_int2a_handler:
_empty_handler:
                iret
    

public _initforceEnableA20
_initforceEnableA20:
                call near ptr forceEnableA20
                retf   

    public __HMARelocationTableStart
__HMARelocationTableStart:   

                public  _int2f_handler
                extern  reloc_call_int2f_handler: near
_int2f_handler:
		db	0eah			; jmp 0:reloc_call_int2f_handler
		dw	offset reloc_call_int2f_handler
		dw	0
                call near ptr forceEnableA20

                public  _int20_handler
                extern  reloc_call_int20_handler: near
_int20_handler:
		db	0eah			;jmp 0:reloc_call_int20_handler
		dw	offset reloc_call_int20_handler
		dw	0
                call near ptr forceEnableA20

                public  _int21_handler
                extern  reloc_call_int21_handler: near
_int21_handler: 
		db	0eah			;jmp 0:reloc_call_int21_handler
		dw	offset reloc_call_int21_handler
		dw	0
                call near ptr forceEnableA20


                public  _low_int25_handler
                extern  reloc_call_low_int25_handler: near
_low_int25_handler: 
		db	0eah			; jmp 0:reloc_call_low_int25_handler
		dw	offset reloc_call_low_int25_handler
		dw	0
                call near ptr forceEnableA20

                public  _low_int26_handler
                extern  reloc_call_low_int26_handler: near
_low_int26_handler:
		db	0eah			;jmp 0:reloc_call_low_int26_handler
		dw	offset reloc_call_low_int26_handler
		dw	0
                call near ptr forceEnableA20

                public  _int27_handler
                extern  reloc_call_int27_handler: near
_int27_handler: 
		db	0eah			;jmp 0:reloc_call_int27_handler
		dw	offset reloc_call_int27_handler
		dw	0
                call near ptr forceEnableA20

                public  _int0_handler
                extern  reloc_call_int0_handler: near
_int0_handler:  
		db	0eah			;jmp 0:reloc_call_int0_handler
		dw	offset reloc_call_int0_handler
		dw	0
                call near ptr forceEnableA20

                public  _int6_handler
                extern  reloc_call_int6_handler: near
_int6_handler:  
		db	0eah			;jmp 0:reloc_call_int6_handler
		dw	offset reloc_call_int6_handler
		dw	0
                call near ptr forceEnableA20

                public  _int19_handler
                extern  reloc_call_int19_handler: near
_int19_handler: 
		db	0eah			;jmp 0:reloc_call_int19_handler
		dw	offset reloc_call_int19_handler
		dw	0
                call near ptr forceEnableA20

                public  _cpm_entry
                extern  reloc_call_cpm_entry: near
_cpm_entry:     
		db	0eah			;jmp 0:reloc_call_cpm_entry
		dw	offset reloc_call_cpm_entry
		dw	0
                call near ptr forceEnableA20

                public  _reloc_call_blk_driver
                extern  _blk_driver: near
_reloc_call_blk_driver:
                db	0eah			;jmp 0:_blk_driver
		dw	offset _blk_driver
		dw	0
                call near ptr forceEnableA20

                public  _reloc_call_clk_driver
                extern  _clk_driver: near
_reloc_call_clk_driver:
                db	0eah			;jmp 0:_clk_driver
		dw	offset _clk_driver
		dw	0
                call near ptr forceEnableA20

                public  _CharMapSrvc ; in _DATA (see AARD)
                extern  _reloc_call_CharMapSrvc: near
_CharMapSrvc:   
		db	0eah			;jmp 0:_reloc_call_CharMapSrvc
		dw	offset _reloc_call_CharMapSrvc
		dw	0
                call near ptr forceEnableA20

                public _init_call_p_0
                extern reloc_call_p_0: near
_init_call_p_0: 
		db	0eah			;jmp  0:reloc_call_p_0
		dw	offset reloc_call_p_0
		dw	0
                call near ptr forceEnableA20


   public __HMARelocationTableEnd
__HMARelocationTableEnd:    

;
; if we were lucky, we found all entries from the outside to the kernel.
; if not, BUMS
;
;
; this routine makes the HMA area available. PERIOD.
; must conserve ALL registers
; will be only ever called, if HMA (DOS=HIGH) is enabled.
; for obvious reasons it should be located at the relocation table
;

    public _ENABLEA20
_ENABLEA20:
    mov ah,5
UsingXMSdriver:

	public _XMS_Enable_Patch
_XMS_Enable_Patch:		; SMC: patch to nop (90h) to enable use of XMS
	retf

    push bx
    db	0eah			; call 0:0			; (immediate far address patched)
    dw	0
    dw	0
    public _XMSDriverAddress
_XMSDriverAddress equ $ - 4	; XMS driver, if detected
    pop  bx
    retf

    public _DISABLEA20
_DISABLEA20:
    mov ah,6
    jmp short UsingXMSdriver


    public forceEnableA20
forceEnableA20:

    push ds
    push es
    push ax
	push si
	push di
	push cx
	pushf
	cld

retry:
	xor si, si		; = 0000h
	mov ds, si		; => low memory (IVT)
	dec si			; = FFFFh
	mov es, si		; => HMA at offset 10h
	inc si			; back to 0, -> IVT entry 0 and 1
	mov di, 10h		; -> HMA, or wrapping around to 0:0
	mov cx, 4
	repe cmpsw		; compare up to 4 words
	je enable

success:
	popf
	pop cx
	pop di
	pop si
    pop ax
    pop es
    pop ds
    retn

enable:
		; ok, we have to enable A20 (at least seems so)
	push cs			; make far call stack frame
	call _ENABLEA20
	jmp short retry


; global f*cking compatibility issues:
;
; very old brain dead software (PKLITE, copyright 1990)
; forces us to execute with A20 disabled
;

	public _ExecUserDisableA20
_ExecUserDisableA20:
    push ax
	push cs			; make far call stack frame
	call _DISABLEA20	; (no-op if not in HMA, patched otherwise)
    pop ax
    iret


; Default Int 24h handler -- always returns fail
; so we have not to relocate it (now)
;
FAIL            equ     03h

                public  _int24_handler
_int24_handler: mov     al,FAIL
                iret

;
; this makes some things easier
;

_LOWTEXT	segment 
                public _TEXT_DGROUP
_TEXT_DGROUP dw DGROUP
_LOWTEXT	ENDS

INIT_TEXT	segment 
                public _INIT_DGROUP
_INIT_DGROUP dw DGROUP
INIT_TEXT	ENDS

CONST	ENDS
	END krnlstart
