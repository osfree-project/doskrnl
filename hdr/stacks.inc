;
; File:
;                         stacks.inc
; Description:
;               Macro support for register stack frame
;
;                       Copyright (c) 1998
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
; $Id: stacks.inc 1591 2011-05-06 01:46:55Z bartoldeman $
;

;
; Standard stack frame used throughout DOS-C
;
;       MS-DOS specific
;
;       +---------------+
;       |    irp hi     |       26
;       +---------------+
;       |    irp low    |       24
;       +---------------+
;       |     flags     |       22
;       +---------------+
;       |       cs      |       20
;       +---------------+
;       |       ip      |       18
;       +---------------+
;       |       es      |       16
;       +---------------+
;       |       ds      |       14
;       +---------------+
;       |       bp      |       12
;       +---------------+
;       |       di      |       10
;       +---------------+
;       |       si      |       8
;       +---------------+
;       |       dx      |       6
;       +---------------+
;       |       cx      |       4
;       +---------------+
;       |       bx      |       2
;       +---------------+
;       |       ax      |       0
;       +---------------+
;

;; Note: The order of the pushed registers _must_ match with the definition
;; of the "iregs" structure within PCB.H, because a pointer to the last
;; pushed register is used as a pointer to a "iregs" structure within the
;; called C sources!       -- 2000/03/22 ska

; Don't use `struc RegFrame' etc. here because it interferes with segment
; definitions.
reg_ax    equ 0
reg_bx    equ 2
reg_cx    equ 4
reg_dx    equ 6
reg_si    equ 8
reg_di    equ 10
reg_bp    equ 12
reg_ds    equ 14
reg_es    equ 16
reg_ip    equ 18
reg_cs    equ 20
reg_flags equ 22
irp_low   equ 24
irp_hi    equ 26

PUSH_ALL	macro
                push    es
                push    ds
                push    bp
                push    di
                push    si
                push    dx
                push    cx
                push    bx
                push    ax
		endm

POP_ALL		macro
                pop     ax
                pop     bx
                pop     cx
                pop     dx
                pop     si
                pop     di
                pop     bp
                pop     ds
                pop     es
		endm

; I386.inc - 10/25/01 by tom ehlert
;
; compiling the kernel for 386 will (sometimes) change the
; high part of (some) registers, which will be (sometimes) be used 
; later
;
; assumption:
; we have never seen MSVC to use anything but eax, ecx, edx,
; nor have we seen Borland C to use anything but eax, ebx, edx,
; so we only protect eax, ebx or ecx, edx to conserve stack space
;
; to save even more stack space, we save only HIGH part of regs
; at some expense of slower execution. it's easier anyway :-)
;
; WATCOM only uses FS: and GS: (using -zff and -zgf) and never
; any high part of the 386 registers
;


IF @CPU AND 00001000B				; 386+
Protect386Registers	macro
		push fs
		push gs
	endm

RestoreSP	macro  
		lea sp, [bp-4]
		endm

Restore386Registers	macro  
		pop gs
		pop fs
	endm

ELSE           
	; no need to save/restore  anything

;	error 1 2 3
Protect386Registers	macro  
	endm
	
RestoreSP	macro  
		mov sp, bp  
	endm

Restore386Registers	macro  
	endm

ENDIF

; macros to define stack arguments
; arg a, {b,4}, c
; defines a and c as "word" arguments and b as a "dword" argument
; for STDCALL defines .a as [bp+4], .b as [bp+6] and .c as [bp+10]
; for PASCAL  defines .a as [bp+10], .b as [bp+6] and .c as [bp+4]
;
; popargs bx, {dx,ax}, cx pops these arguments of the stack (for PASCAL
; in reverse order). Here dx,ax is a dword argument dx:ax where dx is
; the high word. The caller is responsible for dealing with instruction
; pointer (ip) on the stack.

if 0
definearg macro   1-2 2
	%xdefine .%1 bp+.argloc
	%assign .argloc .argloc+%2
	endm

%macro  arg 1-*
	%assign .argloc 4
	%rep  %0
		%rotate -1
		definearg %1
	%endrep
%endmacro
endif

@ArgRev MACRO arglist:REQ
        LOCAL txt, arg
        txt TEXTEQU <>
%   FOR arg, <arglist>
            txt CATSTR <arg>, <,>, txt      ;; Paste each onto list
        ENDM
                                            ;; Remove terminating comma
        txt SUBSTR  txt, 1, @SizeStr( %txt ) - 1
        txt CATSTR  <!<>, txt, <!>>         ;; Add angle brackets
        EXITM txt
ENDM


popargs    MACRO   args:VARARG
%   FOR  arg, @ArgRev( <args> )   ;; Process in reverse order
            pop    arg
        ENDM
    ENDM    

definearg macro   arg
arg equ bp+argloc
argloc = argloc+2
	endm

;argloc equ 0

arg    MACRO   args:VARARG
argloc	= 4
%   FOR  arg, @ArgRev( <args> )   ;; Process in reverse order
		definearg arg
        ENDM
    ENDM    
