; File:
;                         DosIdle.asm
; Description:
;                   Dos Idle Interrupt Call
;
;                            DOS-C
;                   Copyright (c) 1995, 1999
;                        James B. Tabor
;                      All Rights Reserved
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
;
        include segs.inc

PSP_USERSP      equ     2eh
PSP_USERSS      equ     30h

HMA_TEXT	segment 

                public  _DosIdle_int
                public  _DosIdle_hlt

                extern   _InDOS: near
                extern   _cu_psp: near
                extern   _MachineId: near
                extern   critical_sp: near
                extern   _user_r: near
		; variables as the following are "part of" module inthndlr.c
		; because of the define MAIN before include globals.h there!
                extern   _HaltCpuWhileIdle: near
                extern   _DGROUP_: near
;
_DosIdle_hlt:
                push    ds
                mov     ds, word ptr [cs:_DGROUP_]
		assume ds:DGROUP
                cmp     byte ptr [DGROUP:_HaltCpuWhileIdle],1
                jb      DosId0
                pushf
                sti
                hlt                ; save some energy :-)
                popf
DosId0:         pop     ds
                retn
;
_DosIdle_int:
                call    _DosIdle_hlt
                push    ds
                mov     ds, word ptr [cs:_DGROUP_]
		assume ds:DGROUP
                cmp     byte ptr [DGROUP:_InDOS],1
                ja      DosId1
                call    Do_DosI
DosId1:
                pop     ds
                retn

Do_DosI:
                push    ax
                push    es
		assume ds:DGROUP
                push    word ptr [DGROUP:_MachineId]
                push    word ptr DGROUP:[_user_r]
                push    word ptr DGROUP:[_user_r+2]
                mov     es,word ptr [DGROUP:_cu_psp]
                push    word ptr [es:PSP_USERSS]
                push    word ptr [es:PSP_USERSP]

                int     28h

                mov     es,word ptr [DGROUP:_cu_psp]
                pop     word ptr [es:PSP_USERSP]
                pop     word ptr [es:PSP_USERSS]
                pop     word ptr DGROUP:[_user_r+2]
                pop     word ptr DGROUP:[_user_r]
                pop     word ptr [DGROUP:_MachineId]
                pop     es
                pop     ax
                ret

; segment _DATA		; belongs to DGROUP
; whatever	db whatever

HMA_TEXT	ends
		end
		