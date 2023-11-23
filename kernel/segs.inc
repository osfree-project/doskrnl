; File:
;                           segs.inc
; Description:
;     Segment definitions for the kernel
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
; $Header$
;

; CPU specification -- putting it here because all .asm files include this
; file. 
	.8086

PGROUP	group             PSP
LGROUP	group             _IRQTEXT, _LOWTEXT, _IO_TEXT, _IO_FIXED_DATA, _TEXT
DGROUP	group             _FIXED_DATA, _BSS, _DATA, _DATAEND, CONST, CONST2, DCONST, DYN_DATA
TGROUP	group             HMA_TEXT_START, HMA_TEXT, HMA_TEXT_END, INIT_TEXT_START, INIT_TEXT, INIT_TEXT_END
IGROUP	equ TGROUP
I_GROUP	group            ID_B, I_DATA, ICONST, ICONST2, ID_E, IB_B, I_BSS, IB_E
INITSIZE equ init_end; wrt INIT_TEXT
INITTEXTSIZE equ __INIT_DATA_START; wrt INIT_TEXT


PSP	segment PUBLIC 'PSP'
PSP	ENDS

_IRQTEXT	segment PUBLIC 'LCODE'
_IRQTEXT	ENDS

_LOWTEXT	segment PUBLIC 'LCODE'
_LOWTEXT	ENDS

_IO_TEXT	segment	PUBLIC 'LCODE'
_IO_TEXT	ENDS

_IO_FIXED_DATA	segment	PUBLIC WORD 'LCODE'
_IO_FIXED_DATA	ENDS

_TEXT	segment	PUBLIC 'LCODE'
_TEXT	ENDS

_FIXED_DATA	segment PUBLIC PARA 'FDATA'
_FIXED_DATA	ENDS

_BSS	segment PUBLIC WORD 'BSS'
_BSS	ENDS

_DATA	segment PUBLIC WORD 'DATA'
_DATA	ENDS

_DATAEND	segment PUBLIC BYTE 'DATA'
_DATAEND	ENDS

;for WATCOM
CONST	segment PUBLIC WORD 'DATA'
CONST	ENDS

CONST2	segment PUBLIC WORD 'DATA'
CONST2	ENDS

;for MSC
DCONST	segment	PUBLIC WORD 'DCONST'
DCONST	ENDS

DYN_DATA	segment	PUBLIC 'DYN_DATA'
DYN_DATA	ENDS

HMA_TEXT_START	segment	PUBLIC PARA 'CODE'
HMA_TEXT_START	ENDS

HMA_TEXT	segment	PUBLIC 'CODE'
HMA_TEXT	ENDS

HMA_TEXT_END	segment PUBLIC PARA 'CODE'
HMA_TEXT_END	ENDS

INIT_TEXT_START	segment PUBLIC PARA 'CODE'
INIT_TEXT_START	ENDS

INIT_TEXT	segment	PUBLIC 'CODE'
INIT_TEXT	ENDS

INIT_TEXT_END	segment	PUBLIC PARA 'CODE'
INIT_TEXT_END	ENDS


ID_B	segment PUBLIC PARA 'FAR_DATA'
ID_B	ENDS

I_DATA	segment PUBLIC WORD 'FAR_DATA'
I_DATA	ENDS

ICONST	segment PUBLIC WORD 'FAR_DATA'
ICONST	ENDS

ICONST2	segment PUBLIC WORD 'FAR_DATA'
ICONST2	ENDS

ID_E	segment PUBLIC WORD 'FAR_DATA'
ID_E	ENDS

IB_B	segment PUBLIC WORD 'FAR_DATA'
IB_B	ENDS

I_BSS	segment PUBLIC WORD 'FAR_DATA'
I_BSS	ENDS

IB_E	segment PUBLIC WORD 'FAR_DATA'
IB_E	ENDS
