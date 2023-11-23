;
; File:
;                            io.inc
; Description:
;     Segments and external common routines used by various device drivers
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

    include segs.inc

;
;      Error Return Codes
;

E_WRPRT       EQU 0       ; Write Protect
E_UNIT        EQU 1       ; Unknown Unit
E_NOTRDY      EQU 2       ; Device Not Ready
E_CMD         EQU 3       ; Unknown Command
E_CRC         EQU 4       ; Crc Error
E_LENGTH      EQU 5       ; Bad Length
E_SEEK        EQU 6       ; Seek Error
E_MEDIA       EQU 7       ; Unknown MEDIA
E_NOTFND      EQU 8       ; Sector Not Found
E_PAPER       EQU 9       ; No Paper
E_WRITE       EQU 10      ; Write Fault
E_READ        EQU 11      ; Read Fault
E_FAILURE     EQU 12      ; General Failure


	extern	_IOExit: near
	extern	_IOSuccess: near
	extern	_IOErrorExit: near
	extern	_IOErrCnt: near
	extern	_IODone: near
	extern	_IOCommandError: near
	extern	GetUnitNum: near
        extern  _ReqPktPtr: near

