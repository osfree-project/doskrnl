; File:
;                         asmsupt.asm
; Description:
;       Assembly support routines for miscellaneous functions
;
;                    Copyright (c) 1995, 1998
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
; version 1.4 by tom.ehlert@ginko.de
; added some more functions
; changed bcopy, scopy, sncopy,...
; to      memcpy, strcpy, strncpy
; Bart Oldeman: optimized a bit: see /usr/include/bits/string.h from 
; glibc 2.2
;
; $Id: asmsupt.asm 1568 2011-04-09 02:42:51Z bartoldeman $
;

;%ifdef WATCOM
;%ifdef _INIT
;%define WATCOM_INIT ; no seperate init segment for watcom.
;%endif
;%endif


		include segs.inc
		include stacks.inc

;%ifdef _INIT

  ;segment INIT_TEXT
  ;%define  FMEMCPYBACK INIT_FMEMCPYBACK
  ;%define   MEMCPY   INIT_MEMCPY
  ;%define  FMEMCPY  INIT_FMEMCPY
  ;%define   MEMSET   INIT_MEMSET
  ;%define  FMEMSET  INIT_FMEMSET
  ;%define   STRCPY   INIT_STRCPY
  ;%define  FSTRCPY  INIT_FSTRCPY
  ;%define   STRLEN   INIT_STRLEN
  ;%define  FSTRLEN  INIT_FSTRLEN
  ;%define  FMEMCHR  INIT_FMEMCHR
  ;%define  FSTRCHR  INIT_FSTRCHR
  ;%define   STRCHR   INIT_STRCHR
  ;%define  FSTRCMP  INIT_FSTRCMP
  ;%define   STRCMP   INIT_STRCMP
  ;%define FSTRNCMP INIT_FSTRNCMP
  ;%define  STRNCMP  INIT_STRNCMP
  ;%define  FMEMCMP  INIT_FMEMCMP
  ;%define   MEMCMP   INIT_MEMCMP

;%else
  
HMA_TEXT  segment 

;%endif

;*********************************************************************
; this implements some of the common string handling functions
;
; every function has 1 entry
;
;   NEAR FUNC()
;
; currently done:
;
; fmemcpyBack(void FAR *dest, void FAR *src, int count)
;  memcpy(void     *dest, void     *src, int count)
; fmemcpy(void FAR *dest, void FAR *src, int count)
;  memset(void *dest, int ch, int count);
; fmemset(void FAR *dest, int ch, int count);
;  strcpy (void    *dest, void     *src);
; fstrcpy (void FAR*dest, void FAR *src);
;  strlen (void    *dest);
; fstrlen (void FAR*dest);
; fmemchr (BYTE FAR *src , int ch);
; fstrchr (BYTE FAR *src , int ch);
;  strchr (BYTE     *src , int ch);
; fstrcmp (BYTE FAR *s1 , BYTE FAR *s2);
;  strcmp (BYTE     *s1 , BYTE     *s2);
; fstrncmp(BYTE FAR *s1 , BYTE FAR *s2, int count);
;  strncmp(BYTE     *s1 , BYTE     *s2, int count);
; fmemcmp(BYTE FAR *s1 , BYTE FAR *s2, int count);
;  memcmp(BYTE     *s1 , BYTE     *s2, int count);

;***********************************************
; pascal_setup - set up the standard calling frame for C-functions
;                and save registers needed later
;                also preload the args for the near functions
;                di=arg1
;                si=arg2
;                cx=arg3
;
pascal_setup:
                pop     ax                      ; get return address
                
                push    bp                      ; Standard C entry
                mov     bp,sp
;%ifdef WATCOM
                push    bx
                push    cx
                push    es
;%endif
                push    si
                push    di
                push    ds
                ; Set both ds and es to same segment (for near copy)
                push    ds
                pop     es

                ; Set direction to autoincrement
                cld

                mov bl,6       ; majority (4) wants that
		;arg arg1, arg2, arg3
                mov cx,[bp+4] ; majority (8) wants that (near and far)
                mov si,[bp+6] ; majority (3) wants that (near)
                mov di,[bp+7] ; majority (3) wants that (near)
                
                jmp ax



        
;***********************************************
;
;       VOID memcpy(REG BYTE *s, REG BYTE *d, REG COUNT n);
;
                public  MEMCPY
MEMCPY:
                call pascal_setup

                ;mov cx,[4+bp] - preset above
                ;mov si,[6+bp] - preset above
                ;mov di,[8+bp] - preset above

                ;mov bl,6 - preset above


domemcpy:
                ; And do the built-in byte copy, but do a 16-bit transfer
                ; whenever possible.
                shr     cx,1
                rep     movsw
                jnc     memcpy_return
                movsb
memcpy_return:
%if 0                           ; only needed for fmemcpyback
                cld
%endif        

;
; pascal_return - pop saved registers and do return
;
        
                jmp short pascal_return



;************************************************************
;
;       VOID fmemcpy(REG BYTE FAR *d, REG BYTE FAR *s,REG COUNT n);
;       VOID fmemcpyBack(REG BYTE FAR *d, REG BYTE FAR *s,REG COUNT n);
;
                public  FMEMCPY
%if 0
                public  FMEMCPYBACK
FMEMCPYBACK:
                std             ; force to copy the string in reverse order
%endif
FMEMCPY:
                call pascal_setup

		;arg {d,4}, {s,4}, n
                ; Get the repetition count, n preset above
;%ifdef STDCALL
;                mov     cx,[.n]
;%endif

                ; Get the far source pointer, s
                lds     si,[bp+6]

                ; Get the far destination pointer d
                les     di,[bp+10]
		mov	bl,10

                jmp short domemcpy

;***************************************************************
;
;       VOID fmemset(REG VOID FAR *d, REG BYTE ch, REG COUNT n);
;
                public  FMEMSET
FMEMSET:
                call pascal_setup

		;arg {d,4}, ch, n
                ; Get the repetition count, n - preset above
;%ifdef STDCALL
;                mov     cx,[.n]
;%endif

                ; Get the fill byte ch
                mov     ax,[bp+6]
                
                ; Get the far source pointer, s
                les     di,[bp+8]
		mov	bl,8

domemset:                
                mov	ah, al

                shr	cx,1
                rep     stosw
                jnc     pascal_return
                stosb
                
                jmp  short pascal_return

;***************************************************************
;
;       VOID memset(REG VOID *d, REG BYTE ch, REG COUNT n);
;
                public  MEMSET
MEMSET:
                call pascal_setup
                
		;arg d, ch, n
                ; Get the repitition count, n - preset above
                ; mov     cx,[bp+4]

                ; Get the char ch
                mov     ax, [bp+6]

                ; Get the far source pointer, d - preset above
                ; mov      di,[bp+8]

		;mov	bl, 6   ; preset above

                jmp short domemset

;*****
pascal_return:
                lds     di, [bp]    ; return address in ds, saved bp in di
                mov     bh, 0
                add     bp, bx      ; point bp to "as if there were 0 args"
                mov     [bp+2], ds  ; put return address at first arg
                mov     [bp], di    ; saved bp below that one

                pop     ds
                pop     di
                pop     si
;%ifdef WATCOM
                pop     es
                pop     cx
                pop     bx
;%endif
                mov     sp,bp
                pop     bp
                ret

;*****************************************************************
                
; fstrcpy (void FAR*dest, void FAR *src);

                public  FSTRCPY
FSTRCPY:
                call pascal_setup

		;arg {dest,4}, {src,4}
                ; Get the source pointer, ss
                lds   si, dword ptr [bp+4]

                ; and the destination pointer, d
                les   di, dword ptr [bp+8]

		mov   bl,8
                
                jmp short dostrcpy

;******
                public  STRCPY
STRCPY:
                call pascal_setup


;%ifdef PASCAL
                ; Get the source pointer, ss
                mov   si,[bp+4]

                ; and the destination pointer, d
                mov   di,[bp+6]
;%endif
		mov   bl,4

dostrcpy:

strcpy_loop:                
                lodsb
                stosb
                test al,al
                jne  strcpy_loop
                
		jmp  short pascal_return

;******************************************************************                
                public  FSTRLEN
FSTRLEN:
                call pascal_setup

                ; Get the source pointer, ss
                les   di,[bp+4]
		mov   bl,4

                jmp short dostrlen

;**********************************************
                public  STRLEN
STRLEN:
                call pascal_setup
                ; Get the source pointer, ss
;%ifdef PASCAL
                mov   di,[bp+4]
;%endif
		mov   bl,2

dostrlen:           
                mov al,0
                mov cx,0ffffh
                repne scasb

                mov ax,cx
                not ax                
                dec ax

                jmp short pascal_return

;************************************************************
; strchr (BYTE *src , int ch);

                public  STRCHR
STRCHR:
                call pascal_setup

                ; Get the source pointer, ss
		;arg src, ch
;%ifdef STDCALL	; preset above for PASCAL
;                mov             cx,[.ch]
;                mov             si,[.src]
;%endif
		mov bl,4

strchr_loop:                
                lodsb
                cmp  al,cl
                je   strchr_found
                test al,al
                jne  strchr_loop
                
strchr_retzero:
                xor ax, ax               ; return NULL if not found
                mov dx, ax               ; for fstrchr()
                jmp short pascal_return
                
strchr_found:
                mov ax, si
                mov dx, ds               ; for fstrchr()
strchr_found1:
                dec ax

                jmp short pascal_return

;%ifndef _INIT

;*****
;  fstrchr (BYTE     far *src , int ch);
                public  FSTRCHR
FSTRCHR:
                call pascal_setup

		;arg {src,4}, ch
                ; Get ch (preset above)
                ;mov cx, [bp+4]
                
                ;and the source pointer, src
                lds si, [bp+6]

		;mov	bl, 6 - preset above

                jmp short strchr_loop

;******
                public  FMEMCHR
FMEMCHR:
                call pascal_setup

		;arg {src,4}, ch, n
                ; Get the length - preset above
;%ifdef STDCALL
                ;mov cx, [.n]
;%endif

                ; and the search value
                mov ax, [bp+6]

                ; and the source pointer, ss
                les di, [bp+8]

		mov bl, 8

		jcxz strchr_retzero
                repne scasb
                jne strchr_retzero
                mov dx, es
                mov ax, di
                jmp short strchr_found1

;**********************************************************************
                public  FSTRCMP
FSTRCMP:
                call pascal_setup

		;arg {dest,4}, {src,4}
                ; Get the source pointer, ss
                lds             si, [bp+6]

                ; and the destination pointer, d
                les             di, [bp+10]
                
                mov bl,8

%if 0
                jmp short dostrcmp

;******
                public STRCMP
STRCMP:
                call pascal_setup

                mov bl,4

                ; Get the source pointer, ss
                ; mov             si,[bp+4]

                ; and the destination pointer, d
                ; mov             di,[bp+6]
                xchg si,di

dostrcmp:                       
%endif
                                    ; replace strncmp(s1,s2)-->
                                    ;         strncmp(s1,s2,0xffff)
                mov cx,0ffffh
%if 0
                jmp short dostrncmp

                
;**********************************************************************
                public  FSTRNCMP
FSTRNCMP:
                call pascal_setup

                ; Get the source pointer, ss
                lds             si,[bp+4]

                ; and the destination pointer, d
                les             di,[bp+8]
                mov             cx,[bp+12]
                mov             bl,10
                
                jmp short dostrncmp

;******
                public  _strncmp
_strncmp:
                call pascal_setup

                ; Get the source pointer, ss
                ;mov             si,[bp+4]

                ; and the destination pointer, d
                ;mov             di,[bp+6]
                ;mov             cx,[bp+8]
                xchg si,di

dostrncmp:
%endif
                jcxz strncmp_retzero

strncmp_loop:                
                lodsb
                scasb
                jne  strncmp_done
                test al,al
                loopne   strncmp_loop
                jmp  short strncmp_retzero		
;%endif

;**********************************************************************
; fmemcmp(BYTE FAR *s1 , BYTE FAR *s2, int count);
                public  FMEMCMP
FMEMCMP:
                call pascal_setup

		;arg {dest,4}, {src,4}, n
                ; the length - preset above
;%ifdef STDCALL
                ;mov cx, [.n]
;%endif
                
                ; Get the source pointer, ss
                les di,[bp+6]

                ; and the destination pointer, d
                lds si,[bp+10]

		mov bl,10

                jmp short domemcmp

;******
;  memcmp(BYTE     *s1 , BYTE     *s2, int count);        
                public  MEMCMP
MEMCMP:
                call pascal_setup

                ; all preset: Get the source pointer, ss
                ;mov             si,[bp+6]

                ; and the destination pointer, d
                ;mov             di,[bp+8]
                ;mov             cx,[bp+4]
		;mov		 bl,6
                xchg si,di

domemcmp:
                jcxz strncmp_retzero
                repe cmpsb
                jne  strncmp_done
strncmp_retzero:
                xor  ax, ax
                jmp  short strncmp_done2
strncmp_done:
                lahf
		ror  ah,1
strncmp_done2:  jmp  pascal_return

;%endif
HMA_TEXT	ENDS
		END
		