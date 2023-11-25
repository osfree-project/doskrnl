/****************************************************************/
/*                                                              */
/*                          fcbfns.c                            */
/*                                                              */
/*           Old CP/M Style Function Handlers for DOSKRNL       */
/*                                                              */
/*                      Copyright (c) 1995                      */
/*                      Pasquale J. Villani                     */
/*                      All Rights Reserved                     */
/*                                                              */
/* This file is part of DOS-C.                                  */
/*                                                              */
/* DOS-C is free software; you can redistribute it and/or       */
/* modify it under the terms of the GNU General Public License  */
/* as published by the Free Software Foundation; either version */
/* 2, or (at your option) any later version.                    */
/*                                                              */
/* DOS-C is distributed in the hope that it will be useful, but */
/* WITHOUT ANY WARRANTY; without even the implied warranty of   */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See    */
/* the GNU General Public License for more details.             */
/*                                                              */
/* You should have received a copy of the GNU General Public    */
/* License along with DOS-C; see the file COPYING.  If not,     */
/* write to the Free Software Foundation, 675 Mass Ave,         */
/* Cambridge, MA 02139, USA.                                    */
/****************************************************************/

/* Redirect FCB functions to host via SVC

0FH 	Open file (FCB) 	1.0+
10H 	Close file (FCB) 	1.0+
11H 	Find first file (FCB) 	1.0+
12H 	Find next file (FCB) 	1.0+
13H 	Delete file (FCB) 	1.0+
14H 	Sequential read (FCB) 	1.0+
15H 	Sequential write (FCB) 	1.0+
16H 	Create or truncate file (FCB) 	1.0+
17H 	Rename file (FCB) 	1.0+
21H 	Random read (FCB) 	1.0+
22H 	Random write (FCB) 	1.0+
23H 	Get file size in records (FCB) 	1.0+
24H 	Set random record number (FCB) 	1.0+
27H 	Random block read (FCB) 	1.0+
28H 	Random block write (FCB) 	1.0+
29H 	Parse filename (FCB) 	1.0+

Error! E2028: FcbOpen_ is an undefined reference
Error! E2028: FcbClose_ is an undefined reference
Error! E2028: FcbFindFirstNext_ is an undefined reference
Error! E2028: FcbDelete_ is an undefined reference
Error! E2028: FcbReadWrite_ is an undefined reference
Error! E2028: FcbRename_ is an undefined reference
Error! E2028: FatGetDrvData_ is an undefined reference
Error! E2028: FcbRandomIO_ is an undefined reference
Error! E2028: FcbGetFileSize_ is an undefined reference
Error! E2028: FcbSetRandom_ is an undefined reference
Error! E2028: FcbRandomBlockIO_ is an undefined reference
Error! E2028: FcbParseFname_ is an undefined reference
Error! E2028: FcbCloseAll_ is an undefined reference

*/

#include "portab.h"
#include "globals.h"


#define FCB_SUCCESS     0
#define FCB_ERR_NODATA  1
#define FCB_ERR_SEGMENT_WRAP 2
#define FCB_ERR_EOF     3
#define FCB_ERROR       0xff

#ifndef IPL
UWORD FcbParseFname(UBYTE *wTestMode, const BYTE FAR * lpFileName, fcb FAR * lpFcb)
{
  return FP_OFF(lpFileName);
}


UBYTE FcbReadWrite(xfcb FAR * lpXfcb, UCOUNT recno, int mode)
{
  return FCB_SUCCESS;
}

UBYTE FcbGetFileSize(xfcb FAR * lpXfcb)
{
  return FCB_ERROR;
}

void FcbSetRandom(xfcb FAR * lpXfcb)
{
}


UBYTE FcbRandomBlockIO(xfcb FAR * lpXfcb, UWORD *nRecords, int mode)
{
  return FCB_SUCCESS;
}

UBYTE FcbRandomIO(xfcb FAR * lpXfcb, int mode)
{
  return FCB_SUCCESS;
}

/* FcbOpen and FcbCreate
   Expects lpXfcb to point to a valid, unopened FCB, containing file name to open (create)
   Create will attempt to find the file name in the current directory, if found truncates
     setting file size to 0, otherwise if does not exist will create the new file; the
     FCB is filled in same as the open call.
   On any error returns FCB_ERROR
   On success returns FCB_SUCCESS, and sets the following fields (other non-system reserved ones left unchanged)
     drive identifier (fcb_drive) set to actual drive (1=A, 2=B, ...; always >0 if not device)
     current block number (fcb_cublock) to 0
     file size (fcb_fsize) value from directory entry (0 if create)
     record size (fcb_recsiz) to 128; set to 0 for devices
     time & date (fcb_time & fcb_date) values from directory entry
     fcb_sftno, fcb_attrib_hi/_lo, fcb_strtclst, fcb_dirclst/off_unused are for internal use (system reserved)
*/
UBYTE FcbOpen(xfcb FAR * lpXfcb, unsigned flags)
{
  return FCB_SUCCESS;
}


UBYTE FcbDelete(xfcb FAR * lpXfcb)
{
  // Here  we must call DeleteFCB
  return FCB_ERROR;
}

UBYTE FcbRename(xfcb FAR * lpXfcb)
{
  return FCB_ERROR;
}

UBYTE FcbClose(xfcb FAR * lpXfcb)
{
  return FCB_ERROR;
}

/* close all files the current process opened by FCBs */
VOID FcbCloseAll()
{
}

UBYTE FcbFindFirstNext(xfcb FAR * lpXfcb, BOOL First)
{
  return FCB_SUCCESS;
}
#endif

BYTE FAR *FatGetDrvData(UBYTE drive, UBYTE * pspc, UWORD * bps, UWORD * nc)
{
  return NULL;
}
