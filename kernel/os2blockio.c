/****************************************************************/
/*                                                              */
/*                          blockio.c                           */
/*                            DOS-C                             */
/*                                                              */
/*      Block cache functions and device driver interface       */
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
/*                                                              */
/****************************************************************/

#include "portab.h"
#include "globals.h"

/*                                                                      */
/*      Write all disk buffers                                          */
/*                                                                      */
BOOL flush(void)
{
 BOOL ok;
  ok = TRUE;

// Call SVC here???
  network_redirector(REM_FLUSHALL);

  return (ok);
}

/************************************************************************/
/*                                                                      */
/*              Device Driver Interface Functions                       */
/*                                                                      */
/************************************************************************/
/*                                                                      */
/* Transfer one or more blocks to/from disk                             */
/*                                                                      */

UWORD dskxfer(COUNT dsk, ULONG blkno, VOID FAR * buf, UWORD numblocks,
              COUNT mode)
{
// Call SVC here???
  return 0;                     /* Success!  Return 0 for a successful operation. */

}

