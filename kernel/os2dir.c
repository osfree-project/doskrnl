/****************************************************************/
/*                                                              */
/*                          os2dir.c                            */
/*                            DOS-C                             */
/*                                                              */
/*                 OS/2 File System dir Functions               */
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

#include "portab.h"
#include "globals.h"



#ifndef IPL
COUNT dos_findfirst(UCOUNT attr, BYTE * name)
{

  return 0;
}

/*
    BUGFIX TE 06/28/01 
    
    when using FcbFindXxx, the only information available is
    the cluster number + entrycount. everything else MUST\
    be recalculated. 
    a good test for this is MSDOS CHKDSK, which now (seems too) work
*/

COUNT dos_findnext(void)
{
  return 0;
}
#endif
/*
    this receives a name in 11 char field NAME+EXT and builds 
    a zeroterminated string

    unfortunately, blanks are allowed in filenames. like 
        "test e", " test .y z",...
        
    so we have to work from the last blank backward 
*/
void ConvertName83ToNameSZ(BYTE FAR * destSZ, BYTE FAR * srcFCBName)
{
  int loop;
  int noExtension = FALSE;

  if (*srcFCBName == '.')
  {
    noExtension = TRUE;
  }

  fmemcpy(destSZ, srcFCBName, FNAME_SIZE);

  srcFCBName += FNAME_SIZE;

  for (loop = FNAME_SIZE; --loop >= 0;)
  {
    if (destSZ[loop] != ' ')
      break;
  }
  destSZ += loop + 1;

  if (!noExtension)             /* not for ".", ".." */
  {

    for (loop = FEXT_SIZE; --loop >= 0;)
    {
      if (srcFCBName[loop] != ' ')
        break;
    }
    if (loop >= 0)
    {
      *destSZ++ = '.';
      fmemcpy(destSZ, srcFCBName, loop + 1);
      destSZ += loop + 1;
    }
  }
  *destSZ = '\0';
}


/* this routine converts a name portion of a fully qualified path       */
/* name, so . and .. are not allowed, only straightforward 8+3 names    */
const char *ConvertNameSZToName83(char *fcbname, const char *dirname)
{
  int i;
  memset(fcbname, ' ', FNAME_SIZE + FEXT_SIZE);

  for (i = 0; i < FNAME_SIZE + FEXT_SIZE; i++, dirname++)
  {
    char c = *dirname;
    if (c == '.')
      i = FNAME_SIZE - 1;
    else if (c != '\0' && c != '\\')
      fcbname[i] = c;
    else
      break;
  }
  return dirname;
}

