/****************************************************************/
/*                                                              */
/*                           systime.c                          */
/*                                                              */
/*                    DOS/C Date/Time Functions                 */
/*                                                              */
/*                      Copyright (c) 1998                      */
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
#include "time.h"
#include "date.h"
#include "globals.h"


const UWORD days[2][13] = {
  {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
  {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366}
};

/*
    return a pointer to an array with the days for that year
*/

const UWORD *is_leap_year_monthdays(UWORD y)
{
  /* this is correct in a strict mathematical sense   
     return ((y) & 3 ? days[0] : (y) % 100 ? days[1] : (y) % 400 ? days[0] : days[1]); */

  /* this will work until 2200 - long enough for me - and saves 0x1f bytes */

  if ((y & 3) || y == 2100)
    return days[0];

  return days[1];
}


void DosGetTime(struct dostime *dt)
{
  unsigned char hr=0;
  unsigned char mn=0;
  unsigned char sc=0;
  unsigned char hd=0;

  /* http://osfree.org/doku/doku.php?id=en:docs:mvm:api:13 */
  SVC(13);

  asm mov hr,ch
  asm mov mn,cl
  asm mov sc,dh
  asm mov hd,dl

  dt->hour = hr;
  dt->minute = mn;
  dt->second = sc;
  dt->hundredth = hd;
}

int DosSetTime(const struct dostime *dt)
{
  /* http://osfree.org/doku/doku.php?id=en:docs:mvm:api:11 */
  unsigned char hr=dt->hour;
  unsigned char mn=dt->minute;
  unsigned char sc=dt->second;
  unsigned char hd=dt->hundredth;
  unsigned char res=0;

  asm mov ch, hr
  asm mov cl, mn
  asm mov dh, sc
  asm mov dl, hd

  SVC(11);

  asm mov res, al

  return res;
}

unsigned char DosGetDate(struct dosdate *dd)
{
  /* http://osfree.org/doku/doku.php?id=en:docs:mvm:api:12 */
  unsigned short year=0;
  unsigned char month=0;
  unsigned char day=0;
  unsigned char dow=0;

  SVC(12);

  asm {
    mov year, cx
    mov month, dh
    mov day, dl
    mov dow, al
  }

  dd->year=year;
  dd->monthday=day;
  dd->month=month;

  return dow;
}

int DosSetDate(const struct dosdate *dd)
{
  /* http://osfree.org/doku/doku.php?id=en:docs:mvm:api:10 */
  unsigned short year=dd->year;
  unsigned char month=dd->month;
  unsigned char day=dd->monthday;
  unsigned char res=0;

  asm mov cx, year
  asm mov dh, month
  asm mov dl, day

  SVC(10);

  asm mov res, al

  return res;
}

