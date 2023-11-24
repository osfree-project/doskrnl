/****************************************************************/
/*                                                              */
/*                          os2fs.c                             */
/*                           DOS-C                              */
/*                                                              */
/*                 OS/2 File System I/O Functions               */
/*                                                              */
/*                    Copyright (c) 1995,1998                   */
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



struct dpb FAR *get_dpb(COUNT dsk)
{
  register struct cds FAR *cdsp = get_cds(dsk);
  
  if (cdsp == NULL || cdsp->cdsFlags & CDSNETWDRV)
    return NULL;
  return cdsp->cdsDpb;
}

/************************************************************************/
/*                                                                      */
/*      Internal file handlers - open, create, read, write, close, etc. */
/*                                                                      */
/************************************************************************/

/* Open a file given the path. Flags is 0 for read, 1 for write and 2   */
/* for update.                                                          */
/* Returns an long where the high word is a status code and the low     */
/* word is an integer file descriptor or a negative error code          */
/* see DosOpenSft(), dosfns.c for an explanation of the flags bits      */
/* directory opens are allowed here; these are not allowed by DosOpenSft*/

int dos_open(char *path, unsigned flags, unsigned attrib, int fd)
{
  return 0;
}

COUNT dos_close(COUNT fd)
{
  return 0;
}

/* checks whether directory part of path exists */
BOOL dir_exists(char * path)
{
  return 0;
}

void dos_merge_file_changes(int fd)
{
}


COUNT dos_delete(BYTE * path, int attrib)
{
    return 0;
}

COUNT dos_rmdir(BYTE * path)
{
  return 0;
}

COUNT dos_rename(BYTE * path1, BYTE * path2, int attrib)
{
  return 0;
}



/*                                                              */
/* dos_getdate for the file date                                */
/*                                                              */
date dos_getdate(void)
{
  struct dosdate dd;

  /* First - get the system date set by either the user   */
  /* on start-up or the CMOS clock                        */
  DosGetDate(&dd);
  return DT_ENCODE(dd.month, dd.monthday, dd.year - EPOCH_YEAR);
}

/* FAT time notation in the form of hhhh hmmm mmmd dddd (d = double second) */
STATIC time time_encode(struct dostime *t)
{
  return (t->hour << 11) | (t->minute << 5) | (t->second >> 1);
}

/*                                                              */
/* dos_gettime for the file time                                */
/*                                                              */
time dos_gettime(void)
{
  struct dostime dt;

  /* First - get the system time set by either the user   */
  /* on start-up or the CMOS clock                        */
  DosGetTime(&dt);
  return time_encode(&dt);
}


/*                                                              */
/* create a directory - returns success or a negative error     */
/* number                                                       */
/*                                                              */
COUNT dos_mkdir(BYTE * dir)
{

  return SUCCESS;
}


/*
  comments read optimization for large reads: read total clusters in one piece

  running a program like 
  
  while (1) {
    read(fd, header, sizeof(header));   // small read 
    read(fd, buffer, header.size);      // where size is large, up to 63K 
                                        // with average ~32K
    }                                        

    FreeDOS 2025 is really slow. 
    on a P200 with modern 30GB harddisk, doing above for a 14.5 MB file
    
    MSDOS 6.22 clustersize 8K  ~2.5 sec (accumulates over clusters, reads for 63 sectors seen),
    IBM PCDOS 7.0          8K  ~4.3 
    IBM PCDOS 7.0          16K ~2.8 
    FreeDOS ke2025             ~17.5

    with the read optimization (ke2025a),    
    
        clustersize 8K  ~6.5 sec
        clustersize 16K ~4.2 sec
        
    it was verified with IBM feature tool,
    that the drive read ahead cache (says it) is on. still this huge difference ;-)
        

    it's coded pretty conservative to avoid all special cases, 
    so it shouldn't break anything :-)

    possible further optimization:
    
        collect read across clusters (if file is not fragmented).
        MSDOS does this (as readcounts up to 63 sectors where seen)
        specially important for diskettes, where clustersize is 1 sector 
        
        the same should be done for writes as well

    the time to compile the complete kernel (on some P200) is 
    reduced from 67 to 56 seconds - in an otherwise identical configuration.

    it's not clear if this improvement shows up elsewhere, but it shouldn't harm either
    

    TE 10/18/01 14:00
    
    collect read across clusters (if file is not fragmented) done.
        
    seems still to work :-))
    
    no large performance gains visible, but should now work _much_
    better for the people, that complain about slow floppy access

    the 
        fnp->f_offset +to_xfer < fnp->f_dir.dir_size &&  avoid EOF problems 

    condition can probably _carefully_ be dropped    
    
    
    TE 10/18/01 19:00
    
*/

/* Read/write block from disk */
/* checking for valid access was already done by the functions in
   dosfns.c */
long rwblock(COUNT fd, VOID FAR * buffer, UCOUNT count, int mode)
{
  return 0;
}

/* returns the number of unused clusters */
CLUSTER dos_free(struct dpb FAR * dpbp)
{
  return SUCCESS;
}


VOID bpb_to_dpb(bpb FAR * bpbp, REG struct dpb FAR * dpbp)
{
  ULONG size;
  REG UWORD shftcnt;
  bpb sbpb;

  fmemcpy(&sbpb, bpbp, sizeof(sbpb));
  if (sbpb.bpb_nsector == 0) {
    shftcnt = 8;
  } else {
    for (shftcnt = 0; (sbpb.bpb_nsector >> shftcnt) > 1; shftcnt++)
      ;
  }
  dpbp->dpb_shftcnt = shftcnt;

  dpbp->dpb_mdb = sbpb.bpb_mdesc;
  dpbp->dpb_secsize = sbpb.bpb_nbyte;
  dpbp->dpb_clsmask = (sbpb.bpb_nsector - 1) & 0xFF;
  dpbp->dpb_fatstrt = sbpb.bpb_nreserved;
  dpbp->dpb_fats = sbpb.bpb_nfat;
  dpbp->dpb_dirents = sbpb.bpb_ndirent;
  size = sbpb.bpb_nsize == 0 ? sbpb.bpb_huge : (ULONG) sbpb.bpb_nsize;
  dpbp->dpb_fatsize = sbpb.bpb_nfsect;
  dpbp->dpb_dirstrt = dpbp->dpb_fatstrt + dpbp->dpb_fats * dpbp->dpb_fatsize;
  dpbp->dpb_data = dpbp->dpb_dirstrt
      + (dpbp->dpb_dirents + dpbp->dpb_secsize/DIRENT_SIZE - 1) /
          (dpbp->dpb_secsize/DIRENT_SIZE);
  dpbp->dpb_size = (UWORD)((size - dpbp->dpb_data) >> shftcnt) + 1;
  { /* Make sure the number of FAT sectors is actually enough to hold that */
    /* many clusters. Otherwise back the number of clusters down (LG & AB) */
    unsigned fatsiz;
    ULONG tmp = dpbp->dpb_fatsize * (ULONG)(dpbp->dpb_secsize / 2);/* entries/2 */
    if (tmp >= 0x10000UL)
      goto ckok;
    fatsiz = (unsigned) tmp;
    if (dpbp->dpb_size > FAT_MAGIC) {/* FAT16 */
      if (fatsiz <= FAT_MAGIC)       /* FAT12 - let it pass through rather */
        goto ckok;                   /* than lose data correcting FAT type */
    } else {                         /* FAT12 */
      if (fatsiz >= 0x4000)
        goto ckok;
      fatsiz = fatsiz * 4 / 3;
    }
    if (dpbp->dpb_size >= fatsiz)    /* FAT too short */
      dpbp->dpb_size = fatsiz - 1;   /* - 2 reserved entries + 1 */
ckok:;
  }
  dpbp->dpb_flags = 0;
  dpbp->dpb_cluster = UNKNCLUSTER;
  /* number of free clusters */
  dpbp->dpb_nfreeclst = UNKNCLSTFREE;

#ifdef WITHFAT32
  if (extended)
  {
    dpbp->dpb_xfatsize = sbpb.bpb_nfsect == 0 ? sbpb.bpb_xnfsect
        : sbpb.bpb_nfsect;
    dpbp->dpb_xcluster = UNKNCLUSTER;
    dpbp->dpb_xnfreeclst = XUNKNCLSTFREE;       /* number of free clusters */

    dpb16to32(dpbp);

    if (ISFAT32(dpbp))
    {
      dpbp->dpb_xflags = sbpb.bpb_xflags;
      dpbp->dpb_xfsinfosec = sbpb.bpb_xfsinfosec;
      dpbp->dpb_xbackupsec = sbpb.bpb_xbackupsec;
      dpbp->dpb_dirents = 0;
      dpbp->dpb_dirstrt = 0xffff;
      dpbp->dpb_size = 0;
      dpbp->dpb_xdata =
          dpbp->dpb_fatstrt + dpbp->dpb_fats * dpbp->dpb_xfatsize;
      dpbp->dpb_xsize = ((size - dpbp->dpb_xdata) >> shftcnt) + 1;
      dpbp->dpb_xrootclst = sbpb.bpb_xrootclst;
      read_fsinfo(dpbp);
    }
  }
#endif
}

STATIC int rqblockio(unsigned char command, struct dpb FAR * dpbp)
{
 retry:
  MediaReqHdr.r_length = sizeof(request);
  MediaReqHdr.r_unit = dpbp->dpb_subunit;
  MediaReqHdr.r_command = command;
  MediaReqHdr.r_mcmdesc = dpbp->dpb_mdb;
  MediaReqHdr.r_status = 0;

  if (command == C_BLDBPB) /* help USBASPI.SYS & DI1000DD.SYS (TE) */
    MediaReqHdr.r_bpfat = (boot FAR *)DiskTransferBuffer;
  execrh((request FAR *) & MediaReqHdr, dpbp->dpb_device);
  if ((MediaReqHdr.r_status & S_ERROR) || !(MediaReqHdr.r_status & S_DONE))
  {
    FOREVER
    {
      switch (block_error(&MediaReqHdr, dpbp->dpb_unit, dpbp->dpb_device, 0))
      {
      case ABORT:
      case FAIL:
        return DE_INVLDDRV;

      case RETRY:
        goto retry;

      case CONTINUE:
        return SUCCESS;
      }
    }
  }
  return SUCCESS;
}

COUNT media_check(REG struct dpb FAR * dpbp)
{
  int ret;
  if (dpbp == NULL)
    return DE_INVLDDRV;

  /* First test if anyone has changed the removable media         */
  ret = rqblockio(C_MEDIACHK, dpbp);
  if (ret < SUCCESS)
    return ret;

  switch (MediaReqHdr.r_mcretcode | dpbp->dpb_flags)
  {
    case M_NOT_CHANGED:
      /* It was definitely not changed, so ignore it          */
      return SUCCESS;

      /* If it is forced or the media may have changed,       */
      /* rebuild the bpb                                      */
    case M_DONT_KNOW:
      /* IBM PCDOS technical reference says to call BLDBPB if */
      /* there are no used buffers                            */
      if (dirty_buffers(dpbp->dpb_unit))
        return SUCCESS;

      /* If it definitely changed, don't know (falls through) */
      /* or has been changed, rebuild the bpb.                */
    /* case M_CHANGED: */
    default:
      setinvld(dpbp->dpb_unit);
      ret = rqblockio(C_BLDBPB, dpbp);
      if (ret < SUCCESS)
        return ret;
#ifdef WITHFAT32
      /* extend dpb only for internal or FAT32 devices */
      bpb_to_dpb(MediaReqHdr.r_bpptr, dpbp,
                 MediaReqHdr.r_bpptr->bpb_nfsect == 0 ||
                 FP_SEG(dpbp) == FP_SEG(&os_major));
#else
      bpb_to_dpb(MediaReqHdr.r_bpptr, dpbp);
#endif
      return SUCCESS;
  }
}

#ifndef IPL
int dos_cd(char * PathName)
{
  return SUCCESS;
}

COUNT dos_getfattr(BYTE * name)
{
  return SUCCESS ;
}

COUNT dos_setfattr(BYTE * name, UWORD attrp)
{
  return 0;
}
#endif
