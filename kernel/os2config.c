/****************************************************************/
/*                                                              */
/*                         os2config.c                          */
/*                            DOS-C                             */
/*                                                              */
/*                config.sys Processing Functions               */
/*                                                              */
/*                      Copyright (c) 1996                      */
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
/* write to the Free Software Foundation, Inc.,                 */
/* 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.     */
/****************************************************************/

#include "portab.h"
#include "init-mod.h"
#include "dyndata.h"


#ifdef DEBUG
#define DebugPrintf(x) printf x
#else
#define DebugPrintf(x)
#endif
#define para2far(seg) ((mcb FAR *)MK_FP((seg), 0))

UWORD umb_start BSS_INIT(0), UMB_top BSS_INIT(0);
UWORD ram_top BSS_INIT(0); /* How much ram in Kbytes               */
size_t ebda_size BSS_INIT(0);

struct config Config = {
  0,
  NUMBUFF,
  NFILES,
  0,
  NFCBS,
  0,
  "command.com",
  " /P /E:256\r\n",
  NLAST,
  0,
  NSTACKS,
  0,
  STACKSIZE
      /* COUNTRY= is initialized within DoConfig() */
      , 0                       /* strategy for command.com is low by default */
      , 0                       /* default value for switches=/E:nnnn */
};

STATIC seg base_seg BSS_INIT(0);
STATIC seg umb_base_seg BSS_INIT(0);
BYTE FAR *lpTop BSS_INIT(0);
//STATIC unsigned nCfgLine BSS_INIT(0);
COUNT UmbState BSS_INIT(0);

STATIC VOID mcb_init(UCOUNT seg, UWORD size, BYTE type);
STATIC VOID mumcb_init(UCOUNT seg, UWORD size);

#ifdef I86
STATIC VOID FAR * AlignParagraph(VOID FAR * lpPtr);
#else
#define AlignParagraph(x) ((VOID *)x)
#endif

BYTE HMAState BSS_INIT(0);
#define HMA_NONE 0              /* do nothing */
#define HMA_REQ 1               /* DOS = HIGH detected */
#define HMA_DONE 2              /* Moved kernel to HMA */
#define HMA_LOW 3               /* Definitely LOW */


/*
    moved from BLOCKIO.C here.
    that saves some relocation problems
*/

STATIC void config_init_buffers(int wantedbuffers)
{
  struct buffer FAR *pbuffer;
  unsigned buffers = 0;

  /* fill HMA with buffers if BUFFERS count >=0 and DOS in HMA        */
  if (wantedbuffers < 0)
    wantedbuffers = -wantedbuffers;
  else if (HMAState == HMA_DONE)
    buffers = (0xfff0 - HMAFree) / sizeof(struct buffer);

  if (wantedbuffers < 6)         /* min 6 buffers                     */
    wantedbuffers = 6;
  if (wantedbuffers > 99)        /* max 99 buffers                    */
  {
    printf("BUFFERS=%u not supported, reducing to 99\n", wantedbuffers);
    wantedbuffers = 99;
  }
  if (wantedbuffers > buffers)   /* more specified than available -> get em */
    buffers = wantedbuffers;

  LoL->nbuffers = buffers;
  LoL->inforecptr = &LoL->firstbuf;
  {
    size_t bytes = sizeof(struct buffer) * buffers;
    pbuffer = HMAalloc(bytes);

    if (pbuffer == NULL)
    {
      pbuffer = KernelAlloc(bytes, 'B', 0);
      if (HMAState == HMA_DONE)
        firstAvailableBuf = MK_FP(0xffff, HMAFree);
    }
    else
    {
      LoL->bufloc = LOC_HMA;
      /* space in HMA beyond requested buffers available as user space */
      firstAvailableBuf = pbuffer + wantedbuffers;
    }
  }
  LoL->deblock_buf = DiskTransferBuffer;
  LoL->firstbuf = pbuffer;

  DebugPrintf(("init_buffers (size %u) at", sizeof(struct buffer)));
  DebugPrintf((" (%p)", LoL->firstbuf));

  buffers--;
  pbuffer->b_prev = FP_OFF(pbuffer + buffers);
  {
    int i = buffers;
    do
    {
      pbuffer->b_next = FP_OFF(pbuffer + 1);
      pbuffer++;
      pbuffer->b_prev = FP_OFF(pbuffer - 1);
    }
    while (--i);
  }
  pbuffer->b_next = FP_OFF(pbuffer - buffers);

    /* now, we can have quite some buffers in HMA
       -- up to 50 for KE38616.
       so we fill the HMA with buffers
       but not if the BUFFERS count is negative ;-)
     */

  DebugPrintf((" done\n"));

  if (FP_SEG(pbuffer) == 0xffff)
  {
    buffers++;
    printf("Kernel: allocated %d Diskbuffers = %u Bytes in HMA\n",
           buffers, buffers * sizeof(struct buffer));
  }
}

/* Do first time initialization.  Store last so that we can reset it    */
/* later.                                                               */
void PreConfig(void)
{
  /* Initialize the base memory pointers                          */

#ifdef DEBUG
  {
    printf("SDA located at 0x%p\n", internal_data);
  }
#endif
  /* Begin by initializing our system buffers                     */
#ifdef DEBUG
/*  printf("Preliminary %d buffers allocated at 0x%p\n", Config.cfgBuffers, buffers);*/
#endif

  LoL->DPBp =
      DynAlloc("DPBp", blk_dev.dh_name[0], sizeof(struct dpb));

  LoL->sfthead = MK_FP(FP_SEG(LoL), 0xcc); /* &(LoL->firstsftt) */
  /* LoL->FCBp = (sfttbl FAR *)&FcbSft; */
  /* LoL->FCBp = (sfttbl FAR *)
     KernelAlloc(sizeof(sftheader)
     + Config.cfgFiles * sizeof(sft)); */

  config_init_buffers(Config.cfgBuffers);

  LoL->CDSp = KernelAlloc(sizeof(struct cds) * LoL->lastdrive, 'L', 0);

#ifdef DEBUG
/*  printf(" FCB table 0x%p\n",LoL->FCBp);*/
  printf(" sft table 0x%p\n", LoL->sfthead);
  printf(" CDS table 0x%p\n", LoL->CDSp);
  printf(" DPB table 0x%p\n", LoL->DPBp);
#endif

  /* Done.  Now initialize the MCB structure                      */
  /* This next line is 8086 and 80x86 real mode specific          */
#ifdef DEBUG
  printf("Preliminary  allocation completed: top at %p\n", lpTop);
#endif
}

/* Do second pass initialization: near allocation and MCBs              */
void PreConfig2(void)
{
  struct sfttbl FAR *sp;

  /* initialize NEAR allocated things */

  /* Initialize the base memory pointers from last time.          */
  /*
     if the kernel could be moved to HMA, everything behind the dynamic
     near data is free.
     otherwise, the kernel is moved down - behind the dynamic allocated data,
     and allocation starts after the kernel.
   */

  base_seg = LoL->first_mcb = FP_SEG(AlignParagraph((BYTE FAR *) DynLast() + 0x0f));

  if (Config.ebda2move)
  {
    ebda_size = ebdasize();
    if (ebda_size > Config.ebda2move)
      ebda_size = Config.ebda2move;
    ram_top += ebda_size / 1024;
  }

  /* We expect ram_top as Kbytes, so convert to paragraphs */
  mcb_init(base_seg, ram_top * 64 - LoL->first_mcb - 1, MCB_LAST);

  sp = LoL->sfthead;
  sp = sp->sftt_next = KernelAlloc(sizeof(sftheader) + 3 * sizeof(sft), 'F', 0);
  sp->sftt_next = (sfttbl FAR *) - 1;
  sp->sftt_count = 3;

  if (ebda_size)  /* move the Extended BIOS Data Area from top of RAM here */
    movebda(ebda_size, FP_SEG(KernelAlloc(ebda_size, 'I', 0)));

/*  if (UmbState == 2)
      umb_init();
*/
}

/* Do third pass initialization.                                        */
/* Also, run config.sys to load drivers.                                */
void PostConfig(void)
{
  sfttbl FAR *sp;

  /* We could just have loaded FDXMS or HIMEM */
  if (HMAState == HMA_REQ && MoveKernelToHMA())
    HMAState = HMA_DONE;

  if (Config.cfgDosDataUmb)
  {
    Config.cfgFilesHigh = TRUE;
    Config.cfgLastdriveHigh = TRUE;
    Config.cfgStacksHigh = TRUE;
  }

  /* compute lastdrive ... */
  LoL->lastdrive = Config.cfgLastdrive;
  if (LoL->lastdrive < LoL->nblkdev)
    LoL->lastdrive = LoL->nblkdev;

  DebugPrintf(("starting FAR allocations at %x\n", base_seg));

  /* Begin by initializing our system buffers                     */
  /* dma_scratch = (BYTE FAR *) KernelAllocDma(BUFFERSIZE); */
#ifdef DEBUG
  /* printf("DMA scratchpad allocated at 0x%p\n", dma_scratch); */
#endif

  config_init_buffers(Config.cfgBuffers);

/* LoL->sfthead = (sfttbl FAR *)&basesft; */
  /* LoL->FCBp = (sfttbl FAR *)&FcbSft; */
  /* LoL->FCBp = KernelAlloc(sizeof(sftheader)
     + Config.cfgFiles * sizeof(sft)); */
  sp = LoL->sfthead->sftt_next;
  sp = sp->sftt_next = (sfttbl FAR *)
    KernelAlloc(sizeof(sftheader) + (Config.cfgFiles - 8) * sizeof(sft), 'F',
                Config.cfgFilesHigh);
  sp->sftt_next = (sfttbl FAR *) - 1;
  sp->sftt_count = Config.cfgFiles - 8;

  LoL->CDSp = KernelAlloc(sizeof(struct cds) * LoL->lastdrive, 'L', Config.cfgLastdriveHigh);

#ifdef DEBUG
/*  printf(" FCB table 0x%p\n",LoL->FCBp);*/
  printf(" sft table 0x%p\n", LoL->sfthead->sftt_next);
  printf(" CDS table 0x%p\n", LoL->CDSp);
  printf(" DPB table 0x%p\n", LoL->DPBp);
#endif
  if (Config.cfgStacks)
  {
    VOID FAR *stackBase =
        KernelAlloc(Config.cfgStacks * Config.cfgStackSize, 'S',
                    Config.cfgStacksHigh);
    init_stacks(stackBase, Config.cfgStacks, Config.cfgStackSize);

    DebugPrintf(("Stacks allocated at %p\n", stackBase));
  }
  DebugPrintf(("Allocation completed: top at 0x%x\n", base_seg));

}

/* This code must be executed after device drivers has been loaded */
VOID configDone(VOID)
{
  if (UmbState == 1)
    para2far(base_seg)->m_type = MCB_LAST;

  if (HMAState != HMA_DONE)
  {
    mcb FAR *p;
    unsigned short kernel_seg;
    unsigned short hma_paras = (HMAFree+0xf)/16;

    kernel_seg = allocmem(hma_paras);
    p = para2far(kernel_seg - 1);

    p->m_name[0] = 'S';
    p->m_name[1] = 'C';
    p->m_psp = 8;

    DebugPrintf(("HMA not available, moving text to %x\n", kernel_seg));
    MoveKernel(kernel_seg);

    kernel_seg += hma_paras + 1;

    DebugPrintf(("kernel is low, start alloc at %x", kernel_seg));
  }

  /* The standard handles should be reopened here, because
     we may have loaded new console or printer drivers in CONFIG.SYS */
}

/* The following code is 8086 dependant                         */

#if 1                           /* ifdef KERNEL */
STATIC VOID mcb_init_copy(UCOUNT seg, UWORD size, mcb *near_mcb)
{
  near_mcb->m_size = size;
  fmemcpy(MK_FP(seg, 0), near_mcb, sizeof(mcb));
}

STATIC VOID mcb_init(UCOUNT seg, UWORD size, BYTE type)
{
  static mcb near_mcb BSS_INIT({0});
  near_mcb.m_type = type;
  mcb_init_copy(seg, size, &near_mcb);
}

STATIC VOID mumcb_init(UCOUNT seg, UWORD size)
{
  static mcb near_mcb = {
    MCB_NORMAL,
    8, 0,
    {0,0,0},
    {"SC"}
  };
  mcb_init_copy(seg, size, &near_mcb);
}
#endif


/*
    get BIOS key with timeout:

    timeout < 0: no timeout
    timeout = 0: poll only once
    timeout > 0: timeout in seconds

    return
            0xffff : no key hit

            0xHH.. : scancode in upper  half
            0x..LL : asciicode in lower half
*/
#define GetBiosTime() peekl(0, 0x46c)

UWORD GetBiosKey(int timeout)
{
  iregs r;

  ULONG startTime = GetBiosTime();

  if (timeout >= 0)
  {
    do
    {
      /* optionally HLT here - timer will IRQ even if no keypress */
      r.a.x = 0x0100;             /* are there keys available ? */
      init_call_intr(0x16, &r);
      if (!(r.flags & FLG_ZERO)) {
        r.a.x = 0x0000;
        init_call_intr(0x16, &r); /* there is a key, so better fetch it! */
        return r.a.x;
      }
    } while ((unsigned)(GetBiosTime() - startTime) < timeout * 18u);
    return 0xffff;
  }

  /* blocking wait (timeout < 0): fetch it */
#if 0
  do {
      /* optionally HLT here */
      r.a.x = 0x0100;
      init_call_intr(0x16, &r);
  } while (r.flags & FLG_ZERO);
#endif
  r.a.x = 0x0000;
  init_call_intr(0x16, &r);
  return r.a.x;
}



#ifdef I86
#if 0
STATIC BYTE FAR * KernelAllocDma(WORD bytes, char type)
{
  if ((base_seg & 0x0fff) + (bytes >> 4) > 0x1000) {
    KernelAllocPara((base_seg + 0x0fff) & 0xf000 - base_seg, type, NULL, 0);
  }
  return KernelAlloc(bytes, type);
}
#endif

STATIC void FAR * AlignParagraph(VOID FAR * lpPtr)
{
  UWORD uSegVal;

  /* First, convert the segmented pointer to linear address       */
  uSegVal = FP_SEG(lpPtr);
  uSegVal += (FP_OFF(lpPtr) + 0xf) >> 4;
  if (FP_OFF(lpPtr) > 0xfff0)
    uSegVal += 0x1000;          /* handle overflow */

  /* and return an adddress adjusted to the nearest paragraph     */
  /* boundary.                                                    */
  return MK_FP(uSegVal, 0);
}
#endif

struct submcb
{
  char type;
  unsigned short start;
  unsigned short size;
  char unused[3];
  char name[8];
};

void FAR * KernelAllocPara(size_t nPara, char type, char *name, int mode)
{
  seg base, start;
  struct submcb FAR *p;

  /* if no umb available force low allocation */
  if (UmbState != 1)
    mode = 0;

  if (mode)
  {
    base = umb_base_seg;
    start = umb_start;
  }
  else
  {
    base = base_seg;
    start = LoL->first_mcb;
  }

  /* create the special DOS data MCB if it doesn't exist yet */
  DebugPrintf(("kernelallocpara: %x %x %x %c %d\n", start, base, nPara, type, mode));

  if (base == start)
  {
    mcb FAR *p = para2far(base);
    base++;
    mcb_init(base, p->m_size - 1, p->m_type);
    mumcb_init(FP_SEG(p), 0);
    p->m_name[1] = 'D';
  }

  nPara++;
  mcb_init(base + nPara, para2far(base)->m_size - nPara, para2far(base)->m_type);
  para2far(start)->m_size += nPara;

  p = (struct submcb FAR *)para2far(base);
  p->type = type;
  p->start = FP_SEG(p)+1;
  p->size = nPara-1;
  if (name)
    fmemcpy(p->name, name, 8);
  base += nPara;
  if (mode)
    umb_base_seg = base;
  else
    base_seg = base;
  return MK_FP(FP_SEG(p)+1, 0);
}


void FAR * KernelAlloc(size_t nBytes, char type, int mode)
{
  void FAR *p;
  size_t nPara = (nBytes + 15)/16;

  if (LoL->first_mcb == 0)
  {
    /* prealloc */
    lpTop = MK_FP(FP_SEG(lpTop) - nPara, FP_OFF(lpTop));
    p = AlignParagraph(lpTop);
  }
  else
  {
    p = KernelAllocPara(nPara, type, NULL, mode);
  }
  fmemset(p, 0, nBytes);
  return p;
}
