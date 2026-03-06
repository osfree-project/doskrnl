/*! @file svc.h
 *  @brief SVC API for MVM (Multiple Virtual Machine) – OpenWatcom 16-bit C header.
 *
 *  (c) osFree Project 2026, <http://www.osFree.org>
 *  For licence see licence.txt in the root directory or project website.
 *
 *  Documentation: https://osfree.org/doku/en:docs:mvm:api
 *
 *  This header provides C-callable functions for all SVC (Supervisor Call)
 *  services. Each function is implemented using `#pragma aux` to generate
 *  the required HLT instruction sequence:
 *      HLT
 *      DB  <SVC_code>
 *      DB  (NOT <SVC_code>) & 0FFh
 *
 *  Parameters are passed in registers according to the underlying DOS
 *  INT 21h convention (e.g. BX for handle, DS:DX for path, etc.).
 *  Return values are delivered in registers and assigned to the function's
 *  return value or output parameters.
 *
 *  Usage:
 *      #include <svc.h>
 *      unsigned short handle, newhandle;
 *      handle = 0x1234;
 *      newhandle = SvcDupHandle(handle);    // duplicate file handle
 */

#ifndef SVC_H_INCLUDED
#define SVC_H_INCLUDED

/* ========================================================================= */
/*  SVC function codes (SVC_*)                                              */
/* ========================================================================= */
#define SVC_DUPHANDLE       0   /* INT 21h AH=45h */
#define SVC_ERROREXIT       1   /* no DOS equivalent */
#define SVC_EXIT            2   /* INT 21h AH=4Ch */
#define SVC_CLOSE           3   /* INT 21h AH=3Eh */
#define SVC_RESETDISK       4   /* INT 21h AH=0Dh */
#define SVC_GETPSP          5   /* INT 21h AH=62h */
#define SVC_SELECTDRIVE     6   /* INT 21h AH=0Eh */
#define SVC_SETDTA          7   /* INT 21h AH=1Ah */
#define SVC_GETVERSION      8   /* INT 21h AH=30h */
#define SVC_WRITE           9   /* INT 21h AH=40h */
#define SVC_SETDATE        10   /* INT 21h AH=2Bh */
#define SVC_SETTIME        11   /* INT 21h AH=2Dh */
#define SVC_GETDATE        12   /* INT 21h AH=2Ah */
#define SVC_GETTIME        13   /* INT 21h AH=2Ch */
#define SVC_READ           14   /* INT 21h AH=3Fh */
#define SVC_CHGFILEPTR     15   /* INT 21h AH=42h */
#define SVC_RMDIR          16   /* INT 21h AH=3Ah */
#define SVC_MKDIR          17   /* INT 21h AH=39h */
#define SVC_QCURDIR        18   /* INT 21h AH=47h */
#define SVC_CHDIR          19   /* INT 21h AH=3Bh */
#define SVC_GETSETFILETIME 20   /* INT 21h AH=57h */
#define SVC_COMMITFILE     21   /* INT 21h AH=68h */
#define SVC_FINDFIRSTFCB   22   /* INT 21h AH=11h */
#define SVC_FINDNEXTFCB    23   /* INT 21h AH=12h */
#define SVC_FINDFIRST      25   /* INT 21h AH=4Eh */
#define SVC_OPEN           28   /* INT 21h AH=3Dh */
#define SVC_DELETEFCB      29   /* INT 21h AH=13h */
#define SVC_RENAMEFCB      30   /* INT 21h AH=17h */
#define SVC_DELETE         31   /* INT 21h AH=41h */
#define SVC_MOVE           32   /* INT 21h AH=56h */
#define SVC_OPENFCB        37   /* INT 21h AH=0Fh */
#define SVC_CLOSEFCB       39   /* INT 21h AH=10h */
#define SVC_SETVERIFY      44   /* INT 21h AH=2Eh */
#define SVC_FINDNEXT       45   /* INT 21h AH=4Fh */
#define SVC_CREATE         46   /* INT 21h AH=3Ch */
#define SVC_CREATETEMP     47   /* INT 21h AH=5Ah */
#define SVC_PARSEFILENAME  49   /* INT 21h AH=29h */
#define SVC_ABSDISKREAD    61   /* INT 25h */
#define SVC_ABSDISKWRITE   62   /* INT 26h */

/* ========================================================================= */
/*  SVC function prototypes and auxiliary pragmas                           */
/* ========================================================================= */

/* ------------------------------------------------------------------------- */
/* 0: Duplicate file handle                                                  */
/*    https://osfree.org/doku/en:docs:mvm:api:0                              */
/*    Input: handle (BX)                                                     */
/*    Returns: new handle (AX)                                               */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcDupHandle(unsigned short handle);
#pragma aux SvcDupHandle = \
    "hlt" \
    "db  SVC_DUPHANDLE" \
    "db  NOT SVC_DUPHANDLE" \
    parm [bx] \
    value [ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 1: Terminate MVM with error message                                       */
/*    https://osfree.org/doku/en:docs:mvm:api:1                              */
/*    The far pointer to message must be pushed onto stack before call.      */
/* ------------------------------------------------------------------------- */
extern void SvcErrorExit(void);
#pragma aux SvcErrorExit = \
    "hlt" \
    "db  SVC_ERROREXIT" \
    "db  NOT SVC_ERROREXIT" \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 2: Terminate MVM and return code (INT 21h AH=4Ch)                         */
/*    https://osfree.org/doku/en:docs:mvm:api:2                              */
/*    Input: return code (AL)                                                */
/* ------------------------------------------------------------------------- */
extern void SvcExit(unsigned char retcode);
#pragma aux SvcExit = \
    "hlt" \
    "db  SVC_EXIT" \
    "db  NOT SVC_EXIT" \
    parm [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 3: Close file handle (INT 21h AH=3Eh)                                     */
/*    https://osfree.org/doku/en:docs:mvm:api:3                              */
/*    Input: handle (BX)                                                     */
/* ------------------------------------------------------------------------- */
extern void SvcClose(unsigned short handle);
#pragma aux SvcClose = \
    "hlt" \
    "db  SVC_CLOSE" \
    "db  NOT SVC_CLOSE" \
    parm [bx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 4: Reset disk buffers (INT 21h AH=0Dh)                                    */
/*    https://osfree.org/doku/en:docs:mvm:api:4                              */
/* ------------------------------------------------------------------------- */
extern void SvcResetDisk(void);
#pragma aux SvcResetDisk = \
    "hlt" \
    "db  SVC_RESETDISK" \
    "db  NOT SVC_RESETDISK" \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 5: Get current PSP (INT 21h AH=62h)                                       */
/*    https://osfree.org/doku/en:docs:mvm:api:5                              */
/*    Returns: PSP segment (BX)                                              */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcGetPSP(void);
#pragma aux SvcGetPSP = \
    "hlt" \
    "db  SVC_GETPSP" \
    "db  NOT SVC_GETPSP" \
    value [bx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 6: Select default drive (INT 21h AH=0Eh)                                  */
/*    https://osfree.org/doku/en:docs:mvm:api:6                              */
/*    Input: drive number (0=A,1=B,...) in DL                                */
/*    Returns: number of logical drives (AL)                                 */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcSelectDrive(unsigned char drive);
#pragma aux SvcSelectDrive = \
    "hlt" \
    "db  SVC_SELECTDRIVE" \
    "db  NOT SVC_SELECTDRIVE" \
    parm [dl] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 7: Set Disk Transfer Address (INT 21h AH=1Ah)                             */
/*    https://osfree.org/doku/en:docs:mvm:api:7                              */
/*    Input: DTA buffer at DS:DX                                             */
/* ------------------------------------------------------------------------- */
extern void SvcSetDTA(void far *dta);
#pragma aux SvcSetDTA = \
    "hlt" \
    "db  SVC_SETDTA" \
    "db  NOT SVC_SETDTA" \
    parm [ds dx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 8: Get DOS version (INT 21h AH=30h)                                       */
/*    https://osfree.org/doku/en:docs:mvm:api:8                              */
/*    Returns: version in AX (AL=major, AH=minor)                            */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcGetVersion(void);
#pragma aux SvcGetVersion = \
    "hlt" \
    "db  SVC_GETVERSION" \
    "db  NOT SVC_GETVERSION" \
    value [ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 9: Write to file (INT 21h AH=40h)                                         */
/*    https://osfree.org/doku/en:docs:mvm:api:9                              */
/*    Input: handle (BX), buffer (DS:DX), bytes to write (CX)                */
/*    Returns: bytes actually written (AX)                                   */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcWrite(unsigned short handle,
                               const void far *buffer,
                               unsigned short bytes);
#pragma aux SvcWrite = \
    "hlt" \
    "db  SVC_WRITE" \
    "db  NOT SVC_WRITE" \
    parm [bx] [ds dx] [cx] \
    value [ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 10: Set system date (INT 21h AH=2Bh)                                      */
/*     https://osfree.org/doku/en:docs:mvm:api:10                            */
/*     Input: year (CX), month (DH), day (DL)                                */
/*     Returns: 0 on success, 0xFF on error (AL)                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcSetDate(unsigned short year,
                                unsigned char month,
                                unsigned char day);
#pragma aux SvcSetDate = \
    "hlt" \
    "db  SVC_SETDATE" \
    "db  NOT SVC_SETDATE" \
    parm [cx] [dh] [dl] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 11: Set system time (INT 21h AH=2Dh)                                      */
/*     https://osfree.org/doku/en:docs:mvm:api:11                            */
/*     Input: hour (CH), minute (CL), second (DH), hundredths (DL)           */
/*     Returns: 0 on success, 0xFF on error (AL)                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcSetTime(unsigned char hour,
                                unsigned char minute,
                                unsigned char second,
                                unsigned char hundredth);
#pragma aux SvcSetTime = \
    "hlt" \
    "db  SVC_SETTIME" \
    "db  NOT SVC_SETTIME" \
    parm [ch] [cl] [dh] [dl] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 12: Get system date (INT 21h AH=2Ah)                                      */
/*     https://osfree.org/doku/en:docs:mvm:api:12                            */
/*     Returns: values via pointers                                          */
/* ------------------------------------------------------------------------- */
extern void SvcGetDate(unsigned short *year,
                       unsigned char *month,
                       unsigned char *day,
                       unsigned char *dow);
#pragma aux SvcGetDate = \
    "hlt" \
    "db  SVC_GETDATE" \
    "db  NOT SVC_GETDATE" \
    modify [ax cx dx] \
    /* CX=year, DH=month, DL=day, AL=dow - stored by caller stub */;

/* ------------------------------------------------------------------------- */
/* 13: Get system time (INT 21h AH=2Ch)                                      */
/*     https://osfree.org/doku/en:docs:mvm:api:13                            */
/*     Returns: values via pointers                                          */
/* ------------------------------------------------------------------------- */
extern void SvcGetTime(unsigned char *hour,
                       unsigned char *minute,
                       unsigned char *second,
                       unsigned char *hundredth);
#pragma aux SvcGetTime = \
    "hlt" \
    "db  SVC_GETTIME" \
    "db  NOT SVC_GETTIME" \
    modify [cx dx] \
    /* CH=hour, CL=minute, DH=second, DL=hundredth */;

/* ------------------------------------------------------------------------- */
/* 14: Read from file (INT 21h AH=3Fh)                                       */
/*     https://osfree.org/doku/en:docs:mvm:api:14                            */
/*     Input: handle (BX), buffer (DS:DX), bytes to read (CX)                */
/*     Returns: bytes actually read (AX)                                     */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcRead(unsigned short handle,
                              void far *buffer,
                              unsigned short bytes);
#pragma aux SvcRead = \
    "hlt" \
    "db  SVC_READ" \
    "db  NOT SVC_READ" \
    parm [bx] [ds dx] [cx] \
    value [ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 15: Move file pointer (INT 21h AH=42h)                                    */
/*     https://osfree.org/doku/en:docs:mvm:api:15                            */
/*     Input: handle (BX), offset high (CX), offset low (DX), method (AL)    */
/*     Returns: new pointer in DX:AX                                         */
/* ------------------------------------------------------------------------- */
extern unsigned long SvcChgFilePtr(unsigned short handle,
                                   long offset,
                                   unsigned char method);
#pragma aux SvcChgFilePtr = \
    "hlt" \
    "db  SVC_CHGFILEPTR" \
    "db  NOT SVC_CHGFILEPTR" \
    parm [bx] [cx dx] [al] \
    value [dx ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 16: Remove directory (INT 21h AH=3Ah)                                     */
/*     https://osfree.org/doku/en:docs:mvm:api:16                            */
/*     Input: ASCIZ path at DS:DX                                            */
/* ------------------------------------------------------------------------- */
extern void SvcRmDir(const char far *path);
#pragma aux SvcRmDir = \
    "hlt" \
    "db  SVC_RMDIR" \
    "db  NOT SVC_RMDIR" \
    parm [ds dx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 17: Create directory (INT 21h AH=39h)                                     */
/*     https://osfree.org/doku/en:docs:mvm:api:17                            */
/*     Input: ASCIZ path at DS:DX                                            */
/* ------------------------------------------------------------------------- */
extern void SvcMkDir(const char far *path);
#pragma aux SvcMkDir = \
    "hlt" \
    "db  SVC_MKDIR" \
    "db  NOT SVC_MKDIR" \
    parm [ds dx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 18: Get current directory (INT 21h AH=47h)                                */
/*     https://osfree.org/doku/en:docs:mvm:api:18                            */
/*     Input: drive (DL), buffer at DS:SI                                    */
/*     Returns: AL = 0 on success, 0xFF on error                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcQCurDir(unsigned char drive, char far *buffer);
#pragma aux SvcQCurDir = \
    "hlt" \
    "db  SVC_QCURDIR" \
    "db  NOT SVC_QCURDIR" \
    parm [dl] [ds si] \
    value [al] \
    modify [ax bx cx dx si];

/* ------------------------------------------------------------------------- */
/* 19: Change directory (INT 21h AH=3Bh)                                     */
/*     https://osfree.org/doku/en:docs:mvm:api:19                            */
/*     Input: ASCIZ path at DS:DX                                            */
/* ------------------------------------------------------------------------- */
extern void SvcChDir(const char far *path);
#pragma aux SvcChDir = \
    "hlt" \
    "db  SVC_CHDIR" \
    "db  NOT SVC_CHDIR" \
    parm [ds dx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 20: Get/Set file date/time (INT 21h AH=57h)                               */
/*     https://osfree.org/doku/en:docs:mvm:api:20                            */
/*     Input: handle (BX), action (AL=0 get, AL=1 set),                      */
/*            for set: time (CX), date (DX)                                  */
/*     Returns: for get: time (CX), date (DX) (through pointers)             */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcGetSetFileTime(unsigned short handle,
                                        unsigned char action,
                                        unsigned short time,
                                        unsigned short date,
                                        unsigned short *rtime,
                                        unsigned short *rdate);
#pragma aux SvcGetSetFileTime = \
    "hlt" \
    "db  SVC_GETSETFILETIME" \
    "db  NOT SVC_GETSETFILETIME" \
    parm [bx] [al] [cx] [dx] \
    modify [ax bx cx dx] \
    /* results stored by caller stub */;

/* ------------------------------------------------------------------------- */
/* 21: Commit file (INT 21h AH=68h)                                          */
/*     https://osfree.org/doku/en:docs:mvm:api:21                            */
/*     Input: handle (BX)                                                    */
/* ------------------------------------------------------------------------- */
extern void SvcCommitFile(unsigned short handle);
#pragma aux SvcCommitFile = \
    "hlt" \
    "db  SVC_COMMITFILE" \
    "db  NOT SVC_COMMITFILE" \
    parm [bx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 22: Find first file using FCB (INT 21h AH=11h)                            */
/*     https://osfree.org/doku/en:docs:mvm:api:22                            */
/*     Input: FCB at DS:DX                                                   */
/*     Returns: AL = 0 if found, 0xFF if error                               */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcFindFirstFCB(void far *fcb);
#pragma aux SvcFindFirstFCB = \
    "hlt" \
    "db  SVC_FINDFIRSTFCB" \
    "db  NOT SVC_FINDFIRSTFCB" \
    parm [ds dx] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 23: Find next file using FCB (INT 21h AH=12h)                             */
/*     https://osfree.org/doku/en:docs:mvm:api:23                            */
/*     Input: FCB at DS:DX                                                   */
/*     Returns: AL = 0 if found, 0xFF if error                               */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcFindNextFCB(void far *fcb);
#pragma aux SvcFindNextFCB = \
    "hlt" \
    "db  SVC_FINDNEXTFCB" \
    "db  NOT SVC_FINDNEXTFCB" \
    parm [ds dx] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 25: Find first file (INT 21h AH=4Eh)                                      */
/*     https://osfree.org/doku/en:docs:mvm:api:25                            */
/*     Input: path (DS:DX), attributes (CX)                                  */
/* ------------------------------------------------------------------------- */
extern void SvcFindFirst(const char far *path, unsigned short attrib);
#pragma aux SvcFindFirst = \
    "hlt" \
    "db  SVC_FINDFIRST" \
    "db  NOT SVC_FINDFIRST" \
    parm [ds dx] [cx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 28: Open file (INT 21h AH=3Dh)                                            */
/*     https://osfree.org/doku/en:docs:mvm:api:28                            */
/*     Input: path (DS:DX), access mode (AL)                                 */
/*     Returns: file handle (AX)                                             */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcOpen(const char far *path, unsigned char access);
#pragma aux SvcOpen = \
    "hlt" \
    "db  SVC_OPEN" \
    "db  NOT SVC_OPEN" \
    parm [ds dx] [al] \
    value [ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 29: Delete file using FCB (INT 21h AH=13h)                                */
/*     https://osfree.org/doku/en:docs:mvm:api:29                            */
/*     Input: FCB at DS:DX                                                   */
/*     Returns: AL = 0 if success, 0xFF if error                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcDeleteFCB(void far *fcb);
#pragma aux SvcDeleteFCB = \
    "hlt" \
    "db  SVC_DELETEFCB" \
    "db  NOT SVC_DELETEFCB" \
    parm [ds dx] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 30: Rename file using FCB (INT 21h AH=17h)                                */
/*     https://osfree.org/doku/en:docs:mvm:api:30                            */
/*     Input: modified FCB at DS:DX (second filename at offset 0x10)         */
/*     Returns: AL = 0 if success, 0xFF if error                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcRenameFCB(void far *fcb);
#pragma aux SvcRenameFCB = \
    "hlt" \
    "db  SVC_RENAMEFCB" \
    "db  NOT SVC_RENAMEFCB" \
    parm [ds dx] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 31: Delete file (INT 21h AH=41h)                                          */
/*     https://osfree.org/doku/en:docs:mvm:api:31                            */
/*     Input: ASCIZ path at DS:DX                                            */
/* ------------------------------------------------------------------------- */
extern void SvcDelete(const char far *path);
#pragma aux SvcDelete = \
    "hlt" \
    "db  SVC_DELETE" \
    "db  NOT SVC_DELETE" \
    parm [ds dx] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 32: Rename file (INT 21h AH=56h)                                          */
/*     https://osfree.org/doku/en:docs:mvm:api:32                            */
/*     Input: old path at DS:DX, new path at ES:DI                           */
/* ------------------------------------------------------------------------- */
extern void SvcMove(const char far *oldname, const char far *newname);
#pragma aux SvcMove = \
    "hlt" \
    "db  SVC_MOVE" \
    "db  NOT SVC_MOVE" \
    parm [ds dx] [es di] \
    modify [ax bx cx dx es di];

/* ------------------------------------------------------------------------- */
/* 37: Open file using FCB (INT 21h AH=0Fh)                                  */
/*     https://osfree.org/doku/en:docs:mvm:api:37                            */
/*     Input: unopened FCB at DS:DX                                          */
/*     Returns: AL = 0 if success, 0xFF if error                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcOpenFCB(void far *fcb);
#pragma aux SvcOpenFCB = \
    "hlt" \
    "db  SVC_OPENFCB" \
    "db  NOT SVC_OPENFCB" \
    parm [ds dx] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 39: Close file using FCB (INT 21h AH=10h)                                 */
/*     https://osfree.org/doku/en:docs:mvm:api:39                            */
/*     Input: opened FCB at DS:DX                                            */
/*     Returns: AL = 0 if success, 0xFF if error                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcCloseFCB(void far *fcb);
#pragma aux SvcCloseFCB = \
    "hlt" \
    "db  SVC_CLOSEFCB" \
    "db  NOT SVC_CLOSEFCB" \
    parm [ds dx] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 44: Set verify flag (INT 21h AH=2Eh)                                      */
/*     https://osfree.org/doku/en:docs:mvm:api:44                            */
/*     Input: state (AL: 0=off, 1=on)                                        */
/* ------------------------------------------------------------------------- */
extern void SvcSetVerify(unsigned char state);
#pragma aux SvcSetVerify = \
    "hlt" \
    "db  SVC_SETVERIFY" \
    "db  NOT SVC_SETVERIFY" \
    parm [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 45: Find next file (INT 21h AH=4Fh)                                       */
/*     https://osfree.org/doku/en:docs:mvm:api:45                            */
/* ------------------------------------------------------------------------- */
extern void SvcFindNext(void);
#pragma aux SvcFindNext = \
    "hlt" \
    "db  SVC_FINDNEXT" \
    "db  NOT SVC_FINDNEXT" \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 46: Create file (INT 21h AH=3Ch)                                          */
/*     https://osfree.org/doku/en:docs:mvm:api:46                            */
/*     Input: path (DS:DX), attributes (CX)                                  */
/*     Returns: file handle (AX)                                             */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcCreate(const char far *path, unsigned short attrib);
#pragma aux SvcCreate = \
    "hlt" \
    "db  SVC_CREATE" \
    "db  NOT SVC_CREATE" \
    parm [ds dx] [cx] \
    value [ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 47: Create temporary file (INT 21h AH=5Ah)                                */
/*     https://osfree.org/doku/en:docs:mvm:api:47                            */
/*     Input: path template (DS:DX), attributes (CX)                         */
/*     Returns: file handle (AX)                                             */
/* ------------------------------------------------------------------------- */
extern unsigned short SvcCreateTemp(char far *path, unsigned short attrib);
#pragma aux SvcCreateTemp = \
    "hlt" \
    "db  SVC_CREATETEMP" \
    "db  NOT SVC_CREATETEMP" \
    parm [ds dx] [cx] \
    value [ax] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 49: Parse filename into FCB (INT 21h AH=29h)                              */
/*     https://osfree.org/doku/en:docs:mvm:api:49                            */
/*     Input: string at DS:SI, FCB at ES:DI, control flags in AL (0Fh)       */
/*     Returns: AL = status                                                  */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcParseFileName(const char far *string,
                                      void far *fcb);
#pragma aux SvcParseFileName = \
    "mov al, 0Fh" \
    "hlt" \
    "db  SVC_PARSEFILENAME" \
    "db  NOT SVC_PARSEFILENAME" \
    parm [ds si] [es di] \
    value [al] \
    modify [ax bx cx dx si di es];

/* ------------------------------------------------------------------------- */
/* 61: Absolute disk read (INT 25h)                                          */
/*     https://osfree.org/doku/en:docs:mvm:api:61                            */
/*     Input: drive (AL), buffer at DS:BX, number of sectors (CX),           */
/*            starting sector (DX)                                           */
/*     Returns: error code in AL (0 = success?)                              */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcAbsDiskRead(unsigned char drive,
                                    void far *buffer,
                                    unsigned short numsect,
                                    unsigned short first);
#pragma aux SvcAbsDiskRead = \
    "hlt" \
    "db  SVC_ABSDISKREAD" \
    "db  NOT SVC_ABSDISKREAD" \
    parm [al] [ds bx] [cx] [dx] \
    value [al] \
    modify [ax bx cx dx];

/* ------------------------------------------------------------------------- */
/* 62: Absolute disk write (INT 26h)                                         */
/*     https://osfree.org/doku/en:docs:mvm:api:62                            */
/*     Input: same as SvcAbsDiskRead                                         */
/*     Returns: error code in AL                                             */
/* ------------------------------------------------------------------------- */
extern unsigned char SvcAbsDiskWrite(unsigned char drive,
                                     void far *buffer,
                                     unsigned short numsect,
                                     unsigned short first);
#pragma aux SvcAbsDiskWrite = \
    "hlt" \
    "db  SVC_ABSDISKWRITE" \
    "db  NOT SVC_ABSDISKWRITE" \
    parm [al] [ds bx] [cx] [dx] \
    value [al] \
    modify [ax bx cx dx];

#endif /* SVC_H_INCLUDED */
