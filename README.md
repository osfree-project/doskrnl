DOSKRNL
=======

osFree DOS Kernel (DOSKRNL) based on FreeDOS kernel.

The DOSKRNL implements the core MS/PC-DOS (R) compatible functions for Multiple Virtual Machine.
It is derived from The FreeDOS kernel which, in turn, is derived from Pat Villani's DOS-C kernel
and released under the GPL v2.

Memory model
------------

Unlike FreeDOS kernel DOSKRNL uses RAW file and two group model. Whole image consist of
DGROUP and I_GROUP. I_GROUP moved to hight conventional memory on init. HMA_* segments
moved HMA if XMS initialization was ok. If not, then HMA resides at low addresses.

Main targets/To Do
------------------

  - [x] Integrate to osFree build system
  - [x] Remove CONFIG structure (We don't need it for DOSKRNL, may be LBA only)
  - [x] Rework initialization (use DOSKRNL init structure, XMS VDD initialization
  - [ ] Remove XMS handling from CONFIG.SYS) and HMA relocation
  - [x] Use stack, passed in on init, not init_tos stack
  - [x] Remove CONFIG.SYS parsing (DOSKRNL uses settings from properties) as well as device driver loading from settings
  - [ ] Implement virtual device driver init via init structure
  - [ ] Implement DOS device driver loading via init structure
  - [ ] Implement DOS Properties/Settings support
  - [ ] Replace FAT FS by calls to OS/2 host (Seems SVC must be used here)
  - [ ] Implement MVDM extensions to API (see RBIL for more information)
  - [ ] Drop-in replacement of DOSKRNL

