DOSKRNL
=======

osFree DOS Kernel (DOSKRNL) based on FreeDOS kernel.

The DOSKRNL implements the core MS/PC-DOS (R) compatible functions for Multiple Virtual Machine.
It is derived from The FreeDOS kernel which, in turn, is derived from Pat Villani's DOS-C kernel
and released under the GPL v2.

Main targets/To Do
------------------

[x] Integrate to osFree build system
[ ] Remove CONFIG structure (We don't need it for DOSKRNL, may be LBA only)
[ ] Rework initialization (use DOSKRNL init structure, XMS VDD initialization, remove XMS handling from CONFIG.SYS)
[ ] Use stack, passed in on init, not init_tos stack
[x] Remove CONFIG.SYS parsing (DOSKRNL uses settings from properties) as well as device driver loading from settings
[ ] Implement DOS Properties/Settings support
[ ] Replace FAT FS by calls to OS/2 host (Seems SVC must be used here)
[ ] Implement MVDM extensions to API (see RBIL for more information)
[ ] Drop-in replacement of DOSKRNL

