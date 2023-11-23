DOSKRNL
=======

osFree DOS Kernel (DOSKRNL) based on FreeDOS kernel.

The DOSKRNL implements the core MS/PC-DOS (R) compatible functions for Multiple Virtual Machine.
It is derived from The FreeDOS kernel which, in turn, is derived from Pat Villani's DOS-C kernel
and released under the GPL v2.

Main targets
------------

1. Integrate to osFree build system
2. Rework initialization (use DOSKRNL init structure, XMS VDD initialization, remove XMS handling from CONFIG.SYS)
3. Remove CONFIG.SYS parsing (DOSKRNL uses settings from properties) as well as device driver loading from settings
4. Replace FAT FS by calls to OS/2 host (Seems SVC must be used here)
5. Implement MVDM extensions to API (see RBIL for more information)
6. 
