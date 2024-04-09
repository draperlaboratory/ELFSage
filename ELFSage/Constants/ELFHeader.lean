import ELFSage.Util.Hex
/- ELF File Types -/

/- ELf file classes.  The file format is designed to be portable among machines
of various sizes, without imposing the sizes of the largest machine on the
smallest. The class of the file defines the basic types used by the data
structures of the object file container itself. -/
inductive ELFHeader.ei_class.values where
  /-- invalid arch specification --/
  | elfclassnone : ELFHeader.ei_class.values
  /-- 32-bit architecture --/
  | elfclass32 : ELFHeader.ei_class.values
  /-- 64-bit architecture --/
  | elfclass64 : ELFHeader.ei_class.values
  /-- invalid arch --/
  | elfclass_other : Nat → ELFHeader.ei_class.values

def ELFHeader.ei_class.fromNat : Nat → ELFHeader.ei_class.values
  | 0 => .elfclassnone
  | 1 => .elfclass32
  | 2 => .elfclass64
  | n => .elfclass_other n

def ELFHeader.ei_class.toNat : ELFHeader.ei_class.values → Nat
  | .elfclassnone     => 0
  | .elfclass32       => 1
  | .elfclass64       => 2
  | .elfclass_other n => n

instance : ToString (ELFHeader.ei_class.values) where
  toString
  | .elfclassnone     => "None"
  | .elfclass32       => "32-bit"
  | .elfclass64       => "64-bit"
  | .elfclass_other n => s!"Unrecognized ei_class: 0x{Hex.toHex n}"

inductive ELFHeader.ei_data.values where
  /-- invalid arch specification --/
  | elfdatanone : ELFHeader.ei_data.values
  /-- 32-bit architecture --/
  | elfdata2lsb : ELFHeader.ei_data.values
  /-- 64-bit architecture --/
  | elfdata2msb : ELFHeader.ei_data.values
  /-- invalid arch --/
  | elfdata_other : Nat → ELFHeader.ei_data.values

/- ELF data encodings.  Byte e_ident[elf_ei_data] specifies the encoding of
   both the data structures used by object file container and data contained in
   object file sections. -/
def ELFHeader.ei_data.fromNat : Nat → ELFHeader.ei_data.values
  | 0 => .elfdatanone
  | 1 => .elfdata2lsb
  | 2 => .elfdata2msb
  | n => .elfdata_other n

def ELFHeader.ei_data.toNat : ELFHeader.ei_data.values → Nat
  | .elfdatanone     => 0
  | .elfdata2lsb     => 1
  | .elfdata2msb     => 2
  | .elfdata_other n => n

instance : ToString (ELFHeader.ei_data.values) where
  toString
  | .elfdatanone     => "None"
  | .elfdata2lsb     => "LittleEndian"
  | .elfdata2msb     => "BigEndian"
  | .elfdata_other n => s!"Unrecognized ei_data: 0x{Hex.toHex n}"

/- OS and ABI versions.  Byte e_ident[elf_ei_osabi] identifies the OS- or
   ABI-specific ELF extensions used by this file. Some fields in other ELF
   structures have flags and values that have operating system and/or ABI
   specific meanings; the interpretation of those fields is determined by the
   value of this byte. -/
inductive ELFHeader.ei_osabi.values where
  /-- No extensions or unspecified -/
  | elfosabi_none : ELFHeader.ei_osabi.values
  /-- Hewlett-Packard HP-UX -/
  | elfosabi_hpux : ELFHeader.ei_osabi.values
  /-- NetBSD -/
  | elfosabi_netbsd : ELFHeader.ei_osabi.values
  /-- GNU -/
  | elfosabi_gnu : ELFHeader.ei_osabi.values
  /-- Sun Solaris -/
  | elfosabi_solaris : ELFHeader.ei_osabi.values
  /-- AIX -/
  | elfosabi_aix : ELFHeader.ei_osabi.values
  /-- IRIX -/
  | elfosabi_irix : ELFHeader.ei_osabi.values
  /-- FreeBSD -/
  | elfosabi_freebsd : ELFHeader.ei_osabi.values
  /-- Compaq Tru64 Unix -/
  | elfosabi_tru64 : ELFHeader.ei_osabi.values
  /-- Novell Modesto -/
  | elfosabi_modesto : ELFHeader.ei_osabi.values
  /-- OpenBSD -/
  | elfosabi_openbsd : ELFHeader.ei_osabi.values
  /-- OpenVMS -/
  | elfosabi_openvms : ELFHeader.ei_osabi.values
  /-- Hewlett-Packard Non-stop Kernel -/
  | elfosabi_nsk : ELFHeader.ei_osabi.values
  /-- Amiga Research OS -/
  | elfosabi_aros : ELFHeader.ei_osabi.values
  /-- FenixOS highly-scalable multi-core OS -/
  | elfosabi_fenixos : ELFHeader.ei_osabi.values
  /-- Nuxi CloudABI -/
  | elfosabi_cloudabi : ELFHeader.ei_osabi.values
  /-- Stratus technologies OpenVOS -/
  | elfosabi_openvos : ELFHeader.ei_osabi.values
  /-- ARM aeabi -/
  | elfosabi_arm_aeabi : ELFHeader.ei_osabi.values
  /-- ARM abi -/
  | elfosabi_arm : ELFHeader.ei_osabi.values
  /-- standalone (embedded) application -/
  | elfosabi_standalone : ELFHeader.ei_osabi.values
  | elfosabi_other : Nat → ELFHeader.ei_osabi.values

def ELFHeader.ei_osabi.fromNat : Nat → ELFHeader.ei_osabi.values
  | 0   => .elfosabi_none
  | 1   => .elfosabi_hpux
  | 2   => .elfosabi_netbsd
  | 3   => .elfosabi_gnu
  | 6   => .elfosabi_solaris
  | 7   => .elfosabi_aix
  | 8   => .elfosabi_irix
  | 9   => .elfosabi_freebsd
  | 10  => .elfosabi_tru64
  | 11  => .elfosabi_modesto
  | 12  => .elfosabi_openbsd
  | 13  => .elfosabi_openvms
  | 14  => .elfosabi_nsk
  | 15  => .elfosabi_aros
  | 16  => .elfosabi_fenixos
  | 17  => .elfosabi_cloudabi
  | 18  => .elfosabi_openvos
  | 64  => .elfosabi_arm_aeabi
  | 97  => .elfosabi_arm
  | 255 => .elfosabi_standalone
  | n   => .elfosabi_other n

def ELFHeader.ei_osabi.toNat : ELFHeader.ei_osabi.values → Nat
  | .elfosabi_none       => 0
  | .elfosabi_hpux       => 1
  | .elfosabi_netbsd     => 2
  | .elfosabi_gnu        => 3
  | .elfosabi_solaris    => 6
  | .elfosabi_aix        => 7
  | .elfosabi_irix       => 8
  | .elfosabi_freebsd    => 9
  | .elfosabi_tru64      => 10
  | .elfosabi_modesto    => 11
  | .elfosabi_openbsd    => 12
  | .elfosabi_openvms    => 13
  | .elfosabi_nsk        => 14
  | .elfosabi_aros       => 15
  | .elfosabi_fenixos    => 16
  | .elfosabi_cloudabi   => 17
  | .elfosabi_openvos    => 18
  | .elfosabi_arm_aeabi  => 64
  | .elfosabi_arm        => 97
  | .elfosabi_standalone => 255
  | .elfosabi_other n    => n
  --TODO: missing: CUDA, GNU/HURD

instance : ToString (ELFHeader.ei_osabi.values) where
  toString
  | .elfosabi_none       => "SystemV"
  | .elfosabi_hpux       => "HPUX"
  | .elfosabi_netbsd     => "NetBSD"
  | .elfosabi_gnu        => "GNU/Linux"
  | .elfosabi_solaris    => "Solaris"
  | .elfosabi_aix        => "AIX"
  | .elfosabi_irix       => "IRIX"
  | .elfosabi_freebsd    => "FREEBSD"
  | .elfosabi_tru64      => "TRU64"
  | .elfosabi_modesto    => "Modesto"
  | .elfosabi_openbsd    => "OpenBSD"
  | .elfosabi_openvms    => "OpenVMS"
  | .elfosabi_nsk        => "NSK"
  | .elfosabi_aros       => "AROS"
  | .elfosabi_fenixos    => "FenixOS"
  | .elfosabi_cloudabi   => "CloudABI"
  | .elfosabi_openvos    => "OpenVOS" --not recognized by llvm-dumpobj?
  | .elfosabi_arm_aeabi  => "ARM AEABI" --not recognized by llvm-dumpobj?
  | .elfosabi_arm        => "ARM"
  | .elfosabi_standalone => "Standalone"
  | .elfosabi_other n    => s!"Unrecognized ei_osabi: {Hex.toHex n}"

inductive ELFHeader.e_type.values where
  /-- No file type -/
  | et_none  : ELFHeader.e_type.values
  /-- Relocatable file -/
  | et_rel   : ELFHeader.e_type.values
  /-- Executable file -/
  | et_exec  : ELFHeader.e_type.values
  /-- Shared object file -/
  | et_dyn   : ELFHeader.e_type.values
  /-- Core file -/
  | et_core  : ELFHeader.e_type.values
  /-- Operating system or processor specific -/
  | et_other : Nat → ELFHeader.e_type.values

/-- Operating-system specific -/
abbrev ELFHeader.e_type.lo_os : Nat := 65024 --0xfe00
/-- Operating-system specific -/
abbrev ELFHeader.e_type.hi_os : Nat := 65279 --0xfeff
/-- Processor specific -/
abbrev ELFHeader.e_type.lo_proc : Nat := 65280 --0xff00
/-- Processor specific -/
abbrev ELFHeader.e_type.hi_proc : Nat := 65535 --0xffff

-- Are there macros for this kind of symmetic pattern match?
def ELFHeader.e_type.fromNat : Nat → ELFHeader.e_type.values
  | 0 => .et_none
  | 1 => .et_rel
  | 2 => .et_exec
  | 3 => .et_dyn
  | 4 => .et_core
  | n => .et_other n

def ELFHeader.e_type.toNat : ELFHeader.e_type.values → Nat
  | .et_none    => 0
  | .et_rel     => 1
  | .et_exec    => 2
  | .et_dyn     => 3
  | .et_core    => 4
  | .et_other n => n

instance : ToString (ELFHeader.e_type.values) where
  toString
  | .et_none    => "None"
  | .et_rel     => "Relocatable"
  | .et_exec    => "Executable"
  | .et_dyn     => "SharedObject"
  | .et_core    => "Core"
  | .et_other n => s!"Unrecognized e_type: 0x{Hex.toHex n}"


/- ELF Machine Architectures -/

/-- RISC-V -/
def ELFHeader.Arch.riscv : Nat := 243
/-- AMD GPU architecture -/
def ELFHeader.Arch.amdgpu : Nat := 224
/-- Moxie processor family -/
def ELFHeader.Arch.moxie : Nat := 223
/-- FTDI Chip FT32 high performance 32-bit RISC architecture -/
def ELFHeader.Arch.ft32 : Nat := 222
/-- Controls and Data Services VISIUMcore processor -/
def ELFHeader.Arch.visium : Nat := 221
/-- Zilog Z80 -/
def ELFHeader.Arch.z80 : Nat := 220
/-- CSR Kalimba architecture family -/
def ELFHeader.Arch.kalimba : Nat := 219
/-- Nanoradio optimised RISC -/
def ELFHeader.Arch.norc : Nat := 218
/-- iCelero CoolEngine -/
def ELFHeader.Arch.cool : Nat := 217
/-- Cognitive Smart Memory Processor -/
def ELFHeader.Arch.coge : Nat := 216
/-- Paneve CDP architecture family -/
def ELFHeader.Arch.cdp : Nat := 215
/-- KM211 KVARC processor -/
def ELFHeader.Arch.kvarc : Nat := 214
/-- KM211 KMX8 8-bit processor -/
def ELFHeader.Arch.kmx8 : Nat := 213
/-- KM211 KMX16 16-bit processor -/
def ELFHeader.Arch.kmx16 : Nat := 212
/-- KM211 KMX32 32-bit processor -/
def ELFHeader.Arch.kmx32 : Nat := 211
/-- KM211 KM32 32-bit processor -/
def ELFHeader.Arch.km32 : Nat := 210
/-- Microchip 8-bit PIC(r) family -/
def ELFHeader.Arch.mchp_pic : Nat := 204
/-- XMOS xCORE processor family -/
def ELFHeader.Arch.xcore : Nat := 203
/-- Beyond BA2 CPU architecture -/
def ELFHeader.Arch.ba2 : Nat := 202
/-- Beyond BA1 CPU architecture *-/
def ELFHeader.Arch.ba1 : Nat := 201
/-- Freescale 56800EX Digital Signal Controller (DSC) -/
def ELFHeader.Arch._5600ex : Nat := 200
/-- 199 Renesas 78KOR family -/
def ELFHeader.Arch._78kor : Nat := 199
/-- Broadcom VideoCore V processor -/
def ELFHeader.Arch.videocore5 : Nat := 198
/-- Renesas RL78 family -/
def ELFHeader.Arch.rl78 : Nat := 197
/-- Open8 8-bit RISC soft processing core -/
def ELFHeader.Arch.open8 : Nat := 196
/-- Synopsys ARCompact V2 -/
def ELFHeader.Arch.arc_compact2 : Nat := 195
/-- KIPO_KAIST Core-A 2nd generation processor family -/
def ELFHeader.Arch.corea_2nd : Nat := 194
/-- KIPO_KAIST Core-A 1st generation processor family -/
def ELFHeader.Arch.corea_1st : Nat := 193
/-- CloudShield architecture family -/
def ELFHeader.Arch.cloudshield : Nat := 192
/-- Infineon Technologies SLE9X core -/
def ELFHeader.Arch.sle9x : Nat := 179
/-- Intel L10M -/
def ELFHeader.Arch.l10m : Nat := 180
/-- Intel K10M -/
def ELFHeader.Arch.k10m : Nat := 181
/-- ARM 64-bit architecture (AARCH64) -/
def ELFHeader.Arch.aarch64 : Nat := 183
/-- Atmel Corporation 32-bit microprocessor family -/
def ELFHeader.Arch.avr32 : Nat := 185
/-- STMicroelectronics STM8 8-bit microcontroller -/
def ELFHeader.Arch.stm8 : Nat := 186
/-- Tilera TILE64 multicore architecture family -/
def ELFHeader.Arch.tile64 : Nat := 187
/-- Tilera TILEPro multicore architecture family -/
def ELFHeader.Arch.tilepro : Nat := 188
/-- Xilinix MicroBlaze 32-bit RISC soft processor core -/
def ELFHeader.Arch.microblaze : Nat := 189
/-- NVIDIA CUDA architecture -/
def ELFHeader.Arch.cuda : Nat := 190
/-- Tilera TILE-Gx multicore architecture family -/
def ELFHeader.Arch.tilegx : Nat := 191
/-- Cypress M8C microprocessor -/
def ELFHeader.Arch.cypress : Nat := 161
/-- Renesas R32C series microprocessors -/
def ELFHeader.Arch.r32c : Nat := 162
/-- NXP Semiconductors TriMedia architecture family -/
def ELFHeader.Arch.trimedia : Nat := 163
/-- QUALCOMM DSP6 processor -/
def ELFHeader.Arch.qdsp6 : Nat := 164
/-- Intel 8051 and variants -/
def ELFHeader.Arch._8051 : Nat := 165
/-- STMicroelectronics STxP7x family of configurable and extensible RISC processors -/
def ELFHeader.Arch.stxp7x : Nat := 166
/-- Andes Technology compact code size embedded RISC processor family -/
def ELFHeader.Arch.nds32 : Nat := 167
/-- Cyan Technology eCOG1X family -/
def ELFHeader.Arch.ecog1x : Nat := 168
/-- Dallas Semiconductor MAXQ30 Core Micro-controllers -/
def ELFHeader.Arch.maxq30 : Nat := 169
/-- New Japan Radio (NJR) 16-bit DSP Processor -/
def ELFHeader.Arch.ximo16 : Nat := 170
/-- M2000 Reconfigurable RISC Microprocessor -/
def ELFHeader.Arch.manik : Nat := 171
/-- Cray Inc. NV2 vector architecture -/
def ELFHeader.Arch.craynv2 : Nat := 172
/-- Renesas RX family -/
def ELFHeader.Arch.rx : Nat := 173
/-- Imagination Technologies META processor architecture -/
def ELFHeader.Arch.metag : Nat := 174
/-- MCST Elbrus general purpose hardware architecture -/
def ELFHeader.Arch.mcst_elbrus : Nat := 175
/-- Cyan Technology eCOG16 family -/
def ELFHeader.Arch.ecog16 : Nat := 176
/-- National Semiconductor CompactRISC CR16 16-bit microprocessor -/
def ELFHeader.Arch.cr16 : Nat := 177
/-- Freescale Extended Time Processing Unit -/
def ELFHeader.Arch.etpu : Nat := 178
/-- Altium TSK3000 core -/
def ELFHeader.Arch.tsk3000 : Nat := 131
/-- Freescale RS08 embedded processor -/
def ELFHeader.Arch.rs08 : Nat := 132
/-- Analog Devices SHARC family of 32-bit DSP processors -/
def ELFHeader.Arch.sharc : Nat := 133
/-- Cyan Technology eCOG2 microprocessor -/
def ELFHeader.Arch.ecog2 : Nat := 134
/-- Sunplus S+core7 RISC processor -/
def ELFHeader.Arch.ccore7 : Nat := 135
/-- New Japan Radio (NJR) 24-bit DSP Processor -/
def ELFHeader.Arch.dsp24 : Nat := 136
/-- Broadcom VideoCore III processor -/
def ELFHeader.Arch.videocore3 : Nat := 137
/-- RISC processor for Lattice FPGA architecture -/
def ELFHeader.Arch.latticemico32 : Nat := 138
/-- Seiko Epson C17 family -/
def ELFHeader.Arch.c17 : Nat := 139
/-- The Texas Instruments TMS320C6000 DSP family -/
def ELFHeader.Arch.c6000 : Nat := 140
/-- The Texas Instruments TMS320C2000 DSP family -/
def ELFHeader.Arch.c2000 : Nat := 141
/-- The Texas Instruments TMS320C55x DSP family -/
def ELFHeader.Arch.c5500 : Nat := 142
/-- STMicroelectronics 64bit VLIW Data Signal Processor -/
def ELFHeader.Arch.mmdsp_plus : Nat := 160
/-- LSI Logic 16-bit DSP Processor -/
def ELFHeader.Arch.zsp : Nat := 79
/-- Donald Knuth's educational 64-bit processor -/
def ELFHeader.Arch.mmix : Nat := 80
/-- Harvard University machine-independent object files -/
def ELFHeader.Arch.huany : Nat := 81
/-- SiTera Prism -/
def ELFHeader.Arch.prism : Nat := 82
/-- Atmel AVR 8-bit microcontroller -/
def ELFHeader.Arch.avr : Nat := 83
/-- Fujitsu FR30 -/
def ELFHeader.Arch.fr30 : Nat := 84
/-- Mitsubishi D10V -/
def ELFHeader.Arch.d10v : Nat := 85
/-- Mitsubishi D30V -/
def ELFHeader.Arch.d30v : Nat := 86
/-- NEC v850 -/
def ELFHeader.Arch.v850 : Nat := 87
/-- Mitsubishi M32R -/
def ELFHeader.Arch.m32r : Nat := 88
/-- Matsushita MN10300 -/
def ELFHeader.Arch.mn10300 : Nat := 89
/-- Matsushita MN10200 -/
def ELFHeader.Arch.mn10200 : Nat := 90
/-- picoJava -/
def ELFHeader.Arch.pj : Nat := 91
/-- OpenRISC 32-bit embedded processor -/
def ELFHeader.Arch.openrisc : Nat := 92
/-- ARC International ARCompact processor (old spelling/synonym: ELF_MA_ARC_A5) -/
def ELFHeader.Arch.arc_compact : Nat := 93
/-- Tensilica Xtensa Architecture -/
def ELFHeader.Arch.xtensa : Nat := 94
/-- Alphamosaic VideoCore processor -/
def ELFHeader.Arch.videocore : Nat := 95
/-- Thompson Multimedia General Purpose Processor -/
def ELFHeader.Arch.tmm_gpp : Nat := 96
/-- National Semiconductor 32000 series -/
def ELFHeader.Arch.ns32k : Nat := 97
/-- Tenor Network TPC processor -/
def ELFHeader.Arch.tpc : Nat := 98
/-- Trebia SNP 1000 processor -/
def ELFHeader.Arch.snp1k : Nat := 99
/-- STMicroelectronics ST200 microcontroller -/
def ELFHeader.Arch.st200 : Nat := 100
/-- Ubicom IP2xxx microcontroller family -/
def ELFHeader.Arch.ip2k : Nat := 101
/-- MAX Processor -/
def ELFHeader.Arch.max : Nat := 102
/-- National Semiconductor CompactRISC microprocessor -/
def ELFHeader.Arch.cr : Nat := 103
/-- Fujitsu F2MC16 -/
def ELFHeader.Arch.f2mc16 : Nat := 104
/-- Texas Instruments embedded microcontroller msp430 -/
def ELFHeader.Arch.msp430 : Nat := 105
/-- Analog Devices Blackfin (DSP) processor -/
def ELFHeader.Arch.blackfin : Nat := 106
/-- S1C33 Family of Seiko Epson processors -/
def ELFHeader.Arch.se_c33 : Nat := 107
/-- Sharp embedded microprocessor -/
def ELFHeader.Arch.sep : Nat := 108
/-- Arca RISC Microprocessor -/
def ELFHeader.Arch.arca : Nat := 109
/-- Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University -/
def ELFHeader.Arch.unicore : Nat := 110
/-- eXcess: 16/32/64-bit configurable embedded CPU -/
def ELFHeader.Arch.excess : Nat := 111
/-- Icera Semiconductor Inc. Deep Execution Processor -/
def ELFHeader.Arch.dxp : Nat := 112
/-- Altera Nios II soft-core processor -/
def ELFHeader.Arch.altera_nios2 : Nat := 113
/-- National Semiconductor CompactRISC CRX microprocessor -/
def ELFHeader.Arch.crx : Nat := 114
/-- Motorola XGATE embedded processor -/
def ELFHeader.Arch.xgate : Nat := 115
/-- Infineon C16x/XC16x processor -/
def ELFHeader.Arch.c166 : Nat := 116
/-- Renesas M16C series microprocessors -/
def ELFHeader.Arch.m16c : Nat := 117
/-- Microchip Technology dsPIC30F Digital Signal Controller -/
def ELFHeader.Arch.dspic30f : Nat := 118
/-- Freescale Communication Engine RISC core -/
def ELFHeader.Arch.ce : Nat := 119
/-- Renesas M32C series microprocessors -/
def ELFHeader.Arch.m32c : Nat := 120
/-- No machine -/
def ELFHeader.Arch.none : Nat := 0
/-- AT&T WE 32100 -/
def ELFHeader.Arch.m32 : Nat := 1
/-- SPARC -/
def ELFHeader.Arch.sparc : Nat := 2
/-- Intel 80386 -/
def ELFHeader.Arch._386 : Nat := 3
/-- Motorola 68000 -/
def ELFHeader.Arch._68k : Nat := 4
/-- Motorola 88000 -/
def ELFHeader.Arch._88k : Nat := 5
/-- Intel 80860 -/
def ELFHeader.Arch._860 : Nat := 7
/-- MIPS I Architecture -/
def ELFHeader.Arch.mips : Nat := 8
/-- IBM System/370 Processor -/
def ELFHeader.Arch.s370 : Nat := 9
/-- MIPS RS3000 Little-endian -/
def ELFHeader.Arch.mips_rs3_le : Nat := 10
/-- Hewlett-Packard PA-RISC -/
def ELFHeader.Arch.parisc : Nat := 15
/-- Fujitsu VPP500 -/
def ELFHeader.Arch.vpp500 : Nat := 17
/-- Enhanced instruction set SPARC -/
def ELFHeader.Arch.sparc32plus : Nat := 18
/-- Intel 80960 -/
def ELFHeader.Arch._960 : Nat := 19
/-- PowerPC -/
def ELFHeader.Arch.ppc : Nat := 20
/-- 64-bit PowerPC -/
def ELFHeader.Arch.ppc64 : Nat := 21
/-- IBM System/390 Processor -/
def ELFHeader.Arch.s390 : Nat := 22
/-- IBM SPU/SPC -/
def ELFHeader.Arch.spu : Nat := 23
/-- NEC V800 -/
def ELFHeader.Arch.v800 : Nat := 36
/-- Fujitsu FR20 -/
def ELFHeader.Arch.fr20 : Nat := 37
/-- TRW RH-32 -/
def ELFHeader.Arch.rh32 : Nat := 38
/-- Motorola RCE -/
def ELFHeader.Arch.rce : Nat := 39
/-- ARM 32-bit architecture (AARCH32) -/
def ELFHeader.Arch.arm : Nat := 40
/-- Digital Alpha -/
def ELFHeader.Arch.alpha : Nat := 41
/-- Hitachi SH -/
def ELFHeader.Arch.sh : Nat := 42
/-- SPARC Version 9 -/
def ELFHeader.Arch.sparcv9 : Nat := 43
/-- Siemens TriCore embedded processor -/
def ELFHeader.Arch.tricore : Nat := 44
/-- Argonaut RISC Core, Argonaut Technologies Inc. -/
def ELFHeader.Arch.arc : Nat := 45
/-- Hitachi H8/300 -/
def ELFHeader.Arch.h8_300 : Nat := 46
/-- Hitachi H8/300H -/
def ELFHeader.Arch.h8_300h : Nat := 47
/-- Hitachi H8S -/
def ELFHeader.Arch.h8s : Nat := 48
/-- Hitachi H8/500 -/
def ELFHeader.Arch.h8_500 : Nat := 49
/-- Intel IA-64 processor architecture -/
def ELFHeader.Arch.ia_64 : Nat := 50
/-- Stanford MIPS-X -/
def ELFHeader.Arch.mips_x : Nat := 51
/-- Motorola ColdFire -/
def ELFHeader.Arch.coldfire : Nat := 52
/-- Motorola M68HC12 -/
def ELFHeader.Arch._68hc12 : Nat := 53
/-- Fujitsu MMA Multimedia Accelerator -/
def ELFHeader.Arch.mma : Nat := 54
/-- Siemens PCP -/
def ELFHeader.Arch.pcp : Nat := 55
/-- Sony nCPU embedded RISC processor -/
def ELFHeader.Arch.ncpu : Nat := 56
/-- Denso NDR1 microprocessor -/
def ELFHeader.Arch.ndr1 : Nat := 57
/-- Motorola Star*Core processor -/
def ELFHeader.Arch.starcore : Nat := 58
/-- Toyota ME16 processor -/
def ELFHeader.Arch.me16 : Nat := 59
/-- STMicroelectronics ST100 processor -/
def ELFHeader.Arch.st100 : Nat := 60
/-- Advanced Logic Corp. TinyJ embedded processor family -/
def ELFHeader.Arch.tinyj : Nat := 61
/-- AMD x86-64 architecture -/
def ELFHeader.Arch.x86_64 : Nat := 62
/-- Sony DSP Processor -/
def ELFHeader.Arch.pdsp : Nat := 63
/-- Digital Equipment Corp. PDP-10 -/
def ELFHeader.Arch.pdp10 : Nat := 64
/-- Digital Equipment Corp. PDP-11 -/
def ELFHeader.Arch.pdp11 : Nat := 65
/-- Siemens FX66 microcontroller -/
def ELFHeader.Arch.fx66 : Nat := 66
/-- STMicroelectronics ST9+ 8/16 bit microcontroller -/
def ELFHeader.Arch.st9plus : Nat := 67
/-- STMicroelectronics ST7 8-bit microcontroller -/
def ELFHeader.Arch.st7 : Nat := 68
/-- Motorola MC68HC16 Microcontroller -/
def ELFHeader.Arch._68hc16 : Nat := 69
/-- Motorola MC68HC11 Microcontroller -/
def ELFHeader.Arch._68hc11 : Nat := 70
/-- Motorola MC68HC08 Microcontroller -/
def ELFHeader.Arch._68hc08 : Nat := 71
/-- Motorola MC68HC05 Microcontroller -/
def ELFHeader.Arch._68hc05 : Nat := 72
/-- Silicon Graphics SVx -/
def ELFHeader.Arch.svx : Nat := 73
/-- STMicroelectronics ST19 8-bit microcontroller -/
def ELFHeader.Arch.st19 : Nat := 74
/-- Digital VAX -/
def ELFHeader.Arch.vax : Nat := 75
/-- Axis Communications 32-bit embedded processor -/
def ELFHeader.Arch.cris : Nat := 76
/-- Infineon Technologies 32-bit embedded processor -/
def ELFHeader.Arch.javelin : Nat := 77
/-- Element 14 64-bit DSP Processor -/
def ELFHeader.Arch.firepath : Nat := 78
/-- Reserved by Intel -/
def ELFHeader.Arch.intel209 : Nat := 209
/-- Reserved by Intel -/
def ELFHeader.Arch.intel208 : Nat := 208
/-- Reserved by Intel -/
def ELFHeader.Arch.intel207 : Nat := 207
/-- Reserved by Intel -/
def ELFHeader.Arch.intel206 : Nat := 206
/-- Reserved by Intel -/
def ELFHeader.Arch.intel205 : Nat := 205
/-- Reserved by Intel -/
def ELFHeader.Arch.intel182 : Nat := 182
/-- Reserved by ARM -/
def ELFHeader.Arch.arm184 : Nat := 184
/-- Reserved for future use -/
def ELFHeader.Arch.reserved6 : Nat := 6
/-- Reserved for future use -/
def ELFHeader.Arch.reserved11 : Nat := 11
/-- Reserved for future use -/
def ELFHeader.Arch.reserved12 : Nat := 12
/-- Reserved for future use -/
def ELFHeader.Arch.reserved13 : Nat := 13
/-- Reserved for future use -/
def ELFHeader.Arch.reserved14 : Nat := 14
/-- Reserved for future use -/
def ELFHeader.Arch.reserved16 : Nat := 16
/-- Reserved for future use -/
def ELFHeader.Arch.reserved24 : Nat := 24
/-- Reserved for future use -/
def ELFHeader.Arch.reserved25 : Nat := 25
/-- Reserved for future use -/
def ELFHeader.Arch.reserved26 : Nat := 26
/-- Reserved for future use -/
def ELFHeader.Arch.reserved27 : Nat := 27
/-- Reserved for future use -/
def ELFHeader.Arch.reserved28 : Nat := 28
/-- Reserved for future use -/
def ELFHeader.Arch.reserved29 : Nat := 29
/-- Reserved for future use -/
def ELFHeader.Arch.reserved30 : Nat := 30
/-- Reserved for future use -/
def ELFHeader.Arch.reserved31 : Nat := 31
/-- Reserved for future use -/
def ELFHeader.Arch.reserved32 : Nat := 32
/-- Reserved for future use -/
def ELFHeader.Arch.reserved33 : Nat := 33
/-- Reserved for future use -/
def ELFHeader.Arch.reserved34 : Nat := 34
/-- Reserved for future use -/
def ELFHeader.Arch.reserved35 : Nat := 35
/-- Reserved for future use -/
def ELFHeader.Arch.reserved121 : Nat := 121
/-- Reserved for future use -/
def ELFHeader.Arch.reserved122 : Nat := 122
/-- Reserved for future use -/
def ELFHeader.Arch.reserved123 : Nat := 123
/-- Reserved for future use -/
def ELFHeader.Arch.reserved124 : Nat := 124
/-- Reserved for future use -/
def ELFHeader.Arch.reserved125 : Nat := 125
/-- Reserved for future use -/
def ELFHeader.Arch.reserved126 : Nat := 126
/-- Reserved for future use -/
def ELFHeader.Arch.reserved127 : Nat := 127
/-- Reserved for future use -/
def ELFHeader.Arch.reserved128 : Nat := 128
/-- Reserved for future use -/
def ELFHeader.Arch.reserved129 : Nat := 129
/-- Reserved for future use -/
def ELFHeader.Arch.reserved130 : Nat := 130
/-- Reserved for future use -/
def ELFHeader.Arch.reserved143 : Nat := 143
/-- Reserved for future use -/
def ELFHeader.Arch.reserved144 : Nat := 144
/-- Reserved for future use -/
def ELFHeader.Arch.reserved145 : Nat := 145
/-- Reserved for future use -/
def ELFHeader.Arch.reserved146 : Nat := 146
/-- Reserved for future use -/
def ELFHeader.Arch.reserved147 : Nat := 147
/-- Reserved for future use -/
def ELFHeader.Arch.reserved148 : Nat := 148
/-- Reserved for future use -/
def ELFHeader.Arch.reserved149 : Nat := 149
/-- Reserved for future use -/
def ELFHeader.Arch.reserved150 : Nat := 150
/-- Reserved for future use -/
def ELFHeader.Arch.reserved151 : Nat := 151
/-- Reserved for future use -/
def ELFHeader.Arch.reserved152 : Nat := 152
/-- Reserved for future use -/
def ELFHeader.Arch.reserved153 : Nat := 153
/-- Reserved for future use -/
def ELFHeader.Arch.reserved154 : Nat := 154
/-- Reserved for future use -/
def ELFHeader.Arch.reserved155 : Nat := 155
/-- Reserved for future use -/
def ELFHeader.Arch.reserved156 : Nat := 156
/-- Reserved for future use -/
def ELFHeader.Arch.reserved157 : Nat := 157
/-- Reserved for future use -/
def ELFHeader.Arch.reserved158 : Nat := 158
/-- Reserved for future use -/
def ELFHeader.Arch.reserved159 : Nat := 159

/- ELF Format Versions -/

/-- Invalid version -/
def ELFHeader.Version.none : Nat := 0
/-- Current version -/
def ELFHeader.Version.current : Nat := 1

/- Identification indices.  The initial bytes of an ELF header (and an object
  file) correspond to the e_ident member.
-/

/-- File identification -/
abbrev ELFHeader.Ident.mag0 : Nat := 0
/-- File identification -/
abbrev ELFHeader.Ident.mag1 : Nat := 1
/-- File identification -/
abbrev ELFHeader.Ident.mag2 : Nat := 2
/-- File identification -/
abbrev ELFHeader.Ident.mag3 : Nat := 3
/-- File class -/
abbrev ELFHeader.Ident.class : Nat := 4
/-- Data encoding -/
abbrev ELFHeader.Ident.data : Nat := 5
/-- File version -/
abbrev ELFHeader.Ident.version : Nat := 6
/-- Operating system/ABI identification -/
abbrev ELFHeader.Ident.osabi : Nat := 7
/-- ABI version -/
abbrev ELFHeader.Ident.abiversion : Nat := 8
/-- Start of padding bytes -/
abbrev ELFHeader.Ident.pad : Nat := 9
/-- Size of e*_ident[] -/
abbrev ELFHeader.Ident.nident : Nat := 16

/- ELF Header type -/

/-- [ei_nident] is the fixed length of the identification field in the
[elf32_ehdr] type. -/

abbrev ELFHeader.Type.ei_nident : Nat := 16
