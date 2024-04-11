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
  /-- unknown data encoding --/
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
  /-- invalid data encoding --/
  | elfdatanone : ELFHeader.ei_data.values
  /-- twos complement, litteendian (LSB in lowest address)--/
  | elfdata2lsb : ELFHeader.ei_data.values
  /-- twos comp, bigendian (MSB in lowest address) --/
  | elfdata2msb : ELFHeader.ei_data.values
  /-- unknown data encoding --/
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
  /-- unknown OS/ABI version -/
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
  /-- Unknown or Operating system or processor specific -/
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
inductive ELFHeader.e_machine.values  where
  /-- RISC-V -/
  | em_riscv : ELFHeader.e_machine.values
  /-- AMD GPU architecture -/
  | em_amdgpu : ELFHeader.e_machine.values
  /-- Moxie processor family -/
  | em_moxie : ELFHeader.e_machine.values
  /-- FTDI Chip FT32 high performance 32-bit RISC architecture -/
  | em_ft32 : ELFHeader.e_machine.values
  /-- Controls and Data Services VISIUMcore processor -/
  | em_visium : ELFHeader.e_machine.values
  /-- Zilog Z80 -/
  | em_z80 : ELFHeader.e_machine.values
  /-- CSR Kalimba architecture family -/
  | em_kalimba : ELFHeader.e_machine.values
  /-- Nanoradio optimised RISC -/
  | em_norc : ELFHeader.e_machine.values
  /-- iCelero CoolEngine -/
  | em_cool : ELFHeader.e_machine.values
  /-- Cognitive Smart Memory Processor -/
  | em_coge : ELFHeader.e_machine.values
  /-- Paneve CDP architecture family -/
  | em_cdp : ELFHeader.e_machine.values
  /-- KM211 KVARC processor -/
  | em_kvarc : ELFHeader.e_machine.values
  /-- KM211 KMX8 8-bit processor -/
  | em_kmx8 : ELFHeader.e_machine.values
  /-- KM211 KMX16 16-bit processor -/
  | em_kmx16 : ELFHeader.e_machine.values
  /-- KM211 KMX32 32-bit processor -/
  | em_kmx32 : ELFHeader.e_machine.values
  /-- KM211 KM32 32-bit processor -/
  | em_km32 : ELFHeader.e_machine.values
  /-- Microchip 8-bit PIC(r) family -/
  | em_mchp_pic : ELFHeader.e_machine.values
  /-- XMOS xCORE processor family -/
  | em_xcore : ELFHeader.e_machine.values
  /-- Beyond BA2 CPU architecture -/
  | em_ba2 : ELFHeader.e_machine.values
  /-- Beyond BA1 CPU architecture *-/
  | em_ba1 : ELFHeader.e_machine.values
  /-- Freescale 56800EX Digital Signal Controller (DSC) -/
  | em_5600ex : ELFHeader.e_machine.values
  /-- 199 Renesas 78KOR family -/
  | em_78kor : ELFHeader.e_machine.values
  /-- Broadcom VideoCore V processor -/
  | em_videocore5 : ELFHeader.e_machine.values
  /-- Renesas RL78 family -/
  | em_rl78 : ELFHeader.e_machine.values
  /-- Open8 8-bit RISC soft processing core -/
  | em_open8 : ELFHeader.e_machine.values
  /-- Synopsys ARCompact V2 -/
  | em_arc_compact2 : ELFHeader.e_machine.values
  /-- KIPO_KAIST Core-A 2nd generation processor family -/
  | em_corea_2nd : ELFHeader.e_machine.values
  /-- KIPO_KAIST Core-A 1st generation processor family -/
  | em_corea_1st : ELFHeader.e_machine.values
  /-- CloudShield architecture family -/
  | em_cloudshield : ELFHeader.e_machine.values
  /-- Infineon Technologies SLE9X core -/
  | em_sle9x : ELFHeader.e_machine.values
  /-- Intel L10M -/
  | em_l10m : ELFHeader.e_machine.values
  /-- Intel K10M -/
  | em_k10m : ELFHeader.e_machine.values
  /-- ARM 64-bit architecture (AARCH64) -/
  | em_aarch64 : ELFHeader.e_machine.values
  /-- Atmel Corporation 32-bit microprocessor family -/
  | em_avr32 : ELFHeader.e_machine.values
  /-- STMicroelectronics STM8 8-bit microcontroller -/
  | em_stm8 : ELFHeader.e_machine.values
  /-- Tilera TILE64 multicore architecture family -/
  | em_tile64 : ELFHeader.e_machine.values
  /-- Tilera TILEPro multicore architecture family -/
  | em_tilepro : ELFHeader.e_machine.values
  /-- Xilinix MicroBlaze 32-bit RISC soft processor core -/
  | em_microblaze : ELFHeader.e_machine.values
  /-- NVIDIA CUDA architecture -/
  | em_cuda : ELFHeader.e_machine.values
  /-- Tilera TILE-Gx multicore architecture family -/
  | em_tilegx : ELFHeader.e_machine.values
  /-- Cypress M8C microprocessor -/
  | em_cypress : ELFHeader.e_machine.values
  /-- Renesas R32C series microprocessors -/
  | em_r32c : ELFHeader.e_machine.values
  /-- NXP Semiconductors TriMedia architecture family -/
  | em_trimedia : ELFHeader.e_machine.values
  /-- QUALCOMM DSP6 processor -/
  | em_qdsp6 : ELFHeader.e_machine.values
  /-- Intel 8051 and variants -/
  | em_8051 : ELFHeader.e_machine.values
  /-- STMicroelectronics STxP7x family of configurable and extensible RISC processors -/
  | em_stxp7x : ELFHeader.e_machine.values
  /-- Andes Technology compact code size embedded RISC processor family -/
  | em_nds32 : ELFHeader.e_machine.values
  /-- Cyan Technology eCOG1X family -/
  | em_ecog1x : ELFHeader.e_machine.values
  /-- Dallas Semiconductor MAXQ30 Core Micro-controllers -/
  | em_maxq30 : ELFHeader.e_machine.values
  /-- New Japan Radio (NJR) 16-bit DSP Processor -/
  | em_ximo16 : ELFHeader.e_machine.values
  /-- M2000 Reconfigurable RISC Microprocessor -/
  | em_manik : ELFHeader.e_machine.values
  /-- Cray Inc. NV2 vector architecture -/
  | em_craynv2 : ELFHeader.e_machine.values
  /-- Renesas RX family -/
  | em_rx : ELFHeader.e_machine.values
  /-- Imagination Technologies META processor architecture -/
  | em_metag : ELFHeader.e_machine.values
  /-- MCST Elbrus general purpose hardware architecture -/
  | em_mcst_elbrus : ELFHeader.e_machine.values
  /-- Cyan Technology eCOG16 family -/
  | em_ecog16 : ELFHeader.e_machine.values
  /-- National Semiconductor CompactRISC CR16 16-bit microprocessor -/
  | em_cr16 : ELFHeader.e_machine.values
  /-- Freescale Extended Time Processing Unit -/
  | em_etpu : ELFHeader.e_machine.values
  /-- Altium TSK3000 core -/
  | em_tsk3000 : ELFHeader.e_machine.values
  /-- Freescale RS08 embedded processor -/
  | em_rs08 : ELFHeader.e_machine.values
  /-- Analog Devices SHARC family of 32-bit DSP processors -/
  | em_sharc : ELFHeader.e_machine.values
  /-- Cyan Technology eCOG2 microprocessor -/
  | em_ecog2 : ELFHeader.e_machine.values
  /-- Sunplus S+core7 RISC processor -/
  | em_ccore7 : ELFHeader.e_machine.values
  /-- New Japan Radio (NJR) 24-bit DSP Processor -/
  | em_dsp24 : ELFHeader.e_machine.values
  /-- Broadcom VideoCore III processor -/
  | em_videocore3 : ELFHeader.e_machine.values
  /-- RISC processor for Lattice FPGA architecture -/
  | em_latticemico32 : ELFHeader.e_machine.values
  /-- Seiko Epson C17 family -/
  | em_c17 : ELFHeader.e_machine.values
  /-- The Texas Instruments TMS320C6000 DSP family -/
  | em_c6000 : ELFHeader.e_machine.values
  /-- The Texas Instruments TMS320C2000 DSP family -/
  | em_c2000 : ELFHeader.e_machine.values
  /-- The Texas Instruments TMS320C55x DSP family -/
  | em_c5500 : ELFHeader.e_machine.values
  /-- STMicroelectronics 64bit VLIW Data Signal Processor -/
  | em_mmdsp_plus : ELFHeader.e_machine.values
  /-- LSI Logic 16-bit DSP Processor -/
  | em_zsp : ELFHeader.e_machine.values
  /-- Donald Knuth's educational 64-bit processor -/
  | em_mmix : ELFHeader.e_machine.values
  /-- Harvard University machine-independent object files -/
  | em_huany : ELFHeader.e_machine.values
  /-- SiTera Prism -/
  | em_prism : ELFHeader.e_machine.values
  /-- Atmel AVR 8-bit microcontroller -/
  | em_avr : ELFHeader.e_machine.values
  /-- Fujitsu FR30 -/
  | em_fr30 : ELFHeader.e_machine.values
  /-- Mitsubishi D10V -/
  | em_d10v : ELFHeader.e_machine.values
  /-- Mitsubishi D30V -/
  | em_d30v : ELFHeader.e_machine.values
  /-- NEC v850 -/
  | em_v850 : ELFHeader.e_machine.values
  /-- Mitsubishi M32R -/
  | em_m32r : ELFHeader.e_machine.values
  /-- Matsushita MN10300 -/
  | em_mn10300 : ELFHeader.e_machine.values
  /-- Matsushita MN10200 -/
  | em_mn10200 : ELFHeader.e_machine.values
  /-- picoJava -/
  | em_pj : ELFHeader.e_machine.values
  /-- OpenRISC 32-bit embedded processor -/
  | em_openrisc : ELFHeader.e_machine.values
  /-- ARC International ARCompact processor (old spelling/synonym: ELF_MA_ARC_A5) -/
  | em_arc_compact : ELFHeader.e_machine.values
  /-- Tensilica Xtensa Architecture -/
  | em_xtensa : ELFHeader.e_machine.values
  /-- Alphamosaic VideoCore processor -/
  | em_videocore : ELFHeader.e_machine.values
  /-- Thompson Multimedia General Purpose Processor -/
  | em_tmm_gpp : ELFHeader.e_machine.values
  /-- National Semiconductor 32000 series -/
  | em_ns32k : ELFHeader.e_machine.values
  /-- Tenor Network TPC processor -/
  | em_tpc : ELFHeader.e_machine.values
  /-- Trebia SNP 1000 processor -/
  | em_snp1k : ELFHeader.e_machine.values
  /-- STMicroelectronics ST200 microcontroller -/
  | em_st200 : ELFHeader.e_machine.values
  /-- Ubicom IP2xxx microcontroller family -/
  | em_ip2k : ELFHeader.e_machine.values
  /-- MAX Processor -/
  | em_max : ELFHeader.e_machine.values
  /-- National Semiconductor CompactRISC microprocessor -/
  | em_cr : ELFHeader.e_machine.values
  /-- Fujitsu F2MC16 -/
  | em_f2mc16 : ELFHeader.e_machine.values
  /-- Texas Instruments embedded microcontroller msp430 -/
  | em_msp430 : ELFHeader.e_machine.values
  /-- Analog Devices Blackfin (DSP) processor -/
  | em_blackfin : ELFHeader.e_machine.values
  /-- S1C33 Family of Seiko Epson processors -/
  | em_se_c33 : ELFHeader.e_machine.values
  /-- Sharp embedded microprocessor -/
  | em_sep : ELFHeader.e_machine.values
  /-- Arca RISC Microprocessor -/
  | em_arca : ELFHeader.e_machine.values
  /-- Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University -/
  | em_unicore : ELFHeader.e_machine.values
  /-- eXcess: 16/32/64-bit configurable embedded CPU -/
  | em_excess : ELFHeader.e_machine.values
  /-- Icera Semiconductor Inc. Deep Execution Processor -/
  | em_dxp : ELFHeader.e_machine.values
  /-- Altera Nios II soft-core processor -/
  | em_altera_nios2 : ELFHeader.e_machine.values
  /-- National Semiconductor CompactRISC CRX microprocessor -/
  | em_crx : ELFHeader.e_machine.values
  /-- Motorola XGATE embedded processor -/
  | em_xgate : ELFHeader.e_machine.values
  /-- Infineon C16x/XC16x processor -/
  | em_c166 : ELFHeader.e_machine.values
  /-- Renesas M16C series microprocessors -/
  | em_m16c : ELFHeader.e_machine.values
  /-- Microchip Technology dsPIC30F Digital Signal Controller -/
  | em_dspic30f : ELFHeader.e_machine.values
  /-- Freescale Communication Engine RISC core -/
  | em_ce : ELFHeader.e_machine.values
  /-- Renesas M32C series microprocessors -/
  | em_m32c : ELFHeader.e_machine.values
  /-- No machine -/
  | em_none : ELFHeader.e_machine.values
  /-- AT&T WE 32100 -/
  | em_m32 : ELFHeader.e_machine.values
  /-- SPARC -/
  | em_sparc : ELFHeader.e_machine.values
  /-- Intel 80386 -/
  | em_386 : ELFHeader.e_machine.values
  /-- Motorola 68000 -/
  | em_68k : ELFHeader.e_machine.values
  /-- Motorola 88000 -/
  | em_88k : ELFHeader.e_machine.values
  /-- Intel 80860 -/
  | em_860 : ELFHeader.e_machine.values
  /-- MIPS I Architecture -/
  | em_mips : ELFHeader.e_machine.values
  /-- IBM System/370 Processor -/
  | em_s370 : ELFHeader.e_machine.values
  /-- MIPS RS3000 Little-endian -/
  | em_mips_rs3_le : ELFHeader.e_machine.values
  /-- Hewlett-Packard PA-RISC -/
  | em_parisc : ELFHeader.e_machine.values
  /-- Fujitsu VPP500 -/
  | em_vpp500 : ELFHeader.e_machine.values
  /-- Enhanced instruction set SPARC -/
  | em_sparc32plus : ELFHeader.e_machine.values
  /-- Intel 80960 -/
  | em_960 : ELFHeader.e_machine.values
  /-- PowerPC -/
  | em_ppc : ELFHeader.e_machine.values
  /-- 64-bit PowerPC -/
  | em_ppc64 : ELFHeader.e_machine.values
  /-- IBM System/390 Processor -/
  | em_s390 : ELFHeader.e_machine.values
  /-- IBM SPU/SPC -/
  | em_spu : ELFHeader.e_machine.values
  /-- NEC V800 -/
  | em_v800 : ELFHeader.e_machine.values
  /-- Fujitsu FR20 -/
  | em_fr20 : ELFHeader.e_machine.values
  /-- TRW RH-32 -/
  | em_rh32 : ELFHeader.e_machine.values
  /-- Motorola RCE -/
  | em_rce : ELFHeader.e_machine.values
  /-- ARM 32-bit architecture (AARCH32) -/
  | em_arm : ELFHeader.e_machine.values
  /-- Digital Alpha -/
  | em_alpha : ELFHeader.e_machine.values
  /-- Hitachi SH -/
  | em_sh : ELFHeader.e_machine.values
  /-- SPARC Version 9 -/
  | em_sparcv9 : ELFHeader.e_machine.values
  /-- Siemens TriCore embedded processor -/
  | em_tricore : ELFHeader.e_machine.values
  /-- Argonaut RISC Core, Argonaut Technologies Inc. -/
  | em_arc : ELFHeader.e_machine.values
  /-- Hitachi H8/300 -/
  | em_h8_300 : ELFHeader.e_machine.values
  /-- Hitachi H8/300H -/
  | em_h8_300h : ELFHeader.e_machine.values
  /-- Hitachi H8S -/
  | em_h8s : ELFHeader.e_machine.values
  /-- Hitachi H8/500 -/
  | em_h8_500 : ELFHeader.e_machine.values
  /-- Intel IA-64 processor architecture -/
  | em_ia_64 : ELFHeader.e_machine.values
  /-- Stanford MIPS-X -/
  | em_mips_x : ELFHeader.e_machine.values
  /-- Motorola ColdFire -/
  | em_coldfire : ELFHeader.e_machine.values
  /-- Motorola M68HC12 -/
  | em_68hc12 : ELFHeader.e_machine.values
  /-- Fujitsu MMA Multimedia Accelerator -/
  | em_mma : ELFHeader.e_machine.values
  /-- Siemens PCP -/
  | em_pcp : ELFHeader.e_machine.values
  /-- Sony nCPU embedded RISC processor -/
  | em_ncpu : ELFHeader.e_machine.values
  /-- Denso NDR1 microprocessor -/
  | em_ndr1 : ELFHeader.e_machine.values
  /-- Motorola Star*Core processor -/
  | em_starcore : ELFHeader.e_machine.values
  /-- Toyota ME16 processor -/
  | em_me16 : ELFHeader.e_machine.values
  /-- STMicroelectronics ST100 processor -/
  | em_st100 : ELFHeader.e_machine.values
  /-- Advanced Logic Corp. TinyJ embedded processor family -/
  | em_tinyj : ELFHeader.e_machine.values
  /-- AMD x86-64 architecture -/
  | em_x86_64 : ELFHeader.e_machine.values
  /-- Sony DSP Processor -/
  | em_pdsp : ELFHeader.e_machine.values
  /-- Digital Equipment Corp. PDP-10 -/
  | em_pdp10 : ELFHeader.e_machine.values
  /-- Digital Equipment Corp. PDP-11 -/
  | em_pdp11 : ELFHeader.e_machine.values
  /-- Siemens FX66 microcontroller -/
  | em_fx66 : ELFHeader.e_machine.values
  /-- STMicroelectronics ST9+ 8/16 bit microcontroller -/
  | em_st9plus : ELFHeader.e_machine.values
  /-- STMicroelectronics ST7 8-bit microcontroller -/
  | em_st7 : ELFHeader.e_machine.values
  /-- Motorola MC68HC16 Microcontroller -/
  | em_68hc16 : ELFHeader.e_machine.values
  /-- Motorola MC68HC11 Microcontroller -/
  | em_68hc11 : ELFHeader.e_machine.values
  /-- Motorola MC68HC08 Microcontroller -/
  | em_68hc08 : ELFHeader.e_machine.values
  /-- Motorola MC68HC05 Microcontroller -/
  | em_68hc05 : ELFHeader.e_machine.values
  /-- Silicon Graphics SVx -/
  | em_svx : ELFHeader.e_machine.values
  /-- STMicroelectronics ST19 8-bit microcontroller -/
  | em_st19 : ELFHeader.e_machine.values
  /-- Digital VAX -/
  | em_vax : ELFHeader.e_machine.values
  /-- Axis Communications 32-bit embedded processor -/
  | em_cris : ELFHeader.e_machine.values
  /-- Infineon Technologies 32-bit embedded processor -/
  | em_javelin : ELFHeader.e_machine.values
  /-- Element 14 64-bit DSP Processor -/
  | em_firepath : ELFHeader.e_machine.values
  /-- Reserved by Intel -/
  | em_intel209 : ELFHeader.e_machine.values
  /-- Reserved by Intel -/
  | em_intel208 : ELFHeader.e_machine.values
  /-- Reserved by Intel -/
  | em_intel207 : ELFHeader.e_machine.values
  /-- Reserved by Intel -/
  | em_intel206 : ELFHeader.e_machine.values
  /-- Reserved by Intel -/
  | em_intel205 : ELFHeader.e_machine.values
  /-- Reserved by Intel -/
  | em_intel182 : ELFHeader.e_machine.values
  /-- Reserved by ARM -/
  | em_arm184 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved6 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved11 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved12 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved13 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved14 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved16 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved24 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved25 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved26 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved27 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved28 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved29 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved30 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved31 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved32 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved33 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved34 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved35 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved121 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved122 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved123 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved124 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved125 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved126 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved127 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved128 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved129 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved130 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved143 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved144 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved145 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved146 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved147 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved148 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved149 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved150 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved151 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved152 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved153 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved154 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved155 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved156 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved157 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved158 : ELFHeader.e_machine.values
  /-- Reserved for future use -/
  | em_reserved159 : ELFHeader.e_machine.values
  /-- Unknown machine type -/
  | em_other : Nat →ELFHeader.e_machine.values

set_option maxHeartbeats 400000

def ELFHeader.e_machine.fromNat : Nat → ELFHeader.e_machine.values
  | 243 => .em_riscv
  | 224 => .em_amdgpu
  | 223 => .em_moxie
  | 222 => .em_ft32
  | 221 => .em_visium
  | 220 => .em_z80
  | 219 => .em_kalimba
  | 218 => .em_norc
  | 217 => .em_cool
  | 216 => .em_coge
  | 215 => .em_cdp
  | 214 => .em_kvarc
  | 213 => .em_kmx8
  | 212 => .em_kmx16
  | 211 => .em_kmx32
  | 210 => .em_km32
  | 204 => .em_mchp_pic
  | 203 => .em_xcore
  | 202 => .em_ba2
  | 201 => .em_ba1
  | 200 => .em_5600ex
  | 199 => .em_78kor
  | 198 => .em_videocore5
  | 197 => .em_rl78
  | 196 => .em_open8
  | 195 => .em_arc_compact2
  | 194 => .em_corea_2nd
  | 193 => .em_corea_1st
  | 192 => .em_cloudshield
  | 179 => .em_sle9x
  | 180 => .em_l10m
  | 181 => .em_k10m
  | 183 => .em_aarch64
  | 185 => .em_avr32
  | 186 => .em_stm8
  | 187 => .em_tile64
  | 188 => .em_tilepro
  | 189 => .em_microblaze
  | 190 => .em_cuda
  | 191 => .em_tilegx
  | 161 => .em_cypress
  | 162 => .em_r32c
  | 163 => .em_trimedia
  | 164 => .em_qdsp6
  | 165 => .em_8051
  | 166 => .em_stxp7x
  | 167 => .em_nds32
  | 168 => .em_ecog1x
  | 169 => .em_maxq30
  | 170 => .em_ximo16
  | 171 => .em_manik
  | 172 => .em_craynv2
  | 173 => .em_rx
  | 174 => .em_metag
  | 175 => .em_mcst_elbrus
  | 176 => .em_ecog16
  | 177 => .em_cr16
  | 178 => .em_etpu
  | 131 => .em_tsk3000
  | 132 => .em_rs08
  | 133 => .em_sharc
  | 134 => .em_ecog2
  | 135 => .em_ccore7
  | 136 => .em_dsp24
  | 137 => .em_videocore3
  | 138 => .em_latticemico32
  | 139 => .em_c17
  | 140 => .em_c6000
  | 141 => .em_c2000
  | 142 => .em_c5500
  | 160 => .em_mmdsp_plus
  | 79 => .em_zsp
  | 80 => .em_mmix
  | 81 => .em_huany
  | 82 => .em_prism
  | 83 => .em_avr
  | 84 => .em_fr30
  | 85 => .em_d10v
  | 86 => .em_d30v
  | 87 => .em_v850
  | 88 => .em_m32r
  | 89 => .em_mn10300
  | 90 => .em_mn10200
  | 91 => .em_pj
  | 92 => .em_openrisc
  | 93 => .em_arc_compact
  | 94 => .em_xtensa
  | 95 => .em_videocore
  | 96 => .em_tmm_gpp
  | 97 => .em_ns32k
  | 98 => .em_tpc
  | 99 => .em_snp1k
  | 100 => .em_st200
  | 101 => .em_ip2k
  | 102 => .em_max
  | 103 => .em_cr
  | 104 => .em_f2mc16
  | 105 => .em_msp430
  | 106 => .em_blackfin
  | 107 => .em_se_c33
  | 108 => .em_sep
  | 109 => .em_arca
  | 110 => .em_unicore
  | 111 => .em_excess
  | 112 => .em_dxp
  | 113 => .em_altera_nios2
  | 114 => .em_crx
  | 115 => .em_xgate
  | 116 => .em_c166
  | 117 => .em_m16c
  | 118 => .em_dspic30f
  | 119 => .em_ce
  | 120 => .em_m32c
  | 0 => .em_none
  | 1 => .em_m32
  | 2 => .em_sparc
  | 3 => .em_386
  | 4 => .em_68k
  | 5 => .em_88k
  | 7 => .em_860
  | 8 => .em_mips
  | 9 => .em_s370
  | 10 => .em_mips_rs3_le
  | 15 => .em_parisc
  | 17 => .em_vpp500
  | 18 => .em_sparc32plus
  | 19 => .em_960
  | 20 => .em_ppc
  | 21 => .em_ppc64
  | 22 => .em_s390
  | 23 => .em_spu
  | 36 => .em_v800
  | 37 => .em_fr20
  | 38 => .em_rh32
  | 39 => .em_rce
  | 40 => .em_arm
  | 41 => .em_alpha
  | 42 => .em_sh
  | 43 => .em_sparcv9
  | 44 => .em_tricore
  | 45 => .em_arc
  | 46 => .em_h8_300
  | 47 => .em_h8_300h
  | 48 => .em_h8s
  | 49 => .em_h8_500
  | 50 => .em_ia_64
  | 51 => .em_mips_x
  | 52 => .em_coldfire
  | 53 => .em_68hc12
  | 54 => .em_mma
  | 55 => .em_pcp
  | 56 => .em_ncpu
  | 57 => .em_ndr1
  | 58 => .em_starcore
  | 59 => .em_me16
  | 60 => .em_st100
  | 61 => .em_tinyj
  | 62 => .em_x86_64
  | 63 => .em_pdsp
  | 64 => .em_pdp10
  | 65 => .em_pdp11
  | 66 => .em_fx66
  | 67 => .em_st9plus
  | 68 => .em_st7
  | 69 => .em_68hc16
  | 70 => .em_68hc11
  | 71 => .em_68hc08
  | 72 => .em_68hc05
  | 73 => .em_svx
  | 74 => .em_st19
  | 75 => .em_vax
  | 76 => .em_cris
  | 77 => .em_javelin
  | 78 => .em_firepath
  | 209 => .em_intel209
  | 208 => .em_intel208
  | 207 => .em_intel207
  | 206 => .em_intel206
  | 205 => .em_intel205
  | 182 => .em_intel182
  | 184 => .em_arm184
  | 6 => .em_reserved6
  | 11 => .em_reserved11
  | 12 => .em_reserved12
  | 13 => .em_reserved13
  | 14 => .em_reserved14
  | 16 => .em_reserved16
  | 24 => .em_reserved24
  | 25 => .em_reserved25
  | 26 => .em_reserved26
  | 27 => .em_reserved27
  | 28 => .em_reserved28
  | 29 => .em_reserved29
  | 30 => .em_reserved30
  | 31 => .em_reserved31
  | 32 => .em_reserved32
  | 33 => .em_reserved33
  | 34 => .em_reserved34
  | 35 => .em_reserved35
  | 121 => .em_reserved121
  | 122 => .em_reserved122
  | 123 => .em_reserved123
  | 124 => .em_reserved124
  | 125 => .em_reserved125
  | 126 => .em_reserved126
  | 127 => .em_reserved127
  | 128 => .em_reserved128
  | 129 => .em_reserved129
  | 130 => .em_reserved130
  | 143 => .em_reserved143
  | 144 => .em_reserved144
  | 145 => .em_reserved145
  | 146 => .em_reserved146
  | 147 => .em_reserved147
  | 148 => .em_reserved148
  | 149 => .em_reserved149
  | 150 => .em_reserved150
  | 151 => .em_reserved151
  | 152 => .em_reserved152
  | 153 => .em_reserved153
  | 154 => .em_reserved154
  | 155 => .em_reserved155
  | 156 => .em_reserved156
  | 157 => .em_reserved157
  | 158 => .em_reserved158
  | 159 => .em_reserved159
  | n => .em_other n

def ELFHeader.e_machine.toNat : ELFHeader.e_machine.values → Nat
  | .em_riscv => 243
  | .em_amdgpu => 224
  | .em_moxie => 223
  | .em_ft32 => 222
  | .em_visium => 221
  | .em_z80 => 220
  | .em_kalimba => 219
  | .em_norc => 218
  | .em_cool => 217
  | .em_coge => 216
  | .em_cdp => 215
  | .em_kvarc => 214
  | .em_kmx8 => 213
  | .em_kmx16 => 212
  | .em_kmx32 => 211
  | .em_km32 => 210
  | .em_mchp_pic => 204
  | .em_xcore => 203
  | .em_ba2 => 202
  | .em_ba1 => 201
  | .em_5600ex => 200
  | .em_78kor => 199
  | .em_videocore5 => 198
  | .em_rl78 => 197
  | .em_open8 => 196
  | .em_arc_compact2 => 195
  | .em_corea_2nd => 194
  | .em_corea_1st => 193
  | .em_cloudshield => 192
  | .em_sle9x => 179
  | .em_l10m => 180
  | .em_k10m => 181
  | .em_aarch64 => 183
  | .em_avr32 => 185
  | .em_stm8 => 186
  | .em_tile64 => 187
  | .em_tilepro => 188
  | .em_microblaze => 189
  | .em_cuda => 190
  | .em_tilegx => 191
  | .em_cypress => 161
  | .em_r32c => 162
  | .em_trimedia => 163
  | .em_qdsp6 => 164
  | .em_8051 => 165
  | .em_stxp7x => 166
  | .em_nds32 => 167
  | .em_ecog1x => 168
  | .em_maxq30 => 169
  | .em_ximo16 => 170
  | .em_manik => 171
  | .em_craynv2 => 172
  | .em_rx => 173
  | .em_metag => 174
  | .em_mcst_elbrus => 175
  | .em_ecog16 => 176
  | .em_cr16 => 177
  | .em_etpu => 178
  | .em_tsk3000 => 131
  | .em_rs08 => 132
  | .em_sharc => 133
  | .em_ecog2 => 134
  | .em_ccore7 => 135
  | .em_dsp24 => 136
  | .em_videocore3 => 137
  | .em_latticemico32 => 138
  | .em_c17 => 139
  | .em_c6000 => 140
  | .em_c2000 => 141
  | .em_c5500 => 142
  | .em_mmdsp_plus => 160
  | .em_zsp => 79
  | .em_mmix => 80
  | .em_huany => 81
  | .em_prism => 82
  | .em_avr => 83
  | .em_fr30 => 84
  | .em_d10v => 85
  | .em_d30v => 86
  | .em_v850 => 87
  | .em_m32r => 88
  | .em_mn10300 => 89
  | .em_mn10200 => 90
  | .em_pj => 91
  | .em_openrisc => 92
  | .em_arc_compact => 93
  | .em_xtensa => 94
  | .em_videocore => 95
  | .em_tmm_gpp => 96
  | .em_ns32k => 97
  | .em_tpc => 98
  | .em_snp1k => 99
  | .em_st200 => 100
  | .em_ip2k => 101
  | .em_max => 102
  | .em_cr => 103
  | .em_f2mc16 => 104
  | .em_msp430 => 105
  | .em_blackfin => 106
  | .em_se_c33 => 107
  | .em_sep => 108
  | .em_arca => 109
  | .em_unicore => 110
  | .em_excess => 111
  | .em_dxp => 112
  | .em_altera_nios2 => 113
  | .em_crx => 114
  | .em_xgate => 115
  | .em_c166 => 116
  | .em_m16c => 117
  | .em_dspic30f => 118
  | .em_ce => 119
  | .em_m32c => 120
  | .em_none => 0
  | .em_m32 => 1
  | .em_sparc => 2
  | .em_386 => 3
  | .em_68k => 4
  | .em_88k => 5
  | .em_860 => 7
  | .em_mips => 8
  | .em_s370 => 9
  | .em_mips_rs3_le => 10
  | .em_parisc => 15
  | .em_vpp500 => 17
  | .em_sparc32plus => 18
  | .em_960 => 19
  | .em_ppc => 20
  | .em_ppc64 => 21
  | .em_s390 => 22
  | .em_spu => 23
  | .em_v800 => 36
  | .em_fr20 => 37
  | .em_rh32 => 38
  | .em_rce => 39
  | .em_arm => 40
  | .em_alpha => 41
  | .em_sh => 42
  | .em_sparcv9 => 43
  | .em_tricore => 44
  | .em_arc => 45
  | .em_h8_300 => 46
  | .em_h8_300h => 47
  | .em_h8s => 48
  | .em_h8_500 => 49
  | .em_ia_64 => 50
  | .em_mips_x => 51
  | .em_coldfire => 52
  | .em_68hc12 => 53
  | .em_mma => 54
  | .em_pcp => 55
  | .em_ncpu => 56
  | .em_ndr1 => 57
  | .em_starcore => 58
  | .em_me16 => 59
  | .em_st100 => 60
  | .em_tinyj => 61
  | .em_x86_64 => 62
  | .em_pdsp => 63
  | .em_pdp10 => 64
  | .em_pdp11 => 65
  | .em_fx66 => 66
  | .em_st9plus => 67
  | .em_st7 => 68
  | .em_68hc16 => 69
  | .em_68hc11 => 70
  | .em_68hc08 => 71
  | .em_68hc05 => 72
  | .em_svx => 73
  | .em_st19 => 74
  | .em_vax => 75
  | .em_cris => 76
  | .em_javelin => 77
  | .em_firepath => 78
  | .em_intel209 => 209
  | .em_intel208 => 208
  | .em_intel207 => 207
  | .em_intel206 => 206
  | .em_intel205 => 205
  | .em_intel182 => 182
  | .em_arm184 => 184
  | .em_reserved6 => 6
  | .em_reserved11 => 11
  | .em_reserved12 => 12
  | .em_reserved13 => 13
  | .em_reserved14 => 14
  | .em_reserved16 => 16
  | .em_reserved24 => 24
  | .em_reserved25 => 25
  | .em_reserved26 => 26
  | .em_reserved27 => 27
  | .em_reserved28 => 28
  | .em_reserved29 => 29
  | .em_reserved30 => 30
  | .em_reserved31 => 31
  | .em_reserved32 => 32
  | .em_reserved33 => 33
  | .em_reserved34 => 34
  | .em_reserved35 => 35
  | .em_reserved121 => 121
  | .em_reserved122 => 122
  | .em_reserved123 => 123
  | .em_reserved124 => 124
  | .em_reserved125 => 125
  | .em_reserved126 => 126
  | .em_reserved127 => 127
  | .em_reserved128 => 128
  | .em_reserved129 => 129
  | .em_reserved130 => 130
  | .em_reserved143 => 143
  | .em_reserved144 => 144
  | .em_reserved145 => 145
  | .em_reserved146 => 146
  | .em_reserved147 => 147
  | .em_reserved148 => 148
  | .em_reserved149 => 149
  | .em_reserved150 => 150
  | .em_reserved151 => 151
  | .em_reserved152 => 152
  | .em_reserved153 => 153
  | .em_reserved154 => 154
  | .em_reserved155 => 155
  | .em_reserved156 => 156
  | .em_reserved157 => 157
  | .em_reserved158 => 158
  | .em_reserved159 => 159
  | .em_other n => n

--TODO (tedious) update with more expressive names from:
-- https://github.com/llvm/llvm-project/blob/0f5f931a9b32208a4894da57ea5c7428ead9df8d/llvm/tools/llvm-readobj/ELFDumper.cpp#L1109
instance : ToString (ELFHeader.e_machine.values) where
  toString
  | .em_riscv => "EM_RISCV"
  | .em_amdgpu => "EM_AMDGPU"
  | .em_moxie => "EM_MOXIE"
  | .em_ft32 => "EM_FT32"
  | .em_visium => "EM_VISIUM"
  | .em_z80 => "EM_Z80"
  | .em_kalimba => "EM_KALIMBA"
  | .em_norc => "EM_NORC"
  | .em_cool => "EM_COOL"
  | .em_coge => "EM_COGE"
  | .em_cdp => "EM_CDP"
  | .em_kvarc => "EM_KVARC"
  | .em_kmx8 => "EM_KMX8"
  | .em_kmx16 => "EM_KMX16"
  | .em_kmx32 => "EM_KMX32"
  | .em_km32 => "EM_KM32"
  | .em_mchp_pic => "EM_MCHP_PIC"
  | .em_xcore => "EM_XCORE"
  | .em_ba2 => "EM_BA2"
  | .em_ba1 => "EM_BA1"
  | .em_5600ex => "EM_5600EX"
  | .em_78kor => "EM_78KOR"
  | .em_videocore5 => "EM_VIDEOCORE5"
  | .em_rl78 => "EM_RL78"
  | .em_open8 => "EM_OPEN8"
  | .em_arc_compact2 => "EM_ARC_COMPACT2"
  | .em_corea_2nd => "EM_COREA_2ND"
  | .em_corea_1st => "EM_COREA_1ST"
  | .em_cloudshield => "EM_CLOUDSHIELD"
  | .em_sle9x => "EM_SLE9X"
  | .em_l10m => "EM_L10M"
  | .em_k10m => "EM_K10M"
  | .em_aarch64 => "EM_AARCH64"
  | .em_avr32 => "EM_AVR32"
  | .em_stm8 => "EM_STM8"
  | .em_tile64 => "EM_TILE64"
  | .em_tilepro => "EM_TILEPRO"
  | .em_microblaze => "EM_MICROBLAZE"
  | .em_cuda => "EM_CUDA"
  | .em_tilegx => "EM_TILEGX"
  | .em_cypress => "EM_CYPRESS"
  | .em_r32c => "EM_R32C"
  | .em_trimedia => "EM_TRIMEDIA"
  | .em_qdsp6 => "EM_QDSP6"
  | .em_8051 => "EM_8051"
  | .em_stxp7x => "EM_STXP7X"
  | .em_nds32 => "EM_NDS32"
  | .em_ecog1x => "EM_ECOG1X"
  | .em_maxq30 => "EM_MAXQ30"
  | .em_ximo16 => "EM_XIMO16"
  | .em_manik => "EM_MANIK"
  | .em_craynv2 => "EM_CRAYNV2"
  | .em_rx => "EM_RX"
  | .em_metag => "EM_METAG"
  | .em_mcst_elbrus => "EM_MCST_ELBRUS"
  | .em_ecog16 => "EM_ECOG16"
  | .em_cr16 => "EM_CR16"
  | .em_etpu => "EM_ETPU"
  | .em_tsk3000 => "EM_TSK3000"
  | .em_rs08 => "EM_RS08"
  | .em_sharc => "EM_SHARC"
  | .em_ecog2 => "EM_ECOG2"
  | .em_ccore7 => "EM_CCORE7"
  | .em_dsp24 => "EM_DSP24"
  | .em_videocore3 => "EM_VIDEOCORE3"
  | .em_latticemico32 => "EM_LATTICEMICO32"
  | .em_c17 => "EM_C17"
  | .em_c6000 => "EM_C6000"
  | .em_c2000 => "EM_C2000"
  | .em_c5500 => "EM_C5500"
  | .em_mmdsp_plus => "EM_MMDSP_PLUS"
  | .em_zsp => "EM_ZSP"
  | .em_mmix => "EM_MMIX"
  | .em_huany => "EM_HUANY"
  | .em_prism => "EM_PRISM"
  | .em_avr => "EM_AVR"
  | .em_fr30 => "EM_FR30"
  | .em_d10v => "EM_D10V"
  | .em_d30v => "EM_D30V"
  | .em_v850 => "EM_V850"
  | .em_m32r => "EM_M32R"
  | .em_mn10300 => "EM_MN10300"
  | .em_mn10200 => "EM_MN10200"
  | .em_pj => "EM_PJ"
  | .em_openrisc => "EM_OPENRISC"
  | .em_arc_compact => "EM_ARC_COMPACT"
  | .em_xtensa => "EM_XTENSA"
  | .em_videocore => "EM_VIDEOCORE"
  | .em_tmm_gpp => "EM_TMM_GPP"
  | .em_ns32k => "EM_NS32K"
  | .em_tpc => "EM_TPC"
  | .em_snp1k => "EM_SNP1K"
  | .em_st200 => "EM_ST200"
  | .em_ip2k => "EM_IP2K"
  | .em_max => "EM_MAX"
  | .em_cr => "EM_CR"
  | .em_f2mc16 => "EM_F2MC16"
  | .em_msp430 => "EM_MSP430"
  | .em_blackfin => "EM_BLACKFIN"
  | .em_se_c33 => "EM_SE_C33"
  | .em_sep => "EM_SEP"
  | .em_arca => "EM_ARCA"
  | .em_unicore => "EM_UNICORE"
  | .em_excess => "EM_EXCESS"
  | .em_dxp => "EM_DXP"
  | .em_altera_nios2 => "EM_ALTERA_NIOS2"
  | .em_crx => "EM_CRX"
  | .em_xgate => "EM_XGATE"
  | .em_c166 => "EM_C166"
  | .em_m16c => "EM_M16C"
  | .em_dspic30f => "EM_DSPIC30F"
  | .em_ce => "EM_CE"
  | .em_m32c => "EM_M32C"
  | .em_none => "EM_NONE"
  | .em_m32 => "EM_M32"
  | .em_sparc => "EM_SPARC"
  | .em_386 => "EM_386"
  | .em_68k => "EM_68K"
  | .em_88k => "EM_88K"
  | .em_860 => "EM_860"
  | .em_mips => "EM_MIPS"
  | .em_s370 => "EM_S370"
  | .em_mips_rs3_le => "EM_MIPS_RS3_LE"
  | .em_parisc => "EM_PARISC"
  | .em_vpp500 => "EM_VPP500"
  | .em_sparc32plus => "EM_SPARC32PLUS"
  | .em_960 => "EM_960"
  | .em_ppc => "EM_PPC"
  | .em_ppc64 => "EM_PPC64"
  | .em_s390 => "EM_S390"
  | .em_spu => "EM_SPU"
  | .em_v800 => "EM_V800"
  | .em_fr20 => "EM_FR20"
  | .em_rh32 => "EM_RH32"
  | .em_rce => "EM_RCE"
  | .em_arm => "EM_ARM"
  | .em_alpha => "EM_ALPHA"
  | .em_sh => "EM_SH"
  | .em_sparcv9 => "EM_SPARCV9"
  | .em_tricore => "EM_TRICORE"
  | .em_arc => "EM_ARC"
  | .em_h8_300 => "EM_H8_300"
  | .em_h8_300h => "EM_H8_300H"
  | .em_h8s => "EM_H8S"
  | .em_h8_500 => "EM_H8_500"
  | .em_ia_64 => "EM_IA_64"
  | .em_mips_x => "EM_MIPS_X"
  | .em_coldfire => "EM_COLDFIRE"
  | .em_68hc12 => "EM_68HC12"
  | .em_mma => "EM_MMA"
  | .em_pcp => "EM_PCP"
  | .em_ncpu => "EM_NCPU"
  | .em_ndr1 => "EM_NDR1"
  | .em_starcore => "EM_STARCORE"
  | .em_me16 => "EM_ME16"
  | .em_st100 => "EM_ST100"
  | .em_tinyj => "EM_TINYJ"
  | .em_x86_64 => "EM_X86_64"
  | .em_pdsp => "EM_PDSP"
  | .em_pdp10 => "EM_PDP10"
  | .em_pdp11 => "EM_PDP11"
  | .em_fx66 => "EM_FX66"
  | .em_st9plus => "EM_ST9PLUS"
  | .em_st7 => "EM_ST7"
  | .em_68hc16 => "EM_68HC16"
  | .em_68hc11 => "EM_68HC11"
  | .em_68hc08 => "EM_68HC08"
  | .em_68hc05 => "EM_68HC05"
  | .em_svx => "EM_SVX"
  | .em_st19 => "EM_ST19"
  | .em_vax => "EM_VAX"
  | .em_cris => "EM_CRIS"
  | .em_javelin => "EM_JAVELIN"
  | .em_firepath => "EM_FIREPATH"
  | .em_intel209 => "EM_INTEL209"
  | .em_intel208 => "EM_INTEL208"
  | .em_intel207 => "EM_INTEL207"
  | .em_intel206 => "EM_INTEL206"
  | .em_intel205 => "EM_INTEL205"
  | .em_intel182 => "EM_INTEL182"
  | .em_arm184 => "EM_ARM184"
  | .em_reserved6 =>"EM_RESERVED6"
  | .em_reserved11 => "EM_RESERVED11"
  | .em_reserved12 => "EM_RESERVED12"
  | .em_reserved13 => "EM_RESERVED13"
  | .em_reserved14 => "EM_RESERVED14"
  | .em_reserved16 => "EM_RESERVED16"
  | .em_reserved24 => "EM_RESERVED24"
  | .em_reserved25 => "EM_RESERVED25"
  | .em_reserved26 => "EM_RESERVED26"
  | .em_reserved27 => "EM_RESERVED27"
  | .em_reserved28 => "EM_RESERVED28"
  | .em_reserved29 => "EM_RESERVED29"
  | .em_reserved30 => "EM_RESERVED30"
  | .em_reserved31 => "EM_RESERVED31"
  | .em_reserved32 => "EM_RESERVED32"
  | .em_reserved33 => "EM_RESERVED33"
  | .em_reserved34 => "EM_RESERVED34"
  | .em_reserved35 => "EM_RESERVED35"
  | .em_reserved121 => "EM_RESERVED121"
  | .em_reserved122 => "EM_RESERVED122"
  | .em_reserved123 => "EM_RESERVED123"
  | .em_reserved124 => "EM_RESERVED124"
  | .em_reserved125 => "EM_RESERVED125"
  | .em_reserved126 => "EM_RESERVED126"
  | .em_reserved127 => "EM_RESERVED127"
  | .em_reserved128 => "EM_RESERVED128"
  | .em_reserved129 => "EM_RESERVED129"
  | .em_reserved130 => "EM_RESERVED130"
  | .em_reserved143 => "EM_RESERVED143"
  | .em_reserved144 => "EM_RESERVED144"
  | .em_reserved145 => "EM_RESERVED145"
  | .em_reserved146 => "EM_RESERVED146"
  | .em_reserved147 => "EM_RESERVED147"
  | .em_reserved148 => "EM_RESERVED148"
  | .em_reserved149 => "EM_RESERVED149"
  | .em_reserved150 => "EM_RESERVED150"
  | .em_reserved151 => "EM_RESERVED151"
  | .em_reserved152 => "EM_RESERVED152"
  | .em_reserved153 => "EM_RESERVED153"
  | .em_reserved154 => "EM_RESERVED154"
  | .em_reserved155 => "EM_RESERVED155"
  | .em_reserved156 => "EM_RESERVED156"
  | .em_reserved157 => "EM_RESERVED157"
  | .em_reserved158 => "EM_RESERVED158"
  | .em_reserved159 => "EM_RESERVED159"
  | .em_other n => s!"Unrecognized e_machine: 0x{Hex.toHex n}"

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
