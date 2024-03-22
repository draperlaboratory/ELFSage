import ELFSage.Types.ProgramHeaderTable

structure ELF32InterpretedSegment where
  /-- Type of the segment -/
  segment_type  : Nat 
  /-- Size of the segment in bytes -/
  segment_size  : Nat 
  /-- Size of the segment in memory in bytes -/
  segment_memsz : Nat 
  /-- Base address of the segment -/
  segment_base  : Nat 
  /-- Physical address of segment -/
  segment_paddr : Nat 
  /-- Alignment of the segment -/
  segment_align : Nat 
  /-- Offset of the segment -/
  segment_offset : Nat 
  /-- Body of the segment -/
  segment_body  : ByteArray
  /-- READ, WRITE, EXECUTE flags. -/
  segment_flags : Bool × Bool × Bool
  deriving Repr

def ELF32ProgramHeaderTableEntry.toSegment?
  (phte : ELF64ProgramHeaderTableEntry)
  (bytes : ByteArray)
  : Except String ELF32InterpretedSegment :=
  if bytes.size < phte.p_offset.toNat + phte.p_filesz.toNat
  then .error $
    s! "A segment specified in the program header table at offset {phte.p_offset}, " ++
    s! "with size {phte.p_filesz}, runs off the end of the binary."
  else .ok {
    segment_type   := phte.p_type.toNat
    segment_size   := phte.p_filesz.toNat 
    segment_memsz  := phte.p_memsz.toNat
    segment_base   := phte.p_vaddr.toNat
    segment_paddr  := phte.p_paddr.toNat
    segment_align  := phte.p_align.toNat
    segment_offset := phte.p_offset.toNat
    segment_body   := bytes.extract phte.p_offset.toNat (phte.p_offset.toNat + phte.p_filesz.toNat)
    segment_flags  := ⟨
      (phte.p_flags / 4) % 2  == 0,  -- Readable Segment
      (phte.p_flags / 2) % 2  == 0,  -- Writable Segment
      (phte.p_flags / 1) % 2  == 0,  -- Executable Segment
    ⟩
  }
  
structure ELF64InterpretedSegment where
  /-- Type of the segment -/
  segment_type  : Nat 
  /-- Size of the segment in bytes -/
  segment_size  : Nat 
  /-- Size of the segment in memory in bytes -/
  segment_memsz : Nat 
  /-- Base address of the segment -/
  segment_base  : Nat 
  /-- Physical address of segment -/
  segment_paddr : Nat 
  /-- Alignment of the segment -/
  segment_align : Nat 
  /-- Offset of the segment -/
  segment_offset : Nat 
  /-- Body of the segment -/
  segment_body  : ByteArray
  /-- READ, WRITE, EXECUTE flags. -/
  segment_flags : Bool × Bool × Bool
  deriving Repr

def ELF64ProgramHeaderTableEntry.toSegment?
  (phte : ELF64ProgramHeaderTableEntry)
  (bytes : ByteArray)
  : Except String ELF64InterpretedSegment :=
  if bytes.size < phte.p_offset.toNat + phte.p_filesz.toNat
  then .error $
    s! "A segment specified in the program header table at offset {phte.p_offset}, " ++
    s! "with size {phte.p_filesz}, runs off the end of the binary."
  else .ok {
    segment_type   := phte.p_type.toNat
    segment_size   := phte.p_filesz.toNat 
    segment_memsz  := phte.p_memsz.toNat
    segment_base   := phte.p_vaddr.toNat
    segment_paddr  := phte.p_paddr.toNat
    segment_align  := phte.p_align.toNat
    segment_offset := phte.p_offset.toNat
    segment_body   := bytes.extract phte.p_offset.toNat (phte.p_offset.toNat + phte.p_filesz.toNat)
    segment_flags  := ⟨
      (phte.p_flags / 4) % 2  == 0,  -- Readable Segment
      (phte.p_flags / 2) % 2  == 0,  -- Writable Segment
      (phte.p_flags / 1) % 2  == 0,  -- Executable Segment
    ⟩
  }
