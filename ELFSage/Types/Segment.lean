import ELFSage.Types.ProgramHeaderTable

structure InterpretedSegment where
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

def ProgramHeaderTableEntry.toSegment?
  [ProgramHeaderTableEntry α]
  (phte : α)
  (bytes : ByteArray)
  : Except String InterpretedSegment :=
  if bytes.size < p_offset phte + p_filesz phte
  then .error $
    s! "A segment specified in the program header table at offset {p_offset phte}, " ++
    s! "with size {p_filesz phte}, runs off the end of the binary."
  else .ok {
    segment_type   := p_type phte
    segment_size   := p_filesz phte
    segment_memsz  := p_memsz phte
    segment_base   := p_vaddr phte
    segment_paddr  := p_paddr phte
    segment_align  := p_align phte
    segment_offset := p_offset phte
    segment_body   := bytes.extract (p_offset phte) (p_offset phte + p_filesz phte)
    segment_flags  := ⟨
      (p_flags phte / 4) % 2  == 0,  -- Readable Segment
      (p_flags phte / 2) % 2  == 0,  -- Writable Segment
      (p_flags phte / 1) % 2  == 0,  -- Executable Segment
    ⟩
  }

def getInterpretedSegments
  [ProgramHeaderTableEntry α] (pht : List α)
  (bytes : ByteArray)
  : Except String (List (α × InterpretedSegment)) :=
  pht.mapM $ λphte ↦ do
    let seg ← ProgramHeaderTableEntry.toSegment? phte bytes
    return (phte,seg)
