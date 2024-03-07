
--LittleEndian

def ByteArray.getUInt64LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 8) : UInt64 := 
  (bs.get ⟨offset + 0, by omega⟩).toUInt64 <<< 0x38 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt64 <<< 0x30 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt64 <<< 0x28 |||
  (bs.get ⟨offset + 3, by omega⟩).toUInt64 <<< 0x20 |||
  (bs.get ⟨offset + 4, by omega⟩).toUInt64 <<< 0x18 |||
  (bs.get ⟨offset + 5, by omega⟩).toUInt64 <<< 0x10 |||
  (bs.get ⟨offset + 6, by omega⟩).toUInt64 <<< 0x08 |||
  (bs.get ⟨offset + 7, by omega⟩).toUInt64

def ByteArray.getUInt32LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 4) : UInt32 := 
  (bs.get ⟨offset + 0, by omega⟩).toUInt32 <<< 0x18 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt32 <<< 0x10 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt32 <<< 0x08 |||
  (bs.get ⟨offset + 3, by omega⟩).toUInt32

def ByteArray.getUInt16LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 2) : UInt16 := 
  (bs.get ⟨offset + 0, by omega⟩).toUInt16 <<< 0x08 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt16


--BigEndian

def ByteArray.getUInt64BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 8) : UInt64 := 
  (bs.get ⟨offset + 7, by omega⟩).toUInt64 <<< 0x38 |||
  (bs.get ⟨offset + 6, by omega⟩).toUInt64 <<< 0x30 |||
  (bs.get ⟨offset + 5, by omega⟩).toUInt64 <<< 0x28 |||
  (bs.get ⟨offset + 4, by omega⟩).toUInt64 <<< 0x20 |||
  (bs.get ⟨offset + 3, by omega⟩).toUInt64 <<< 0x18 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt64 <<< 0x10 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt64 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt64

def ByteArray.getUInt32BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 4) : UInt32 := 
  (bs.get ⟨offset + 3, by omega⟩).toUInt32 <<< 0x18 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt32 <<< 0x10 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt32 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt32

def ByteArray.getUInt16BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 2) : UInt16 := 
  (bs.get ⟨offset + 1, by omega⟩).toUInt16 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt16
