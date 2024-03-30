import ELFSage.Util.String

namespace Hex

def toHex (n: Nat) := String.toUpper $ String.mk $ Nat.toDigits 16 n

def toHexMinLength (n: Nat) (minChars: Nat) := (toHex n).leftpad minChars '0'

end Hex
