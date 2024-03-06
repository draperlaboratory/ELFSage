import Cli

instance : Cli.ParseableType System.FilePath where
  name := "FilePath"
  parse? s := ↑s
