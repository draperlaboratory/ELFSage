import Lake
open Lake DSL

package «ELFSage» where
  -- add package configuration options here

require Cli from git
  "https://github.com/leanprover/lean4-cli.git"@"main"

lean_lib «ELFSage» where
  -- add library configuration options here

@[default_target]
lean_exe «elfsage» where
  root := `Main
