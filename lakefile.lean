import Lake
open Lake DSL

package «elfbuilder» where
  -- add package configuration options here

lean_lib «Elfbuilder» where
  -- add library configuration options here

@[default_target]
lean_exe «elfbuilder» where
  root := `Main
