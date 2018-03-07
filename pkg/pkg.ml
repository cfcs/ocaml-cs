#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "cs" @@ fun _c ->
  Ok [ Pkg.lib "pkg/META"
     ; Pkg.mllib ~api:["Cs"] "lib/cs.mllib"
     ; Pkg.test "test/alcotest_cs"
     ]
