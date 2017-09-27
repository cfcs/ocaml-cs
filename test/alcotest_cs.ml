let cs = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal

let ptime = Alcotest.testable (Ptime.pp_rfc3339 ())
                              (fun a b -> 0 = Ptime.compare a b)

let get_opt = function Some x -> x | None -> failwith "get_opt"

let any () =
    Alcotest.testable (fun fmt _ -> Fmt.pf fmt "[polymorphic compare]")
      (fun (a:'t) (b:'t) -> 0 = compare a b)

let test_to_list () =
  Alcotest.(check @@ list char) "\"123\" = ['1';'2';'3']]"
    ['1';'2';'3'] (Cs.to_list (Cs.of_string "123"))

let test_of_list () =
  Alcotest.(check cs) "of_list |> to_list"
    (Cs.of_list ['a';'b';'c']) @@ Cs.of_string "abc"

let test_cs_w () =
  Alcotest.(check cs) "Cs.W"
    (Cs.of_string ("a" ^ "bcd"^ "EFG" ^ "1234"))
       (let w = Cs.W.create 2 in
        Cs.W.char w 'a';
        Cs.W.str w "bcd";
        Cs.W.cs w (Cs.of_string "EFG") ;
        ignore @@ Cs.W.e_ptimespan32 `TODO w (Ptime.Span.of_int_s 0x31323334) ;
        Cs.W.to_cs w |> Cs.W.of_cs |> Cs.W.to_cs
       )

open Rresult

let test_cs_r () =
  (let r = Cs.R.of_string (`Broken) "a" in
   Alcotest.(check @@ result char reject) "first: 'a'" (Ok 'a') (Cs.R.char r) ;
  Alcotest.(check @@ result char pass) "can't read beyond"
    (Error `Broken)
    (Cs.R.char r)
  );
  let r2 = Cs.R.of_string (`Msg "Cs.R broken") "\x00\x03\x00\x00\x01\x00" in
  Alcotest.(check @@ result (any ()) reject) "uint16" (Ok 3) (Cs.R.uint16 r2) ;
  Alcotest.(check @@ result (any ()) reject) "uint32" (Ok 256l) (Cs.R.uint32 r2)

let test_e_is_empty () =
  Alcotest.(check @@ result unit reject) "empty"
    (Ok ()) (Cs.e_is_empty `e (Cs.of_string "")) ;
  Alcotest.(check @@ result unit (any ())) "not empty"
    (Error `e) (Cs.e_is_empty `e (Cs.of_string "a"))

let test_strip_leading_char () =
  Alcotest.(check cs) "aaaabc -> bc" (Cs.of_string "bc")
    (Cs.of_string "aaaabc" |> Cs.strip_leading_char 'a')

let test_tai64 () =
  let test_helper hex second =
    let mt = Cs.e_ptime_of_tai64 `e (Cs.of_hex hex |> R.get_ok) |> R.get_ok in
    let ct = Cs.create_tai64_of_ptime mt in
    let st = get_opt (Ptime.Span.of_int_s second |> Ptime.of_span) in
    Alcotest.(check ptime)  hex st mt ;
    Alcotest.(check string) hex (Cs.to_hex ct) (String.lowercase_ascii hex) ;
    Alcotest.(check cs)     hex ct (Cs.create_tai64_of_ptime st)
  in
  (* From http://cr.yp.to/libtai/tai64.html *)
  test_helper "3fFFffFFffFFffFF" (-1) ;
  test_helper "400000002a2b2c2d" 707472429

let tests =
  [ "Cs.to_list", `Quick, test_to_list
  ; "Cs.of_list", `Quick, test_of_list
  ; "Cs.W", `Quick, test_cs_w
  ; "Cs.R", `Quick, test_cs_r
  ; "Cs.e_is_empty", `Quick, test_e_is_empty
  ; "Cs.strip_leading_char", `Quick, test_strip_leading_char
  ; "Cs.e_ptime_of_tai64", `Quick, test_tai64
  ]

let () =
  Alcotest.run "ocaml-cs test suite" ["Cs (cstruct wrapper module)", tests]
