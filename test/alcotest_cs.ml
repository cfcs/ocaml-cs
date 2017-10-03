let cs = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal

let ptime = Alcotest.testable (Ptime.pp_rfc3339 ~frac_s:1000000())
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

let test_empty () =
  Alcotest.(check cs) "empty"
    (Cs.of_string "") (Cs.empty)

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
  let x = Cs.R.of_string (`Msg "Cs.R broken")
                         "\x00\x03\x00\x00\x01\x00abc\x00DEF\x00\x00\x00\x00B" in
  Alcotest.(check @@ result (int) reject) "uint16" (Ok 3) (Cs.R.uint16 x);
  Alcotest.(check @@ result (int32) reject) "uint32" (Ok 256l) (Cs.R.uint32 x);
  Alcotest.(check @@ result unit reject) "equal_string"
    (Ok ()) (Cs.R.equal_string x "abc\x00");
  Alcotest.(check @@ result (string) reject) "string_z" (Ok "DEF") (Cs.R.string_z x 6)

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
    let unhexed = Cs.of_hex hex |> R.get_ok in
    let t,tn = Cs.e_ptime_of_tai64 `e unhexed |> R.get_ok,
               Cs.e_ptime_of_tai64_n `e unhexed |> R.get_ok
    in
    let ct,ctn = Cs.create_tai64_of_ptime t,
                 Cs.create_tai64_n_of_ptime tn
    in
    let st = match second with
    | `D x -> (match Ptime.of_rfc3339 x |> R.get_ok with t,_,_->t)
    | `S s -> get_opt (Ptime.Span.of_int_s s |> Ptime.of_span) in
    Alcotest.(check ptime)  ("st = tn: "^hex) st tn ;
    Alcotest.(check string) ("str: "^hex)
      (Cs.to_hex ctn) (String.lowercase_ascii hex) ;
    Alcotest.(check cs)  ("ct=stn: "^hex) ct (Cs.create_tai64_of_ptime st) ;
    Alcotest.(check cs)  ("ctn=stn: "^hex) ctn (Cs.create_tai64_n_of_ptime st)
  in
  (* From http://cr.yp.to/libtai/tai64.html
   * NOTE that these currently fail because leap seconds are not implemented *)
  test_helper "3fFFffFFffFFffFF00000000" @@ `S (-1) ;
  test_helper "400000002a2b2c2d00000000" @@ `S 707472429 ;
  test_helper "3fFFffFFa1f2cd8a00000000" @@ `S (-1577923200) ;
  test_helper "3fFFffFF8000000000000000" @@ `S (-2147483648) ;
  test_helper "400000007fFFffFF00000000" @@ `S 2147483647 ;
  test_helper "400000000000000000000000" @@ `S 0 ;
  test_helper "4000000055932da2362888d3" @@ `D "2015-06-30T23:59:59.908626131Z"

let tests =
  [ "Cs.to_list", `Quick, test_to_list
  ; "Cs.of_list", `Quick, test_of_list
  ; "Cs.empty", `Quick, test_empty
  ; "Cs.W", `Quick, test_cs_w
  ; "Cs.R", `Quick, test_cs_r
  ; "Cs.e_is_empty", `Quick, test_e_is_empty
  ; "Cs.strip_leading_char", `Quick, test_strip_leading_char
  ; "Cs.e_ptime_of_tai64", `Quick, test_tai64
  ]

let () =
  Alcotest.run "ocaml-cs test suite" ["Cs (cstruct wrapper module)", tests]
