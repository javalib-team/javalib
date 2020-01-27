open Javalib_pack 
open Javalib
open JBasics

let add_class basename i map =
  let name = Printf.sprintf "%s%02d" basename i in
  let cname = make_cn name in
  ClassMap.add cname i map  

let gen_class basename i =
  let name = Printf.sprintf "%s%02d" basename i in
  let _cname = make_cn name in
  ()

let iter_body _cn i =
  Printf.printf "%d " i
  
let _ =
  let map = ref ClassMap.empty in  
  for i=0 to 20 do
    map := add_class "Test" i !map
  done;
  ClassMap.iter iter_body !map;
  print_newline ();
  ClassMap.iter_ordered iter_body !map; 
  print_newline ();
  for i=0 to 20 do
    gen_class "Noise" i
  done;
  map := ClassMap.empty;
  for i=0 to 20 do
    map := add_class "TestAgain" i !map
  done;
  ClassMap.iter iter_body !map;
  print_newline ();
  ClassMap.iter_ordered iter_body !map; 
  print_newline ();
      
