open Javalib
open JBasics
open JCode
open SeaOfNodes

let get_jopcodes c =
  match c with
  | JInterface _ ->
      assert false
  | JClass _ -> (
      let methods = get_methods c in
      let m = List.hd @@ MethodMap.value_elements methods in
      match m with
      | AbstractMethod _ ->
          assert false
      | ConcreteMethod cm -> (
        match cm.cm_implementation with
        | Native ->
            assert false
        | Java code ->
            let jcode = Lazy.force code in
            jcode.c_code ) )

let rec map_option (f : 'a -> 'b option) (l : 'a list) : 'b list =
  match l with
  | [] ->
      []
  | hd :: tl -> (
    match f hd with None -> map_option f tl | Some hd -> hd :: map_option f tl )

let _ =
  let cp = Javalib.class_path "java_files" in
  let class_name, _ = Javalib.extract_class_name_from_file "java_files/Test1.class" in
  let jclass = Javalib.get_class cp class_name in
  let jopcodes = get_jopcodes jclass in
  let graph = Translator.translate_jopcodes jopcodes in
  let data =
    List.hd
    @@ map_option (fun (_, n) ->
           match n with
           | Type.Node.Control (Type.Control.Return {operand; _}) ->
               Some operand
           | _ ->
               None )
    @@ SeaOfNodes.Type.Son.bindings graph
  in
  assert (Interpretor.eval_data data == 42) ;
  (* check that hash-consing works *)
  let nodes = List.map snd (SeaOfNodes.Type.Son.bindings graph) in
  let data_nodes =
    map_option (function SeaOfNodes.Type.Node.Data data -> Some data | _ -> None) nodes
  in
  assert (
    List.for_all Fun.id
    @@ List.map (fun a -> List.for_all (fun n -> not (a = n && a != n)) data_nodes) data_nodes ) ;
  Printf.printf "Test passed successfully.\n"
