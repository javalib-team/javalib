(*
 * This file is part of Javalib
 * Copyright (c)2023 Martin Andrieux (ENS Rennes)
 * Copyright (c)2023 Alban Dutilleul (ENS Rennes)
 * Copyright (c)2023 David Pichardie (Facebook France)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

(* Utils on lists *)

let insert_at_reverse i a l =
  let rec aux i a l =
    match (i, l) with 0, l -> a :: l | _, [] -> [a] | i, x :: l -> x :: aux (i - 1) a l
  in
  List.rev l |> aux i a |> List.rev

let%test "insert_at_reverse_empty" =
  insert_at_reverse 0 1 [] = [1]

let%test "insert_at_reverse_0" =
  insert_at_reverse 0 1 [2; 3] = [2; 3; 1]

let%test "insert_at_reverse_1" =
  insert_at_reverse 1 1 [2; 3; 7; 8] = [2; 3; 7; 1; 8]

let%test "insert_at_reverse_out_of_bounds" =
  insert_at_reverse 20 1 [2; 3; 7; 8] = [1; 2; 3; 7; 8]

let rec transpose list =
  match list with
  | [] ->
      []
  | [] :: xss ->
      transpose xss
  | (x :: xs) :: xss ->
      List.((x :: map hd xss) :: transpose (xs :: map tl xss))

let%test "transpose_empty" =
  transpose [] = []

let%test "transpose_1" =
  transpose [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] = [[1; 4; 7]; [2; 5; 8]; [3; 6; 9]]

let%test "transpose_2" =
  transpose [[1; 2]; [3; 4]] = [[1; 3]; [2; 4]]

