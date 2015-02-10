(*
 *  Labeled Arguments
 *)
let rec range ~first:a ~last:b =
  if a > b then []
  else a :: range ~first:(succ a) ~last:b;;
(* val range : first:int -> last:int -> int list = <fun> *)
range 3 6;;
range ~first:3 ~last:6;;
range ~last:6 ~first:3;;

(*
 *  Label punning
 *)
let find l ~f =
  let rec loop = function
    | [] -> None
    | hd :: tl -> if f hd then Some hd else loop tl
  in loop l;;
(* val find: 'a list -> f:('a -> bool) -> 'a option = <fun> *)
find ~f:(fun x -> x = 3) [1;2;3];;  (* - : int option = Some 3 *)

let ratio ~num ~denom = float num /. float denom;;
(* val ration : num : int -> denom: int -> float = <fun> *)
let num = 3 in
let denom = 4 in
ratio ~num ~denom;; (* - : float = 0.75 *)

(*
 * Inference of labeled arguments
 *)

let foobar ~x ~y ~f =
  let dx = (f ~x ~y) in
  let dy = (f ~x ~y) in
  (dx, dy)
;;
(* val foobar : x'a -> y:'b -> f:(x'a -> y:'b -> 'c) -> 'c * 'c = <fun> *)

let foobar ~x ~y ~(f: x:'a -> y:'b -> 'c) =
  let dx = (f ~x ~y) in
  let dy = (f ~y ~x) in
  (dx, dy)
;;
(* Error: This function is applied to arguments
in an order different from other calls.
This is only allowed when the real type is known. *)

(* Providing explicit type information *)
let foobar ~x ~y ~(f: x:'a -> y:'b -> 'c) =
  let dx = (f ~x ~y) in
  let dy = (f ~y ~x) in
  (dx, dy)
;;
(* val foobar : x:'a -> y:'b -> f:(x:'a -> y:'b -> 'c) -> 'c * 'c = <fun> *)

open Core.Std;;
List.map ~f:((+) 3) [4;5;6];;
List.fold_left ~f:(+) ~init:0 [1;2;3;4;5];;

(*
 * Optional Parameters
 * ?(label = expression)
 *)

let rec range2 ?(step=1) a b =
  if a > b then []
  else a :: range2 ~step (a + step) b;;
(* val range2: ?step:int -> int -> int list = <fun> *)
range2 1 10;;
range2 1 10 ~step:2;;

String.concat;;
String.concat ["foo"; "bar"];;
String.concat ?sep:None ["foo"; "bar"];;

let upper_concat ?sep l = String.concat ?sep (List.map ~f:String.uppercase l);;
(* val upper_concat : ?sep:string ->  string list -> string = <fun> *)

(*
 * Optional arguments & partial application
 *)
let foo ?(z = 0) x y = (x + y) > z ;;
(* val foo : ?z:int -> int -> int -> bool = <fun> *)

let bar = foo 3;;
(* val bar : int -> bool = <fun> *)

bar 2;;
(* - : bool = true *)

(* bar 2 ~z:7;; *)
(* Error: This function has type int -> bool. *)
(* It is applied to too many arguments; maybe you forgot `;'. *)

let foo x ?(z = 0) y = (x + y) > z;;
(* val foo : int -> ?z:int -> int -> bool = <fun> *)

let bar = foo 3;;
(* val bar : int -> bool = <fun> *)

bar 2 ~z:7;;
(* - : bool = false *)
