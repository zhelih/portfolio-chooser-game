(* numeric subroutines *)

let integrate a b f =
  (* trapezoidal integration, use just 1_000 steps here for simplicity *)
  let n = 1_000 in
  let step = (b-.a) /. (float n) in
  let xs = List.rev @@ List.init n (fun i -> b-.((float i)*.step)) in
  let fs = List.map f xs in
  let rec loop l fk sum =
  match l with
  | [] -> sum
  | hd::tl -> loop tl hd (sum +. ((hd+.fk)/.2.*.step))
  in
  loop fs (f a) 0.

let bin_digits_of_float num  =
  let base = 2 in
  let rec loop n res =
    if List.length res >= 20 then res else begin
      let n2 = n*.2. in
      if n2 >= 1. then
        loop (n2-.1.) (1::res)
      else
        loop n2 (0::res)
    end
  in
  List.rev @@ loop num []

let rec cantor_func x n =
  let one_thrd = 1./.3. and two_thrd = 2./.3. in
  match n with
  | 0 -> x
  | _ when x > one_thrd && x < two_thrd -> 0.5
  | n when x < one_thrd -> 0.5*.(cantor_func (3.*.x) (n-1)) (*TODO tail *)
  | n -> 0.5+.0.5*.(cantor_func (3.*.x-.2.) (n-1))
