(* numeric subroutines *)

let integrate a b f =
  (* trapezoidal integration, use just 1_000 steps here for simplicity *)
  let n = 1_000 in
  let step = (b-.a) /. (float n) in
  let xs = List.rev @@ List.init n (fun i -> b-.((float i)*.step)) in
  let f_a = f a in (* needed to nicely fold everything and avoid any numerical inconsitencies later *)
  let fs = List.map f xs in
  let rec loop l fk sum =
  match l with
  | [] -> sum
  | hd::tl -> loop tl hd (sum +. ((hd+.fk)/.2.*.step))
  in
  loop fs f_a 0.
