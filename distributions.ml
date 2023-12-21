module type Distribution =
  sig
    val name        : string
    val expectation : float
    val cdf         : float -> float
    val next        : unit -> float

    (* conditionl expectation E[ X | X > c ] given c *)
    val cex_gr_c    : float -> float
  end

(* U[0;1] *)
module Uniform : Distribution =
  struct
    let name = "U[0;1]"
    let expectation = 0.5
    let cdf = function x when x < 0. -> 0. | x when x > 1. -> 1. | x -> x
    let next () = Random.float 1.
    let cex_gr_c = function c when c < 0. -> 1. | c when c > 1. -> 0. | c -> (1.+.c)/.2.
  end

(* N(0,1) *)
module Normal : Distribution =
  struct
    let name = "N(0,1)"
    let prefix = 1. /. (Float.(sqrt (2.*.pi)))
    let expectation = 0.
    let pdf x = prefix *. Float.exp ( (-.x)*.x/.2. )
    let cdf x =
      if x < -10. then 0. else Numeric.integrate (-10.) x pdf
    (* use Box-Muller for sampling *)
    let next () =
      let u1 = Random.float 1. in
      let u2 = Random.float 1. in
      let part1 = Float.(sqrt (-2.*.(log u1))) in
      let part2 = Float.(cos (pi *. 2. *. u2)) in
    part1*.part2
    let cex_gr_c c =
      let f x = x*.(pdf x) in
      let intgral = Numeric.integrate c 100. f in
      intgral /. (1.-.(cdf c))
  end

(* Exp(1) *)
module Exponential : Distribution =
  struct
    let name = "Exp(1)"
    let expectation = 1.
    let cdf x = if x < 0. then 0. else 1.-.(Float.exp (-.x))
    let next () = -. Float.log (1.-.(Random.float 1.)) (* use 1. to prevent 0 *)
    let cex_gr_c c = ((Float.exp (-.c)) *. (1.+.c)) /. (1.-.(cdf c))
  end

(* and example of a discrete distribution *)
module Geometric : Distribution =
  struct
    let name = "Geom(0.5) from 1"
    let p = 0.5
    let expectation = 1./.p
    let cdf x = if x < 1. then 0. else 1.-.(Float.pow (1.-.p) (Float.floor x))
    let next () = 1.+.Float.floor ( (Float.log @@ Random.float 1.) /. Float.log (1.-.p)) (* can do pmf-based, but this trick works for Geom *)
    let cex_gr_c c =
      (* when computing pmf * x, do for the first 1_000 numbers only *)
      let res = ref 0. in
      for i = 1 to 1_000 do
        let fi = float i in
        if fi > c then (* strict! matters for discrete *)
          res := !res +. fi*.p*.(Float.pow (1.-.p) (fi-.1.));
      done;
      !res /. (1.-.(cdf c))
 end
