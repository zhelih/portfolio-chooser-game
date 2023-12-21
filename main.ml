open Printf

module Run = functor (D : Distributions.Distribution) ->
  struct
  module Strategy = Strategies.MakeStrategy(D)
  let run n m =
    let dp_result = Strategy.generateDP_strict n m in
(*    for ni = 0 to n do
      for mi = 0 to m do
        printf "%f " dp_result.(ni).(mi)
      done;
      printf "\n"
    done;
*)
    let rec loop ni mi sum =
      if ni == 0 then printf "Ended run. Final sum is %f\n" sum else begin
        let inp = D.next () in
        if mi == 0 then begin
          printf "Number %d: got %.6f, cutoff:      NA, verdict: IGNORE, sum = %f\n" (n-ni+1) inp sum;
          loop (ni-1) mi sum
        end else
        if ni == mi then begin
          printf "Number %d: got %.6f, cutoff:      NA, verdict: MUST KEEP, sum = %f\n" (n-ni+1) inp (sum+.inp);
          loop (ni-1) (mi-1) (sum+.inp)
        end else begin
          let cutoff = Strategy.cutoff dp_result ni mi in
          if inp > cutoff then begin
            printf "Number %d: got %.6f, cutoff %.6f, verdict: KEEP, sum = %f\n" (n-ni+1) inp cutoff (sum+.inp);
            loop (ni-1) (mi-1) (sum+.inp)
          end else begin
            printf "Number %d: got %.6f, cutoff %.6f, verdict: PASS, sum = %f\n" (n-ni+1) inp cutoff sum;
            loop (ni-1) mi sum
          end
        end
      end
    in
    loop n m 0.;
    ()

  end (* struct *)

module UniformRun = Run(Distributions.Uniform)
module NormalRun = Run(Distributions.Normal)
module ExponentialRun = Run(Distributions.Exponential)
module GeometricRun = Run(Distributions.Geometric)
module CantorRun = Run(Distributions.Cantor)

let () =
  print_endline "Enter N:";
  let n = read_int () in
  print_endline "Enter M:";
  let m = read_int () in
  print_endline "Distributions Avaliable:";
  print_endline "1. Uniform U[0;1]";
  print_endline "2. Normal N(0,1)";
  print_endline "3. Exponential Exp(1)";
  print_endline "4. Geometric Geom(1)";
  print_endline "5. Cantor";
  print_endline "Select:";
  let d = read_int () in
  Random.self_init();
  match d with
  | 1 -> UniformRun.run n m; ()
  | 2 -> NormalRun.run n m; ()
  | 3 -> ExponentialRun.run n m; ()
  | 4 -> GeometricRun.run n m; ()
  | 5 -> CantorRun.run n m; ()
  | _ -> print_endline "Unknown solver"
