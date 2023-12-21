module MakeStrategy = functor ( D: Distributions.Distribution ) ->
  struct
  let generateDP_strict n m =
    let dp = Array.make_matrix (n+1) (m+1) Float.neg_infinity in

    (* generate DP matrix for choosing m out of n *)
    for ni = 0 to n do
      for mi = 0 to m do
        if ni = mi then dp.(ni).(mi) <- (float ni)*.D.expectation else
        if mi > ni then dp.(ni).(mi) <- Float.neg_infinity else
        if mi = 0 then dp.(ni).(mi) <- 0.
        else begin
          let cutoff = dp.(ni - 1).(mi) -. dp.(ni - 1).(mi - 1) in
          let cdf_cutoff = D.cdf cutoff in
          let prob_greater_c = 1. -. cdf_cutoff in
          dp.(ni).(mi) <- dp.(ni - 1).(mi) *. cdf_cutoff +. (dp.(ni - 1).(mi - 1) +. (D.cex_gr_c cutoff))*.prob_greater_c
        end
      done
    done;
    dp

  let cutoff dp_res n m =
    dp_res.(n - 1).(m) -. dp_res.(n-1).(m-1)
  end (* struct *)
(*
P(X<=c) = CDF(C)
P(X>c) = 1-CDF(C)

not take X = f[n-1,m]
take X = f[n-1,m-1]+E(X|X>c)

f[n,n] = n*EX

E(X|X>c) = ?

*)
