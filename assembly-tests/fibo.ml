(* Written by Luca Saiu in 2013
   I hereby release this file into the public domain, up to the extent
   permitted by the law. *)

let rec fibo n =
  if n < 2 then
    n
  else
    (fibo (n - 2)) + (fibo (n - 1))

let _ = Printf.printf "%i\n" (fibo 40)
