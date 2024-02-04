open Byoppl
open Distribution
open Basic.Rejection_sampling

let knuth prob () =
  let c1 = sample prob (bernoulli ~p:0.5) == 1 in

  let rec f1 () =
    let c2 = sample prob (bernoulli ~p:0.5) == 1 in
    let c3 = sample prob (bernoulli ~p:0.5) == 1 in
    if c2 then
      if c3 then f1 ()
      else 1
    else
      if c3 then 2
      else 3
  in
  
  let rec f2 () =
    let c2 = sample prob (bernoulli ~p:0.5) == 1 in
    let c3 = sample prob (bernoulli ~p:0.5) == 1 in
    if c2 then
      if c3 then 4
      else 5
    else
      if not c3 then f2 ()
      else 6
  in

  if c1 then f1 ()
  else f2 ()


(* let _ =
  Format.printf "@.-- Knuth, Values test --@.";
  Array.iter (fun x -> Format.printf "%d " x) (Array.init 100 (fun _ -> knuth ())); *)

let _ = 
  Format.printf "@.-- Knuth, Basic Rejection Sampling --@.";
  let dist = infer knuth () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values 

open Basic.Importance_sampling

let _ = 
  Format.printf "@.-- Knuth, Importance Sampling --@.";
  let dist = infer knuth () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

open Cps_operators
open Infer.Gen

let knuth () =
  let* c1 = sample (bernoulli ~p:0.5) in

  let rec f1 () =
    let* c2 = sample (bernoulli ~p:0.5) in
    let* c3 = sample (bernoulli ~p:0.5) in
    if c2 == 1 then
      if c3 == 1 then f1 ()
      else return 1
    else
      if c3 == 1 then return 2
      else return 3
  in
  
  let rec f2 () =
    let* c2 = sample (bernoulli ~p:0.5) in
    let* c3 = sample (bernoulli ~p:0.5) in
    if c2 == 1 then
      if c3 == 1 then return 4
      else return 5
    else
      if c3 == 0 then f2 ()
      else return 6
  in

  if c1 == 1 then f1 ()
  else f2 ()

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Generation --@.";
  for _ = 1 to 10 do
    let v = draw knuth () in
    Format.printf "%d " v
  done;
  Format.printf "@."

open Cps_operators
open Infer.Importance_sampling

let knuth () =
  let* c1 = sample (bernoulli ~p:0.5) in

  let rec f1 () =
    let* c2 = sample (bernoulli ~p:0.5) in
    let* c3 = sample (bernoulli ~p:0.5) in
    if c2 == 1 then
      if c3 == 1 then f1 ()
      else return 1
    else
      if c3 == 1 then return 2
      else return 3
  in
  
  let rec f2 () =
    let* c2 = sample (bernoulli ~p:0.5) in
    let* c3 = sample (bernoulli ~p:0.5) in
    if c2 == 1 then
      if c3 == 1 then return 4
      else return 5
    else
      if c3 == 0 then f2 ()
      else return 6
  in

  if c1 == 1 then f1 ()
  else f2 ()

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Importance Sampling --@.";
  let dist = infer knuth () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values