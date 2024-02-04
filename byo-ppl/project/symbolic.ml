open Byoppl
open Distribution

(* Partie A *)
(* Partie B *)

type dist = 
  | Beta of node * node
  | Bernoulli of node
  | Gaussian of node * node
  | V of float
and node = dist ref

exception Not_tractable

let sample d = 
  (* return a new node with distribution [d] *)
  let n:node = ref d in
  n

let observe d v = 
  (* update the parameters of the distribution [d] given an observation 
     Raise [Not_tractable] if this is not possible 
  *)
  match d with
  | Bernoulli (x) -> 
    begin match !x with
    | Beta (({ contents = V a }), ({ contents = V b })) ->
      x := Beta (ref (V (a +. v)), ref (V (b -. v +. 1.)));
    | _ -> raise Not_tractable
    end
  | Gaussian (x, { contents = V sigma }) ->
    begin match !x with
    | Gaussian({ contents = V u0}, { contents = V sigma0 }) ->
      let u1 = ((u0 /. (sigma0 ** 2.)) +. (v /. (sigma ** 2.))) /. ((1. /. (sigma0 ** 2.)) +. (1. /. (sigma ** 2.))) in
      let sigma1 = sqrt (1. /. ((1. /. (sigma0 ** 2.)) +. (1. /. (sigma ** 2.)))) in
      x := Gaussian (ref (V u1), ref (V sigma1));
    | _ -> raise Not_tractable
    end
  | _ -> raise Not_tractable

let infer model params = 
  (* return the posterior distribution of a model
    We assume that a model always returns a value of type [node]
  *)
  let d:node = model params in
  !d

let beta_bernoulli v = 
  let p = sample (Beta (ref (V 1.), ref (V 1.))) in
  observe (Bernoulli(p)) v;
  p

let gauss_gauss v = 
  let m = sample (Gaussian (ref (V 0.), ref (V 1.))) in
  observe (Gaussian(m, ref (V 1.))) v;
  m

let fail data = 
  let m = sample (Gaussian (ref (V 0.5), ref (V 0.1))) in
  let () = List.iter (observe (Bernoulli(m))) data in
  m

let _ =
  Format.printf "-- Symbolic inference: beta_bernoulli --@.";
  let dist = infer beta_bernoulli 1. in
  Format.printf "infer beta_bernoulli 1.: %a@." (fun fmt n -> match n with
    | Beta ({ contents = V a }, { contents = V b }) -> Format.fprintf fmt "Beta(%f, %f)" a b
    | _ -> Format.fprintf fmt "Not a beta distribution") dist;

  Format.printf "@.-- Symbolic inference: gauss_gauss --@.";
  let dist = infer gauss_gauss 1. in
  Format.printf "infer gauss_gauss 1.: %a@." (fun fmt n -> match n with
    | Gaussian ({ contents = V u }, { contents = V sigma }) -> Format.fprintf fmt "Gaussian(%f, %f)" u sigma
    | _ -> Format.fprintf fmt "Not a gaussian distribution") dist;

  Format.printf "@.-- Symbolic inference: gauss_bernoulli --@.";
  try 
    let _ = infer fail [1.; 1.; 0.; 1.] in 
    Format.printf "Tractable (unexpected) @.";
  with Not_tractable -> 
    Format.printf "Not tractable (as expected) @.";
  

(* Partie C *)

type prob = { 
  id : int; 
  scores : float array;  
}

let rec value d = 
  match !d with
  | V v -> d := V v; v
  | Beta (a, b) -> let v = Distribution.draw (Distribution.beta ~a:(value a) ~b:(value b)) in d := V v; v
  | Bernoulli (x) -> let v = Distribution.draw (Distribution.bernoulli_float ~p:(value x)) in d := V v; v
  | Gaussian (m, s) -> let v = Distribution.draw (Distribution.gaussian ~mu:(value m) ~sigma:(value s)) in d := V v; v

let sample _prob d = 
  let n:node = ref d in
  n

let sample_importance _prob d = V (Distribution.draw d)

let factor prob s = prob.scores.(prob.id) <- prob.scores.(prob.id) +. s

let observe_importance prob d x = factor prob (Distribution.logpdf d x) 

let observe prob d v =
  match d with
  | Bernoulli (x) -> 
    begin match !x with
    | Beta (a, b) ->
      let va = value (ref !a) in
      let vb = value (ref !b) in
      x := Beta (ref (V (va +. v)), ref (V (vb -. v +. 1.)));
      let marginale = Distribution.bernoulli_float ~p:(va /. (va +. vb)) in
      observe_importance prob marginale v;
    | _ -> observe_importance prob (Distribution.bernoulli_float ~p:(value x)) v
    end
  | Gaussian (x, { contents = V sigma }) ->
    begin match !x with
    | Gaussian(u0, sigma0) ->
      let vu0 = value (ref !u0) in
      let vsigma0 = value (ref !sigma0) in
      let u1 = ((vu0 /. (vsigma0 ** 2.)) +. (v /. (sigma ** 2.))) /. ((1. /. (vsigma0 ** 2.)) +. (1. /. (sigma ** 2.))) in
      let sigma1 = sqrt (1. /. ((1. /. (vsigma0 ** 2.)) +. (1. /. (sigma ** 2.)))) in
      x := Gaussian (ref (V u1), ref (V sigma1));
      let marginale = Distribution.gaussian ~mu:vu0 ~sigma:(sqrt (vsigma0 ** 2. +. sigma ** 2.)) in
      observe_importance prob marginale v;
    | _ -> observe_importance prob (Distribution.gaussian ~mu:(value x) ~sigma:sigma) v
    end
  | Gaussian (x, s) -> observe_importance prob (Distribution.gaussian ~mu:(value x) ~sigma:(value s)) v
  | Beta (a, b) -> observe_importance prob (Distribution.beta ~a:(value a) ~b:(value b)) v
  | V s -> if s = v then factor prob 1. else factor prob 0. (* edge case *)

let infer ?(n = 1000) model data =
    let scores = Array.make n 0. in
    let values = Array.init n (fun i -> model { scores; id = i } data) in
    Distribution.support ~values ~logits:scores

let example_1 prob v =  (* beta_bernoulli *) 
  let p = sample prob (Beta (ref (V 1.), ref (V 1.))) in
  observe prob (Bernoulli(p)) v;
  p

let example_2 prob data = (* failed before *)
  let m = sample prob (Gaussian (ref (V 0.5), ref (V 0.05))) in
  let () = List.iter (observe prob (Bernoulli(m))) data in
  m

let example_3 prob v = (* half importance half symbolic *)
  let s = sample prob (Bernoulli (ref (V 0.5))) in
  let m = sample prob (Gaussian (s, ref (V 1.))) in
  observe prob (Gaussian(m, ref (V 1.))) v;
  m

let example_4 prob data = (* hmm *)
  let rec gen states data =
    match (states, data) with
    | [], y :: data -> gen [ ref (V y) ] data
    | states, [] -> states
    | pre_x :: _, y :: data ->
        let x = sample prob (Gaussian(pre_x, ref (V 1.0))) in
        let () = observe prob (Gaussian(x, ref (V 1.0))) y in
        gen (x :: states) data
  in
  gen [] data

let rec f_result dist = 
  match !dist with
  | Beta (a, b) -> Format.sprintf "Beta(%s, %s)" (f_result a) (f_result b)
  | Bernoulli (x) -> Format.sprintf "Bernoulli(%s)" (f_result x)
  | Gaussian (m, s) -> Format.sprintf "Gaussian(%s, %s)" (f_result m) (f_result s)
  | V v -> Format.sprintf "%f" v

let _ =
  Format.printf "@.-- Semi-Symbolic inference --@.";
  Format.printf "@.-- Semi-Symbolic inference : example 1 --@.";
  let dist1 = infer example_1 1. in
  let { values; probs; _ } = get_support ~shrink:true dist1 in
  Array.iteri (fun i v -> Format.printf "infer example_1 1.: %f %s@." probs.(i) (f_result v)) values;

  Format.printf "@.-- Semi-Symbolic inference : example 2 --@.";
  let dist2 = infer example_2 [1.; 1.; 0.; 1.] in
  let { values; probs; _ } = get_support ~shrink:true dist2 in
  Format.printf "infer example_2 [1.; 1.; 0.; 1.]: @.";
  (* Array.iteri (fun i v -> Format.printf "%f %s@." probs.(i) (f_result v)) values; *)
  let (wsum, _) = Array.fold_left (fun (acc, i) v -> (acc +. ((value v) *. probs.(i)), i + 1)  ) (0., 0) values in
  Format.printf "weighted sum of particles: %f@." wsum;

  Format.printf "@.-- Semi-Symbolic inference : example 3 --@.";
  let dist3 = infer example_3 1. in
  let { values; probs; _ } = get_support ~shrink:true dist3 in
  Format.printf "infer example_3 1.: @.";
  Array.iteri (fun i v -> Format.printf "%f %s@." probs.(i) (f_result v)) values;

  Format.printf "@.-- Semi-Symbolic inference : example 4 --@.";
  let data = Owl.Arr.linspace 0. 20. 20 |> Owl.Arr.to_array |> Array.to_list in
  let dist = Distribution.split_list (infer ~n:1000 example_4 data) in
  let dist_float =  List.map (fun d -> 
    let {values; logits; _ } = get_support ~shrink:true d in
    Distribution.support ~values:(Array.map value values) ~logits
  ) dist in 
  let m_x = List.map Distribution.mean (List.rev dist_float) in
  (* List.iter2 (Format.printf "%f >> %f@.") data m_x; *)
  let moy_error = List.fold_left2 (fun acc x y -> acc +. abs_float (x -. y)) 0. data m_x in
  Format.printf "error mean: %f@." (moy_error /. (float_of_int (List.length data)));
  (* List.iter (fun d -> let { values; probs; _} = get_support d in Array.iteri (fun i v -> if probs.(i) <= 0.0001 then () else Format.printf "infer example_4: %f %s@." probs.(i) (f_result v)) values) dist; *)
  
  (* let time model data =
    let t = Sys.time() in
    let _ = infer ~n:1000 model data in
    Sys.time() -. t
  in

  let nb = 100000 in
  let results = Array.init nb (fun _ -> time example_4 data) in
  let moy = (Array.fold_left (+.) 0. results) /. (float_of_int nb) in
  let variance = (Array.fold_left (fun acc x -> acc +. ((x -. moy) ** 2.)) 0. results) /. (float_of_int nb) in
  let std = sqrt variance in
  Format.printf "time: %f +-%f@." moy std; *)


(* Extension *)

type dist_op = 
  | Beta of node_op * node_op
  | Bernoulli of node_op
  | Gaussian of node_op * node_op
  | V of float
  | Binop of node_op * node_op * (float -> float -> float)
and node_op = dist_op ref

let rec value d = 
  match !d with
  | V v -> d := V v; v
  | Beta (a, b) -> let v = Distribution.draw (Distribution.beta ~a:(value a) ~b:(value b)) in d := V v; v
  | Bernoulli (x) -> let v = Distribution.draw (Distribution.bernoulli_float ~p:(value x)) in d := V v; v
  | Gaussian (m, s) -> let v = Distribution.draw (Distribution.gaussian ~mu:(value m) ~sigma:(value s)) in d := V v; v
  | Binop (a, b, f) -> let v = f (value a) (value b) in d := V v; v

let sample _prob d = 
  let n:node_op = ref d in
  n

let model_op prob () = 
  let x = sample prob (Bernoulli(ref (V 0.5))) in
  ref (Binop(ref (Binop (x, ref (V 2.), ( *. ))), ref (V 1.), ( +. )))    (* rescaling de Bernoulli entre 1 et 3*)

let rec f_result_op dist = 
  match !dist with
  | Beta (a, b) -> Format.sprintf "Beta(%s, %s)" (f_result_op a) (f_result_op b)
  | Bernoulli (x) -> Format.sprintf "Bernoulli(%s)" (f_result_op x)
  | Gaussian (m, s) -> Format.sprintf "Gaussian(%s, %s)" (f_result_op m) (f_result_op s)
  | Binop (a, b, _) -> Format.sprintf "Binop(%s, %s, <function>)" (f_result_op a) (f_result_op b)
  | V v -> Format.sprintf "%f" v

let _ = 
  Format.printf "@.-- Extension --@.";
  let dist = infer model_op () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Format.printf "infer model_op (): @.";
  Array.iteri (fun i v -> Format.printf "%f %s@." probs.(i) (f_result_op v)) values;
  Format.printf "Draw in infered distribution: @.";
  Array.init 5 (fun _ -> Format.printf "value model_op: %f@." (value (model_op probs ())));

(* open Basic.Importance_sampling

let example_1 prob v =  (* beta_bernoulli *) 
  let p = sample prob (beta ~a:1. ~b:1.) in
  observe prob (bernoulli_float ~p:p) v;
  p

let example_2 prob data = (* failed before *)
  let m = sample prob (gaussian ~mu:0.5 ~sigma:0.05) in
  let () = List.iter (observe prob (bernoulli_float ~p:m)) data in
  m

let example_3 prob v = (* half importance half symbolic *)
  let s = sample prob (bernoulli_float ~p:0.5) in
  let m = sample prob (gaussian ~mu:s ~sigma:1.) in
  observe prob (gaussian ~mu:m ~sigma:1.) v;
  m

let example_4 prob data = (* hmm *)
  let rec gen states data =
    match (states, data) with
    | [], y :: data -> gen [ y ] data
    | states, [] -> states
    | pre_x :: _, y :: data ->
        let x = sample prob (gaussian ~mu:pre_x ~sigma:1.0) in
        let () = observe prob (gaussian ~mu:x ~sigma:1.0) y in
        gen (x :: states) data
  in
  gen [] data

let _ = 
  Format.printf "@.-- Importance sampling --@.";
 
  let time model data =
    let t = Sys.time() in
    let _ = Basic.Importance_sampling.infer ~n:1000 model data in
    Sys.time() -. t
  in

  let data = Owl.Arr.linspace 0. 20. 20 |> Owl.Arr.to_array |> Array.to_list in

  let nb = 100000 in
  let results = Array.init nb (fun _ -> time example_4 data) in
  let moy = (Array.fold_left (+.) 0. results) /. (float_of_int nb) in
  let variance = (Array.fold_left (fun acc x -> acc +. ((x -. moy) ** 2.)) 0. results) /. (float_of_int nb) in
  let std = sqrt variance in
  Format.printf "time: %f +-%f@." moy std; *)