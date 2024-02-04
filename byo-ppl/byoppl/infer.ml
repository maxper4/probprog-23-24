module Gen = struct
  type 'a prob = 'a option
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample d k prob =
    let v = Distribution.draw d in
    k v prob

  let factor _s k prob = k () prob
  let observe d x = factor (Distribution.logpdf d x)
  let assume p = factor (if p then 0. else -.infinity)
  let exit v _prob = Some v

  let draw model data =
    let v = (model data) exit None in
    Option.get v
end

module Importance_sampling = struct
  type 'a prob = { id : int; particles : 'a particle array }
  and 'a particle = { value : 'a option; score : float; k : 'a next }
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample d k prob =
    let v = Distribution.draw d in
    k v prob

  let factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with score = s +. particle.score };
    k () prob

  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)

  let exit v prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with value = Some v };
    prob

  let infer ?(n = 1000) model data =
    let particles =
      Array.make n { value = None; score = 0.; k = (model data) exit }
    in
    Array.iteri (fun i p -> ignore (p.k { id = i; particles })) particles;

    let values = Array.map (fun p -> Option.get p.value) particles in
    let logits = Array.map (fun p -> p.score) particles in
    Distribution.support ~values ~logits
end

module Particle_filter = struct
  include Importance_sampling

  let resample particles =
    let logits = Array.map (fun p -> p.score) particles in
    let values = Array.map (fun p -> { p with score = 0. }) particles in
    let dist = Distribution.support ~logits ~values in
    Array.iteri (fun i _ -> particles.(i) <- Distribution.draw dist) particles

  let factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <-
      { particle with score = s +. particle.score; k = k () };
    prob

  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)

  let infer ?(n = 1000) model data =
    let particles =
      Array.make n { value = None; score = 0.; k = (model data) exit }
    in
    while Array.exists (fun p -> Option.is_none p.value) particles do
      Array.iteri (fun i p -> ignore (p.k { id = i; particles })) particles;
      resample particles
    done;

    let values = Array.map (fun p -> Option.get p.value) particles in
    let logits = Array.map (fun p -> p.score) particles in
    Distribution.support ~values ~logits
end

module Metropolis_hastings = struct
  type 'a prob = {
    score : float;
    trace : 'a sample_site list;
    value : 'a option;
  }

  and 'a sample_site =
    | Sample : {
        k : 'b -> 'a next;
        score : float;
        dist : 'b Distribution.t;
      }
        -> 'a sample_site

  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample dist k prob =
    let value = Distribution.draw dist in
    let sample_site = Sample { k; score = prob.score; dist } in
    k value { prob with trace = sample_site :: prob.trace }

  let factor s k prob = k () { prob with score = prob.score +. s }
  let assume p k prob = factor (if p then 0. else -.infinity) k prob
  let observe d x k prob = factor (Distribution.logpdf d x) k prob
  let exit v prob = { prob with value = Some v }

  let mh prob prob' =
    let fw = -.log (prob.trace |> List.length |> Float.of_int) in
    let bw = -.log (prob'.trace |> List.length |> Float.of_int) in
    min 1. (exp (prob'.score -. prob.score +. bw -. fw))

  let rec gen n values prob =
    if n = 0 then values
    else
      let regen_from = Random.int (List.length prob.trace) in
      (* pick regen point *)
      let (Sample regen) = List.nth prob.trace regen_from in
      (* compute new proposal *)
      let prob' =
        sample regen.dist regen.k
          {
            prob with
            score = regen.score;
            trace = Utils.slice prob.trace regen_from;
          }
      in
      (* accept / reject proposal *)
      let next_prob = if Random.float 1. < mh prob prob' then prob' else prob in
      gen (n - 1) (Option.get next_prob.value :: values) next_prob

  let infer ?(n = 1000) m data =
    let prob = (m data) exit { score = 0.; trace = []; value = None } in
    let values = gen n [] prob |> Array.of_list in
    Distribution.uniform_support ~values
end
