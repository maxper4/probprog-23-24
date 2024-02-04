open Byoppl
open Distribution
open Basic.Rejection_sampling

let true_skills prob () = 
  let skill_a = sample prob (gaussian ~mu:100.0 ~sigma:10.0) in
  let skill_b = sample prob (gaussian ~mu:100.0 ~sigma:10.0) in
  let skill_c = sample prob (gaussian ~mu:100.0 ~sigma:10.0) in

  let perf_a_1 = sample prob (gaussian ~mu:skill_a ~sigma:15.0) in
  let perf_b_1 = sample prob (gaussian ~mu:skill_b ~sigma:15.0) in
  let () = assume prob (perf_a_1 > perf_b_1) in

  let perf_b_2 = sample prob (gaussian ~mu:skill_b ~sigma:15.0) in
  let perf_c_2 = sample prob (gaussian ~mu:skill_c ~sigma:15.0) in
  let () = assume prob (perf_b_2 > perf_c_2) in

  let perf_a_3 = sample prob (gaussian ~mu:skill_a ~sigma:15.0) in
  let perf_c_3 = sample prob (gaussian ~mu:skill_c ~sigma:15.0) in
  let () = assume prob (perf_a_3 > perf_c_3) in

  skill_a::skill_b::skill_c::[]

let _ =
  Format.printf "@.-- Knuth, Basic Rejection Sampling --@.";
  let dist = Distribution.split_list (infer true_skills ()) in
  let m_x = List.map Distribution.mean dist in
  List.iter (Format.printf "%f@.") m_x
