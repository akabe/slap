(** An example of visualization of convergence of gradient-based optimization
    (by gnuplot)

    This implementation is explained at
    http://akabe.github.io/slap/demo-gradopt.html#visualization-tips *)

open Format
open Slap.D
open Slap.Io

(** Gaussian function *)
let gauss a b x =
  let xb = Vec.sub x b in
  ~-. (exp (dot xb (symv a xb) /. 2.0))

(** The derivative of [gauss] *)
let dgauss a b x = symv ~alpha:(gauss a b x) a (Vec.sub x b)

(** Send commands to gnuplot. *)
let gnuplot f =
  let oc = Unix.open_process_out "gnuplot" in (* Open a pipe to gnuplot *)
  let ppf = formatter_of_out_channel oc in (* For fprintf *)
  f ppf; (* Send gnuplot commands *)
  pp_print_flush ppf (); (* Flush an output buffer. *)
  print_endline "Press Enter to exit gnuplot";
  ignore (read_line ()); (* Prevent gnuplot from immediately exiting *)
  ignore (Unix.close_process_out oc) (* Exit gnuplot *)

(** [splot_fun ?option ?n ?x1 ?x2 ?y1 ?y2 ppf f] plots a 3D graph of
    OCaml function [f] in [x1 <= x <= x2] and [y1 <= y <= y2]. *)
let splot_fun
    ?(option = "") ?(n = 10)
    ?(x1 = -10.0) ?(x2 = 10.0) ?(y1 = -10.0) ?(y2 = 10.0) ppf f =
  let cx = (x2 -. x1) /. float n in
  let cy = (y2 -. y1) /. float n in
  fprintf ppf "splot '-' %s@\n" option;
  for i = 0 to n do
    for j = 0 to n do
      let x = cx *. float i +. x1 in
      let y = cy *. float j +. y1 in
      let z = f [%vec [x; y]] in
      fprintf ppf "%g %g %g@\n" x y z;
    done;
    fprintf ppf "@\n"
  done;
  fprintf ppf "end@\n"

let steepest_descent ppf ~loops ~eta df f x =
  fprintf ppf "splot '-' with linespoints linetype 2 title ''@\n\
               %a 0@\n" pp_rfvec x;
  for i = 1 to loops do
    axpy ~alpha:(~-. eta) (df x) x; (* x := x - eta * (df x) *)
    fprintf ppf "%a 0@\n" pp_rfvec x
  done;
  fprintf ppf "end@\n"

let () =
  let a = [%mat [-0.1, 0.1;
                 0.1, -0.2]] in
  let b = [%vec [1.0; 3.0]] in
  let (x1, x2, y1, y2) = (-2.0, 3.0, 0.0, 5.0) in
  gnuplot
    (fun ppf ->
       fprintf ppf "set multiplot@\n\
                    set view 0,0      # Fix a view@\n\
                    unset ztics       # Don't show ztics@\n\
                    unset clabel      # Don't show contour labels@\n\
                    set ticslevel 0@\n\
                    set xrange [%g:%g]@\n\
                    set yrange [%g:%g]@\n" x1 x2 y1 y2;
       steepest_descent ppf
         ~loops:100 ~eta:2.0 (dgauss a b) (gauss a b) [%vec [0.0; 1.0]];
       fprintf ppf "set contour             # Plot contour@\n\
                    unset xtics             # Don't show xtics twice@\n\
                    unset ytics             # Don't show xtics twice@\n\
                    unset surface           # Don't show a 3D surface@\n\
                    set cntrparam levels 15 # Levels of contour@\n";
       splot_fun ppf (gauss a b) ~n:50 ~x1 ~x2 ~y1 ~y2
         ~option:"w l lt 1 title ''")
