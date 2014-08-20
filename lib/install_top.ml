let printers =
  [
    "Slap.Io.Toplevel.pp_rfvec";
    "Slap.Io.Toplevel.pp_rcvec";
    "Slap.Io.Toplevel.pp_rivec";
    "Slap.Io.Toplevel.pp_fmat";
    "Slap.Io.Toplevel.pp_cmat";
    "Slap.Io.Toplevel.pp_imat";
  ]

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let install_printers printers =
  List.for_all
    (fun printer ->
     let cmd = Printf.sprintf "#install_printer %s;;" printer in
     eval_string cmd)
    printers

let install_ssc () =
  eval_string "let ssc = Slap.Io.Toplevel.ssc;;"
  && eval_string "let lsc = Slap.Io.Toplevel.lsc;;"

let () =
  if not (install_ssc () && install_printers printers) then
    Format.eprintf "Problem installing SLAP-top@."
