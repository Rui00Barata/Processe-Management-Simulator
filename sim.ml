(*Simulacao de um processo*)
(* Modulos *)
Open Lib

(*let rec simular processo =
  if (processo.estado = 2) then ()
  if processo.pc != 0 then
    begin
    let aux = ref 0 in
    while aux != processo.pc do
      if ((input_char fi)=='\n') then aux := !aux + 1
    done
    end*)
  
let ins line =
  let x = String.split_on_char ' ' line in
  match x with
  |[] -> {ins = char_of_int 0;n = 0;nome = ""}
  |[hd] -> {ins = hd.[0];n = 0;nome = ""}
  |h::t -> 
    if h.[0] != 'L'  then {ins = h.[0];n = int_of_string (List.hd t);nome = ""}
    else {ins = h.[0];n = 0;nome = (List.hd t)}

(*let () =
  let fi = open_in processo.nome in
  simular processo*)