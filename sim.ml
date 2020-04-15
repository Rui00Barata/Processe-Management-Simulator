(*Simulacao de um processo*)
(* Modulos *)
Open Lib

(*Variáveis*)
let memory_model = Array.make 1000 {ins = 'N'; n = 0; nome = ""}
let ultimo = ref 0


(*Funções*)
let tirar_CR str =
  let n = String.length str in
  if n > 0 && str.[n-1] = '\r' then
    String.sub str 0 (n-1)
  else
    str

let line_to_instr line =
  let x = String.split_on_char ' ' (tirar_CR line) in
  match x with
  |[] -> {ins = char_of_int 0; n = 0; nome = ""}
  |[hd] -> {ins = hd.[0]; n = 0; nome = ""}
  |h::t -> 
    if h.[0] <> 'L'  then {ins = h.[0];n = int_of_string (List.hd t); nome = ""}
    else if h.[0] = 'L' then {ins = h.[0];n = 0;nome = (List.hd t)}
    else {ins = 'E';n = 0;nome = ""}


let abrir path =
  let fi = open_in path in
  let flag = ref true in
  let line = ref "" in
  let instr = ref {ins = 'N'; n = 0; nome = ""} in
  while (!flag) do
      try 
      begin 
        line := input_line fi;
        instr := line_to_instr !line;
        memory_model.(!ultimo) <- !instr;
        ultimo := !ultimo + 1 
      end
      with End_of_file -> begin line := "EOF"; close_in fi; flag := false end
  done


