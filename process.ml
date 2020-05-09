(*Gestor de processos*)
open Lib

let fc = open_in "control.txt"
let buffercommand = ref '$'

let read_plan () =
  let fp = open_in "plan.txt" in
  let flag = ref true in
  let line = ref "" in
  while (!flag) do
      try
        begin
          line := remove_CR (input_line fp);
          let args = (String.split_on_char ' ' !line) in
          Queue.push ({name = List.nth args 0; time = int_of_string (List.nth args 1); priority = int_of_string (List.nth args 2)}) newQ;
        end
      with End_of_file -> begin close_in fp; flag := false end
  done


let read_command c = 
  match c with
  |'E' -> if Queue.is_empty readyQ then buffercommand := 'E' else (Short.short_sched (); buffercommand := '$')
  |'I' -> Printf.printf "INTERRUPT\n"
  |'D' -> Long.long_sched (Queue.length blockedQ)
  |'R' -> Report.report ()
  |'T' -> begin Report.global_report (); clock_flag := false; exit 0 end
  | _ -> Printf.fprintf stderr "Comando inválido\n"

let read_terminal ()=
  let character = ref 'x' in
  try 
    begin 
      let str = read_line () in
      if (String.length str) > 1 then
        Printf.fprintf stderr "Comando Inválido\n"
      else
        begin
          character := str.[0];
          read_command !character
        end
    end
  with End_of_file -> ()


let read_control fi = 
  let character = ref 'x' in
  try
    let str = remove_CR (input_line fi) in
    if (String.length str) > 1 then
      Printf.fprintf stderr "Comando Inválido\n"
    else
      begin
        character := str.[0];
        read_command !character
      end
    with 
    | End_of_file -> close_in fi;
    | Sys_error s -> failwith "Fim do ficheiro control.txt. Considere adicionar \"T\" ao ficheiro control.txt"



  let controller () =
    if !buffercommand = '$' then
      if !debug_mode then read_terminal () else read_control fc
    else if Queue.is_empty newQ then (read_command !buffercommand; buffercommand := '$' ) else read_command !buffercommand