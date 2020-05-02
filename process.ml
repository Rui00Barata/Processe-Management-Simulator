(*Gestor de processos*)
open Lib

let read_plan () =
  let fi = open_in "plan.txt" in
  let flag = ref true in
  let line = ref "" in
  while (!flag) do
      try
        begin
          line := remove_CR (input_line fi);
          let args = (String.split_on_char ' ' !line) in
          Queue.push ({name = List.nth args 0; time = int_of_string (List.nth args 1); priority = int_of_string (List.nth args 2)}) newQ
        end
      with End_of_file -> begin close_in fi; flag := false; end
  done


let read_command c =
  match c with
  |'E' -> execute (pop readyQ)
  |'I' -> Printf.printf "INTERRUPT\n"
  |'D' -> Printf.printf "LONG\n"
  |'R' -> Printf.printf "REPORT\n"
  |'T' -> begin Printf.printf "TERMINATE\n"; exit 0 end
  | _ -> Printf.fprintf stderr "Comando inválido\n"

let read_terminal () =
  let flag = ref true in
  let character = ref 'x' in
  while (!flag) do
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
    with End_of_file -> flag := false
  done

let read_control fi = 
  let flag = ref true in
  let character = ref 'x' in
  while (!flag) do
    try 
      let str = remove_CR (input_line fi) in
      if (String.length str) > 1 then
        Printf.fprintf stderr "Comando Inválido\n"
      else
        begin
          character := str.[0];
          read_command !character
        end
    with End_of_file -> begin close_in fi; flag := false end
  done

  let controller debug =
    if debug then
      read_terminal ()
    else
      let fi = open_in "control.txt" in
      read_control fi