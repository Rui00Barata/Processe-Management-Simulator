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

let interrupt () = 
  begin
    Queue.push (List.nth !pcb_table running_proc.ind) blockedQ;
    running_proc.ind <- -1;
    running_proc.pid <- -1;
    running_proc.pc <- -1;
    executing_flag := false
  end

let read_command c = 
  match c with
  |'E' -> let () = stop_time_flag := false in
          let () = rr_flag := false in
          if Queue.is_empty readyQ then 
            if (running_proc.ind <> -1) then (rem_time := !time_quantum; executing_flag := true; buffercommand := '$') 
            else 
              if (not (Queue.is_empty newQ)) then buffercommand := 'E' 
              else (stop_time_flag := true) 
          else (Short.short_sched (); rem_time := !time_quantum; executing_flag := true; buffercommand := '$')
  |'I' -> (interrupt ();stop_time_flag := true)
  |'D' -> (Long.long_sched (Queue.length blockedQ);stop_time_flag := true; if ((match Short.(!selected_scheduller) with |3|5 -> true |_ -> false)) then preempt_flag := true)
  |'R' -> (Report.report ();stop_time_flag := true)
  |'T' -> begin Report.global_report (); clock_flag := false; exit 0 end
  | _ -> Printf.printf  "Comando inválido\n"

let read_terminal ()=
  let character = ref 'x' in
  try 
    begin 
      Printf.printf ">> ";
      let str = read_line () in
      if (String.length str) > 1 then
        Printf.printf  "Comando Inválido\n"
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
      Printf.printf  "Comando Inválido\n"
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