open Lib

let clock () = 
  while(!clock_flag) do
    begin
      Printf.printf "%d: flag: %B\t" !time !executing_flag;
      (* Printf.printf "clock: %d\t" !time;
      Printf.printf "%c\n" Process.(!buffercommand); *)
      while not (Queue.is_empty newQ) && (Queue.peek newQ).time = !time do
        Sim.openfile (Queue.pop newQ)
      done;
      if not !executing_flag then Process.controller ();
      if ((running_proc.ind <> -1) || !executing_flag && (!rem_time > 0)) then (* mudamos if *)
        begin
          Exec.execute running_proc.ind;
          if !rem_time = 0 then executing_flag := false
        end;
      time := !time + 1;
      (* Printf.printf "run: %d\n" running_proc.ind;
      Printf.printf "pc: %d\n" running_proc.pc; *)
      (* Printf.printf "v: %d\n" (List.nth !pcb_table running_proc.ind).variable *)
    end
  done

let scheduling_menu () =
  let flag = ref true in
  while !flag do
  begin
      Sys.command "clear";
      for i = 0 to 70 do print_char '-' done;
      (Printf.printf "\n|\n|\tEscalonamento selecionado:   %s\n|\tTipo de escalonamento:       %s\n" 
      (match Short.(!selected_scheduller) with | 1 -> "First Come First Serve" | _ -> "First Come First Serve")
      (if true then "Não preemptivo" else "Preemptivo"));
      (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
      (if !debug_mode then "Ativado" else "Desativado"));
      Printf.printf "\n|\n|\n|\tMenu de Escalonamento\n|\n|\t1 - FCFS\n|\n|\t0 - Voltar ao Menu de Opções\n|\n|\tSELECT: ";
      let op = read_int () in
      (match op with
      | 1 -> (flag := false; Short.selected_scheduller := 1)
      | 0 -> flag := false
      | _ -> ());
    end
  done

let options_menu () =
  let flag = ref true in
  while !flag do
    begin
      Sys.command "clear";
      for i = 0 to 70 do print_char '-' done;
      (Printf.printf "\n|\n|\tEscalonamento Selecionado:   %s\n|\tTipo de Escalonamento:       %s\n" 
      (match Short.(!selected_scheduller) with | 1 -> "First Come First Serve" | _ -> "First Come First Serve")
      (if true then "Não preemptivo" else "Preemptivo"));
      (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
      (if !debug_mode then "Ativado" else "Desativado"));
      Printf.printf "\n|\n|\n|\tMenu de Opções\n|\n|\t1 - Escalonamento Não Preemptivo\n|\t2 - Escalonamento Preemptivo\n|\t3 - Alterar Time Quantum\n|\n|\t0 - Voltar ao Menu Principal\n|\n|\tSELECT: ";
      let op = read_int () in
      (match op with
      | 1 -> scheduling_menu ()
      | 2 -> Printf.printf " ---------- Work in Progress ----------\n"
      | 3 -> begin Printf.printf "|\n|\tInsira o novo valor do Time Quantum: "; time_quantum := read_int () end
      | 0 -> flag := false
      | _ -> ());
    end
  done

let menu () = 
  let () = Process.read_plan () in
  let flag = ref true in
  while !flag do
    begin
      Sys.command "clear";
      for i = 0 to 70 do print_char '-' done;
      (Printf.printf "\n|\n|\tEscalonamento Selecionado:   %s\n|\tTipo de Escalonamento:       %s\n" 
      (match Short.(!selected_scheduller) with | 1 -> "First Come First Serve" | _ -> "First Come First Serve")
      (if true then "Não preemptivo" else "Preemptivo"));
      (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
      (if !debug_mode then "Ativado" else "Desativado"));
      Printf.printf "\n|\n|\n|\tMenu\n|\n|\t1 - Iniciar\n|\t2 - Opções\n|\n|\t0 - Sair\n|\n|\tSELECT: ";
      (match (read_int ()) with
      | 1 -> (Sys.command "clear"; flag := false; clock ())
      | 2 -> options_menu ()
      | 0 -> (Sys.command "clear"; exit 0)
      | _ -> ());
    end
  done

let () = 
    if (Array.length Sys.argv < 2) then
      menu ()
    else
        if Sys.argv.(1) = "-d" then (debug_mode := true; menu () )
        else failwith "Opção inválida. Talvez queira utilizar \"-d\"\n"