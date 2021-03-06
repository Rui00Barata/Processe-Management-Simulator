open Lib

let clock () = 
  while(!clock_flag) do
    begin
      while not (Queue.is_empty newQ) && (Queue.peek newQ).time = !time do
        Sim.openfile (Queue.pop newQ);
        if ((match Short.(!selected_scheduller) with |3|5 -> true |_ -> false)) then preempt_flag := true
      done;
      if ((!time mod !time_quantum) = 0) then Process.controller ();
      if (!preempt_flag && (!rem_time <> !time_quantum) && (not !rr_flag)) then 
        (Short.short_sched (); rem_time := !time_quantum; executing_flag := true);
      if (!executing_flag && (!rem_time > 0) && (not !stop_time_flag)) then
      begin
        Exec.execute running_proc.ind;
        if !rem_time = 0 then executing_flag := false
      end;
      if (((Process.(!buffercommand) <> '$') || !rr_flag || (running_proc.ind <> -1) || (!time_flag) || (((running_proc.ind = -1) && (Queue.is_empty readyQ)) )) && (not !stop_time_flag)) then (time := !time + 1; time_flag := false);
    end
  done

let scheduling_menu () =
  let flag = ref true in
  while !flag do
    begin
      ignore(Sys.command "clear");
      for i = 0 to 90 do print_char '-' done;
      (Printf.printf "\n|\n|\tEscalonamento selecionado:   %s\n|\tTipo de escalonamento:       %s\n" 
      (match Short.(!selected_scheduller) with | 1 -> "First Come First Serve" |2 -> "Priority" |3 -> "Priority" |4 -> "Shortest Job First" |5 -> "Shortest Job First" |6 -> "Round Robin" | _ -> "First Come First Serve")
      (if (match Short.(!selected_scheduller) with |3|5|6 -> false |_ -> true) then "Não preemptivo" else "Preemptivo"));
      (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
      (if !debug_mode then "Ativado" else "Desativado"));
      Printf.printf "\n|\tNúmero de partições:         %-5d" (Array.length !heap_f);
      Printf.printf "\n|\n|\n|\tMenu de Escalonamento\n|\n|\t1 - FCFS\n|\t2 - Priority\n|\t3 - SJF\n|\n|\t0 - Voltar ao Menu de Opções\n|\n";for i = 0 to 90 do print_char '-' done; Printf.printf "\n>> ";
      let op = read_int () in
      (match op with
      | 1 -> (flag := false; Short.selected_scheduller := 1)
      | 2 -> (flag := false; Short.selected_scheduller := 2)
      | 3 -> (flag := false; Short.selected_scheduller := 4)
      | 0 -> flag := false
      | _ -> ());
    end
  done

  let scheduling_menup () =
    let flag = ref true in
    while !flag do
      begin
        ignore(Sys.command "clear");
        for i = 0 to 90 do print_char '-' done;
        (Printf.printf "\n|\n|\tEscalonamento selecionado:   %s\n|\tTipo de escalonamento:       %s\n" 
        (match Short.(!selected_scheduller) with |1 -> "First Come First Serve" |2 -> "Priority" |3 -> "Priority" |4 -> "Shortest Job First" |5 -> "Shortest Job First" |6 -> "Round Robin" |_ -> "First Come First Serve")
        (if (match Short.(!selected_scheduller) with |3|5|6 -> false |_ -> true) then "Não preemptivo" else "Preemptivo"));
        (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
        (if !debug_mode then "Ativado" else "Desativado"));
        Printf.printf "\n|\tNúmero de partições:         %-5d" (Array.length !heap_f);
        Printf.printf "\n|\n|\n|\tMenu de Escalonamento\n|\n|\t1 - Priority\n|\t2 - SJF\n|\t3 - Round Robin\n|\n|\t0 - Voltar ao Menu de Opções\n|\n";for i = 0 to 90 do print_char '-' done; Printf.printf "\n>> ";
        let op = read_int () in
        (match op with
        |1 -> (flag := false; Short.selected_scheduller := 3)
        |2 -> (flag := false; Short.selected_scheduller := 5)
        |3 -> (flag := false; Short.selected_scheduller := 6)
        |0 -> flag := false
        |_ -> ());
      end
    done

let memory_management () =
  let () = Printf.printf "Qual o tamanho pretendido para a memória? >> " in
  let op1 = read_int () in
  let () = Printf.printf "Qual o tamanho pretendido para as partições? >> " in
  let op2 = read_int () in
  (heap_f := Array.make (op1/op2) (-1);
  heap_n := Array.make (op1/op2) (-1);
  heap_b := Array.make (op1/op2) (-1);
  heap_w := Array.make (op1/op2) (-1))

let options_menu () =
  let flag = ref true in
  while !flag do
    begin
      ignore(Sys.command "clear");
      for i = 0 to 90 do print_char '-' done;
      (Printf.printf "\n|\n|\tEscalonamento Selecionado:   %s\n|\tTipo de Escalonamento:       %s\n" 
      (match Short.(!selected_scheduller) with | 1 -> "First Come First Serve" |2 -> "Priority" |3 -> "Priority" |4 -> "Shortest Job First" |5 -> "Shortest Job First" |6 -> "Round Robin" | _ -> "First Come First Serve")
      (if (match Short.(!selected_scheduller) with |3|5|6 -> false |_ -> true) then "Não preemptivo" else "Preemptivo"));
      (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
      (if !debug_mode then "Ativado" else "Desativado"));
      Printf.printf "\n|\tNúmero de partições:         %-5d" (Array.length !heap_f);
      Printf.printf "\n|\n|\n|\tMenu de Opções\n|\n|\t1 - Escalonamento Não Preemptivo\n|\t2 - Escalonamento Preemptivo\n|\t3 - Alterar Time Quantum\n|\t4 - Alterar o tamanho para o Heap\n|\n|\t0 - Voltar ao Menu Principal\n|\n";for i = 0 to 90 do print_char '-' done; Printf.printf "\n>> ";
      let op = read_int () in
      (match op with
      | 1 -> scheduling_menu ()
      | 2 -> scheduling_menup ()
      | 3 -> begin Printf.printf "\nInsira o novo valor do Time Quantum: >> "; time_quantum := read_int () end
      | 4 -> memory_management ()
      | 0 -> flag := false
      | _ -> ());
    end
  done

let menu () = 
  let () = Process.read_plan () in
  let flag = ref true in
  while !flag do
    begin
      ignore(Sys.command "clear");
      for i = 0 to 90 do print_char '-' done;
      (Printf.printf "\n|\n|\tEscalonamento Selecionado:   %s\n|\tTipo de Escalonamento:       %s\n" 
      (match Short.(!selected_scheduller) with |1 -> "First Come First Serve" |2 -> "Priority" |3 -> "Priority" |4 -> "Shortest Job First" |5 -> "Shortest Job First" |6 -> "Round Robin" | _ -> "First Come First Serve")
      (if (match Short.(!selected_scheduller) with |3|5|6 -> false |_ -> true) then "Não preemptivo" else "Preemptivo"));
      (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
      (if !debug_mode then "Ativado" else "Desativado"));
      Printf.printf "\n|\tNúmero de partições:         %-5d" (Array.length !heap_f);
      Printf.printf "\n|\n|\n|\tMenu\n|\n|\t1 - Iniciar\n|\t2 - Opções\n|\n|\t0 - Sair\n|\n";for i = 0 to 90 do print_char '-' done; Printf.printf "\n>> ";
      (match (read_int ()) with
      | 1 -> (ignore(Sys.command "clear"); flag := false; if (Short.(!selected_scheduller) <> 6) then rem_time := !time_quantum; clock ())
      | 2 -> options_menu ()
      | 0 -> (ignore(Sys.command "clear"); exit 0)
      | _ -> ());
    end
  done

let () = 
  if (Array.length Sys.argv < 2) then
    menu ()
  else
    if Sys.argv.(1) = "-d" then (debug_mode := true; menu () )
    else failwith "Opção inválida. Talvez queira utilizar \"-d\"\n"