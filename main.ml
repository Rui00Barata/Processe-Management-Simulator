open Lib

let clock () = 
  while(!clock_flag) do
    begin
      while not (Queue.is_empty newQ) && (Queue.peek newQ).time = !time do
        Sim.openfile (Queue.pop newQ);
      done;
      if (not !executing_flag) then Process.controller ();
      if (!executing_flag && (!rem_time > 0)) then
      begin
        Exec.execute running_proc.ind;
        if !rem_time = 0 then executing_flag := false
      end;
      if ((Process.(!buffercommand) <> '$') || (running_proc.ind <> -1) || (!time_flag)) then (time := !time + 1; time_flag := false);
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
      "Não preemptivo");
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

  let scheduling_menup () =
    let flag = ref true in
    while !flag do
      begin
        Sys.command "clear";
        for i = 0 to 70 do print_char '-' done;
        (Printf.printf "\n|\n|\tEscalonamento selecionado:   %s\n|\tTipo de escalonamento:       %s\n" 
        (match Short.(!selected_scheduller) with | 1 -> "First Come First Serve" | _ -> "First Come First Serve")
        "Preemptivo");
        (Printf.printf "|\tTime Quantum:                %d\n|\tModo de Debug:               %s" !time_quantum 
        (if !debug_mode then "Ativado" else "Desativado"));
        Printf.printf "\n|\n|\n|\tMenu de Escalonamento\n|\n|\t1 - Priority\n|\n|\t0 - Voltar ao Menu de Opções\n|\n|\tSELECT: ";
        let op = read_int () in
        (match op with
        | 1 -> (flag := false; Short.selected_scheduller := 3)
        | 0 -> flag := false
        | _ -> ());
      end
    done

let memory_management_ () =
  (* Printf.printf "Qual o tamanho pretendido para a memória dinâmica?" *)
  let op1 = read_int () in
  let op2 = read_int () in
  heap := Array.make (op1/op2) (-1)

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
      | 2 -> scheduling_menup ()
      | 3 -> begin Printf.printf "|\n|\tInsira o novo valor do Time Quantum: "; time_quantum := read_int () end
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