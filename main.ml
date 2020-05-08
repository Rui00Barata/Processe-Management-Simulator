open Lib

let clock () = 
  while(!clock_flag) do
    begin
      while (Queue.peek newQ).time = !time do
        Sim.openfile (Queue.pop newQ)
      done;
      if not !executing_flag then
        Process.controller ();
      if (!executing_flag && (!rem_time > 0)) then
        begin
          Exec.execute running_proc.ind;
          if !rem_time = 0 then executing_flag := false
        end;
      time := !time + 1;
      Printf.printf "run: %d\n" running_proc.ind;
      Printf.printf "pc: %d\n" running_proc.pc;
      (* Printf.printf "v: %d\n" (List.nth !pcb_table running_proc.ind).variable *)
    end
  done

let scheduling_menu () =
  let flag = ref true in
  while !flag do
    Printf.printf "Menu de Escalonamento\n\n1 - FCFS\n\n0 - Sair\n";
    let op = read_int () in
    match op with
    | 1 -> 
      begin 
        Short.selected_scheduller := 1;
        flag := false
      end
    | 0 -> exit 0
    | _ -> ()
done

let options_menu () =
  let flag = ref true in
  while !flag do
    Printf.printf "Menu de Opções\n\n1 - Escalonamento Não Preemptivo\n2 - Escalonamento Preemptivo\n3 - Alterar Time Quantum\n\n0 - Sair\n";
    let op = read_int () in
    match op with
    | 1 -> begin 
      scheduling_menu ();
      flag := false
    end
    | 2 -> Printf.printf "Work in Progress\n"
    | 3 -> begin Printf.printf "Insira o novo valor do Time Quantum: "; time_quantum := read_int () end
    | 0 -> flag := false
    | _ -> ()
  done

let menu () = 
    let () = Process.read_plan () in
    let flag = ref true in
    while !flag do
      Printf.printf "Menu\n\n1 - Iniciar\n2 - Opções\n\n0 - Sair\n";
      match (read_int ()) with
      | 1 -> clock ()
      | 2 -> options_menu ()
      | 0 -> exit 0 
      | _ -> ()
    done

let () = 
    if (Array.length Sys.argv < 2) then
      menu ()
    else
        if Sys.argv.(1) = "-d" then (debug_mode := true; menu () )
        else failwith "Opção inválida. Talvez queira utilizar \"-d\"\n"