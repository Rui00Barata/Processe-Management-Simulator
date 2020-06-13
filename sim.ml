open Lib

let line_to_instr line =
  let x = String.split_on_char ' ' (remove_CR line) in
  match x with
  |[] -> {ins = char_of_int 0; n = 0; name = ""}
  |[hd] -> {ins = hd.[0]; n = 0; name = ""}
  |h::t -> 
    if h.[0] <> 'L'  then {ins = h.[0];n = int_of_string (List.hd t); name = ""}
    else if h.[0] = 'L' then {ins = h.[0];n = 0;name = (List.hd t)}
    else {ins = 'E';n = 0;name = ""}

let copy_process (p : pcb) = 
  {name = p.name; start = p.start; variable = p.variable; pid = !next_pid; ppid = p.pid; priority = p.priority; arrival_time = !time + 1; burst_time = p.time; time = 0; pc = (p.pc + 1); status = 1; finish = -1}

let rec find_process_name s i =
  if (i = List.length !pcb_table) then -1
  else if (List.nth !pcb_table i).name = s then i
  else find_process_name s (i + 1)

let openfile newP =
  let ind = find_process_name (String.sub newP.name 0 (String.length newP.name - 4)) 0 in
    if (ind <> -1) then
      begin
        let p = List.nth !pcb_table ind in
        let process = {name = String.sub newP.name 0 (String.length newP.name - 4); start = p.start; variable = 0; pid = !next_pid; ppid = 0; priority = newP.priority; arrival_time = newP.time; burst_time = p.burst_time; time = 0; pc = 0; status = 0; finish = -1} in
        next_pid := !next_pid + 1;
        pcb_table := !pcb_table @ [process]; 
        Queue.push process readyQ
      end
    else
      let process = {name = String.sub newP.name 0 (String.length newP.name - 4); start = !next_memory_index; variable = 0; pid = !next_pid; ppid = 0; priority = newP.priority; arrival_time = newP.time; burst_time = 0; time = 0; pc = 0; status = 0; finish = -1} in
      let fi = open_in (newP.name) in
      let flag = ref true in
      let burst = ref 0 in
      let line = ref "" in
      let instr = ref {ins = 'N'; n = 0; name = ""} in
      while (!flag) do
        try 
          begin 
            line := input_line fi;
            instr := line_to_instr !line;
            memory.(!next_memory_index) <- !instr;
            next_memory_index := !next_memory_index + 1;
            if (!instr.ins = 'C') then burst := !burst - !instr.n + 2 else burst := !burst + 1
          end
        with End_of_file -> begin process.burst_time <- !burst ;close_in fi; flag := false; pcb_table := !pcb_table @ [process]; Queue.push process readyQ; next_pid := !next_pid + 1 end
      done

let openfile_string str =
  let fi = open_in (str^".prg") in
  let flag = ref true in
  let line = ref "" in
  let burst = ref 0 in
  let instr = ref {ins = 'N'; n = 0; name = ""} in
  let () = while (!flag) do
    try 
      begin 
        line := input_line fi;
        instr := line_to_instr !line;
        memory.(!next_memory_index) <- !instr;
        next_memory_index := !next_memory_index + 1;
        burst := !burst + 1;
      end
    with End_of_file -> begin close_in fi; flag := false end
  done in !burst
  
let read_instr process =
  let instr = memory.(process.start + process.pc) in
  begin
    (match instr.ins with
    | 'M' -> (process.variable <- instr.n; process.pc<-(process.pc+1))
    | 'A' -> (process.variable <- (process.variable + instr.n); process.pc<-(process.pc+1)) 
    | 'S' -> (process.variable <- (process.variable - instr.n); process.pc<-(process.pc+1)) 
    | 'B' -> (process.status <- 2; process.pc <- (process.pc+1); 
              if (process.ppid = 0) then (executing_flag := false; running_proc.ind <- -1; running_proc.pid <- -1; running_proc.pc <- -1; rem_time := 1; if (Short.(!selected_scheduller) = 6) then rr_flag := true; if (not (Queue.is_empty readyQ)) then preempt_flag := true)
              else (running_proc.ind <- Short.findProcInd !pcb_table process.ppid 0; running_proc.pid <- process.ppid; running_proc.pc <- (List.nth !pcb_table running_proc.ind).pc))
    | 'T' -> (let () = (if Memory.deallocate_mem (running_proc.pid) = -1 then Printf.printf  "Erro de dealocação de memória - Processo pid: %d sem memória alocada\n" process.pid else (); process.pc<-(process.pc+1)) in
              if (process.ppid <> 0) 
                then (running_proc.ind <- Short.findProcInd !pcb_table process.ppid 0; running_proc.pid <- process.ppid; running_proc.pc <- (List.nth !pcb_table running_proc.ind).pc)
                else (running_proc.ind <- -1; running_proc.pid <- -1; running_proc.pc <- -1); if (not (Queue.is_empty readyQ)) then preempt_flag := true;
                  process.status <- 3; process.pc<-(process.pc+1); executing_flag := false; if (Short.(!selected_scheduller) = 6) then rr_flag := true; rem_time := 1; process.finish <- (!time + 1))
    | 'C' -> let filho = copy_process process in
              begin
                process.pc <- (process.pc + instr.n);
                process.status <- 0;
                next_pid := !next_pid + 1;
                pcb_table := !pcb_table @ [filho];
                running_proc.ind <- Short.findProcInd !pcb_table filho.pid 0;
                running_proc.pid <- filho.pid;
                running_proc.pc <- filho.pc;

              end
    | 'L' -> let ind = find_process_name instr.name 0 in
              if (ind <> -1) then                               (* Encontrou o programa já aberto -> não precisa de abrir *)
                begin
                  process.start <- (List.nth !pcb_table ind).start;
                  process.name <- instr.name;
                  process.variable <- 0;
                  process.pc <- 0;
                  process.burst_time <- (List.nth !pcb_table ind).burst_time
                end
              else                                              (* Não encontrou o programa -> Temos de abrir *)
                begin
                  process.start <- !next_memory_index;
                  process.name <- instr.name;
                  process.variable <- 0;
                  process.pc <- 0;
                  process.burst_time <- openfile_string instr.name;
                end
    | 'H' -> (Memory.solicitate_allocation process.pid; process.pc<-(process.pc+1))
    | 'D' -> (if Memory.deallocate_mem (running_proc.pid) = -1 then Printf.printf  "Erro de dealocação de memória - Processo pid: %d sem memória alocada\n" process.pid else (); process.pc<-(process.pc+1))
    | _ -> Printf.printf  "Instrução inválida\n");
    running_proc.pc <- process.pc
  end