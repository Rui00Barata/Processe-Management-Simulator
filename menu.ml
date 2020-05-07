open Lib

let debug_menu = 
  let flag = ref true in
  while !flag do
    Printf.printf "Quer entrar no debug mode [y/n]: ";
    if ('y' = read_char () || 'Y' = read_char ()) then begin Process.debug_mode := true; flag := false end
    else if ('n' = read_char () || 'N' = read_char ()) then begin Process.debug_mode := false; flag := false end
    else Printf.printf "ERRO! Opção inválida\n"
  done

let scheduling_menu  =
  let flag = ref true in
  while !flag do
    Printf.printf "Menu de Escalonamento\n\n1 - FCFS\n\n0 - Sair\n";
    let op = read_int () in
    match op with
    | 1 -> begin 
      controller false;
      flag := false
    end
    | 0 -> exit 0
    | _ -> ()
done

let options_menu  =
  let flag = ref true in
  while !flag do
    Printf.printf "Menu de Opções\n\n1 - Escalonamento Não Preemptivo\n2 - Escalonamento Preemptivo\n3 - Alterar Time Quantum\n4 - Debug Mode\n\n0 - Sair\n";
    let op = read_int () in
    match op with
    | 1 -> begin 
      scheduling_menu;
      flag := false
    end
    | 2 -> Printf.printf "Work in Progress\n"
    | 3 -> begin Printf.printf "Insira o novo valor do Time Quantum: "; time_quantum := read_int () end
    | 4 -> debug_menu 
    | 0 -> flag := false
    | _ -> ()
  done

let menu = 
  let flag = ref true in
  let () = read_plan () in
  while !flag do
    Printf.printf "Menu\n\n1 - Iniciar\n2 - Opções\n\n0 - Sair\n";
    match (read_int ()) with
    | 1 -> clock 
    | 2 -> options_menu
    | 0 -> exit 0 
    | _ -> ()
  done