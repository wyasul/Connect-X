open Connectx

let trim_command str : string= String.trim (String.lowercase_ascii (str))

let read_command () =
  match String.trim (read_line ()) with
  | "quit" -> exit 0
  | str -> str

let rec check_command_int input=
  let check_int (str:string): int = 
    try str |> int_of_string with
    | Failure(e) -> 0
  in if (check_int input) = 0 then
    check_command_int (read_command ())
  else (check_int input)

let check_difficulty input =
  let rec helper i = 
    if i <> "easy" && i <> "medium" && i <> "hard"
    then  (print_endline ("\nPlease enter either "
                          ^"'easy', 'medium', or 'hard': ");
           print_string  "> "; helper (read_command () |> trim_command))
    else i
  in 
  if input <> "easy" && input <> "medium" && input <> "hard"
  then helper (input)
  else input

let turn_rotation game tracker = 
  match tracker mod 2 with
  | 0 -> fst (Connectx.player_names game)
  | _ -> snd (Connectx.player_names game)

let check_play_col input (cols:int)=
  let check_int (str:string): int = 
    try str |> int_of_string with
    | Failure(e) -> 0 
  in 
  let rec helper i = 
    if (check_int i) <= 0 || (check_int i) > cols
    then  (print_endline ("\nPlease enter a positive, "
                          ^"non-zero number less or equal to than "
                          ^(string_of_int cols)^": ");
           print_string  "> ";helper (read_command () |> trim_command))
    else (check_int i) 
  in 
  if (check_int input) <= 0 || (check_int input) > cols
  then helper (input)
  else (check_int input)

let check_chip input =
  let is_chip i = 
    i = "white" || i = "black" || i = "red" || i = "orange" || i = "yellow" || 
    i = "green" || i = "blue" || i = "purple" || i = "usa" || i = "ogre" || 
    i = "skull" || i = "pup" || i = "frog" || i = "uni" || i = "dragon" || 
    i = "peach" || i = "coconut" || i = "pineapple" || i = "moai" || 
    i = "diamond"  
  in
  let rec helper i = 
    if not (is_chip i) || i = "chips"
    then  (print_string ("\n'black'     -> ⚫");
           print_endline ("   'white'     -> ⚪");
           print_string ("'orange'    -> 🟠");
           print_endline ("   'red'       -> 🔴");
           print_string ("'green'     -> 🟢");
           print_endline ("   'yellow'    -> 🟡");
           print_string ("'purple'    -> 🟣");
           print_endline ("   'blue'      -> 🔵");
           print_string ("'moai'      -> 🗿");
           print_endline ("   'usa'       -> 🗽");
           print_string ("'skull'     -> 💀");
           print_endline ("   'ogre'      -> 👹");
           print_string ("'frog'      -> 🐸");
           print_endline ("   'pup'       -> 🐶");
           print_string ("'dragon'    -> 🐲");
           print_endline ("   'uni        -> 🦄");
           print_string ("'coconut'   -> 🥥");
           print_endline ("   'peach'     -> 🍑");
           print_string ("'diamond'   -> 🔶");
           print_endline ("   'pineapple' -> 🍍");
           print_endline ("Please enter one of the above chip types: ");
           print_string  "> "; helper (read_command () |> trim_command))
    else i
  in 
  if not (is_chip input) || input = "chips"
  then helper (input)
  else input

let rec player_move_safe col game chip=
  try Connectx.make_move_player game (col-1) chip 
  with Failure (e) -> print_endline ("\n"^e^" Please enter another column: ");
    print_string  "> "; 
    player_move_safe 
      (check_play_col (read_command () |> trim_command) 
         (Connectx.board_col game)) game chip

let rec ai_move_safe col game chip=
  try Connectx.make_move_ai game col chip 
  with Failure (e) -> 
    ai_move_safe (Random.int (Connectx.board_col game)) game chip

let rec take_turn_pvp game tracker= 
  ANSITerminal.(
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst |> 
             Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd |> 
             Connectx.match_string_to_chip
    in
    print_endline 
      ((player) ^ 
       "'s turn. Please enter which col you want to drop a chip into (1-" ^
       ((Connectx.board_row game) |> string_of_int)^"): ");
    print_string [cyan] "> ";
    let col = check_play_col (read_command ()) (Connectx.board_col game) in
    player_move_safe col game chip;
    if Connectx.winning_move game (col-1) chip = chip 
    then print_endline (player^" wins!") 
    else begin 
      if Connectx.full_board game 
      then print_endline 
          ("There are no further legal moves. The game is a draw.") 
      else take_turn_pvp game (tracker+1)
    end )

let rec take_turn_ai_easy game tracker= 
  ANSITerminal.(
    Random.self_init ();
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst |> 
             Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd |> 
             Connectx.match_string_to_chip
    in
    let ai_funcs game player chip tracker = 
      let col = Random.int (Connectx.board_col game-1) in 
      print_endline 
        ((player) ^ "'s turn ("^(chip |> Connectx.match_chip_to_string)^
         "). AI chose column "^string_of_int (col+1));
      ai_move_safe col game chip;
      if Connectx.winning_move game (col) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_easy game (tracker+1)
      end 
    in
    let player_funcs game player chip tracker= 
      print_endline 
        ((player) ^ "'s turn ("^(chip |> Connectx.match_chip_to_string)^
         "). Please enter which col you want to drop a chip into (1-"^
         ((Connectx.board_row game) |> string_of_int)^"): ");
      print_string [cyan] "> "; 
      let col = check_play_col (read_command ()) (Connectx.board_col game) in
      player_move_safe col game chip;
      if Connectx.winning_move game (col-1) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_easy game (tracker+1)
      end 
    in
    if player <> "A.I." 
    then player_funcs game player chip tracker 
    else ai_funcs game player chip tracker)

let rec take_turn_ai_medium game tracker col =
  ANSITerminal.(
    Random.self_init ();
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst |> 
             Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd |> 
             Connectx.match_string_to_chip
    in
    let rand3 = Random.int 3 in
    let ai_funcs game player chip tracker = 
      let newcol = 
        if rand3 = 0 && col >=2 then col-2 else
        if rand3 = 1 && col >=1 then col-1 else
        if rand3 = 2 then col else col in
      ai_move_safe newcol game chip;
      if Connectx.winning_move game (col) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_medium game (tracker+1) col
      end 
    in
    let player_funcs game player chip tracker= 
      print_endline ((player) ^ "'s turn ("^
                     (chip |> Connectx.match_chip_to_string)^
                     "). Please enter which col you want to"^"
                      drop a chip into (1-"^
                     ((Connectx.board_row game) |> string_of_int)^"): ");
      print_string [cyan] "> "; 
      let col = check_command_int (read_command ()) in
      player_move_safe col game chip;
      if Connectx.winning_move game (col-1) chip = chip then 
        print_endline (player^" wins!")
      else if Connectx.full_board game = true then 
        print_endline "The grid is full with no winner. It's a tie!" 
      else take_turn_ai_medium game (tracker+1) col in
    if player <> "A.I." then player_funcs game player chip tracker 
    else ai_funcs game player chip tracker) 

let rec take_turn_ai_hard game tracker col=
  ANSITerminal.(
    Random.self_init ();
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst |> 
             Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd |> 
             Connectx.match_string_to_chip
    in
    let rand3 = Random.int 3 in
    let ai_funcs game player chip tracker = 
      let newcol = 
        if rand3 = 0 && col >=2 then col-2 else
        if rand3 = 1 && col >=1 then col-1 else
        if rand3 = 2 then col else col in
      ai_move_safe newcol game chip;
      let newrand = Random.int 3 in
      let col2 = if newrand = 0 && col >=2 then col-2 else
        if newrand = 1 && col >=1 then col-1 else
        if newrand = 2 then col else col in
      ai_move_safe col2 game chip;
      if Connectx.winning_move game (col) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_hard game (tracker+1) col
      end 
    in
    let player_funcs game player chip tracker= 
      print_endline 
        ((player) ^ "'s turn ("^(chip |> Connectx.match_chip_to_string)^
         "). Please enter which row you want to drop a chip into (1-"^
         ((Connectx.board_row game) |> string_of_int)^"): ");
      print_string [cyan] "> "; 
      let col = check_command_int (read_command ()) in
      player_move_safe col game chip;
      if Connectx.winning_move game (col-1) chip = chip then 
        print_endline (player^" wins!")
      else if Connectx.full_board game = true then 
        print_endline "The grid is full with no winner. It's a tie!" 
      else take_turn_ai_hard game (tracker+1) col in
    if player <> "A.I." then player_funcs game player chip tracker 
    else ai_funcs game player chip tracker) 

let rec take_turn_ai_easy_terrain game tracker= 
  ANSITerminal.(
    set_terrain game;
    Random.self_init ();
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst |> 
             Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd |> 
             Connectx.match_string_to_chip
    in
    let ai_funcs game player chip tracker = 
      let col = Random.int (Connectx.board_col game-1) in 
      print_endline 
        ((player) ^ "'s turn ("^(chip |> Connectx.match_chip_to_string)^
         "). AI chose column "^string_of_int (col+1));
      ai_move_safe col game chip;
      if Connectx.winning_move game (col) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_easy_terrain game (tracker+1)
      end 
    in
    let player_funcs game player chip tracker= 
      print_endline 
        ((player) ^ "'s turn ("^(chip |> Connectx.match_chip_to_string)^
         "). Please enter which col you want to drop a chip into (1-"^
         ((Connectx.board_row game) |> string_of_int)^"): ");
      print_string [cyan] "> "; 
      let col = check_play_col (read_command ()) (Connectx.board_col game) in
      player_move_safe col game chip;
      if Connectx.winning_move game (col-1) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_easy_terrain game (tracker+1)
      end 
    in
    if player <> "A.I." 
    then player_funcs game player chip tracker 
    else ai_funcs game player chip tracker)

let rec take_turn_ai_medium_terrain game tracker col =
  ANSITerminal.(
    set_terrain game;
    Random.self_init ();
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst |> 
             Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd |> 
             Connectx.match_string_to_chip
    in
    let rand3 = Random.int 3 in
    let ai_funcs game player chip tracker = 
      let newcol = 
        if rand3 = 0 && col >=2 then col-2 else
        if rand3 = 1 && col >=1 then col-1 else
        if rand3 = 2 then col else col in
      ai_move_safe newcol game chip;
      if Connectx.winning_move game (col) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_medium_terrain game (tracker+1) col
      end 
    in
    let player_funcs game player chip tracker= 
      print_endline 
        ((player) ^ "'s turn ("^(chip |> Connectx.match_chip_to_string)^
         "). Please enter which col you want to drop a chip into (1-"^
         ((Connectx.board_row game) |> string_of_int)^"): ");
      print_string [cyan] "> "; 
      let col = check_command_int (read_command ()) in
      player_move_safe col game chip;
      if Connectx.winning_move game (col-1) chip = chip then 
        print_endline (player^" wins!")
      else if Connectx.full_board game = true then 
        print_endline "The grid is full with no winner. It's a tie!" 
      else take_turn_ai_medium_terrain game (tracker+1) col in
    if player <> "A.I." then player_funcs game player chip tracker 
    else ai_funcs game player chip tracker) 

let rec take_turn_ai_hard_terrain game tracker col=
  ANSITerminal.(
    set_terrain game;
    Random.self_init ();
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst |>
             Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd |> 
             Connectx.match_string_to_chip
    in
    let rand3 = Random.int 3 in
    let ai_funcs game player chip tracker = 
      let newcol = 
        if rand3 = 0 && col >=2 then col-2 else
        if rand3 = 1 && col >=1 then col-1 else
        if rand3 = 2 then col else col in
      ai_move_safe newcol game chip;
      let newrand = Random.int 3 in
      let col2 = if newrand = 0 && col >=2 then col-2 else
        if newrand = 1 && col >=1 then col-1 else
        if newrand = 2 then col else col in
      ai_move_safe col2 game chip;
      if Connectx.winning_move game (col) chip = chip 
      then print_endline (player^" wins!")
      else begin 
        if Connectx.full_board game 
        then print_endline ("There are no further legal moves."
                            ^" The game is a draw.") 
        else take_turn_ai_hard_terrain game (tracker+1) col
      end 
    in
    let player_funcs game player chip tracker= 
      print_endline
        ((player) ^ "'s turn ("^(chip |> Connectx.match_chip_to_string)^
         "). Please enter which row you want to drop a chip into (1-"^
         ((Connectx.board_row game) |> string_of_int)^"): ");
      print_string [cyan] "> "; 
      let col = check_command_int (read_command ()) in
      player_move_safe col game chip;
      if Connectx.winning_move game (col-1) chip = chip then 
        print_endline (player^" wins!")
      else if Connectx.full_board game = true then 
        print_endline "The grid is full with no winner. It's a tie!" 
      else take_turn_ai_hard_terrain game (tracker+1) col in
    if player <> "A.I." then player_funcs game player chip tracker 
    else ai_funcs game player chip tracker) 

let rec take_turn_pvp_terrain game tracker= 
  set_terrain game;
  ANSITerminal.(
    let player = turn_rotation game tracker in
    let (chip:Connectx.chips)= match tracker mod 2 with
      | 0 -> game |> Connectx.player_chips |> fst
             |> Connectx.match_string_to_chip
      | _ -> game |> Connectx.player_chips |> snd 
             |> Connectx.match_string_to_chip
    in
    print_endline 
      ((player) ^ 
       "'s turn. Please enter which col you want to drop a chip into (1-" ^
       ((Connectx.board_row game) |> string_of_int)^"): ");
    print_string [cyan] "> ";
    let col = check_play_col (read_command ()) (Connectx.board_col game) in
    player_move_safe col game chip;
    if Connectx.winning_move game (col-1) chip = chip 
    then print_endline (player^" wins!") 
    else begin 
      if Connectx.full_board game 
      then print_endline 
          ("There are no further legal moves. The game is a draw.") 
      else take_turn_pvp game (tracker+1)
    end)

let play_game_pvp state= 
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_pvp state 0

let play_game_pvp_terrain state =
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_pvp_terrain state 0

let play_game_ai_easy state = 
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_ai_easy state 0

let play_game_ai_medium state = 
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_ai_medium state 0 0

let play_game_ai_hard state = 
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_ai_hard state 0 0

let play_game_ai_easy_terrain state = 
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_ai_easy_terrain state 0

let play_game_ai_medium_terrain state = 
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_ai_medium_terrain state 0 0

let play_game_ai_hard_terrain state = 
  Connectx.clear_screen ();
  Connectx.print_current_grid state;
  take_turn_ai_hard_terrain state 0 0 

let check_mode input =
  ANSITerminal.(
    let rec helper i = 
      if i <> "normal" && i <> "terrain"
      then  (print_endline ("\nPlease enter either 'normal' or 'terrain': ");
             print_string [cyan] "> "; helper (read_command () |> trim_command))
      else i
    in 
    if input <> "normal" && input <> "terrain"
    then helper (input)
    else input)

let pvp rows cols connect = 
  ANSITerminal.(
    print_endline "\nPlease enter the name of player 1: ";
    print_string [cyan]  "> ";
    let p1 = read_command() in

    print_endline 
      ("\nWhat kind of chip would you like to play with? Enter a chip"
       ^" or enter 'chips' for a list of possible chips: ");
    print_string  [cyan] "> ";
    let input = read_command () |> trim_command in
    let chip1 = check_chip input in

    print_endline "\nPlease enter the name of player 2: ";
    print_string  [cyan] "> ";
    let p2 = read_command() in

    print_endline 
      ("\nWhat kind of chip would you like to play with? Enter a chip"
       ^" or enter 'chips' for a list of possible chips: ");
    print_string  [cyan] "> ";
    let input = read_command () |> trim_command in
    let chip2 = check_chip input in
    if rows>=5 then
      (print_endline "Normal or Terrain mode? ";
       print_string [cyan] "> ";
       let mode = check_mode 
           (read_command() |> trim_command |> String.lowercase_ascii) in
       if mode = "terrain" then 
         play_game_pvp_terrain (Connectx.init_game rows cols connect (p1,p2) 
                                  (chip1,chip2))
       else
         play_game_pvp (Connectx.init_game rows cols connect (p1,p2) 
                          (chip1,chip2)))
    else play_game_pvp (Connectx.init_game rows cols connect (p1,p2) 
                          (chip1,chip2)))

let pvai rows cols connect = 
  ANSITerminal.(
    print_endline "\nPlease enter your name: ";
    print_string [cyan] "> ";
    let p1 = read_command() in
    print_endline 
      ("\nWhat kind of chip would you like to play with? Enter a chip"
       ^" or enter 'chips' for a list of possible chips: ");
    print_string [cyan] "> ";
    let input = read_command () |> trim_command in
    let chip1 = check_chip input in
    print_endline 
      ("\nWhat kind of chip would you like the ai play with? Enter a "
       ^"chip or enter 'chips' for a list of possible chips: ");
    print_string  [cyan] "> ";
    let input = read_command () |> trim_command in
    let chip2 = check_chip input in
    if rows>=5 then
      (print_endline "Normal or Terrain mode? ";
       print_string [cyan] "> ";
       let mode = check_mode 
           (read_command() |> trim_command |> String.lowercase_ascii) in
       print_endline "\nEasy, Medium, or Hard diffulty? ";
       print_string [cyan] "> ";
       let input = read_command() |> trim_command |> String.lowercase_ascii in
       let difficulty = check_difficulty input in
       if mode = "normal" then begin
         if difficulty = "easy" then
           play_game_ai_easy (Connectx.init_game rows cols connect (p1,"A.I.")
                                (chip1,chip2))
         else if difficulty = "medium" then 
           play_game_ai_medium (Connectx.init_game rows cols connect 
                                  (p1,"A.I.") (chip1,chip2))
         else if difficulty = "hard" then
           play_game_ai_hard (Connectx.init_game rows cols connect (p1,"A.I.")
                                (chip1,chip2))
       end
       else if mode = "terrain" then  
         if difficulty = "easy" then
           play_game_ai_easy_terrain (Connectx.init_game rows cols connect 
                                        (p1,"A.I.") (chip1,chip2))
         else if difficulty = "medium" then 
           play_game_ai_medium_terrain (Connectx.init_game rows cols connect
                                          (p1,"A.I.") (chip1,chip2))
         else if difficulty = "hard" then
           play_game_ai_hard_terrain (Connectx.init_game rows cols connect 
                                        (p1,"A.I.") (chip1,chip2)))
    else print_endline "\nEasy, Medium, or Hard diffulty? ";
    print_string [cyan] "> ";
    let input = read_command() |> trim_command |> String.lowercase_ascii in
    let difficulty = check_difficulty input in
    if difficulty = "easy" then
      play_game_ai_easy (Connectx.init_game rows cols connect (p1,"A.I.") 
                           (chip1,chip2))
    else if difficulty = "medium" then 
      play_game_ai_medium (Connectx.init_game rows cols connect (p1,"A.I.") 
                             (chip1,chip2))
    else if difficulty = "hard" then
      play_game_ai_hard (Connectx.init_game rows cols connect (p1,"A.I.") 
                           (chip1,chip2)))
