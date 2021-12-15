open Connectai

let trim_command str : string= String.trim (String.lowercase_ascii (str))


let read_command () =
  match String.trim (read_line ()) with
  | "quit" -> exit 0
  | str -> str

let check_rc input=
  ANSITerminal.(
    let check_int (str:string): int = 
      try str |> int_of_string with
      | Failure(e) -> 0 
    in 
    let rec helper i = 
      if (check_int i) < 0 || (check_int i) > 10
      then (print_endline ("\nPlease enter a positive, non-zero number " ^ 
                           "less than eleven: ");
            print_string [cyan] "> "; helper (read_command () |> trim_command))
      else (check_int i) 
    in 
    if (check_int input) < 0 || (check_int input) > 10
    then helper (input)
    else (check_int input))


let check_to_win input (rows:int) (cols:int) =
  ANSITerminal.(
    let win = if rows > cols then rows else cols in
    let check_int (str:string): int = 
      try str |> int_of_string with
      | Failure(e) -> 0 
    in 
    let rec helper i = 
      if (check_int i) <= 0 || ((check_int i) > win)
      then (print_endline 
              ("\nPlease enter a positive, non-zero number less or equal to "^
               "than "^(string_of_int win)^": ");
            print_string [cyan] "> ";helper (read_command () |> trim_command))
      else (check_int i) 
    in 
    if (check_int input) <= 0 || (check_int input) > win
    then helper (input)
    else (check_int input))

let check_play_type input =
  ANSITerminal.(
    let rec helper i = 
      if i <> "ai" && i <> "player"
      then  (print_endline ("\nPlease enter either 'ai' or 'player': ");
             print_string [cyan] "> "; helper (read_command () |> trim_command))
      else i
    in 
    if input <> "ai" && input <> "player"
    then helper (input)
    else input)

let check_start_menu input = 
  ANSITerminal.(
    let rec helper i = 
      if i <> "start" && i <> "tutorial"
      then  (print_endline ("\nPlease enter either 'start' or 'tutorial': ");
             print_string [cyan] "> "; helper (read_command () |> trim_command))
      else i
    in 
    if input <> "start" && input <> "tutorial"
    then helper (input)
    else input)

let check_endscreen input =
  ANSITerminal.(
    let rec helper i = 
      if i <> "main"
      then  (print_endline ("\nPlease enter either 'main' or 'quit': ");
             print_string [cyan] "> "; helper (read_command () |> trim_command))
      else i
    in 
    if input <> "main"
    then helper (input)
    else input)

let print_title () = ANSITerminal.(
    Connectx.clear_screen ();
    print_string [red] 
      ("\n\n          🟥🟥🟥🟥🟥  🟥🟥🟥🟥🟥  🟥      🟥  🟥      🟥  "^
       "🟥🟥🟥🟥🟥  🟥🟥🟥🟥🟥  🟥🟥🟥🟥🟥\n          🟥      🟥  🟥 "^
       "     🟥  🟥🟥    🟥  🟥🟥    🟥  🟥          🟥      🟥      🟥"^
       "\n          🟥          🟥      🟥  🟥  🟥  🟥  🟥  🟥  🟥  🟥🟥"^
       "🟥      🟥              🟥\n          🟥      🟥  🟥      🟥  🟥  "^
       "  🟥🟥  🟥    🟥🟥  🟥          🟥      🟥      🟥\n          🟥"^
       "🟥🟥🟥🟥  🟥🟥🟥🟥🟥  🟥      🟥  🟥      🟥  🟥🟥🟥🟥🟥  🟥"^
       "🟥🟥🟥🟥      🟥      \n\n            🟨🔳🔳🔳🟣🔳🟨            "^
       "        🟥      🟥                🟨🔵🔳🔳🟨\n            🟨🔳🟡"^
       "🟡🟡🔳🟨                      🟥  🟥                  🟨🔵🔳🔳🟨"^
       "\n            🟨🟡🟣🟣🟣🔳🟨                        🟥            "^
       "        🟨🟢🔳🟢🟨\n            🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 🟨             "^
       "         🟥  🟥                  🟨1️⃣ 2️⃣ 3️⃣ 🟨\n            🟨    "^
       "      🟨                    🟥      🟥                🟨      🟨
       "))

let run_tutorial ()=
  ANSITerminal.(
    Connectx.clear_screen ();
    print_string [yellow] 
      "\nWelcome to Connect-X!\n";
    print_string [white]
      ("Connect-X mirrors the popular tabletop game of connect four. In \n" ^
       "Connect 4 players play on a vertical 6x7 board, and take turns \n" ^
       "dropping chips in until one player gets four chips in a row, as \n"^
       "shown below! This can be horizontal, vertical, or even diagonal.\n"^
       "In this example, the red player has just won: \n\n"^
       "🟨🔳🔳🔳🔴🔳🔳🔳🟨    🟨🔳🔳🔳🔳🔳🔳🔳🟨    "^
       "🟨🔳🔳🔳🔳🔳🔳🔳🟨    🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "🟨🔳🔳🔳🔳🔳🔳🔳🟨    🟨🔳🔳🔳🔴🔳🔳🔳🟨    "^
       "🟨🔳🔳🔳🔳🔳🔳🔳🟨    🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "🟨⚫🔳🔳🔳🔳🔳⚫🟨    🟨⚫🔳🔳🔳🔳🔳⚫🟨    "^
       "🟨⚫🔳🔳🔴🔳🔳⚫🟨    🟨⚫🔳🔳🟢🔳🔳⚫🟨\n"^
       "🟨⚫🔴🔴⚫🔳🔳🔴🟨 -> 🟨⚫🔴🔴⚫🔳🔳🔴🟨 -> "^
       "🟨⚫🔴🔴⚫🔳🔳🔴🟨 -> 🟨⚫🔴🟢⚫🔳🔳🔴🟨\n"^
       "🟨🔴🔴🔴⚫🔳🔳🔴🟨    🟨🔴🔴🔴⚫🔳🔳🔴🟨    "^
       "🟨🔴🔴🔴⚫🔳🔳🔴🟨    🟨🔴🟢🔴⚫🔳🔳🔴🟨\n"^
       "🟨🔴⚫⚫⚫🔴🔳⚫🟨    🟨🔴⚫⚫⚫🔴🔳⚫🟨    "^
       "🟨🔴⚫⚫⚫🔴🔳⚫🟨    🟨🟢⚫⚫⚫🔴🔳⚫🟨\n"^
       "🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 🟨    🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ "^
       "🟨    🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 🟨    🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 🟨"^
       "\n🟨              🟨    🟨              🟨    🟨              🟨"^
       "    🟨              🟨\n\n");
    print_string [yellow] 
      ("Note: the above diagram depicts a player placing a chip into the \n"^
       "fourth column and it falling down. The green chips simply highlight\n"^
       "the chips that make up red's winning move (note there are four chips\n"^
       "in a row diagonally in figure 3).\n");
    print_string [white] "Press enter when you want to continue";
    read_command ();
    Connectx.clear_screen ();
    print_string [yellow] 
      "\nOptions\n";
    print_string [white]
      ("When you boot up the game, Connect-X will ask you a series of \n" ^
       "questions about how you would like the game to be. This includes \n" ^
       "the amount of rows you want to play with, the amount of columns you \n"^
       "want to play with, and the number of chips needed to be in a row \n"^
       "for a player to win. In the below example, the players are playing \n"^
       "on a board with 7 columns and 6 rows, and need to connect 4 chips \n"^
       "to win (the standard connect 4). The columns are numbered for ease \n"^
       "of understanding. You can play with a maximum of 10 rows and 10 \n"^
       "columns, with minimun of 1 (although that would be a quick game). \n"^
       "Be aware that you can't have the win condition be more chips than \n"^
       "your board's longest axis! \n"^
       "\n"^
       "   🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "   🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "   🟨⚫🔳🔳🟢🔳🔳⚫🟨\n"^
       "   🟨⚫🔴🟢⚫🔳🔳🔴🟨\n"^
       "   🟨🔴🟢🔴⚫🔳🔳🔴🟨\n"^
       "   🟨🟢⚫⚫⚫🔴🔳⚫🟨\n"^
       "   🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 🟨\n"^
       "   🟨              🟨\n\n" ^
       "You can also customize the chips that you play with. The game above \n"^
       "uses red and black chips. You can play with many other types of \n"^
       "color chips, as well as others!\n\n"^
       "Press enter when you want to continue:\n");
    read_command ();
    Connectx.clear_screen ();
    print_string [yellow] 
      "\nGamemodes\n";
    print_string [white]
      ("There are a number of different gamemodes and difficulties you can \n" ^
       "play with. The first choice you have is whether to play with a \n"^
       "friend at the same computer, or whether to play against the AI. If \n"^
       "you want to play against the AI, there are three different \n"^
       "difficulty levels: easy, medium, and hard. In hard mode, the AI gets \n"^
       "two moves for every one that you make!.\n\n"^
       "There are also two different gamemodes you can play on: classic or \n"^
       "terrain mode. In terrain mode, the bottom of the board is broken up \n"^
       "by obsticles. You must be playing with at least 3 rows to play on \n"^
       "terrain mode. A terrain mode board is shown below: \n\n"^
       "   🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "   🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "   🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "   🟨🔳🔳🔳🔳🔳🔳🔳🟨\n"^
       "   🟨🔳🔳⚫🔳🔳🔳🔳🟨\n"^
       "   🟨🔳⚫⚫⚫🔳🔳⚫🟨\n"^
       "   🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 🟨\n"^
       "   🟨              🟨\n\n" ^
       "You are now ready to play the game! Press enter to exit the \n"^
       "and start the game! Remember, you can enter 'quit' at any time to \n"^
       "exit the game.\n");
    read_command ();
    Connectx.clear_screen ();
  )

let rec initialize () =
  ANSITerminal.(
    Connectx.clear_screen ();
    print_string [yellow] 
      ("\n\n          🟥🟥🟥🟥🟥  🟥🟥🟥🟥🟥  🟥      🟥  🟥      🟥  "^
       "🟥🟥🟥🟥🟥  🟥🟥🟥🟥🟥  🟥🟥🟥🟥🟥\n          🟥      🟥  🟥 "^
       "     🟥  🟥🟥    🟥  🟥🟥    🟥  🟥          🟥      🟥      🟥"^
       "\n          🟥          🟥      🟥  🟥  🟥  🟥  🟥  🟥  🟥  🟥🟥"^
       "🟥      🟥              🟥\n          🟥      🟥  🟥      🟥  🟥  "^
       "  🟥🟥  🟥    🟥🟥  🟥          🟥      🟥      🟥\n          🟥"^
       "🟥🟥🟥🟥  🟥🟥🟥🟥🟥  🟥      🟥  🟥      🟥  🟥🟥🟥🟥🟥  🟥"^
       "🟥🟥🟥🟥      🟥      \n\n            🟨🔳🔳🔳🟣🔳🟨            "^
       "        🟥      🟥                🟨🔵🔳🔳🟨\n            🟨🔳🟡"^
       "🟡🟡🔳🟨                      🟥  🟥                  🟨🔵🔳🔳🟨"^
       "\n            🟨🟡🟣🟣🟣🔳🟨                        🟥            "^
       "        🟨🟢🔳🟢🟨\n            🟨1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 🟨             "^
       "         🟥  🟥                  🟨1️⃣ 2️⃣ 3️⃣ 🟨\n            🟨    "^
       "      🟨                    🟥      🟥                🟨      🟨
      \n                                         Welcome to Connect-X! \n\n"^
       "In this game you can play the "^
       "classic game connect4 with any number of customizable "^
       "game options. \nTo begin a game, enter 'start'. To run a tutorial for "^
       "how to play the various gamemodes, enter 'tutorial'. "^
       "\nEnter 'quit at any time to quit the game.\n");
    print_string [cyan] "> ";
    let input = read_command () |> trim_command in
    let menu = check_start_menu (input) in 
    if menu = "tutorial" then run_tutorial ();
    print_title ();
    print_endline ("\nPlease enter how many columns to play with "
                   ^"(a number between 1 and 10): ");
    print_string [cyan] "> ";
    let input = read_command () |> trim_command in
    let cols = check_rc (input) in 
    print_title ();
    print_endline ("\nPlease enter how many rows to play with "
                   ^"(a number between 1 and 10): ");
    print_string [cyan] "> ";
    let input = read_command () |> trim_command in
    let rows = check_rc (input) in
    print_title ();
    print_endline ("\nPlease enter how many connected chips are needed to win "
                   ^"(e.g. connect 4 would have 4): ");
    print_string [cyan]  "> ";
    let input = read_command () |> trim_command in
    let connect =  check_to_win input rows cols in
    print_title ();
    print_endline ("\nDo you want to play against another player, or against AI?"
                   ^" Enter 'player' for player, and 'ai' for ai: ");
    print_string [cyan] "> ";
    let input = read_command () |> trim_command in
    let playtype = check_play_type input in
    if playtype = "ai" 
    then pvai rows cols connect 
    else pvp rows cols connect; 
    print_endline ("\nWould you like to play another game? Enter 'main' to "
                   ^"return to the main menu. Or, enter 'quit'"
                   ^" exit the program: ");
    print_string [cyan] "> ";
    let input = read_command () |> trim_command in
    let endscreen = check_endscreen input in
    if endscreen = "main" 
    then Connectx.clear_screen (); initialize ()
  )

let main () =
  initialize

let () = main () () 