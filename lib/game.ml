open Printf

(**Represents a chess piece.*)
type chess_piece =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | King
  | Queen

(**Players.*)
type player =
  | P1
  | P2

type game_piece = {
  piece : chess_piece;
  player : player;
  mutable pos : int * int;
}

type action = {
  game_piece : game_piece;
  new_pos : int * int;
}

type t = {
  mutable is_terminal : bool;
  mutable state : game_piece list;
  mutable check : bool * bool;
  mutable player_turn : player;
}

exception IllegalMoveError of string
exception GamePieceNotFoundError of string

(*HELPER FUNCTIONS*)
module Helpers = struct
  (**[string_of_chess_piece piece] is the string representation of [piece].*)
  let string_of_chess_piece = function
    | Pawn -> "Pawn"
    | Rook -> "Rook"
    | Knight -> "Knight"
    | Bishop -> "Bishop"
    | King -> "King"
    | Queen -> "Queen"

  (**[string_of_pos pos] is the string representation of [pos].*)
  let string_of_pos pos = sprintf "(%d , %d)" (fst pos) (snd pos)

  (**[string_of_player player] is the string representation of [player].*)
  let string_of_player = function
    | P1 -> "Player 1"
    | P2 -> "Player 2"

  (*determine if game_piece is occupied: @dom i just allowed player to be its
    own argument so you can get pieces occupied by current player and opponent
    with the same function*)

  (**[is_occupied_by_player pos game player] is true iff [pos] is occupied by
     player in the context of [game].*)
  let is_occupied_by_player pos game player =
    let player_pieces = List.filter (fun gp -> gp.player = player) game.state in
    List.mem pos (List.map (fun gp -> gp.pos) player_pieces)

  (** [capture] filters the game stage so as to remove the captured element form
      play. *)
  let capture (action : action) game =
    List.filter_map
      (fun gp ->
        if gp.pos = action.new_pos && gp <> action.game_piece then None
        else Some gp)
      game.state

  (*find if two positions form an l shape for knights*)
  let is_l_shaped pos1 pos2 =
    let dy = abs (snd pos2 - snd pos1) in
    let dx = abs (fst pos2 - fst pos1) in
    (dx = 2 && dy = 1) || (dx = 1 && dy = 2)

  let is_valid_position pos =
    fst pos <= 8 && fst pos >= 1 && snd pos <= 8 && snd pos >= 1

  (**[start_postion_to_piece pos] onverts the starting position [pos] to the
     coresponding piece if it has one. Raises a failure if [pos] is not a valid
     sstarting position for a chess piece in an 8*8 chess board.*)
  let starting_position_to_piece pos =
    (*helper to get royal from x coordinate.*)
    let row_to_royal row =
      match row with
      | 1 | 8 -> Rook
      | 2 | 7 -> Knight
      | 3 | 6 -> Bishop
      | 4 -> Queen
      | 5 -> King
      | _ -> failwith (sprintf "Invalid row of %s" (string_of_int row))
    in
    (*build game pieces *)
    match pos with
    | y, 1 -> { player = P1; piece = row_to_royal y; pos }
    | _, 2 -> { player = P1; piece = Pawn; pos }
    | _, 7 -> { player = P2; piece = Pawn; pos }
    | y, 8 -> { player = P2; piece = row_to_royal y; pos }
    | _ -> failwith (sprintf "Invalid position: (%d, %d)" (fst pos) (snd pos))

  let string_of_game_piece game_piece =
    match (game_piece.piece, game_piece.player) with
    | Pawn, P1 -> "\u{265F}" (* White Pawn *)
    | Rook, P1 -> "\u{265C}" (* White Rook *)
    | Bishop, P1 -> "\u{265D}" (* White Bishop *)
    | Knight, P1 -> "\u{265E}" (* White Knight *)
    | Queen, P1 -> "\u{265B}" (* White Queen *)
    | King, P1 -> "\u{265A}" (* White King *)
    | Pawn, P2 -> "\u{2659}" (* Black Pawn *)
    | Rook, P2 -> "\u{2656}" (* Black Rook *)
    | Bishop, P2 -> "\u{2657}" (* Black Bishop *)
    | Knight, P2 -> "\u{2658}" (* Black Knight *)
    | Queen, P2 -> "\u{2655}" (* Black Queen *)
    | King, P2 -> "\u{2654}" (* Black King *)

  let string_of_square pos =
    if
      ((fst pos + 1) mod 2 = 0 && (snd pos + 1) mod 2 = 0)
      || ((fst pos + 1) mod 2 = 1 && (snd pos + 1) mod 2 = 1)
    then "\u{25A0}"
    else "\u{25A1}"
end

(*All possible chess board positions.*)
let possible_positions = List.init 64 (fun x -> ((x / 8) + 1, (x mod 8) + 1))

(*keeps track of which players made their first move*)
let players_moved = Array.make 2 false

(*Build a new game*)
let new_game () =
  (*convert position to corresponding piece*)

  (*starting game_pieces for each player*)
  let starting_game_pieces =
    List.filter
      (fun x -> snd x <= 2 || (7 <= snd x && 8 >= snd x))
      possible_positions
  in
  let start_state =
    List.map Helpers.starting_position_to_piece starting_game_pieces
  in
  {
    is_terminal = false;
    state = start_state;
    check = (false, false);
    player_turn = P1;
  }

let player_to_int = function
  | P1 -> 0
  | _ -> 1

(*NOTE: This function is very long and messy, i will refactor later, feel free
  to collapse :p *)
let get_legal_moves game game_piece =
  (*variable declarations*)
  let current_player = game_piece.player in
  let opponent = if current_player = P1 then P2 else P1 in
  let current_pos = game_piece.pos in
  let current_player_pieces =
    List.filter (fun x -> x.player = current_player) game.state
  in
  let opponent_pieces = List.filter (fun x -> x.player = opponent) game.state in
  let positions_occupied_by_current_player =
    List.map (fun x -> x.pos) current_player_pieces
  in
  let positions_occupied_by_opponent =
    List.map (fun x -> x.pos) opponent_pieces
  in
  let is_in_check = function
    | P1 -> fst game.check
    | _ -> snd game.check
  in
  match game_piece.piece with
  (*Dominicks fix for double moves below. these aditions work by preserving the
    origional functionality that josh added in, with one big diffrence: a new
    check is being made for pawns which are located in the intal starting
    position, allowing them to call the "move functiomn" twice if anf only if
    htey arein the correct position, lmiting movment to only the forward
    direction.pos

    theoretical issue: This is my second version of the program. origional has
    been discarded, but initlay this allowed the pawns to move twice in any
    direction.pos I have not been able to extensively test if this is the case,
    nor have I been able to confirm that a pawn who moves to the starting
    position of another pawn in the file cannot also jump forward.

    Advice for optimization: lets split all of these moves up into seperate
    helper functions. this will be my next job personaly, as this program if far
    to long for my linking rn. *)
  | Pawn ->
      if is_in_check current_player then []
      else
        (* Step 1: Funciton is idenitcla to how josh intlay imput. way to go *)
        let single_step_moves =
          if current_player = P1 then
            List.filter
              (fun pos ->
                snd pos = snd current_pos + 1
                && fst pos = fst current_pos
                && not
                     (Helpers.is_occupied_by_player pos game current_player
                     || Helpers.is_occupied_by_player pos game opponent))
              possible_positions
          else
            List.filter
              (fun pos ->
                snd pos = snd current_pos - 1
                && fst pos = fst current_pos
                && not
                     (Helpers.is_occupied_by_player pos game current_player
                     || Helpers.is_occupied_by_player pos game opponent))
              possible_positions
        in

        (* Step 2: Determine the two-step forward move, applicable iff the pawn
           is in its initial position. *)
        let double_step_moves =
          let one_step_pos =
            if current_player = P1 then (fst current_pos, snd current_pos + 1)
            else (fst current_pos, snd current_pos - 1)
          in
          (* Determine the intermediate and final positions for the two-step
             move. *)
          let two_step_pos =
            if current_player = P1 then (fst current_pos, snd current_pos + 2)
            else (fst current_pos, snd current_pos - 2)
          in
          if
            (*square must be free and only works on the player's first move *)
            (not
               (Helpers.is_occupied_by_player one_step_pos game P1
               || Helpers.is_occupied_by_player one_step_pos game P2))
            && (not
                  (Helpers.is_occupied_by_player two_step_pos game P1
                  || Helpers.is_occupied_by_player two_step_pos game P2))
            && not players_moved.(player_to_int current_player)
          then [ two_step_pos ]
          else []
        in
        let capture_moves =
          List.filter
            (fun pos ->
              if current_player = P1 then
                let old1, old2 =
                  ( (fst current_pos - 1, snd current_pos + 1),
                    (fst current_pos + 1, snd current_pos + 1) )
                in
                pos = old1 || pos = old2
              else
                let old1, old2 =
                  ( (fst current_pos - 1, snd current_pos - 1),
                    (fst current_pos + 1, snd current_pos - 1) )
                in
                pos = old1 || pos = old2)
            positions_occupied_by_opponent
        in

        (* Combine the potential one-step and two-step moves into the list of
           legal moves. *)
        single_step_moves @ double_step_moves @ capture_moves
  | Rook ->
      if is_in_check current_player then []
      else
        (*get all valid moves from the left*)
        let rec get_moves_left pos acc =
          if not (Helpers.is_valid_position pos) then acc
          else if
            (*free square?*)
            not (Helpers.is_occupied_by_player pos game current_player)
          then get_moves_left (fst pos - 1, snd pos) (pos :: acc)
          else if (*capturable?*)
                  Helpers.is_occupied_by_player pos game opponent
          then pos :: acc
          else []
        in
        (*get all valid moves from the right*)
        let rec get_moves_right pos acc =
          if not (Helpers.is_valid_position pos) then acc
          else if
            (*free square?*)
            not (Helpers.is_occupied_by_player pos game current_player)
          then get_moves_right (fst pos + 1, snd pos) (pos :: acc)
          else if (*capturable?*)
                  Helpers.is_occupied_by_player pos game opponent
          then pos :: acc
          else []
        in

        (*get all possible moves when moving up*)
        let rec get_moves_up pos acc =
          if not (Helpers.is_valid_position pos) then acc
          else if
            (*free square?*)
            not (Helpers.is_occupied_by_player pos game current_player)
          then get_moves_up (fst pos, snd pos + 1) (pos :: acc)
          else if (*capturable?*)
                  Helpers.is_occupied_by_player pos game opponent
          then pos :: acc
          else []
        in
        (*get all possible moves when moving down*)
        let rec get_moves_down pos acc =
          if not (Helpers.is_valid_position pos) then acc
          else if
            (*free square?*)
            not (Helpers.is_occupied_by_player pos game current_player)
          then get_moves_down (fst pos, snd pos - 1) (pos :: acc)
          else if (*capturable?*)
                  Helpers.is_occupied_by_player pos game opponent
          then List.rev (pos :: acc)
          else []
        in
        (*combine legal moves from all directions*)
        get_moves_left (fst current_pos - 1, snd current_pos) []
        @ get_moves_right (fst current_pos + 1, snd current_pos) []
        @ get_moves_down (fst current_pos, snd current_pos - 1) []
        @ get_moves_up (fst current_pos, snd current_pos + 1) []
  | Knight ->
      if is_in_check current_player then []
      else
        (*free gamepieces*)
        let free_game_pieces =
          List.filter
            (fun x -> not (List.mem x positions_occupied_by_current_player))
            possible_positions
        in
        (*l shaped moves*)
        List.filter (Helpers.is_l_shaped current_pos) free_game_pieces
  | Bishop ->
      if is_in_check current_player then []
      else
        (*northwest diags*)
        let rec get_diagonal_moves_nw pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then acc
          else get_diagonal_moves_nw (fst pos - 1, snd pos + 1) (pos :: acc)
        in
        (*northeast diags*)
        let rec get_diagonal_moves_ne pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then acc
          else get_diagonal_moves_ne (fst pos + 1, snd pos + 1) (pos :: acc)
        in
        (*southwest diags*)
        let rec get_diagonal_moves_sw pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then acc
          else get_diagonal_moves_sw (fst pos - 1, snd pos - 1) (pos :: acc)
        in
        (*southeast diags*)
        let rec get_diagonal_moves_se pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then acc
          else get_diagonal_moves_se (fst pos + 1, snd pos - 1) (pos :: acc)
        in
        (*combine moves from all directions*)
        get_diagonal_moves_nw (fst current_pos - 1, snd current_pos + 1) []
        @ get_diagonal_moves_ne (fst current_pos + 1, snd current_pos + 1) []
        @ get_diagonal_moves_sw (fst current_pos - 1, snd current_pos - 1) []
        @ get_diagonal_moves_se (fst current_pos + 1, snd current_pos - 1) []
  | Queen ->
      if is_in_check current_player then []
      else
        (*TODO: this code contains duplicate lines from the Bishop branch. I
          will refactor later, but for the purpose of getting something that
          works I will keep this as is for now*)
        let rec get_moves_north pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then List.rev acc
          else get_moves_north (fst pos, snd pos + 1) (pos :: acc)
        in
        let rec get_moves_south pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then List.rev acc
          else get_moves_south (fst pos, snd pos - 1) (pos :: acc)
        in
        let rec get_moves_west pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then List.rev acc
          else get_moves_west (fst pos - 1, snd pos) (pos :: acc)
        in
        let rec get_moves_east pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then List.rev acc
          else get_moves_east (fst pos + 1, snd pos) (pos :: acc)
        in
        (*northwest diags*)
        let rec get_diagonal_moves_nw pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then List.rev acc
          else get_diagonal_moves_nw (fst pos - 1, snd pos + 1) (pos :: acc)
        in
        (*northeast diags*)
        let rec get_diagonal_moves_ne pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then acc
          else get_diagonal_moves_ne (fst pos + 1, snd pos + 1) (pos :: acc)
        in
        (*southwest diags*)
        let rec get_diagonal_moves_sw pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then List.rev acc
          else get_diagonal_moves_sw (fst pos - 1, snd pos - 1) (pos :: acc)
        in
        (*southeast diags*)
        let rec get_diagonal_moves_se pos acc =
          if
            (not (Helpers.is_valid_position pos))
            || List.mem pos positions_occupied_by_current_player
          then List.rev acc
          else get_diagonal_moves_se (fst pos + 1, snd pos - 1) (pos :: acc)
        in
        (*combine moves from all directions*)
        get_moves_north current_pos []
        @ get_moves_south current_pos []
        @ get_moves_west current_pos []
        @ get_moves_east current_pos []
        @ get_diagonal_moves_nw current_pos []
        @ get_diagonal_moves_ne current_pos []
        @ get_diagonal_moves_sw current_pos []
        @ get_diagonal_moves_se current_pos []
  | King ->
      let one_square_away =
        [
          (*one square to west*)
          (fst current_pos - 1, snd current_pos);
          (*one square to northwest*)
          (fst current_pos - 1, snd current_pos + 1);
          (*one square to north*)
          (fst current_pos, snd current_pos + 1);
          (*one square to northeast*)
          (fst current_pos + 1, snd current_pos + 1);
          (*one square to east*)
          (fst current_pos + 1, snd current_pos);
          (*one square to southeast*)
          (fst current_pos + 1, snd current_pos - 1);
          (*one square to south*)
          (fst current_pos, snd current_pos - 1);
          (*one square to southwest*)
          (fst current_pos - 1, snd current_pos - 1);
        ]
      in
      (*get all the valid squares*)
      List.filter
        (fun pos ->
          Helpers.is_valid_position pos
          && not (List.mem pos positions_occupied_by_current_player))
        one_square_away

let get_all_occupied_positions game = List.map (fun gp -> gp.pos) game.state

let get_all_free_positions game =
  List.filter
    (fun pos -> not (List.mem pos (get_all_occupied_positions game)))
    possible_positions

let game_piece_with_pos game pos =
  let piece_list = List.filter (fun gp -> pos = gp.pos) game.state in
  if List.is_empty piece_list then failwith "Invalid Position!"
  else List.hd piece_list

let rec show_end_game_message winner =
  let message1, message2 =
    match winner with
    | P1 ->
        ( ANSITerminal.sprintf [ ANSITerminal.green ] "P1 Wins",
          ANSITerminal.sprintf [ ANSITerminal.white ] "P1 Wins" )
    | _ ->
        ( ANSITerminal.sprintf [ ANSITerminal.blue ] "P2 Wins",
          ANSITerminal.sprintf [ ANSITerminal.white ] "P2 Wins" )
  in
  print_endline message1;
  Unix.sleepf 0.1;
  print_endline "\x1Bc";
  print_endline message2;
  Unix.sleepf 0.1;
  print_endline "\x1Bc";
  show_end_game_message winner

let print_chess_game game =
  (* print_endline "\x1Bc"; *)
  if game.is_terminal then
    let winner =
      if List.exists (fun gp -> gp.player = P1 && gp.piece = King) game.state
      then P1
      else P2
    in
    show_end_game_message winner
  else
    (*get occupied and free positions*)
    let positions_occupied_by_current_player =
      get_all_occupied_positions game
    in

    (*helper function to build row by row*)
    let rec build_rows_to_print acc i =
      if i > 8 then acc
      else
        let row = List.filter (fun pos -> snd pos = i) possible_positions in
        build_rows_to_print (row :: acc) (i + 1)
    in
    let rows_to_print = build_rows_to_print [] 1 in
    (*get string list*)
    let string_list =
      List.map
        (List.map (fun pos ->
             (*determine if new line must be printed*)
             let delimiter = if fst pos = 8 then "\n" else "" in
             (*match pieces to proper ascii representation*)
             (if List.mem pos positions_occupied_by_current_player then
                Helpers.string_of_game_piece (game_piece_with_pos game pos)
              else Helpers.string_of_square pos)
             ^ delimiter))
        rows_to_print
    in
    print_endline (" |" ^ String.concat " |" (List.concat string_list))

let update_game game new_state =
  print_endline "updating state";
  let opponent =
    match game.player_turn with
    | P1 -> P2
    | _ -> P1
  in
  let p1_pieces, p2_pieces =
    ( List.find_all (fun gp -> gp.player = P1) new_state,
      List.find_all (fun gp -> gp.player = P2) new_state )
  in
  let p1_king = List.find_opt (fun gp -> gp.piece = King) p1_pieces in
  let p2_king = List.find_opt (fun gp -> gp.piece = King) p2_pieces in
  match (p1_king, p2_king) with
  (*end game if king is captured*)
  | _, None | None, _ ->
      {
        game with
        state = new_state;
        player_turn = opponent;
        is_terminal = true;
      }
  (*otherwise continue*)
  | Some p1_king, Some p2_king ->
      let temp_game =
        {
          game with
          state = new_state;
          player_turn = opponent;
          is_terminal = false;
        }
      in
      if game.player_turn = P1 then players_moved.(0) <- true
      else players_moved.(1) <- true;
      (*update to reflect changes in check conditions*)
      let p1_all_possible_moves =
        List.concat
          (List.map (fun gp -> get_legal_moves temp_game gp) p1_pieces)
      in
      let p2_all_possible_moves =
        List.concat
          (List.map (fun gp -> get_legal_moves temp_game gp) p2_pieces)
      in
      let p1_check =
        if List.mem p1_king.pos p2_all_possible_moves then true else false
      in
      let p2_check =
        if List.mem p2_king.pos p1_all_possible_moves then true else false
      in
      { temp_game with check = (p1_check, p2_check) }

let take_action game action =
  print_endline "taking action";
  let game_piece, move = (action.game_piece, action.new_pos) in
  if not (List.mem game_piece game.state) then
    raise
      (GamePieceNotFoundError
         (sprintf
            "{piece: %s; player: %s; pos: %s} not in the current game state"
            (Helpers.string_of_chess_piece game_piece.piece)
            (Helpers.string_of_player game_piece.player)
            (Helpers.string_of_pos game_piece.pos)))
  else
    let legal_moves = get_legal_moves game game_piece in
    if not (List.mem move legal_moves) then
      raise
        (IllegalMoveError
           (sprintf "%s's %s at %s cannot be moved to %s."
              (Helpers.string_of_player game_piece.player)
              (Helpers.string_of_chess_piece game_piece.piece)
              (Helpers.string_of_pos game_piece.pos)
              (Helpers.string_of_pos move)))
    else
      (*player of the next state*)
      let opponent =
        match game.player_turn with
        | P1 -> P2
        | _ -> P1
      in
      (*update position of game piece and end turn.*)
      let new_piece =
        (*promote pawn to queen if makes it across the board*)
        if
          game_piece.player = P1 && game_piece.piece = Pawn
          && snd game_piece.pos = 8
        then { game_piece with piece = Queen; pos = action.new_pos }
        else if
          game_piece.player = P2 && game_piece.piece = Pawn
          && snd game_piece.pos = 1
        then { game_piece with piece = Queen; pos = action.new_pos }
        else { game_piece with pos = action.new_pos }
      in
      let new_state =
        if Helpers.is_occupied_by_player move game opponent then
          (*capture opponent square*)
          List.map
            (fun gp -> if gp = game_piece then new_piece else gp)
            (Helpers.capture action game)
        else
          List.map
            (fun gp -> if gp = game_piece then new_piece else gp)
            game.state
      in
      update_game game new_state
