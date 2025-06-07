open Curses
open Mess

let init () =
    initscr () |> ignore;
    noecho () |> ignore;
    raw () |> ignore;
    nonl ();
    start_color () |> ignore;
    keypad (stdscr ()) true |> ignore

let finish () = endwin ()

let get_filename args =
    if Array.length args < 2 then begin
        finish ();
        invalid_arg "No filename given" end
    else args.(1)

let read_file (filename: string): string list =
    let open In_channel in
    let ic =
        try
            open_text filename
        with Sys_error msg ->
            finish ();
            raise (Sys_error msg)
    in
    let contents = input_lines ic in
    close ic;
    contents

let is_possible_position_length_maxy source_length max_y first_line =
    if first_line < 0 then false
    else if first_line > source_length-max_y then false
    else true

let rec list_fromi list i =
    match i with
    | 0 -> list
    | n -> list_fromi (List.tl list) (n-1)

let rec best_line_around_func is_possible_position candidate_line =
    if not (0 |> is_possible_position) then 0
    else if is_possible_position candidate_line then
        candidate_line
    else
        best_line_around_func is_possible_position (candidate_line-1)

let get_percentage_buf buf first_line =
    let cur_pos_float =
        min (List.length buf) (Wnd.get_max_y ())
        |> (+) first_line
        |> float_of_int
    in
    let total_lines_float =
        List.length buf
        |> float_of_int
    in
    cur_pos_float /. total_lines_float
    |> ( *. ) 100.
    |> int_of_float

type find_mode_status =
    | Yes of string
    | No

let () =
    init ();
    let filename = Sys.argv |> get_filename in
    let buf = read_file filename in

    let is_possible_position =
        is_possible_position_length_maxy (List.length buf) (Wnd.get_max_y ())
    in
    let get_percentage =
        get_percentage_buf buf
    in
    let find_text =
        Cmd.find_text_buf buf
    in
    let find_text_back =
        Cmd.find_text_back_buf buf
    in
    let best_line_around =
        best_line_around_func is_possible_position
    in

    let rec main first_line is_find_mode =
        list_fromi buf first_line
        |> Wnd.show;
        get_percentage first_line
        |> Wnd.show_percentage;
        refresh () |> ignore;

        match getch () |> keyname with
        | "^Q" -> ()

        | "^F" -> begin
            let q = Wnd.get_q () in
            let found_line =
                match q |> find_text first_line with
                | Some line -> line
                | None -> first_line
            in
            main (best_line_around found_line) (Yes q)
            end

        | "^N" -> begin
            match is_find_mode with
            | No -> main first_line is_find_mode
            | Yes q -> begin
                let next_line =
                    match q |> find_text (first_line+1) with
                    | Some line -> line
                    | None -> first_line
                in
                main (best_line_around next_line) is_find_mode
                end
            end

        | "^P" -> begin
            match is_find_mode with
            | No -> main first_line is_find_mode
            | Yes q -> begin
                let prev_line =
                    match q |> find_text_back (first_line-1) with
                    | Some line -> line
                    | None -> first_line
                in
                main (best_line_around prev_line) is_find_mode
                end
            end

        | "^L" ->
            let line =
                match Wnd.get_line () with
                | Some line -> line
                | None -> first_line
            in
            main (best_line_around line) is_find_mode

        | "KEY_UP" ->
            if first_line-1 |> is_possible_position then
                main (first_line-1) is_find_mode
            else
                main first_line is_find_mode

        | "KEY_DOWN" ->
            if first_line+1 |> is_possible_position then
                main (first_line+1) is_find_mode
            else
                main first_line is_find_mode

        | "KEY_PPAGE" ->
            main 0 is_find_mode

        | "KEY_NPAGE" ->
            main (List.length buf |> best_line_around) is_find_mode

        | "^[" ->
            main first_line No

        | _ ->
            main first_line is_find_mode
    in
    main 0 No;

    finish ()
