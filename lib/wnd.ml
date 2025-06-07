open Curses


let get_max_y () = getmaxyx (stdscr ()) |> fst |> pred

let drop_last (target: string): string =
    String.sub target 0 ((String.length target)-1)

let prompt msg =
    move (get_max_y ()) 0 |> ignore;
    deleteln () |> ignore;
    Printf.sprintf "%s: " msg |> addstr |> ignore;
    echo () |> ignore;
    let rec inner text =
        match getch() |> keyname with
        | "^M" -> text
        | "^[" -> ""
        | "KEY_BACKSPACE" -> begin
            try
                delch () |> ignore;
                inner (drop_last text)
            with Invalid_argument _ ->
                addstr " " |> ignore;
                inner ""
            end
        | key -> inner (text^key)
    in
    let result = inner "" in
    noecho () |> ignore;
    result

let rec add_wide_str str =
    let head_of_string str = str.[0] in
    let tail_of_string str =
        let length_of_tail = (String.length str) - 1 in
        String.sub str 1 length_of_tail
    in
    match str with
    | "" -> ()
    | text ->
        text |> head_of_string |> int_of_char |> addch |> ignore;
        text |> tail_of_string |> add_wide_str

let show (buf: string list) =
    move 0 0 |> ignore;
    let rec show_line buf offset max_y =
        if offset > (max_y-1) then
            ()
        else begin
            (*match buf with
            | [] -> ()
            | head :: tail -> begin
                add_wide_str (head^"\n") |> ignore;
                show_line tail (offset+1) max_y end*)
            match max_y with
            | 0 -> ()
            | _ ->
                addstr "ĉi estas 日本語ですの。\n" |> ignore;
                show_line buf 0 (max_y - 1);
        end
    in
    show_line buf 0 (get_max_y ())

let show_percentage percentage =
    move (get_max_y ()) 0 |> ignore;
    deleteln () |> ignore;
    percentage |> string_of_int |> addstr |> ignore;
    addstr "%" |> ignore

let get_q (): string =
    prompt "text"

let get_line (): int option =
    let got_input = prompt "line" in
    let line =
        try Some (got_input |> int_of_string)
        with Failure _ -> None
    in
    line
