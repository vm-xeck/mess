open Str

let rec find_text_buf buf first_line (q: string): int option =
    if first_line >= (List.length buf) then
        None
    else begin
        let q_regexp = Printf.sprintf ".*%s.*" q |>  regexp_case_fold in
        if string_match q_regexp (List.nth buf  first_line) 0 then
            Some first_line
        else
            find_text_buf buf (first_line+1) q
        end

let rec find_text_back_buf buf first_line (q: string): int option =
    if first_line < 0 then
        None
    else begin
        let q_regexp = Printf.sprintf ".*%s.*" q |> regexp_case_fold in
        if string_match q_regexp (List.nth buf first_line) 0 then
            Some first_line
        else
            find_text_back_buf buf (first_line-1) q
        end
