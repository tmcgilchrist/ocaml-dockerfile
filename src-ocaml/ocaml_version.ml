type t = { major: int; minor: int; patch: int option; extra: string option }

let v ?patch ?extra major minor = { major; minor; patch; extra }

let to_string =
  function
  | {major;minor;patch=None;extra=None} -> Printf.sprintf "%d.%02d" major minor
  | {major;minor;patch=Some patch;extra=None} -> Printf.sprintf "%d.%02d.%d" major minor patch
  | {major;minor;patch=Some patch;extra=Some extra} -> Printf.sprintf "%d.%02d.%d+%s" major minor patch extra
  | {major;minor;patch=None;extra=Some extra} -> Printf.sprintf "%d.%02d+%s" major minor extra

let parse s =
  try Scanf.sscanf s "%d.%d.%d+%s" (fun major minor patch extra -> v ~patch ~extra major minor)
  with End_of_file | Scanf.Scan_failure _ -> begin
      try Scanf.sscanf s "%d.%d+%s" (fun major minor extra -> v ~extra major minor)
      with End_of_file | Scanf.Scan_failure _ -> begin
          try Scanf.sscanf s "%d.%d.%d" (fun major minor patch -> v ~patch major minor)
          with End_of_file | Scanf.Scan_failure _ -> begin
              Scanf.sscanf s "%d.%d" (fun major minor -> v major minor)
            end
        end
    end

let of_string s =
  try parse s with
  | exn ->
      raise (Invalid_argument (Printf.sprintf "Unable to parse OCaml version '%s'" s))

let pp ppf v = Fmt.pf ppf "%s" (to_string v)

let ( ++ ) x fn =
  match x with
  | 0 -> fn ()
  | r -> r

let compare {major; minor; patch; extra} a =
  compare major a.major ++ fun () ->
    compare minor a.minor ++ fun () ->
      compare patch a.patch ++ fun () ->
        compare extra a.extra

let sys_version = of_string Sys.ocaml_version

module Releases = struct
  let v4_00_1 = of_string "4.00.1"
  let v4_01_0 = of_string "4.01.0"
  let v4_02_0 = of_string "4.02.0"
  let v4_02_1 = of_string "4.02.1"
  let v4_02_2 = of_string "4.02.2"
  let v4_02_3 = of_string "4.02.3"
  let v4_03_0 = of_string "4.03.0"
  let v4_04_0 = of_string "4.04.0"
  let v4_04_1 = of_string "4.04.1"
  let v4_04_2 = of_string "4.04.2"
  let v4_05_0 = of_string "4.05.0"
  let v4_06_0 = of_string "4.06.0"

  let all = [
    v4_00_1;
    v4_01_0; v4_02_0; v4_02_1; v4_02_2; v4_02_3;
    v4_03_0; v4_04_0; v4_04_1; v4_04_2; v4_05_0;
    v4_06_0
  ]

  let all_major = [ v4_00_1; v4_01_0; v4_02_3; v4_03_0; v4_04_2; v4_05_0; v4_06_0 ]

  let recent_major = [ v4_03_0; v4_04_2; v4_05_0; v4_06_0 ]

  let dev = [ of_string "4.07.0" ]

  let recent_major_and_dev = List.concat [recent_major;dev]

  let latest_major = v4_06_0
end

type arch = [`X86_64 | `Aarch64 ]
module Since = struct
  let bytes = of_string "4.03.0"
  let arch (a:arch) =
    match a with
    | `Aarch64 -> of_string "4.03.0"
    | `X86_64 -> of_string "3.07.0" (* TODO probably earlier *)
end

module Has = struct
  let bytes v =
    match compare Since.bytes v with
    |(-1) | 0 -> true
    |n -> false

  let arch (a:arch) v =
    match compare (Since.arch a) v with
    |(-1) | 0 -> true
    |n -> false

end

module Opam = struct

  let variants {major; minor; _} =
    match major,minor with
    | 4,7 -> ["trunk";"trunk+afl";"trunk+flambda"]
    | 4,6 -> ["afl";"flambda";"default-unsafe-string";"force-safe-string"]
    | 4,5 -> ["afl";"flambda"]
    | 4,4 -> ["flambda"]
    | 4,3 -> ["flambda"]
    | _ -> []

  let default_variant {major; minor; _} =
    match major,minor with
    | 4,7 -> Some "trunk"
    | 4,6 -> None
    | 4,5 -> None
    | 4,4 -> None
    | 4,3 -> None
    | _ -> None

  let default_switch t =
    to_string { t with extra = default_variant t }

  let variants ov =
    let default_variant = default_variant ov in
    variants ov |>
    List.filter (fun extra -> default_variant <> (Some extra)) |>
    List.map (fun extra -> to_string { ov with extra = Some extra })
end
