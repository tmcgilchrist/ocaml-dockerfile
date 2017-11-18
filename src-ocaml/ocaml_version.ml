(* Copyright (c) 2017 Anil Madhavapeddy
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type t = { major: int; minor: int; patch: int option; extra: string option }
let v ?patch ?extra major minor = { major; minor; patch; extra }

let major { major } = major
let minor { minor } = minor
let patch { patch } = patch
let extra { extra } = extra

let to_string ?(sep='+') =
  function
  | {major;minor;patch=None;extra=None} -> Printf.sprintf "%d.%02d" major minor
  | {major;minor;patch=Some patch;extra=None} -> Printf.sprintf "%d.%02d.%d" major minor patch
  | {major;minor;patch=Some patch;extra=Some extra} -> Printf.sprintf "%d.%02d.%d%c%s" major minor patch sep extra
  | {major;minor;patch=None;extra=Some extra} -> Printf.sprintf "%d.%02d%c%s" major minor sep extra

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

let with_variant t extra = { t with extra }

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
let arches = [ `X86_64; `Aarch64 ]

let string_of_arch = function
  | `Aarch64 -> "arm64"
  | `X86_64 -> "amd64"

let arch_of_string = function
  | "arm64" | "aarch64" -> Ok `Aarch64
  | "amd64" | "x86_64" -> Ok `X86_64
  | arch -> Error (`Msg ("Unknown architecture " ^ arch))

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

  let switches t =
    match default_variant t with
    | None -> { t with extra = None } :: (List.map (fun e -> { t with extra = Some e}) (variants t))
    | Some _ -> List.map (fun e -> { t with extra = Some e }) (variants t)

  let default_switch t =
    { t with extra = default_variant t }

  let variant_switches t =
    let default_variant = default_variant t in
    switches t |>
    List.filter (fun {extra} -> extra <> default_variant)
end
