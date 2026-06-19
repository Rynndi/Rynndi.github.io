open Js_of_ocaml

let parse_english_js (sentence : Js.js_string Js.t) : Js.js_string Js.t =
  let sentence = Js.to_string sentence in
  let result = Parser_core.parse_english_for_web sentence in
  Js.string result

let () =
  Js.export "parseEnglish" parse_english_js