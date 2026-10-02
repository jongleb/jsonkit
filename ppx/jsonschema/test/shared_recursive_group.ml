open Jsonkit.Primitives

let ppx_defs_helper_hidden = 41

type helper_hidden = Helper_end | Helper_hidden of helper_hidden_tail

and helper_hidden_tail =
  | Helper_hidden_tail_end
  | Helper_hidden_tail of helper_hidden
[@@deriving jsonschema]

let _ =
  Helper_hidden Helper_hidden_tail_end, Helper_hidden_tail Helper_end

let probe_calls = ref 0
type 'a probe = 'a

let probe_jsonschema schema =
  incr probe_calls;
  schema

type probed = { value : int probe; tail : probed_tail option }
and probed_tail = Probed_tail of probed option [@@deriving jsonschema]

let () =
  let sample = { value = 1; tail = Some (Probed_tail None) } in
  ignore (sample.value, sample.tail);
  if ppx_defs_helper_hidden + 1 <> 42 then
    failwith "generated helper escaped its scope";
  if !probe_calls <> 2 then
    failwith "recursive schema body was evaluated more than once"
