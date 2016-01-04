
type ('z, 'o, 'l, 'b) genGenSpec =
  | Zero of 'z
  | One of 'o * 'b
  | List of 'l * 'b

module type ArgSpec = sig

  type 'arg t
  type acc

  val doCommand: 'arg -> 'arg t -> acc -> unit

end
