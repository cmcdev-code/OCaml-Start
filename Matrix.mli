(* Abstract representation of a matrix *)
module type MATRIX = sig

  (* abstract type of matrix*)
  type t

  (* initialize a rows x cols matrix with default value*)
  val make: int -> int -> float -> t


  (* make n x n diagonal with default value*)
  val diag: int -> float -> t


  (* number of rows*)
  val rows: t -> int

  (* number of columns*)
  val columns: t -> int

  (* returns entry in row column*)
  val get: t -> int -> int -> float

  (* transpose *)
  val transpose: t -> t

  (* matrix multiplication *)
  val multiply: t-> t-> t

  (* matrix addition*)
  val addition: t-> t -> t


  (* matrix inverse *)
  val invert: t -> t

end
