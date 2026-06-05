
(* Full ols linear reg *)
type model

(* Fits using OLS *)
val fit: Matrix.t -> Matrix.t -> model

(* Generate predictions for feature matrix*)
val predict: model -> Matrix.t -> Matrix.t

(* Weights*)
val weights : model -> Matrix.t
