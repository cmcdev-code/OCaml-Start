
type model = {
  beta: Matrix.t
}

let fit x y =
  (* (X^t X)^{-1}X^T y *)
  let x_t = Matrix.transpose x in
  let gram = Matrix.multiply x_t x in
  let inv_gram = Matrix.invert gram in

  let x_t_y = Matrix.multiply x_t y in

  let beta =  Matrix.multiply inv_gram x_t_y in
  {
    beta
  }

let predict  model x =
  Matrix.multiply x model.beta

let weights model =
  model.beta
