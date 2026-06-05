
let test_make_and_get () =
  let m = Matrix.make 2 3 5.0 in
  Alcotest.(check int) "Rows are correct" 2 (Matrix.rows m);
  Alcotest.(check int) "Cols are correct" 3 (Matrix.cols m);
  Alcotest.(check (float 1e-5)) "Value is initialized" 5.0 (Matrix.get m 1 2)

let test_diag () =
  let m = Matrix.diag 3 1.0 in
  Alcotest.(check (float 1e-5)) "Diagonal is 1.0" 1.0 (Matrix.get m 0 0);
  Alcotest.(check (float 1e-5)) "Diagonal is 1.0" 1.0 (Matrix.get m 2 2);
  Alcotest.(check (float 1e-5)) "Off-diagonal is 0.0" 0.0 (Matrix.get m 0 1)

let test_scalar_mult () =
  let m = Matrix.make 2 2 2.0 in
  let res = Matrix.scalar_mult m 3.0 in
  Alcotest.(check (float 1e-5)) "2.0 * 3.0 = 6.0" 6.0 (Matrix.get res 1 1)

let test_transpose () =
  let m = Matrix.make 2 3 1.0 in
  let t = Matrix.transpose m in
  Alcotest.(check int) "Rows become 3" 3 (Matrix.rows t);
  Alcotest.(check int) "Cols become 2" 2 (Matrix.cols t)

let test_addition () =
  let a = Matrix.make 2 2 3.0 in
  let b = Matrix.make 2 2 4.5 in
  let c = Matrix.addition a b in
  Alcotest.(check (float 1e-5)) "3.0 + 4.5 = 7.5" 7.5 (Matrix.get c 1 1)

let test_addition_mismatch () =
  let a = Matrix.make 2 2 1.0 in
  let b = Matrix.make 3 3 1.0 in
  Alcotest.check_raises "Fails on bad dims"
    (Failure "Incompatible dimensions for addition")
    (fun () -> ignore (Matrix.addition a b))

let test_multiply () =
  let a = Matrix.make 2 3 2.0 in
  let b = Matrix.make 3 2 3.0 in
  let c = Matrix.multiply a b in
  Alcotest.(check int) "Result rows" 2 (Matrix.rows c);
  Alcotest.(check int) "Result cols" 2 (Matrix.cols c);
  (* (2.0*3.0) + (2.0*3.0) + (2.0*3.0) = 18.0 *)
  Alcotest.(check (float 1e-5)) "Calculates dot product" 18.0 (Matrix.get c 0 0)

let test_invert () =
  let m = Matrix.diag 2 4.0 in (* Diagonal matrix with 4.0 *)
  let inv = Matrix.invert m in
  Alcotest.(check (float 1e-5)) "Inverse of 4.0 is 0.25" 0.25 (Matrix.get inv 0 0);
  Alcotest.(check (float 1e-5)) "Inverse of 4.0 is 0.25" 0.25 (Matrix.get inv 1 1);
  Alcotest.(check (float 1e-5)) "Off-diagonal remains 0.0" 0.0 (Matrix.get inv 0 1)

let test_invert_errors () =
  let rect = Matrix.make 2 3 1.0 in
  Alcotest.check_raises "Fails on non-square matrix"
    (Failure "Matrix must be square to invert")
    (fun () -> ignore (Matrix.invert rect));

  let singular = Matrix.make 2 2 0.0 in
  Alcotest.check_raises "Fails on singular matrix"
    (Failure "Matrix is singular or nearly singular")
    (fun () -> ignore (Matrix.invert singular))

(* Combine everything into the test runner *)
let () =
  let open Alcotest in
  run "Matrix Module" [
    "Creation & Access", [
      test_case "make and get" `Quick test_make_and_get;
      test_case "diag" `Quick test_diag;
    ];
    "Operations", [
      test_case "scalar_mult" `Quick test_scalar_mult;
      test_case "transpose" `Quick test_transpose;
      test_case "addition" `Quick test_addition;
      test_case "addition errors" `Quick test_addition_mismatch;
      test_case "multiply" `Quick test_multiply;
    ];
    "Linear Algebra", [
      test_case "invert" `Quick test_invert;
      test_case "invert errors" `Quick test_invert_errors;
    ]
  ]
