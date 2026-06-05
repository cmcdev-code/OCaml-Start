module Matrix : MATRIX = struct

  type t = float array array

  let make rows cols default =
    Array.make_matrix rows cols default

  let diag n default =
    Array.init n (fun i ->
      Array.init n (fun j->
        if i = j then default else 0.0
      )
    )

  let rows m = Array.length m

  let cols m = if Array.length m = 0 then 0 else Array.length m.(0)

  let get m r c = m.(r).(c)

  let transpose m =
    let r = rows m in
    let c = cols m in

    Array.init c  (fun i ->
      Array.init r (fun j->
        m.(j).(i)
      )
    )

    let multiply a b =
      let r1 = rows a in
      let c1 = cols a in
      let r2 = rows b in
      let c2 = cols b in

      if c1 <> r2 then failwith "Incompatible dimensions for multiplication";

      Array.init r1 (fun i ->
        Array.init c2 (fun j ->
          let sum = ref 0.0 in

          for k = 0 to c1 - 1 do
            sum := !sum +. a.(i).(k) *. b.(k).(j)
          done;
          !sum
        )
      )

    let addition a b =
      let r1 = rows a in
      let c1 = cols a in
      let r2 = rows b in
      let c2 = cols b in

      if c1 <> c2 || r1 <> r2 then failwith "Incompatible dimensions for addition";

      Array.init r1 (fun i->
        Array.init c1 (fun j->
           a.(i).(j) +. b.(i).(j)
        )
      )


    let invert m =
      let n = rows m in
      if n <> cols m then failwith "Matrix must be square to invert";


      let aug = Array.init n (fun i ->
        Array.init (2 * n) (fun j ->
          if j < n then m.(i).(j)
          else if j - n = i then 1.0
          else 0.0
        )
      ) in


      for i = 0 to n - 1 do

        let max_row = ref i in
        for k = i + 1 to n - 1 do
          if abs_float aug.(k).(i) > abs_float aug.(!max_row).(i) then
            max_row := k
        done;

        if !max_row <> i then begin
          let temp = aug.(i) in
          aug.(i) <- aug.(!max_row);
          aug.(!max_row) <- temp
        end;

        let pivot = aug.(i).(i) in
        if abs_float pivot < 1e-20 then
          failwith "Matrix is singular or nearly singular";

        for j = i to (2 * n) - 1 do
          aug.(i).(j) <- aug.(i).(j) /. pivot
        done;


        for k = 0 to n - 1 do
          if k <> i then begin
            let factor = aug.(k).(i) in
            for j = i to (2 * n) - 1 do
              aug.(k).(j) <- aug.(k).(j) -. (factor *. aug.(i).(j))
            done
          end
        done;
      done;

      Array.init n (fun i ->
        Array.init n (fun j ->
          aug.(i).(j + n)
        )
      )

end
