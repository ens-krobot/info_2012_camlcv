open CvCore
open CvHighGui
open Test_base

let pi = 4. *. (atan 1.)

let x = 500
let y = 500

let img = Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout 500 500
let out = create_image ~x ~y depth_u8 channel_3

let points = [| 12.3, 3.5;
                2.4, 5.2;
                7.6, 26.8;
                10., 10.;
                11., 11.;
                12., 12.;
                13., 13.; |]

let rec show () =
  (* let x,y = image_size img in *)
  (* let points' = Array.map (fun (x,y) -> int_of_float x, int_of_float y) points in *)

  (* draw_points *)
  (*   ~color:white *)
  (*   ~size:1 *)
  (*   img *)
  (*   points; *)

  (* Array.iter (fun (x,y) -> *)
  (*   let i, j = int_of_float x, int_of_float y in *)
  (*   img.{i,j} <- 255) points; *)

  for i = 100 to 300 do
    img.{i,i} <- 255;
  done;

  for i = 100 to 300 do
    img.{i+30,400-i} <- 255;
  done;

  let t1 = Unix.gettimeofday () in
  let lines =
    houghLinesP_mat
      ~minLineLength:2.
      ~maxLineGap:20.
      img 0.1 (pi /. 180.) 50 in
  let t2 = Unix.gettimeofday () in

  Printf.printf "lines: %i time: %f\n%!"
    (Array.length lines) (t2 -. t1);

  Array.iter (fun (x1,y1,x2,y2) ->
    line ~color:red out (x1,y1) (x2,y2)) lines;

  show_image "out" out;

  match wait_key 10 with
    | Some 'q' -> ()
    | _ -> show ()

let _ = show ()
