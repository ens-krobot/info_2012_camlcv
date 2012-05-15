open CvCore
open CvHighGui
open Test_base

let v_prod (x1,y1) (x2,y2) = x1 *. x2 +. y1 *. y2

let i_array a =
  Array.mapi (fun i a ->
    Array.mapi (fun j v -> v,(i,j)) a) a

let get_minv v a =
  List.hd (List.sort (fun (v1,_) (v2,_) ->
    compare (v_prod v v1) (v_prod v v2)) a)

let x_size = 8.
let y_size = 6.

let float_c (x,y) = int_of_float x, int_of_float y

let lower_left (ix,iy) a' =
  let a = List.flatten (Array.to_list (Array.map Array.to_list (i_array a'))) in
  let ix, iy = float ix, float iy in
  let p00 = get_minv (ix,-.iy) a in
  let pxy = get_minv (-.ix,iy) a in
  let p0y = get_minv (ix,iy) a in
  let px0 = get_minv (-.ix,-.iy) a in
  p00, px0, p0y, pxy

let norm v =
  match v with
    | [|a;b;c|] -> [|a/.c;b/.c;1.|]
    | _ -> assert false

let print_a = Array.iter (Printf.printf " %f")

let rec show () =
  let image = get_image () in
  let gray = convert_color image bgr2gray in
  let r = find_chessboard_corners gray (6,8) in
  (match array_of_chessboard_corners r with
    | None -> ()
    | Some a ->
      let (p0,_),(p1,_),(p2,_),(p3,_) = lower_left (image_size image) a in
      draw_chessboard_corners image r;
      circle image ~color:red (float_c p0) 30;
      circle image ~color:green (float_c p1) 30;
      circle image ~color:blue (float_c p2) 30;
      circle image ~color:yellow (float_c p3) 30;
      let homo = find_homography
        ~obj_pos:[|0.,0.;
                   x_size,0.;
                   0.,y_size;
                   x_size,y_size|]
        ~img_pos:[|p0;p1;p2;p3|] in
      let v1 = norm (mult homo [|0.;0.;1.|]) in
      let v2 = norm (mult homo [|3.;3.;1.|]) in
      line image ~color:red ~thickness:5 (int_of_float v1.(0),int_of_float v1.(1))
        (int_of_float v2.(0),int_of_float v2.(1));
      Array.iter (fun a -> print_a a; Printf.printf "\n") homo;
      Printf.printf "\n%!");
  show_image "out" image;
  match wait_key 10 with
    | Some 'q' -> ()
    | _ -> show ()

let _ = show ()
