open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "canny"
let canny_t1 = create_trackbar ~default:50 ~name:"t1" ~window:"canny" 255
let canny_t2 = create_trackbar ~default:200 ~name:"t2" ~window:"canny" 255

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "h"
let hmin = create_trackbar ~default:0 ~name:"hmin" ~window:"h" 255
let hmax = create_trackbar ~default:76 ~name:"hmax" ~window:"h" 255

let pi = 4. *. (atan 1.)

let rec show () =
  let src = get_image () in
  let hsv = convert_color src bgr2hsv in
  let h,s,v = split hsv in
  (* let gray = convert_color src bgr2gray in *)
  let gray = v in
  let a = good_features_to_track
    gray
    50
    0.01
    5. in
  let gray = gaussian_blur ~size:(3,3) gray 1. 1. in
  let edges = canny gray (float (get_var canny_t1)) (float (get_var canny_t2)) in

  let h' = between h (get_var hmin) (get_var hmax) in
  let dilate_h = erode h' (3,3) in
  let edges' = copy edges (Some dilate_h) in

  let lines =
    houghLinesP
      ~minLineLength:30.
      ~maxLineGap:20.
      edges' 1. (pi /. 180.) 30 in

  Array.iter (fun (x1,y1,x2,y2) -> line ~color:red src (x1,y1) (x2,y2)) lines;

  draw_points ~color:blue src a;

  show_image "h" h';
  show_image "canny" edges';
  show_image "gray" gray;
  show_image "src" src;

  match wait_key 10 with
    | Some 'q' -> ()
    | _ -> show ()

let _ = show ()
