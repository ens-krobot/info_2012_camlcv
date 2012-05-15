open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "canny"
let canny_t1 = create_trackbar ~default:50 ~name:"t1" ~window:"canny" 255
let canny_t2 = create_trackbar ~default:200 ~name:"t2" ~window:"canny" 255

let pi = 4. *. (atan 1.)

let rec show () =
  let src = get_image () in
  let hsv = convert_color src bgr2hsv in
  let h,s,v = split hsv in
  (* let gray = convert_color src bgr2gray in *)
  let gray = v in
  let gray = gaussian_blur ~size:(3,3) gray 2. 2. in
  let edges = canny gray (float (get_var canny_t1)) (float (get_var canny_t2)) in

  let x,y = image_size edges in
  let circles = houghCircles gray
    ~param1:(float (get_var canny_t1))
    ~param2:(float (get_var canny_t2))
    ~minRadius:0
    ~maxRadius:0
    1. (float x /. 8.) in

  Array.iter (fun (x1,y1,r) ->
    circle src
      (int_of_float x1,int_of_float y1)
      (int_of_float r)) circles;

  let lines =
    houghLinesP
      ~minLineLength:50.
      ~maxLineGap:20.
      edges 1. (pi /. 180.) 50 in

  Array.iter (fun (x1,y1,x2,y2) -> line ~color:red src (x1,y1) (x2,y2)) lines;

  show_image "canny" edges;
  show_image "gray" gray;
  show_image "src" src;

  match wait_key 10 with
    | Some 'q' -> ()
    | _ -> show ()

let _ = show ()
