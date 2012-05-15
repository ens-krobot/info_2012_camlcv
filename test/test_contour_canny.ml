open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "canny"
let canny_t1 = create_trackbar ~default:50 ~name:"t1" ~window:"canny" 255
let canny_t2 = create_trackbar ~default:200 ~name:"t2" ~window:"canny" 255

let rec show () =
  let src = get_image () in
  let hsv = convert_color src bgr2hsv in
  let h,s,v = split hsv in
  let gray = convert_color src bgr2gray in
  (*  let gray = s in *)
  let gray = gaussian_blur ~size:(3,3) gray 2. 2. in
  let gray' = convert_color gray gray2bgr in
  let edges = canny gray (float (get_var canny_t1)) (float (get_var canny_t2)) in
  let edges' = clone_image edges in
  let contours = contours (find_contours ~meth:CV_CHAIN_APPROX_TC89_KCOS edges) in

  List.iter (fun (Contour (c,l)) -> draw_contours ~level:0 gray' c) contours;

  let ellipses = List.map (fun (Contour (c,l)) -> fit_ellipse c) contours in
  List.iter (function | None -> ()
    | Some e -> ellipse' src e) ellipses;

  show_image "canny" edges';
  show_image "gray" gray';
  show_image "src" src;

  match wait_key 10 with
    | Some 'q' -> ()
    | _ -> show ()

let _ = show ()
