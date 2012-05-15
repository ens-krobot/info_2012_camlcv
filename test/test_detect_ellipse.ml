open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "s"
let _ = named_window ~option:CV_WINDOW_AUTOSIZE "v"

let smin = create_trackbar ~default:0 ~name:"min" ~window:"s" 256
let smax = create_trackbar ~default:256 ~name:"max" ~window:"s" 256

let vmin = create_trackbar ~default:0 ~name:"min" ~window:"v" 256
let vmax = create_trackbar ~default:256 ~name:"max" ~window:"v" 256

let rec show () =
  let src = get_image () in
  let src = gaussian_blur src ~size:(3,3) 2. 2. in
  let hsv = convert_color src bgr2hsv in
  let h,s,v = split hsv in

  let v' = between v (* (equalize_hist v) *) (get_var vmin) (get_var vmax) in
  let s' = between s (* (equalize_hist s) *) (get_var smin) (get_var smax) in

  let filtered = copy s' (Some v') in
  let filtered = erode filtered (5,5) in
  let filtered = dilate filtered (5,5) in

  let filtered_src = copy src (Some filtered) in

(*  let final_src = copy src (Some final) in *)

  let contours = contours (find_contours filtered) in

  List.iter (fun (Contour (c,l)) -> draw_contours ~level:0 src c) contours;

  let ellipses = List.map (fun (Contour (c,l)) -> fit_ellipse c) contours in
  List.iter (function | None -> ()
    | Some e -> ellipse' src e) ellipses;

  show_image "s" s';
  show_image "v" v';
  show_image "src" src;
  show_image "filtered" filtered_src;
(*  show_image "final" final_src; *)
  match wait_key 10 with
    | Some 'q' ->
      Printf.printf "s: %i %i\nv: %i %i\n%!" (get_var smin) (get_var smax) (get_var vmin) (get_var vmax)
    | _ -> show ()

let _ = show ()
