open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "threshold"
let t = create_trackbar ~default:256 ~name:"t" ~window:"threshold" 256

let () =
  while true do
    let i = get_image () in
    let gray = convert_color i bgr2gray in
    let it = threshold gray (float (get_var t) -. 1.) 0. CV_THRESH_TOZERO_INV in
    let iat = adaptive_threshold gray 255. CV_ADAPTIVE_THRESH_GAUSSIAN_C CV_THRESH_BINARY
      3 5. in
    let iatm = adaptive_threshold gray 255. CV_ADAPTIVE_THRESH_MEAN_C CV_THRESH_BINARY
      3 5. in
    show_image "src" gray;
    show_image "threshold" it;
    show_image "adaptive threshold gaussian" iat;
    show_image "adaptive threshold mean" iatm;
    ignore(wait_key 10);
  done
