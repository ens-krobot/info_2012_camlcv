open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "canny"
let canny_t1 = create_trackbar ~name:"t1" ~window:"canny" 255
let canny_t2 = create_trackbar ~name:"t2" ~window:"canny" 255

let () =
  let rec show () =
    let i = get_image () in
    let gray = convert_color i bgr2gray in
    show_image "src" i;
    show_image "gray" gray;
    show_image "canny" (canny gray (float (get_var canny_t1)) (float (get_var canny_t2)));
    show_image "erode" (erode i (9,9));
    show_image "equalized" (equalize_hist gray);
    match wait_key 10 with
      | Some 'q' -> ()
      | _ -> show () in
  show ()
