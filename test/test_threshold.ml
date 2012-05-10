open CvCore
open CvHighGui
open Test_base

let () =
  while true do
    let i = get_image () in
    let gray = convert_color i bgr2gray in
    let it = threshold gray 155. 255. CV_THRESH_TOZERO in
    let iat = adaptive_threshold gray 255. CV_ADAPTIVE_THRESH_GAUSSIAN_C CV_THRESH_BINARY
      3 5. in
    let iatm = adaptive_threshold gray 255. CV_ADAPTIVE_THRESH_MEAN_C CV_THRESH_BINARY
      3 5. in
    show_image "src" i;
    show_image "threshold" it;
    show_image "adaptive threshold gaussian" iat;
    show_image "adaptive threshold mean" iatm;
    ignore(wait_key 10);
  done
