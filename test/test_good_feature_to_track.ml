open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "out"

let () =
  while true do
    let i = get_image () in
    let gray = convert_color i bgr2gray in
    let out = convert_color gray gray2bgr in
    let harris = clone_image out in
    let a = good_features_to_track
      gray
      50
      0.01
      5. in
    let a_harris = good_features_to_track
      ~useHarris:true
      gray
      50
      0.01
      5. in
    draw_points out a;
    draw_points harris a_harris;
    show_image "src" i;
    show_image "out" out;
    show_image "harris" harris;
    ignore(wait_key 10);
  done
