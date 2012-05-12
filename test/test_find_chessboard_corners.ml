open CvCore
open CvHighGui
open Test_base

let _ = named_window ~option:CV_WINDOW_AUTOSIZE "out"
let _ = named_window ~option:CV_WINDOW_AUTOSIZE "subpix"

let () =
  while true do
    let i = get_image () in
    let i2 = clone_image i in
    let gray = convert_color i bgr2gray in
    let r = find_chessboard_corners gray (8,6) in
    draw_chessboard_corners i r;
    find_corner_subpix gray r;
    draw_chessboard_corners i2 r;
    show_image "out" i;
    show_image "subpix" i2;
    ignore (wait_key 10);
  done
