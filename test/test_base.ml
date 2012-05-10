open CvCore
open CvHighGui

let get_image =
  try
    let capture = capture_from_cam 0 in
    set_capture_property capture CV_CAP_PROP_FRAME_HEIGHT 480.;
    set_capture_property capture CV_CAP_PROP_FRAME_WIDTH 640.;
    ignore(query_frame capture);
    fun () -> query_frame capture
  with
    | _ ->
      fun () -> load_image "test/chessboard.jpg" load_color
