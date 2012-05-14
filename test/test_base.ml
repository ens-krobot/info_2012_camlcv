open CvCore
open CvHighGui

let rec get_cam i m =
  if i <= m
  then
    try
      Some (capture_from_cam i)
    with
      | _ -> get_cam (i+1) m
  else None

let get_image =
  match get_cam 0 10 with
    | None ->
      fun () -> load_image "test/chessboard.jpg" load_color
    | Some capture ->
      set_capture_property capture CV_CAP_PROP_FRAME_HEIGHT 480.;
      set_capture_property capture CV_CAP_PROP_FRAME_WIDTH 640.;
      ignore(query_frame capture);
      fun () -> query_frame capture


