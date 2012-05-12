open CvCore
open CvHighGui
open Test_base

let image = get_image ()

exception Fini

let print_a = Array.iter (Printf.printf " %f")

let () = Printf.printf "go\n%!"

let go () =
  let t = ref 0. in
  let gray_shots = ref [] in
  let c = ref (init_calibration (6,8) 100.) in
  let remap_o = ref None in
  let aux () =
    while true do
      let k = wait_key 100 in
      if k = Some 'q' then raise Fini;
      let image = get_image () in
      let gray = convert_color image bgr2gray in
      let r = find_chessboard_corners gray (6,8) in
      find_corner_subpix gray r;
      if (found_chessboard_corners r) && (Unix.gettimeofday () -. !t > 0.5)
        && (k = Some ' ')
      then
        begin
          Printf.printf "flash\n%!";
          gray_shots := (clone_image gray) :: !gray_shots;
          c := add_calibration_image !c (clone_image gray);
          t := Unix.gettimeofday ();
          let (r,(v1,v2)) = calibrate !c in
          let () =  Array.iter (fun a -> print_a a; Printf.printf "\n") v1;
            Printf.printf "\n";
            print_a v2;
            Printf.printf "\n%!" in
          let remap_v = init_undistort_map (image_size image) (v1,v2) in
          remap_o := Some remap_v;
        end;
      (match !remap_o with
        | None -> ()
        | Some r ->
          let image' = remap r image in
          show_image "remap" image');
      draw_chessboard_corners image r;
      show_image "out" image;
    done in
  try aux (); assert false with
    | Fini ->
      Printf.printf "done\n%!"
    | e ->
      Printf.printf "error\n%!"

let () = go ()
