open CvCore
open CvHighGui
open Test_base

let () =
  while true do
    let i = get_image () in
    ignore(clone_image i);
    let v' = convert_color i bgr2hsv in
    ignore(create_image ~x:500 ~y:500 depth_u8 channel_3);
    show_image "toto" v';
    ignore(wait_key 10);
  done
