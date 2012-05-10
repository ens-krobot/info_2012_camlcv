open CvCore
open CvHighGui
open Test_base

let () =
  while true do
    let i = get_image () in
    let gray = convert_color i bgr2gray in
    let i2 = convert_color gray gray2bgr in
    circle ~color:yellow i2 (100,50) 40;
    ellipse ~angle:10. i2 (50,100) (30,60);
    line i2 ~color:green (10,130) (132,15);
    rectangle i2 ~color:red (10,130) (132,15);
    show_image "some drawings" i2;
    ignore(wait_key 10);
  done
