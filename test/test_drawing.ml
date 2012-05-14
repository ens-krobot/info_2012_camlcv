open CvCore
open CvHighGui
open Test_base

let f i =
  let angle = Random.float 800. -. 300. in
  let x = Random.int 10000 in
  let y = Random.int 10000 in
  let w = Random.int 10000 in
  let h = Random.int 10000 in
  Printf.printf "%f (%i,%i) (%i,%i)\n%!" angle x y w h;
  ellipse ~angle i (x,y) (w,h)

let () =
  while true do
    let i = get_image () in
    let gray = convert_color i bgr2gray in
    let i2 = convert_color gray gray2bgr in
    circle ~color:yellow i2 (100,50) 40;
    ellipse ~angle:10. i2 (50,100) (30,60);
    line i2 ~color:green (10,10) (132,15);
    rectangle i2 ~color:red (10,130) (132,15);

(*
    for i = 0 to 100 do
      f i2;
    done;
*)
    (* segfault
    ellipse i2 ~angle:(-.212.314126) (742,5538357) (404,4308342);
    *)

    show_image "some drawings" i2;
    ignore(wait_key 10);
  done
