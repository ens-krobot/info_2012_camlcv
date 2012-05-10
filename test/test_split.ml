open CvCore
open CvHighGui
open Test_base

let () =
  while true do
    let i = get_image () in
    let c1,c2,c3 = split i in
    let i2 = merge c2 c2 c1 in
    show_image "toto" i2;
    ignore(wait_key 10);
  done
