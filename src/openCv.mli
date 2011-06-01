
type ('chan_num,'depth,'kind) iplImage
type cvCapture
type int_var

type channel_num_ = [ `One | `Two | `Three | `Foor ]
type 'a channel_num

val one : [ `One ] channel_num
val two : [ `Two ] channel_num
val three : [ `Three ] channel_num
val four : [ `Four ] channel_num

type 'depth image_depth

val depth_u1 : [`U1] image_depth
val depth_u8 : [`U8] image_depth
val depth_u16 : [`U16] image_depth
val depth_f32 : [`F32] image_depth

type ('kind_src,'kind_dst) conversion

val bgr2hsv : ([`BGR],[`HSV]) conversion
val hsv2bgr : ([`HSV],[`BGR]) conversion

type 'a load_color

val load_color : [`BGR] load_color
val load_grayscale : [`GREY] load_color

val create_image : x:int -> y:int -> 'depth image_depth -> 'chan_num channel_num ->
  ('chan_num,'depth,'kind) iplImage
val clone_image : ('a,'b,'c) iplImage -> ('a,'b,'c) iplImage

val convert_color :
  src:('a,'b,'kind_src) iplImage ->
  dst:('a,'b,'kind_dst) iplImage -> 
  ('kind_src,'kind_dst) conversion ->
  unit

val wait_key : int -> char

val capture_from_cam : int -> cvCapture
val query_frame : cvCapture -> ([`Three],[`U8],[`BGR]) iplImage
val show_image : string -> ('a,'b,'c) iplImage -> unit

val load_image : string -> 'kind load_color -> ([`Three],[`U8],'kind) iplImage
val zero_image : ('a,'b,'c) iplImage -> unit

val image_size : ('a,'b,'c) iplImage -> int * int
val image_channels : ('a,'b,'c) iplImage -> int
val image_depth : ('a,'b,'c) iplImage -> int
val image_data_order : ('a,'b,'c) iplImage -> int


type window_option =
  | CV_WINDOW_DEFAULT
  | CV_WINDOW_AUTOSIZE

val named_window : ?option:window_option -> string -> unit

type image_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

external image_data : ('a,[`U8],'b) iplImage -> image_data = "ocaml_image_to_bigarray"
