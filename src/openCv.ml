
type ('a,'b,'c) iplImage
type cvCapture
type cvSize = int*int
type cvType_ (* int *)
type iscolor_ (* int *)
type color_conversion_ (* int *)
type int_var

type channel_num_ = [ `Channel_1 | `Channel_2 | `Channel_3 | `Channel_4 ]
type 'a channel_num = int

let channel_1 = 1
let channel_2 = 2
let channel_3 = 3
let channel_4 = 4

type cvType =
  | IPL_DEPTH_1U
  | IPL_DEPTH_8U
  | IPL_DEPTH_16U
  | IPL_DEPTH_32F
  | IPL_DEPTH_8S
  | IPL_DEPTH_16S
  | IPL_DEPTH_32S

type depth_ = [ `U1 | `U8 | `U16 | `F32 | `S8 | `S16 | `S32 ]
type 'a depth = cvType

let depth_u1 = IPL_DEPTH_1U
let depth_u8 = IPL_DEPTH_8U
let depth_u16 = IPL_DEPTH_16U
let depth_f32 = IPL_DEPTH_32F

type kind_ = [ `BGR | `RGB | `GRAY | `HSV ]
type 'a kind

type 'a image_depth = 'a depth

type iscolor =
  | CV_LOAD_IMAGE_UNCHANGED
  | CV_LOAD_IMAGE_GRAYSCALE
  | CV_LOAD_IMAGE_COLOR
  | CV_LOAD_IMAGE_ANYDEPTH
  | CV_LOAD_IMAGE_ANYCOLOR

type 'a load_color = iscolor

let load_color = CV_LOAD_IMAGE_COLOR
let load_grayscale = CV_LOAD_IMAGE_GRAYSCALE

type color_conversion =
  | CV_BGR2BGRA
  | CV_RGB2RGBA
  | CV_BGRA2BGR
  | CV_RGBA2RGB
  | CV_BGR2RGBA
  | CV_RGB2BGRA
  | CV_RGBA2BGR
  | CV_BGRA2RGB
  | CV_BGR2RGB
  | CV_RGB2BGR
  | CV_BGRA2RGBA
  | CV_RGBA2BGRA
  | CV_BGR2GRAY
  | CV_RGB2GRAY
  | CV_GRAY2BGR
  | CV_GRAY2RGB
  | CV_GRAY2BGRA
  | CV_GRAY2RGBA
  | CV_BGRA2GRAY
  | CV_RGBA2GRAY
  | CV_BGR2BGR565
  | CV_RGB2BGR565
  | CV_BGR5652BGR
  | CV_BGR5652RGB
  | CV_BGRA2BGR565
  | CV_RGBA2BGR565
  | CV_BGR5652BGRA
  | CV_BGR5652RGBA
  | CV_GRAY2BGR565
  | CV_BGR5652GRAY
  | CV_BGR2BGR555
  | CV_RGB2BGR555
  | CV_BGR5552BGR
  | CV_BGR5552RGB
  | CV_BGRA2BGR555
  | CV_RGBA2BGR555
  | CV_BGR5552BGRA
  | CV_BGR5552RGBA
  | CV_GRAY2BGR555
  | CV_BGR5552GRAY
  | CV_BGR2XYZ
  | CV_RGB2XYZ
  | CV_XYZ2BGR
  | CV_XYZ2RGB
  | CV_BGR2YCrCb
  | CV_RGB2YCrCb
  | CV_YCrCb2BGR
  | CV_YCrCb2RGB
  | CV_BGR2HSV
  | CV_RGB2HSV
  | CV_BGR2Lab
  | CV_RGB2Lab
  | CV_BayerBG2BGR
  | CV_BayerGB2BGR
  | CV_BayerRG2BGR
  | CV_BayerGR2BGR
  | CV_BayerBG2RGB
  | CV_BayerGB2RGB
  | CV_BayerRG2RGB
  | CV_BayerGR2RGB
  | CV_BGR2Luv
  | CV_RGB2Luv
  | CV_BGR2HLS
  | CV_RGB2HLS
  | CV_HSV2BGR
  | CV_HSV2RGB
  | CV_Lab2BGR
  | CV_Lab2RGB
  | CV_Luv2BGR
  | CV_Luv2RGB
  | CV_HLS2BGR
  | CV_HLS2RGB

type window_option =
  | CV_WINDOW_DEFAULT
  | CV_WINDOW_AUTOSIZE

type threshold_type =
  | CV_THRESH_BINARY
  | CV_THRESH_BINARY_INV
  | CV_THRESH_TRUNC
  | CV_THRESH_TOZERO
  | CV_THRESH_TOZERO_INV
  | CV_THRESH_MASK
  | CV_THRESH_OTSU

type ('a,'b) conversion = color_conversion

let bgr2hsv = CV_BGR2HSV
let hsv2bgr = CV_HSV2BGR

external cvCreateImage : (int*int) -> cvType_ -> int -> ('a,'b,'c) iplImage = "ocaml_cvCreateImage"
external cvLoadImage : string -> iscolor_ -> ('a,'b,'c) iplImage = "ocaml_cvLoadImage"
external cvNamedWindow : string -> int -> unit = "ocaml_cvNamedWindow"
external cvCreateTrackbar : string -> string -> int_var -> int -> int = "ocaml_cvCreateTrackbar"
external cvCvtColor : ('a,'b,'c) iplImage -> ('d,'e,'f) iplImage -> color_conversion_ -> unit = "ocaml_cvCvtColor"
external cvThreshold : ('a,'b,'c) iplImage -> ('d,'e,'f) iplImage -> float -> float -> threshold_type -> unit = "ocaml_cvThreshold"

external create_int_var : int -> int_var = "create_int_var"
external get_cvType : cvType -> cvType_ = "ocaml_get_cvType"
external get_iscolor : iscolor -> iscolor_ = "ocaml_get_iscolor"
external get_color_conversion : color_conversion -> color_conversion_
  = "ocaml_get_color_conversion"
external image_size : ('a,'b,'c) iplImage -> (int*int) = "ocaml_image_size"
external image_channels : ('a,'b,'c) iplImage -> int = "ocaml_image_channels"
external image_depth : ('a,'b,'c) iplImage -> int = "ocaml_image_depth"
external image_data_order : ('a,'b,'c) iplImage -> int = "ocaml_image_data_order"

(* type float_3point = *)
(*     { mutable f0 : float; *)
(*       mutable f1 : float; *)
(*       mutable f2 : float; } *)

(* type int_3point = *)
(*     { mutable i0 : int; *)
(*       mutable i1 : int; *)
(*       mutable i2 : int; } *)

(* external get_float_3point : *)
(*   ([`One],[`F32],[`BGR | `RGB | `HSV ]) iplImage -> *)
(*     float_point -> unit *)
(*   = "ocaml_get_float_3point" "noalloc"  *)

(* external set_float_3point : *)
(*   ([`One],[`F32],[`BGR | `RGB | `HSV ]) iplImage -> *)
(*     float_point -> unit *)
(*   = "ocaml_set_float_3point" "noalloc"  *)

(* external unsafe_get_int_3point : *)
(*   ([`One],[`U8],[<`BGR | `RGB | `HSV ]) iplImage -> *)
(*     int -> int_3point -> unit *)
(*   = "ocaml_get_int_3point" "noalloc" *)

(* external unsafe_set_int_3point : *)
(*   ([`One],[`U8],[<`BGR | `RGB | `HSV ]) iplImage -> *)
(*     int -> int_3point -> unit *)
(*   = "ocaml_set_int_3point" "noalloc" *)

type image_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

external image_data : ('a,[`U8],'b) iplImage -> image_data = "ocaml_image_to_bigarray"

external wait_key : int -> char = "ocaml_cvWaitKey"
external capture_from_cam : int -> cvCapture = "ocaml_cvCaptureFromCAM"
external query_frame : cvCapture -> ('a,'b,'c) iplImage = "ocaml_cvQueryFrame"
external show_image : string -> ('a,'b,'c) iplImage -> unit = "ocaml_cvShowImage"
external zero_image : ('a,'b,'c) iplImage -> unit = "ocaml_cvZero"

external clone_image : ('a,'b,'c) iplImage -> ('a,'b,'c) iplImage
  = "ocaml_cvCloneImage"

let create_image ~x ~y _type i = cvCreateImage (x,y) (get_cvType _type) i
let load_image file iscolor = cvLoadImage file (get_iscolor iscolor)
let convert_color ~src ~dst color_conversion =
  cvCvtColor src dst (get_color_conversion color_conversion)
let named_window ?(option=CV_WINDOW_DEFAULT) name =
  let opt =
    match option with
      | CV_WINDOW_DEFAULT -> 0
      | CV_WINDOW_AUTOSIZE -> 1 in
  cvNamedWindow name opt
