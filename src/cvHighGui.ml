open CvCore

(* image loading from file *)

type iscolor_ (* int *)
type iscolor =
  | CV_LOAD_IMAGE_UNCHANGED
  | CV_LOAD_IMAGE_GRAYSCALE
  | CV_LOAD_IMAGE_COLOR
  | CV_LOAD_IMAGE_ANYDEPTH
  | CV_LOAD_IMAGE_ANYCOLOR

type 'a load_color = iscolor

let load_color = CV_LOAD_IMAGE_COLOR
let load_grayscale = CV_LOAD_IMAGE_GRAYSCALE

external get_iscolor : iscolor -> iscolor_ = "ocaml_get_iscolor"
external cvLoadImage : string -> iscolor_ -> ('a,'b) iplImage = "ocaml_cvLoadImage"

let load_image file iscolor = cvLoadImage file (get_iscolor iscolor)

(* capture *)

type cvCapture
external capture_from_cam : int -> cvCapture = "ocaml_cvCaptureFromCAM"
external query_frame : cvCapture -> ('a,'b) iplImage = "ocaml_cvQueryFrame"

(* interface *)

type window_option =
  | CV_WINDOW_DEFAULT
  | CV_WINDOW_AUTOSIZE

external cvNamedWindow : string -> int -> unit = "ocaml_cvNamedWindow"
let named_window ?(option=CV_WINDOW_DEFAULT) name =
  let opt =
    match option with
      | CV_WINDOW_DEFAULT -> 0
      | CV_WINDOW_AUTOSIZE -> 1 in
  cvNamedWindow name opt

external wait_key : int -> char = "ocaml_cvWaitKey"
external show_image : string -> ('a,'b) iplImage -> unit = "ocaml_cvShowImage"

type int_var

external create_int_var : int -> int_var = "ocaml_create_int_var"
external get_int_var : int_var -> int = "ocaml_get_int_var"
external set_int_var : int_var -> int -> unit = "ocaml_set_int_var"
external cvCreateTrackbar : string -> string -> int_var -> int -> int = "ocaml_cvCreateTrackbar"

let create_trackbar ~name ~window var max = ignore (cvCreateTrackbar name window var max)
