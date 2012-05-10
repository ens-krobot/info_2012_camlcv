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

external destroy_window : string -> unit = "ocaml_cvDestroyWindow"
external destroy_all_windows : unit -> unit = "ocaml_cvDestroyAllWindows"

external wait_key : int -> char = "ocaml_cvWaitKey"
external show_image : string -> ('a,'b) iplImage -> unit = "ocaml_cvShowImage"

type int_var

external create_int_var : int -> int_var = "ocaml_create_int_var"
external get_var : int_var -> int = "ocaml_get_int_var"
external set_var : int_var -> int -> unit = "ocaml_set_int_var"
external cvCreateTrackbar : string -> string -> int_var -> int -> int = "ocaml_cvCreateTrackbar"

let create_trackbar ?default ~name ~window max =
  let default = match default with
    | None -> max / 2
    | Some d -> d in
  let var = create_int_var default in
  ignore (cvCreateTrackbar name window var max);
  var

type cap_prop =
  | CV_CAP_PROP_DC1394_OFF
  | CV_CAP_PROP_DC1394_MODE_MANUAL
  | CV_CAP_PROP_DC1394_MODE_AUTO
  | CV_CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO
  | CV_CAP_PROP_POS_MSEC
  | CV_CAP_PROP_POS_FRAMES
  | CV_CAP_PROP_POS_AVI_RATIO
  | CV_CAP_PROP_FRAME_WIDTH
  | CV_CAP_PROP_FRAME_HEIGHT
  | CV_CAP_PROP_FPS
  | CV_CAP_PROP_FOURCC
  | CV_CAP_PROP_FRAME_COUNT
  | CV_CAP_PROP_FORMAT
  | CV_CAP_PROP_MODE
  | CV_CAP_PROP_BRIGHTNESS
  | CV_CAP_PROP_CONTRAST
  | CV_CAP_PROP_SATURATION
  | CV_CAP_PROP_HUE
  | CV_CAP_PROP_GAIN
  | CV_CAP_PROP_EXPOSURE
  | CV_CAP_PROP_CONVERT_RGB
  | CV_CAP_PROP_WHITE_BALANCE_BLUE_U
  | CV_CAP_PROP_RECTIFICATION
  | CV_CAP_PROP_MONOCROME
  | CV_CAP_PROP_SHARPNESS
  | CV_CAP_PROP_AUTO_EXPOSURE
  | CV_CAP_PROP_GAMMA
  | CV_CAP_PROP_TEMPERATURE
  | CV_CAP_PROP_TRIGGER
  | CV_CAP_PROP_TRIGGER_DELAY
  | CV_CAP_PROP_WHITE_BALANCE_RED_V
  | CV_CAP_PROP_MAX_DC1394
  | CV_CAP_PROP_AUTOGRAB
  | CV_CAP_PROP_SUPPORTED_PREVIEW_SIZES_STRING
  | CV_CAP_PROP_PREVIEW_FORMAT
  | CV_CAP_OPENNI_DEPTH_GENERATOR
  | CV_CAP_OPENNI_IMAGE_GENERATOR
  | CV_CAP_OPENNI_GENERATORS_MASK
  | CV_CAP_PROP_OPENNI_OUTPUT_MODE
  | CV_CAP_PROP_OPENNI_FRAME_MAX_DEPTH
  | CV_CAP_PROP_OPENNI_BASELINE
  | CV_CAP_PROP_OPENNI_FOCAL_LENGTH
  | CV_CAP_PROP_OPENNI_REGISTRATION_ON
  | CV_CAP_PROP_OPENNI_REGISTRATION
  | CV_CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE
  | CV_CAP_OPENNI_DEPTH_GENERATOR_BASELINE
  | CV_CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH
  | CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON
  | CV_CAP_GSTREAMER_QUEUE_LENGTH
  | CV_CAP_PROP_PVAPI_MULTICASTIP
  | CV_CAP_PROP_XI_DOWNSAMPLING
  | CV_CAP_PROP_XI_DATA_FORMAT
  | CV_CAP_PROP_XI_OFFSET_X
  | CV_CAP_PROP_XI_OFFSET_Y
  | CV_CAP_PROP_XI_TRG_SOURCE
  | CV_CAP_PROP_XI_TRG_SOFTWARE
  | CV_CAP_PROP_XI_GPI_SELECTOR
  | CV_CAP_PROP_XI_GPI_MODE
  | CV_CAP_PROP_XI_GPI_LEVEL
  | CV_CAP_PROP_XI_GPO_SELECTOR
  | CV_CAP_PROP_XI_GPO_MODE
  | CV_CAP_PROP_XI_LED_SELECTOR
  | CV_CAP_PROP_XI_LED_MODE
  | CV_CAP_PROP_XI_MANUAL_WB
  | CV_CAP_PROP_XI_AUTO_WB
  | CV_CAP_PROP_XI_AEAG
  | CV_CAP_PROP_XI_EXP_PRIORITY
  | CV_CAP_PROP_XI_AE_MAX_LIMIT
  | CV_CAP_PROP_XI_AG_MAX_LIMIT
  | CV_CAP_PROP_XI_AEAG_LEVEL
  | CV_CAP_PROP_XI_TIMEOUT

external set_capture_property : cvCapture -> cap_prop -> float -> unit =
    "ocaml_cvSetCaptureProperty"

external get_capture_property : cvCapture -> cap_prop -> float =
    "ocaml_cvGetCaptureProperty"
