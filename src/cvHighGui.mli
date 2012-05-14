open CvCore

(** image loading from file *)

type 'depth load_color

val load_color : [`Channel_3] load_color
val load_grayscale : [`Channel_1] load_color

val load_image : string -> 'depth load_color -> ('depth,[`U8]) iplImage
val save_image : string -> ('a,'b) iplImage -> int

(** camera capture *)

type cvCapture

val capture_from_cam : int -> cvCapture
val query_frame : cvCapture -> ([`Channel_3],[`U8]) iplImage

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

val set_capture_property : cvCapture -> cap_prop -> float -> unit
val get_capture_property : cvCapture -> cap_prop -> float

(** Gui interface *)

type window_option =
  | CV_WINDOW_DEFAULT
  | CV_WINDOW_AUTOSIZE

val named_window : ?option:window_option -> string -> unit
(** [named_window name] create a window named [name] *)

val destroy_window : string -> unit
val destroy_all_windows : unit -> unit

val show_image : string -> ('a,'b) iplImage -> unit
(** [show_image window image] display the image [image] in the window
    [window] *)
val wait_key' : int -> int
val wait_key : int -> char option
(** [wait_key time] wait for [time] milliseconds for an event to
    occur.  If [time < 0] it waits indefinitely. Nothing happens in
    the gui until this function is called. *)

type int_var

val create_int_var : int -> int_var
val get_var : int_var -> int
val set_var : int_var -> int -> unit
val create_trackbar : ?default:int -> name:string -> window:string -> int -> int_var
