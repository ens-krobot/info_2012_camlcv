
type cvScalar = float * float * float * float
type cvPoint = int * int
type cvPoint2D32f = float * float
type cvPoint3D32f = float * float * float
type cvPoint2D64f = float * float
type cvPoint3D64f = float * float * float
type cvSize = int * int
type cvTermCriteria =
    { termcrit_iter : bool;
      termcrit_epsilon : bool;
      max_iter : int;
      epsilon : float; }

type ('chan_num,'depth) iplImage

type channel_num_ = [ `Channel_1 | `Channel_2 | `Channel_3 | `Channel_4 ]
type 'num channel_num

val channel_1 : [ `Channel_1 ] channel_num
val channel_2 : [ `Channel_2 ] channel_num
val channel_3 : [ `Channel_3 ] channel_num
val channel_4 : [ `Channel_4 ] channel_num

type 'depth image_depth

val depth_u1 : [`U1] image_depth
val depth_u8 : [`U8] image_depth
val depth_u16 : [`U16] image_depth
val depth_f32 : [`F32] image_depth

val create_image : x:int -> y:int -> 'depth image_depth -> 'chan_num channel_num ->
  ('chan_num,'depth) iplImage
val clone_image : ('a,'b) iplImage -> ('a,'b) iplImage
val zero_image : ('a,'b) iplImage -> unit

val image_size : ('a,'b) iplImage -> int * int
val image_channels : ('a,'b) iplImage -> int
val image_depth : ('a,'b) iplImage -> int

(** color conversions *)
type ('c_src,'c_dst) conversion

val bgr2rgb : ([`Channel_3],[`Channel_3]) conversion
val rgb2bgr : ([`Channel_3],[`Channel_3]) conversion
val bgr2hsv : ([`Channel_3],[`Channel_3]) conversion
val hsv2bgr : ([`Channel_3],[`Channel_3]) conversion
val rgb2hsv : ([`Channel_3],[`Channel_3]) conversion
val hsv2rgb : ([`Channel_3],[`Channel_3]) conversion
val bgr2gray : ([`Channel_3],[`Channel_1]) conversion
val gray2bgr : ([`Channel_1],[`Channel_3]) conversion
val bgr2bgra : ([`Channel_3],[`Channel_4]) conversion
val rgb2gray : ([`Channel_3],[`Channel_1]) conversion
val gray2rgb : ([`Channel_1],[`Channel_3]) conversion
val rgb2rgba : ([`Channel_3],[`Channel_4]) conversion

val convert_color :
  src:('c_src,'b) iplImage ->
  dst:('c_dst,'b) iplImage ->
  ('c_src,'c_dst) conversion ->
  unit

val split_3 : ([`Channel_3],'b) iplImage -> ([`Channel_1],'b) iplImage ->
  ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage -> unit

val merge_3 : ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage ->
  ([`Channel_1],'b) iplImage -> ([`Channel_3],'b) iplImage -> unit

(** threshold *)

type threshold_type =
  | CV_THRESH_BINARY
  | CV_THRESH_BINARY_INV
  | CV_THRESH_TRUNC
  | CV_THRESH_TOZERO
  | CV_THRESH_TOZERO_INV
  | CV_THRESH_MASK
  | CV_THRESH_OTSU

type adaptive_method =
  | CV_ADAPTIVE_THRESH_MEAN_C
  | CV_ADAPTIVE_THRESH_GAUSSIAN_C

val threshold : ('a,'b) iplImage -> ('d,'e) iplImage -> float -> float -> threshold_type -> unit
(** [threshold src dst threshold maxValue thresholdType] *)
val adaptive_threshold : ('a,'b) iplImage -> ('d,'e) iplImage -> float -> adaptive_method -> threshold_type -> int -> float -> unit
(** [adaptive_threshold src dst maxValue adaptiveMethod thresholdType blockSize param1] *)

(** drawing *)

val cvCircle : ('a,[`U8]) iplImage -> cvPoint -> int -> cvScalar -> int -> unit
val cvEllipse : ('a,[`U8]) iplImage -> cvPoint -> cvSize -> float -> float -> float -> cvScalar -> int -> unit
val cvRectangle : ('a,[`U8]) iplImage -> cvPoint -> cvPoint -> cvScalar -> int -> unit
val cvLine : ('a,[`U8]) iplImage -> cvPoint -> cvPoint -> cvScalar -> int -> unit

(**/**)
val image_data_order : ('a,'b) iplImage -> int
type image_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
external image_data : ('a,[`U8]) iplImage -> image_data = "ocaml_image_to_bigarray"

(* contour finding *)

type contour_retrieval_mode =
  | CV_RETR_EXTERNAL
  | CV_RETR_LIST
  | CV_RETR_CCOMP
  | CV_RETR_TREE

type contour_approximation_method =
  | CV_CHAIN_CODE
  | CV_CHAIN_APPROX_NONE
  | CV_CHAIN_APPROX_SIMPLE
  | CV_CHAIN_APPROX_TC89_L1
  | CV_CHAIN_APPROX_TC89_KCOS
  | CV_LINK_RUNS

type seq

type seq_info = {
  h_prev : seq option;
  h_next : seq option;
  v_prev : seq option;
  v_next : seq option;
}

val seq_info : seq -> seq_info
val find_contours : ?mode:contour_retrieval_mode -> ?meth:contour_approximation_method ->
  ([`Channel_1],[`U8]) iplImage -> seq

val draw_contours : ('a,[`U8]) iplImage -> seq -> cvScalar -> cvScalar -> int -> int -> cvPoint -> unit

type ellipse = {
  ellipse_center : float * float;
  ellipse_size : float * float;
  ellipse_angle : float;
}

val fit_ellipse : seq -> ellipse option

type calib_cb =
  | CV_CALIB_CB_ADAPTIVE_THRESH
  | CV_CALIB_CB_NORMALIZE_IMAGE
  | CV_CALIB_CB_FILTER_QUADS
  | CV_CALIB_CB_FAST_CHECK

type chessboard_corners

val find_chessboard_corners : ?flags:calib_cb list -> ('a,[`U8]) iplImage -> cvSize ->
  chessboard_corners

val found_chessboard_corners : chessboard_corners -> bool

val array_of_chessboard_corners : chessboard_corners -> (float*float) array array option

val draw_chessboard_corners : ([`Channel_3],[`U8]) iplImage ->
  chessboard_corners -> unit

val find_corner_subpix : ?criteria:cvTermCriteria -> ?winSize:cvSize ->
  ?zeroZone:cvSize -> ([`Channel_1],[`U8]) iplImage -> chessboard_corners -> unit

type calibration

val init_calibration : cvSize -> float -> calibration
val add_calibration_image :
  calibration -> ([ `Channel_1 ], [ `U8 ]) iplImage -> calibration
val calibrate : calibration -> float * ( float array array * float array )

val init_undistort_map : cvSize -> ( float array array * float array ) ->
  ([ `Channel_1 ], [ `F32 ]) iplImage * ([ `Channel_1 ], [ `F32 ]) iplImage

external cvRemap :
  ('a,'b) iplImage -> ('a,'b) iplImage ->
  ([`Channel_1],[`F32]) iplImage ->
  ([`Channel_1],[`F32]) iplImage ->
  unit
  = "cvRemap"
