
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
type vec3f = float * float * float
type vec4f = float * float * float * float
type vec4i = int * int * int * int

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

val copy' : ('a,'b) iplImage -> ('a,'b) iplImage ->
  ([`Channel_1],[`U8]) iplImage option -> unit

val copy : ('a,'b) iplImage ->
  ([`Channel_1],[`U8]) iplImage option -> ('a,'b) iplImage

val set : ('a,'b) iplImage -> cvScalar ->
  ([`Channel_1],[`U8]) iplImage option -> unit

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

val convert_color' :
  src:('c_src,'b) iplImage ->
  dst:('c_dst,'b) iplImage ->
  ('c_src,'c_dst) conversion ->
  unit

val convert_color :
  ('c_src,'b) iplImage ->
  ('c_src,'c_dst) conversion ->
  ('c_dst,'b) iplImage

val split_3' : ([`Channel_3],'b) iplImage -> ([`Channel_1],'b) iplImage ->
  ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage -> unit

val split : ([`Channel_3],'b) iplImage ->
  ([`Channel_1],'b) iplImage * ([`Channel_1],'b) iplImage * ([`Channel_1],'b) iplImage

val merge_3' : ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage ->
  ([`Channel_1],'b) iplImage -> ([`Channel_3],'b) iplImage -> unit

val merge : ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage ->
  ([`Channel_1],'b) iplImage -> ([`Channel_3],'b) iplImage

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

val threshold' : ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage ->
  float -> float -> threshold_type -> unit
(** [threshold' src dst threshold maxValue thresholdType] *)

val threshold : ([`Channel_1],'b) iplImage -> float -> float -> threshold_type ->
  ([`Channel_1],'b) iplImage
(** [threshold src threshold maxValue thresholdType] *)

val adaptive_threshold' : ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage ->
  float -> adaptive_method -> threshold_type -> int -> float -> unit
(** [adaptive_threshold' src dst maxValue adaptiveMethod thresholdType blockSize param1] *)

val adaptive_threshold : ([`Channel_1],'b) iplImage ->
  float -> adaptive_method -> threshold_type -> int -> float -> ([`Channel_1],'b) iplImage
(** [adaptive_threshold src maxValue adaptiveMethod thresholdType blockSize param1] *)

val between : ([`Channel_1],[`U8]) iplImage -> int -> int -> ([`Channel_1],[`U8]) iplImage
(** [between src min max] is 255 for each point p when min <= p < max *)

(** edge detection *)

val canny' : ([`Channel_1],[`U8]) iplImage -> ([`Channel_1],[`U8]) iplImage ->
  float -> float -> int -> unit
val canny : ([ `Channel_1 ], [ `U8 ]) iplImage ->
  ?apertureSize:int ->
  float -> float -> ([ `Channel_1 ], [ `U8 ]) iplImage
(** [canny src threshold1 threshold2] *)

(** image processing *)

val blur' : ('a,'b) iplImage -> ('a,'b) iplImage -> cvSize -> unit
val blur : ?size:cvSize -> ('a, 'b) iplImage -> ('a, 'b) iplImage

val gaussian_blur' : ('a,'b) iplImage -> ('a,'b) iplImage -> cvSize -> float ->
  float -> unit
val gaussian_blur : ?size:cvSize -> ('a,'b) iplImage -> float ->
  float -> ('a,'b) iplImage

val median_blur' : ('a,'b) iplImage -> ('a,'b) iplImage -> int -> unit
val median_blur : ?size:int -> ('a, 'b) iplImage -> ('a, 'b) iplImage

val equalize_hist' : ([`Channel_1],[`U8]) iplImage ->
  ([`Channel_1],[`U8]) iplImage -> unit
val equalize_hist : ([`Channel_1],[`U8]) iplImage ->
  ([`Channel_1],[`U8]) iplImage

type morph_shape =
  | MORPH_RECT
  | MORPH_ELLIPSE
  | MORPH_CROSS

val dilate' : ('a, [ `U8 ]) iplImage ->
  ('a, [ `U8 ]) iplImage ->
  morph_shape -> cvSize -> int -> unit

val erode' : ('a, [ `U8 ]) iplImage ->
  ('a, [ `U8 ]) iplImage ->
  morph_shape -> cvSize -> int -> unit

val dilate : ?iter:int ->
  ?shape:morph_shape ->
  ('a, [ `U8 ]) iplImage ->
  cvSize -> ('a, [ `U8 ]) iplImage

val erode : ?iter:int ->
  ?shape:morph_shape ->
  ('a, [ `U8 ]) iplImage ->
  cvSize -> ('a, [ `U8 ]) iplImage

(** drawing *)

type color = int * int * int (** red, green, blue *)

val red : color
val green : color
val blue : color
val yellow : color
val magenta : color
val black : color
val white : color

val circle : ('a, [ `U8 ]) iplImage ->
  ?thickness:int -> ?color:color ->
  cvPoint -> int -> unit

type ellipse = {
  ellipse_center : float * float;
  ellipse_size : float * float;
  ellipse_angle : float;
}

val ellipse : ('a, [ `U8 ]) iplImage ->
  ?thickness:int ->
  ?color:color ->
  ?angle:float ->
  ?start_angle:float -> ?end_angle:float -> cvPoint -> cvSize -> unit

val ellipse' : ('a, [ `U8 ]) iplImage ->
  ?thickness:int ->
  ?color:color ->
  ?start_angle:float -> ?end_angle:float ->
  ellipse -> unit

val rectangle : ('a, [ `U8 ]) iplImage ->
  ?thickness:int -> ?color:color ->
  cvPoint -> cvPoint -> unit

val line : ('a, [ `U8 ]) iplImage ->
  ?thickness:int -> ?color:color ->
  cvPoint -> cvPoint -> unit

val draw_points :
  ?color:color ->
  ?size:int ->
  ('a, [ `U8 ]) iplImage ->
  cvPoint2D32f array -> unit


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

type contour

type contour_info = {
  h_prev : contour option;
  h_next : contour option;
  v_prev : contour option;
  v_next : contour option;
}

type contours = Contour of (contour * contours list)

val contours : contour option -> contours list

val contour_info : contour -> contour_info
val find_contours : ?mode:contour_retrieval_mode -> ?meth:contour_approximation_method ->
  ([`Channel_1],[`U8]) iplImage -> contour option

val draw_contours :
  ?in_color:color ->
  ?out_color:color ->
  ?thickness:int ->
  ?offset:cvPoint ->
  ?level:int ->
  ('a, [ `U8 ]) iplImage -> contour -> unit

val fit_ellipse : contour -> ellipse option

val good_features_to_track :
  ([ `Channel_1 ], 'a) iplImage ->
  ?mask:([ `Channel_1 ], [ `U8 ]) iplImage ->
  ?blockSize:int ->
  ?useHarris:bool ->
  ?k:float -> int -> float -> float -> cvPoint2D32f array
(** [good_features_to_track image ?mask ?(blockSize=3) ?(useHarris=false) ?(k=0.04)
   maxCorners qualityLevel minDistance] *)

 val houghCircles :
   ('a, [ `U8 ]) iplImage ->
   ?param1:float ->
   ?param2:float ->
   ?minRadius:int -> ?maxRadius:int -> float -> float -> vec3f array
(** [houghCircles img dp minDist] *)

 val houghLinesP :
   ('a, [ `U8 ]) iplImage ->
   ?minLineLength:float ->
   ?maxLineGap:float -> float -> float -> int -> vec4i array
(** [houghLinesP img rho theta threshold] *)

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

val load_calibration_file : string -> ( float array array * float array )
val load_homography_file : string -> float array array

type remap

val init_undistort_map : cvSize -> ( float array array * float array ) -> remap
val remap' : remap -> ('a, 'b) iplImage -> ('a, 'b) iplImage -> unit
val remap : remap -> ('a, 'b) iplImage -> ('a, 'b) iplImage

val find_homography : obj_pos: (float*float) array -> img_pos: (float*float) array ->
  float array array

val invert : float array array -> float * float array array

val mult : float array array -> float array -> float array
