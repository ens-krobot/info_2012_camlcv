
type ('a,'b) iplImage
type cvType_ (* int *)
type color_conversion_ (* int *)

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

type 'a image_depth = 'a depth

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

type ('a,'b) conversion = color_conversion

let bgr2rgb = CV_BGR2RGB
let rgb2bgr = CV_RGB2BGR
let bgr2hsv = CV_BGR2HSV
let hsv2bgr = CV_HSV2BGR
let rgb2hsv = CV_RGB2HSV
let hsv2rgb = CV_HSV2RGB
let bgr2gray = CV_BGR2GRAY
let gray2bgr = CV_GRAY2BGR
let bgr2bgra = CV_BGR2BGRA
let rgb2gray = CV_RGB2GRAY
let gray2rgb = CV_GRAY2RGB
let rgb2rgba = CV_RGB2RGBA

(* zeros are: I don't know *)
let convert_output_channels = function
  | CV_BGR2BGRA -> 4
  | CV_RGB2RGBA -> 4
  | CV_BGRA2BGR -> 3
  | CV_RGBA2RGB -> 3
  | CV_BGR2RGBA -> 4
  | CV_RGB2BGRA -> 4
  | CV_RGBA2BGR -> 3
  | CV_BGRA2RGB -> 3
  | CV_BGR2RGB -> 3
  | CV_RGB2BGR -> 3
  | CV_BGRA2RGBA -> 4
  | CV_RGBA2BGRA -> 4
  | CV_BGR2GRAY -> 1
  | CV_RGB2GRAY -> 1
  | CV_GRAY2BGR -> 3
  | CV_GRAY2RGB -> 3
  | CV_GRAY2BGRA -> 4
  | CV_GRAY2RGBA -> 4
  | CV_BGRA2GRAY -> 1
  | CV_RGBA2GRAY -> 1
  | CV_BGR2BGR565 -> 0
  | CV_RGB2BGR565 -> 0
  | CV_BGR5652BGR -> 3
  | CV_BGR5652RGB -> 3
  | CV_BGRA2BGR565 -> 0
  | CV_RGBA2BGR565 -> 0
  | CV_BGR5652BGRA -> 4
  | CV_BGR5652RGBA -> 4
  | CV_GRAY2BGR565 -> 0
  | CV_BGR5652GRAY -> 1
  | CV_BGR2BGR555 -> 0
  | CV_RGB2BGR555 -> 0
  | CV_BGR5552BGR -> 3
  | CV_BGR5552RGB -> 3
  | CV_BGRA2BGR555 -> 0
  | CV_RGBA2BGR555 -> 0
  | CV_BGR5552BGRA -> 4
  | CV_BGR5552RGBA -> 4
  | CV_GRAY2BGR555 -> 0
  | CV_BGR5552GRAY -> 1
  | CV_BGR2XYZ -> 3
  | CV_RGB2XYZ -> 3
  | CV_XYZ2BGR -> 3
  | CV_XYZ2RGB -> 3
  | CV_BGR2YCrCb -> 0
  | CV_RGB2YCrCb -> 0
  | CV_YCrCb2BGR -> 3
  | CV_YCrCb2RGB -> 3
  | CV_BGR2HSV -> 3
  | CV_RGB2HSV -> 3
  | CV_BGR2Lab -> 3
  | CV_RGB2Lab -> 3
  | CV_BayerBG2BGR -> 3
  | CV_BayerGB2BGR -> 3
  | CV_BayerRG2BGR -> 3
  | CV_BayerGR2BGR -> 3
  | CV_BayerBG2RGB -> 3
  | CV_BayerGB2RGB -> 3
  | CV_BayerRG2RGB -> 3
  | CV_BayerGR2RGB -> 3
  | CV_BGR2Luv -> 3
  | CV_RGB2Luv -> 3
  | CV_BGR2HLS -> 3
  | CV_RGB2HLS -> 3
  | CV_HSV2BGR -> 3
  | CV_HSV2RGB -> 3
  | CV_Lab2BGR -> 3
  | CV_Lab2RGB -> 3
  | CV_Luv2BGR -> 3
  | CV_Luv2RGB -> 3
  | CV_HLS2BGR -> 3
  | CV_HLS2RGB -> 3


external cvCreateImage : (int*int) -> cvType_ -> int -> ('a,'b) iplImage = "ocaml_cvCreateImage"
external cvCvtColor : ('a,'b) iplImage -> ('d,'e) iplImage -> color_conversion_ -> unit = "ocaml_cvCvtColor"
external threshold' : ([`Channel_1],'b) iplImage -> ([`Channel_1],'b) iplImage -> float -> float -> threshold_type -> unit = "ocaml_cvThreshold"
external adaptive_threshold' : ('a,'b) iplImage -> ('d,'e) iplImage -> float -> adaptive_method -> threshold_type -> int -> float -> unit = "ocaml_cvAdaptiveThreshold_bytecode" "ocaml_cvAdaptiveThreshold"

external canny' : ([`Channel_1],[`U8]) iplImage -> ([`Channel_1],[`U8]) iplImage ->
  float -> float -> int -> unit = "ocaml_cvCanny"

external cvSplit : ('a,'b) iplImage -> ([`Channel_1],'b) iplImage option ->
  ([`Channel_1],'b) iplImage option -> ([`Channel_1],'b) iplImage option ->
  ([`Channel_1],'b) iplImage option -> unit = "ocaml_cvSplit"

external cvMerge : ([`Channel_1],'b) iplImage option -> ([`Channel_1],'b) iplImage option ->
  ([`Channel_1],'b) iplImage option -> ([`Channel_1],'b) iplImage option ->
  ('a,'b) iplImage -> unit = "ocaml_cvMerge"

external get_cvType : cvType -> cvType_ = "ocaml_get_cvType"
external cvType_int : int -> cvType_ = "%identity"
external get_color_conversion : color_conversion -> color_conversion_
  = "ocaml_get_color_conversion"
external image_size : ('a,'b) iplImage -> (int*int) = "ocaml_image_size"
external image_channels : ('a,'b) iplImage -> int = "ocaml_image_channels"
external image_depth : ('a,'b) iplImage -> int = "ocaml_image_depth"
external image_data_order : ('a,'b) iplImage -> int = "ocaml_image_data_order"

let create_image_from ?channels ?depth src =
  let size = image_size src in
  let type_ = cvType_int (match depth with
    | None -> image_depth src
    | Some d -> d) in
  let channels = match channels with
    | None -> image_channels src
    | Some c -> c in
  cvCreateImage size type_ channels

let split_3' src c1 c2 c3 =
  cvSplit src (Some c1) (Some c2) (Some c3) None

let split src =
  let size = image_size src in
  let type_ = cvType_int (image_depth src) in
  let c1 = cvCreateImage size type_ 1 in
  let c2 = cvCreateImage size type_ 1 in
  let c3 = cvCreateImage size type_ 1 in
  cvSplit src (Some c1) (Some c2) (Some c3) None;
  c1,c2,c3

let merge_3' c1 c2 c3 dst =
  cvMerge (Some c1) (Some c2) (Some c3) None dst

let merge c1 c2 c3 =
  let dst = create_image_from ~channels:3 c1 in
  cvMerge (Some c1) (Some c2) (Some c3) None dst;
  dst

let threshold src threshold maxValue thresholdType =
  let dst = create_image_from src in
  threshold' src dst threshold maxValue thresholdType;
  dst

let adaptive_threshold src maxValue adaptiveMethod thresholdType blockSize param1 =
  let dst = create_image_from src in
  adaptive_threshold' src dst maxValue adaptiveMethod thresholdType blockSize param1;
  dst

let canny src ?(apertureSize=3) th1 th2 =
  let dst = create_image_from src in
  canny' src dst th1 th2 apertureSize;
  dst

type image_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

external image_data : ('a,[`U8]) iplImage -> image_data = "ocaml_image_to_bigarray"

external zero_image : ('a,'b) iplImage -> unit = "ocaml_cvZero"

external clone_image : ('a,'b) iplImage -> ('a,'b) iplImage = "ocaml_cvCloneImage"

let create_image ~x ~y _type i = cvCreateImage (x,y) (get_cvType _type) i

let convert_color' ~src ~dst color_conversion =
  cvCvtColor src dst (get_color_conversion color_conversion)

let convert_color src color_conversion =
  let channels = convert_output_channels color_conversion in
  let size = image_size src in
  let type_ = cvType_int (image_depth src) in
  let dst = cvCreateImage size type_ channels in
  cvCvtColor src dst (get_color_conversion color_conversion);
  dst

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

type vec3f_vect

external create_vec3f_vect : unit -> vec3f_vect = "ocaml_create_Vec3f_vector"
external vec3f_vect_size : vec3f_vect -> int = "ocaml_vector_size_Vec3f"
external vec3f_vect_add : vec3f_vect -> vec3f -> unit = "ocaml_vector_add_Vec3f"
external vec3f_vect_get : vec3f_vect -> int -> vec3f = "ocaml_vector_get_Vec3f"

external cvCircle : ('a,[`U8]) iplImage -> cvPoint -> int -> cvScalar -> int -> unit = "ocaml_cvCircle"
external cvEllipse : ('a,[`U8]) iplImage -> cvPoint -> cvSize ->
  float -> float -> float -> cvScalar -> int -> unit =
    "ocaml_cvEllipse_bytecode" "ocaml_cvEllipse"
external cvRectangle : ('a,[`U8]) iplImage -> cvPoint -> cvPoint -> cvScalar -> int -> unit = "ocaml_cvRectangle"
external cvLine : ('a,[`U8]) iplImage -> cvPoint -> cvPoint -> cvScalar -> int -> unit = "ocaml_cvLine"

type color = int * int * int

let red = 255, 0, 0
let green = 0, 255, 0
let blue = 0, 0, 255
let yellow = 255, 255, 0
let magenta = 255, 0, 255
let black = 0, 0, 0
let white = 255, 255, 255

let scalar_color (r,g,b) = (float b, float g, float r, 0.)

let circle dst ?(thickness=1) ?(color=blue) center radius =
  cvCircle dst center radius (scalar_color color) thickness

let ellipse dst ?(thickness=1) ?(color=blue) ?(angle=0.) ?(start_angle=0.) ?(end_angle=360.)
    center size =
  cvEllipse dst center size angle start_angle end_angle (scalar_color color) thickness

type ellipse = {
  ellipse_center : float * float;
  ellipse_size : float * float;
  ellipse_angle : float;
}

let ellipse' dst ?thickness ?color ?start_angle ?end_angle
    {
      ellipse_center = x,y;
      ellipse_size = w,h;
      ellipse_angle = angle;
    } =
  ellipse dst ?thickness ?color ?start_angle ?end_angle
    (int_of_float x, int_of_float y)
    (int_of_float w, int_of_float h)

let rectangle dst ?(thickness=1) ?(color=blue) p1 p2 =
  cvRectangle dst p1 p2 (scalar_color color) thickness

let line dst ?(thickness=1) ?(color=blue) p1 p2 =
  cvLine dst p1 p2 (scalar_color color) thickness

let draw_points ?(color=red) ?(size=2) i a =
  Array.iter (fun (x,y) -> circle i ~color (int_of_float x,int_of_float y) size) a

(* memory handling *)
type cvMemStorage

external create_CvMemStorage : unit -> cvMemStorage = "ocaml_create_CvMemStorage"
external free_CvMemStorage : cvMemStorage -> unit = "ocaml_free_CvMemStorage"

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

type cvSeq

external cvFindContours : ([`Channel_1],[`U8]) iplImage -> cvMemStorage ->
  contour_retrieval_mode -> contour_approximation_method -> cvPoint -> cvSeq =
    "ocaml_cvFindContours"

type cvSeq_info = {
  cv_h_prev : cvSeq option;
  cv_h_next : cvSeq option;
  cv_v_prev : cvSeq option;
  cv_v_next : cvSeq option;
}

external cvSeq_info : cvSeq -> cvSeq_info = "ocaml_CvSeq_info"

type seq = {
  seq_stor : cvMemStorage;
  seq : cvSeq;
}

type seq_info = {
  h_prev : seq option;
  h_next : seq option;
  v_prev : seq option;
  v_next : seq option;
}

let add_stor s = function
  | None -> None
  | Some v -> Some { seq_stor = s; seq = v }

let seq_info seq =
  let info = cvSeq_info seq.seq in
  { h_prev = add_stor seq.seq_stor info.cv_h_prev;
    h_next = add_stor seq.seq_stor info.cv_h_next;
    v_prev = add_stor seq.seq_stor info.cv_v_prev;
    v_next = add_stor seq.seq_stor info.cv_v_next; }

let find_contours ?(mode=CV_RETR_LIST) ?(meth=CV_CHAIN_APPROX_SIMPLE) image =
  let stor = create_CvMemStorage () in
  let cvSeq = cvFindContours image stor mode meth (0,0) in
  { seq_stor = stor;
    seq = cvSeq }

external cvDrawContours : ('a,[`U8]) iplImage -> cvSeq -> cvScalar -> cvScalar ->
  int -> int -> cvPoint -> unit = "ocaml_cvDrawContours_bytecode" "ocaml_cvDrawContours"

let draw_contours image seq in_color out_color level thickness offset =
  cvDrawContours image seq.seq in_color out_color level thickness offset

external cvFitEllipse2 : cvSeq -> (float * float * float * float * float) option = "ocaml_cvFitEllipse2"

let fit_ellipse seq =
  match cvFitEllipse2 seq.seq with
    | None -> None
    | Some (x,y,width,height,angle) ->
      Some { ellipse_center = x,y;
             ellipse_size = width,height;
             ellipse_angle = angle }

external cvGoodFeaturesToTrack :
  ([`Channel_1],'b) iplImage ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  float -> float ->
  ([`Channel_1],[`U8]) iplImage option ->
  int ->
  bool ->
  float ->
  int
  = "ocaml_cvGoodFeaturesToTrack_bytecode" "ocaml_cvGoodFeaturesToTrack"

let good_features_to_track image ?mask ?(blockSize=3) ?(useHarris=false) ?(k=0.04)
    maxCorners qualityLevel minDistance =
  let ba = Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout maxCorners 2 in
  let count = cvGoodFeaturesToTrack image ba
    qualityLevel minDistance
    mask blockSize useHarris k in
  Array.init count (fun i -> ba.{i,0}, ba.{i,1})

external houghCircles' :
  ('a,[`U8]) iplImage ->
  vec3f_vect ->
  float -> float -> float -> float ->
  int -> int -> unit
    = "ocaml_HoughCircles_bytecode" "ocaml_HoughCircles"

let houghCircles img ?(param1=100.) ?(param2=100.) ?(minRadius=0) ?(maxRadius=0) dp minDist =
  let vec = create_vec3f_vect () in
  houghCircles' img vec dp minDist param1 param2 minRadius maxRadius;
  Array.init (vec3f_vect_size vec) (fun i -> vec3f_vect_get vec i)

type calib_cb =
  | CV_CALIB_CB_ADAPTIVE_THRESH
  | CV_CALIB_CB_NORMALIZE_IMAGE
  | CV_CALIB_CB_FILTER_QUADS
  | CV_CALIB_CB_FAST_CHECK

type chessboard_corners = cvSize * bool * int *
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array3.t

external cvFindChessboardCorners : ('a,[`U8]) iplImage -> cvSize -> calib_cb list ->
  bool * int * (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array3.t
  = "ocaml_cvFindChessboardCorners"

let find_chessboard_corners ?(flags=[CV_CALIB_CB_ADAPTIVE_THRESH]) image size =
  let (found,count,ba) = cvFindChessboardCorners image size flags in
  (size,found,count,ba)

let found_chessboard_corners (_,found,_,_) = found

let array_of_chessboard_corners (size,found,count,ba) =
  if found
  then
    Some (Array.init (Bigarray.Array3.dim1 ba)
            (fun i -> Array.init (Bigarray.Array3.dim2 ba)
              (fun j -> ba.{i,j,0}, ba.{i,j,1})))
  else
    None

external cvDrawChessboardCorners : ('a,[`U8]) iplImage -> cvSize ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array3.t ->
    int -> bool -> unit
      = "ocaml_cvDrawChessboardCorners"

let draw_chessboard_corners i (size,found,count,ba) =
  cvDrawChessboardCorners i size ba count found

external cvFindCornerSubPix : ('a,[`U8]) iplImage ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array3.t ->
    int -> cvSize -> cvSize -> cvTermCriteria -> unit
      = "ocaml_cvFindCornerSubPix_bytecode" "ocaml_cvFindCornerSubPix"

let default_criteria =
  { termcrit_iter = true;
    termcrit_epsilon = true;
    max_iter = 30;
    epsilon = 0.1; }

let find_corner_subpix ?(criteria=default_criteria) ?(winSize=11,11) ?(zeroZone=(-1,-1))
    i (size,found,count,ba) =
  if found
  then
    begin
      cvFindCornerSubPix i ba count (11,11) (-1,-1) criteria;
    end

(** camera calibration *)

type ('channel) cvMat_float32 =
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array3.t

type ('channel) cvMat_float64 =
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array3.t

type ('channel) cvMat_int =
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array3.t
(* there is no 64 bit type in CvMat... so stick to int32 *)

external cvCalibrateCamera2 : [`Channel_3] cvMat_float32 -> [`Channel_2] cvMat_float32 ->
  [`Channel_1] cvMat_int -> cvSize -> [`Channel_1] cvMat_float32 ->
  [`Channel_1] cvMat_float32 -> float
  = "ocaml_cvCalibrateCamera2_bytecode" "ocaml_cvCalibrateCamera2"

type calibration = cvSize * float * cvSize option * ((float*float) array array*(float*float*float) array array) list

let init_calibration cb_size square_size : calibration = (cb_size, square_size, None, [])

let corner_positions (x,y) square_size =
  Array.init y
    (fun i -> Array.init x
      (fun j -> (float j) *. square_size, (float i) *. square_size, 0.))

let add_calibration_image ((cb_size, square_size, i_size, state):calibration) image : calibration =
  let i_size = match i_size with
    | None -> image_size image
    | Some s ->
      if s <> image_size image
      then raise (Invalid_argument "not same size");
      s in
  let r = find_chessboard_corners image cb_size in
  let a = corner_positions cb_size square_size in
  find_corner_subpix image r;
  let state =
    match array_of_chessboard_corners r with
      | None -> state
      | Some v -> (v,a)::state in
  (cb_size, square_size, Some i_size, state)

let map_a2 a = Array.map (fun e -> Array.map (fun (x,y) -> [|x;y|]) e) a
let map_a3 a = Array.map (fun e -> Array.map (fun (x,y,z) -> [|x;y;z|]) e) a

let calibrate (cal:calibration) =
  let (cb_x,cb_y), square_size, i_size, state = cal in
  let i_size = match i_size with
    | None -> raise (Invalid_argument "no calibration data")
    | Some v -> v in
  let points = cb_x * cb_y in
  let obj_pos = Array.concat (List.map (fun (_,a) -> map_a3 a) state) in
  let obj_pos = Bigarray.Array3.of_array Bigarray.float32 Bigarray.c_layout obj_pos in
  let obj_pos = Bigarray.reshape_3
    (Bigarray.genarray_of_array3 obj_pos)
    ((List.length state) * points) 1 3 in
  let img_pos = Array.concat (List.map (fun (a,_) -> map_a2 a) state) in
  let img_pos = Bigarray.Array3.of_array Bigarray.float32 Bigarray.c_layout img_pos in
  let img_pos = Bigarray.reshape_3
    (Bigarray.genarray_of_array3 img_pos)
    ((List.length state) * points) 1 2 in
  let point_count = Array.map Int32.of_int (Array.create (List.length state) points) in
  let point_count = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout point_count in
  let point_count = Bigarray.reshape_3
    (Bigarray.genarray_of_array1 point_count)
    (List.length state) 1 1 in
  let out_mat = Bigarray.Array3.create Bigarray.float32 Bigarray.c_layout 3 3 1 in
  Bigarray.Array3.fill out_mat 0.;
  out_mat.{0,0,0} <- 1.;
  out_mat.{1,1,0} <- 1.;
  let out_vect = Bigarray.Array3.create Bigarray.float32 Bigarray.c_layout 5 1 1 in
  let r = cvCalibrateCamera2
    obj_pos img_pos point_count i_size out_mat out_vect in
  let r_mat = Array.init 3 (fun i -> Array.init 3 (fun j -> out_mat.{i,j,0})) in
  let r_vect = Array.init 5 (fun i -> out_vect.{i,0,0}) in
  r, (r_mat, r_vect)

external cvInitUndistortMap :
  [`Channel_1] cvMat_float32 ->
  [`Channel_1] cvMat_float32 ->
  ([`Channel_1],[`F32]) iplImage ->
  ([`Channel_1],[`F32]) iplImage ->
  unit
  = "ocaml_cvInitUndistortMap"

type remap = ([ `Channel_1 ], [ `F32 ]) iplImage * ([ `Channel_1 ], [ `F32 ]) iplImage

let init_undistort_map (x,y) (mat,v) =
  let mat = Bigarray.Array3.of_array Bigarray.float32 Bigarray.c_layout
    ((Array.map (Array.map (fun v -> [|v|]))) mat) in
  let v = Bigarray.Array3.of_array Bigarray.float32 Bigarray.c_layout
    (Array.map (fun v -> [|[|v|]|]) v) in
  let i1 = create_image ~x ~y depth_f32 channel_1 in
  let i2 = create_image ~x ~y depth_f32 channel_1 in
  cvInitUndistortMap mat v i1 i2;
  i1, i2

external cvRemap :
  ('a,'b) iplImage -> ('a,'b) iplImage ->
  ([`Channel_1],[`F32]) iplImage ->
  ([`Channel_1],[`F32]) iplImage ->
  unit
  = "ocaml_cvRemap"

let remap' ((mapx,mapy):remap) src dst =
  cvRemap src dst mapx mapy

let remap ((mapx,mapy):remap) src =
  let size = image_size src in
  let type_ = cvType_int (image_depth src) in
  let channels = image_channels src in
  let dst = cvCreateImage size type_ channels in
  cvRemap src dst mapx mapy;
  dst

external cvFindHomography :
  [`Channel_2] cvMat_float64 ->
  [`Channel_2] cvMat_float64 ->
  [`Channel_1] cvMat_float64 ->
  unit
    = "ocaml_cvFindHomography"
