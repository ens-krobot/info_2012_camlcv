open CvCore

(** image loading from file *)

type 'depth load_color

val load_color : [`Channel_3] load_color
val load_grayscale : [`Channel_1] load_color

val load_image : string -> 'depth load_color -> ('depth,[`U8]) iplImage

(** camera capture *)

type cvCapture

val capture_from_cam : int -> cvCapture
val query_frame : cvCapture -> ([`Channel_3],[`U8]) iplImage

(** Gui interface *)

type window_option =
  | CV_WINDOW_DEFAULT
  | CV_WINDOW_AUTOSIZE

val named_window : ?option:window_option -> string -> unit
(** [named_window name] create a window named [name] *)

val show_image : string -> ('a,'b) iplImage -> unit
(** [show_image window image] display the image [image] in the window
    [window] *)
val wait_key : int -> char
(** [wait_key time] wait for [time] milliseconds for an event to
    occur.  If [time < 0] it waits indefinitely. Nothing happens in
    the gui until this function is called. *)

type int_var

val create_int_var : int -> int_var
val get_int_var : int_var -> int
val set_int_var : int_var -> int -> unit
val create_trackbar : name:string -> window:string -> int_var -> int -> unit
