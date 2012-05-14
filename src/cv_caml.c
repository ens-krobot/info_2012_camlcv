extern "C" {
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
}

#include <assert.h>
#include <stdio.h>

#define CV_NO_BACKWARD_COMPATIBILITY

#define int64 int64_opencv
#define uint64 uint64_opencv
#define schar schar_opencv
#include <cv.h>
#include <highgui.h>
#undef int64
#undef uint64
#undef schar

using namespace cv;

#define ERRWRAP(expr) \
try \
{ \
    expr; \
} \
catch (const cv::Exception &e) \
{ \
    caml_failwith(e.what()); \
}

static value Val_some(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);
  res = caml_alloc(1,0);
  Field(res,0) = v;
  CAMLreturn(res);
}

static value Val_none = Val_unit;

static Vec3f Vec3f_val(value v) {
  double v0 = Double_val(Field(v,0));
  double v1 = Double_val(Field(v,1));
  double v2 = Double_val(Field(v,2));
  return (Vec3f(v0,v1,v2));
}

static value Val_Vec3f(Vec3f v) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_tuple(3);
  for(int i = 0; i < 3; i++) {
    Field(res,i) = caml_copy_double(v[i]);
  }
  CAMLreturn(res);
}

static Vec4f Vec4f_val(value v) {
  double v0 = Double_val(Field(v,0));
  double v1 = Double_val(Field(v,1));
  double v2 = Double_val(Field(v,2));
  double v3 = Double_val(Field(v,3));
  return (Vec4f(v0,v1,v2,v3));
}

static value Val_Vec4f(Vec4f v) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_tuple(4);
  for(int i = 0; i < 4; i++) {
    Field(res,i) = caml_copy_double(v[i]);
  }
  CAMLreturn(res);
}

static Vec4i Vec4i_val(value v) {
  int v0 = Int_val(Field(v,0));
  int v1 = Int_val(Field(v,1));
  int v2 = Int_val(Field(v,2));
  int v3 = Int_val(Field(v,3));
  return (Vec4i(v0,v1,v2,v3));
}

static value Val_Vec4i(Vec4i v) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_tuple(4);
  for(int i = 0; i < 4; i++) {
    Field(res,i) = Val_int(v[i]);
  }
  CAMLreturn(res);
}

/* c++ vector */

template <typename Type>
vector<Type>* Vector_val(value v)
{
  return *(vector<Type>**) Data_custom_val(v);
}

extern "C" void caml_finalize_vector(value vec)
{
  //it is the same finalizer for all types... problems ?
  vector<void*>* del = Vector_val<void*>(vec);
  delete del;
  return;
}

static struct custom_operations vector_operations = {
  (char*) "ocaml_vector",
  caml_finalize_vector,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};
template <typename Type>
value ocaml_create_vector(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);
  res = caml_alloc_custom(&vector_operations, sizeof(vector<Type>*), 1, 10);
  *(vector<Type>**) Data_custom_val(res) = new vector<Type>();
  CAMLreturn(res);
}

template <typename Type>
value ocaml_vector_size(value v)
{
  CAMLparam1(v);
  vector<Type>* vec = Vector_val<Type>(v);
  CAMLreturn(Val_int(vec->size()));
}

template <typename Type>
value ocaml_vector_add(value v, value vx, Type x)
{
  CAMLparam2(v,vx);
  Vector_val<Type>(v)->push_back(x);
  CAMLreturn(Val_unit);
}

template <typename Type>
Type ocaml_vector_get(value v, value x)
{
  vector<Type>* vec = Vector_val<Type>(v);
  Type ret;
  try {
    ret = vec->at(Int_val(x));
  }
  catch (const std::exception &e) {
    caml_failwith(e.what());
  };
  return(ret);
}

extern "C" CAMLprim value ocaml_create_int_vector(value v)
{
  return(ocaml_create_vector<int>(v));
}

extern "C" CAMLprim value ocaml_vector_size_int(value v)
{
  return(ocaml_vector_size<int>(v));
}

extern "C" CAMLprim value ocaml_vector_add_int(value v, value x)
{
  return(ocaml_vector_add<int>(v,x,Int_val(x)));
}

extern "C" CAMLprim value ocaml_vector_get_int(value v, value x)
{
  CAMLparam2(v,x);
  CAMLreturn(Val_int(ocaml_vector_get<int>(v,x)));
}

extern "C" CAMLprim value ocaml_create_Vec3f_vector(value v)
{
  return(ocaml_create_vector<Vec3f>(v));
}

extern "C" CAMLprim value ocaml_vector_size_Vec3f(value v)
{
  return(ocaml_vector_size<Vec3f>(v));
}

extern "C" CAMLprim value ocaml_vector_add_Vec3f(value v, value x)
{
  return(ocaml_vector_add<Vec3f>(v,x,Vec3f_val(x)));
}
extern "C" CAMLprim value ocaml_vector_get_Vec3f(value v, value x)
{
  CAMLparam2(v,x);
  Vec3f r = ocaml_vector_get<Vec3f>(v,x);
  CAMLreturn(Val_Vec3f(r));
}

extern "C" CAMLprim value ocaml_create_Vec4f_vector(value v)
{
  return(ocaml_create_vector<Vec4f>(v));
}

extern "C" CAMLprim value ocaml_vector_size_Vec4f(value v)
{
  return(ocaml_vector_size<Vec4f>(v));
}

extern "C" CAMLprim value ocaml_vector_add_Vec4f(value v, value x)
{
  return(ocaml_vector_add<Vec4f>(v,x,Vec4f_val(x)));
}
extern "C" CAMLprim value ocaml_vector_get_Vec4f(value v, value x)
{
  CAMLparam2(v,x);
  Vec4f r = ocaml_vector_get<Vec4f>(v,x);
  CAMLreturn(Val_Vec4f(r));
}

extern "C" CAMLprim value ocaml_create_Vec4i_vector(value v)
{
  return(ocaml_create_vector<Vec4i>(v));
}

extern "C" CAMLprim value ocaml_vector_size_Vec4i(value v)
{
  return(ocaml_vector_size<Vec4i>(v));
}

extern "C" CAMLprim value ocaml_vector_add_Vec4i(value v, value x)
{
  return(ocaml_vector_add<Vec4i>(v,x,Vec4i_val(x)));
}
extern "C" CAMLprim value ocaml_vector_get_Vec4i(value v, value x)
{
  CAMLparam2(v,x);
  Vec4i r = ocaml_vector_get<Vec4i>(v,x);
  CAMLreturn(Val_Vec4i(r));
}

/* basic caml value conversions */

static CvScalar CvScalar_val(value v) {
  double v0 = Double_val(Field(v,0));
  double v1 = Double_val(Field(v,1));
  double v2 = Double_val(Field(v,2));
  double v3 = Double_val(Field(v,3));
  return (cvScalar(v0,v1,v2,v3));
}

static CvPoint CvPoint_val(value v) {
  int v0 = Int_val(Field(v,0));
  int v1 = Int_val(Field(v,1));
  return (cvPoint(v0,v1));
}

/* unused
static CvPoint2D32f CvPoint2D32f_val(value v) {
  double v0 = Double_val(Field(v,0));
  double v1 = Double_val(Field(v,1));
  return (cvPoint2D32f(v0,v1));
}

static CvPoint3D32f CvPoint3D32f_val(value v) {
  double v0 = Double_val(Field(v,0));
  double v1 = Double_val(Field(v,1));
  double v2 = Double_val(Field(v,2));
  return (cvPoint3D32f(v0,v1,v2));
}

static CvPoint2D64f CvPoint2D64f_val(value v) {
  double v0 = Double_val(Field(v,0));
  double v1 = Double_val(Field(v,1));
  return (cvPoint2D64f(v0,v1));
}

static CvPoint3D64f CvPoint3D64f_val(value v) {
  double v0 = Double_val(Field(v,0));
  double v1 = Double_val(Field(v,1));
  double v2 = Double_val(Field(v,2));
  return (cvPoint3D64f(v0,v1,v2));
}
*/

static CvSize CvSize_val(value v) {
  int v0 = Int_val(Field(v,0));
  int v1 = Int_val(Field(v,1));
  return (cvSize(v0,v1));
}

static CvTermCriteria CvTermCriteria_val(value v) {
  int tc_iter = Int_val(Field(v,0));
  int tc_eps = Int_val(Field(v,1));
  int type = 0;
  if (tc_iter) type |= CV_TERMCRIT_ITER;
  if (tc_eps) type |= CV_TERMCRIT_EPS;
  int max_iter = Int_val(Field(v,2));
  double epsilon = Double_val(Field(v,3));
  return (cvTermCriteria(type,max_iter,epsilon));
}

static CvMat CvMat_val(value v) {
  void* data = Data_bigarray_val(v);
  int rows = Bigarray_val(v)->dim[0];
  int cols = Bigarray_val(v)->dim[1];
  int channels = Bigarray_val(v)->dim[2];
  int kind = Bigarray_val(v)->flags & BIGARRAY_KIND_MASK;
  int type = 0;
  switch (kind) {

  case BIGARRAY_FLOAT32:
    switch (channels) {
    case 1:
      type = CV_32FC1;
      break;
    case 2:
      type = CV_32FC2;
      break;
    case 3:
      type = CV_32FC3;
      break;
    default:
      break;
    }
    break;

  case BIGARRAY_FLOAT64:
    switch (channels) {
    case 1:
      type = CV_64FC1;
      break;
    case 2:
      type = CV_64FC2;
      break;
    case 3:
      type = CV_64FC3;
      break;
    default:
      break;
    }
    break;

  case BIGARRAY_INT32:
    switch (channels) {
    case 1:
      type = CV_32SC1;
      break;
    case 2:
      type = CV_32SC2;
      break;
    case 3:
      type = CV_32SC3;
      break;
    default:
      break;
    }
    break;

  default:
    break;
  }
  if(type == 0) caml_failwith("CvMat_val case not handled");

  return (cvMat(rows, cols, type, data));
}

uint cvType_table[] = {
  IPL_DEPTH_1U,
  IPL_DEPTH_8U,
  IPL_DEPTH_16U,
  IPL_DEPTH_32F,
  IPL_DEPTH_8S,
  IPL_DEPTH_16S,
  IPL_DEPTH_32S
};

extern "C" CAMLprim value ocaml_get_cvType(value vtype)
{
  return (Val_int(cvType_table[Int_val(vtype)]));
}

int iscolor_table[] = {
  CV_LOAD_IMAGE_UNCHANGED,
  CV_LOAD_IMAGE_GRAYSCALE,
  CV_LOAD_IMAGE_COLOR,
  CV_LOAD_IMAGE_ANYDEPTH,
  CV_LOAD_IMAGE_ANYCOLOR
};

extern "C" CAMLprim value ocaml_get_iscolor(value iscolor)
{
  return (Val_int(iscolor_table[Int_val(iscolor)]));
}

#define CvMemStorage_val(v) (*(CvMemStorage**) (Data_custom_val(v)))

void caml_finalize_CvMemStorage(value vstor)
{
  CvMemStorage* stor = CvMemStorage_val(vstor);
  if (stor) {
    cvReleaseMemStorage(&stor);
    CvMemStorage_val(vstor) = 0;
  }
}

static struct custom_operations CvMemStorage_operations = {
  (char*) "ocaml_opencv_CvMemStorage",
  caml_finalize_CvMemStorage,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

extern "C" CAMLprim value ocaml_create_CvMemStorage(value vunit)
{
  CAMLparam1(vunit);
  CAMLlocal1 (res);
  CvMemStorage *stor = cvCreateMemStorage(0);
  res = caml_alloc_custom(&CvMemStorage_operations, sizeof(CvMemStorage*), 1, 10);
  CvMemStorage_val(res) = stor;
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_free_CvMemStorage(value vstor)
{
  CAMLparam1(vstor);
  caml_finalize_CvMemStorage(vstor);
  CAMLreturn(Val_unit);
}

struct image
{
    IplImage* image;
    int releasable;
};

#define Image_val(v) ((struct image*) (Data_custom_val(v)))

void caml_finalize_IplImage(value vimage)
{
  struct image* image = Image_val(vimage);
  if(image->releasable) cvReleaseImage(&(image->image));
}

static struct custom_operations IplImage_operations = {
  (char*) "ocaml_opencv_IplImage",
  caml_finalize_IplImage,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLexport value caml_alloc_IplImage(IplImage *image)
{
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&IplImage_operations, sizeof(struct image), 1, 10);
  Image_val(res)->image = image;
  Image_val(res)->releasable = 1;
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_cvCreateImage(value vsize, value vtype, value vi)
{
  CAMLparam3 (vsize, vtype, vi);
  CAMLlocal1 (res);
  IplImage* image = cvCreateImage(CvSize_val(vsize), Int_val(vtype), Int_val(vi));
  res = caml_alloc_IplImage(image);
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_cvCreateImage_depth(value vsize, value vdepth, value vi)
{
  CAMLparam3 (vsize, vdepth, vi);
  CAMLlocal1 (res);
  IplImage* image = cvCreateImage(CvSize_val(vsize), Int_val(vdepth), Int_val(vi));
  res = caml_alloc_IplImage(image);
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_cvLoadImage(value vfile, value viscolor)
{
  CAMLparam2 (vfile, viscolor);
  CAMLlocal1 (res);
  IplImage* image = cvLoadImage( String_val(vfile), Int_val(viscolor) );
  if(image == 0) { caml_failwith("load_image: can't load image"); };
  res = caml_alloc_IplImage(image);
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_cvSaveImage(value vfile, value vimg)
{
  CAMLparam2 (vfile, vimg);

  int ret = cvSaveImage(String_val(vfile),
                        Image_val(vimg)->image);

  CAMLreturn (Val_int(ret));
}

extern "C" CAMLprim value ocaml_cvCloneImage(value vimage)
{
  CAMLparam1 (vimage);
  CAMLlocal1 (res);
  IplImage* image = cvCloneImage( Image_val(vimage)->image );
  res = caml_alloc_IplImage(image);
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_cvCopy(value vsrc, value vdst, value vmask)
{
  CAMLparam3 (vsrc, vdst, vmask);
  CvArr* mask = Is_long(vmask) ? NULL : Image_val(Field(vmask,0))->image;
  ERRWRAP(
  cvCopy(Image_val(vsrc)->image,
         Image_val(vdst)->image,
         mask));
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_cvSet(value vdst, value vval, value vmask)
{
  CAMLparam3 (vdst, vval, vmask);
  CvArr* mask = Is_long(vmask) ? NULL : Image_val(Field(vmask,0))->image;
  ERRWRAP(
  cvSet(Image_val(vdst)->image,
        CvScalar_val(vval),
        mask));
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_cvZero(value vimage)
{
  CAMLparam1 (vimage);
  cvZero( Image_val(vimage)->image );
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_cvShowImage(value vwindow, value vimage)
{
  CAMLparam2 (vwindow, vimage);
  cvShowImage( String_val(vwindow), Image_val(vimage)->image);
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_image_size(value vimage)
{
  CAMLparam1 (vimage);
  CAMLlocal1 (res);
  IplImage* image = Image_val(vimage)->image;
  res = caml_alloc_tuple(2);
  Field(res,0) = Val_int(image->width);
  Field(res,1) = Val_int(image->height);
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_cvNamedWindow(value vwindow, value vi)
{
  CAMLparam2 (vwindow, vi);
  ERRWRAP(cvNamedWindow(String_val(vwindow), Int_val(vi)));
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_cvDestroyWindow(value vwindow)
{
  CAMLparam1 (vwindow);
  ERRWRAP(cvDestroyWindow(String_val(vwindow)));
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_cvDestroyAllWindows(value vunit)
{
  CAMLparam1 (vunit);
  ERRWRAP(cvDestroyAllWindows());
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_cvWaitKey(value vi)
{
  return (Val_int(cvWaitKey(Int_val(vi))));
}

#define Capture_val(v) (*(CvCapture**) (Data_custom_val(v)))

CAMLexport void caml_finalize_CvCapture(value vcapture)
{
  CvCapture *capture = Capture_val(vcapture);
  cvReleaseCapture(&capture);
}

static struct custom_operations CvCapture_operations = {
  (char*) "ocaml_opencv_CvCapture",
  caml_finalize_CvCapture,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLexport value caml_alloc_CvCapture(CvCapture *capture)
{
  value res;
  res = caml_alloc_custom(&CvCapture_operations, sizeof(CvCapture*), 1, 1000);
  Capture_val(res) = capture;
  return res;
}

extern "C" CAMLprim value ocaml_cvCaptureFromCAM(value vsource)
{
  CAMLparam1 (vsource);
  CAMLlocal1 (res);
  CvCapture *capture = cvCaptureFromCAM(Int_val(vsource));
  if(capture == 0) { caml_failwith("capture_from_cam: can't start capture"); };
  res = caml_alloc_CvCapture(capture);
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_cvQueryFrame(value vcapture)
{
  CAMLparam1 (vcapture);
  CAMLlocal1 (res);
  IplImage* image = cvQueryFrame( Capture_val(vcapture) );
  if(image == 0) { caml_failwith("query_frame: can't query frame"); };
  res = caml_alloc_IplImage(image);
  Image_val(res)->releasable = 0;
  CAMLreturn (res);
}

int cv_cap_table[] = {
  CV_CAP_PROP_DC1394_OFF,
  CV_CAP_PROP_DC1394_MODE_MANUAL,
  CV_CAP_PROP_DC1394_MODE_AUTO,
  CV_CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO,
  CV_CAP_PROP_POS_MSEC,
  CV_CAP_PROP_POS_FRAMES,
  CV_CAP_PROP_POS_AVI_RATIO,
  CV_CAP_PROP_FRAME_WIDTH,
  CV_CAP_PROP_FRAME_HEIGHT,
  CV_CAP_PROP_FPS,
  CV_CAP_PROP_FOURCC,
  CV_CAP_PROP_FRAME_COUNT,
  CV_CAP_PROP_FORMAT,
  CV_CAP_PROP_MODE,
  CV_CAP_PROP_BRIGHTNESS,
  CV_CAP_PROP_CONTRAST,
  CV_CAP_PROP_SATURATION,
  CV_CAP_PROP_HUE,
  CV_CAP_PROP_GAIN,
  CV_CAP_PROP_EXPOSURE,
  CV_CAP_PROP_CONVERT_RGB,
  CV_CAP_PROP_WHITE_BALANCE_BLUE_U,
  CV_CAP_PROP_RECTIFICATION,
  CV_CAP_PROP_MONOCROME,
  CV_CAP_PROP_SHARPNESS,
  CV_CAP_PROP_AUTO_EXPOSURE,
  CV_CAP_PROP_GAMMA,
  CV_CAP_PROP_TEMPERATURE,
  CV_CAP_PROP_TRIGGER,
  CV_CAP_PROP_TRIGGER_DELAY,
  CV_CAP_PROP_WHITE_BALANCE_RED_V,
  CV_CAP_PROP_MAX_DC1394,
  CV_CAP_PROP_AUTOGRAB,
  CV_CAP_PROP_SUPPORTED_PREVIEW_SIZES_STRING,
  CV_CAP_PROP_PREVIEW_FORMAT,
  CV_CAP_OPENNI_DEPTH_GENERATOR,
  CV_CAP_OPENNI_IMAGE_GENERATOR,
  CV_CAP_OPENNI_GENERATORS_MASK,
  CV_CAP_PROP_OPENNI_OUTPUT_MODE,
  CV_CAP_PROP_OPENNI_FRAME_MAX_DEPTH,
  CV_CAP_PROP_OPENNI_BASELINE,
  CV_CAP_PROP_OPENNI_FOCAL_LENGTH,
  CV_CAP_PROP_OPENNI_REGISTRATION_ON,
  CV_CAP_PROP_OPENNI_REGISTRATION,
  CV_CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE,
  CV_CAP_OPENNI_DEPTH_GENERATOR_BASELINE,
  CV_CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH,
  CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON,
  CV_CAP_GSTREAMER_QUEUE_LENGTH,
  CV_CAP_PROP_PVAPI_MULTICASTIP,
  CV_CAP_PROP_XI_DOWNSAMPLING,
  CV_CAP_PROP_XI_DATA_FORMAT,
  CV_CAP_PROP_XI_OFFSET_X,
  CV_CAP_PROP_XI_OFFSET_Y,
  CV_CAP_PROP_XI_TRG_SOURCE,
  CV_CAP_PROP_XI_TRG_SOFTWARE,
  CV_CAP_PROP_XI_GPI_SELECTOR,
  CV_CAP_PROP_XI_GPI_MODE,
  CV_CAP_PROP_XI_GPI_LEVEL,
  CV_CAP_PROP_XI_GPO_SELECTOR,
  CV_CAP_PROP_XI_GPO_MODE,
  CV_CAP_PROP_XI_LED_SELECTOR,
  CV_CAP_PROP_XI_LED_MODE,
  CV_CAP_PROP_XI_MANUAL_WB,
  CV_CAP_PROP_XI_AUTO_WB,
  CV_CAP_PROP_XI_AEAG,
  CV_CAP_PROP_XI_EXP_PRIORITY,
  CV_CAP_PROP_XI_AE_MAX_LIMIT,
  CV_CAP_PROP_XI_AG_MAX_LIMIT,
  CV_CAP_PROP_XI_AEAG_LEVEL,
  CV_CAP_PROP_XI_TIMEOUT
};

extern "C" CAMLprim value ocaml_cvSetCaptureProperty(value vcapture,value vprop,
                                                     value vval)
{
  CAMLparam3(vcapture, vprop, vval);
  ERRWRAP(cvSetCaptureProperty(Capture_val(vcapture),
                               cv_cap_table[Int_val(vprop)],
                               Double_val(vval)));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvGetCaptureProperty(value vcapture,value vprop)
{
  CAMLparam2(vcapture, vprop);
  double ret;
  ERRWRAP(ret = cvGetCaptureProperty(Capture_val(vcapture),
                                     cv_cap_table[Int_val(vprop)]));
  CAMLreturn(caml_copy_double(ret));
}

#define Int_var_val(v) (*(int**) (Data_custom_val(v)))

CAMLexport void caml_finalize_Int_var(value vvar)
{
  int* var = Int_var_val(vvar);
  free(var);
}

static struct custom_operations int_var_operations = {
  (char*) "ocaml_opencv_int_var",
  caml_finalize_Int_var,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

void *cv_malloc(size_t size)
{
  void *ptr = malloc(size);
  if (ptr == NULL) {
    perror("cannot allocate memory");
    abort();
  }
  return ptr;
}

CAMLexport value caml_alloc_int_var(int def)
{
  value res;
  res = caml_alloc_custom(&int_var_operations, sizeof(int*), 1, 1000);
  Int_var_val(res) = (int*) cv_malloc(sizeof(int));
  *(Int_var_val(res)) = def;
  return res;
}

extern "C" CAMLprim value ocaml_create_int_var(value vdef)
{
  CAMLparam1 (vdef);
  CAMLlocal1 (res);
  res = caml_alloc_int_var(Int_val(vdef));
  CAMLreturn (res);
}

extern "C" CAMLprim value ocaml_get_int_var(value vvar)
{
  CAMLparam1 (vvar);
  int* var = Int_var_val(vvar);
  CAMLreturn (Val_int(*var));
}

extern "C" CAMLprim value ocaml_set_int_var(value vvar, value vval)
{
  CAMLparam2 (vvar, vval);
  int* var = Int_var_val(vvar);
  *var = Int_val(vval);
  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_cvCreateTrackbar(value vname, value vwindow, value vvar, value vmax)
{
  CAMLparam4 (vname, vwindow, vvar, vmax);
  int res;
  ERRWRAP(
  res = cvCreateTrackbar(String_val(vname), String_val(vwindow),
                         Int_var_val(vvar), Int_val(vmax), NULL));
  CAMLreturn (Val_int(res));
}

int conversion_table[] = {
  CV_BGR2BGRA,
  CV_RGB2RGBA,
  CV_BGRA2BGR,
  CV_RGBA2RGB,
  CV_BGR2RGBA,
  CV_RGB2BGRA,
  CV_RGBA2BGR,
  CV_BGRA2RGB,
  CV_BGR2RGB,
  CV_RGB2BGR,
  CV_BGRA2RGBA,
  CV_RGBA2BGRA,
  CV_BGR2GRAY,
  CV_RGB2GRAY,
  CV_GRAY2BGR,
  CV_GRAY2RGB,
  CV_GRAY2BGRA,
  CV_GRAY2RGBA,
  CV_BGRA2GRAY,
  CV_RGBA2GRAY,
  CV_BGR2BGR565,
  CV_RGB2BGR565,
  CV_BGR5652BGR,
  CV_BGR5652RGB,
  CV_BGRA2BGR565,
  CV_RGBA2BGR565,
  CV_BGR5652BGRA,
  CV_BGR5652RGBA,
  CV_GRAY2BGR565,
  CV_BGR5652GRAY,
  CV_BGR2BGR555,
  CV_RGB2BGR555,
  CV_BGR5552BGR,
  CV_BGR5552RGB,
  CV_BGRA2BGR555,
  CV_RGBA2BGR555,
  CV_BGR5552BGRA,
  CV_BGR5552RGBA,
  CV_GRAY2BGR555,
  CV_BGR5552GRAY,
  CV_BGR2XYZ,
  CV_RGB2XYZ,
  CV_XYZ2BGR,
  CV_XYZ2RGB,
  CV_BGR2YCrCb,
  CV_RGB2YCrCb,
  CV_YCrCb2BGR,
  CV_YCrCb2RGB,
  CV_BGR2HSV,
  CV_RGB2HSV,
  CV_BGR2Lab,
  CV_RGB2Lab,
  CV_BayerBG2BGR,
  CV_BayerGB2BGR,
  CV_BayerRG2BGR,
  CV_BayerGR2BGR,
  CV_BayerBG2RGB,
  CV_BayerGB2RGB,
  CV_BayerRG2RGB,
  CV_BayerGR2RGB,
  CV_BGR2Luv,
  CV_RGB2Luv,
  CV_BGR2HLS,
  CV_RGB2HLS,
  CV_HSV2BGR,
  CV_HSV2RGB,
  CV_Lab2BGR,
  CV_Lab2RGB,
  CV_Luv2BGR,
  CV_Luv2RGB,
  CV_HLS2BGR,
  CV_HLS2RGB
};

extern "C" CAMLprim value ocaml_get_color_conversion(value vtype)
{
  return (Val_int(conversion_table[Int_val(vtype)]));
}

extern "C" CAMLprim value ocaml_cvCvtColor(value vsrc, value vdst, value vtype)
{
  CAMLparam3(vsrc, vdst, vtype);
  cvCvtColor(Image_val(vsrc)->image,
	     Image_val(vdst)->image,
	     Int_val(vtype));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvSplit(value vsrc, value vdst0,
                             value vdst1, value vdst2, value vdst3)
{
  CAMLparam5(vsrc, vdst0, vdst1, vdst2, vdst3);

  CvArr* dst0 = Is_long(vdst0) ? NULL : Image_val(Field(vdst0,0))->image;
  CvArr* dst1 = Is_long(vdst1) ? NULL : Image_val(Field(vdst1,0))->image;
  CvArr* dst2 = Is_long(vdst2) ? NULL : Image_val(Field(vdst2,0))->image;
  CvArr* dst3 = Is_long(vdst3) ? NULL : Image_val(Field(vdst3,0))->image;

  cvSplit(Image_val(vsrc)->image,
          dst0,
          dst1,
          dst2,
          dst3);

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvMerge(value vsrc0, value vsrc1, value vsrc2,
                             value vsrc3, value vdst)
{
  CAMLparam5(vsrc0, vsrc1, vsrc2, vsrc3, vdst);

  CvArr* src0 = Is_long(vsrc0) ? NULL : Image_val(Field(vsrc0,0))->image;
  CvArr* src1 = Is_long(vsrc1) ? NULL : Image_val(Field(vsrc1,0))->image;
  CvArr* src2 = Is_long(vsrc2) ? NULL : Image_val(Field(vsrc2,0))->image;
  CvArr* src3 = Is_long(vsrc3) ? NULL : Image_val(Field(vsrc3,0))->image;

  cvMerge(src0,
          src1,
          src2,
          src3,
          Image_val(vdst)->image);

  CAMLreturn(Val_unit);
}

int thresholdType_table[] = {
  CV_THRESH_BINARY,
  CV_THRESH_BINARY_INV,
  CV_THRESH_TRUNC,
  CV_THRESH_TOZERO,
  CV_THRESH_TOZERO_INV,
  CV_THRESH_MASK,
  CV_THRESH_OTSU
};

int adaptiveMethod_table[] = {
  CV_ADAPTIVE_THRESH_MEAN_C,
  CV_ADAPTIVE_THRESH_GAUSSIAN_C,
};

extern "C" CAMLprim value ocaml_cvThreshold(value vsrc, value vdst, value vthreshold,
                                 value vmaxValue, value vthresholdType)
{
  CAMLparam5(vsrc, vdst, vthreshold, vmaxValue, vthresholdType);
  cvThreshold(Image_val(vsrc)->image,
              Image_val(vdst)->image,
              Double_val(vthreshold),
              Double_val(vmaxValue),
              thresholdType_table[Int_val(vthresholdType)]);
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvAdaptiveThreshold(value vsrc, value vdst, value vmaxValue,
                                         value vadaptiveMethod, value vthresholdType,
                                         value vblockSize, value vparam1)
{
  CAMLparam5(vsrc, vdst, vmaxValue, vadaptiveMethod, vthresholdType);
  CAMLxparam2(vblockSize, vparam1);
  cvAdaptiveThreshold(Image_val(vsrc)->image,
                      Image_val(vdst)->image,
                      Double_val(vmaxValue),
                      adaptiveMethod_table[Int_val(vadaptiveMethod)],
                      thresholdType_table[Int_val(vthresholdType)],
                      Int_val(vblockSize),
                      Double_val(vparam1));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvAdaptiveThreshold_bytecode( value * argv, int argn )
{
  return ocaml_cvAdaptiveThreshold( argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6] );
}

extern "C" CAMLprim value ocaml_cvCanny(value vimage, value vedges,
                                        value vthreshold1, value vthreshold2,
                                        value vapertureSize)
{
  CAMLparam5(vimage, vedges, vthreshold1, vthreshold2, vapertureSize);

  cvCanny(Image_val(vimage)->image,
          Image_val(vedges)->image,
          Double_val(vthreshold1),
          Double_val(vthreshold2),
          Int_val(vapertureSize));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvEqualizeHist(value vsrc, value vdst)
{
  CAMLparam2 (vsrc, vdst);

  ERRWRAP(
  cvEqualizeHist(Image_val(vsrc)->image,
                 Image_val(vdst)->image));
  CAMLreturn (Val_unit);
}


int morph_table[] = {
  MORPH_RECT,
  MORPH_ELLIPSE,
  MORPH_CROSS
};

extern "C" CAMLprim value ocaml_dilate(value vsrc, value vdst, value shape,
                                        value ksize, value viter)
{
  CAMLparam5 (vsrc, vdst, shape, ksize, viter);

  Mat sel = getStructuringElement(morph_table[Int_val(shape)],
                                  CvSize_val(ksize));

  Mat src = Mat(Image_val(vsrc)->image);
  Mat dst = Mat(Image_val(vdst)->image);

  ERRWRAP(
  dilate(src,
        dst,
        sel,
        Point(-1,-1),
        Int_val(viter)));

  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_erode(value vsrc, value vdst, value shape,
                                        value ksize, value viter)
{
  CAMLparam5 (vsrc, vdst, shape, ksize, viter);

  Mat sel = getStructuringElement(morph_table[Int_val(shape)],
                                  CvSize_val(ksize));

  Mat src = Mat(Image_val(vsrc)->image);
  Mat dst = Mat(Image_val(vdst)->image);

  ERRWRAP(
  erode(src,
        dst,
        sel,
        Point(-1,-1),
        Int_val(viter)));

  CAMLreturn (Val_unit);
}

extern "C" CAMLprim value ocaml_medianBlur(value vsrc, value vdst, value ksize)
{
  CAMLparam3(vsrc, vdst, ksize);

  Mat dst = Mat(Image_val(vdst)->image);

  ERRWRAP(
  medianBlur(Mat(Image_val(vsrc)->image),
             dst,
             Int_val(ksize)));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_blur(value vsrc, value vdst, value ksize)
{
  CAMLparam3(vsrc, vdst, ksize);

  Mat dst = Mat(Image_val(vdst)->image);

  ERRWRAP(
  blur(Mat(Image_val(vsrc)->image),
       dst,
       CvSize_val(ksize)));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_GaussianBlur(value vsrc, value vdst, value ksize,
                                             value sigmaX, value sigmaY)
{
  CAMLparam5(vsrc, vdst, ksize, sigmaX, sigmaY);

  Mat dst = Mat(Image_val(vdst)->image);

  ERRWRAP(
  GaussianBlur(Mat(Image_val(vsrc)->image),
               dst,
               CvSize_val(ksize),
               Double_val(sigmaX),
               Double_val(sigmaY)));

  CAMLreturn(Val_unit);
}


extern "C" CAMLprim value ocaml_cvGoodFeaturesToTrack(
           value image,
           value vcorners,
           value qualityLevel,
           value minDistance,
           value vmask,
           value blockSize,
           value useHarris,
           value k)
{
    CAMLparam5(image,
               vcorners,
               qualityLevel,
               minDistance,
               vmask);
    CAMLxparam3(blockSize,
                useHarris,
                k);

    CvArr* mask = Is_long(vmask) ? NULL : Image_val(Field(vmask,0))->image;
    CvPoint2D32f* corners = (CvPoint2D32f*) Data_bigarray_val(vcorners);
    int cornerCount = Bigarray_val(vcorners)->dim[0];;

    ERRWRAP(
    cvGoodFeaturesToTrack(Image_val(image)->image,
                          NULL,
                          NULL,
                          corners,
                          &cornerCount,
                          Double_val(qualityLevel),
                          Double_val(minDistance),
                          mask,
                          Int_val(blockSize),
                          Bool_val(useHarris),
                          Double_val(k)));

    CAMLreturn(Val_int(cornerCount));
}

extern "C" CAMLprim value ocaml_cvGoodFeaturesToTrack_bytecode( value * argv, int argn )
{
  return ocaml_cvGoodFeaturesToTrack( argv[0], argv[1], argv[2], argv[3],
                                      argv[4], argv[5], argv[6], argv[7] );
}

extern "C" CAMLprim value ocaml_image_to_bigarray(value vimage)
{
  CAMLparam1(vimage);
  CAMLlocal1(ba);
  intnat dim[2];
  IplImage* image = Image_val(vimage)->image;
  dim[0] = image->height;
  dim[1] = image->widthStep;
  ba = caml_ba_alloc(CAML_BA_UINT8|CAML_BA_EXTERNAL|CAML_BA_C_LAYOUT,2,
		     image->imageData, dim );
  CAMLreturn(ba);
}

extern "C" CAMLprim value ocaml_image_channels(value vimage)
{
  CAMLparam1(vimage);
  CAMLreturn(Val_int(Image_val(vimage)->image->nChannels));
}

extern "C" CAMLprim value ocaml_image_depth(value vimage)
{
  CAMLparam1(vimage);
  CAMLreturn(Val_int(Image_val(vimage)->image->depth));
}

extern "C" CAMLprim value ocaml_image_data_order(value vimage)
{
  CAMLparam1(vimage);
  CAMLreturn(Val_int(Image_val(vimage)->image->dataOrder));
}

/* drawing */
extern "C" CAMLprim value ocaml_cvCircle(value vimg, value vcenter, value vradius,
                              value vcolor, value vthickness)
{
  CAMLparam5(vimg, vcenter, vradius, vcolor, vthickness);
  cvCircle(Image_val(vimg)->image, CvPoint_val(vcenter), Int_val(vradius), CvScalar_val(vcolor),
           Int_val(vthickness), CV_AA, 0);
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvEllipse(value vimg, value vcenter,
                               value vaxes, value vangle,
                               value vstart_angle, value vend_angle,
                               value vcolor, value vthickness)
{
  CAMLparam5(vimg, vcenter, vaxes, vangle, vstart_angle);
  CAMLxparam3(vend_angle, vcolor, vthickness);
  ERRWRAP(
  cvEllipse(Image_val(vimg)->image, CvPoint_val(vcenter), CvSize_val(vaxes),
            Double_val(vangle), Double_val(vstart_angle), Double_val(vend_angle),
            CvScalar_val(vcolor), Int_val(vthickness), CV_AA, 0));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvEllipse_bytecode( value * argv, int argn )
{
  return ocaml_cvEllipse( argv[0], argv[1], argv[2], argv[3],
                          argv[4], argv[5], argv[6], argv[7] );
}

extern "C" CAMLprim value ocaml_cvRectangle(value vimg, value vpt1, value vpt2,
                                 value vcolor, value vthickness)
{
  CAMLparam5(vimg, vpt1, vpt2, vcolor, vthickness);
  cvRectangle(Image_val(vimg)->image, CvPoint_val(vpt1), CvPoint_val(vpt2),
              CvScalar_val(vcolor), Int_val(vthickness), CV_AA, 0);
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvLine(value vimg, value vpt1, value vpt2,
                            value vcolor, value vthickness)
{
  CAMLparam5(vimg, vpt1, vpt2, vcolor, vthickness);
  cvLine(Image_val(vimg)->image, CvPoint_val(vpt1), CvPoint_val(vpt2),
         CvScalar_val(vcolor), Int_val(vthickness), CV_AA, 0);
  CAMLreturn(Val_unit);
}

/* contour functions */

#define CvSeq_val(v) (*(struct CvSeq**) (Data_custom_val(v)))
#define CvContour_val(v) (*(struct CvContour**) (Data_custom_val(v)))

static struct custom_operations cvSeq_operations = {
  (char*) "ocaml_opencv_cvSeq",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLexport value caml_alloc_cvSeq(CvSeq* seq)
{
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&cvSeq_operations, sizeof(CvSeq*), 1, 1000);
  CvSeq_val(res) = seq;
  CAMLreturn(res);
}

static int contour_retrieval_mode_table[] = {
  CV_RETR_EXTERNAL,
  CV_RETR_LIST,
  CV_RETR_CCOMP,
  CV_RETR_TREE
};

static int contour_approximation_method_table[] = {
  CV_CHAIN_CODE,
  CV_CHAIN_APPROX_NONE,
  CV_CHAIN_APPROX_SIMPLE,
  CV_CHAIN_APPROX_TC89_L1,
  CV_CHAIN_APPROX_TC89_KCOS,
  CV_LINK_RUNS
};

extern "C" CAMLprim value ocaml_cvFindContours(value vimg, value vstor, value vmode,
                                    value vmethod, value voffset)
{
  CAMLparam5(vimg, vstor, vmode, vmethod, voffset);
  CAMLlocal1(res);
  CvSeq *first_contour = 0;

  ERRWRAP(
  cvFindContours(Image_val(vimg)->image,
                 CvMemStorage_val(vstor),
                 &first_contour,
                 sizeof(CvContour),
                 contour_retrieval_mode_table[Int_val(vmode)],
                 contour_approximation_method_table[Int_val(vmethod)],
                 CvPoint_val(voffset)));

  if(first_contour) {
    res = Val_some(caml_alloc_cvSeq(first_contour));
  }
  else {
    res = Val_none;
  }

  CAMLreturn(res);
}

extern "C" CAMLprim value ocaml_HoughCircles(value vimg,
                                             value vcircles,
                                             value dp,
                                             value minDist,
                                             value param1,
                                             value param2,
                                             value minRadius,
                                             value maxRadius)
{
  CAMLparam4( vimg, vcircles, dp, minDist );
  CAMLxparam4( param1, param2, minRadius, maxRadius );

  ERRWRAP(
  HoughCircles(Mat(Image_val(vimg)->image),
               *Vector_val<Vec3f>(vcircles),
               CV_HOUGH_GRADIENT,
               Double_val(dp),
               Double_val(minDist),
               Double_val(param1),
               Double_val(param2),
               Int_val(minRadius),
               Int_val(maxRadius) ));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_HoughCircles_bytecode( value * argv, int argn )
{
  return ocaml_HoughCircles( argv[0], argv[1], argv[2], argv[3],
                             argv[4], argv[5], argv[6], argv[7] );
}

/* houghlines */

extern "C" CAMLprim value ocaml_HoughLinesP(value vimg,
                                            value vlines,
                                            value rho,
                                            value theta,
                                            value threshold,
                                            value minLineLength,
                                            value maxLineGap)
{
  CAMLparam5( vimg, vlines, rho, theta, threshold );
  CAMLxparam2( minLineLength, maxLineGap );

  ERRWRAP(
  HoughLinesP(Mat(Image_val(vimg)->image),
              *Vector_val<Vec4i>(vlines),
              Double_val(rho),
              Double_val(theta),
              Int_val(threshold),
              Double_val(minLineLength),
              Double_val(maxLineGap)));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_HoughLinesP_bytecode( value * argv, int argn )
{
  return ocaml_HoughLinesP( argv[0], argv[1], argv[2], argv[3],
                            argv[4], argv[5], argv[6] );
}

extern "C" CAMLprim value ocaml_CvSeq_info(value vseq)
{
  CAMLparam1(vseq);
  CAMLlocal1(res);

  CvSeq *seq = CvSeq_val(vseq);

  res = caml_alloc_tuple(4);
  Field(res,0) = Val_none;
  Field(res,1) = Val_none;
  Field(res,2) = Val_none;
  Field(res,3) = Val_none;

  if(seq->h_prev) Field(res,0) = Val_some( caml_alloc_cvSeq(seq->h_prev) );
  if(seq->h_next) Field(res,1) = Val_some( caml_alloc_cvSeq(seq->h_next) );
  if(seq->v_prev) Field(res,2) = Val_some( caml_alloc_cvSeq(seq->v_prev) );
  if(seq->v_next) Field(res,3) = Val_some( caml_alloc_cvSeq(seq->v_next) );

  CAMLreturn(res);
}

extern "C" CAMLprim value ocaml_cvDrawContours(value vimg, value vcontour,
                                    value vexternal_color, value vhole_color,
                                    value vmax_level, value vthickness,
                                    value voffset) {
  CAMLparam5(vimg, vcontour, vexternal_color, vhole_color, vmax_level);
  CAMLxparam2(vthickness, voffset);

  cvDrawContours(Image_val(vimg)->image,
                 CvSeq_val(vcontour),
                 CvScalar_val(vexternal_color),
                 CvScalar_val(vhole_color),
                 Int_val(vmax_level),
                 Int_val(vthickness),
                 CV_AA,
                 CvPoint_val(voffset));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvDrawContours_bytecode( value * argv, int argn )
{
  return ocaml_cvDrawContours( argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6] );
}

extern "C" CAMLprim value ocaml_cvFitEllipse2( value vpoints )
{
  CAMLparam1(vpoints);
  CAMLlocal1(res);
  res = caml_alloc_tuple(5);
  CvSeq *points = CvSeq_val(vpoints);
  if(points->total < 5) CAMLreturn(Val_none);
  CvBox2D box;
  ERRWRAP (box = cvFitEllipse2(points));
  Field(res,0) = caml_copy_double(box.center.x);
  Field(res,1) = caml_copy_double(box.center.y);
  Field(res,2) = caml_copy_double(box.size.width);
  Field(res,3) = caml_copy_double(box.size.height);
  Field(res,4) = caml_copy_double(box.angle);
  CAMLreturn(Val_some(res));
}

static int cv_calib_cb_table[] = {
  CV_CALIB_CB_ADAPTIVE_THRESH,
  CV_CALIB_CB_NORMALIZE_IMAGE,
  CV_CALIB_CB_FILTER_QUADS,
  CV_CALIB_CB_FAST_CHECK
};

extern "C" CAMLprim value ocaml_cvFindChessboardCorners(value vimg, value vpatternSize, value vflags)
{
  CAMLparam3(vimg, vpatternSize, vflags);
  CAMLlocal2(res,vcorners);

  CvSize size = CvSize_val(vpatternSize);

  intnat dim[3];
  dim[0] = size.width;
  dim[1] = size.height;
  dim[2] = 2;
  vcorners = caml_ba_alloc(CAML_BA_FLOAT32|CAML_BA_MANAGED|CAML_BA_C_LAYOUT, 3, NULL, dim );
  CvPoint2D32f* corners = (CvPoint2D32f*) Data_bigarray_val(vcorners);
  int cornerCount = 0;
  int flags = 0;
  value flag_list = vflags;
  for(;Is_long(flag_list);flag_list=Field(flag_list,1))
    flags |= cv_calib_cb_table[Int_val(Field(flag_list,0))];

  int ret;

  ERRWRAP(
      ret = cvFindChessboardCorners(Image_val(vimg)->image,
                                    size,
                                    corners,
                                    &cornerCount,
                                    flags));

  res = caml_alloc_tuple(3);
  Field(res,0) = Val_bool(ret);
  Field(res,1) = Val_int(cornerCount);
  Field(res,2) = vcorners;

  CAMLreturn(res);
}

extern "C" CAMLprim value ocaml_cvDrawChessboardCorners(value vimg, value vpatternSize,
                                             value vcorners, value vcount,
                                             value vpatternWasFound)
{
  CAMLparam5(vimg, vpatternSize, vcorners, vcount, vpatternWasFound);

  cvDrawChessboardCorners(Image_val(vimg)->image,
                          CvSize_val(vpatternSize),
                          (CvPoint2D32f*) Data_bigarray_val(vcorners),
                          Int_val(vcount),
                          Int_val(vpatternWasFound));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvFindCornerSubPix(value vimg, value vcorners,
                                        value vcount, value vwinSize,
                                        value vzeroZone, value vcriteria)
{
  CAMLparam5(vimg, vcorners, vcount, vwinSize, vzeroZone);
  CAMLxparam1(vcriteria);

  cvFindCornerSubPix(Image_val(vimg)->image,
                     (CvPoint2D32f*) Data_bigarray_val(vcorners),
                     Int_val(vcount),
                     CvSize_val(vwinSize),
                     CvSize_val(vzeroZone),
                     CvTermCriteria_val(vcriteria));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvFindCornerSubPix_bytecode( value * argv, int argn )
{
  return ocaml_cvFindCornerSubPix( argv[0], argv[1], argv[2], argv[3],
                                   argv[4], argv[5] );
}

extern "C" CAMLprim value ocaml_cvCalibrateCamera2(value vobjectPoints, value vimagePoints,
                                        value vpointCounts, value vimageSize,
                                        value vcameraMatrix, value vdistCoeffs)
{
  CAMLparam5(vobjectPoints, vimagePoints, vpointCounts, vimageSize, vcameraMatrix);
  CAMLxparam1(vdistCoeffs);

  CvMat objectPoints = CvMat_val(vobjectPoints);
  CvMat imagePoints = CvMat_val(vimagePoints);
  CvMat pointCounts = CvMat_val(vpointCounts);
  CvMat cameraMatrix = CvMat_val(vcameraMatrix);
  CvMat distCoeffs = CvMat_val(vdistCoeffs);

  double ret = 0;

  ERRWRAP(ret = cvCalibrateCamera2(&objectPoints,
                                   &imagePoints,
                                   &pointCounts,
                                   CvSize_val(vimageSize),
                                   &cameraMatrix,
                                   &distCoeffs,
                                   NULL,
                                   NULL,
                                   CV_CALIB_FIX_ASPECT_RATIO))

  CAMLreturn(caml_copy_double(ret));
}

extern "C" CAMLprim value ocaml_cvCalibrateCamera2_bytecode( value * argv, int argn )
{
  return ocaml_cvCalibrateCamera2( argv[0], argv[1], argv[2], argv[3],
                                   argv[4], argv[5] );
}

extern "C" CAMLprim value ocaml_cvInitUndistortMap( value vcameraMatrix, value vdistCoeffs,
                                         value vmap1, value vmap2 )
{
  CAMLparam4( vcameraMatrix, vdistCoeffs, vmap1, vmap2 );

  CvMat cameraMatrix = CvMat_val(vcameraMatrix);
  CvMat distCoeffs = CvMat_val(vdistCoeffs);

  ERRWRAP(cvInitUndistortMap(&cameraMatrix,
                             &distCoeffs,
                             Image_val(vmap1)->image,
                             Image_val(vmap2)->image));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvRemap( value vsrc, value vdst,
                              value vmapx, value vmapy )
{
  CAMLparam4( vsrc, vdst, vmapx, vmapy );

  ERRWRAP(cvRemap(Image_val(vsrc)->image,
                  Image_val(vdst)->image,
                  Image_val(vmapx)->image,
                  Image_val(vmapy)->image,
                  CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS,
                  cvScalarAll(0)));

  CAMLreturn(Val_unit);
}


extern "C" CAMLprim value ocaml_cvFindHomography( value vsrc, value vdst, value vH )
{
  CAMLparam3( vsrc, vdst, vH );

  CvMat src = CvMat_val(vsrc);
  CvMat dst = CvMat_val(vdst);
  CvMat H = CvMat_val(vH);

  ERRWRAP(cvFindHomography(&src,
                           &dst,
                           &H,
                           0,
                           3,
                           NULL));

  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value ocaml_cvInvert( value vsrc, value vdst )
{
  CAMLparam2( vsrc, vdst );
  CvMat src = CvMat_val(vsrc);
  CvMat dst = CvMat_val(vdst);

  double ret;
  ERRWRAP(ret = cvInvert( &src, &dst, CV_LU));

  CAMLreturn(caml_copy_double(ret));
}
