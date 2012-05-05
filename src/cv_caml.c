#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/callback.h>

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

int cvType_table[] = {
  IPL_DEPTH_1U,
  IPL_DEPTH_8U,
  IPL_DEPTH_16U,
  IPL_DEPTH_32F,
  IPL_DEPTH_8S,
  IPL_DEPTH_16S,
  IPL_DEPTH_32S
};

int ocaml_get_cvType(int vtype)
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

int ocaml_get_iscolor(int iscolor)
{
  return (Val_int(iscolor_table[Int_val(iscolor)]));
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
  "ocaml_opencv_IplImage",
  caml_finalize_IplImage,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLexport value caml_alloc_IplImage(IplImage *image)
{
  CAMLparam0();
  CAMLlocal1 (res);
  res = caml_alloc_custom(&IplImage_operations, sizeof(struct image), 1, 1000);
  Image_val(res)->image = image;
  Image_val(res)->releasable = 1;
  CAMLreturn (res);
}

CAMLprim value ocaml_cvCreateImage(value vsize, value vtype, value vi)
{
  CAMLparam3 (vsize, vtype, vi);
  CAMLlocal1 (res);
  IplImage* image = cvCreateImage(cvSize(Int_val(Field (vsize, 0)),
					 Int_val(Field (vsize, 1))),
				  Int_val(vtype), Int_val(vi));
  res = caml_alloc_IplImage(image);
  CAMLreturn (res);
}

CAMLprim value ocaml_cvLoadImage(value vfile, value viscolor)
{
  CAMLparam2 (vfile, viscolor);
  CAMLlocal1 (res);
  IplImage* image = cvLoadImage( String_val(vfile), Int_val(viscolor) );
  if(image == 0) { caml_failwith("load_image: can't load image"); };
  res = caml_alloc_IplImage(image);
  CAMLreturn (res);
}

CAMLprim value ocaml_cvCloneImage(value vimage)
{
  CAMLparam1 (vimage);
  CAMLlocal1 (res);
  IplImage* image = cvCloneImage( Image_val(vimage)->image );
  if(image == 0) { caml_failwith("load_image: can't load image"); };
  res = caml_alloc_IplImage(image);
  CAMLreturn (res);
}

CAMLprim value ocaml_cvZero(value vimage)
{
  CAMLparam1 (vimage);
  cvZero( Image_val(vimage)->image );
  CAMLreturn (Val_unit);
}

CAMLprim value ocaml_cvShowImage(value vwindow, value vimage)
{
  CAMLparam2 (vwindow, vimage);
  cvShowImage( String_val(vwindow), Image_val(vimage)->image);
  CAMLreturn (Val_unit);
}

CAMLprim value ocaml_image_size(value vimage)
{
  CAMLparam1 (vimage);
  CAMLlocal1 (res);
  IplImage* image = Image_val(vimage)->image;
  res = caml_alloc_tuple(2);
  Field(res,0) = Val_int(image->width);
  Field(res,1) = Val_int(image->height);
  CAMLreturn (res);
}

CAMLprim value ocaml_cvNamedWindow(value vwindow, value vi)
{
  CAMLparam2 (vwindow, vi);
  cvNamedWindow(String_val(vwindow), Int_val(vi));
  CAMLreturn (Val_unit);
}

CAMLprim value ocaml_cvWaitKey(value vi)
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
  "ocaml_opencv_CvCapture",
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

CAMLprim value ocaml_cvCaptureFromCAM(value vsource)
{
  CAMLparam1 (vsource);
  CAMLlocal1 (res);
  CvCapture *capture = cvCaptureFromCAM(Int_val(vsource));
  if(capture == 0) { caml_failwith("capture_from_cam: can't start capture"); };
  res = caml_alloc_CvCapture(capture);
  CAMLreturn (res);
}

CAMLprim value ocaml_cvQueryFrame(value vcapture)
{
  CAMLparam1 (vcapture);
  CAMLlocal1 (res);
  IplImage* image = cvQueryFrame( Capture_val(vcapture) );
  if(image == 0) { caml_failwith("query_frame: can't query frame"); };
  res = caml_alloc_IplImage(image);
  Image_val(res)->releasable = 0;
  CAMLreturn (res);
}

#define Int_var_val(v) (*(int**) (Data_custom_val(v)))

CAMLexport void caml_finalize_Int_var(value vvar)
{
  int* var = Int_var_val(vvar);
  free(var);
}

static struct custom_operations int_var_operations = {
  "ocaml_opencv_int_var",
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
  Int_var_val(res) = cv_malloc(sizeof(int));
  *(Int_var_val(res)) = def;
  return res;
}

CAMLprim value create_int_var(value vdef)
{
  CAMLparam1 (vdef);
  CAMLlocal1 (res);
  res = caml_alloc_int_var(Int_val(vdef));
  CAMLreturn (res);
}

CAMLprim value ocaml_cvCreateTrackbar(value vname, value vwindow, value vvar, value vmax)
{
  CAMLparam4 (vname, vwindow, vvar, vmax);
  int res = cvCreateTrackbar(String_val(vname), String_val(vwindow),
			     Int_var_val(vvar), Int_val(vmax), NULL);
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

int ocaml_get_color_conversion(int vtype)
{
  return (Val_int(conversion_table[Int_val(vtype)]));
}

CAMLprim value ocaml_cvCvtColor(value vsrc, value vdst, value vtype)
{
  CAMLparam3(vsrc, vdst, vtype);
  cvCvtColor(Image_val(vsrc)->image,
	     Image_val(vdst)->image,
	     Int_val(vtype));
  CAMLreturn(Val_unit);
}

int threshold_table[] = {
  CV_THRESH_BINARY,
  CV_THRESH_BINARY_INV,
  CV_THRESH_TRUNC,
  CV_THRESH_TOZERO,
  CV_THRESH_TOZERO_INV,
  CV_THRESH_MASK,
  CV_THRESH_OTSU
};

int ocaml_get_threshold(int vtype)
{
  return (Val_int(threshold_table[Int_val(vtype)]));
}

CAMLprim value ocaml_cvThreshold(value vsrc, value vdst, value vthreshold,
                                 value vmaxValue, value vthresholdType)
{
  CAMLparam5(vsrc, vdst, vthreshold, vmaxValue, vthresholdType);
  cvThreshold(Image_val(vsrc)->image,
              Image_val(vdst)->image,
              Double_val(vthreshold),
              Double_val(vmaxValue),
              threshold_table[Int_val(vthresholdType)]);
  CAMLreturn(Val_unit);
}


/* CAMLprim value ocaml_get_float_3point(value vimage, value vindex, value vres) */
/* { */
/*   IplImage* image = Image_val(vsrc)->image; */
/*   int index = Int_val(vindex); */
/*   Store_double_field(vres,0,image->imageData[3*i]); */
/*   Store_double_field(vres,1,image->imageData[3*i+1]); */
/*   Store_double_field(vres,2,image->imageData[3*i+2]); */
/*   CAMLreturn(Val_unit); */
/* } */

/* CAMLprim value ocaml_set_float_3point(value vimage, value vindex, value vval) */
/* { */
/*   IplImage* image = Image_val(vsrc)->image; */
/*   int index = Int_val(vindex); */
/*   camlimage->imageData[3*i] = Double_field(vval,0); */
/*   camlimage->imageData[3*i+1] = Double_field(vval,1); */
/*   camlimage->imageData[3*i+2] = Double_field(vval,2); */
/*   CAMLreturn(Val_unit); */
/* } */

/* CAMLprim value ocaml_get_int_3point(value vimage, value vindex, value vres) */
/* { */
/*   IplImage* image = Image_val(vimage)->image; */
/*   int i = Int_val(vindex); */
/*   Field(vres,0) = image->imageData[3*i]; */
/*   Field(vres,1) = image->imageData[3*i+1]; */
/*   Field(vres,2) = image->imageData[3*i+2]; */
/*   return Val_unit; */
/* } */

/* CAMLprim value ocaml_set_int_3point(value vimage, value vindex, value vres) */
/* { */
/*   IplImage* image = Image_val(vimage)->image; */
/*   int i = Int_val(vindex); */
/*   image->imageData[3*i] = Field(vres,0); */
/*   image->imageData[3*i+1] = Field(vres,1); */
/*   image->imageData[3*i+2] = Field(vres,2); */
/*   return Val_unit; */
/* } */

CAMLprim value ocaml_image_to_bigarray(value vimage)
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

CAMLprim value ocaml_image_channels(value vimage)
{
  CAMLparam1(vimage);
  CAMLreturn(Val_int(Image_val(vimage)->image->nChannels));
}

CAMLprim value ocaml_image_depth(value vimage)
{
  CAMLparam1(vimage);
  CAMLreturn(Val_int(Image_val(vimage)->image->depth));
}

CAMLprim value ocaml_image_data_order(value vimage)
{
  CAMLparam1(vimage);
  CAMLreturn(Val_int(Image_val(vimage)->image->dataOrder));
}

/* find contours */
/* TODO
CAMLprim value ocaml_find_contour()
{
    CvMemStorage* stor;
    CvSeq* cont;

}
*/
