/* Sized Linear Algebra Package (SLAP)

   Copyright (C) 2013- Akinori ABE <abe@kb.ecei.tohoku.ac.jp>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*/

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "slap_utils.h"

extern void scopy_ (int *n, float *x, int *incx, float *y, int *incy);
extern void dcopy_ (int *n, double *x, int *incx, double *y, int *incy);
extern void ccopy_ (int *n, float *x, int *incx, float *y, int *incy);
extern void zcopy_ (int *n, double *x, int *incx, double *y, int *incy);

#define COPY(type) do {                                                 \
    type *x = (type *) xdata, *y = (type *) ydata;                      \
    int i;                                                              \
    for (i = 0; i < n; ++i, x += incx, y += incy) *y = *x;              \
  } while(0)

void
slap_vec_copy (int n, enum caml_ba_kind kind,
               void * xdata, int incx,
               void * ydata, int incy)
{
  switch (kind)
  {
  case CAML_BA_FLOAT32: scopy_(&n, xdata, &incx, ydata, &incy); break;
  case CAML_BA_FLOAT64: dcopy_(&n, xdata, &incx, ydata, &incy); break;
  case CAML_BA_COMPLEX32: ccopy_(&n, xdata, &incx, ydata, &incy); break;
  case CAML_BA_COMPLEX64: zcopy_(&n, xdata, &incx, ydata, &incy); break;
#if HAVE_CAML_BA_CHAR
  case CAML_BA_CHAR:
#endif
  case CAML_BA_SINT8:
  case CAML_BA_UINT8: COPY(int8_t); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16: COPY(int16_t); break;
  case CAML_BA_INT32: COPY(int32_t); break;
  case CAML_BA_INT64: COPY(int64_t); break;
  case CAML_BA_NATIVE_INT: COPY(intnat); break;
  case CAML_BA_CAML_INT: COPY(intnat); break;
  default: caml_failwith("Slap.Vec.copy: Unsupported kind");
  }

  return;
}

#undef COPY

CAMLprim value
slap_vec_copy_stub (value v_n,
                    value v_ofsx, value v_incx, value v_x,
                    value v_ofsy, value v_incy, value v_y)
{
  slap_vec_copy(Int_val(v_n), SLAP_BA_KIND(v_x),
                SLAP_BA_VEC_DATA(v_x, v_ofsx), Int_val(v_incx),
                SLAP_BA_VEC_DATA(v_y, v_ofsy), Int_val(v_incy));

  return Val_unit;
}

CAMLprim value
slap_vec_copy_stub_bc (value * argv, int argn)
{
  return slap_vec_copy_stub(argv[0], argv[1], argv[2], argv[3], argv[4],
                            argv[5], argv[6]);
}

#define FILL(type, init_value) {                     \
    type init = init_value;                          \
    type *p = xdata;                                 \
    int i;                                           \
    for (i = 0; i < n; ++i, p += incx) *p = init;    \
  }

void
slap_vec_fill (int n, enum caml_ba_kind kind,
               void *xdata, int incx,
               value v_init)
{
  switch (kind)
  {
  case CAML_BA_FLOAT32: FILL(float, Double_val(v_init)); break;
  case CAML_BA_FLOAT64: FILL(double, Double_val(v_init)); break;
#if HAVE_CAML_BA_CHAR
  case CAML_BA_CHAR:
#endif
  case CAML_BA_SINT8:
  case CAML_BA_UINT8: FILL(unsigned char, Int_val(v_init)); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16: FILL(int16_t, Int_val(v_init)); break;
  case CAML_BA_INT32: FILL(int32_t, Int32_val(v_init)); break;
  case CAML_BA_INT64: FILL(int64_t, Int64_val(v_init)); break;
  case CAML_BA_NATIVE_INT: FILL(intnat, Nativeint_val(v_init)); break;
  case CAML_BA_CAML_INT: FILL(intnat, Long_val(v_init)); break;
  case CAML_BA_COMPLEX32: FILL(complex32_t, Complex_val(v_init)); break;
  case CAML_BA_COMPLEX64: FILL(complex64_t, Complex_val(v_init)); break;
  default: caml_failwith("Slap.Vec.fill: Unsupported kind");
  }

  return;
}

#undef FILL

CAMLprim value
slap_vec_fill_stub (value v_n,
                    value v_ofsx, value v_incx, value v_x,
                    value v_init)
{
  slap_vec_fill(Int_val(v_n),
                SLAP_BA_KIND(v_x),
                SLAP_BA_VEC_DATA(v_x, v_ofsx),
                Int_val(v_incx),
                v_init);

  return Val_unit;
}

CAMLprim value
slap_vec_fill_stub_bc (value * argv, int argn)
{
  return slap_vec_fill_stub(argv[0], argv[1], argv[2], argv[3], argv[4]);
}
