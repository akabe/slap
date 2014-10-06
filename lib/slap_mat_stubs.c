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

#define FILL(type, init_value) {                     \
    type init = init_value;                          \
    type *p = a;                                     \
    for (j = 0; j < n; ++j, p += lda)                \
      for (i = 0; i < m; ++i) p[i] = init;           \
  }

void
slap_mat_fill (int m, int n, enum caml_ba_kind kind,
               void *a, int lda,
               value v_init)
{
  int i, j;

  switch (kind)
  {
  case CAML_BA_FLOAT32: FILL(float, Double_val(v_init)); break;
  case CAML_BA_FLOAT64: FILL(double, Double_val(v_init)); break;
  case CAML_BA_CHAR:
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
  default: caml_failwith("Slap.Mat.fill: Unsupported kind");
  }

  return;
}

#undef FILL

CAMLprim value
slap_mat_fill_stub (value v_m, value v_n,
                    value v_ar, value v_ac, value v_a,
                    value v_init)
{
  slap_mat_fill(Int_val(v_m), Int_val(v_n),
                SLAP_BA_KIND(v_a),
                SLAP_BA_MAT_DATA(v_a, v_ar, v_ac),
                SLAP_BA_LD(v_a),
                v_init);

  return Val_unit;
}

CAMLprim value
slap_mat_fill_stub_bc (value * argv, int argn)
{
  return slap_mat_fill_stub(argv[0], argv[1], argv[2], argv[3], argv[4],
                            argv[5]);
}

void slacpy_ (char *uplo, int *m, int *n,
              float *a, int *lda, float *b, int *ldb);
void dlacpy_ (char *uplo, int *m, int *n,
              double *a, int *lda, double *b, int *ldb);
void clacpy_ (char *uplo, int *m, int *n,
              float *a, int *lda, float *b, int *ldb);
void zlacpy_ (char *uplo, int *m, int *n,
              double *a, int *lda, double *b, int *ldb);

CAMLprim value
slap_mat_copy_stub (value v_m, value v_n,
                    value v_ar, value v_ac, value v_a,
                    value v_br, value v_bc, value v_b)
{
  int m = Int_val(v_m);
  int n = Int_val(v_n);
  void *a = SLAP_BA_MAT_DATA(v_a, v_ar, v_ac);
  void *b = SLAP_BA_MAT_DATA(v_b, v_br, v_bc);
  int lda = SLAP_BA_LD(v_a);
  int ldb = SLAP_BA_LD(v_b);

  switch (SLAP_BA_KIND(v_a))
  {
  case CAML_BA_FLOAT32: slacpy_("*", &m, &n, a, &lda, b, &ldb); break;
  case CAML_BA_FLOAT64: dlacpy_("*", &m, &n, a, &lda, b, &ldb); break;
  case CAML_BA_COMPLEX32: clacpy_("*", &m, &n, a, &lda, b, &ldb); break;
  case CAML_BA_COMPLEX64: zlacpy_("*", &m, &n, a, &lda, b, &ldb); break;
  default: {
    int elm_size = SLAP_BA_ELEMENT_SIZE(v_a);
    int len = m * elm_size;
    int j;
    lda *= elm_size;
    ldb *= elm_size;
    for (j = 0; j < n; ++j) {
      memcpy(b, a, len);
      a = (byte *) a + lda;
      b = (byte *) b + ldb;
    }
  }
  }

  return Val_unit;
}

CAMLprim value
slap_mat_copy_stub_bc (value * argv, int argn)
{
  return slap_mat_copy_stub(argv[0], argv[1], argv[2], argv[3], argv[4],
                            argv[5], argv[6], argv[7]);
}

CAMLprim value
slap_mat_packed_stub (value v_n, value v_up, value v_x,
                      value v_ar, value v_ac, value v_a)
{
  int n = Int_val(v_n);
  byte *xdata = SLAP_BA_DATA(v_x);
  char *adata = SLAP_BA_MAT_DATA(v_a, v_ar, v_ac);
  int elm_size = SLAP_BA_ELEMENT_SIZE(v_x);
  int lda = SLAP_BA_LD(v_a) * elm_size;
  int j;

  if (Bool_val(v_up))
  {
    int askip = lda;
    for (j = 0; j < n; ++j)
    {
      int len = (j + 1) * elm_size;
      memcpy(xdata, adata, len);
      xdata += len;
      adata += askip;
    }
  }
  else
  {
    int askip = lda + 1 * elm_size;
    for (j = 0; j < n; ++j)
    {
      int len = (n - j) * elm_size;
      memcpy(xdata, adata, len);
      xdata += len;
      adata += askip;
    }
  }

  return Val_unit;
}

CAMLprim value
slap_mat_packed_stub_bc (value * argv, int argn)
{
  return slap_mat_packed_stub(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}

CAMLprim value
slap_mat_unpacked_stub (value v_n, value v_up, value v_fill_num, value v_x,
                        value v_ar, value v_ac, value v_a)
{
  int n = Int_val(v_n);
  byte *xdata = SLAP_BA_DATA(v_x);
  char *adata = SLAP_BA_MAT_DATA(v_a, v_ar, v_ac);
  int elm_size = SLAP_BA_ELEMENT_SIZE(v_x);
  int lda = SLAP_BA_LD(v_a);
  int j;

  if (v_fill_num != Val_none)
  {
    slap_mat_fill(n, n, SLAP_BA_KIND(v_a), adata, lda, Some_val(v_fill_num));
  }

  if (Bool_val(v_up))
  {
    int askip = lda * elm_size;
    for (j = 0; j < n; ++j)
    {
      int len = (j + 1) * elm_size;
      memcpy(adata, xdata, len);
      xdata += len;
      adata += askip;
    }
  }
  else
  {
    int askip = (lda + 1) * elm_size;
    for (j = 0; j < n; ++j)
    {
      int len = (n - j) * elm_size;
      memcpy(adata, xdata, len);
      xdata += len;
      adata += askip;
    }
  }

  return Val_unit;
}

CAMLprim value
slap_mat_unpacked_stub_bc (value * argv, int argn)
{
  return slap_mat_unpacked_stub(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6]);
}

CAMLprim value
slap_mat_geband_stub (value v_m, value v_n,
                      value v_kl, value v_ku,
                      value v_ar, value v_ac, value v_a,
                      value v_br, value v_bc, value v_b)
{
  int m = Int_val(v_m);
  int n = Int_val(v_n);
  int kl = Int_val(v_kl);
  int ku = Int_val(v_ku);
  char *adata = SLAP_BA_MAT_DATA(v_a, v_ar, v_ac);
  char *bdata = SLAP_BA_MAT_DATA(v_b, v_br, v_bc);
  int elm_size = SLAP_BA_ELEMENT_SIZE(v_a);
  int lda = SLAP_BA_LD(v_a) * elm_size;
  int ldb = SLAP_BA_LD(v_b) * elm_size;
  int start_a, start_b, len, j;

  for (j = 0; j < n; ++j)
  {
    if (j <= ku) start_b = (ku - j) * elm_size, start_a = 0;
    else         start_a = (j - ku) * elm_size, start_b = 0;

    len = MIN(m, j + kl + 1) * elm_size - start_a;
    if (len <= 0) break;

    memcpy(&bdata[start_b], &adata[start_a], len);

    adata += lda;
    bdata += ldb;
  }

  return Val_unit;
}

CAMLprim value
slap_mat_geband_stub_bc (value * argv, int argn)
{
  return slap_mat_geband_stub(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5], argv[6], argv[7], argv[8], argv[9]);
}

CAMLprim value
slap_mat_ungeband_stub (value v_m, value v_n,
                        value v_kl, value v_ku,
                        value v_fill_num,
                        value v_ar, value v_ac, value v_a,
                        value v_br, value v_bc, value v_b)
{
  int m = Int_val(v_m);
  int n = Int_val(v_n);
  int kl = Int_val(v_kl);
  int ku = Int_val(v_ku);
  char *adata = SLAP_BA_MAT_DATA(v_a, v_ar, v_ac);
  char *bdata = SLAP_BA_MAT_DATA(v_b, v_br, v_bc);
  int elm_size = SLAP_BA_ELEMENT_SIZE(v_a);
  int lda = SLAP_BA_LD(v_a);
  int ldb = SLAP_BA_LD(v_b);
  int start_a, start_b, len, j;

  if (v_fill_num != Val_none)
  {
    slap_mat_fill(m, n, SLAP_BA_KIND(v_a), adata, lda, Some_val(v_fill_num));
  }

  lda *= elm_size;
  ldb *= elm_size;

  for (j = 0; j < n; ++j)
  {
    if (j <= ku) start_b = (ku - j) * elm_size, start_a = 0;
    else         start_a = (j - ku) * elm_size, start_b = 0;

    len = MIN(m, j + kl + 1) * elm_size - start_a;
    if (len <= 0) break;

    memcpy(&adata[start_a], &bdata[start_b], len);

    adata += lda;
    bdata += ldb;
  }

  return Val_unit;
}

CAMLprim value
slap_mat_ungeband_stub_bc (value * argv, int argn)
{
  return slap_mat_ungeband_stub(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6], argv[7], argv[8], argv[9],
                                argv[10]);
}
