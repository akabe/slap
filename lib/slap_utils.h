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

#ifndef SLAP_UTILS_H
#define SLAP_UTILS_H

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

typedef char byte;

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define Val_none Val_int(0)
#define Some_val(v) Field(v,0)

#define SLAP_BA_KIND(v_ba) (Bigarray_val(v_ba)->flags & CAML_BA_KIND_MASK)

#define SLAP_BA_ELEMENT_SIZE(v_ba) (slap_ba_element_size[SLAP_BA_KIND(v_ba)])

#define SLAP_BA_LD(v_ba) (Bigarray_val(v_ba)->dim[0])

#define SLAP_BA_DATA(v_ba) ((byte *) Bigarray_val(v_ba)->data)

#define SLAP_BA_MAT_DATA(v_a, v_ar, v_ac)                               \
  (SLAP_BA_DATA(v_a) + (SLAP_BA_LD(v_a) * (Int_val(v_ac) - 1)           \
                        + (Int_val(v_ar) - 1)) * SLAP_BA_ELEMENT_SIZE(v_a))

extern const int * const slap_ba_element_size;

#endif /* SLAP_UTILS_H */
