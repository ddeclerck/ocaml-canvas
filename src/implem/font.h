/**************************************************************************/
/*                                                                        */
/*    Copyright 2022 OCamlPro                                             */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#ifndef __FONT_H
#define __FONT_H

#include <stdbool.h>

#include "point.h"
#include "rect.h"
#include "polygon.h"
#include "transform.h"
#include "font_desc.h"

typedef struct font_t font_t;

font_t *
font_create(
  font_desc_t *fd);

void
font_destroy(
  font_t *f);

bool
font_matches(
  const font_t *f,
  const font_desc_t *fd);

bool
font_char_as_poly(
  const font_t *f,
  const transform_t *t,
  uint32_t c,
  point_t *pen, // in/out
  polygon_t *p, // out
  rect_t *bbox); // out

bool
font_char_as_poly_outline(
  const font_t *f,
  const transform_t *t,
  uint32_t c,
  double w,
  point_t *pen, // in/out
  polygon_t *p, // out
  rect_t *bbox); // out

#endif /* __FONT_H */
