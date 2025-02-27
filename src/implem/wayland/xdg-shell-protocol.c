#ifdef HAS_WAYLAND

/* Generated by wayland-scanner 1.16.0 */

/*
 * Copyright © 2008-2013 Kristian Høgsberg
 * Copyright © 2013      Rafael Antognolli
 * Copyright © 2013      Jasper St. Pierre
 * Copyright © 2010-2013 Intel Corporation
 * Copyright © 2015-2017 Samsung Electronics Co., Ltd
 * Copyright © 2015-2017 Red Hat Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <stdlib.h>
#include <stdint.h>
#include "wayland-util.h"

#ifndef __has_attribute
# define __has_attribute(x) 0  /* Compatibility with non-clang compilers. */
#endif

#if (__has_attribute(visibility) || defined(__GNUC__) && __GNUC__ >= 4)
#define WL_PRIVATE __attribute__ ((visibility("hidden")))
#else
#define WL_PRIVATE
#endif

extern const struct wl_interface wl_output_interface;
extern const struct wl_interface wl_seat_interface;
extern const struct wl_interface wl_surface_interface;
extern const struct wl_interface xdg_popup_interface;
extern const struct wl_interface xdg_positioner_interface;
extern const struct wl_interface xdg_surface_interface;
extern const struct wl_interface xdg_toplevel_interface;

static const struct wl_interface *types[] = {
	NULL,
	NULL,
	NULL,
	NULL,
	&xdg_positioner_interface,
	&xdg_surface_interface,
	&wl_surface_interface,
	&xdg_toplevel_interface,
	&xdg_popup_interface,
	&xdg_surface_interface,
	&xdg_positioner_interface,
	&xdg_toplevel_interface,
	&wl_seat_interface,
	NULL,
	NULL,
	NULL,
	&wl_seat_interface,
	NULL,
	&wl_seat_interface,
	NULL,
	NULL,
	&wl_output_interface,
	&wl_seat_interface,
	NULL,
};

static const struct wl_message xdg_wm_base_requests[] = {
	{ "destroy", "", types + 0 },
	{ "create_positioner", "n", types + 4 },
	{ "get_xdg_surface", "no", types + 5 },
	{ "pong", "u", types + 0 },
};

static const struct wl_message xdg_wm_base_events[] = {
	{ "ping", "u", types + 0 },
};

WL_PRIVATE const struct wl_interface xdg_wm_base_interface = {
	"xdg_wm_base", 1,
	4, xdg_wm_base_requests,
	1, xdg_wm_base_events,
};

static const struct wl_message xdg_positioner_requests[] = {
	{ "destroy", "", types + 0 },
	{ "set_size", "ii", types + 0 },
	{ "set_anchor_rect", "iiii", types + 0 },
	{ "set_anchor", "u", types + 0 },
	{ "set_gravity", "u", types + 0 },
	{ "set_constraint_adjustment", "u", types + 0 },
	{ "set_offset", "ii", types + 0 },
};

WL_PRIVATE const struct wl_interface xdg_positioner_interface = {
	"xdg_positioner", 1,
	7, xdg_positioner_requests,
	0, NULL,
};

static const struct wl_message xdg_surface_requests[] = {
	{ "destroy", "", types + 0 },
	{ "get_toplevel", "n", types + 7 },
	{ "get_popup", "n?oo", types + 8 },
	{ "set_window_geometry", "iiii", types + 0 },
	{ "ack_configure", "u", types + 0 },
};

static const struct wl_message xdg_surface_events[] = {
	{ "configure", "u", types + 0 },
};

WL_PRIVATE const struct wl_interface xdg_surface_interface = {
	"xdg_surface", 1,
	5, xdg_surface_requests,
	1, xdg_surface_events,
};

static const struct wl_message xdg_toplevel_requests[] = {
	{ "destroy", "", types + 0 },
	{ "set_parent", "?o", types + 11 },
	{ "set_title", "s", types + 0 },
	{ "set_app_id", "s", types + 0 },
	{ "show_window_menu", "ouii", types + 12 },
	{ "move", "ou", types + 16 },
	{ "resize", "ouu", types + 18 },
	{ "set_max_size", "ii", types + 0 },
	{ "set_min_size", "ii", types + 0 },
	{ "set_maximized", "", types + 0 },
	{ "unset_maximized", "", types + 0 },
	{ "set_fullscreen", "?o", types + 21 },
	{ "unset_fullscreen", "", types + 0 },
	{ "set_minimized", "", types + 0 },
};

static const struct wl_message xdg_toplevel_events[] = {
	{ "configure", "iia", types + 0 },
	{ "close", "", types + 0 },
};

WL_PRIVATE const struct wl_interface xdg_toplevel_interface = {
	"xdg_toplevel", 1,
	14, xdg_toplevel_requests,
	2, xdg_toplevel_events,
};

static const struct wl_message xdg_popup_requests[] = {
	{ "destroy", "", types + 0 },
	{ "grab", "ou", types + 22 },
};

static const struct wl_message xdg_popup_events[] = {
	{ "configure", "iiii", types + 0 },
	{ "popup_done", "", types + 0 },
};

WL_PRIVATE const struct wl_interface xdg_popup_interface = {
	"xdg_popup", 1,
	2, xdg_popup_requests,
	2, xdg_popup_events,
};

#else

const int xdg_shell_protocol = 0;

#endif /* HAS_WAYLAND */
