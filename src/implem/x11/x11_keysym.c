/**************************************************************************/
/*                                                                        */
/*    Copyright 2022 OCamlPro                                             */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#ifdef HAS_X11

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <xcb/xcb.h>

#include "x11_backend_internal.h"

/* Unicode keysyms */

#define is_unicode_keysym(ks) (((ks) & 0xFF000000) == 0x01000000)

#define get_unicode(ks) ((ks) & 0x00FFFFFF)

/* Regular keysyms */

#define N_REGULAR  0x20AD

#define is_regular_keysym(ks) (((ks) <= N_REGULAR))

#define get_regular(ks) (regular_map[(ks)])

/* Note: using uint16_t to save space, but these are really uint32_t */
static const uint16_t regular_map[N_REGULAR] = {

/* Latin 1 */
  [0x0020] = 0x0020,
  [0x0021] = 0x0021,
  [0x0022] = 0x0022,
  [0x0023] = 0x0023,
  [0x0024] = 0x0024,
  [0x0025] = 0x0025,
  [0x0026] = 0x0026,
  [0x0027] = 0x0027,
  [0x0028] = 0x0028,
  [0x0029] = 0x0029,
  [0x002a] = 0x002A,
  [0x002b] = 0x002B,
  [0x002c] = 0x002C,
  [0x002d] = 0x002D,
  [0x002e] = 0x002E,
  [0x002f] = 0x002F,
  [0x0030] = 0x0030,
  [0x0031] = 0x0031,
  [0x0032] = 0x0032,
  [0x0033] = 0x0033,
  [0x0034] = 0x0034,
  [0x0035] = 0x0035,
  [0x0036] = 0x0036,
  [0x0037] = 0x0037,
  [0x0038] = 0x0038,
  [0x0039] = 0x0039,
  [0x003a] = 0x003A,
  [0x003b] = 0x003B,
  [0x003c] = 0x003C,
  [0x003d] = 0x003D,
  [0x003e] = 0x003E,
  [0x003f] = 0x003F,
  [0x0040] = 0x0040,
  [0x0041] = 0x0041,
  [0x0042] = 0x0042,
  [0x0043] = 0x0043,
  [0x0044] = 0x0044,
  [0x0045] = 0x0045,
  [0x0046] = 0x0046,
  [0x0047] = 0x0047,
  [0x0048] = 0x0048,
  [0x0049] = 0x0049,
  [0x004a] = 0x004A,
  [0x004b] = 0x004B,
  [0x004c] = 0x004C,
  [0x004d] = 0x004D,
  [0x004e] = 0x004E,
  [0x004f] = 0x004F,
  [0x0050] = 0x0050,
  [0x0051] = 0x0051,
  [0x0052] = 0x0052,
  [0x0053] = 0x0053,
  [0x0054] = 0x0054,
  [0x0055] = 0x0055,
  [0x0056] = 0x0056,
  [0x0057] = 0x0057,
  [0x0058] = 0x0058,
  [0x0059] = 0x0059,
  [0x005a] = 0x005A,
  [0x005b] = 0x005B,
  [0x005c] = 0x005C,
  [0x005d] = 0x005D,
  [0x005e] = 0x005E,
  [0x005f] = 0x005F,
  [0x0060] = 0x0060,
  [0x0061] = 0x0061,
  [0x0062] = 0x0062,
  [0x0063] = 0x0063,
  [0x0064] = 0x0064,
  [0x0065] = 0x0065,
  [0x0066] = 0x0066,
  [0x0067] = 0x0067,
  [0x0068] = 0x0068,
  [0x0069] = 0x0069,
  [0x006a] = 0x006A,
  [0x006b] = 0x006B,
  [0x006c] = 0x006C,
  [0x006d] = 0x006D,
  [0x006e] = 0x006E,
  [0x006f] = 0x006F,
  [0x0070] = 0x0070,
  [0x0071] = 0x0071,
  [0x0072] = 0x0072,
  [0x0073] = 0x0073,
  [0x0074] = 0x0074,
  [0x0075] = 0x0075,
  [0x0076] = 0x0076,
  [0x0077] = 0x0077,
  [0x0078] = 0x0078,
  [0x0079] = 0x0079,
  [0x007a] = 0x007A,
  [0x007b] = 0x007B,
  [0x007c] = 0x007C,
  [0x007d] = 0x007D,
  [0x007e] = 0x007E,
  [0x00a0] = 0x00A0,
  [0x00a1] = 0x00A1,
  [0x00a2] = 0x00A2,
  [0x00a3] = 0x00A3,
  [0x00a4] = 0x00A4,
  [0x00a5] = 0x00A5,
  [0x00a6] = 0x00A6,
  [0x00a7] = 0x00A7,
  [0x00a8] = 0x00A8,
  [0x00a9] = 0x00A9,
  [0x00aa] = 0x00AA,
  [0x00ab] = 0x00AB,
  [0x00ac] = 0x00AC,
  [0x00ad] = 0x00AD,
  [0x00ae] = 0x00AE,
  [0x00af] = 0x00AF,
  [0x00b0] = 0x00B0,
  [0x00b1] = 0x00B1,
  [0x00b2] = 0x00B2,
  [0x00b3] = 0x00B3,
  [0x00b4] = 0x00B4,
  [0x00b5] = 0x00B5,
  [0x00b6] = 0x00B6,
  [0x00b7] = 0x00B7,
  [0x00b8] = 0x00B8,
  [0x00b9] = 0x00B9,
  [0x00ba] = 0x00BA,
  [0x00bb] = 0x00BB,
  [0x00bc] = 0x00BC,
  [0x00bd] = 0x00BD,
  [0x00be] = 0x00BE,
  [0x00bf] = 0x00BF,
  [0x00c0] = 0x00C0,
  [0x00c1] = 0x00C1,
  [0x00c2] = 0x00C2,
  [0x00c3] = 0x00C3,
  [0x00c4] = 0x00C4,
  [0x00c5] = 0x00C5,
  [0x00c6] = 0x00C6,
  [0x00c7] = 0x00C7,
  [0x00c8] = 0x00C8,
  [0x00c9] = 0x00C9,
  [0x00ca] = 0x00CA,
  [0x00cb] = 0x00CB,
  [0x00cc] = 0x00CC,
  [0x00cd] = 0x00CD,
  [0x00ce] = 0x00CE,
  [0x00cf] = 0x00CF,
  [0x00d0] = 0x00D0,
  [0x00d1] = 0x00D1,
  [0x00d2] = 0x00D2,
  [0x00d3] = 0x00D3,
  [0x00d4] = 0x00D4,
  [0x00d5] = 0x00D5,
  [0x00d6] = 0x00D6,
  [0x00d7] = 0x00D7,
  [0x00d8] = 0x00D8,
//  [0x00d8] = 0x00D8,
  [0x00d9] = 0x00D9,
  [0x00da] = 0x00DA,
  [0x00db] = 0x00DB,
  [0x00dc] = 0x00DC,
  [0x00dd] = 0x00DD,
  [0x00de] = 0x00DE,
  [0x00df] = 0x00DF,
  [0x00e0] = 0x00E0,
  [0x00e1] = 0x00E1,
  [0x00e2] = 0x00E2,
  [0x00e3] = 0x00E3,
  [0x00e4] = 0x00E4,
  [0x00e5] = 0x00E5,
  [0x00e6] = 0x00E6,
  [0x00e7] = 0x00E7,
  [0x00e8] = 0x00E8,
  [0x00e9] = 0x00E9,
  [0x00ea] = 0x00EA,
  [0x00eb] = 0x00EB,
  [0x00ec] = 0x00EC,
  [0x00ed] = 0x00ED,
  [0x00ee] = 0x00EE,
  [0x00ef] = 0x00EF,
  [0x00f0] = 0x00F0,
  [0x00f1] = 0x00F1,
  [0x00f2] = 0x00F2,
  [0x00f3] = 0x00F3,
  [0x00f4] = 0x00F4,
  [0x00f5] = 0x00F5,
  [0x00f6] = 0x00F6,
  [0x00f7] = 0x00F7,
  [0x00f8] = 0x00F8,
//  [0x00f8] = 0x00F8,
  [0x00f9] = 0x00F9,
  [0x00fa] = 0x00FA,
  [0x00fb] = 0x00FB,
  [0x00fc] = 0x00FC,
  [0x00fd] = 0x00FD,
  [0x00fe] = 0x00FE,
  [0x00ff] = 0x00FF,

/* Latin 2 */
  [0x01a1] = 0x0104,
  [0x01a2] = 0x02D8,
  [0x01a3] = 0x0141,
  [0x01a5] = 0x013D,
  [0x01a6] = 0x015A,
  [0x01a9] = 0x0160,
  [0x01aa] = 0x015E,
  [0x01ab] = 0x0164,
  [0x01ac] = 0x0179,
  [0x01ae] = 0x017D,
  [0x01af] = 0x017B,
  [0x01b1] = 0x0105,
  [0x01b2] = 0x02DB,
  [0x01b3] = 0x0142,
  [0x01b5] = 0x013E,
  [0x01b6] = 0x015B,
  [0x01b7] = 0x02C7,
  [0x01b9] = 0x0161,
  [0x01ba] = 0x015F,
  [0x01bb] = 0x0165,
  [0x01bc] = 0x017A,
  [0x01bd] = 0x02DD,
  [0x01be] = 0x017E,
  [0x01bf] = 0x017C,
  [0x01c0] = 0x0154,
  [0x01c3] = 0x0102,
  [0x01c5] = 0x0139,
  [0x01c6] = 0x0106,
  [0x01c8] = 0x010C,
  [0x01ca] = 0x0118,
  [0x01cc] = 0x011A,
  [0x01cf] = 0x010E,
  [0x01d0] = 0x0110,
  [0x01d1] = 0x0143,
  [0x01d2] = 0x0147,
  [0x01d5] = 0x0150,
  [0x01d8] = 0x0158,
  [0x01d9] = 0x016E,
  [0x01db] = 0x0170,
  [0x01de] = 0x0162,
  [0x01e0] = 0x0155,
  [0x01e3] = 0x0103,
  [0x01e5] = 0x013A,
  [0x01e6] = 0x0107,
  [0x01e8] = 0x010D,
  [0x01ea] = 0x0119,
  [0x01ec] = 0x011B,
  [0x01ef] = 0x010F,
  [0x01f0] = 0x0111,
  [0x01f1] = 0x0144,
  [0x01f2] = 0x0148,
  [0x01f5] = 0x0151,
  [0x01f8] = 0x0159,
  [0x01f9] = 0x016F,
  [0x01fb] = 0x0171,
  [0x01fe] = 0x0163,
  [0x01ff] = 0x02D9,

/* Latin 3 */
  [0x02a1] = 0x0126,
  [0x02a6] = 0x0124,
  [0x02a9] = 0x0130,
  [0x02ab] = 0x011E,
  [0x02ac] = 0x0134,
  [0x02b1] = 0x0127,
  [0x02b6] = 0x0125,
  [0x02b9] = 0x0131,
  [0x02bb] = 0x011F,
  [0x02bc] = 0x0135,
  [0x02c5] = 0x010A,
  [0x02c6] = 0x0108,
  [0x02d5] = 0x0120,
  [0x02d8] = 0x011C,
  [0x02dd] = 0x016C,
  [0x02de] = 0x015C,
  [0x02e5] = 0x010B,
  [0x02e6] = 0x0109,
  [0x02f5] = 0x0121,
  [0x02f8] = 0x011D,
  [0x02fd] = 0x016D,
  [0x02fe] = 0x015D,

/* Latin 4 */
  [0x03a2] = 0x0138,
  [0x03a3] = 0x0156,
  [0x03a5] = 0x0128,
  [0x03a6] = 0x013B,
  [0x03aa] = 0x0112,
  [0x03ab] = 0x0122,
  [0x03ac] = 0x0166,
  [0x03b3] = 0x0157,
  [0x03b5] = 0x0129,
  [0x03b6] = 0x013C,
  [0x03ba] = 0x0113,
  [0x03bb] = 0x0123,
  [0x03bc] = 0x0167,
  [0x03bd] = 0x014A,
  [0x03bf] = 0x014B,
  [0x03c0] = 0x0100,
  [0x03c7] = 0x012E,
  [0x03cc] = 0x0116,
  [0x03cf] = 0x012A,
  [0x03d1] = 0x0145,
  [0x03d2] = 0x014C,
  [0x03d3] = 0x0136,
  [0x03d9] = 0x0172,
  [0x03dd] = 0x0168,
  [0x03de] = 0x016A,
  [0x03e0] = 0x0101,
  [0x03e7] = 0x012F,
  [0x03ec] = 0x0117,
  [0x03ef] = 0x012B,
  [0x03f1] = 0x0146,
  [0x03f2] = 0x014D,
  [0x03f3] = 0x0137,
  [0x03f9] = 0x0173,
  [0x03fd] = 0x0169,
  [0x03fe] = 0x016B,

/* Katakana */
  [0x047e] = 0x203E,
  [0x04a1] = 0x3002,
  [0x04a2] = 0x300C, // xlib: 3008
  [0x04a3] = 0x300D, // xlib: 3009
  [0x04a4] = 0x3001,
  [0x04a5] = 0x30FB,
  [0x04a6] = 0x30F2,
  [0x04a7] = 0x30A1,
  [0x04a8] = 0x30A3,
  [0x04a9] = 0x30A5,
  [0x04aa] = 0x30A7,
  [0x04ab] = 0x30A9,
  [0x04ac] = 0x30E3,
  [0x04ad] = 0x30E5,
  [0x04ae] = 0x30E7,
  [0x04af] = 0x30C3,
  [0x04b0] = 0x30FC,
  [0x04b1] = 0x30A2,
  [0x04b2] = 0x30A4,
  [0x04b3] = 0x30A6,
  [0x04b4] = 0x30A8,
  [0x04b5] = 0x30AA,
  [0x04b6] = 0x30AB,
  [0x04b7] = 0x30AD,
  [0x04b8] = 0x30AF,
  [0x04b9] = 0x30B1,
  [0x04ba] = 0x30B3,
  [0x04bb] = 0x30B5,
  [0x04bc] = 0x30B7,
  [0x04bd] = 0x30B9,
  [0x04be] = 0x30BB,
  [0x04bf] = 0x30BD,
  [0x04c0] = 0x30BF,
  [0x04c1] = 0x30C1,
  [0x04c2] = 0x30C4,
  [0x04c3] = 0x30C6,
  [0x04c4] = 0x30C8,
  [0x04c5] = 0x30CA,
  [0x04c6] = 0x30CB,
  [0x04c7] = 0x30CC,
  [0x04c8] = 0x30CD,
  [0x04c9] = 0x30CE,
  [0x04ca] = 0x30CF,
  [0x04cb] = 0x30D2,
  [0x04cc] = 0x30D5,
  [0x04cd] = 0x30D8,
  [0x04ce] = 0x30DB,
  [0x04cf] = 0x30DE,
  [0x04d0] = 0x30DF,
  [0x04d1] = 0x30E0,
  [0x04d2] = 0x30E1,
  [0x04d3] = 0x30E2,
  [0x04d4] = 0x30E4,
  [0x04d5] = 0x30E6,
  [0x04d6] = 0x30E8,
  [0x04d7] = 0x30E9,
  [0x04d8] = 0x30EA,
  [0x04d9] = 0x30EB,
  [0x04da] = 0x30EC,
  [0x04db] = 0x30ED,
  [0x04dc] = 0x30EF,
  [0x04dd] = 0x30F3,
  [0x04de] = 0x309B,
  [0x04df] = 0x309C,

/* Arabic */
  [0x0590] = 0x06F0,
  [0x0591] = 0x06F1,
  [0x0592] = 0x06F2,
  [0x0593] = 0x06F3,
  [0x0594] = 0x06F4,
  [0x0595] = 0x06F5,
  [0x0596] = 0x06F6,
  [0x0597] = 0x06F7,
  [0x0598] = 0x06F8,
  [0x0599] = 0x06F9,
  [0x05a5] = 0x066A,
  [0x05a6] = 0x0670,
  [0x05a7] = 0x0679,
  [0x05a8] = 0x067E,
  [0x05a9] = 0x0686,
  [0x05aa] = 0x0688,
  [0x05ab] = 0x0691,
  [0x05ac] = 0x060C,
  [0x05ae] = 0x06D4,
  [0x05b0] = 0x0660,
  [0x05b1] = 0x0661,
  [0x05b2] = 0x0662,
  [0x05b3] = 0x0663,
  [0x05b4] = 0x0664,
  [0x05b5] = 0x0665,
  [0x05b6] = 0x0666,
  [0x05b7] = 0x0667,
  [0x05b8] = 0x0668,
  [0x05b9] = 0x0669,
  [0x05bb] = 0x061B,
  [0x05bf] = 0x061F,
  [0x05c1] = 0x0621,
  [0x05c2] = 0x0622,
  [0x05c3] = 0x0623,
  [0x05c4] = 0x0624,
  [0x05c5] = 0x0625,
  [0x05c6] = 0x0626,
  [0x05c7] = 0x0627,
  [0x05c8] = 0x0628,
  [0x05c9] = 0x0629,
  [0x05ca] = 0x062A,
  [0x05cb] = 0x062B,
  [0x05cc] = 0x062C,
  [0x05cd] = 0x062D,
  [0x05ce] = 0x062E,
  [0x05cf] = 0x062F,
  [0x05d0] = 0x0630,
  [0x05d1] = 0x0631,
  [0x05d2] = 0x0632,
  [0x05d3] = 0x0633,
  [0x05d4] = 0x0634,
  [0x05d5] = 0x0635,
  [0x05d6] = 0x0636,
  [0x05d7] = 0x0637,
  [0x05d8] = 0x0638,
  [0x05d9] = 0x0639,
  [0x05da] = 0x063A,
  [0x05e0] = 0x0640,
  [0x05e1] = 0x0641,
  [0x05e2] = 0x0642,
  [0x05e3] = 0x0643,
  [0x05e4] = 0x0644,
  [0x05e5] = 0x0645,
  [0x05e6] = 0x0646,
  [0x05e7] = 0x0647,
  [0x05e8] = 0x0648,
  [0x05e9] = 0x0649,
  [0x05ea] = 0x064A,
  [0x05eb] = 0x064B,
  [0x05ec] = 0x064C,
  [0x05ed] = 0x064D,
  [0x05ee] = 0x064E,
  [0x05ef] = 0x064F,
  [0x05f0] = 0x0650,
  [0x05f1] = 0x0651,
  [0x05f2] = 0x0652,
  [0x05f3] = 0x0653,
  [0x05f4] = 0x0654,
  [0x05f5] = 0x0655,
  [0x05f6] = 0x0698,
  [0x05f7] = 0x06A4,
  [0x05f8] = 0x06A9,
  [0x05f9] = 0x06AF,
  [0x05fa] = 0x06BA,
  [0x05fb] = 0x06BE,
  [0x05fc] = 0x06CC,
  [0x05fd] = 0x06D2,
  [0x05fe] = 0x06C1,

/* Cyrillic */
  [0x0680] = 0x0492,
  [0x0681] = 0x0496,
  [0x0682] = 0x049A,
  [0x0683] = 0x049C,
  [0x0684] = 0x04A2,
  [0x0685] = 0x04AE,
  [0x0686] = 0x04B0,
  [0x0687] = 0x04B2,
  [0x0688] = 0x04B6,
  [0x0689] = 0x04B8,
  [0x068a] = 0x04BA,
  [0x068c] = 0x04D8,
  [0x068d] = 0x04E2,
  [0x068e] = 0x04E8,
  [0x068f] = 0x04EE,
  [0x0690] = 0x0493,
  [0x0691] = 0x0497,
  [0x0692] = 0x049B,
  [0x0693] = 0x049D,
  [0x0694] = 0x04A3,
  [0x0695] = 0x04AF,
  [0x0696] = 0x04B1,
  [0x0697] = 0x04B3,
  [0x0698] = 0x04B7,
  [0x0699] = 0x04B9,
  [0x069a] = 0x04BB,
  [0x069c] = 0x04D9,
  [0x069d] = 0x04E3,
  [0x069e] = 0x04E9,
  [0x069f] = 0x04EF,
  [0x06a1] = 0x0452,
  [0x06a2] = 0x0453,
  [0x06a3] = 0x0451,
  [0x06a4] = 0x0454,
  [0x06a5] = 0x0455,
  [0x06a6] = 0x0456,
  [0x06a7] = 0x0457,
  [0x06a8] = 0x0458,
  [0x06a9] = 0x0459,
  [0x06aa] = 0x045A,
  [0x06ab] = 0x045B,
  [0x06ac] = 0x045C,
  [0x06ad] = 0x0491,
  [0x06ae] = 0x045E,
  [0x06af] = 0x045F,
  [0x06b0] = 0x2116,
  [0x06b1] = 0x0402,
  [0x06b2] = 0x0403,
  [0x06b3] = 0x0401,
  [0x06b4] = 0x0404,
  [0x06b5] = 0x0405,
  [0x06b6] = 0x0406,
  [0x06b7] = 0x0407,
  [0x06b8] = 0x0408,
  [0x06b9] = 0x0409,
  [0x06ba] = 0x040A,
  [0x06bb] = 0x040B,
  [0x06bc] = 0x040C,
  [0x06bd] = 0x0490,
  [0x06be] = 0x040E,
  [0x06bf] = 0x040F,
  [0x06c0] = 0x044E,
  [0x06c1] = 0x0430,
  [0x06c2] = 0x0431,
  [0x06c3] = 0x0446,
  [0x06c4] = 0x0434,
  [0x06c5] = 0x0435,
  [0x06c6] = 0x0444,
  [0x06c7] = 0x0433,
  [0x06c8] = 0x0445,
  [0x06c9] = 0x0438,
  [0x06ca] = 0x0439,
  [0x06cb] = 0x043A,
  [0x06cc] = 0x043B,
  [0x06cd] = 0x043C,
  [0x06ce] = 0x043D,
  [0x06cf] = 0x043E,
  [0x06d0] = 0x043F,
  [0x06d1] = 0x044F,
  [0x06d2] = 0x0440,
  [0x06d3] = 0x0441,
  [0x06d4] = 0x0442,
  [0x06d5] = 0x0443,
  [0x06d6] = 0x0436,
  [0x06d7] = 0x0432,
  [0x06d8] = 0x044C,
  [0x06d9] = 0x044B,
  [0x06da] = 0x0437,
  [0x06db] = 0x0448,
  [0x06dc] = 0x044D,
  [0x06dd] = 0x0449,
  [0x06de] = 0x0447,
  [0x06df] = 0x044A,
  [0x06e0] = 0x042E,
  [0x06e1] = 0x0410,
  [0x06e2] = 0x0411,
  [0x06e3] = 0x0426,
  [0x06e4] = 0x0414,
  [0x06e5] = 0x0415,
  [0x06e6] = 0x0424,
  [0x06e7] = 0x0413,
  [0x06e8] = 0x0425,
  [0x06e9] = 0x0418,
  [0x06ea] = 0x0419,
  [0x06eb] = 0x041A,
  [0x06ec] = 0x041B,
  [0x06ed] = 0x041C,
  [0x06ee] = 0x041D,
  [0x06ef] = 0x041E,
  [0x06f0] = 0x041F,
  [0x06f1] = 0x042F,
  [0x06f2] = 0x0420,
  [0x06f3] = 0x0421,
  [0x06f4] = 0x0422,
  [0x06f5] = 0x0423,
  [0x06f6] = 0x0416,
  [0x06f7] = 0x0412,
  [0x06f8] = 0x042C,
  [0x06f9] = 0x042B,
  [0x06fa] = 0x0417,
  [0x06fb] = 0x0428,
  [0x06fc] = 0x042D,
  [0x06fd] = 0x0429,
  [0x06fe] = 0x0427,
  [0x06ff] = 0x042A,

/* Greek */
  [0x07a1] = 0x0386,
  [0x07a2] = 0x0388,
  [0x07a3] = 0x0389,
  [0x07a4] = 0x038A,
  [0x07a5] = 0x03AA,
  [0x07a7] = 0x038C,
  [0x07a8] = 0x038E,
  [0x07a9] = 0x03AB,
  [0x07ab] = 0x038F,
  [0x07ae] = 0x0385,
  [0x07af] = 0x2015,
  [0x07b1] = 0x03AC,
  [0x07b2] = 0x03AD,
  [0x07b3] = 0x03AE,
  [0x07b4] = 0x03AF,
  [0x07b5] = 0x03CA,
  [0x07b6] = 0x0390,
  [0x07b7] = 0x03CC,
  [0x07b8] = 0x03CD,
  [0x07b9] = 0x03CB,
  [0x07ba] = 0x03B0,
  [0x07bb] = 0x03CE,
  [0x07c1] = 0x0391,
  [0x07c2] = 0x0392,
  [0x07c3] = 0x0393,
  [0x07c4] = 0x0394,
  [0x07c5] = 0x0395,
  [0x07c6] = 0x0396,
  [0x07c7] = 0x0397,
  [0x07c8] = 0x0398,
  [0x07c9] = 0x0399,
  [0x07ca] = 0x039A,
  [0x07cb] = 0x039B,
  [0x07cc] = 0x039C,
  [0x07cd] = 0x039D,
  [0x07ce] = 0x039E,
  [0x07cf] = 0x039F,
  [0x07d0] = 0x03A0,
  [0x07d1] = 0x03A1,
  [0x07d2] = 0x03A3,
  [0x07d4] = 0x03A4,
  [0x07d5] = 0x03A5,
  [0x07d6] = 0x03A6,
  [0x07d7] = 0x03A7,
  [0x07d8] = 0x03A8,
  [0x07d9] = 0x03A9,
  [0x07e1] = 0x03B1,
  [0x07e2] = 0x03B2,
  [0x07e3] = 0x03B3,
  [0x07e4] = 0x03B4,
  [0x07e5] = 0x03B5,
  [0x07e6] = 0x03B6,
  [0x07e7] = 0x03B7,
  [0x07e8] = 0x03B8,
  [0x07e9] = 0x03B9,
  [0x07ea] = 0x03BA,
  [0x07eb] = 0x03BB,
  [0x07ec] = 0x03BC,
  [0x07ed] = 0x03BD,
  [0x07ee] = 0x03BE,
  [0x07ef] = 0x03BF,
  [0x07f0] = 0x03C0,
  [0x07f1] = 0x03C1,
  [0x07f2] = 0x03C3,
  [0x07f3] = 0x03C2,
  [0x07f4] = 0x03C4,
  [0x07f5] = 0x03C5,
  [0x07f6] = 0x03C6,
  [0x07f7] = 0x03C7,
  [0x07f8] = 0x03C8,
  [0x07f9] = 0x03C9,

/* Technical */
  [0x08a1] = 0x23B7,
  [0x08a2] = 0x250C,
  [0x08a3] = 0x2500,
  [0x08a4] = 0x2320,
  [0x08a5] = 0x2321,
  [0x08a6] = 0x2502,
  [0x08a7] = 0x23A1, // xlib: 231C
  [0x08a8] = 0x23A3, // xlib: 231D
  [0x08a9] = 0x23A4, // xlib: 231E
  [0x08aa] = 0x23A6, // xlib: 231F
  [0x08ab] = 0x239B,
  [0x08ac] = 0x239D,
  [0x08ad] = 0x239E,
  [0x08ae] = 0x23A0,
  [0x08af] = 0x23A8,
  [0x08b0] = 0x23AC,
  [0x08b1] = 0x0000, // No Unicode equivalent  topleftsummation
  [0x08b2] = 0x0000, // No Unicode equivalent  botleftsummation
  [0x08b3] = 0x0000, // No Unicode equivalent  topvertsummationconnector
  [0x08b4] = 0x0000, // No Unicode equivalent  botvertsummationconnector
  [0x08b5] = 0x0000, // No Unicode equivalent  toprightsummation
  [0x08b6] = 0x0000, // No Unicode equivalent  toprightsummation
  [0x08b7] = 0x0000, // No Unicode equivalent  rightmiddlesummation
  [0x08bc] = 0x2264,
  [0x08bd] = 0x2260,
  [0x08be] = 0x2265,
  [0x08bf] = 0x222B,
  [0x08c0] = 0x2234,
  [0x08c1] = 0x221D,
  [0x08c2] = 0x221E,
  [0x08c5] = 0x2207,
  [0x08c8] = 0x223C, // xlib, gtk: 2245
  [0x08c9] = 0x2243, // xlib: 2246
  [0x08cd] = 0x21D4,
  [0x08ce] = 0x21D2,
  [0x08cf] = 0x2261,
  [0x08d6] = 0x221A,
  [0x08da] = 0x2282,
  [0x08db] = 0x2283,
  [0x08dc] = 0x2229,
  [0x08dd] = 0x222A,
  [0x08de] = 0x2227,
  [0x08df] = 0x2228,
  [0x08ef] = 0x2202,
  [0x08f6] = 0x0192,
  [0x08fb] = 0x2190,
  [0x08fc] = 0x2191,
  [0x08fd] = 0x2192,
  [0x08fe] = 0x2193,

/* Special */
  [0x09df] = 0x2422,
  [0x09e0] = 0x25C6, // xlib: 2666
  [0x09e1] = 0x2592, // xlib: 25A6
  [0x09e2] = 0x2409,
  [0x09e3] = 0x240C,
  [0x09e4] = 0x240D,
  [0x09e5] = 0x240A,
  [0x09e8] = 0x2424, // xlib: 240A
  [0x09e9] = 0x240B,
  [0x09ea] = 0x2518,
  [0x09eb] = 0x2510,
  [0x09ec] = 0x250C,
  [0x09ed] = 0x2514,
  [0x09ee] = 0x253C,
  [0x09ef] = 0x23BA,
  [0x09f0] = 0x23BB,
  [0x09f1] = 0x2500,
  [0x09f2] = 0x23BC,
  [0x09f3] = 0x23BD,
  [0x09f4] = 0x251C,
  [0x09f5] = 0x2524,
  [0x09f6] = 0x2534,
  [0x09f7] = 0x252C,
  [0x09f8] = 0x2502,

/* Publishing */
  [0x0aa1] = 0x2003,
  [0x0aa2] = 0x2002,
  [0x0aa3] = 0x2004,
  [0x0aa4] = 0x2005,
  [0x0aa5] = 0x2007,
  [0x0aa6] = 0x2008,
  [0x0aa7] = 0x2009,
  [0x0aa8] = 0x200A,
  [0x0aa9] = 0x2014,
  [0x0aaa] = 0x2013,
  [0x0aac] = 0x2423,
  [0x0aae] = 0x2026,
  [0x0aaf] = 0x2025,
  [0x0ab0] = 0x2153,
  [0x0ab1] = 0x2154,
  [0x0ab2] = 0x2155,
  [0x0ab3] = 0x2156,
  [0x0ab4] = 0x2157,
  [0x0ab5] = 0x2158,
  [0x0ab6] = 0x2159,
  [0x0ab7] = 0x215A,
  [0x0ab8] = 0x2105,
  [0x0abb] = 0x2012,
  [0x0abc] = 0x27E8, // xlib: 2039, gtk: 2329
  [0x0abd] = 0x002E, // xlib: 2024
  [0x0abe] = 0x27E9, // xlib: 203A, gtk: 232A
  [0x0abf] = 0x0000, // No Unicode equivalent  marker
  [0x0ac3] = 0x215B,
  [0x0ac4] = 0x215C,
  [0x0ac5] = 0x215D,
  [0x0ac6] = 0x215E,
  [0x0ac9] = 0x2122,
  [0x0aca] = 0x2613, // xlib: 2120
  [0x0acb] = 0x0000, // No Unicode equivalent  trademarkincircle
  [0x0acc] = 0x25C1,
  [0x0acd] = 0x25B7,
  [0x0ace] = 0x25CB,
  [0x0acf] = 0x25AF, // gtk: 25A1
  [0x0ad0] = 0x2018,
  [0x0ad1] = 0x2019,
  [0x0ad2] = 0x201C,
  [0x0ad3] = 0x201D,
  [0x0ad4] = 0x211E,
  [0x0ad5] = 0x2030,
  [0x0ad6] = 0x2032,
  [0x0ad7] = 0x2033,
  [0x0ad9] = 0x271D,
  [0x0ada] = 0x0000, // No Unicode equivalent  hexagram (2721 could work)
  [0x0adb] = 0x25AC, // xlib: 220E
  [0x0adc] = 0x25C0, // xlib: 25C2
  [0x0add] = 0x25B6, // xlib: 2023
  [0x0ade] = 0x25CF,
  [0x0adf] = 0x25AE, // xlib: 25AC, gtk: 25A0
  [0x0ae0] = 0x25E6,
  [0x0ae1] = 0x25AB,
  [0x0ae2] = 0x25AD, // xlib: 25AE
  [0x0ae3] = 0x25B3, // xlib: 25B5
  [0x0ae4] = 0x25BD, // xlib: 25BF
  [0x0ae5] = 0x2606,
  [0x0ae6] = 0x2022,
  [0x0ae7] = 0x25AA,
  [0x0ae8] = 0x25B2, // xlib: 25B4
  [0x0ae9] = 0x25BC, // xlib: 25BE
  [0x0aea] = 0x261C, // xlib: 261A
  [0x0aeb] = 0x261E, // xlib: 261B
  [0x0aec] = 0x2663,
  [0x0aed] = 0x2666,
  [0x0aee] = 0x2665,
  [0x0af0] = 0x2720,
  [0x0af1] = 0x2020,
  [0x0af2] = 0x2021,
  [0x0af3] = 0x2713,
  [0x0af4] = 0x2717, // xlib: 2612
  [0x0af5] = 0x266F,
  [0x0af6] = 0x266D,
  [0x0af7] = 0x2642,
  [0x0af8] = 0x2640,
  [0x0af9] = 0x260E, // xlib: 2121
  [0x0afa] = 0x2315,
  [0x0afb] = 0x2117,
  [0x0afc] = 0x2038,
  [0x0afd] = 0x201A,
  [0x0afe] = 0x201E,
  [0x0aff] = 0x0000, // No Unicode equivalent  cursor

/* APL */
  [0x0ba3] = 0x003C,
  [0x0ba6] = 0x003E,
  [0x0ba8] = 0x2228,
  [0x0ba9] = 0x2227,
  [0x0bc0] = 0x00AF,
  [0x0bc2] = 0x22A4,
  [0x0bc3] = 0x2229,
  [0x0bc4] = 0x230A,
  [0x0bc6] = 0x005F,
  [0x0bca] = 0x2218,
  [0x0bcc] = 0x2395,
  [0x0bce] = 0x22A5,
  [0x0bcf] = 0x25CB,
  [0x0bd3] = 0x2308,
  [0x0bd6] = 0x222A,
  [0x0bd8] = 0x2283,
  [0x0bda] = 0x2282,
  [0x0bdc] = 0x22A3,
  [0x0bfc] = 0x22A2,

/* Hebrew */
  [0x0cdf] = 0x2017,
  [0x0ce0] = 0x05D0,
  [0x0ce1] = 0x05D1,
  [0x0ce2] = 0x05D2,
  [0x0ce3] = 0x05D3,
  [0x0ce4] = 0x05D4,
  [0x0ce5] = 0x05D5,
  [0x0ce6] = 0x05D6,
  [0x0ce7] = 0x05D7,
  [0x0ce8] = 0x05D8,
  [0x0ce9] = 0x05D9,
  [0x0cea] = 0x05DA,
  [0x0ceb] = 0x05DB,
  [0x0cec] = 0x05DC,
  [0x0ced] = 0x05DD,
  [0x0cee] = 0x05DE,
  [0x0cef] = 0x05DF,
  [0x0cf0] = 0x05E0,
  [0x0cf1] = 0x05E1,
  [0x0cf2] = 0x05E2,
  [0x0cf3] = 0x05E3,
  [0x0cf4] = 0x05E4,
  [0x0cf5] = 0x05E5,
  [0x0cf6] = 0x05E6,
  [0x0cf7] = 0x05E7,
  [0x0cf8] = 0x05E8,
  [0x0cf9] = 0x05E9,
  [0x0cfa] = 0x05EA,

/* Thai */
  [0x0da1] = 0x0E01,
  [0x0da2] = 0x0E02,
  [0x0da3] = 0x0E03,
  [0x0da4] = 0x0E04,
  [0x0da5] = 0x0E05,
  [0x0da6] = 0x0E06,
  [0x0da7] = 0x0E07,
  [0x0da8] = 0x0E08,
  [0x0da9] = 0x0E09,
  [0x0daa] = 0x0E0A,
  [0x0dab] = 0x0E0B,
  [0x0dac] = 0x0E0C,
  [0x0dad] = 0x0E0D,
  [0x0dae] = 0x0E0E,
  [0x0daf] = 0x0E0F,
  [0x0db0] = 0x0E10,
  [0x0db1] = 0x0E11,
  [0x0db2] = 0x0E12,
  [0x0db3] = 0x0E13,
  [0x0db4] = 0x0E14,
  [0x0db5] = 0x0E15,
  [0x0db6] = 0x0E16,
  [0x0db7] = 0x0E17,
  [0x0db8] = 0x0E18,
  [0x0db9] = 0x0E19,
  [0x0dba] = 0x0E1A,
  [0x0dbb] = 0x0E1B,
  [0x0dbc] = 0x0E1C,
  [0x0dbd] = 0x0E1D,
  [0x0dbe] = 0x0E1E,
  [0x0dbf] = 0x0E1F,
  [0x0dc0] = 0x0E20,
  [0x0dc1] = 0x0E21,
  [0x0dc2] = 0x0E22,
  [0x0dc3] = 0x0E23,
  [0x0dc4] = 0x0E24,
  [0x0dc5] = 0x0E25,
  [0x0dc6] = 0x0E26,
  [0x0dc7] = 0x0E27,
  [0x0dc8] = 0x0E28,
  [0x0dc9] = 0x0E29,
  [0x0dca] = 0x0E2A,
  [0x0dcb] = 0x0E2B,
  [0x0dcc] = 0x0E2C,
  [0x0dcd] = 0x0E2D,
  [0x0dce] = 0x0E2E,
  [0x0dcf] = 0x0E2F,
  [0x0dd0] = 0x0E30,
  [0x0dd1] = 0x0E31,
  [0x0dd2] = 0x0E32,
  [0x0dd3] = 0x0E33,
  [0x0dd4] = 0x0E34,
  [0x0dd5] = 0x0E35,
  [0x0dd6] = 0x0E36,
  [0x0dd7] = 0x0E37,
  [0x0dd8] = 0x0E38,
  [0x0dd9] = 0x0E39,
  [0x0dda] = 0x0E3A,
  [0x0dde] = 0x0E3E,
  [0x0ddf] = 0x0E3F,
  [0x0de0] = 0x0E40,
  [0x0de1] = 0x0E41,
  [0x0de2] = 0x0E42,
  [0x0de3] = 0x0E43,
  [0x0de4] = 0x0E44,
  [0x0de5] = 0x0E45,
  [0x0de6] = 0x0E46,
  [0x0de7] = 0x0E47,
  [0x0de8] = 0x0E48,
  [0x0de9] = 0x0E49,
  [0x0dea] = 0x0E4A,
  [0x0deb] = 0x0E4B,
  [0x0dec] = 0x0E4C,
  [0x0ded] = 0x0E4D,
  [0x0df0] = 0x0E50,
  [0x0df1] = 0x0E51,
  [0x0df2] = 0x0E52,
  [0x0df3] = 0x0E53,
  [0x0df4] = 0x0E54,
  [0x0df5] = 0x0E55,
  [0x0df6] = 0x0E56,
  [0x0df7] = 0x0E57,
  [0x0df8] = 0x0E58,
  [0x0df9] = 0x0E59,

/* Korean */
  [0x0ea1] = 0x3131,
  [0x0ea2] = 0x3132,
  [0x0ea3] = 0x3133,
  [0x0ea4] = 0x3134,
  [0x0ea5] = 0x3135,
  [0x0ea6] = 0x3136,
  [0x0ea7] = 0x3137,
  [0x0ea8] = 0x3138,
  [0x0ea9] = 0x3139,
  [0x0eaa] = 0x313A,
  [0x0eab] = 0x313B,
  [0x0eac] = 0x313C,
  [0x0ead] = 0x313D,
  [0x0eae] = 0x313E,
  [0x0eaf] = 0x313F,
  [0x0eb0] = 0x3140,
  [0x0eb1] = 0x3141,
  [0x0eb2] = 0x3142,
  [0x0eb3] = 0x3143,
  [0x0eb4] = 0x3144,
  [0x0eb5] = 0x3145,
  [0x0eb6] = 0x3146,
  [0x0eb7] = 0x3147,
  [0x0eb8] = 0x3148,
  [0x0eb9] = 0x3149,
  [0x0eba] = 0x314A,
  [0x0ebb] = 0x314B,
  [0x0ebc] = 0x314C,
  [0x0ebd] = 0x314D,
  [0x0ebe] = 0x314E,
  [0x0ebf] = 0x314F,
  [0x0ec0] = 0x3150,
  [0x0ec1] = 0x3151,
  [0x0ec2] = 0x3152,
  [0x0ec3] = 0x3153,
  [0x0ec4] = 0x3154,
  [0x0ec5] = 0x3155,
  [0x0ec6] = 0x3156,
  [0x0ec7] = 0x3157,
  [0x0ec8] = 0x3158,
  [0x0ec9] = 0x3159,
  [0x0eca] = 0x315A,
  [0x0ecb] = 0x315B,
  [0x0ecc] = 0x315C,
  [0x0ecd] = 0x315D,
  [0x0ece] = 0x315E,
  [0x0ecf] = 0x315F,
  [0x0ed0] = 0x3160,
  [0x0ed1] = 0x3161,
  [0x0ed2] = 0x3162,
  [0x0ed3] = 0x3163,
  [0x0ed4] = 0x11A8,
  [0x0ed5] = 0x11A9,
  [0x0ed6] = 0x11AA,
  [0x0ed7] = 0x11AB,
  [0x0ed8] = 0x11AC,
  [0x0ed9] = 0x11AD,
  [0x0eda] = 0x11AE,
  [0x0edb] = 0x11AF,
  [0x0edc] = 0x11B0,
  [0x0edd] = 0x11B1,
  [0x0ede] = 0x11B2,
  [0x0edf] = 0x11B3,
  [0x0ee0] = 0x11B4,
  [0x0ee1] = 0x11B5,
  [0x0ee2] = 0x11B6,
  [0x0ee3] = 0x11B7,
  [0x0ee4] = 0x11B8,
  [0x0ee5] = 0x11B9,
  [0x0ee6] = 0x11BA,
  [0x0ee7] = 0x11BB,
  [0x0ee8] = 0x11BC,
  [0x0ee9] = 0x11BD,
  [0x0eea] = 0x11BE,
  [0x0eeb] = 0x11BF,
  [0x0eec] = 0x11C0,
  [0x0eed] = 0x11C1,
  [0x0eee] = 0x11C2,
  [0x0eef] = 0x316D,
  [0x0ef0] = 0x3171,
  [0x0ef1] = 0x3178,
  [0x0ef2] = 0x317F,
  [0x0ef3] = 0x3181,
  [0x0ef4] = 0x3184,
  [0x0ef5] = 0x3186,
  [0x0ef6] = 0x318D,
  [0x0ef7] = 0x318E,
  [0x0ef8] = 0x11EB,
  [0x0ef9] = 0x11F0,
  [0x0efa] = 0x11F9,
  [0x0eff] = 0x20A9,

/* Latin 8 (Irish/Walsh) */
  [0x12a1] = 0x1E02,
  [0x12a2] = 0x1E03,
  [0x12a6] = 0x1E0A,
  [0x12a8] = 0x1E80,
  [0x12aa] = 0x1E82,
  [0x12ab] = 0x1E0B,
  [0x12ac] = 0x1EF2,
  [0x12b0] = 0x1E1E,
  [0x12b1] = 0x1E1F,
  [0x12b4] = 0x1E40,
  [0x12b5] = 0x1E41,
  [0x12b7] = 0x1E56,
  [0x12b8] = 0x1E81,
  [0x12b9] = 0x1E57,
  [0x12ba] = 0x1E83,
  [0x12bb] = 0x1E60,
  [0x12bc] = 0x1EF3,
  [0x12bd] = 0x1E84,
  [0x12be] = 0x1E85,
  [0x12bf] = 0x1E61,
  [0x12d0] = 0x0174,
  [0x12d7] = 0x1E6A,
  [0x12de] = 0x0176,
  [0x12f0] = 0x0175,
  [0x12f7] = 0x1E6B,
  [0x12fe] = 0x0177,

/* Latin 9 */
  [0x13bc] = 0x0152,
  [0x13bd] = 0x0153,
  [0x13be] = 0x0178,

/* Armenian */
  [0x14a1] = 0x058D, // (right-facing) or U+058E (left-facing) *
  [0x14a2] = 0x0587,
  [0x14a3] = 0x0589,
  [0x14a4] = 0x0029,
  [0x14a5] = 0x0028,
  [0x14a6] = 0x00BB,
  [0x14a7] = 0x00AB,
  [0x14a8] = 0x2014,
  [0x14a9] = 0x002E,
  [0x14aa] = 0x055D,
  [0x14ab] = 0x002C,
  [0x14ac] = 0x2013,
  [0x14ad] = 0x058A,
  [0x14ae] = 0x2026,
  [0x14af] = 0x055C,
  [0x14b0] = 0x055B,
  [0x14b1] = 0x055E,
  [0x14b2] = 0x0531,
  [0x14b3] = 0x0561,
  [0x14b4] = 0x0532,
  [0x14b5] = 0x0562,
  [0x14b6] = 0x0533,
  [0x14b7] = 0x0563,
  [0x14b8] = 0x0534,
  [0x14b9] = 0x0564,
  [0x14ba] = 0x0535,
  [0x14bb] = 0x0565,
  [0x14bc] = 0x0536,
  [0x14bd] = 0x0566,
  [0x14be] = 0x0537,
  [0x14bf] = 0x0567,
  [0x14c0] = 0x0538,
  [0x14c1] = 0x0568,
  [0x14c2] = 0x0539,
  [0x14c3] = 0x0569,
  [0x14c4] = 0x053A,
  [0x14c5] = 0x056A,
  [0x14c6] = 0x053B,
  [0x14c7] = 0x056B,
  [0x14c8] = 0x053C,
  [0x14c9] = 0x056C,
  [0x14ca] = 0x053D,
  [0x14cb] = 0x056D,
  [0x14cc] = 0x053E,
  [0x14cd] = 0x056E,
  [0x14ce] = 0x053F,
  [0x14cf] = 0x056F,
  [0x14d0] = 0x0540,
  [0x14d1] = 0x0570,
  [0x14d2] = 0x0541,
  [0x14d3] = 0x0571,
  [0x14d4] = 0x0542,
  [0x14d5] = 0x0572,
  [0x14d6] = 0x0543,
  [0x14d7] = 0x0573,
  [0x14d8] = 0x0544,
  [0x14d9] = 0x0574,
  [0x14da] = 0x0545,
  [0x14db] = 0x0575,
  [0x14dc] = 0x0546,
  [0x14dd] = 0x0576,
  [0x14de] = 0x0547,
  [0x14df] = 0x0577,
  [0x14e0] = 0x0548,
  [0x14e1] = 0x0578,
  [0x14e2] = 0x0549,
  [0x14e3] = 0x0579,
  [0x14e4] = 0x054A,
  [0x14e5] = 0x057A,
  [0x14e6] = 0x054B,
  [0x14e7] = 0x057B,
  [0x14e8] = 0x054C,
  [0x14e9] = 0x057C,
  [0x14ea] = 0x054D,
  [0x14eb] = 0x057D,
  [0x14ec] = 0x054E,
  [0x14ed] = 0x057E,
  [0x14ee] = 0x054F,
  [0x14ef] = 0x057F,
  [0x14f0] = 0x0550,
  [0x14f1] = 0x0580,
  [0x14f2] = 0x0551,
  [0x14f3] = 0x0581,
  [0x14f4] = 0x0552,
  [0x14f5] = 0x0582,
  [0x14f6] = 0x0553,
  [0x14f7] = 0x0583,
  [0x14f8] = 0x0554,
  [0x14f9] = 0x0584,
  [0x14fa] = 0x0555,
  [0x14fb] = 0x0585,
  [0x14fc] = 0x0556,
  [0x14fd] = 0x0586,
  [0x14fe] = 0x055A,
  [0x14ff] = 0x00A7,

/* Georgian */
  [0x15d0] = 0x10D0,
  [0x15d1] = 0x10D1,
  [0x15d2] = 0x10D2,
  [0x15d3] = 0x10D3,
  [0x15d4] = 0x10D4,
  [0x15d5] = 0x10D5,
  [0x15d6] = 0x10D6,
  [0x15d7] = 0x10D7,
  [0x15d8] = 0x10D8,
  [0x15d9] = 0x10D9,
  [0x15da] = 0x10DA,
  [0x15db] = 0x10DB,
  [0x15dc] = 0x10DC,
  [0x15dd] = 0x10DD,
  [0x15de] = 0x10DE,
  [0x15df] = 0x10DF,
  [0x15e0] = 0x10E0,
  [0x15e1] = 0x10E1,
  [0x15e2] = 0x10E2,
  [0x15e3] = 0x10E3,
  [0x15e4] = 0x10E4,
  [0x15e5] = 0x10E5,
  [0x15e6] = 0x10E6,
  [0x15e7] = 0x10E7,
  [0x15e8] = 0x10E8,
  [0x15e9] = 0x10E9,
  [0x15ea] = 0x10EA,
  [0x15eb] = 0x10EB,
  [0x15ec] = 0x10EC,
  [0x15ed] = 0x10ED,
  [0x15ee] = 0x10EE,
  [0x15ef] = 0x10EF,
  [0x15f0] = 0x10F0,
  [0x15f1] = 0x10F1,
  [0x15f2] = 0x10F2,
  [0x15f3] = 0x10F3,
  [0x15f4] = 0x10F4,
  [0x15f5] = 0x10F5,
  [0x15f6] = 0x10F6,

/* Caucasus, Inupiak, Guarani */
  [0x16a2] = 0x0000, // No Unicode equivalent  Ç with dot above
  [0x16a3] = 0x1E8A,
  [0x16a5] = 0x0000, // No Unicode equivalent  Q with dot above
  [0x16a6] = 0x012C,
  [0x16a7] = 0x0000, // No Unicode equivalent  IE ligature
  [0x16a8] = 0x0000, // No Unicode equivalent  UO ligature
  [0x16a9] = 0x01B5,
  [0x16aa] = 0x01E6,
  [0x16af] = 0x019F,

  [0x16b2] = 0x0000, // No Unicode equivalent  ç with dot above
  [0x16b3] = 0x1E8B,
  [0x16b4] = 0x01D1,
  [0x16b5] = 0x0000, // No Unicode equivalent  q with dot above
  [0x16b6] = 0x012D,
  [0x16b7] = 0x0000, // No Unicode equivalent  ie ligature
  [0x16b8] = 0x0000, // No Unicode equivalent  uo ligature
  [0x16b9] = 0x01B6,
  [0x16ba] = 0x01E7,
  [0x16bd] = 0x01D2,
  [0x16bf] = 0x0275,

  [0x16c6] = 0x018F,
  [0x16d1] = 0x1E36,
  [0x16d2] = 0x0000, // No Unicode equivalent  L with stroke and dot below
  [0x16d3] = 0x0000, // No Unicode equivalent  G with tilde
  [0x16e1] = 0x1E37,
  [0x16e2] = 0x0000, // No Unicode equivalent  l with stroke and dot below
  [0x16e3] = 0x0000, // No Unicode equivalent  g with tilde
  [0x16f6] = 0x0259,

/* Vietnamese */
  [0x1e9f] = 0x0303,
  [0x1ea0] = 0x1EA0,
  [0x1ea1] = 0x1EA1,
  [0x1ea2] = 0x1EA2,
  [0x1ea3] = 0x1EA3,
  [0x1ea4] = 0x1EA4,
  [0x1ea5] = 0x1EA5,
  [0x1ea6] = 0x1EA6,
  [0x1ea7] = 0x1EA7,
  [0x1ea8] = 0x1EA8,
  [0x1ea9] = 0x1EA9,
  [0x1eaa] = 0x1EAA,
  [0x1eab] = 0x1EAB,
  [0x1eac] = 0x1EAC,
  [0x1ead] = 0x1EAD,
  [0x1eae] = 0x1EAE,
  [0x1eaf] = 0x1EAF,
  [0x1eb0] = 0x1EB0,
  [0x1eb1] = 0x1EB1,
  [0x1eb2] = 0x1EB2,
  [0x1eb3] = 0x1EB3,
  [0x1eb4] = 0x1EB4,
  [0x1eb5] = 0x1EB5,
  [0x1eb6] = 0x1EB6,
  [0x1eb7] = 0x1EB7,
  [0x1eb8] = 0x1EB8,
  [0x1eb9] = 0x1EB9,
  [0x1eba] = 0x1EBA,
  [0x1ebb] = 0x1EBB,
  [0x1ebc] = 0x1EBC,
  [0x1ebd] = 0x1EBD,
  [0x1ebe] = 0x1EBE,
  [0x1ebf] = 0x1EBF,
  [0x1ec0] = 0x1EC0,
  [0x1ec1] = 0x1EC1,
  [0x1ec2] = 0x1EC2,
  [0x1ec3] = 0x1EC3,
  [0x1ec4] = 0x1EC4,
  [0x1ec5] = 0x1EC5,
  [0x1ec6] = 0x1EC6,
  [0x1ec7] = 0x1EC7,
  [0x1ec8] = 0x1EC8,
  [0x1ec9] = 0x1EC9,
  [0x1eca] = 0x1ECA,
  [0x1ecb] = 0x1ECB,
  [0x1ecc] = 0x1ECC,
  [0x1ecd] = 0x1ECD,
  [0x1ece] = 0x1ECE,
  [0x1ecf] = 0x1ECF,
  [0x1ed0] = 0x1ED0,
  [0x1ed1] = 0x1ED1,
  [0x1ed2] = 0x1ED2,
  [0x1ed3] = 0x1ED3,
  [0x1ed4] = 0x1ED4,
  [0x1ed5] = 0x1ED5,
  [0x1ed6] = 0x1ED6,
  [0x1ed7] = 0x1ED7,
  [0x1ed8] = 0x1ED8,
  [0x1ed9] = 0x1ED9,
  [0x1eda] = 0x1EDA,
  [0x1edb] = 0x1EDB,
  [0x1edc] = 0x1EDC,
  [0x1edd] = 0x1EDD,
  [0x1ede] = 0x1EDE,
  [0x1edf] = 0x1EDF,
  [0x1ee0] = 0x1EE0,
  [0x1ee1] = 0x1EE1,
  [0x1ee2] = 0x1EE2,
  [0x1ee3] = 0x1EE3,
  [0x1ee4] = 0x1EE4,
  [0x1ee5] = 0x1EE5,
  [0x1ee6] = 0x1EE6,
  [0x1ee7] = 0x1EE7,
  [0x1ee8] = 0x1EE8,
  [0x1ee9] = 0x1EE9,
  [0x1eea] = 0x1EEA,
  [0x1eeb] = 0x1EEB,
  [0x1eec] = 0x1EEC,
  [0x1eed] = 0x1EED,
  [0x1eee] = 0x1EEE,
  [0x1eef] = 0x1EEF,
  [0x1ef0] = 0x1EF0,
  [0x1ef1] = 0x1EF1,
  [0x1ef2] = 0x0300,
  [0x1ef3] = 0x0301,
  [0x1ef4] = 0x1EF4,
  [0x1ef5] = 0x1EF5,
  [0x1ef6] = 0x1EF6,
  [0x1ef7] = 0x1EF7,
  [0x1ef8] = 0x1EF8,
  [0x1ef9] = 0x1EF9,
  [0x1efa] = 0x01A0,
  [0x1efb] = 0x01A1,
  [0x1efc] = 0x01AF,
  [0x1efd] = 0x01B0,
  [0x1efe] = 0x0309,
  [0x1eff] = 0x0323,

/* Currency */
  [0x20a0] = 0x20A0,
  [0x20a1] = 0x20A1,
  [0x20a2] = 0x20A2,
  [0x20a3] = 0x20A3,
  [0x20a4] = 0x20A4,
  [0x20a5] = 0x20A5,
  [0x20a6] = 0x20A6,
  [0x20a7] = 0x20A7,
  [0x20a8] = 0x20A8,
  [0x20a9] = 0x20A9,
  [0x20aa] = 0x20AA,
  [0x20ab] = 0x20AB,
  [0x20ac] = 0x20AC,

};

/* Dead keysyms */

#define F_DEAD  0xFE50
#define L_DEAD  0xFE93
#define N_DEAD  (L_DEAD - F_DEAD + 1)

#define is_dead_keysym(ks) (((ks) >= F_DEAD) && ((ks) <= L_DEAD))

#define get_dead(ks) (dead_map[(ks) - F_DEAD])

/* Note: using uint16_t to save space, but these are really uint32_t */
static const uint16_t dead_map[N_DEAD] = {

  [-F_DEAD+0xfe50] = 0x0300, // grave               0x02CB  0x0060
  [-F_DEAD+0xfe51] = 0x0301, // acute               0x02CA  0x00B4
  [-F_DEAD+0xfe52] = 0x0302, // circumflex          0x02C6 ~0x005E
  [-F_DEAD+0xfe53] = 0x0303, // tilde              ~0x02DC  0x007E
  [-F_DEAD+0xfe54] = 0x0304, // macron              0x02C9  0x00AF
  [-F_DEAD+0xfe55] = 0x0306, // breve               0x02D8
  [-F_DEAD+0xfe56] = 0x0307, // abovedot            0x02D9
  [-F_DEAD+0xfe57] = 0x0308, // diaeresis                   0x00A8
  [-F_DEAD+0xfe58] = 0x030A, // abovering           0x02DA
  [-F_DEAD+0xfe59] = 0x030B, // doubleacute         0x02DD ~0x02F6
  [-F_DEAD+0xfe5a] = 0x030C, // caron               0x02C7
  [-F_DEAD+0xfe5b] = 0x0327, // cedilla                     0x00B8
  [-F_DEAD+0xfe5c] = 0x0328, // ogonek              0x02DB
  [-F_DEAD+0xfe5d] = 0x0345, // iota                0x037A  (mod:0x1DA5)
  [-F_DEAD+0xfe5e] = 0x3099, // dakuten             0x309B  (half:0xFF9E)
  [-F_DEAD+0xfe5f] = 0x309A, // handakuten          0x309C  (half:0xFF9F)

  [-F_DEAD+0xfe60] = 0x0323, // belowdot
  [-F_DEAD+0xfe61] = 0x0309, // hook
  [-F_DEAD+0xfe62] = 0x031B, // horn
  [-F_DEAD+0xfe63] = 0x0335, // stroke                      (long:0x0336)
  [-F_DEAD+0xfe64] = 0x0313, // abovecomma
  [-F_DEAD+0xfe65] = 0x0314, // abovereversedcomma  0x02BD
  [-F_DEAD+0xfe66] = 0x030F, // doublegrave
  [-F_DEAD+0xfe67] = 0x0325, // belowring           0x02F3
  [-F_DEAD+0xfe68] = 0x0331, // belowmacron         0x02CD
  [-F_DEAD+0xfe69] = 0x032D, // belowcircumflex     0xA788
  [-F_DEAD+0xfe6a] = 0x0330, // belowtilde          0x02F7
  [-F_DEAD+0xfe6b] = 0x032E, // belowbreve
  [-F_DEAD+0xfe6c] = 0x0324, // belowdiaeresis
  [-F_DEAD+0xfe6d] = 0x0311, // invertedbreve
  [-F_DEAD+0xfe6e] = 0x0326, // belowcomma

  [-F_DEAD+0xfe90] = 0x0332, // lowline                     0x005F (full:0xFF3F)
  [-F_DEAD+0xfe91] = 0x030D, // aboveverticalline   0x02C8
  [-F_DEAD+0xfe92] = 0x0329, // belowverticalline   0x02CC
  [-F_DEAD+0xfe93] = 0x0338, // longsolidusoverlay          0x002F

};

/* Keypad keysyms */

#define F_KP  0xFFAA
#define L_KP  0xFFBD
#define N_KP  (L_KP - F_KP + 1)

#define is_keypad_keysym(ks) (((ks) >= F_KP) && ((ks) <= L_KP))

#define get_keypad(ks) (kp_map[(ks) - F_KP])

/* Note: using uint16_t to save space, but these are really uint32_t */
static const uint16_t kp_map[N_KP] = {

  [-F_KP+0xffaa] = 0x002A, // multiply
  [-F_KP+0xffab] = 0x002B, // add
  [-F_KP+0xffac] = 0x002C, // separator
  [-F_KP+0xffad] = 0x002D, // subtract
  [-F_KP+0xffae] = 0x002E, // decimal
  [-F_KP+0xffaf] = 0x002F, // divide
  [-F_KP+0xffb0] = 0x0030, // 0
  [-F_KP+0xffb1] = 0x0031, // 1
  [-F_KP+0xffb2] = 0x0032, // 2
  [-F_KP+0xffb3] = 0x0033, // 3
  [-F_KP+0xffb4] = 0x0034, // 4
  [-F_KP+0xffb5] = 0x0035, // 5
  [-F_KP+0xffb6] = 0x0036, // 6
  [-F_KP+0xffb7] = 0x0037, // 7
  [-F_KP+0xffb8] = 0x0038, // 8
  [-F_KP+0xffb9] = 0x0039, // 9
  [-F_KP+0xffbd] = 0x003D, // equals
};

/* Unmapped */
/*
0xfe6f // currency
0xfe80 // a
0xfe81 // A
0xfe82 // e
0xfe83 // E
0xfe84 // i
0xfe85 // I
0xfe86 // o
0xfe87 // O
0xfe88 // u
0xfe89 // U
0xfe8a // small_schwa
0xfe8b // capital_schwa
0xfe8c // greek
*/

bool
x11_keysym_is_dead(
  uint32_t keysym)
{
  return is_dead_keysym(keysym);
}

uint32_t
x11_keysym_to_unicode(
  uint32_t keysym)
{
  if (is_unicode_keysym(keysym)) {
    return get_unicode(keysym);
  } else if (is_regular_keysym(keysym)) {
    return get_regular(keysym);
  } else if (is_dead_keysym(keysym)) {
    return get_dead(keysym);
  } else if (is_keypad_keysym(keysym)) {
    return get_keypad(keysym);
  } else {
    return 0;
  }
}

xcb_keysym_t
x11_keysym_of_event(
  xcb_key_press_event_t *e)
{
  assert(x11_back != NULL);
  assert(x11_back->xkb_get_map_reply != NULL);

/*
    XCB_MOD_MASK_SHIFT = 1,   // 0x32 (LShift) / 0x3E (Rshift)
    XCB_MOD_MASK_LOCK = 2,    // 0x42 (CapsLock)
    XCB_MOD_MASK_CONTROL = 4, // 0x25 (LCtrl) / 0x69 (RCtrl)
    XCB_MOD_MASK_1 = 8,       // 0x40 (LAlt) / 0xCD (Meta) // WHICH META ?
                                     - Also Mode switch on X11 on OSX

    XCB_MOD_MASK_2 = 16,  // 0x45 (NumLock) - Also meta on X11 on OSX
    XCB_MOD_MASK_3 = 32,  // unused
    XCB_MOD_MASK_4 = 64,  // 0x85 (LWin) 0x86 (RWin) 0xCE (Super) 0xCF (Hyper)
                          // L/R Super (Windows), L Hyper, (mac meta ???)
    XCB_MOD_MASK_5 = 128, // 0x5C (RAlt) 0xCB (ModeSwitch))
                          // AltGr (aka ISO Level3 shift), Mode_switch
                          // 5C should be 0x6C (Level3)
*/
/*
  int alt = (x11_back->modifiers & MOD_ALT) != 0;
  int shift = (x11_back->modifiers & MOD_SHIFT) != 0;
  int caps = (x11_back->modifiers & MOD_CAPSLOCK) != 0;
*/
/*
  int altgr = (e->state & XCB_MOD_MASK_5) != 0; // Not always
  int shift = (e->state & XCB_MOD_MASK_SHIFT) != 0;
  int caps = (e->state & XCB_MOD_MASK_LOCK) != 0;
*/

/* https://www.x.org/releases/X11R7.7/doc/kbproto/xkbproto.html#Computing_A_State_Field_from_an_XKB_State */

  x11_keysyms_t *ks = &(x11_back->keysyms[e->detail]);

  uint8_t nb_groups = ks->grp_info & 0x0F;
  if (nb_groups == 0) {
    return 0;
  }

  uint8_t group = (e->state & 0x6000) >> 13;

  if (group >= nb_groups) {
    switch (ks->grp_info & 0xC0) {
      default:
      case 0x00: // Wrap
        group %= nb_groups;
        break;
      case 0x40: // Clamp
        group = nb_groups - 1;
        break;
      case 0x80: // Redirect
        group = (ks->grp_info & 0x30) >> 4;
        if (group >= nb_groups) {
          group = 0;
        }
        break;
    }
  }

  x11_keytypes_t *kt = &(x11_back->keytypes[ks->grp_kt[group]]);

  uint8_t mods = e->state & kt->mods_mask;
  uint8_t level = 0;
  for (uint8_t i = 0; i < kt->nb_entries; ++i) {
    xcb_xkb_kt_map_entry_t *kte = &(kt->keytypes[i]);
    if (kte->active && kte->mods_mask == mods) {
      level = kte->level;
      break;
    }
  }

  if (level > ks->grp_width) {
    level = 0;
  }

  return ks->keysyms[group * ks->grp_width + level];
}

#else

const int x11_keysym = 0;

#endif /* HAS_X11 */
