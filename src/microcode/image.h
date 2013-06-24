/* -*-C-*-

$Id: image.h,v 9.25 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1987, 1988, 1989, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

extern Image_Fast_Transpose ();
/* REAL * Array;
   long nrows;
   OPTIMIZATION for square images */

extern Image_Transpose ();
/* REAL * Array;
   REAL * New_Array;
   long nrows;
   long ncols; */

extern Image_Rotate_90clw ();
/* REAL * Array;
   REAL * Rotated_Array;
   long nrows;
   long ncols; */

extern Image_Rotate_90cclw ();
/* REAL * Array;
   REAL * Rotated_Array;
   long nrows;
   long ncols; */

extern Image_Mirror ();
/* REAL * Array;
   long nrows;
   long ncols; */

extern Image_Mirror_Upside_Down ();
/* REAL * Array;
   long nrows;
   long ncols;
   REAL * Temp_Row; */

extern Image_Rotate_90clw_Mirror ();
/* REAL * Array;
   REAL * Rotated_Array;
   long nrows;
   long ncols; */

extern Image_Draw_Magnify_N_Times_With_Offset_Scale ();
extern Image_Draw_Magnify_N_Times_With_Offset_Scale_Only ();
