/* -*-C-*-

$Id: image.h,v 9.27 2003/02/14 18:28:19 cph Exp $

Copyright (c) 1987, 1988, 1989, 1999 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

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
