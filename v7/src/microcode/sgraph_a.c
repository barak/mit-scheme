/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/sgraph_a.c,v 1.6 1989/02/19 17:51:11 jinx Rel $ */

#include "scheme.h"
#include "prims.h"
#include "flonum.h"
#include "Sgraph.h"
#include "array.h"

#ifndef STARBASE_COLOR_TABLE_START
#define STARBASE_COLOR_TABLE_START 0
#endif

#ifndef STARBASE_COLOR_TABLE_SIZE
#define STARBASE_COLOR_TABLE_SIZE 16
#endif

float Color_Table[STARBASE_COLOR_TABLE_SIZE][3];

DEFINE_PRIMITIVE ("PLOT-ARRAY-IN-BOX", Prim_plot_array_in_box, 3, 3, 0)
{
  float Plotting_Box[4];   /* x_min, y_min, x_max, y_max */
  long Length; int fill_with_lines;
  REAL *Array, Scale, Offset;
  Pointer Answer, *Orig_Free;
  Primitive_3_Args();

  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  Array = Scheme_Array_To_C_Array(Arg1);
  Arg_2_Type(TC_LIST);
  Get_Plotting_Box(Plotting_Box, Arg2);
  Arg_3_Type(TC_FIXNUM);
  Range_Check(fill_with_lines, Arg3, 0, 1, ERR_ARG_1_BAD_RANGE);  /* plot only points or fill with lines */

  Plot_C_Array(Array, Length, Plotting_Box, fill_with_lines, &Offset, &Scale);
  
  Primitive_GC_If_Needed(4);
  Answer = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free += 4;
  Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = EMPTY_LIST;
  PRIMITIVE_RETURN(Answer);
}

DEFINE_PRIMITIVE ("PLOT-ARRAY-IN-BOX-WITH-OFFSET-SCALE", Prim_plot_array_in_box_with_offset_scale, 5, 5, 0)
{
  float Plotting_Box[4];   /* x_min, y_min, x_max, y_max */
  long Length; int fill_with_lines;
  REAL *Array, Scale, Offset;
  Pointer Answer, *Orig_Free;
  int Error_Number;
  Primitive_5_Args();

  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  Array = Scheme_Array_To_C_Array(Arg1);

  Arg_2_Type(TC_LIST);
  Get_Plotting_Box(Plotting_Box, Arg2);
    
  Error_Number = Scheme_Number_To_REAL(Arg3, &Offset);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);

  Error_Number = Scheme_Number_To_REAL(Arg4, &Scale);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);

  Arg_5_Type(TC_FIXNUM);
  Range_Check(fill_with_lines, Arg5, 0, 1, ERR_ARG_1_BAD_RANGE);  /* plot only points or fill with lines */
  
  Plot_C_Array_With_Offset_Scale(Array, Length, Plotting_Box, fill_with_lines, Offset, Scale);

  Primitive_GC_If_Needed(4);
  Answer = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free += 4;
  Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = EMPTY_LIST;
  PRIMITIVE_RETURN(Answer);
}

#define max(x,y)	(((x)<(y)) ? (y) : (x))
#define min(x,y)	(((x)<(y)) ? (x) : (y))

Plot_C_Array(Array, Length, Plotting_Box, fill_with_lines, pOffset, pScale)
     /* the pOffset,pScale are for RETURNING VALUES */
     float *Plotting_Box; long Length; 
     int fill_with_lines;             /* plots filled with lines from 0 to y(t) */
     REAL *Array, *pScale, *pOffset;
{
  float box_x_min = Plotting_Box[0],
        box_y_min = Plotting_Box[1];
  float box_x_max = Plotting_Box[2],
        box_y_max = Plotting_Box[3];
  float Box_Length = box_x_max - box_x_min,
        Box_Height = box_y_max - box_y_min;
  register float x_position,y_position, clipped_offset, index_inc;
  /* index_inc is for skipping values if there are two many to plot */
  REAL y_offset, scale, Array_Min, Array_Max;
  long i, nmin, nmax;
  
  C_Array_Find_Min_Max(Array, Length, &nmin, &nmax);
  Array_Min = Array[nmin];  Array_Max = Array[nmax];

  Find_Offset_Scale_For_Linear_Map(Array_Min,Array_Max, ((REAL) box_y_min), ((REAL) box_y_max),
				   &y_offset, &scale);
  index_inc = ((float) Box_Length/Length);
  
  x_position = box_x_min;
  if (fill_with_lines == 0)
  {
    for (i = 0; i < Length; i++)
    {
      y_position = ((float) (y_offset + (scale * Array[i])));
      move2d(screen_handle, x_position, y_position);
      draw2d(screen_handle, x_position, y_position);
      x_position = x_position + index_inc;
    }
  }
  else
  {
    clipped_offset = min( max(box_y_min, ((float) y_offset)), box_y_max);
    /* fill from zero-line but do not go outside box, (don't bother with starbase clipping) */
    for (i = 0; i < Length; i++)
    {
      y_position = ((float) (y_offset + (scale * Array[i])));
      move2d(screen_handle, x_position, clipped_offset);
      draw2d(screen_handle, x_position, y_position);
      x_position = x_position + index_inc;
    }
  }
  make_picture_current(screen_handle);
  
  *pOffset = y_offset;          /* returning values */
  *pScale  = scale;
}

/* The following is useful for comparison purposes 
 */

Plot_C_Array_With_Offset_Scale(Array, Length, Plotting_Box, fill_with_lines, Offset, Scale) 
     float *Plotting_Box; long Length;
     int fill_with_lines;             /* plots filled with lines from 0 to y(t) */   
     REAL *Array, Scale, Offset;
{
  float box_x_min = Plotting_Box[0],
        box_y_min=Plotting_Box[1];
  float box_x_max = Plotting_Box[2],
        box_y_max = Plotting_Box[3];
  float Box_Length = box_x_max - box_x_min,
        Box_Height = box_y_max - box_y_min;
  register float x_position, y_position, index_inc, clipped_offset;
  long i;
  
  index_inc = ((float) Box_Length/Length);
  x_position = box_x_min;
  if (fill_with_lines == 0)
  { /* plot just the points */
    for (i = 0; i < Length; i++)
    {
      y_position = ((float) (Offset + (Scale * Array[i])));
      move2d(screen_handle, x_position, y_position);
      draw2d(screen_handle, x_position, y_position);
      x_position = x_position + index_inc;
    }
  }
  else
  { /* fill with lines */
    clipped_offset = min( max(box_y_min, ((float) Offset)), box_y_max);
    /* fill from zero-line but do not go outside box, (don't bother with starbase clipping) */
    for (i = 0; i < Length; i++)
    {
      y_position = ((float) (Offset + (Scale * Array[i])));
      move2d(screen_handle, x_position, clipped_offset);
      draw2d(screen_handle, x_position, y_position);
      x_position = x_position + index_inc;
    }
  }
  make_picture_current(screen_handle);
}

Get_Plotting_Box(Plotting_Box, Arg2)
     float *Plotting_Box;
     Pointer Arg2;
{
  Pointer List;
  long i, fixnum;

  Touch_In_Primitive(Arg2, List);
  for (i = 0; i < 4; i++)
  {
    Make_Flonum(Vector_Ref(List, CONS_CAR), Plotting_Box[i], fixnum, 
		ERR_ARG_2_WRONG_TYPE);
    Touch_In_Primitive( Vector_Ref(List, CONS_CDR), List );
  }
  if (List != EMPTY_LIST)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
}

Plot_Box(Box)
     float *Box;
{
  perimeter_color_index(screen_handle, 1);
  interior_style(screen_handle, INT_HOLLOW, TRUE);
  rectangle(screen_handle, Box[0], Box[1], Box[2], Box[3]);
  make_picture_current(screen_handle);
}

DEFINE_PRIMITIVE ("CLEAR-BOX", Prim_clear_box, 1, 1, 0)
{
  float Plotting_Box[4];   /* x_min, y_min, x_max, y_max */
  Primitive_1_Args();

  Arg_1_Type(TC_LIST);
  Get_Plotting_Box(Plotting_Box, Arg1);
  C_Clear_Rectangle(Plotting_Box);
  PRIMITIVE_RETURN(SHARP_F);
}

C_Clear_Rectangle(Box)
     float *Box;
{
  xposition = 0.0;
  yposition = 0.0;
  move2d(screen_handle, xposition, yposition);
  
  clip_rectangle(screen_handle, Box[0], Box[2], Box[1], Box[3]); /* shuffle around the coords */
  clear_control(screen_handle, CLEAR_CLIP_RECTANGLE);
  clear_view_surface(screen_handle);
  make_picture_current(screen_handle);
  clear_control(screen_handle, CLEAR_DISPLAY_SURFACE); /* back to the default */
  clip_rectangle(screen_handle, sb_xmin, sb_xmax, sb_ymin, sb_ymax);
}

DEFINE_PRIMITIVE ("BOX-MOVE", Prim_box_move, 2, 2, 0) 
{
  float From_Box[4];   /* x_min, y_min, x_max, y_max */
  float To_Box[4];
  float x_source, y_source, x_dest, y_dest, x_length, y_length;
  Primitive_2_Args();

  Arg_1_Type(TC_LIST);
  Arg_1_Type(TC_LIST);
  Get_Plotting_Box(From_Box, Arg1);
  Get_Plotting_Box(  To_Box, Arg2);
  
  x_source = From_Box[0]; y_source = From_Box[3];
  x_dest   =   To_Box[0]; y_dest   =   To_Box[3];
  y_length = From_Box[3] - From_Box[1];                          /* notice convention of matrix row, column! */
  x_length = From_Box[2] - From_Box[0];
  if ((y_length != (To_Box[3]-To_Box[1])) || (x_length != (To_Box[2]-To_Box[0])))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  block_move(screen_handle, x_source, y_source, ((int) x_length), ((int) y_length), 
	     x_dest, y_dest);
  PRIMITIVE_RETURN(SHARP_F);
}

DEFINE_PRIMITIVE ("BOX-ROTATE-MOVE", Prim_box_rotate_move, 2, 2, 0) 
{
  float From_Box[4];
  float   To_Box[4];
  float x_source, y_source, x_dest, y_dest, x_length, y_length;
  Primitive_2_Args();
  Arg_1_Type(TC_LIST);
  Arg_1_Type(TC_LIST);

  Get_Plotting_Box(From_Box, Arg1);
  Get_Plotting_Box(  To_Box, Arg2);

  x_source = From_Box[0]; y_source = From_Box[3];
  x_dest   =   To_Box[0]; y_dest   =   To_Box[3];
  x_length = From_Box[3] - From_Box[1];
  y_length = From_Box[2] - From_Box[0];
  if ((x_length != (To_Box[3]-To_Box[1])) || (y_length != (To_Box[2]-To_Box[0])))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);

  block_read(screen_handle, x_source, y_source, ((int) x_length), ((int) y_length), 
	     x_dest, y_dest);
#if false
  Char_Array_90clw();
#else
  fprintf(stderr, "\nPrim_Box_Rotate_Move: Char_Array_90clw undefined.\n");
  Primitive_Error(ERR_EXTERNAL_RETURN);
#endif
  
  block_read(screen_handle, x_source, y_source, ((int) x_length), ((int) y_length), 
	     x_dest, y_dest);
  PRIMITIVE_RETURN(SHARP_F);
}


/*_________________________________ image drawing ___________________________ */

/* 
  ;; Image Drawing (halftoning)
  ;; HG = Hard Grey levels (i.e. output device greys)
  ;; SG = Soft Grey levels (i.e. simulated grey levels)
  ;; There are 3 methods: PSAM, OD, BN (see below)
  There are also the old 16-color drawing routines.
*/

/* 
  ;; PSAM (Pulse-Surface-Area Modulation) works only for 2 HG grey levels.
  ;; It maps 1 pxl to a square of 16 pxls.
  ;; The distribution of on/off pxls in the square gives 16 grey levels.
  ;; It's the most efficient for B&W monitors, but see below for better quality drawing using OD and BN.
  ;; Halftoning using OD and BN works for any number of grey levels, and there are many methods available (see below).
  
  IMAGE-PSAM-ATXY-WMM  fixed magnification 1pxl->16pxls
  Draw line (width 4) by line.  Pdata space needed = (4*ncols*16) .
  ;; The following 2 primitives simply take in arguments, and allocate space,
  ;; They call C_image_psam_atxy_wmm to do the actual drawing.
  */

DEFINE_PRIMITIVE ("IMAGE-PSAM-ATXY-WMM", Prim_image_psam_atxy_wmm, 5, 5, 0)
{ REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  
  Primitive_5_Args();
  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray);                                         /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at);                   /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at);                   /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
 
  Error_Number = Scheme_Number_To_REAL(Arg4, &Min);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &Max);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  Length = nrows*ncols;
  
  Primitive_GC_If_Needed( (16*4*ncols) * sizeof(unsigned char) );
  pdata = ((unsigned char *) Free);
  /* the following draws the picture, clipping values outside Min,Max */ 
  C_image_psam_atxy_wmm(Array, pdata, nrows, ncols,
			((float) x_at), ((float) y_at), 
			Min, Max);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("IMAGE-PSAM-ATXY-WOMM", Prim_image_psam_atxy_womm, 5, 5, 0)
{ REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  Primitive_5_Args();

  Arg_1_Type(TC_LIST);		/* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray); /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE); /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE); /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at); /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at); /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
 
  Error_Number = Scheme_Number_To_REAL(Arg4, &Min);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &Max);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  Length = nrows*ncols;
  
  Primitive_GC_If_Needed( (16*4*ncols) * sizeof(unsigned char) );
  pdata = ((unsigned char *) Free);
  C_image_psam_atxy_womm(Array, pdata, nrows, ncols,
			 ((float) x_at), ((float) y_at),
			 Min, Max);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("IMAGE-HT-OD-ATXY-WMM", Prim_image_ht_od_atxy_wmm, 7, 7, 0)
{ REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray;
  Pointer *Orig_Free;
  long nrows, ncols, Length, HG, ODmethod;
  REAL *Array, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  
  Primitive_7_Args();
  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray);                                         /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at);                   /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at);                   /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
 
  Error_Number = Scheme_Number_To_REAL(Arg4, &Min);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &Max);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  Length = nrows*ncols;
  Arg_6_Type(TC_FIXNUM);
  Range_Check(HG, Arg6, 1, 256, ERR_ARG_6_BAD_RANGE);  /* don't expect more color levels than this */
  Arg_7_Type(TC_FIXNUM);
  Range_Check(ODmethod, Arg7, 0, 7, ERR_ARG_7_BAD_RANGE);  /* see below HT_OD_TABLE_MAX_INDEX */
  
  Primitive_GC_If_Needed( ncols * sizeof(unsigned char) );
  pdata = ((unsigned char *) Free);
  /* the following draws the picture, clipping values outside Min,Max */ 
  C_image_ht_od_atxy_wmm(Array, pdata, nrows,ncols,
			 ((float) x_at), ((float) y_at),  Min,Max,
			 HG,ODmethod);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("IMAGE-HT-BN-ATXY-WMM", Prim_image_ht_bn_atxy_wmm, 7, 7, 0)
{ REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray;
  Pointer *Orig_Free;
  long nrows, ncols, Length, HG, BNmethod;
  REAL *Array, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  float **er_rows;

  Primitive_7_Args();
  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray);                                         /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at);                   /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at);                   /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
 
  Error_Number = Scheme_Number_To_REAL(Arg4, &Min);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &Max);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  Length = nrows*ncols;
  Arg_6_Type(TC_FIXNUM);
  Range_Check(HG, Arg6, 1, 256, ERR_ARG_6_BAD_RANGE);  /* don't expect more color levels than this */
  Arg_7_Type(TC_FIXNUM);
  Range_Check(BNmethod, Arg7, 0, 2, ERR_ARG_7_BAD_RANGE);  /* 3 masks (methods) available, see below  */
  
  Primitive_GC_If_Needed( (ncols*sizeof(unsigned char)) + (3*(ncols+4) * sizeof(float)) );
  /* 1-row for pdata, and 3 rows of 2+ncols+2 length for er_rows */ 
  /* Primitive_GC_If_Needed takes either number of bytes, or number of scheme pointers,it works either way. */
  pdata = ((unsigned char *) Free);
  er_rows = ((float **) (pdata+ncols));
  er_rows[0] = ((float *) (er_rows+3));
  er_rows[1] = er_rows[0] + (ncols+4);
  er_rows[2] = er_rows[1] + (ncols+4);
  
  C_image_ht_bn_atxy_wmm(Array, pdata, nrows,ncols,
			 ((float) x_at), ((float) y_at),  Min,Max,
			 HG,BNmethod, er_rows);
  PRIMITIVE_RETURN(SHARP_T);
}

#define MINTEGER long

DEFINE_PRIMITIVE ("IMAGE-HT-IBN-ATXY-WMM", Prim_image_ht_ibn_atxy_wmm, 8, 8, 0)
{ REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray;
  Pointer *Orig_Free;
  long nrows, ncols, Length, HG, BNmethod;
  REAL *Array, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  MINTEGER **er_rows, PREC_SCALE;
  
  Primitive_8_Args();
  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray);                                         /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at);                   /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at);                   /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
 
  Error_Number = Scheme_Number_To_REAL(Arg4, &Min);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &Max);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  Length = nrows*ncols;
  Arg_6_Type(TC_FIXNUM);
  Range_Check(HG, Arg6, 1, 256, ERR_ARG_6_BAD_RANGE);  /* don't expect more color levels than this */
  Arg_7_Type(TC_FIXNUM);
  Range_Check(BNmethod, Arg7, 0, 2, ERR_ARG_7_BAD_RANGE);  /* 3 masks (methods) available, see below  */
  Arg_8_Type(TC_FIXNUM);
  Range_Check(PREC_SCALE, Arg8, 1, (1<<(8*sizeof(MINTEGER)-2))/64,
	      ERR_ARG_8_BAD_RANGE);  /* avoid overflow, BN~64, see below  */
  
  Primitive_GC_If_Needed( (ncols*sizeof(unsigned char)) + (3*(ncols+4) * sizeof(MINTEGER)) );
  /* 1-row for pdata, and 3 rows of 2+ncols+2 length for er_rows */ 
  /* Primitive_GC_If_Needed takes either number of bytes, or number of scheme pointers,it works either way. */
  pdata = ((unsigned char *) Free);
  er_rows = ((MINTEGER **) (pdata+ncols));
  er_rows[0] = ((MINTEGER *) (er_rows+3));
  er_rows[1] = er_rows[0] + (ncols+4);
  er_rows[2] = er_rows[1] + (ncols+4);
  
  C_image_ht_ibn_atxy_wmm(Array, pdata, nrows,ncols,
			  ((float) x_at), ((float) y_at),  Min,Max,
			  HG,BNmethod, er_rows, PREC_SCALE);
  PRIMITIVE_RETURN(SHARP_T);
}


/* THE FOLLOWING 3 ROUTINES ARE THE OLD 16-color drawing routines
   they also do magnification.
   */

/* color_table entries 0 and 1 are not used */
/* Just like in array-plotting,
   Use Min,Max and Offset,Scale s.t. values map into [2,15] */

#define SCREEN_BACKGROUND_COLOR 0
#define MINIMUM_INTENSITY_INDEX 2
#define MAXIMUM_INTENSITY_INDEX 15

/* ARGS = (image x_at y_at magnification) magnification can be 1, 2, or 3 
 */

DEFINE_PRIMITIVE ("DRAW-MAGNIFY-IMAGE-AT-XY", Prim_draw_magnify_image_at_xy, 4, 4, 0)
{
  REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray, Answer;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array;
  unsigned char *pdata;
  int Error_Number;
  long Magnification;
  REAL Offset, Scale;		/* To make intensities fit in [2,15] */
  Primitive_4_Args();

  Arg_1_Type(TC_LIST);		/* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray);                                         /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at);                   /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at);                   /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  Arg_4_Type(TC_FIXNUM);
  Range_Check(Magnification, Arg4, 1, 100, ERR_ARG_4_BAD_RANGE);
  
  Length = nrows*ncols;

  { REAL Array_Min, Array_Max;
    long nmin, nmax;
    
    C_Array_Find_Min_Max(Array, Length, &nmin, &nmax);
    Array_Min = Array[nmin];  Array_Max = Array[nmax];
    Find_Offset_Scale_For_Linear_Map(Array_Min, Array_Max,
				     2.0, 15.0, &Offset, &Scale);  /* Do not use colors 0 and 1 */
    
    Primitive_GC_If_Needed( (Magnification*ncols) * sizeof(unsigned char) ); 
    pdata = ((unsigned char *) Free);
    Image_Draw_Magnify_N_Times_With_Offset_Scale(Array, pdata, nrows, ncols,
						 ((float) x_at), ((float) y_at),
						 Offset, Scale,
						 Magnification);    
    PRIMITIVE_RETURN(SHARP_T);
  }
}

DEFINE_PRIMITIVE ("DRAW-MAGNIFY-IMAGE-AT-XY-WITH-MIN-MAX", Prim_draw_magnify_image_at_xy_with_min_max, 6, 6, 0)
{
  REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray, Answer;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array, Offset, Scale, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  long Magnification;
  
  Primitive_6_Args();
  Arg_1_Type(TC_LIST);		/* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray); /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE); /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE); /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at); /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at); /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
 
  Error_Number = Scheme_Number_To_REAL(Arg4, &Min);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &Max);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  Arg_6_Type(TC_FIXNUM);
  Range_Check(Magnification, Arg6, 1, 100, ERR_ARG_6_BAD_RANGE);
  
  Length = nrows*ncols;
  
  /* NOW MAKE THE PICTURE, CLIPPING MIN, MAX */ 
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   2.0, 15.0, &Offset, &Scale);	/* Do not use colors 0 and 1 */
  
  Primitive_GC_If_Needed( (Magnification*ncols) * sizeof(unsigned char) ); 
  pdata = ((unsigned char *) Free);
  Image_Draw_Magnify_N_Times_With_Offset_Scale(Array, pdata, nrows, ncols,
					       ((float) x_at), ((float) y_at), 
					       Offset, Scale,
					       Magnification);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("DRAW-MAGNIFY-IMAGE-AT-XY-ONLY-BETWEEN-MIN-MAX", Prim_draw_magnify_image_at_xy_only_between_min_max, 6, 6, 0)
{
  REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray, Answer;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array, Offset, Scale, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  long Magnification;
  
  Primitive_6_Args();
  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != EMPTY_LIST) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Array = Scheme_Array_To_C_Array(Parray);                                         /* ARRAY */
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NROWS */
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);                         /* NCOLS */
  Error_Number = Scheme_Number_To_REAL(Arg2, &x_at);                   /* X_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &y_at);                   /* Y_AT */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
 
  Error_Number = Scheme_Number_To_REAL(Arg4, &Min);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_4_WRONG_TYPE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &Max);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  Arg_6_Type(TC_FIXNUM);
  Range_Check(Magnification, Arg6, 1, 100, ERR_ARG_6_BAD_RANGE);
  Length = nrows*ncols;
  
  /* NOW MAKE THE PICTURE, CLIPPING MIN, MAX */ 
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   2.0, 15.0, &Offset, &Scale);  /* Do not use colors 0 and 1 */
  
  Primitive_GC_If_Needed( (Magnification*ncols) * sizeof(unsigned char) ); 
  pdata = ((unsigned char *) Free);
  Image_Draw_Magnify_N_Times_With_Offset_Scale_Only(Array, pdata, nrows, ncols,
						    ((float) x_at), ((float) y_at), 
						    Offset, Scale,
						    Magnification);
  PRIMITIVE_RETURN(SHARP_T);
}


/*_______________________________________________________________________________*/
/* Below are the real drawing routines */

/* ht = halftoning
   od = ordered-dither (dispersed dot), Ulichney terminology
   bn = blue noise (also called: minimized average error)
   psam = pulse surface area modulation
     Also, there are the old drawing routines for 16 colors, which are basically 
     fixed threshold ordered dither.
   */

/* The macro Adjust_Value_Wmm is used by most drawing routines.
   The macro Adjust_Value_Womm is used only by psam-atxy-womm.
   REAL value, newvalue, ngreys_min, ngreys_max, Vmin,Vmax, offset,scale; 
   offset, scale must be such as to map (min,max) into (ngreys_min,ngreys_max)
   */
#define Adjust_Value_Wmm(value, newvalue, ngreys_min, ngreys_max, Vmin, Vmax, offset, scale) \
{ if      (value >= Vmax)     newvalue = ngreys_max;   \
  else if (value <= Vmin)     newvalue = ngreys_min;   \
  else                        newvalue = offset + (value * scale); }
#define Adjust_Value_Womm(value, newvalue, ngreys_min, ngreys_max, Vmin, Vmax, offset, scale) \
{ if      (value >= Vmax)     newvalue = ngreys_min;    \
  else if (value <= Vmin)     newvalue = ngreys_min;    \
  else                        newvalue = offset + (value * scale); }


/* The following geometrical map is slightly tricky.
 */
void Find_Offset_Scale_For_Linear_Map(Min,Max, New_Min, New_Max, Offset, Scale)
     REAL Min,Max, New_Min, New_Max, *Offset, *Scale;
{ /* no local variables */
  if ((Min == Max) && (Max == 0.0))
  { *Scale = 0.0; *Offset = (New_Max + New_Min) / 2.0; }
  else if (Min == Max)
  { *Scale = 0.25 * (mabs( (New_Max - New_Min) / Max ) );
    *Offset = (New_Max + New_Min) / 2.0; }
  else
  { *Scale  = (New_Max - New_Min) / (Max - Min);
    *Offset = New_Min- ((*Scale) * Min); }
}

#define Round_REAL(x) ((long) ((x >= 0) ? (x+.5) : (x-.5)))

/* Ordered Dither MASKS 
   A mask is a SQUARE matrix of threshold values, 
   that is effectively replicated periodically all over the image.
   
   ht_od_table[][0] --->  int SG;               number of soft greys
   ht_od_table[][1] --->   int SGarray_nrows;     nrows=ncols i.e. square matrix of threshold values
   ht_od_table[][2+i] ----> int SGarray[36];      threshold values with range [0,SG).
   
   ATTENTION: Currently, the LARGEST SGarray is 6X6 MATRIX
  */

static int ht_od_table[8][2+36] = 
{ {2,1, 1},			/* fixed threshold at halfpoint */ 
  {3,2, 1,2,2,1},		/* this one and following 4 come from Ulichney p.135 */
  {5,3, 2,3,2, 4,1,4, 2,3,2},
  {9,4, 1,8,2,7, 5,3,6,4, 2,7,1,8, 6,4,5,3},
  {17,5, 2,16,3,13,2, 10,6,11,7,10, 4,14,1,15,4, 12,8,9,5,12, 2,16,3,13,2},
  {33,6, 1,30,8,28,2,29, 17,9,24,16,18,10, 5,25,3,32,6,26, 21,13,19,11,22,14, 2,29,7,27,1,30, 18,10,23,15,17,9},
  {4,2, 0,2,3,1},		/* this one and following 1 come from Jarvis,Judice,Ninke: CGIP 5, p.23 */
  {17,4, 0,8,2,10, 12,4,14,6, 3,11,1,9, 15,7,13,5}
};
#define HT_OD_TABLE_MAX_INDEX 7

/* ordered dither
   pdata must have length ncols 
   HG= Hardware Grey levels (output pixel values 0,HG-1) 
   ODmethod is index for ht_od method 
   */

C_image_ht_od_atxy_wmm(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG,ODmethod)
     REAL Array[], Min,Max; 
     unsigned char *pdata; 
     int nrows,ncols,HG,ODmethod;
     float x_at,y_at;
{ int i,j, SG, *SGarray, SGarray_nrows, dither, pixel, array_index;
  REAL    REAL_pixel, value, offset,scale, HG1_SG; 
  /* static int ht_od_table[][]; */
  /* void Find_Offset_Scale_For_Linear_Map(); */
  
  if (ODmethod>HT_OD_TABLE_MAX_INDEX) {printf("HT_OD methods 0,7 only\n");fflush(stdout);return(0);}; 
  SG = ht_od_table[ODmethod][0];
  SGarray_nrows = ht_od_table[ODmethod][1]; /* nrows=ncols   */
  SGarray = &(ht_od_table[ODmethod][2]);    /* square matrix */
  
  HG1_SG = ((REAL) ((HG-1)*SG));
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, HG1_SG,  &offset, &scale); /* HG output greys */
  array_index=0;
  for (i=0; i<nrows; i++)
  { for (j=0; j<ncols; j++)
    { value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_SG, Min,Max, offset,scale);
      pixel = ((long) REAL_pixel); /* Turn into integer--- integer arithmetic gives speed */
      if (pixel == HG1_SG) pixel = pixel-1;	/* this special case is necessary to avoid ouput_pxl greater than.. */
      /* */
      dither = SGarray[ (i%SGarray_nrows)*SGarray_nrows + (j%SGarray_nrows) ];
      /*         Array[        row_index            *    ncols     +    column_index ] ---- Read threshold value */
      /* */
      pdata[j] = ((unsigned char) ((pixel + SG - dither) / SG)); /* integer division */ }
    block_write(screen_handle, x_at, (y_at-((float) i)), ncols, 1, pdata, 0);
  }
}

/* Blue Noise (minimized average error)
   pdata must have length ncols 
   HG= Hardware Grey levels (output pixel values 0,HG-1) 
   BNmethod is index for ht_bn method 
   */
/* 
  er_rows[][] should be 3 arrays of integers, of length (ncols+2*ER_C), which store previous errors, (ALLOCATED STORAGE)
  ER_R is number of error rows, (currently 3)
  ER_C is number of extra columns (to the left and to the right) of each er_row,
  they always contain ZEROS and serve to simplify the error summing process, i.e. we don't have
  to check for i,j bounds at edges, (no conditionals in the sum loop).
  Also, the code handles all cases in a uniform manner.
  (for better explanation get pas halftoning notes)
  */
C_image_ht_bn_atxy_wmm(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG,BNmethod, er_rows)
     REAL Array[], Min,Max;
     unsigned char *pdata; 
     int nrows,ncols,HG,BNmethod;
     float x_at,y_at,  **er_rows;
{ /* no local vars */
  if (BNmethod==0)
    C_image_ht_bn_atxy_wmm_0_(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG, er_rows);
  else if (BNmethod==1)
    C_image_ht_bn_atxy_wmm_1_(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG, er_rows);
  else if (BNmethod==2)
    C_image_ht_bn_atxy_wmm_2_(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG, er_rows);
  else
  {printf("HT_BN methods 0,1,2 only\n");fflush(stdout);}; 
}

/* 
  the following 3 routines are identical, 
  except for the mask weight numbers in computing ersum,
  the sole reason for this duplication is speed (if any)
  */

/* FLOYD-STEINBERG-75
 */
C_image_ht_bn_atxy_wmm_0_(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG, er_rows)
     REAL Array[], Min,Max;
     unsigned char *pdata; 
     int nrows,ncols,HG;
     float x_at,y_at,  **er_rows;
{ int i,j, m, array_index;
  int row_offset, col_offset, INT_pixel;
  REAL    REAL_pixel, value, offset,scale, HG1_2;
  float ersum, weight, pixel, *temp;
  static int ER_R=3, ER_R1=2, ER_C=2, ER_C1=1;
  
  for (i=0;i<ER_R;i++) 
    for (j=0;j<ncols+(2*ER_C);j++) er_rows[i][j] = 0.0; /* initialize error rows */
  HG1_2 = ((REAL) ((HG-1)*2));	/* notice this is REAL number */
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, HG1_2,  &offset, &scale); /* HG output greys */
  array_index=0;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ersum = (1.0/16.0)*er_rows[ER_R1+(-1)][ER_C+(-1)+j] + (5.0/16.0)*er_rows[ER_R1+(-1)][ER_C+(0)+j]
	+ (3.0/16.0)*er_rows[ER_R1+(-1)][ER_C+(1)+j] + (7.0/16.0)*er_rows[ER_R1+(0)][ER_C+(-1)+j];
      /*      this encodes the FLOYD-STEINBERG-75 mask for computating the average error correction */ 
      value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_2, Min,Max, offset,scale);
      /* */      
      pixel = ((float) REAL_pixel) + ersum; /*     corrected intensity */
      INT_pixel = ((long) ((pixel + 1) / 2.0));	/* the (long) does truncation, this corresponds to "IF J>R/2 R 0" */
      pdata[j] = ((unsigned char) INT_pixel); /*   output pixel to be painted */
      er_rows[ER_R1][ER_C +j] = (pixel/2.0) - ((float) INT_pixel); /*  error estimate */
    }
    block_write(screen_handle, x_at, (y_at-((float) i)), ncols, 1, pdata, 0); /* paint a row */
    /* */      
    temp = er_rows[0];		/* rotate rows */
    er_rows[0] = er_rows[1];
    er_rows[1] = er_rows[2];
    er_rows[2] = temp;
    for (m=ER_C;m<ncols;m++) er_rows[2][m]=0.0; /* initialize (clean up) the new error row */
  }
}

/* JARVIS-JUDICE-NINKE-76 mask
 */
C_image_ht_bn_atxy_wmm_1_(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG, er_rows)
     REAL Array[], Min,Max;
     unsigned char *pdata; 
     int nrows,ncols,HG;
     float x_at,y_at,  **er_rows;
{ int i,j, m, array_index;
  int row_offset, col_offset, INT_pixel;
  REAL    REAL_pixel, value, offset,scale, HG1_2;
  float ersum, weight, pixel, *temp;
  static int ER_R=3, ER_R1=2, ER_C=2, ER_C1=1;
  
  for (i=0;i<ER_R;i++) 
    for (j=0;j<ncols+(2*ER_C);j++) er_rows[i][j] = 0.0; /* initialize error rows */
  HG1_2 = ((REAL) ((HG-1)*2));	/* notice this is REAL number */
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, HG1_2,  &offset, &scale); /* HG output greys */
  array_index=0;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ersum = (1.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(-2)+j] + (3.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(-1)+j] +
	(5.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(0)+j] + (3.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(1)+j] +
	  (1.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(2)+j] /* first row ends */
	    + (3.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(-2)+j] + (5.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(-1)+j] + 
	      (7.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(0)+j] + (5.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(1)+j] + 
		(3.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(2)+j] /* second row ends */
		  + (5.0/48.0)*er_rows[ER_R1+(0)][ER_C+(-2)+j] + (7.0/48.0)*er_rows[ER_R1+(0)][ER_C+(-1)+j];
      /*      this encodes the JARVIS-JUDICE-NINKE-76 mask for computating the average error correction */ 
      value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_2, Min,Max, offset,scale);
      /* */      
      pixel = ((float) REAL_pixel) + ersum; /*     corrected intensity */
      INT_pixel = ((long) ((pixel + 1) / 2.0));	/* the (long) does truncation, this corresponds to "IF J>R/2 R 0" */
      pdata[j] = ((unsigned char) INT_pixel); /*   output pixel to be painted */
      er_rows[ER_R1][ER_C +j] = (pixel/2.0) - ((float) INT_pixel); /*  error estimate */
    }
    block_write(screen_handle, x_at, (y_at-((float) i)), ncols, 1, pdata, 0); /* paint a row */
    /* */      
    temp = er_rows[0];		/* rotate rows */
    er_rows[0] = er_rows[1];
    er_rows[1] = er_rows[2];
    er_rows[2] = temp;
    for (m=ER_C;m<ncols;m++) er_rows[2][m]=0.0; /* initialize (clean up) the new error row */
  }
}

/* STUCKI-81 mask
 */
C_image_ht_bn_atxy_wmm_2_(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG, er_rows)
     REAL Array[], Min,Max;
     unsigned char *pdata; 
     int nrows,ncols,HG;
     float x_at,y_at,  **er_rows;
{ int i,j, m, array_index;
  int row_offset, col_offset, INT_pixel;
  REAL    REAL_pixel, value, offset,scale, HG1_2;
  float ersum, weight, pixel, *temp;
  static int ER_R=3, ER_R1=2, ER_C=2, ER_C1=1;
  
  for (i=0;i<ER_R;i++) 
    for (j=0;j<ncols+(2*ER_C);j++) er_rows[i][j] = 0.0; /* initialize error rows */
  HG1_2 = ((REAL) ((HG-1)*2));	/* notice this is REAL number */
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, HG1_2,  &offset, &scale); /* HG output greys */
  array_index=0;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ersum = (1.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(-2)+j] + (2.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(-1)+j] +
	(4.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(0)+j] + (2.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(1)+j] +
	  (1.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(2)+j] /* first row ends */
	    + (2.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(-2)+j] + (4.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(-1)+j] + 
	      (8.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(0)+j] + (4.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(1)+j] + 
		(2.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(2)+j] /* second row ends */
		  + (4.0/42.0)*er_rows[ER_R1+(0)][ER_C+(-2)+j] + (8.0/42.0)*er_rows[ER_R1+(0)][ER_C+(-1)+j];
      /*      this encodes the STUCKI-81 mask for computating the average error correction */ 
      value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_2, Min,Max, offset,scale);
      /* */      
      pixel = ((float) REAL_pixel) + ersum; /*     corrected intensity */
      INT_pixel = ((long) ((pixel + 1) / 2.0));	/* the (long) does truncation, this corresponds to "IF J>R/2 R 0" */
      pdata[j] = ((unsigned char) INT_pixel); /*   output pixel to be painted */
      er_rows[ER_R1][ER_C +j] = (pixel/2.0) - ((float) INT_pixel); /*  error estimate */
    }
    block_write(screen_handle, x_at, (y_at-((float) i)), ncols, 1, pdata, 0); /* paint a row */
    /* */      
    temp = er_rows[0];		/* rotate rows */
    er_rows[0] = er_rows[1];
    er_rows[1] = er_rows[2];
    er_rows[2] = temp;
    for (m=ER_C;m<ncols;m++) er_rows[2][m]=0.0; /* initialize (clean up) the new error row */
  }
}


/* INTEGER BLUE NOISE 
   pdata must have length ncols 
   HG= Hardware Grey levels (output pixel values 0,HG-1) 
   BNmethod is index for ht_ibn method 

   IBN = integer blue noise
   uses integer arithmetic for speed, but also has different effect 
   depending on the scaling of the integer intensities and error-corrections.
   A scale of PREC_SCALE=4 gives a very clear picture, with EDGE-INHANCEMENT.
   */

/*
  ht_ibn_table[][0] --->  int BN;               sum of error weights
  ht_ibn_table[][1] --->   int BNentries;       number of weight entries 
  ht_ibn_table[][2+i+0,1,2] ----> int row_offset,col_offset,weight;
  */

static int ht_ibn_table[3][2+(3*12)] = 
{ {16,4,  -1,-1,1, -1,0,5,  -1,1,3,  0,-1,7},
  {48,12, -2,-2,1, -2,-1,3, -2,0,5, -2,1,3, -2,2,1,
          -1,-2,3, -1,-1,5, -1,0,7, -1,1,5, -1,2,3,
           0,-2,5,  0,-1,7},
  {42,12, -2,-2,1, -2,-1,2, -2,0,4, -2,1,2, -2,2,1,
          -1,-2,2, -1,-1,4, -1,0,8, -1,1,4, -1,2,2,
           0,-2,4,  0,-1,8}
};

/* 
  er_rows[][] should be 3 arrays of integers, of length (ncols+2*ER_C), which store previous errors, (ALLOCATED STORAGE)
  ER_R is number of error rows, (currently 3)
  ER_C is number of extra columns (to the left and to the right) of each er_row,
  they always contain ZEROS and serve to simplify the error summing process, i.e. we don't have
  to check for i,j bounds at edges, (no conditionals in the sum loop).
  Also, the code handles all cases in a uniform manner.
  (for better explanation get pas halftoning notes)
  */
C_image_ht_ibn_atxy_wmm(Array, pdata, nrows,ncols, x_at,y_at, Min,Max, HG,BNmethod, er_rows, PREC_SCALE)
     REAL Array[], Min,Max; 
     unsigned char *pdata; 
     int nrows,ncols,HG,BNmethod;
     MINTEGER   **er_rows, PREC_SCALE;
     float x_at,y_at;
{ int i,j, m, BNentries, array_index, row_offset, col_offset;
  MINTEGER  BN, ersum, weight, PREC_2, PREC, *temp, pixel;  
  /* PREC is a scale factor that varies the precision in ersum --  using integer arithmetic for speed */
  REAL    REAL_pixel, value, offset,scale, HG1_2_PREC;
  static int ER_R=3, ER_R1=2, ER_C=2, ER_C1=1;
  
  for (i=0;i<ER_R;i++) 
    for (j=0;j<ncols+(2*ER_C);j++) er_rows[i][j] = 0; /* initialize error rows */
  BN = ((MINTEGER) ht_ibn_table[BNmethod][0]);
  BNentries = ht_ibn_table[BNmethod][1];
  /* */
  HG1_2_PREC = ((REAL) PREC_SCALE);
  /* HG1_2_PREC = ((REAL) ( (1<<( 8*(sizeof(MINTEGER))-1 )) / BN)); */
  /* max_intensity   maps to  (max_integer/BN), so that */
  PREC_2 = ((MINTEGER) HG1_2_PREC) / ((MINTEGER) (HG-1));     /* neither ersum*BN nor (max_intensity + ersum) overflow */
  PREC   = PREC_2 / 2;
  /* */
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, HG1_2_PREC,  &offset, &scale); /* HG output greys */
  array_index=0;  
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ersum=0;
      for (m=0;m<(3*BNentries); m=m+3) { row_offset =             ht_ibn_table[BNmethod][2+m+0]; /* should be 0,1,2 */
					 col_offset =             ht_ibn_table[BNmethod][2+m+1];
					 weight     = ((MINTEGER) ht_ibn_table[BNmethod][2+m+2]);
					 ersum += weight * er_rows[ER_R1+row_offset][ER_C +j+ col_offset]; } /*ATT*/
      ersum = ersum / BN;
      /* */
      value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_2_PREC, Min,Max, offset,scale);
      pixel = ((MINTEGER) REAL_pixel); /* Turn into integer--- integer arithmetic gives speed */
      /* */      
      pixel = pixel + ersum;	/* corrected intensity */
      ersum = ((pixel + PREC) / PREC_2); /* integer division -- ersum is used as temp -- */ 
      pdata[j] = ((unsigned char) ersum); /* output pixel to be painted -- range is 0,HG1 -- */
      er_rows[ER_R1][ER_C +j] = pixel - (PREC_2*ersum); /*  error estimate */
    }
    block_write(screen_handle, x_at, (y_at-((float) i)), ncols, 1, pdata, 0); /* paint a row */
    /* */      
    temp = er_rows[0];		/* rotate rows */
    er_rows[0] = er_rows[1];
    er_rows[1] = er_rows[2];
    er_rows[2] = temp;
    for (m=0;m<(ncols+(2*ER_C));m++) er_rows[2][m]=0; /* initialize (clean up) the new error row */
  }
}



/* PSAM drawing (see scheme primitives above, for description)
   Pdata must be 4*16*ncols in size.
   */
C_image_psam_atxy_wmm(Array, pdata, nrows, ncols, x_origin, y_origin, Min,Max)
     REAL Array[], Min,Max;
     unsigned char *pdata; /* pdata should have length 16*4*ncols */
     long nrows, ncols;
     float x_origin, y_origin;
{ register long i,j, i4;
  register long array_index, pdata_index;
  long ncols4 = 4 * ncols;
  long color_index;
  REAL REAL_pixel, value, offset,scale;

  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, 15.0,  &offset, &scale); /* 16 grey levels */
  
  array_index=0;    i4=0;
  for (i=0; i<nrows; i++) 
  { pdata_index = 0;
    for (j=0; j<ncols; j++) 
    { value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, 15.0, Min,Max, offset,scale);
      color_index = ((long) (REAL_pixel + .5));	/* integer between 0 and 15 */
      /* */
      my_write_dither(pdata, pdata_index, ncols4, color_index);
      pdata_index = pdata_index + 4; /* dependency between this and my_write_dither */
    }
    block_write(screen_handle, x_origin, y_origin-i4, ncols4, 4, pdata, 0);
    i4 = i4+4;
  }
  /* A(i,j) --> Array[i*ncols + j] */
}

/* Same as above, except use Adjust_Value_Womm.
 */
C_image_psam_atxy_womm(Array, pdata, nrows, ncols, x_origin, y_origin, Min,Max)
     REAL Array[], Min,Max;
     unsigned char *pdata; /* pdata should have length 16*4*ncols */
     long nrows, ncols;
     float x_origin, y_origin;
{ register long i,j, i4;
  register long array_index, pdata_index;
  long ncols4 = 4*ncols;
  long color_index;
  REAL REAL_pixel, value, offset,scale;
  
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, 15.0,  &offset, &scale); /* 16 grey levels */
  array_index=0;    i4=0;
  for (i=0; i<nrows; i++) 
  { pdata_index = 0;
    for (j=0; j<ncols; j++) 
    { value = Array[array_index++];
      Adjust_Value_Womm(value, REAL_pixel, 0.0, 15.0, Min,Max, offset,scale);  /* ONLY DIFFERENCE WITH PREVIOUS ONE */
      color_index = ((long) (REAL_pixel + .5));	/* integer between 0 and 15 */
      /* */
      my_write_dither(pdata, pdata_index, ncols4, color_index);
      pdata_index = pdata_index + 4; /* dependency between this and my_write_dither */
    }
    block_write(screen_handle, x_origin, y_origin-i4, ncols4, 4, pdata, 0);
    i4 = i4+4;
  }
  /* A(i,j) --> Array[i*ncols + j] */
}

/* psam dither[11] is left out, { 1,1,0,1, 1,1,1,0, 0,1,1,0, 1,0,1,1 } */

/* The following routine writes a 4x4 dither cell
   in 4 consecutive rows of pdata. It assumes a lot about
   pdata and the other args passed to it. READ carefully.
   Designed TO BE USED BY C_image_psam_atxy_wmm
*/

my_write_dither(pdata, pdata_row_index, ncols , color_index)
     unsigned char *pdata;
     long pdata_row_index, ncols;
     long color_index; /* should be 0 to 15 */
{ static unsigned char dither_table[16][16] = {{ 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 },
					       { 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,0,0,0 },
					       { 0,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,0 },
					       { 0,0,0,0, 0,1,1,0, 0,0,1,0, 0,0,0,0 },
					       { 0,0,0,0, 0,1,1,0, 0,1,1,0, 0,0,0,0 },
					       { 1,0,0,0, 0,1,1,0, 0,1,1,0, 0,0,0,0 },
					       { 1,0,0,0, 0,1,1,0, 0,1,1,0, 0,0,0,1 },
					       { 1,0,0,1, 0,1,1,0, 0,1,1,0, 0,0,0,1 },
					       { 1,0,0,1, 0,1,1,0, 0,1,1,0, 1,0,0,1 },
					       { 1,1,0,1, 0,1,1,0, 0,1,1,0, 1,0,0,1 },
					       { 1,1,0,1, 1,1,1,0, 0,1,1,0, 1,0,0,1 },
					       { 1,1,0,1, 1,1,1,0, 0,1,1,1, 1,0,1,1 },
					       { 1,1,0,1, 1,1,1,0, 1,1,1,1, 1,0,1,1 },
					       { 1,1,1,1, 1,1,1,0, 1,1,1,1, 1,0,1,1 },
					       { 1,1,1,1, 1,1,1,0, 1,1,1,1, 1,1,1,1 },
					       { 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1 }};
  long i, row_start,m;
  long dither_index;		/* do not mix up the counters, indexes */
  dither_index=0;
  for (i=0;i<4;i++) { row_start = pdata_row_index + (i*ncols);
		      for (m=row_start; m<row_start+4; m++) 
			pdata[m] = dither_table[color_index][dither_index++]; }
}


/* Below are the OLD DRAWING ROUTINES for 16 color monitors.
   In effect they are fixed threshold, with 16 HG levels.
   The only difference is they also do magnification by replicating pixels.
   */

/* Image_Draw_Magnify_N_Times : N^2 in area 
 */
Image_Draw_Magnify_N_Times_With_Offset_Scale(Array, pdata, nrows, ncols, 
					     x_origin, y_origin, Offset, Scale, N)
     REAL Array[], Offset, Scale;
     unsigned char *pdata;
     long nrows, ncols, N;
     float x_origin, y_origin;
{ register long i,j,m;
  register long array_index;
  long ncolsN= N * ncols;
  long nrowsN= N * nrows;
  register unsigned char pixel;
  register REAL REAL_pixel;
  
  array_index = 0;
  for (i = 0; i < nrowsN;)	/* note that i is NOT incremented here */
  { for (j = 0; j < ncolsN;)	/* note that j is NOT incremented here */
    { REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)
	pixel = MAXIMUM_INTENSITY_INDEX;
      else if (REAL_pixel < 2.0)
	pixel = MINIMUM_INTENSITY_INDEX;
      else
	pixel = ((unsigned char) (Round_REAL(REAL_pixel)));
      for (m=0; m<N; m++) { pdata[j] = pixel;
			    j++; }
    }
    for (m=0; m<N; m++) {
      block_write(screen_handle, x_origin, y_origin-i, ncolsN, 1, pdata, 0);
      i++; }
    /* A(i,j) --> Array[i*ncols + j] */
  }
}

/* Image_Draw_Magnify_N_Times_Only : N^2 in area 
   This procedure throws away (i.e. maps to SCREEN_BACKGROUND_COLOR) 
   all values outside the range given by Offset,Scale.
   */
Image_Draw_Magnify_N_Times_With_Offset_Scale_Only(Array, pdata, nrows, ncols, 
						  x_origin, y_origin, Offset, Scale, N)
     REAL Array[], Offset, Scale;
     unsigned char *pdata;
     long nrows, ncols, N;
     float x_origin, y_origin;
{ register long i,j,m;
  register long array_index;
  long ncolsN= N * ncols;
  long nrowsN= N * nrows;
  register unsigned char pixel;
  register REAL REAL_pixel;
  
  array_index = 0;
  for (i=0; i<nrowsN;)	/* note that i is NOT incremented here */
  { for (j=0; j<ncolsN;)	/* note that j is NOT incremented here */
    { REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)     pixel = SCREEN_BACKGROUND_COLOR;
      else if (REAL_pixel < 2.0) pixel = SCREEN_BACKGROUND_COLOR;
      else                       pixel = ((unsigned char) (Round_REAL(REAL_pixel)));
      for (m=0; m<N; m++) 
      {	pdata[j] = pixel;
	j++; }
    }
    for (m=0; m<N; m++) {
      block_write(screen_handle, x_origin, y_origin - i, ncolsN, 1, pdata, 0);
      i++; }
    /* A(i,j) --> Array[i*ncols + j] */
  }
}


/* END image drawing
 */


/*_______________________ Grey Level Manipulations _____________________ */

DEFINE_PRIMITIVE ("NEW-COLOR", Prim_new_color, 4, 4, 0)
{ int i, err;
  long index;
  float red, green, blue;
  Primitive_4_Args();

  Range_Check(index, Arg1, STARBASE_COLOR_TABLE_START, (STARBASE_COLOR_TABLE_SIZE - 1), ERR_ARG_1_BAD_RANGE);
  Float_Range_Check(red,   Arg2, 0, 1, ERR_ARG_2_BAD_RANGE);
  Float_Range_Check(green, Arg3, 0, 1, ERR_ARG_3_BAD_RANGE);
  Float_Range_Check(blue,  Arg4, 0, 1, ERR_ARG_4_BAD_RANGE);

  inquire_color_table(screen_handle, STARBASE_COLOR_TABLE_START, STARBASE_COLOR_TABLE_SIZE, Color_Table);
  Color_Table[index][0] = red;
  Color_Table[index][1] = green;
  Color_Table[index][2] = blue;
  define_color_table(screen_handle, STARBASE_COLOR_TABLE_START, STARBASE_COLOR_TABLE_SIZE, Color_Table);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("INQUIRE-COLORS", Prim_inquire_colors, 0, 0, 0)
{ int i;
  Primitive_0_Args();

  inquire_color_table(screen_handle, STARBASE_COLOR_TABLE_START, STARBASE_COLOR_TABLE_SIZE, Color_Table);
  for (i = 0; i < STARBASE_COLOR_TABLE_SIZE; i++) 
    printf("%d  %f %f %f\n", i, Color_Table[i][0], Color_Table[i][1], Color_Table[i][2]); /* implem. dependent */
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("INQUIRE-COLOR", Prim_inquire_color, 1, 1, 0)
{ int i; int index;
  Pointer Answer, *Orig_Free;
  REAL red, green, blue;
  Primitive_1_Args();

  Arg_1_Type(TC_FIXNUM);
  Range_Check(index, Arg1, STARBASE_COLOR_TABLE_START,
	      (STARBASE_COLOR_TABLE_SIZE-1), ERR_ARG_1_BAD_RANGE);
  inquire_color_table(screen_handle, STARBASE_COLOR_TABLE_START,
		      STARBASE_COLOR_TABLE_SIZE, Color_Table);
  red   = ((REAL) Color_Table[index][0]);
  green = ((REAL) Color_Table[index][1]);
  blue  = ((REAL) Color_Table[index][2]);
  Primitive_GC_If_Needed(6);
  Answer = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free += 6;
  Store_Reduced_Flonum_Result(red, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Store_Reduced_Flonum_Result(green, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Store_Reduced_Flonum_Result(blue, *Orig_Free);
  Orig_Free++;
  *Orig_Free = EMPTY_LIST;
  PRIMITIVE_RETURN(Answer);
}

DEFINE_PRIMITIVE ("READ-COLORS-FROM-FILE", Prim_read_colors_from_file, 1, 1, 0)
{ long i;
  FILE *fopen(), *fp;
  char *file_string;
  Boolean Open_File();
  Primitive_1_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  if (!(Open_File(Arg1, "r", &fp)))
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (feof(fp)!=0)
  {
    fprintf(stderr, "\nColor Datafile is empty!\n");
    Primitive_Error(ERR_EXTERNAL_RETURN);
  }
  for (i = 0; i < STARBASE_COLOR_TABLE_SIZE; i++) 
    fscanf(fp,"%f %f %f\n", &Color_Table[i][0],
	   &Color_Table[i][1], &Color_Table[i][2]);
  Close_File(fp);		/*    fflush(stdout); */
  define_color_table(screen_handle, STARBASE_COLOR_TABLE_START,
		     STARBASE_COLOR_TABLE_SIZE, Color_Table);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("SAVE-COLORS-IN-FILE", Prim_save_colors_in_file, 1, 1, 0)
{ long i;
  FILE *fopen(), *fp;
  char *file_string;
  Boolean Open_File();
  Primitive_1_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  if (!(Open_File(Arg1, "r", &fp)))
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  inquire_color_table(screen_handle, STARBASE_COLOR_TABLE_START,
		      STARBASE_COLOR_TABLE_SIZE, Color_Table);
  for (i = 0; i < STARBASE_COLOR_TABLE_SIZE; i++) 
    fprintf(fp,"%f %f %f\n", Color_Table[i][0], Color_Table[i][1], Color_Table[i][2]);
  Close_File(fp);                 
  PRIMITIVE_RETURN(SHARP_T);
}
/* END */
