/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/sgraph_a.c,v 1.2 1987/05/29 17:40:27 jinx Rel $ */

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "Sgraph.h"
#include "array.h"

float Color_Table[STARBASE_COLOR_TABLE_SIZE][3];

Define_Primitive(Prim_Plot_Array_In_Box, 3, "PLOT-ARRAY-IN-BOX")
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
  My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = NIL;
  PRIMITIVE_RETURN(Answer);
}

Define_Primitive(Prim_Plot_Array_In_Box_With_Offset_Scale, 5, "PLOT-ARRAY-IN-BOX-WITH-OFFSET-SCALE")
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
  My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = NIL;
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
  if (List != NIL)
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

Define_Primitive(Prim_Clear_Box, 1, "CLEAR-BOX")
{
  float Plotting_Box[4];   /* x_min, y_min, x_max, y_max */
  Primitive_1_Args();

  Arg_1_Type(TC_LIST);
  Get_Plotting_Box(Plotting_Box, Arg1);
  C_Clear_Rectangle(Plotting_Box);
  PRIMITIVE_RETURN(NIL);
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
  clip_rectangle(screen_handle,
		 STARBASE_XMIN, STARBASE_XMAX,
		 STARBASE_YMIN, STARBASE_YMAX);
}

Define_Primitive(Prim_Box_Move, 2, "BOX-MOVE") 
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
  PRIMITIVE_RETURN(NIL);
}

Define_Primitive(Prim_Box_Rotate_Move, 2, "BOX-ROTATE-MOVE") 
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
  PRIMITIVE_RETURN(NIL);
}

/* START image drawing... */

/* color_table entries 0 and 1 are not used */
/* Just like in array-plotting,
   find Min,Max and Offset Scale s.t. values fit in [2,15] intensity values */

#define SCREEN_BACKGROUND_COLOR 0
#define MINIMUM_INTENSITY_INDEX 2
#define MAXIMUM_INTENSITY_INDEX 15

/* ARGS = (image x_at y_at magnification) magnification can be 1, 2, or 3 
 */

Define_Primitive(Prim_Draw_Magnify_Image_At_XY, 4, "DRAW-MAGNIFY-IMAGE-AT-XY")
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
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
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

  {
    REAL Array_Min, Array_Max;
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
    
    Primitive_GC_If_Needed(4);
    Answer = Make_Pointer(TC_LIST, Free);
    Orig_Free = Free;
    Free += 4;
    My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
    Orig_Free++;
    *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
    Orig_Free++;
    My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
    Orig_Free++;
    *Orig_Free = NIL;
    PRIMITIVE_RETURN(Answer);
  }
}

Define_Primitive(Prim_Draw_Magnify_Image_At_XY_With_Min_Max, 6, "DRAW-MAGNIFY-IMAGE-AT-XY-WITH-MIN-MAX")
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
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
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
  Image_Draw_Magnify_N_Times_With_Offset_Scale(Array, pdata, nrows, ncols,
					       ((float) x_at), ((float) y_at), 
					       Offset, Scale,
					       Magnification);
  Primitive_GC_If_Needed(4);
  Answer = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free += 4;
  My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = NIL;
  PRIMITIVE_RETURN(Answer);
}

Define_Primitive(Prim_Draw_Magnify_Image_At_XY_Only_Between_Min_Max, 6, "DRAW-MAGNIFY-IMAGE-AT-XY-ONLY-BETWEEN-MIN-MAX")
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
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
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
  Primitive_GC_If_Needed(4);
  Answer = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free += 4;
  My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free + 1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = NIL;
  PRIMITIVE_RETURN(Answer);
}

/* This is a very tricky procedure, But it's all geometry... 
*/
void
Find_Offset_Scale_For_Linear_Map(Min,Max, New_Min, New_Max, Offset, Scale)
     REAL Min,Max, New_Min, New_Max, *Offset, *Scale;
{
  if ((Min == Max) && (Max == 0.0))
  {
    *Scale = 0.0;                                 
    *Offset = (New_Max + New_Min) / 2.0;
  }
  else if (Min == Max)
  {
    *Scale = 0.25 * (mabs( (New_Max - New_Min) / Max ) );
    *Offset = (New_Max + New_Min) / 2.0;
  }
  else
  {
    *Scale  = (New_Max - New_Min) / (Max - Min);
    *Offset = New_Min- ((*Scale) * Min);
  }
}

/* For B&W monitors there are 3 drawing-primitives similar to the previous 3
   above. BUT, these have a FIXED MAGNIFICATION 4times, and the code
   is rather specialized, hacked especially for the 6003-jupiter-probset.
   It's USE-AND-DON'T-ASK-QUESTIONS.
   I could have used starbase (gescape, fill-color, rectangle, etc) but these
   would have been too slow. So for the sake of efficiency, I hacked the screen bits
   and I am doing my own dithering-algorithm (a 4x4 dither square is drawn for each
   point in the image, giving a total of 16 colors). Draw line (width 4) by line.
   Pdata space needed = (4*ncols*16) .
   */

/* ARGS = (image x_at y_at) 
 */

Define_Primitive(Prim_Draw_BW_Image_At_XY, 3, "DRAW-BW-IMAGE-AT-XY")
{
  REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray, Answer;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array;
  unsigned char *pdata;
  int Error_Number;
  REAL Offset, Scale;                   /* To make intensities fit in [0,15] */
  Primitive_3_Args();

  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
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

  Length = nrows*ncols;

  {
    REAL Array_Min, Array_Max;
    long nmin, nmax;
    C_Array_Find_Min_Max(Array, Length, &nmin, &nmax);
    Array_Min = Array[nmin];  Array_Max = Array[nmax];
    Find_Offset_Scale_For_Linear_Map(Array_Min, Array_Max,
				     0.0, 15.0, &Offset, &Scale);  /* USE COLORS from 0 to 15 */
    
    Primitive_GC_If_Needed( (16*4*ncols) * sizeof(unsigned char) );
    pdata = ((unsigned char *) Free);
    Image_Draw_BW_With_Offset_Scale(Array, pdata, nrows, ncols,
				    ((float) x_at), ((float) y_at),
				    Offset, Scale);
    Primitive_GC_If_Needed(4);
    Answer = Make_Pointer(TC_LIST, Free);
    Orig_Free = Free;
    Free += 4;
    My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
    Orig_Free++;
    *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
    Orig_Free++;
    My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
    Orig_Free++;
    *Orig_Free = NIL;
    PRIMITIVE_RETURN(Answer);
  }
}

Define_Primitive(Prim_Draw_BW_Image_At_XY_With_Min_Max, 5, "DRAW-BW-IMAGE-AT-XY-WITH-MIN-MAX")
{
  REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray, Answer;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array, Offset, Scale, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  
  Primitive_5_Args();
  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
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
  
  /* NOW MAKE THE PICTURE, CLIPPING MIN, MAX */ 
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, 15.0, &Offset, &Scale); /* USE COLORS from 0 to 15 */
  
  Primitive_GC_If_Needed( (16*4*ncols) * sizeof(unsigned char) );
  pdata = ((unsigned char *) Free);
  Image_Draw_BW_With_Offset_Scale(Array, pdata, nrows, ncols,
				  ((float) x_at), ((float) y_at), 
				  Offset, Scale);
  Primitive_GC_If_Needed(4);
  Answer = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free += 4;
  My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = NIL;
  PRIMITIVE_RETURN(Answer);
}

Define_Primitive(Prim_Draw_BW_Image_At_XY_Only_Between_Min_Max, 5, "DRAW-BW-IMAGE-AT-XY-ONLY-BETWEEN-MIN-MAX")
{
  REAL x_at, y_at;
  Pointer Pnrows, Pncols, Prest, Parray, Answer;
  Pointer *Orig_Free;
  long nrows, ncols, Length;
  REAL *Array, Offset, Scale, Min,Max;
  unsigned char *pdata;
  int Error_Number;
  Primitive_5_Args();

  Arg_1_Type(TC_LIST);                                  /* '(nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
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
  
  /* NOW MAKE THE PICTURE, CLIPPING MIN, MAX */ 
  Find_Offset_Scale_For_Linear_Map(Min, Max,
				   0.0, 15.0, &Offset, &Scale); /* USE COLORS from 0 to 15 */
  Primitive_GC_If_Needed( (16*4*ncols) * sizeof(unsigned char) );
  pdata = ((unsigned char *) Free);
  Image_Draw_BW_With_Offset_Scale_Only(Array, pdata, nrows, ncols,
				       ((float) x_at), ((float) y_at),
				       Offset, Scale);
  Primitive_GC_If_Needed(4);
  Answer = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free += 4;
  My_Store_Reduced_Flonum_Result(Offset, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(Scale, *Orig_Free);
  Orig_Free++;
  *Orig_Free = NIL;
  PRIMITIVE_RETURN(Answer);
}

/******************************************************************************/
/* THE REAL DRAWING ROUTINES
 */
#define Round_REAL(x) ((long) ((x >= 0) ? (x+.5) : (x-.5)))

/* Image_Draw_Magnify_N_Times : N^2 in area 
 */

Image_Draw_Magnify_N_Times_With_Offset_Scale(Array, pdata, nrows, ncols, 
					     x_origin, y_origin, Offset, Scale, N)
     REAL Array[], Offset, Scale;
     unsigned char *pdata;
     long nrows, ncols, N;
     float x_origin, y_origin;
{
  register long i,j,m;
  register long array_index;
  long ncolsN= N * ncols;
  long nrowsN= N * nrows;
  register unsigned char pixel;
  register REAL REAL_pixel;
  
  array_index = 0;
  for (i = 0; i < nrowsN;)	/* note that i is NOT incremented here */
  {
    for (j = 0; j < ncolsN;)	/* note that j is NOT incremented here */
    {          
      REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)
	pixel = MAXIMUM_INTENSITY_INDEX;
      else if (REAL_pixel < 2.0)
	pixel = MINIMUM_INTENSITY_INDEX;
      else
	pixel = ((unsigned char) (Round_REAL(REAL_pixel)));
      for (m = 0; m < N; m++)
      {
	pdata[j] = pixel;
	j++;
      }
    }
    for (m = 0; m < N; m++)
    {
      block_write(screen_handle, x_origin, y_origin-i, ncolsN, 1, pdata, 0);
      i++;
    }
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
{
  register long i,j,m;
  register long array_index;
  long ncolsN= N * ncols;
  long nrowsN= N * nrows;
  register unsigned char pixel;
  register REAL REAL_pixel;
  
  array_index = 0;
  for (i = 0; i < nrowsN;)	/* note that i is NOT incremented here */
  {
    for (j = 0; j < ncolsN;)	/* note that j is NOT incremented here */
    {
      REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)
	pixel = SCREEN_BACKGROUND_COLOR;
      else if (REAL_pixel < 2.0)
	pixel = SCREEN_BACKGROUND_COLOR;
      else
	pixel = ((unsigned char) (Round_REAL(REAL_pixel)));
      for (m = 0; m < N; m++)
      {
	pdata[j] = pixel;
	j++;
      }
    }
    for (m = 0; m < N; m++)
    {
      block_write(screen_handle, x_origin, y_origin - i, ncolsN, 1, pdata, 0);
      i++;
    }
    /* A(i,j) --> Array[i*ncols + j] */
  }
}

/* Here are 2 routines for BW monitors. */
/* First, Image_Draw_BW_With_Offset_Scale.
   Pdata must be 4*16*ncols in size.
   */

Image_Draw_BW_With_Offset_Scale(Array, pdata, nrows, ncols, x_origin, y_origin, Offset, Scale)
     REAL Array[], Offset, Scale;
     unsigned char *pdata; /* pdata should have length 16*4*ncols */
     long nrows, ncols;
     float x_origin, y_origin;
{
  register long i,j, i4;
  register long array_index, pdata_index;
  long ncols4 = 4 * ncols;
  long color_index;
  REAL REAL_pixel;
  
  array_index = 0; i4 = 0;
  for (i = 0; i < nrows; i++)
  {
    pdata_index = 0;
    for (j = 0; j < ncols; j++)
    {
      REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)
	color_index = 15;
      else if (REAL_pixel < 0.0)
	color_index = 0;
      else color_index = ((long) (REAL_pixel + .5));
      my_write_dither(pdata, pdata_index, ncols4, color_index);
      pdata_index = pdata_index + 4;      /* dependency between this and my_write_dither */
    }
    block_write(screen_handle, x_origin, y_origin-i4, ncols4, 4, pdata, 0);
    i4 = i4+4;
  }
  /* A(i,j) --> Array[i*ncols + j] */
}

/* Image_Draw_BW_With_Offset_Scale_Only.
   Pdata must be 4*16*ncols in size.
   */
Image_Draw_BW_With_Offset_Scale_Only(Array, pdata, nrows, ncols, x_origin, y_origin, Offset, Scale)
     REAL Array[], Offset, Scale;
     unsigned char *pdata; /* pdata should have length 16*4*ncols */
     long nrows, ncols;
     float x_origin, y_origin;
{
  register long i,j, i4;
  register long array_index, pdata_index;
  long ncols4 = 4*ncols;
  long color_index;
  REAL REAL_pixel;
  
  array_index = 0; i4 = 0;
  for (i = 0;i < nrows;i++)
  {
    pdata_index = 0;
    for (j = 0; j < ncols; j++)
    {
      REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)
	color_index = 0;	/* WITH_OFFSET_SCALE_ONLY */
      else if (REAL_pixel < 0.0)
	color_index = 0;
      else color_index = ((long) (REAL_pixel + .5));
      my_write_dither(pdata, pdata_index, ncols4, color_index);
      pdata_index = pdata_index + 4; /* dependency between this and my_write_dither */
    }
    block_write(screen_handle, x_origin, y_origin-i4, ncols4, 4, pdata, 0);
    i4 = i4 + 4;
  }
  /* A(i,j) --> Array[i*ncols + j] */
}

/* dither 11 is left out { 1,1,0,1, 1,1,1,0, 0,1,1,0, 1,0,1,1 } */

/* The following routine writes a 4x4 dither cell
   in 4 consecutive rows of pdata. It assumes a lot about
   pdata and the other args passed to it. READ carefully.
   Designed TO BE USED ONLY BY Image_Draw_Bw_With_Offset_Scale.
*/

my_write_dither(pdata, pdata_row_index, ncols , color_index)
     unsigned char *pdata;
     long pdata_row_index, ncols;
     long color_index; /* should be 0 to 15 */
{
  static unsigned char dither_table[16][16] = {{ 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 },
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
					     { 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1 }
					     };
  long i, row_start,m;
  long dither_index;		/* do not mix up the counters, indexes */

  dither_index = 0;
  for (i=0;i<4;i++) {
    row_start = pdata_row_index + (i*ncols);
    for (m=row_start; m<row_start+4; m++) 
      pdata[m] = dither_table[color_index][dither_index++];
  }
}
/* END image drawing
 */

/* COLOR (or B&W INTENSITIES) OPERATIONS 
 */

Define_Primitive(Prim_New_Color, 4, "NEW-COLOR")
{
  int i, err;
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
  PRIMITIVE_RETURN(TRUTH);
}

Define_Primitive(Prim_Inquire_Colors, 0, "INQUIRE-COLORS")
{
  int i;
  Primitive_0_Args();

  inquire_color_table(screen_handle, STARBASE_COLOR_TABLE_START, STARBASE_COLOR_TABLE_SIZE, Color_Table);
  for (i = 0; i < STARBASE_COLOR_TABLE_SIZE; i++) 
    printf("%d  %f %f %f\n", i, Color_Table[i][0], Color_Table[i][1], Color_Table[i][2]);     /* implem. dependent */
  PRIMITIVE_RETURN(TRUTH);
}

Define_Primitive(Prim_Inquire_Color, 1, "INQUIRE-COLOR")
{
  int i; int index;
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
  My_Store_Reduced_Flonum_Result(red, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(green, *Orig_Free);
  Orig_Free++;
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  My_Store_Reduced_Flonum_Result(blue, *Orig_Free);
  Orig_Free++;
  *Orig_Free = NIL;
  PRIMITIVE_RETURN(Answer);
}

Define_Primitive(Prim_Read_Colors_From_File, 1, "READ-COLORS-FROM-FILE")
{
  long i;
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
  Close_File(fp);                  /*    fflush(stdout); */
  define_color_table(screen_handle, STARBASE_COLOR_TABLE_START,
		     STARBASE_COLOR_TABLE_SIZE, Color_Table);
  PRIMITIVE_RETURN(TRUTH);
}

Define_Primitive(Prim_Save_Colors_In_File, 1, "SAVE-COLORS-IN-FILE")
{
  long i;
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
  PRIMITIVE_RETURN(TRUTH);
}
/* END */
