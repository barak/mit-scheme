/*  C  */

extern Image_Fast_Transpose();     /* REAL *Array; long nrows; OPTIMIZATION for square images */
extern Image_Transpose();     /* REAL *Array, *New_Array; long nrows, ncols; */
extern Image_Rotate_90clw();     /* REAL *Array, *Rotated_Array; long nrows, ncols; */
extern Image_Rotate_90cclw();     /* REAL *Array, *Rotated_Array; long nrows, ncols; */
extern Image_Mirror();            /* REAL *Array; long nrows, ncols; */

extern Image_Mirror_Upside_Down();     /* Array,nrows,ncols,Temp_Array;
					  REAL *Array,*Temp_Row; long nrows, ncols; */
extern Image_Read_From_CTSCAN_File();  /* FILE *fp; REAL *Array; long nrows, ncols */

extern Image_Rotate_90clw_Mirror();     /* REAL *Array, *Rotated_Array; long nrows, ncols; */

extern Image_Draw_Magnify_N_Times_With_Offset_Scale();
extern Image_Draw_Magnify_N_Times_With_Offset_Scale_Only();
