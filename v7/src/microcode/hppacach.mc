/* $Id: hppacach.mc,v 1.2 1993/11/11 05:50:54 gjr Exp $ */

{
  "9000/850",

  {
    /*
      I-cache information:
	size		131072 bytes (128 K).
	conf		0x01410002
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		2 associations per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It does not issue coherent operations.
      Only FDC needs to be used to flush.
    */
    { 131072, 0x01410002, 0x0, 32, 2048, 2 },
    /*
      D-cache information:
	size		131072 bytes (128 K).
	conf		0x01410002
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		2 associations per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It does not issue coherent operations.
      Only FDC needs to be used to flush.
    */
    { 131072, 0x01410002, 0x0, 32, 2048, 2 },
    /*
      I-TLB information:
	size		2048 entries (2 K).
	conf		0x00020001
	sp_base		0x0
	sp_stride	1
	sp_count	1
	off_base	0x0
	off_stride	2048
	off_count	2048
	loop		1 association per entry.
      It does not issue coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 2048, 0x00020001, 0x0, 1, 1, 0x0, 2048, 2048, 1 },
    /*
      D-TLB information:
	size		2048 entries (2 K).
	conf		0x00020001
	sp_base		0x0
	sp_stride	1
	sp_count	1
	off_base	0x0
	off_stride	2048
	off_count	2048
	loop		1 association per entry.
      It does not issue coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 2048, 0x00020001, 0x0, 1, 1, 0x0, 2048, 2048, 1 }
  }};

{
  "9000/835",

  {
    /*
      I-cache information:
	size		131072 bytes (128 K).
	conf		0x01410002
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		2 associations per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It does not issue coherent operations.
      Only FDC needs to be used to flush.
    */
    { 131072, 0x01410002, 0x0, 32, 2048, 2 },
    /*
      D-cache information:
	size		131072 bytes (128 K).
	conf		0x01410002
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		2 associations per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It does not issue coherent operations.
      Only FDC needs to be used to flush.
    */
    { 131072, 0x01410002, 0x0, 32, 2048, 2 },
    /*
      I-TLB information:
	size		2048 entries (2 K).
	conf		0x00020001
	sp_base		0x0
	sp_stride	1
	sp_count	1
	off_base	0x0
	off_stride	2048
	off_count	2048
	loop		1 association per entry.
      It does not issue coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 2048, 0x00020001, 0x0, 1, 1, 0x0, 2048, 2048, 1 },
    /*
      D-TLB information:
	size		2048 entries (2 K).
	conf		0x00020001
	sp_base		0x0
	sp_stride	1
	sp_count	1
	off_base	0x0
	off_stride	2048
	off_count	2048
	loop		1 association per entry.
      It does not issue coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 2048, 0x00020001, 0x0, 1, 1, 0x0, 2048, 2048, 1 }
  }};

{
  "9000/834",

  {
    /*
      I-cache information:
	size		131072 bytes (128 K).
	conf		0x01410002
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		2 associations per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It does not issue coherent operations.
      Only FDC needs to be used to flush.
    */
    { 131072, 0x01410002, 0x0, 32, 2048, 2 },
    /*
      D-cache information:
	size		131072 bytes (128 K).
	conf		0x01410002
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		2 associations per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It does not issue coherent operations.
      Only FDC needs to be used to flush.
    */
    { 131072, 0x01410002, 0x0, 32, 2048, 2 },
    /*
      I-TLB information:
	size		2048 entries (2 K).
	conf		0x00020001
	sp_base		0x0
	sp_stride	1
	sp_count	1
	off_base	0x0
	off_stride	2048
	off_count	2048
	loop		1 association per entry.
      It does not issue coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 2048, 0x00020001, 0x0, 1, 1, 0x0, 2048, 2048, 1 },
    /*
      D-TLB information:
	size		2048 entries (2 K).
	conf		0x00020001
	sp_base		0x0
	sp_stride	1
	sp_count	1
	off_base	0x0
	off_stride	2048
	off_count	2048
	loop		1 association per entry.
      It does not issue coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 2048, 0x00020001, 0x0, 1, 1, 0x0, 2048, 2048, 1 }
  }};

{
  "9000/720",

  {
    /*
      I-cache information:
	size		131072 bytes (128 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		4096 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 131072, 0x01402000, 0x0, 32, 4096, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/730",

  {
    /*
      I-cache information:
	size		131072 bytes (128 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		4096 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 131072, 0x01402000, 0x0, 32, 4096, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/750",

  {
    /*
      I-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/710",

  {
    /*
      I-cache information:
	size		32768 bytes (32 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		1024 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 32768, 0x01402000, 0x0, 32, 1024, 1 },
    /*
      D-cache information:
	size		65536 bytes (64 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 65536, 0x01402000, 0x0, 32, 2048, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/877",

  {
    /*
      I-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/705",

  {
    /*
      I-cache information:
	size		32768 bytes (32 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		1024 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 32768, 0x01402000, 0x0, 32, 1024, 1 },
    /*
      D-cache information:
	size		65536 bytes (64 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 65536, 0x01402000, 0x0, 32, 2048, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/735",

  {
    /*
      I-cache information:
	size		262144 bytes (256 K).
	conf		0x71402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	113 lines.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x71402000, 0x0, 32, 8192, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x71402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	113 lines.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x71402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		120 entries (0 K).
	conf		0x000d2000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Either PDTLB or PITLB can be used to purge.
    */
    { 120, 0x000d2000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		120 entries (0 K).
	conf		0x000c2000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Either PDTLB or PITLB can be used to purge.
    */
    { 120, 0x000c2000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/715",

  {
    /*
      I-cache information:
	size		65536 bytes (64 K).
	conf		0x51402000
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		1 association per entry.
	block size	81 lines.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 65536, 0x51402000, 0x0, 32, 2048, 1 },
    /*
      D-cache information:
	size		65536 bytes (64 K).
	conf		0x51402000
	base		0x0
	stride		32 bytes.
	count		2048 entries.
	loop		1 association per entry.
	block size	81 lines.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 65536, 0x51402000, 0x0, 32, 2048, 1 },
    /*
      I-TLB information:
	size		120 entries (0 K).
	conf		0x000d2000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Either PDTLB or PITLB can be used to purge.
    */
    { 120, 0x000d2000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		120 entries (0 K).
	conf		0x000c2000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Either PDTLB or PITLB can be used to purge.
    */
    { 120, 0x000c2000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/867",

  {
    /*
      I-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

{
  "9000/755",

  {
    /*
      I-cache information:
	size		262144 bytes (256 K).
	conf		0x71402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	113 lines.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x71402000, 0x0, 32, 8192, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x71402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	113 lines.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x71402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		120 entries (0 K).
	conf		0x000d2000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Either PDTLB or PITLB can be used to purge.
    */
    { 120, 0x000d2000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		120 entries (0 K).
	conf		0x000c2000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Either PDTLB or PITLB can be used to purge.
    */
    { 120, 0x000c2000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};
