/* mswindows-specific Lisp objects.
   Copyright (C) 1998 Andy Piper.
   
This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* written by Andy Piper <andyp@parallax.co.uk> plagerising buts from
   glyphs-x.c */

#include <config.h>
#include "lisp.h"
#include "lstream.h"
#include "console-msw.h"
#include "glyphs-msw.h"
#include "objects-msw.h"

#include "buffer.h"
#include "frame.h"
#include "insdel.h"
#include "opaque.h"
#include "sysfile.h"
#include "faces.h"
#include "imgproc.h"

#ifdef FILE_CODING
#include "file-coding.h"
#endif

DEFINE_IMAGE_INSTANTIATOR_FORMAT (bmp);
Lisp_Object Qbmp;
Lisp_Object Vmswindows_bitmap_file_path;
static	COLORREF transparent_color = RGB (1,1,1);

static void
mswindows_initialize_dibitmap_image_instance (struct Lisp_Image_Instance *ii,
					    enum image_instance_type type);
static void
mswindows_initialize_image_instance_mask (struct Lisp_Image_Instance* image, 
					  struct frame* f);

COLORREF mswindows_string_to_color (CONST char *name);

/************************************************************************/
/* convert from a series of RGB triples to a BITMAPINFO formated for the*/
/* proper display 							*/
/************************************************************************/
static BITMAPINFO* convert_EImage_to_DIBitmap (Lisp_Object device,
					       int width, int height,
					       unsigned char *pic,
					       int *bit_count,
					       unsigned char** bmp_data)
{
  struct device *d = XDEVICE (device);
  int i,j;
  RGBQUAD* colortbl;
  int		ncolors;
  BITMAPINFO*	bmp_info;
  unsigned char *ip, *dp;

  if (DEVICE_MSWINDOWS_BITSPIXEL (d) > 0)
    {
      int bpline=(int)(~3UL & (unsigned long)((width*3) +3));
      /* FIXME: we can do this because 24bpp implies no colour table, once
       * we start paletizing this is no longer true. The X versions of
       * this function quantises to 256 colours or bit masks down to a
       * long. Windows can actually handle rgb triples in the raw so I
       * don't see much point trying to optimize down to the best
       * structure - unless it has memory / color allocation implications
       * .... */
      bmp_info=xnew_and_zero (BITMAPINFO);
      
      if (!bmp_info)
	{
	  return NULL;
	}

      bmp_info->bmiHeader.biBitCount=24; /* just RGB triples for now */
      bmp_info->bmiHeader.biCompression=BI_RGB; /* just RGB triples for now */
      bmp_info->bmiHeader.biSizeImage=width*height*3; 

      /* bitmap data needs to be in blue, green, red triples - in that
	 order, eimage is in RGB format so we need to convert */
      *bmp_data = xnew_array_and_zero (unsigned char, bpline * height);
      *bit_count = bpline * height;

      if (!bmp_data)
	{
	  xfree (bmp_info);
	  return NULL;
	}

      ip = pic;
      for (i = height-1; i >= 0; i--) {
	dp = (*bmp_data) + (i * bpline);
	for (j = 0; j < width; j++) {
	  dp[2] =*ip++;
	  dp[1] =*ip++;
	  *dp   =*ip++;
	  dp += 3;
	}
      }
    }
  else				/* scale to 256 colors */
    {
      int rd,gr,bl;
      quant_table *qtable;
      int bpline= (int)(~3UL & (unsigned long)(width +3));
      /* Quantize the image and get a histogram while we're at it.
	 Do this first to save memory */
      qtable = build_EImage_quantable(pic, width, height, 256);
      if (qtable == NULL) return NULL;

      /* use our quantize table to allocate the colors */
      ncolors = qtable->num_active_colors;
      bmp_info=(BITMAPINFO*)xmalloc_and_zero (sizeof(BITMAPINFOHEADER) + 
					     sizeof(RGBQUAD) * ncolors);
      if (!bmp_info)
	{
	  xfree (qtable);
	  return NULL;
	}

      colortbl=(RGBQUAD*)(((unsigned char*)bmp_info)+sizeof(BITMAPINFOHEADER));

      bmp_info->bmiHeader.biBitCount=8; 
      bmp_info->bmiHeader.biCompression=BI_RGB; 
      bmp_info->bmiHeader.biSizeImage=bpline*height;
      bmp_info->bmiHeader.biClrUsed=ncolors; 
      bmp_info->bmiHeader.biClrImportant=ncolors; 
      
      *bmp_data = (unsigned char *) xmalloc_and_zero (bpline * height);
      *bit_count = bpline * height;

      if (!*bmp_data)
	{
	  xfree (qtable);
	  xfree (bmp_info);
	  return NULL;
	}
      
      /* build up an RGBQUAD colortable */
      for (i = 0; i < qtable->num_active_colors; i++) {
	colortbl[i].rgbRed = qtable->rm[i];
	colortbl[i].rgbGreen = qtable->gm[i];
	colortbl[i].rgbBlue = qtable->bm[i];
	colortbl[i].rgbReserved = 0;
      }

      /* now build up the data. picture has to be upside-down and
         back-to-front for msw bitmaps */
      ip = pic;
      for (i = height-1; i >= 0; i--) {
	dp = (*bmp_data) + (i * bpline);
	for (j = 0; j < width; j++) {
	  rd = *ip++;
	  gr = *ip++;
	  bl = *ip++;
	  *dp++ = QUANT_GET_COLOR (qtable,rd,gr,bl);
	}
      }
      xfree (qtable);
    } 
  /* fix up the standard stuff */
  bmp_info->bmiHeader.biWidth=width;
  bmp_info->bmiHeader.biHeight=height;
  bmp_info->bmiHeader.biPlanes=1;
  bmp_info->bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
  bmp_info->bmiHeader.biXPelsPerMeter=0; /* unless you know better */
  bmp_info->bmiHeader.biYPelsPerMeter=0; 

  return bmp_info;
}

/* Given a pixmap filename, look through all of the "standard" places
   where the file might be located.  Return a full pathname if found;
   otherwise, return Qnil. */

static Lisp_Object
mswindows_locate_pixmap_file (Lisp_Object name)
{
  /* This function can GC if IN_REDISPLAY is false */
  Lisp_Object found;

  /* Check non-absolute pathnames with a directory component relative to
     the search path; that's the way Xt does it. */
  if (IS_DIRECTORY_SEP(XSTRING_BYTE (name, 0)) ||
      (XSTRING_BYTE (name, 0) == '.' &&
       (IS_DIRECTORY_SEP(XSTRING_BYTE (name, 1)) ||
	(XSTRING_BYTE (name, 1) == '.' &&
	 (IS_DIRECTORY_SEP(XSTRING_BYTE (name, 2)))))))
    {
      if (!NILP (Ffile_readable_p (name)))
	return name;
      else
	return Qnil;
    }

  if (locate_file (Vmswindows_bitmap_file_path, name, "", &found, R_OK) < 0)
    {
      Lisp_Object temp = list1 (Vdata_directory);
      struct gcpro gcpro1;

      GCPRO1 (temp);
      locate_file (temp, name, "", &found, R_OK);
      UNGCPRO;
    }
    
  return found;
}


/* Initialize an image instance from a bitmap

   DEST_MASK specifies the mask of allowed image types.

   If this fails, signal an error.  INSTANTIATOR is only used
   in the error message. */

static void
init_image_instance_from_dibitmap (struct Lisp_Image_Instance *ii,
				   BITMAPINFO *bmp_info,
				   int dest_mask,
				   void *bmp_data,
				   int bmp_bits,
				   Lisp_Object instantiator, 
				   int x_hot, int y_hot,
				   int create_mask)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  struct device *d = XDEVICE (device);
  struct frame *f = XFRAME (DEVICE_SELECTED_FRAME (d));
  void* bmp_buf=0;
  int type;
  HBITMAP bitmap;
  HDC hdc;

  if (!DEVICE_MSWINDOWS_P (d))
    signal_simple_error ("Not an mswindows device", device);

  if (NILP (DEVICE_SELECTED_FRAME (d)))
    signal_simple_error ("No selected frame on mswindows device", device);

  if (dest_mask & IMAGE_COLOR_PIXMAP_MASK)
    type = IMAGE_COLOR_PIXMAP;
  else if (dest_mask & IMAGE_POINTER_MASK)
    type = IMAGE_POINTER;
  else 
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_COLOR_PIXMAP_MASK | IMAGE_POINTER_MASK);
  hdc = FRAME_MSWINDOWS_DC (f);

  bitmap=CreateDIBSection (hdc,  
			  bmp_info,
			  DIB_RGB_COLORS,
			  &bmp_buf, 
			  0,0);

  if (!bitmap || !bmp_buf)
    signal_simple_error ("Unable to create bitmap", instantiator);

  /* copy in the actual bitmap */
  memcpy (bmp_buf, bmp_data, bmp_bits);

  mswindows_initialize_dibitmap_image_instance (ii, type);

  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  IMAGE_INSTANCE_MSWINDOWS_BITMAP (ii) = bitmap;
  IMAGE_INSTANCE_MSWINDOWS_MASK (ii) = NULL;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = bmp_info->bmiHeader.biWidth;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = bmp_info->bmiHeader.biHeight;
  IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = bmp_info->bmiHeader.biBitCount;
  XSETINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii), x_hot);
  XSETINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii), y_hot);

  if (create_mask)
    {
      mswindows_initialize_image_instance_mask (ii, f);
    }
  
  if (type == IMAGE_POINTER)
    {
      mswindows_initialize_image_instance_icon(ii, TRUE);
    }
}

static void
mswindows_init_image_instance_from_eimage (struct Lisp_Image_Instance *ii,
					   int width, int height,
					   unsigned char *eimage, 
					   int dest_mask,
					   Lisp_Object instantiator,
					   Lisp_Object domain)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  BITMAPINFO*		bmp_info;
  unsigned char*	bmp_data;
  int			bmp_bits;
  COLORREF		bkcolor;
  
  if (!DEVICE_MSWINDOWS_P (XDEVICE (device)))
    signal_simple_error ("Not an mswindows device", device);

  /* this is a hack but MaskBlt and TransparentBlt are not supported
     on most windows variants */
  bkcolor = COLOR_INSTANCE_MSWINDOWS_COLOR 
    (XCOLOR_INSTANCE (FACE_BACKGROUND (Vdefault_face, domain)));

  /* build a bitmap from the eimage */
  if (!(bmp_info=convert_EImage_to_DIBitmap (device, width, height, eimage,
					     &bmp_bits, &bmp_data)))
    {
      signal_simple_error ("EImage to DIBitmap conversion failed",
			   instantiator);
    }

  /* Now create the pixmap and set up the image instance */
  init_image_instance_from_dibitmap (ii, bmp_info, dest_mask,
				     bmp_data, bmp_bits, instantiator,
				     0, 0, 0);

  xfree (bmp_info);
  xfree (bmp_data);
}

static void set_mono_pixel ( unsigned char* bits, 
			     int bpline, int height, 
			     int x, int y, int white ) 
{ 
  int index;
  unsigned char    bitnum;  
  /* Find the byte on which this scanline begins */
  index = (height - y - 1) * bpline; 
  /* Find the byte containing this pixel */
  index += (x >> 3); 
  /* Which bit is it? */
  bitnum = (unsigned char)( 7 - (x % 8) );  
  if( white )         /* Turn it on */
    bits[index] |= (1<<bitnum);
  else         /* Turn it off */
    bits[index] &= ~(1<<bitnum); 
} 

static void
mswindows_initialize_image_instance_mask (struct Lisp_Image_Instance* image, 
					  struct frame* f)
{
  HBITMAP mask, bmp;
  HDC hcdc = FRAME_MSWINDOWS_CDC (f);
  BITMAPINFO* bmp_info = 
    xmalloc_and_zero (sizeof(BITMAPINFOHEADER) + sizeof(RGBQUAD));
  int i, j;
  int height = IMAGE_INSTANCE_PIXMAP_HEIGHT (image);
  
  void* and_bits;
  int bpline= (int)(~3UL & (unsigned long)
		    (((IMAGE_INSTANCE_PIXMAP_WIDTH (image)+7)/8) +3));

  bmp_info->bmiHeader.biWidth=IMAGE_INSTANCE_PIXMAP_WIDTH (image);
  bmp_info->bmiHeader.biHeight = height;
  bmp_info->bmiHeader.biPlanes=1;
  bmp_info->bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
  bmp_info->bmiHeader.biBitCount=1; 
  bmp_info->bmiHeader.biCompression=BI_RGB; 
  bmp_info->bmiHeader.biClrUsed = 2; 
  bmp_info->bmiHeader.biClrImportant = 2; 
  bmp_info->bmiHeader.biSizeImage = height * bpline; 
  bmp_info->bmiColors[0].rgbRed = 0;
  bmp_info->bmiColors[0].rgbGreen = 0;
  bmp_info->bmiColors[0].rgbBlue = 0;
  bmp_info->bmiColors[0].rgbReserved = 0;
  bmp_info->bmiColors[1].rgbRed = 255;
  bmp_info->bmiColors[1].rgbGreen = 255;
  bmp_info->bmiColors[1].rgbBlue = 255;
  bmp_info->bmiColors[0].rgbReserved = 0;
    
  if (!(mask = CreateDIBSection (hcdc,  
				 bmp_info,
				 DIB_RGB_COLORS,
				 &and_bits, 
				 0,0)))
    {
      xfree (bmp_info);
      return;
    }

  xfree (bmp_info);
  SelectObject (hcdc, IMAGE_INSTANCE_MSWINDOWS_BITMAP (image));
  
  for(i=0; i<IMAGE_INSTANCE_PIXMAP_WIDTH (image); i++)     
    { 
      for(j=0; j<height; j++)         
	{ 
	  if( GetPixel( hcdc, i, j ) == transparent_color )
	    { 
	      SetPixel( hcdc, i, j, RGB (0,0,0));  
	      set_mono_pixel( and_bits, bpline, height, i, j, TRUE );
	    }
	  else             
	    { 
	      set_mono_pixel( and_bits, bpline, height, i, j, FALSE );
            }
	}
    }

  GdiFlush();
  SelectObject(hcdc, 0);

  IMAGE_INSTANCE_MSWINDOWS_MASK (image) = mask;
}

void
mswindows_initialize_image_instance_icon (struct Lisp_Image_Instance* image,
					  int cursor)
{
  ICONINFO x_icon;

  /* we rely on windows to do any resizing necessary */
  x_icon.fIcon=cursor ? FALSE : TRUE;
  x_icon.xHotspot=XINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (image));
  x_icon.yHotspot=XINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (image));
  x_icon.hbmMask=IMAGE_INSTANCE_MSWINDOWS_MASK (image);
  x_icon.hbmColor=IMAGE_INSTANCE_MSWINDOWS_BITMAP (image);
  
  IMAGE_INSTANCE_MSWINDOWS_ICON (image)=
    CreateIconIndirect (&x_icon);
}

int
mswindows_resize_dibitmap_instance (struct Lisp_Image_Instance* ii,
				    struct frame* f,
				    int newx, int newy)
{
  HBITMAP newbmp;
  HDC hcdc = FRAME_MSWINDOWS_CDC (f);
  HDC hdcDst = CreateCompatibleDC (hcdc);  
  
  SelectObject(hcdc, IMAGE_INSTANCE_MSWINDOWS_BITMAP (ii)); 
  
  newbmp = CreateCompatibleBitmap(hcdc, newx, newy);

  DeleteObject( SelectObject(hdcDst, newbmp) );
  
  if (!StretchBlt(hdcDst, 0, 0, newx, newy,
		  hcdc, 0, 0, 
		  IMAGE_INSTANCE_PIXMAP_WIDTH (ii), 
		  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii), 
		  SRCCOPY))
    {
      return FALSE;
    }
  
  SelectObject(hdcDst, 0);
  SelectObject(hcdc, 0);
  
  if (IMAGE_INSTANCE_MSWINDOWS_BITMAP (ii))
    DeleteObject (IMAGE_INSTANCE_MSWINDOWS_BITMAP (ii));
  if (IMAGE_INSTANCE_MSWINDOWS_MASK (ii))
    DeleteObject (IMAGE_INSTANCE_MSWINDOWS_MASK (ii));

  IMAGE_INSTANCE_MSWINDOWS_BITMAP (ii) = newbmp;
  IMAGE_INSTANCE_MSWINDOWS_MASK (ii) = newbmp;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = newx;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = newy;

  DeleteDC(hdcDst);

  return TRUE;
}

/**********************************************************************
 *                               XPM                                  *
 **********************************************************************/

#ifdef HAVE_XPM

struct color_symbol
{
  char*		name;
  COLORREF	color;
};

static struct color_symbol*
extract_xpm_color_names (Lisp_Object device,
			 Lisp_Object domain,
			 Lisp_Object color_symbol_alist,
			 int* nsymbols)
{
  /* This function can GC */
  Lisp_Object rest;
  Lisp_Object results = Qnil;
  int i, j;
  struct color_symbol *colortbl;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (results, device);

  /* We built up results to be (("name" . #<color>) ...) so that if an
     error happens we don't lose any malloc()ed data, or more importantly,
     leave any pixels allocated in the server. */
  i = 0;
  LIST_LOOP (rest, color_symbol_alist)
    {
      Lisp_Object cons = XCAR (rest);
      Lisp_Object name = XCAR (cons);
      Lisp_Object value = XCDR (cons);
      if (NILP (value))
	continue;
      if (STRINGP (value))
	value =
	  Fmake_color_instance
	  (value, device, encode_error_behavior_flag (ERROR_ME_NOT));
      else
        {
          assert (COLOR_SPECIFIERP (value));
          value = Fspecifier_instance (value, domain, Qnil, Qnil);
        }
      if (NILP (value))
        continue;
      results = noseeum_cons (noseeum_cons (name, value), results);
      i++;
    }
  UNGCPRO;			/* no more evaluation */

  *nsymbols=i;
  if (i == 0) return 0;

  colortbl = xnew_array_and_zero (struct color_symbol, i);

  for (j=0; j<i; j++)
    {
      Lisp_Object cons = XCAR (results);
      colortbl[j].color = 
	COLOR_INSTANCE_MSWINDOWS_COLOR (XCOLOR_INSTANCE (XCDR (cons)));

      colortbl[j].name = (char *) XSTRING_DATA (XCAR (cons));
      free_cons (XCONS (cons));
      cons = results;
      results = XCDR (results);
      free_cons (XCONS (cons));
    }
  return colortbl;
}

static int xpm_to_eimage (Lisp_Object image, CONST Extbyte *buffer,
			  unsigned char** data,
			  int* width, int* height,
			  int* x_hot, int* y_hot,
			  int* transp,
			  struct color_symbol* color_symbols,
			  int nsymbols)
{
  XpmImage xpmimage;
  XpmInfo xpminfo;
  int result, i, j, transp_idx, maskbpline;
  unsigned char* dptr;
  unsigned int* sptr;
  COLORREF color; /* the american spelling virus hits again .. */
  COLORREF* colortbl; 

  xzero (xpmimage);
  xzero (xpminfo);
  xpminfo.valuemask=XpmHotspot;
  *transp=FALSE;

  result = XpmCreateXpmImageFromBuffer ((char*)buffer,
				       &xpmimage,
				       &xpminfo);
  switch (result)
    {
    case XpmSuccess:
      break;
    case XpmFileInvalid:
      {
	signal_simple_error ("invalid XPM data", image);
      }
    case XpmNoMemory:
      {
	signal_double_file_error ("Parsing pixmap data",
				  "out of memory", image);
      }
    default:
      {
	signal_double_file_error_2 ("Parsing pixmap data",
				    "unknown error code",
				    make_int (result), image);
      }
    }
  
  *width = xpmimage.width;
  *height = xpmimage.height;
  maskbpline = (int)(~3UL & (unsigned long)
		     (((~7UL & (unsigned long)(*width + 7)) / 8) + 3));
  
  *data = xnew_array_and_zero (unsigned char, *width * *height * 3);

  if (!*data)
    {
      XpmFreeXpmImage (&xpmimage);
      XpmFreeXpmInfo (&xpminfo);
      return 0;
    }

  /* build a color table to speed things up */
  colortbl = xnew_array_and_zero (COLORREF, xpmimage.ncolors);
  if (!colortbl)
    {
      xfree (*data);
      XpmFreeXpmImage (&xpmimage);
      XpmFreeXpmInfo (&xpminfo);
      return 0;
    }

  for (i=0; i<xpmimage.ncolors; i++)
    {
				/* pick up symbolic colors */
      if (xpmimage.colorTable[i].c_color == 0 
	  &&
	  xpmimage.colorTable[i].symbolic != 0)
	{
	  if (!color_symbols)
	    {
	      xfree (*data);
	      xfree (colortbl);
	      XpmFreeXpmImage (&xpmimage);
	      XpmFreeXpmInfo (&xpminfo);
	      return 0;
	    }
	  for (j = 0; j<nsymbols; j++)
	    {
	      if (!strcmp (xpmimage.colorTable[i].symbolic,
			   color_symbols[j].name ))
		{
		  colortbl[i]=color_symbols[j].color;		  
		}
	    }
	}
				/* pick up transparencies */
      else if (!strcasecmp (xpmimage.colorTable[i].c_color,"None")
	       ||
	       xpmimage.colorTable[i].symbolic
	       &&
	       (!strcasecmp (xpmimage.colorTable[i].symbolic,"BgColor")
		||
		!strcasecmp (xpmimage.colorTable[i].symbolic,"None")))
	{
	  *transp=TRUE;
	  colortbl[i]=transparent_color; 
	  transp_idx=i;
	}
      else
	{
	  colortbl[i]=
	    mswindows_string_to_color (xpmimage.colorTable[i].c_color);
	}
    }

  /* convert the image */
  sptr=xpmimage.data;
  dptr=*data;
  for (i = 0; i< *width * *height; i++)
    {
      color = colortbl[*sptr++];

      /* split out the 0x02bbggrr colorref into an rgb triple */
      *dptr++=GetRValue (color); /* red */
      *dptr++=GetGValue (color); /* green */
      *dptr++=GetBValue (color); /* blue */
    }

  *x_hot=xpminfo.x_hotspot;
  *y_hot=xpminfo.y_hotspot;

  XpmFreeXpmImage (&xpmimage);
  XpmFreeXpmInfo (&xpminfo);
  xfree (colortbl);
  return TRUE;
}

static void
mswindows_xpm_instantiate (Lisp_Object image_instance,
			   Lisp_Object instantiator,
			   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			   int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  CONST Extbyte		*bytes;
  Extcount 		len;
  unsigned char		*eimage;
  int			width, height, x_hot, y_hot;
  BITMAPINFO*		bmp_info;
  unsigned char*	bmp_data;
  int			bmp_bits;
  COLORREF		bkcolor;
  int 			nsymbols=0, transp;
  struct color_symbol*	color_symbols=NULL;
  
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  Lisp_Object color_symbol_alist = find_keyword_in_vector (instantiator,
							   Q_color_symbols);

  if (!DEVICE_MSWINDOWS_P (XDEVICE (device)))
    signal_simple_error ("Not an mswindows device", device);

  assert (!NILP (data));

  GET_STRING_BINARY_DATA_ALLOCA (data, bytes, len);

  /* in case we have color symbols */
  color_symbols = extract_xpm_color_names (device, domain,
					   color_symbol_alist, &nsymbols);

  /* convert to an eimage to make processing easier */
  if (!xpm_to_eimage (image_instance, bytes, &eimage, &width, &height,
		      &x_hot, &y_hot, &transp, color_symbols, nsymbols))
    {
      signal_simple_error ("XPM to EImage conversion failed", 
			   image_instance);
    }
  
  if (color_symbols)
    xfree(color_symbols);
  
  /* build a bitmap from the eimage */
  if (!(bmp_info=convert_EImage_to_DIBitmap (device, width, height, eimage,
					     &bmp_bits, &bmp_data)))
    {
      signal_simple_error ("XPM to EImage conversion failed",
			   image_instance);
    }
  xfree (eimage);

  /* Now create the pixmap and set up the image instance */
  init_image_instance_from_dibitmap (ii, bmp_info, dest_mask,
				     bmp_data, bmp_bits, instantiator,
				     x_hot, y_hot, transp);

  xfree (bmp_info);
  xfree (bmp_data);
}
#endif /* HAVE_XPM */

/**********************************************************************
 *                               BMP                                  *
 **********************************************************************/

static void
bmp_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
bmp_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  return simple_image_type_normalize (inst, console_type, Qbmp);
}

static int
bmp_possible_dest_types (void)
{
  return IMAGE_COLOR_PIXMAP_MASK;
}

static void
bmp_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		 int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  CONST Extbyte		*bytes;
  Extcount 		len;
  BITMAPFILEHEADER*	bmp_file_header;
  BITMAPINFO*		bmp_info;
  void*			bmp_data;
  int			bmp_bits;
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);

  if (!DEVICE_MSWINDOWS_P (XDEVICE (device)))
    signal_simple_error ("Not an mswindows device", device);

  assert (!NILP (data));

  GET_STRING_BINARY_DATA_ALLOCA (data, bytes, len);
  
  /* Then slurp the image into memory, decoding along the way.
     The result is the image in a simple one-byte-per-pixel
     format. */
  
  bmp_file_header=(BITMAPFILEHEADER*)bytes;
  bmp_info = (BITMAPINFO*)(bytes + sizeof(BITMAPFILEHEADER));
  bmp_data = (Extbyte*)bytes + bmp_file_header->bfOffBits;
  bmp_bits = bmp_file_header->bfSize - bmp_file_header->bfOffBits;

  /* Now create the pixmap and set up the image instance */
  init_image_instance_from_dibitmap (ii, bmp_info, dest_mask,
				     bmp_data, bmp_bits, instantiator,
				     0, 0, 0);
}


/************************************************************************/
/*                      image instance methods                          */
/************************************************************************/

static void
mswindows_print_image_instance (struct Lisp_Image_Instance *p,
				Lisp_Object printcharfun,
				int escapeflag)
{
  char buf[100];

  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      sprintf (buf, " (0x%lx", 
	       (unsigned long) IMAGE_INSTANCE_MSWINDOWS_BITMAP (p));
      write_c_string (buf, printcharfun);
      if (IMAGE_INSTANCE_MSWINDOWS_MASK (p))
	{
	  sprintf (buf, "/0x%lx", 
		   (unsigned long) IMAGE_INSTANCE_MSWINDOWS_MASK (p));
	  write_c_string (buf, printcharfun);
	}
      write_c_string (")", printcharfun);
      break;
    default:
      break;
    }
}

static void
mswindows_finalize_image_instance (struct Lisp_Image_Instance *p)
{
  if (!p->data)
    return;

  if (DEVICE_LIVE_P (XDEVICE (p->device)))
    {
      if (IMAGE_INSTANCE_MSWINDOWS_BITMAP (p))
	DeleteObject (IMAGE_INSTANCE_MSWINDOWS_BITMAP (p));
      IMAGE_INSTANCE_MSWINDOWS_BITMAP (p) = 0;
      if (IMAGE_INSTANCE_MSWINDOWS_MASK (p))
	DeleteObject (IMAGE_INSTANCE_MSWINDOWS_MASK (p));
      IMAGE_INSTANCE_MSWINDOWS_MASK (p) = 0;
      if (IMAGE_INSTANCE_MSWINDOWS_ICON (p))
	DestroyIcon (IMAGE_INSTANCE_MSWINDOWS_ICON (p));
      IMAGE_INSTANCE_MSWINDOWS_ICON (p) = 0;
    }

  xfree (p->data);
  p->data = 0;
}

static int
mswindows_image_instance_equal (struct Lisp_Image_Instance *p1,
				struct Lisp_Image_Instance *p2, int depth)
{
  switch (IMAGE_INSTANCE_TYPE (p1))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      if (IMAGE_INSTANCE_MSWINDOWS_BITMAP (p1) 
	  != IMAGE_INSTANCE_MSWINDOWS_BITMAP (p2))
	return 0;
      break;
    default:
      break;
    }

  return 1;
}

static unsigned long
mswindows_image_instance_hash (struct Lisp_Image_Instance *p, int depth)
{
  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      return (unsigned long) IMAGE_INSTANCE_MSWINDOWS_BITMAP (p);
    default:
      return 0;
    }
}

/* Set all the slots in an image instance structure to reasonable
   default values.  This is used somewhere within an instantiate
   method.  It is assumed that the device slot within the image
   instance is already set -- this is the case when instantiate
   methods are called. */

static void
mswindows_initialize_dibitmap_image_instance (struct Lisp_Image_Instance *ii,
					    enum image_instance_type type)
{
  ii->data = xnew_and_zero (struct mswindows_image_instance_data);
  IMAGE_INSTANCE_TYPE (ii) = type;
  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_MASK_FILENAME (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_FG (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_BG (ii) = Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_glyphs_mswindows (void)
{
}

void
console_type_create_glyphs_mswindows (void)
{
  /* image methods */

  CONSOLE_HAS_METHOD (mswindows, print_image_instance);
  CONSOLE_HAS_METHOD (mswindows, finalize_image_instance);
  CONSOLE_HAS_METHOD (mswindows, image_instance_equal);
  CONSOLE_HAS_METHOD (mswindows, image_instance_hash);
  CONSOLE_HAS_METHOD (mswindows, init_image_instance_from_eimage);
  CONSOLE_HAS_METHOD (mswindows, locate_pixmap_file);
#ifdef HAVE_XPM
  CONSOLE_HAS_METHOD (mswindows, xpm_instantiate);
#endif
}

void
image_instantiator_format_create_glyphs_mswindows (void)
{
  /* image-instantiator types */

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (bmp, "bmp");

  IIFORMAT_HAS_METHOD (bmp, validate);
  IIFORMAT_HAS_METHOD (bmp, normalize);
  IIFORMAT_HAS_METHOD (bmp, possible_dest_types);
  IIFORMAT_HAS_METHOD (bmp, instantiate);

  IIFORMAT_VALID_KEYWORD (bmp, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (bmp, Q_file, check_valid_string);
}

void
vars_of_glyphs_mswindows (void)
{
  Fprovide (Qbmp);
  DEFVAR_LISP ("mswindows-bitmap-file-path", &Vmswindows_bitmap_file_path /*
A list of the directories in which mswindows bitmap files may be found.
This is used by the `make-image-instance' function.
*/ );
  Vmswindows_bitmap_file_path = Qnil;
}

void
complex_vars_of_glyphs_mswindows (void)
{
}
