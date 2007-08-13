/* ----------------------------------------------------------------------------
 * Animated display of collapse and expansion of nodes in a tree.
 * ----------------------------------------------------------------------------
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_USLEEP
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#else
int usleep(unsigned long microSeconds);
#endif /* HAVE_USLEEP */

#include <X11/Xlib.h>

#include "dissolve.h"

#define NUM_DISSOLVE_STEPS 8
#define NUM_LINE_STEPS     4

#define first_width 16
#define first_height 16
static unsigned char first_bits[] = {
 0x88, 0x04, 0x00, 0x80, 0x20, 0x10, 0x00, 0x81, 0x12, 0x00, 0x00, 0x00,
 0x00, 0x02, 0x82, 0x40, 0x00, 0x00, 0x12, 0x14, 0x00, 0x00, 0x0a, 0x28,
 0x40, 0x01, 0x05, 0x00, 0xa0, 0x92, 0x08, 0x00 };

#define second_width 16
#define second_height 16
static unsigned char second_bits[] = {
 0x51, 0x20, 0x04, 0x02, 0x00, 0x88, 0x02, 0x00, 0x08, 0x09, 0x40, 0x00,
 0x04, 0x04, 0x00, 0xa0, 0x80, 0x08, 0x08, 0x00, 0x00, 0xa8, 0x00, 0x00,
 0x28, 0x28, 0x00, 0x80, 0x01, 0x00, 0x10, 0x82 };

#define third_width 16
#define third_height 16
static unsigned char third_bits[] = {
 0x00, 0x01, 0x12, 0x44, 0x00, 0x01, 0x00, 0x08, 0x00, 0x42, 0x2a, 0x08,
 0x80, 0x00, 0x04, 0x10, 0x01, 0x04, 0x00, 0x80, 0xa9, 0x04, 0x00, 0x00,
 0x00, 0x10, 0x0a, 0x05, 0x40, 0x00, 0x00, 0x50 };

#define fourth_width 16
#define fourth_height 16
static unsigned char fourth_bits[] = {
 0x02, 0x88, 0x80, 0x00, 0x04, 0x40, 0x11, 0x02, 0x40, 0x90, 0x05, 0x00,
 0x00, 0x08, 0x11, 0x01, 0x40, 0x00, 0x00, 0x41, 0x14, 0x00, 0x00, 0x12,
 0x10, 0x00, 0x40, 0x40, 0x08, 0x00, 0xa0, 0x04 };

#define fifth_width 16
#define fifth_height 16
static unsigned char fifth_bits[] = {
 0x24, 0x00, 0x00, 0x08, 0x09, 0x20, 0x20, 0x04, 0x00, 0x00, 0x00, 0x85,
 0x10, 0x20, 0x40, 0x02, 0x14, 0x40, 0x00, 0x08, 0x02, 0x01, 0x10, 0x40,
 0x04, 0x04, 0x20, 0x20, 0x00, 0x00, 0x42, 0x29 };

#define sixth_width 16
#define sixth_height 16
static unsigned char sixth_bits[] = {
 0x00, 0x12, 0x28, 0x00, 0x02, 0x00, 0x88, 0x00, 0x01, 0x20, 0x90, 0x02,
 0x01, 0x50, 0x20, 0x04, 0x08, 0xa0, 0x41, 0x00, 0x00, 0x00, 0x24, 0x05,
 0x00, 0x80, 0x00, 0x10, 0x10, 0x40, 0x05, 0x00 };

#define seventh_width 16
#define seventh_height 16
static unsigned char seventh_bits[] = {
 0x00, 0x40, 0x01, 0x10, 0x90, 0x02, 0x00, 0x50, 0xa4, 0x04, 0x00, 0x20,
 0x20, 0x80, 0x08, 0x08, 0x00, 0x01, 0x04, 0x00, 0x40, 0x52, 0x00, 0x00,
 0x81, 0x42, 0x10, 0x00, 0x04, 0x25, 0x00, 0x00 };

#define eighth_width 16
#define eighth_height 16
static unsigned char eighth_bits[] = {
 0x00, 0x00, 0x40, 0x21, 0x40, 0x04, 0x44, 0x20, 0x00, 0x00, 0x00, 0x50,
 0x4a, 0x01, 0x00, 0x00, 0x22, 0x12, 0xa0, 0x22, 0x00, 0x00, 0xc1, 0x80,
 0x02, 0x00, 0x80, 0x0a, 0x02, 0x08, 0x00, 0x00 };

static Pixmap DissolvePixmaps[NUM_DISSOLVE_STEPS];
static GC     DissolveInGC;
static GC     DissolveOutGC;
static GC     DissolveInLineGC[NUM_LINE_STEPS];
static GC     DissolveOutLineGC[NUM_LINE_STEPS];

static char first_dash[] =  {1, 3};
static char second_dash[] = {1, 1};

void
InitializeDissolveEffect(Display *dpy,
			 Drawable drawable,
			 int fg_pixel,
			 int bg_pixel)
{
   unsigned long  gcvaluemask;
   XGCValues      gcvalues;
   int i;

   /* make DissolveOutGC */
   gcvalues.background = bg_pixel;
   gcvalues.foreground = bg_pixel;
   gcvalues.function   = GXcopy;
   gcvalues.fill_style = FillStippled;
   gcvalues.line_width = 0;
   gcvaluemask = GCFunction | GCForeground | GCBackground | GCFillStyle |
                 GCLineWidth;
   DissolveOutGC = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetTSOrigin(dpy, DissolveOutGC, 0, 0);

   /* make DissolveInGC */
   gcvalues.foreground = fg_pixel;
   DissolveInGC = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetTSOrigin(dpy, DissolveInGC, 0, 0);

   /* make DissolveOutLineGC */
   i = 0;
   gcvalues.foreground = bg_pixel;
   gcvalues.fill_style = FillSolid;
   gcvalues.line_style = LineOnOffDash;
   gcvalues.line_width = 0;
   gcvaluemask = GCFunction | GCForeground | GCBackground |
                 GCFillStyle | GCLineStyle | GCLineWidth ;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveOutLineGC[i], 0, first_dash, 2);
   i++;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveOutLineGC[i], 0, second_dash, 2);
   i++;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveOutLineGC[i], 3, first_dash, 2);
   i++;
   gcvalues.line_style = LineSolid;
   DissolveOutLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);

   /* make DissolveInLineGC */
   i = 0;
   gcvalues.foreground = fg_pixel;
   gcvalues.fill_style = FillSolid;
   gcvalues.line_style = LineOnOffDash;
   gcvalues.line_width = 0;
   gcvaluemask = GCFunction | GCForeground | GCBackground |
                 GCFillStyle | GCLineStyle | GCLineWidth;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveInLineGC[i], 0, first_dash, 2);
   i++;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveInLineGC[i], 0, second_dash, 2);
   i++;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);
   XSetDashes(dpy, DissolveInLineGC[i], 3, first_dash, 2);
   i++;
   gcvalues.line_style = LineSolid;
   DissolveInLineGC[i] = XCreateGC(dpy, drawable, gcvaluemask, &gcvalues);

   i = 0;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) first_bits, first_width, first_height);
   i++;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) second_bits, second_width, second_height);
   i++;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) third_bits, third_width, third_height);
   i++;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) fourth_bits, fourth_width, fourth_height);
   i++;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) fifth_bits, fifth_width, fifth_height);
   i++;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) sixth_bits, sixth_width, sixth_height);
   i++;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) seventh_bits, seventh_width, seventh_height);
   i++;
   DissolvePixmaps[i] =
     XCreateBitmapFromData(dpy, drawable,
			   (char *) eighth_bits, eighth_width, eighth_height);
}

#if 0 /* Currently Unused */
void
DissolveRectangle(Display *dpy, Window drawable,
		  int x, int y, int width, int height, int mode)
{
   int i;
   GC gc;

   gc = mode ? DissolveOutGC : DissolveInGC;

   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      if (mode)
	 XFillRectangle(dpy, drawable, gc, x, y, width, height);
      else
	 XDrawRectangle(dpy, drawable, gc, x, y, width, height);
      XFlush(dpy);
      usleep(50000);
   }
}

void
DissolveRectangles(Display *dpy,
		   Window drawable,
		   XRectangle rectangles[],
		   int nrectangles,
		   int mode)
{
   int i;
   GC gc;

   gc = mode ? DissolveOutGC : DissolveInGC;

   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      if (mode)
	 XFillRectangles(dpy, drawable, gc, rectangles, nrectangles);
      else
	 XDrawRectangles(dpy, drawable, gc, rectangles, nrectangles);
      XFlush(dpy);
      usleep(50000);
   }
}

void
DissolveSegments(Display *dpy, Window drawable,
		 XSegment segments[], int nsegments, int mode)
{
   int i;
   GC *gc;

   gc = mode ? DissolveOutLineGC : DissolveInLineGC;

   for (i = 0 ; i < NUM_LINE_STEPS ; i++) {
      XDrawSegments(dpy, drawable, gc[i], segments, nsegments);
      XFlush(dpy);
      usleep(50000);
   }
}

#endif /* 0 - Unused */

void
DissolveTree(Display *dpy,
	     Window drawable,
	     XRectangle rectangles[],
	     int nrectangles,
	     XSegment segments[],
	     int nsegments,
	     int mode)
{
   int i;
   int j = 0;
   int idle;
   GC gc;
   GC *lineGC;

   gc = mode ? DissolveOutGC : DissolveInGC;
   lineGC = mode ? DissolveOutLineGC : DissolveInLineGC;

   /* speed up if there are lots of nodes */
   idle = nrectangles > 50 ? 0 : 25000;

   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      if (mode)
	 XFillRectangles(dpy, drawable, gc, rectangles, nrectangles);
      else
	 XDrawRectangles(dpy, drawable, gc, rectangles, nrectangles);
      if (i % 2)
	 XDrawSegments(dpy, drawable, lineGC[j++], segments, nsegments);
      XFlush(dpy);
      usleep(idle);
   }
}

#if 0 /* Currently Unused */
void
DissolvePolygon(Display *dpy,
		Window drawable,
		XPoint *pts,
		int num_pts,
		int mode)
{
   int i;
   GC gc;

   gc = mode ? DissolveOutGC : DissolveInGC;

   for (i = 0 ; i < NUM_DISSOLVE_STEPS ; i++) {
      XSetStipple(dpy, gc, DissolvePixmaps[i]);
      XFillPolygon(dpy, drawable, gc, pts, num_pts,
		   Nonconvex, CoordModeOrigin);
      XFlush(dpy);
      usleep(50000);
   }
}

#endif /* Currently Unused */
