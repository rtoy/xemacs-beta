/* ----------------------------------------------------------------------------
 * Animated display of collapse and expansion of nodes in a tree.
 * ----------------------------------------------------------------------------
 */

void
InitializeDissolveEffect(Display *dpy,
			 Drawable drawable,
			 int fg_pixel,
			 int bg_pixel);

void
DissolveTree(Display *dpy,
	     Window drawable,
	     XRectangle rectangles[],
	     int nrectangles,
	     XSegment segments[],
	     int nsegments,
	     int mode);
