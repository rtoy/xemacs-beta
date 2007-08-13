#ifndef BALLOON_HELP_H
#define BALLOON_HELP_H

#include <X11/Intrinsic.h>

void balloon_help_create( Display* dpy,
			  Pixel fg, Pixel bg, Pixel shine, Pixel shadow,
			  XFontStruct* font );
void balloon_help_destroy( void );
void balloon_help_set_delay( unsigned long milliseconds );
void balloon_help_show( const char* text );
void balloon_help_hide( void );
void balloon_help_move_to_pointer( void );

#endif /* BALLOON_HELP_H */
