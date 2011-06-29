/* Static array to put the dumped data in and its management
   Copyright (C) 2003 Olivier Galibert

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_dump_data_h_
#define INCLUDED_dump_data_h_

/* inline dumped data size */
size_t dumped_data_size (void);

/* maximum space available for inline data */
size_t dumped_data_max_size (void);

/* alignment offset for data inclusion */
size_t dumped_data_align_offset (void);

/* inline data */
Rawbyte *dumped_data_get (void);

#endif
