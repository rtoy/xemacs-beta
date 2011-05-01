#!bash

# Copyright (C) 1998 Andy Piper

# This file is part of XEmacs.
# 
# XEmacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
# 
# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

install_prog=$1
shift

tstr=""

while [[ $# -gt 0 ]]
do
  if [[ -f $1.exe ]]
  then
    if [[ "$2" == *.exe ]]
    then
      tstr="$tstr$1 $2"
    else
      tstr="$tstr$1.exe $2.exe"
    fi
    shift 2
  else
    tstr="$tstr$1 "
  fi
  shift
done
echo "$install_prog $tstr"
eval "$install_prog $tstr"
exit

