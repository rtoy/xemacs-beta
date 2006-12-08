: #-*- Perl -*-

### make-mswin-unicode --- generate Unicode-encapsulation code for MS Windows

## Copyright (C) 2001, 2002, 2004 Ben Wing.

## Author: Ben Wing <ben@xemacs.org>
## Maintainer: Ben Wing <ben@xemacs.org>
## Current Version: 1.0, August 24, 2001

## This file is part of XEmacs.

## XEmacs is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.

## XEmacs is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with XEmacs; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

eval 'exec perl -w -S $0 ${1+"$@"}'
  if 0;

use strict;
use File::Basename;
use Getopt::Long;

my ($myName, $myPath) = fileparse ($0);

my $usage="
Usage: $myName [--c-output FILE] [--h-output FILE] [--help] [FILES ...]

The purpose of this script is to auto-generate Unicode-encapsulation
code for MS Windows library functions that come in two versions (ANSI
and Unicode).  The MS Windows header files provide a way of
automatically calling the right version, but only at compile-time,
which is *NOT* sufficient for any real-world program.  The solution is
run-time Unicode encapsulation, which is not conceptually difficult
but is time-consuming, and is not supported standardly only due to
evil marketing decisions made by Microsoft.  See src/intl-win32.c
for more information.

In XEmacs, this file is normally run using `nmake -f xemacs.mak
unicode-encapsulate'.

This script processes the specified files, looking for commands
indicating library routines to Unicode-encapsulate, as follows:

Portions of the files that should be processed are enclosed in lines
consisting only of the words \"begin-unicode-encapsulation-script\"
and \"end-unicode-encapsulation-script\".  More than one section can
occur in a single file.  Processed lines begin with a command word,
followed by one or more args (no quotes are necessary for spaces):

file specifies a file to start reading from.
yes indicates a function to be automatically Unicode-encapsulated.
   (All parameters either need no special processing or are LPTSTR or
   LPCTSTR.)
soon indicates a function that should be automatically Unicode-encapsulated,
   but we're not ready to process it yet.
no indicates a function we don't support (it will be #defined to cause
   a compile error, with the text after the function included in the
   erroneous definition to indicate why we don't support it).
skip indicates a function we support manually; only a comment about this
   will be generated.
split indicates a function with a split structure (different versions
   for Unicode and ANSI), but where the only difference is in pointer
   types, and the actual size does not differ.  The structure name
   should follow the function name, and it will be automatically
   Unicode-encapsulated with appropriate casts.
begin-bracket indicates a #if statement to be inserted here.
end-bracket indicates the corresponding #endif statement.
blank lines and lines beginning with // are ignored.
";

# ------------------ process command-line options ------------------

my %options;
my @SAVE_ARGV = @ARGV;

$Getopt::Long::ignorecase = 0;
&GetOptions (
	     \%options,
	     'c-output=s',
	     'h-output=s',
             'includedir=s',
	     'help',
	    );

die $usage if $options{"help"};

my $in_script;
my $slurp;

my ($cout, $hout, $dir) = ($options{"c-output"},
                          $options{"h-output"},
                          $options{"includedir"});
if (!$dir)
  {
    $dir=$ENV{"MSVCDIR"} or die "Environment variable MSVCDIR undefined - run vcvars32.bat from your MSVC installation";
    $dir.='/include';
  }
die "Can't find MSVC include files in \"$dir\"" unless ((-f $dir.'/WINDOWS.H') || (-f $dir.'/windows.h'));

open (COUT, ">$cout") or die "Can't open C output file $cout: $!";
open (HOUT, ">$hout") or die "Can't open C output file $hout: $!";

select (STDOUT); $| = 1;

print COUT "/* Automatically-generated Unicode-encapsulation file,
   using the command

   $myPath$myName @SAVE_ARGV

   Do not edit.  See `$myName'.
*/

#include <config.h>
#include \"lisp.h\"

#include \"syswindows.h\"

";
print HOUT "/* Automatically-generated Unicode-encapsulation header file.
   Do not edit.  See `$myName'.
*/\n\n";

my %files;
my %processed;
my %bracket;

my $current_file;
my @current_bracket;

while (<>)
  {
    chomp;
    # remove trailing CR. #### Should not be necessary!  Perl should be
    # opening these in text mode by default, as the docs claim, and
    # automatically remove the CR's.
    tr/\r//d;

  if (/^begin-unicode-encapsulation-script$/)
    {
      $in_script = 1;
    }
    elsif (/^end-unicode-encapsulation-script$/)
      {
	$in_script = 0;
      }
    elsif ($in_script)
      {
	next if (m!^//!);
	next if (/^[ \t]*$/);
	if (/(file|yes|soon|no|skip|split|begin-bracket|end-bracket)(?: (.*))?/)
	  {
	    my ($command, $parms) = ($1, $2);
	    if ($command eq "file")
	      {
		$current_file = $parms;
	      }
	    elsif ($command eq "begin-bracket")
	      {
		my $current_bracket = $current_bracket[$#current_bracket];
		if (defined ($current_bracket))
		  {
		    $current_bracket .= "&& $parms";
		  }
		else
		  {
		    $current_bracket = "$parms";
		  }
		push @current_bracket, $current_bracket;
	      }
	    elsif ($command eq "end-bracket")
	      {
		pop @current_bracket;
	      }
	    else
	      {
		my ($fun, $reason) = split /\s+/, $parms, 2;
		$files{$current_file}{$fun} = [$command, $reason];
		$bracket{$current_file}{$fun} =
		  $current_bracket[$#current_bracket];
	      }
	  }
	else
	  {
	    print "WARNING: Unknown line $_\n";
	  }
      }
  }


foreach my $file (keys %files)
  {
    $slurp = &FileContents ($file);
    print "Processing file $file\n";
    print HOUT "\n/* Processing file $file */\n\n";
    my $totalspace = 70 - length ("Processing file $file");
    $totalspace = 0 if $totalspace < 0;
    my $alignspaceleft = $totalspace / 2;
    my $alignspaceright = ($totalspace + 1) / 2;
    print COUT "
/*----------------------------------------------------------------------*/
/*" . (" " x $alignspaceleft) . "Processing file $file" .
  (" " x $alignspaceright) . "*/
/*----------------------------------------------------------------------*/

";

    my ($ws_re, $must_ws_re, $tok_ch) =
      ("\\s*", "\\s+", "\\w");
    # unfortunately there is no surefire way short of
    # parsing all include files for typedefs to
    # distinguish types from parameters, and prototypes
    # appear in the include files both with and without
    # parameters -- the latter kinds appear in a very
    # different style and were obviously added later.  so
    # we rely on the fact that defined types are all
    # upper-case, and parameters generally are not, and
    # special-case the exceptions.
    my $typeword_re =
      # note the negative lookahead assertions: the first
      # one excludes the words "X" and "Y" from type
      # words, since they appear as parameter names in
      # CreateWindowEx; the second prevents "void
      # *Argument" from being parsed as a type "void *A"
      # followed by a parameter "rgument".
      "(?:(?!(?:X\\b|Y\\b))(?:unsigned|int|long|short|va_list|[A-Z_0-9]+)(?!${tok_ch}))";
    my $typetoken_re = "(?:$typeword_re$ws_re\\**$ws_re)";
    my $arg_re = "(?:($typetoken_re+)(${tok_ch}+)?(?: OPTIONAL)?)";
    my $fun_re = "(SHSTDAPI_\\(${tok_ch}+\\)|${tok_ch}" . "[A-Za-z_0-9 \t\n\r\f]*?${tok_ch})${ws_re}(${tok_ch}+)W${ws_re}\\(((${ws_re}${arg_re}${ws_re},)*${ws_re}${arg_re}${ws_re})\\);";

    # print "regexp: $fun_re\n";
    while ($slurp =~ /$fun_re/g)
      {
	my ($rettype, $fun, $args) = ($1, $2, $3);
	$processed{$fun} = 1;
	print "Processing: $fun";

	my ($command, $reason) = ($files{$file}{$fun}[0], $files{$file}{$fun}[1]);
	if (!defined ($command))
	  {
	    print " (no command found)\n";
	  }
	else
	  {
	    print "\n";
	    my $bracket = $bracket{$file}{$fun};
	    if (defined ($bracket))
	      {
		print HOUT "#if $bracket\n";
		print COUT "#if $bracket\n\n";
	      }
	    if ($command eq "no")
	      {
		if (!defined ($reason))
		  {
		    print "WARNING: No reason given for `no' with function $fun\n";
		    $reason = "";
		  }

		print HOUT "#undef $fun\n";
		(my $munged_reason = $reason) =~ s/[^A-Za-z0-9]/_/g;
		print HOUT "#define $fun error_$munged_reason\n";
		print COUT "/* Error if $fun used: $reason */\n\n";
	      }
	    elsif ($command eq "skip")
	      {
		if (!defined ($reason))
		  {
		    print "WARNING: No reason given for `skip' with function $fun\n";
		    $reason = "";
		  }

		print HOUT "/* Skipping $fun because $reason */\n";
		print COUT "/* Skipping $fun because $reason */\n\n";
	      }
	    elsif ($command eq "soon")
	      {
		$reason = "" if !defined ($reason);

		print HOUT "/* Not yet: $fun $reason */\n";
		print COUT "/* Not yet: $fun $reason */\n\n";
	      }
	    else
	      {
		my (@args, %argtype, %ansiarg, %xarg, $split_struct,
		    $split_rettype);
		if ($command eq "split")
		  {
		    ($split_struct, $reason) = split /\s+/, $reason, 2;
		  }
		my $argno = 0;
		while ($args =~ /$arg_re/g)
		  {
		    $argno++;
		    my ($argtype, $argname) = ($1, $2);
		    $argtype =~ s/\s*$//;
		    next if $argtype eq "void" || $argtype eq "VOID";
		    $argname = "arg$argno" if !defined ($argname);
		    $argtype{$argname} = $argtype;
		    $ansiarg{$argname} = $argtype;
		    $ansiarg{$argname} =~ s/\bLPWSTR\b/LPSTR/;
		    $ansiarg{$argname} =~ s/\bLPCWSTR\b/LPCSTR/;
		    $xarg{$argname} = $argtype;
		    $xarg{$argname} =~ s/\bLPWSTR\b/Extbyte */;
		    $xarg{$argname} =~ s/\bLPCWSTR\b/const Extbyte */;
		    if (defined ($split_struct))
		      {
			my $fuck_cperl1 = "\\b${split_struct}W\\b";
			my $fuck_cperl2 = "${split_struct}A";
			$ansiarg{$argname} =~ s/$fuck_cperl1/$fuck_cperl2/;
		      }
		    push @args, $argname;
		}
		$rettype =~ s/\bSHSTDAPI_\((.*)\)/$1/;
		$rettype =~ s/\s*WIN\w*?API\s*//g;
		$rettype =~ s/\bAPIENTRY\b//;
		$rettype =~ s/\bSHSTDAPI\b/HRESULT/;
		if ($rettype =~ /LPC?WSTR/)
		  {
		    $split_rettype = 1;
		    $rettype =~ s/\bLPWSTR\b/Extbyte */;
		    $rettype =~ s/\bLPCWSTR\b/const Extbyte */;
		  }
		print HOUT "#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED\n";
		print HOUT "#undef $fun\n";
		print HOUT "#define $fun error_use_qxe${fun}_or_${fun}A_and_${fun}W\n";
		print HOUT "#endif\n";
		if (defined ($reason))
		  {
		    print COUT "/* NOTE: $reason */\n";
		  }
		print COUT "$rettype\nqxe$fun (";
		print HOUT "$rettype qxe$fun (";
		my $first = 1;
		if (!@args)
		  {
		    print COUT "void";
		    print HOUT "void";
		  }
		else
		  {
		    foreach my $x (@args)
		      {
			print COUT ", " if !$first;
			print HOUT ", " if !$first;
			$first = 0;
			print COUT "$xarg{$x} $x";
			print HOUT "$xarg{$x} $x";
		      }
		  }
		print HOUT ");\n";
		print COUT ")\n{\n  if (XEUNICODE_P)\n    ";
		if ($rettype ne "void" && $rettype ne "VOID")
		  {
		    print COUT "return ";
		    print COUT "($rettype) " if $split_rettype;
		  }
		print COUT "${fun}W (";
		$first = 1;
		foreach my $x (@args)
		  {
		    print COUT ", " if !$first;
		    $first = 0;
		    print COUT ($argtype{$x} eq $xarg{$x} ? $x :
				"($argtype{$x}) $x");
		  }
		print COUT ");\n  else\n    ";
		if ($rettype ne "void" && $rettype ne "VOID")
		  {
		    print COUT "return ";
		    print COUT "($rettype) " if $split_rettype;
		  }
		print COUT "${fun}A (";
		$first = 1;
		foreach my $x (@args)
		  {
		    print COUT ", " if !$first;
		    $first = 0;
		    print COUT ($argtype{$x} eq $ansiarg{$x} ? $x :
				"($ansiarg{$x}) $x");
		  }
		print COUT ");\n}\n\n";
	      }
	    if (defined ($bracket))
	      {
		print HOUT "#endif /* $bracket */\n";
		print COUT "#endif /* $bracket */\n\n";
	      }
	    print HOUT "\n";
	  }
      }
  }

foreach my $file (keys %files)
  {
    foreach my $fun (keys %{$files{$file}})
      {
	if (!$processed{$fun} && $files{$file}{$fun}[0] =~ /^(yes|soon|split)$/)
	  {
	    print "WARNING: Can't locate prototype for $fun\n";
	  }
      }
  }


sub FileContents
{
  local $/ = undef;
  open (FILE, "< $dir/$_[0]") or die "$dir/$_[0]: $!";
  my $retval = scalar <FILE>;
  # must hack away CRLF junk.
  $retval =~ s/\r\n/\n/g;
  return $retval;
}
