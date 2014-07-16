#!/usr/bin/perl
#
# Copyright (C) 2013-2014 Robert Zeegers ( geda at myken dot nl)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
#
# Usage:
# cd $PREFIX/gEDA/sym/vhdl
# ./verilog2vhdl.pl
#

use strict;
use warnings;

sub SetToInvisible {
# wrote this sub routing because I couldn't find a other way of changing the fifth character of the line.
	if (@_ != 1) {
		print "Warning: number of arguments passed to SetToInvisible subrouting is incorrect\n";
	}
	my @lineparts = split(' ', $_[0]);
	print VHDLFILE "$lineparts[0] "; # Character T
	print VHDLFILE "$lineparts[1] "; # First X coordinate
	print VHDLFILE "$lineparts[2] "; # First Y coordinate
	print VHDLFILE "$lineparts[3] "; # Color index
	print VHDLFILE "$lineparts[4] "; # Size of text
	print VHDLFILE "0 "; 			 # Visibility of text
	print VHDLFILE "$lineparts[6] "; # Attribute visibility control
	print VHDLFILE "$lineparts[7] "; # Angle of the text
	print VHDLFILE "$lineparts[8] "; # Alignment/origin of the text
	print VHDLFILE "$lineparts[9]\n"; # Number of lines of text (1 based)
}

my $verilogdir = "../verilog";

    opendir(DIR, $verilogdir) or die $!;

    while (my $verilogfile = readdir(DIR)) {

        # We only want files
        next unless (-f "$verilogdir/$verilogfile");

        # Use a regular expression to find files ending in
		# (n)and-1.sym or (x|n)or-1.sym or ipad-1.sym or opad-1.sym or iopad-1.sym
        next unless ($verilogfile =~ m/(^(a|na)nd[0-9]|^(o|no|xo)r[0-9]|ipad|opad|iopad)-1\.sym$/i);

		# get the number of ports from the symbol file name
		(my $portnumber) = $verilogfile =~ /(\d+)/; # in case of ipad, opad and iopad the port number will be set to 1
		       										# extracted from the -1.sym. This no problem since we will not use the
													# $portnumber variable with ipad, opad or iopad symbols.
		# Check if the file realy exists.
		if (-e $verilogfile) {
			print "Processing: $verilogdir/$verilogfile with $portnumber ports\n";

			open(VERILOGFILE, "$verilogdir/$verilogfile") or die "cannot open verilog symbol file: $verilogfile : $!\n";
			open(VHDLFILE, "> $verilogfile") or die "cannot open vhdl symbol file: $verilogfile : $!\n";

			# For the next part I concatenate lines containing a text and attributes object type
			# Like:
			# T 1000 600 5 8 0 0 0 0 1
			# pinnumber=OUT
			#
			# This way it will be easier to process the combination without a look forward or look backward.
			# Sure there are much easier and smarter ways of doing this but I like simple and easy to follow code.
			# This process will break if a file contains a text and attribute object that spans more than 2 lines.
			# Like:
			# T 1000 600 5 8 0 0 0 0 1
			# Last summer we drove
			# to the river.
			#
			# TODO: Check the num_lines field and account for multiple lines.
			#
			# The used verilog files don't have text and attribute object spanning 2 line so for now I'm OK
			my @lines = ();
			my $line;
			my $nextline;
			my $newline;
			while( $line = <VERILOGFILE> ) {
				chomp ($line);
				if ($line =~ /^T.*/) {			# We found a text and attribute object.
					$nextline = <VERILOGFILE>;	# Read the next line.
					if( $nextline ) {			# Check if there is a nextline. Just in case we reached the end of the file.
						chomp ($nextline);
						$newline = $line . "===" . $nextline; # Concatenate the lines
						push (@lines, $newline);
					} else {
						push (@lines, $line);	# If there is no nextline just push the line we do have it in our array.
												# This is an error in the file format since this combination always must have at least 2 lines.
						print "Warning: possible file format error in $verilogdir/$verilogfile\n";
					}
				} else {
					push (@lines, $line);		# Push any other line.
				}
			}

			for my $i (0 .. $#lines)
			{
				if( $lines[$i] =~ /===/ ) {
					my @subline = split("===", $lines[$i]);
					if ($subline[1] =~ /pinnumber=/) {
						my @values = split("=",$subline[1]);
						if ($values[1] =~ /^OUT/) {
							print VHDLFILE "$subline[0]\n";
							print VHDLFILE "$subline[1]0\n"; # We append a 0 (zero) to the line pinnumber=OUT since OUT is a reserved word in VHDL
							&SetToInvisible($subline[0]);
							print VHDLFILE "pintype=OUT\n";
						} elsif ($values[1] =~ /^IN\d+/) {
							print VHDLFILE "$subline[0]\n";
							print VHDLFILE "$subline[1]\n";
							&SetToInvisible($subline[0]);
							print VHDLFILE "pintype=IN\n";
						} elsif ($values[1] =~ /^INOUT/) {
							print VHDLFILE "$subline[0]\n";
							print VHDLFILE "$subline[1]\n";
							&SetToInvisible($subline[0]);
							print VHDLFILE "pintype=INOUT\n";
						} elsif ($values[1] =~ /^OPAD/) {
							print VHDLFILE "$subline[0]\n";
							print VHDLFILE "$subline[1]\n";
							&SetToInvisible($subline[0]);
							print VHDLFILE "pintype=OUT\n";
						} elsif ($values[1] =~ /^IPAD/) {
							print VHDLFILE "$subline[0]\n";
							print VHDLFILE "$subline[1]\n";
							&SetToInvisible($subline[0]);
							print VHDLFILE "pintype=IN\n";
						} elsif ($values[1] =~ /^IOPAD/) {
							print VHDLFILE "$subline[0]\n";
							print VHDLFILE "$subline[1]\n";
							&SetToInvisible($subline[0]);
							print VHDLFILE "pintype=INOUT\n";
						}
					# Next add the port number we found to de definition of the device.
					} elsif( $lines[$i] =~ /device=((a|na)nd|(o|no|xo)r)/ ) {
						print VHDLFILE "$subline[0]\n";
						print VHDLFILE "$subline[1]$portnumber\n";
					# Change the verilog_port attribute.
					# It is not necessary to change it, we can also remove it. I like to change it for possible future projects ;-)
					} elsif( $lines[$i] =~ /VERILOG_PORTS/ ) {
							print VHDLFILE "$subline[0]\n";
							print VHDLFILE "VHDL_ARCHITECTURE=rtl\n";
					# Next print al the text and attribute object lines that are processed yet.
					} else {
						print VHDLFILE "$subline[0]\n";
						print VHDLFILE "$subline[1]\n";
					}
				# print all remaining lines.
				} else {
					print VHDLFILE "$lines[$i]\n";
				}
			}
			close(VERILOGFILE);
			close(VHDLFILE);
		} else {
			print "Cannot open verilog symbol file: $verilogdir/$verilogfile : file doesn't exitst\n";
		}
    }

    closedir(DIR);
    exit 0;

