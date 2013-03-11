#!/usr/bin/perl
# -*- perl -*-
# Copyright 2006 DJ Delorie, licenced under GPL v2

# Additions by (* jcl *)

#   1. Added support for multiple files on the command line
#   2. Misc. Perlification

$Scale = shift;
die "Scale must be greater than 0" unless $Scale > 0;

%syntax = map { my @v = split; shift(@v) => [@v] }
    ("L 1 1 1 1 0 1 0 0 1 1",
     "G 1 1 1 1 0 0 0 0",
     "B 1 1 1 1 0 1 0 0 1 1 0 1 0 1 0 1",
     "V 1 1 1 0 1 0 0 1 1 0 1 0 1 0 1",
     "A 1 1 1 0 0 0 1 0 0 1 1",
     "T 1 1 0 1 0 0 0 0 0",
     "N 1 1 1 1 0",
     "U 1 1 1 1 0 0",
     "P 1 1 1 1 0 0 0",
     "C 1 1 0 0 0 0",
     "F 0 1 0");

foreach $infilename (@ARGV) {
    $outfilename = $infilename;
    $outfilename =~ s/\.sym$/_$Scale.sym/;
    open(OUT, ">$outfilename") || die "Could not open $outfilename for output";
    while (<>) {
	s/[\r\n]+$//;
	my ($type, @args) = split;
	if ($syntax{$type}) {
	    my @scale_p = @ { $syntax{$type} };
	    die "The number of args does not match the syntax definition at input line $."
		unless $#args = $#scale_p;
	    foreach my $i (0..$#scale_p) {
		next unless $scale_p[$i];
		$args[$i] = int($args[$i] * $Scale);
	    }
	    print OUT join(' ', $type, @args), "\n";
	    if ($type eq "T") {
		foreach (1..$args[8]) {
		    $_ = <>;
		    print OUT;
		    last if eof;
		}
	    }
	} else {
	    print OUT "$_\n";
	}
    }
    close(OUT) || die "Could not close $outfilename";
}



# Style (adapted from the Perl Cookbook, First Edition, Recipe 12.4)

# 1. Names of functions and local variables are all lowercase.
# 2. The program's persistent variables (either file lexicals
#    or package globals) are capitalized.
# 3. Identifiers with multiple words have each of these
#    separated by an underscore for readability.
# 4. Constants are all uppercase.
# 5. If the arrow operator (->) is followed by either a
#    method name or a variable containing a method name then
#    there is a space before and after the operator.

