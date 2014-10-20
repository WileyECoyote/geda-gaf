Symbols for Vishay MOSFET's were generated using gmake-transistor-sym
using the template and modifier options. The command-line syntax is:

$ gmake-transistors-sym -t -m pmosfet vishay_pmosfets
$ gmake-transistors-sym -t -m nmosfet vishay_nmosfets

The .spm parameter files exported from the .xls file were then modified
be deleting all lines not having the PPSOP package and the final 8 (the
package modifier option) was changed to 12, and PPSOP was substituted
with 1212-8, then the two commands above were re-issued to generate
the dash 12 style.

