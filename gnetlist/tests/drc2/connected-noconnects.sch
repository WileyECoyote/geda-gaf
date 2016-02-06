v 20160214 2
C 40000 40000 0 0 0 title-B.sym
C 48500 45300 1 90 0 resistor-drc2.sym
{
T 48200 45400 5 10 0 1 90 0 1
device=RESISTOR
T 48550 45750 5 10 1 1 0 0 1
refdes=R1
}
C 49200 45300 1 90 0 resistor-drc2.sym
{
T 48950 45350 5 10 0 1 90 0 1
device=RESISTOR
T 49200 45750 5 10 1 1 0 0 1
refdes=R2
}
C 48300 45000 1 0 0 gnd-1.sym
C 49000 45000 1 0 0 gnd-1.sym
N 48400 46200 48400 46600 4
N 48400 46400 49100 46400 4
{
T 48450 46450 5 10 1 1 0 0 1
netname=bad_net
}
N 49100 46400 49100 46200 4
C 49800 45300 1 90 0 resistor-drc2.sym
{
T 49550 45400 5 10 0 1 90 0 1
device=RESISTOR
T 49850 45750 5 10 1 1 0 0 1
refdes=R3
}
C 49600 45000 1 0 0 gnd-1.sym
C 48200 46600 1 0 0 nc-top-1.sym
{
T 48600 46950 5 10 0 1 0 0 1
footprint=none
T 48600 46800 5 10 0 0 0 0 1
symversion=0.2
T 48600 47100 5 10 0 0 0 0 1
value=NoConnection
T 48600 47300 5 10 0 0 0 0 1
device=DRC_Directive
}
C 49500 46200 1 0 0 nc-top-1.sym
{
T 49900 46550 5 10 0 1 0 0 1
footprint=none
T 49900 46400 5 10 0 0 0 0 1
symversion=0.2
T 49900 46700 5 10 0 0 0 0 1
value=NoConnection
T 49900 46900 5 10 0 0 0 0 1
device=DRC_Directive
}
