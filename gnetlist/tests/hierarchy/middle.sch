v 20160214 2
N 6700 4500 5900 4500 4
{
T 5900 4700 5 10 1 1 0 0 1
netname=middleA
}
N 8300 4500 9600 4500 4
{
T 8900 4700 5 10 1 1 0 0 1
netname=middleB
}
C 5300 4400 1 0 0 in-1.sym
{
T 5400 5050 5 10 0 0 0 0 1
symversion=0.4
T 5400 4850 5 10 0 0 0 0 1
device=none
T 5850 4450 5 10 0 1 0 8 1
footprint=none
T 5300 4700 5 10 1 1 0 0 1
refdes=A
}
C 9600 4400 1 0 0 out-1.sym
{
T 9700 4800 5 10 0 0 0 0 1
symversion=0.4
T 10200 4650 5 10 0 1 0 0 1
device=OUTPUT
T 10200 4500 5 10 0 0 0 0 1
footprint=none
T 9900 4700 5 8 1 1 0 0 1
refdes=B
}
C 6700 3900 1 0 0 bottom.sym
{
T 7000 5000 5 10 1 1 0 0 1
refdes=Umiddle
T 7500 5300 5 10 1 1 0 0 1
source=bottom.sch
}
