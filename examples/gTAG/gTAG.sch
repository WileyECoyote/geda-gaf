v 20151204 2
C 44300 51500 0 0 0 title-A2.sym
N 52800 59800 54100 59800 4
{
T 53300 59900 5 14 1 1 0 0 1
netname=tdi_ttl
}
N 52800 59400 54100 59400 4
{
T 53300 59500 5 14 1 1 0 0 1
netname=tdo_ttl
}
N 52800 59000 54100 59000 4
{
T 53300 59100 5 14 1 1 0 0 1
netname=tms_ttl
}
N 52800 58600 54100 58600 4
{
T 53300 58700 5 14 1 1 0 0 1
netname=tck_ttl
}
N 52800 58200 54100 58200 4
{
T 53300 58300 5 14 1 1 0 0 1
netname=trst_ttl
}
N 53900 62300 51200 62300 4
N 51200 62300 51200 60300 4
{
T 49800 61000 5 14 1 1 0 0 1
netname=jtag_power
}
N 57100 62300 60100 62300 4
N 60100 62300 60100 60300 4
{
T 60200 61200 5 14 1 1 0 0 1
netname=usb_power
}
N 57300 59100 58600 59100 4
{
T 57500 59200 5 14 1 1 0 0 1
netname=usb_io+
}
N 57300 58800 58600 58800 4
{
T 57500 58900 5 14 1 1 0 0 1
netname=usb_io-
}
C 54100 58000 1 0 0 gTAG-ucont.sym
{
T 55085 60100 5 10 1 1 0 0 1
description=USB Controller
T 54400 60100 5 10 1 1 0 0 1
refdes=S1
T 54900 57700 5 14 1 1 0 0 1
source=gTAG-ucont.sch
}
C 49900 58000 1 0 0 gTAG-jtagio.sym
{
T 49900 60100 5 10 1 1 0 0 1
refdes=S2
T 50300 57700 5 14 1 1 0 0 1
source=gTAG-jtagio.sch
T 51000 59000 5 10 1 1 0 3 1
description=JTAG Interface
}
C 58600 58000 1 0 0 gTAG-consio.sym
{
T 58900 60100 5 10 1 1 0 0 1
refdes=S3
T 59300 57700 5 14 1 1 0 0 1
source=gTAG-consio.sch
T 60200 59057 5 10 1 1 0 3 1
description=Console IO
}
C 53900 61700 1 0 0 gTAG-psu.sym
{
T 54200 63600 5 10 1 1 0 0 1
refdes=S4
T 54800 61400 5 14 1 1 0 0 1
source=gTAG-psu.sch
T 55500 63100 5 10 1 1 0 3 1
description=Power Supply Unit
}
N 57300 59800 57400 59800 4
N 57400 59800 57400 61900 4
{
T 57500 60900 5 14 1 1 0 0 1
netname=pon_reset#
}
C 44600 51700 1 0 0 copyleft.sym
N 57400 61900 57100 61900 4
T 50500 57100 9 14 1 0 0 0 1
JTAG (IEEE1149.1) consist of tdi, tdo, tck, tms, (trst)
T 62200 51600 9 10 1 0 0 0 1
5
T 60700 51600 9 10 1 0 0 0 1
1
T 48500 65200 9 50 1 0 0 0 1
gTAG - usb to jtag interface
T 61500 52400 9 20 1 0 0 0 1
gTAG - top level
T 60700 51900 9 10 1 0 0 0 1
gTAG.sch
T 64500 51600 9 10 1 0 0 0 1
Stefan Petersen (spe@stacken.kth.se)
T 64500 51900 9 10 1 0 0 0 1
$Revision$
