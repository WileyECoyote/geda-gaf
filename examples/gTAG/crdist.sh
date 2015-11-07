#!/bin/sh

DATETAG=`date '+%Y%m%d'`
DISTFILE=gTAG-sch-${DATETAG}

gTAG_Files="gTAG/attribs gTAG/crdist.sh gTAG/gafrc gTAG/gnetlistrc gTAG/gTAG.sch
            gTAG/gTAG-pcb.net gTAG/ChangeLog-1.0 gTAG/README gTAG/bom/gTAG.bom
            gTAG/bom/gTAG_bom.csv
            gTAG/ps/*.ps
            gTAG/sch/*.sch
            gTAG/sht/*.sym
            gTAG/sym/*.sym"

rm -f gTAG-sch-*.tar.gz rm bom/gTAG.bom bom/gTAG_bom.csv *~ */*~ #gTAG.pcb
gnetlist -g bom -o bom/gTAG.bom gTAG.sch
gnetlist -g bom2 -o bom/gTAG_bom.csv gTAG.sch
gnetlist -g PCB -o gTAG-pcb.net gTAG.sch

cd ..
tar cf gTAG/${DISTFILE}.tar $gTAG_Files
cd gTAG
gzip ${DISTFILE}.tar
