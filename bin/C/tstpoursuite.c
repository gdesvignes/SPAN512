#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[], char *arge[])
{int ixc,izc,iincc,errorcode,stopcode;

int jyr,jmn,jdy,jhr,jmi,jsc,jcc;	// -- Date and TStime to start observation
float asdr,decl;			// -- R.A. and DEC. of target
float duree=1080.0;			// -- Integration Time 18mnx60s=1080s --
double xc,zc,yc,rc,xcc,ycc,incc;	// -- Receiver positions
double tural,tupou,tutot;		// -- Durees des ralliement (tural), poursuite (tupou), duree totale (tutot)
double rtsdman,rtsfpou;			// -- Temps Sideral de depart manip (rtsdman) et de fin de manip (rtsfpou)

xc=0.0; zc=0.0; incc=60000.0;
jyr=2012; jmn=1; jdy=15;
jhr=20; jmi=30; jsc=0; jcc=0;
asdr=jhr+jmi/60.0+0.5; decl=38.5;

duree=1080.0;
yc=rc=xcc=ycc=0.0;

if(argc>=8) {
  sscanf(argv[1],"%f",&asdr);		// -- RA --
  sscanf(argv[2],"%f",&decl);		// -- DEC --
  sscanf(argv[3],"%d",&jyr);	// -- Year --
  sscanf(argv[4],"%d",&jmn);	// -- Month --
  sscanf(argv[5],"%d",&jdy);	// -- Day --
  sscanf(argv[6],"%d",&jhr);	// -- Sideral Hour --
  sscanf(argv[7],"%d",&jmi);	// -- Sideral Min --
  if(argc>=11) {
    sscanf(argv[8],"%d",&ixc); xc=(double)ixc;
    sscanf(argv[9],"%d",&izc); zc=(double)izc;
    sscanf(argv[10],"%d",&iincc); incc=(double)iincc;
    printf("%s> Read xc= %.1lf  zc= %.1lf  incc= %.1lf\n",argv[0],xc,zc,incc);
  }
} else {
  printf(" Use : %s RA(float) DEC(float) Year(int) Month(int) Day(int) SideralHour(int) SideralMin(int) [Xc(int) Zc(int) Incc(int)]\n",argv[0]);
  return(-1);
}

printf("%s> Calling fillsou with asdr= %f decl= %f\n",argv[0],asdr,decl);
fillsou_(&asdr,&decl);
fillmap_(&duree);

printf("%s> Calling calpou() ...\n",argv[0]);
// printf("%s> tsbuf= %d %d %d %d %d %d %d\n",argv[0],tsbuf[0],tsbuf[1],tsbuf[2],tsbuf[3],tsbuf[4],tsbuf[5],tsbuf[6]);
calpou_(&errorcode,&stopcode,&jyr,&jmn,&jdy,&jhr,&jmi,&jsc,&jcc,
   &xc,&zc,&yc,&rc,&xcc,&ycc,&incc,&tural,&tupou,&tutot,&rtsdman,&rtsfpou);

printf("%s> rtsdman= %.6lf rtsfpou= %.6lf  tural= %.6lf tupou= %.6lf tutot= %.6lf\n",argv[0],rtsdman,rtsfpou,tural,tupou,tutot);
printf("%s> Ratio onSky/Total= %.3lf\n",argv[0],tupou/tutot);

return(0);
}
