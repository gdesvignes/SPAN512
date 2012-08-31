cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Cette librairie contient tout les modules fortran de converions de temps
c   Les temps D70 sont des temps exprimes en nanosecondes ecoulees depuis
c   le 1er Janvier 1970 a 0h TU.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_djl0(ia,im,ij)
c
c  calcule la date julienne a 0 h t.u. pour le jour donne
c
	implicit none
	integer*4 ia,im,ij,ip,it,jdn

	it=ia*365+ia/4+1721013
	ip=2-(4-mod(ia,4))/4
	jdn=(im*3057)/100+ij+it
	if(im.lt.3)jdn=jdn+ip
	f_djl0=jdn-0.5d0
	return
	end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cvdjlgreg(datjul,iy,im,id,iwd)
c
c   conversion d'une date julienne en date gregorienne
c
	IMPLICIT INTEGER*4 (I-N)
      double precision  dat000,datjul
      dimension mntday(13)
      data dat000/1721425.5d0/
      data mntday/0,31,59,90,120,151,181,212,243,273,304,334,365/
c
      idays=datjul-dat000
      jdate=idays
      iy=idays/146097*400
      irest=mod(idays,146097)
      iadd=irest/36524
      if(iadd.ne.4) go to 5
      iy=iy+400
      idays=365
      go to 15
 5    iy=iy+iadd*100
      irest=mod(irest,36524)
      iadd=irest/1461
      iy=iy+iadd*4
      irest=mod(irest,1461)
      iadd=irest/365
      if(iadd.ne.4) go to 10
      iy=iy+4
      idays=365
      go to 15
 10   iy=iy+iadd+1
      idays=mod(irest,365)
 15   leap=0
      if(mod(iy,4).ne.0) go to 20
      leap=1
      if(mod(iy,100).eq.0.and.mod(iy,400).ne.0) leap=0
      idays=idays-leap
 20   do 50 i50=2,13
      if (idays.ge.mntday(i50)) go to 50
      id=idays-mntday(i50-1) +1
      im = i50-1
      if(leap.eq.0) go to 60
      if(i50.gt.3) go to 60
      id = id + 1
      if (id.ne.32 ) go to 60
      id = 1
      im = 2
      go to 60
 50   continue
 60   iwd=mod(jdate,7)+1
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_ts0(datjul)
c
c  calcule en rotation le TS a 0 heure TU a Nancay pour le jour donne
c
	implicit none
        include 'constante_def.inc'
	real*8 cd,c1,c2,c3,datjul,t,t2,t3,ts0
c
	cd=24110.54841d0
	c1=8640184.812866d0
        c2=0.093104d0
	c3=-6.2d-6
	t=(datjul-2451545.0d0)/36525.0d0
	t2=t*t
	t3=t*t*t
c
c   temps sideral a 0h TU a Nancay a la date DATJUL ...en rotation
c
	ts0=(cd +c1*t + c2*t2 +c3*t3)/86400.d0 - longitude_nancay
        ts0=mod(ts0,1.d0)
	if(ts0.lt.0.d0)ts0=ts0+1.d0
        f_ts0=ts0
	return
	end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_cvtstu(ia,im,ij,ts,ipass)
c
c   Conversion du temps sideral local en temps universel
c   ia,im,ij = date de conversion <--
c   TS= temps sideral local ... en rotation  <--
c   f_cvtstu= heure t.u. correspondante ... en rotation  -->
c   ipass=1 => un seul passage a la date donnee, ipass=2 => deux passages  -->
c
c
c   dans tous les cas 'tu' est l'heure du premier passage
c   pour avoir l'heure t.u. du deuxieme passage, il suffit de faire
c   tu=tu+cv_dts_dtu
c
c
	implicit none
	include 'constante_def.inc'
	integer*4 ia,im,ij,ipass
	real*8 datjul,tu,ts,dts,ts0,f_ts0,f_djl0
	real*8 dat,epsil,depsil,dnut,ts1,ts2,obliq
c
        datjul=f_djl0(ia,im,ij)
	ts0=f_ts0(datjul)
c
	dts=ts-ts0 
c       I.Cognard a decommente les 3 lignes dts=mod(dts+1.d0,1.d0)
	dts=mod(dts+1.d0,1.d0)

c
c   premiere determination de TU pour calcul de la nutation
c
	tu=dts*cv_dts_dtu
c
	dat=datjul+tu
	epsil=obliq(dat)
	call nut(dat,dnut,depsil)
	epsil=epsil +depsil
	dnut=cos(epsil)*dnut
	dnut=dnut*cv_rad_rot			! en rotation
	ts1=ts-dnut
	dts=ts1-ts0
	dts=mod(dts+1.d0,1.d0)

c
c   deuxieme determination de TU pour une autre iteration
c
	tu=dts*cv_dts_dtu
c
	dat=datjul+tu
	epsil=obliq(dat)
	call nut(dat,dnut,depsil)
	epsil=epsil +depsil
	dnut=cos(epsil)*dnut
	dnut=dnut*cv_rad_rot			! en rotation
	ts2=ts-dnut
	dts=ts2-ts0
	dts=mod(dts+1.d0,1.d0)

c
c   troisieme determination de TU pour resultat
c
	tu=dts*cv_dts_dtu
c
	f_cvtstu=tu
	ipass=1
	if(((dts+1.d0)*cv_dts_dtu).lt.1.d0)ipass=2
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_cvtuts(ia,im,ij,tu)
c
c   Conversion du temps universel en temps sideral local
c   pour la date defini par ia,im,ij et a l'heure TU (en fraction de jour)
c   formule de l astronomical almanac:
c	gmst at 0 h ut
c	=24110.54841 + 8640184.812866 t + 0.093104 t*t - 6.2e-6 t*t*t
c	t=(jd - 2451545.0)/36525			
c
	implicit none
	include 'constante_def.inc'
        integer*4 ia,im,ij
	real*8 datjul,tu,ts0,ts,dts,f_ts0,f_djl0
	real*8 dat,epsil,dnut,obliq,depsil

        datjul=f_djl0(ia,im,ij)
        ts0=f_ts0(datjul)
	dts=tu*cv_dtu_dts
	dat=datjul+tu
	epsil=obliq(dat)
	call nut(dat,dnut,depsil)
	epsil=epsil +depsil
	dnut=cos(epsil)*dnut
	dnut=dnut*cv_rad_rot		! en rotation
	ts=ts0   +    dnut  +  dts
	ts=mod(ts,1.0d0)
	if(ts.lt.0.0d0)ts=ts+1.0d0
	f_cvtuts=ts
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function obliq(dat)
c
c	obliquite moyenne  de l eccliptique en radians
c	standard 1984		astron. alma. 1984 s26
c
	implicit real*8 (d)
	real*8 csar
	data csar/4.848136811095356d-06/	! conv sec arc en radians
	dt=(dat-2451545.0d0)/36525.0d0
	dt2=dt*dt
	dt3=dt*dt*dt
	de=84381.448d0 - 46.8150d0 * dt -0.00059d0 * dt2 +
     1	dt3*0.001813d0
	obliq=de*csar
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine nut(datjul,dnut,depsil)
c
c   calcul de la nutation en longitude   .... dnut
c   		     en obliquite        .... depsil
c   date julienne
c
c   improved iau system	reference j2000.0
c
c   pour le calcul des arguments     c1(1-4) ....
c   les arguments dans les sinus     c(5)
c
	implicit real*8 (d), real*8 (t), real*8 (c)
	dimension c(5),c1(5),c2(5),c3(5),c4(5),c5(5)
	equivalence (c(1),cl),(c(2),cll),(c(3),cf),(c(4),cd),(c(5),cg)
c	.............................................................
c		les coefficients pour les * les sinus
c
	dimension icnut(106),ictnut(106)
	dimension icbl(106),ictbl(106)
	integer*2 icnut,ictnut,icbl,ictbl
c	................................... coefficients des sinus
	dimension pl(106),pll(106),pf(106),pd(106),pg(106)
	dimension p (106,5)
C	logical*1 pl,pll,pf,pd,pg
C	logical*1 p
	integer*1 pl,pll,pf,pd,pg
	integer*1 p
	equivalence (p(1,1),pl(1)),(p(1,2),pll(1)),(p(1,3),pf(1))
	equivalence (p(1,4),pd(1)),(p(1,5),pg(1))
c	................................................. data
	data pl/0,0,-2,2,-2,1,0,2,5*0,2,5*0,-2,0,2,0,1,2,3*0,-1,0,0,
     1	1,0,1,1,-1,0,1,-1,-1,1,0,2,1,2,0,-1,-1,1,-1,1,0,0,1,1,2,0,0,1,
     2	0,1,2,0,1,0,1,1,1,-1,-2,3,0,1,-1,2,1,3,0,-1,1,-2,-1,2,1,1,-2,-1,
     3	1,2,2,1,0,3,1,0,-1,3*0,1,0,1,1,2,0,0/
c	................................................. data
	data pll/5*0,-1,-2,0,0,1,1,-1,3*0,2,1,2,-1,0,-1,0,1,0,1,0,1,1,0,1,
     1	20*0,1,1,-1,7*0,-1,0,1,0,0,1,0,-1,-1,0,0,-1,1,10*0,1,3*0,-1,
     2	6*0,1,-1,0,0,1,0,-1,1,3*0,1/
c	................................................. data
	data pf/0,0,2,-2,2,0,2,-2,2,0,3*2,0,2,2*0,2,2*0,2,0,2,2*0,2*-2,
     1	2*0,2*2,0,2*2,0,2,3*0,3*2,0,4*2,2*0,2,0,3*2,0,2,0,2*2,2*0,2,
     2	0,-2,2*0,3*2,0,4*2,3*0,2,2*0,2*2,0,3*2,4,0,2*2,0,4,3*2,0,-2,2,
     3	0,-2,2,0,-2,0,2,0/
c	................................................. data
	data pd/5*0,-1,-2,0,-2,0,5*-2,2*0,-2,0,2,3*-2,-1,-2,2*2,0,1,-2,
     1	4*0,-2,0,2,2*0,2,0,2,0,-2,3*0,2,-2,2,-2,2*0,2*2,-2,2*2,
     2	2*-2,2*0,-2,0,1,3*0,2,2*0,2,0,-2,3*0,1,0,-4,2,4,-4,-2,2,4,
     3	0,2*-2,2*2,3*-2,0,2,0,-1,2,-2,0,-2,2*2,4,1/
c	................................................. data
	data pg/1,2,1,0,2,0,1,1,2,0,2*2,1,3*0,1,2,5*1,2*0,1,0,2,1,0,
     1	2,0,1,2,0,2,0,2*1,2,1,2,0,2*2,0,4*1,0,3*2,0,2,4*1,0,1,5*0,
     2	2*2,1,3*2,2*1,2,0,2*2,0,2*2,0,2,1,2*2,0,1,2,1,2*2,0,3*1,
     3	2,2*0,2*1,2*0,2,0/
c	................................................. data
c			icnut(1)= -171996    >. i*2
c		         ***************************
	data icnut/-0,2062,46,11,2*-3,-2,1,-13187,1426,-517,
     1	217,129,48,-22,17,-15,-16,-12,-6,-5,4,4,-4,2*1,-1,2*1,-1,
     2	-2274,712,-386,-301,-158,123,2*63,-58,-59,-51,-38,2*29,
     3	-31,26,21,16,-13,-10,-7,7,-7,-8,2*6,-6,-7,6,-5,5,-5,-4,4,
     4	-4,-3,3,2*-3,-2,2*-3,2,-2,2,-2,2*2,1,-1,1,-2,-1,1,2*-1,
     5	3*1,2*-1,2*1,-1,2*1,7*-1,1,-1,1/
c	................................................. data
	data ictnut/-1742,2,6*0,-16,-34,12,-5,1,2*0,-1,0,1,12*0,
     1	-2,1,-4,4*0,1,-1,67*0/
c	................................................. data
c			icbl(1)= 92025    >. i*2
c		         ***************************
	data icbl/0,-895,-24,0,1,0,1,0,5736,54,224,-95,-70,
     1	1,2*0,9,7,6,2*3,2*-2,7*0,977,-7,200,129,-1,-53,-2,-33,
     2	32,26,27,16,-1,-12,13,-1,-10,-8,7,5,0,-3,2*3,0,-3,
     3	2*3,-3,3,0,3,5*0,5*1,-1,1,-1,1,0,2*-1,0,-1,1,0,-1,
     4	2*1,2*0,-1,17*0/
c	................................................. data
	data ictbl/89,5,6*0,-31,-1,-6,3,18*0,-5,2*0,-1,72*0/
c	...................................................................
	data c1/485866.733d0,715922.633d0,31.310d0,0.064d0,1717200.0d3/
	data c2/1287099.804d0,1292581.224d0,-0.577d0,-0.012d0,128304.0d3/
	data c3/335778.877d0,295263.137d0,-13.257d0,0.011d0,1739232.0d3/
	data c4/1072261.307d0,1105601.328d0,-6.891d0,0.019d0,1601856.0d3/
	data c5/450160.280d0,-482890.539d0,7.455d0,0.008d0,-6480.0d3/
c
c	.............................................................
c	constantes
      data conwr/4.848136811095356d-10/		! conversion en radians
      data convsr/4.848136811095356d-6/		! conversion en radians
c						! 0.0001 sec arc
c
c	calcul de t
c
	t=(datjul-2451545.0d0)/36525.0d0
	t2=t*t
	t3=t*t*t
c	.............................................................
c
c	calcul des elements
c
	cl=c1(1) + c1(2)*t + c1(3)*t2 + c1(4)*t3+ c1(5)*t
	cll=c2(1) + c2(2)*t + c2(3)*t2 + c2(4)*t3+ c2(5)*t
	cf=c3(1) + c3(2)*t + c3(3)*t2 + c3(4)*t3+ c3(5)*t
	cd=c4(1) + c4(2)*t + c4(3)*t2 + c4(4)*t3+ c4(5)*t
	cg=c5(1) + c5(2)*t + c5(3)*t2 + c5(4)*t3+ c5(5)*t
c
c
c
	td=t/10.0d0			! correction du data ictnut .. ictbl
c					on initialise dnut et depsil
c					par icnut(1)*dsin(cg)
c					par icbl(1)*dcos(cg)
	dnut=(-171996.0d0 - 1742.0d0*td)*dsin(cg*convsr)
	depsil=(92025.0d0 + 89.0d0*td)*dcos(cg*convsr)
c
c		boucle de sommation
c
	do 50 i=2,106
	darg=0.0d0
c				calcul de l argument des sin ou cos
	do 40 j=1,5
	darg=darg + p(i,j)*c(j)
40	continue
	dargrd=darg*convsr
	csin=dsin(dargrd)
	ccos=dcos(dargrd)
	dnut=dnut +  ( icnut(i) + ictnut(i)*td  )*csin
	depsil=depsil +  ( icbl(i) + ictbl(i)*td  )*ccos
50	continue
c
c	dnut	par les formules donne en 0"0001
c	depsil		 "
c
c	conversion en radian
c
	dnut=dnut*conwr
	depsil=depsil*conwr
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer*8 function f_cvgregd70(ia,im,ij,ih,iu,is,ic,dtu)

      implicit none
      include 'constante_def.inc'
      integer*4 ia,im,ij,ih,iu,is,ic,dtu
      integer*8 jnansec,hnansec,ih8,iu8,is8
      real*8 f_djl0

      ih8=ih
      iu8=iu
      is8=is
      jnansec=(f_djl0(ia,im,ij)-djl070)*8640000*10000000
      hnansec=(ih8*360000+iu8*6000+is8*100+ic+dtu)*10000000
      f_cvgregd70=jnansec+hnansec
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cvd70greg(nansec,ia,im,ij,ih,iu,is,ic,js)
c
c   js=jour de la semaine 1=lundi ...
c
      implicit none
      include 'constante_def.inc'
      integer*4 ia,im,ij,ih,iu,is,ic,js
      integer*8 nansec,f_rndcsd70,jnansec,hnansec,ih8,iu8,is8,ic8
      real*8 datjul

      nansec=f_rndcsd70(nansec)
      jnansec=nansec/86400000000000
      hnansec=nansec-(jnansec*86400000000000)
C      hnansec=nansec-(jnansec*86.4D12)
      datjul=djl070+dfloat(jnansec)
      call cvdjlgreg(datjul,ia,im,ij,js)
      ih8=hnansec/3600000000000
C      ih8=hnansec/3.6D12
      hnansec=hnansec-(ih8*3600000000000)
C      hnansec=hnansec-(ih8*3.6D12)
C      iu8=hnansec/60000000000
      iu8=hnansec/60000000000
C      hnansec=hnansec-(iu8*60000000000)
      hnansec=hnansec-(iu8*6.0D10)
C      is8=hnansec/1000000000
      is8=hnansec/1000000000
C      hnansec=hnansec-(is8*1000000000)
      hnansec=hnansec-(is8*1000000000)
C      ic8=hnansec/10000000
      ic8=hnansec/10000000
      ih=ih8
      iu=iu8
      is=is8
      ic=ic8
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cvtsgregtugreg(ia,im,ij,ihs,ius,iss,ics,dtu,
     &                                   ih ,iu ,is ,ic,ip)
      implicit none
      include 'constante_def.inc'
      integer*4 ia,im,ij,ih,iu,is,ic,dtu,ip
      integer*4 ihs,ius,iss,ics,tuc,ipass
      real*8 ts,tu
      real*8 f_cvtstu
c
      ts=(ihs*360000+ius*6000+iss*100+ics)/8640000.d0
      tu=f_cvtstu(ia,im,ij,ts,ipass)
      if((ipass.eq.2).and.(ip.ne.0))tu=tu+cv_dts_dtu
      tuc=nint(tu*8640000.d0)-dtu
      ih=tuc/360000
      tuc=tuc-ih*360000
      iu=tuc/6000
      tuc=tuc-iu*6000
      is=tuc/100
      ic=tuc-is*100
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cvtugregtsgreg(ia,im,ij,ih ,iu ,is ,ic ,dtu,
     &                                   ihs,ius,iss,ics)
      implicit none
      include 'constante_def.inc'
      integer*4 ia,im,ij,ih,iu,is,ic,dtu
      integer*4 ihs,ius,iss,ics,tsc
      real*8 tu,ts
      real*8 f_cvtuts
c
      tu=(ih*360000+iu*6000+is*100+ic+dtu)/8640000.d0
      ts=f_cvtuts(ia,im,ij,tu)
      tsc=nint(ts*8640000.d0)
      ihs=tsc/360000
      tsc=tsc-ihs*360000
      ius=tsc/6000
      tsc=tsc-ius*6000
      iss=tsc/100
      ics=tsc-iss*100
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine gettugreg(ia,im,ij,ih,iu,is,ic)
c
c  retourne la date gregorienne et l'heure de la machine
c
	implicit none
	integer*4 value(8),ia,im,ij,ih,iu,is,ic
	character*12 bidon(3)
c
	call date_and_time(bidon(1),bidon(2),bidon(3),value)
	ia=value(1)
	im=value(2)
	ij=value(3)
	ih=value(5)
	iu=value(6)
	is=value(7)
	ic=value(8)/10
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine gettsgreg(ia,im,ij,ihs,ius,iss,ics,dtu)
c
c  retourne la date gregorienne et l'heure ts de la machine
c
	implicit none
	integer*4 ihs,ius,iss,ics,dtu
	integer*4 ia,im,ij,ih,iu,is,ic
c
	call gettugreg(ia,im,ij,ih,iu,is,ic)
        call cvtugregtsgreg(ia,im,ij,ih ,iu ,is ,ic ,dtu,
     &                               ihs,ius,iss,ics)
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer*4 function f_getdtu()
c
c  retourne en centieme de seconde le DTU.
c
	implicit none
	integer*4 dtu
	character*40 filename
c
	write(filename,11)
 11	format('/data/cordtu/dtucourant')
        open(unit=1,file=filename,status='old',err=100)
	read(1,21)dtu
 21	format(i4)
	close(unit=1)
        f_getdtu=dtu
        return
 100	continue
	write(*,101)filename
 101	format(' f_getdtu: err open fichier ',a)
        f_getdtu=0
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_cvd70rot(nansec)
        integer*8 nansec
        real*8 rot
c
	rot=dfloat(nansec)/86400.d9
        f_cvd70rot=rot
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer*8 function f_cvrotd70(rot)
        real*8 rot
c
        f_cvrotd70=rot*86400.d9
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine d70ascii(datbuf,d70)
c
c   conversion d'une date et d'une heure contenues dans D70 en une chaine
c   de 32 caracteres datbuf
c
	implicit none
	character*32 datbuf
	character*8 jour(0:7)
	character*3 mois(12)
        integer*8 d70
	integer*4 k,ia,im,ij,ih,iu,is,ic,js
	data jour/'        ','Lundi   ','Mardi   ','Mercredi',
     1		  'Jeudi   ','Vendredi','Samedi  ','Dimanche'/
	data mois/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1		  'OCT','NOV','DEC'/
c
        call cvd70greg(d70,ia,im,ij,ih,iu,is,ic,js)
	write(datbuf,1)jour(js),ij,mois(im),ia,ih,iu,is,ic
1	format(a,1x,i2,'-',a,'-',i4,1x,i2,':',i2.2,':',i2.2,'.',i2.2)
	return
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine d70asciibref(datbuf,d70)
c
c   conversion d'une date et d'une heure contenues dans D70 en une chaine
c   de 20 caracteres datbuf
c
	implicit none
	character*20 datbuf
        integer*8 d70
	integer*4 k,ia,im,ij,ih,iu,is,ic,js
c
        call cvd70greg(d70,ia,im,ij,ih,iu,is,ic,js)
	write(datbuf,1)ij,im,mod(ia,100),ih,iu,is,ic
1	format(i2,'/',i2.2,'/',i2.2,1x,i2,':',i2.2,':',i2.2,'.',i2.2)
	return
	end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer*8 function f_rndsd70(nansec)
        integer*8 nansec
c
        f_rndsd70=((nansec+500000000)/1000000000)*1000000000
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer*8 function f_rndmsd70(nansec)
        integer*8 nansec
c
        f_rndmsd70=((nansec+500000)/1000000)*1000000
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer*8 function f_rndcsd70(nansec)
        integer*8 nansec
c
        f_rndcsd70=((nansec+5000000)/10000000)*10000000
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer*8 function f_tumerid70(tudmand70,alpha,ahactuel)
c
c   retourne la date tu du meridien en 'd70' et l'angle horaire actuel
c   en minutes TS en fonction de:
c     -'tudmand70' date debut de manip en d70
c     -'alpha' ascension droite de la source en heure decimale TS
c
        implicit none
        include 'constante_def.inc'
        integer*4 ia,im,ij,ih,iu,is,ic,js,f_getdtu,ipass,iwd,icount
        integer*8 tudmand70,tu0d70,difftud70,f_cvgregd70,f_cvrotd70
	real*8 alpha,f_cvtstu,f_djl0,turot,dturot,tsrot,ahactuel,djlpm
c
	icount=0
        call cvd70greg(tudmand70,ia,im,ij,ih,iu,is,ic,js)
        djlpm=f_djl0(ia,im,ij)
        tsrot=alpha/24.d0
 100	continue
	icount=icount+1
	if(icount.ge.10)then
	   f_tumerid70=0
	   ahactuel=0.0
	   return
	endif
        call cvdjlgreg(djlpm,ia,im,ij,iwd)
	turot=f_cvtstu(ia,im,ij,tsrot,ipass)
c
c	write(*,1)ia,im,ij,tsrot,turot,ipass
c1       format('f_tumerid70>f_cvtstu> ia=',I4,' im=',I2,' ij=',I2,' tsrot=',F9.6,' -> turot=',F9.6,' ipass=',I2)
C	modif I.Cognard sep26,2000 le test .and.(ih.gt.12) est un peu brutal... voie plus bas
C       if((ipass.eq.2).and.(ih.gt.12))turot=turot+cv_dts_dtu
c
C	dturot=f_getdtu()/8640000.d0
        dturot=0.0
        turot=turot-dturot
        tu0d70=f_cvgregd70(ia,im,ij,0,0,0,0,0)
        f_tumerid70=tu0d70+f_cvrotd70(turot)
        difftud70=f_tumerid70-tudmand70
	if((ipass.eq.2).and.(abs(difftud70).ge.43200.d9))then
	   f_tumerid70=f_tumerid70+f_cvrotd70(cv_dts_dtu)
	endif
	difftud70=f_tumerid70-tudmand70
        if(difftud70.gt.+43200.d9)then
           djlpm=djlpm-1.d0
           go to 100
        endif
        if(difftud70.lt.-43200.d9)then
           djlpm=djlpm+1.d0
           go to 100
        endif
        ahactuel=(tudmand70-f_tumerid70)*cv_nanotu_minuts
        return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_alpha(tumerid70)
c
c   retourne l'alpha en heure TS decimale correspondant a une date tu
c   du meridien en 'd70'
c
        implicit none
        include 'constante_def.inc'
        integer*4 ia,im,ij,ih,iu,is,ic,js,dtu,f_getdtu
        integer*4 ihs,ius,iss,ics
        integer*8 tumerid70
c
        call cvd70greg(tumerid70,ia,im,ij,ih,iu,is,ic,js)
        dtu=f_getdtu()
        call cvtugregtsgreg(ia,im,ij,ih ,iu ,is ,ic ,dtu,
     &                               ihs,ius,iss,ics)
        f_alpha=ihs/1.d0 + ius/60.d0 + iss/3600.d0 + ics/360000.d0
        return
	end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer*8 function f_cvahd70(tumerid70,ah)
c
c   retourne la date en 'd70' correspondante a un angle horaire 'ah' en
c   minute TS et une date 'd70' du passage au meridien 'tumerid70'
c
        implicit none
        include 'constante_def.inc'
	real*8 ah
        integer*8 tumerid70,ahtud70
c
        ahtud70=ah*60.d9*cv_dts_dtu
        f_cvahd70=tumerid70+ahtud70
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cvd70djlex(datd70,datjulex)
c
c   conversion d'une date et d'une heure en D70 en une date
c   julienne exacte contenue dans datjulex
c
      implicit none
      integer*4 ia,im,ij,ih,iu,is,ic,js
      integer*8 datd70
      real*8 datjulex,f_djl0,tu

      call cvd70greg(datd70,ia,im,ij,ih,iu,is,ic,js)
      tu=ih/24.d0 + iu/1440.d0 + (is+ic*0.01d0)/86400.d0
      datjulex=f_djl0(ia,im,ij)+tu
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine rotasciir(DELAI,STRING)
c
c   conversion d'un delai en rotation dans DELAI en une chaine
c   de 11 caracteres STRING
c
	implicit none
	real*8 delai,rdelai
	integer*4 ih,im,is,ic
        integer*4 csec,sec
	character*(*) string
	integer*4 k
c
	rdelai=mod(delai,1.d0)
        if(rdelai.lt.0)rdelai=1.d0+rdelai
	csec=rdelai*8640000.d0
        ic=mod(csec,100)
        sec=csec/100
	ih=sec/3600
	sec=sec-ih*3600
	im=sec/60
	is=sec-im*60
	write(string,1)ih,im,is,ic
1	format(i2,':',i2.2,':',i2.2,'.',i2.2)
	return
	end















