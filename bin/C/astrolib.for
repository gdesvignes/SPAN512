cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cld(DATJUL,IY,IM,ID,IWD)
	IMPLICIT INTEGER*4 (I-N)
C
C****** NRAO **** GABRIEL *****
C
      double precision  dat000,datjul
	data dat000/1721425.5d0/
      dimension mntday(13)
      data mntday/0,31,59,90,120,151,181,212,243,273,304,334,365/
c compute the number of days passed since 00
      idays=datjul-dat000
      jdate=idays
c determine the number of 400, 100, 4 and 1 years passed since the
c first of january 0001. the remainder are the days of that year.
      iy=idays/146097*400
      irest=mod(idays,146097)
      iadd=irest/36524
c in case of the 31 dec of each 400 years i have problems with the
c fact that the 100 year period is one day shorter
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
c in the case of the 31 of dec in a leap year i would get the first of
c january of the next, since i check for leap years later. this
c if avoids that case
      if(iadd.ne.4) go to 10
      iy=iy+4
      idays=365
      go to 15
c in all other cases i obtain the final number of years and
c days in the next 2 statements. \he one is added for the start.
 10   iy=iy+iadd+1
      idays=mod(irest,365)
c check if the year is a leap year
 15   leap=0
      if(mod(iy,4).ne.0) go to 20
      leap=1
      if(mod(iy,100).eq.0.and.mod(iy,400).ne.0) leap=0
c in the case of a leap year i subtract 1 day, the months march
c through december are then computed like in an ordinary year
      idays=idays-leap
c compute month and day by comparing the number of days with the
c array of the number of days per month
 20   do 50 i50=2,13
      if (idays.ge.mntday(i50)) go to 50
c the one in case of the days is added for the subtracted first
c of january of the start
      id=idays-mntday(i50-1) +1
      im = i50-1
      if(leap.eq.0) go to 60
c correcting for the subtracted day in jan and feb in a leap year
      if(i50.gt.3) go to 60
      id = id + 1
      if (id.ne.32 ) go to 60
c with this procedure i could obtain a january 32
      id = 1
      im = 2
      go to 60
 50   continue
c calculation of the weekday
 60   iwd=mod(jdate,7)+1
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine dmap(DAT,DEQ,DELDAT,DEQRA,DEQDEC,DAPRA,DAPDEC)
C
C  NRAO  42/ 1 S (D)    DMAP       F4S       LCD=  45
C     DELDAT=EXTRAPOLATION INTERVAL FOR BESSELIAN DAY NUMBERS IN DAYS,
C     USUALLY = 1.0D0,
C     DAT=JULIAN DATE OF OBSERVATION, DEQ=MEAN EQUINOX GIVEN,
C     DEQRA,DEQDEC=MEAN RIGHT ASCENSION AND DECLINATION GIVEN (AT EQUINOX=DEQ),
C     DAPRA,DAPDEC= APPARENT RIGHT ASCENSION AND DECLINATION WANTED (AT DAT).
C
	double precision dred
      double precision deq,dapra,d1old,deqdec,dyqold,dyq,deqra,
     1d2old,dapdec,deqold     ,deldat,dprmat,dat,dyqra,deltim,dyqdec
      dimension dprmat(3,3),bdns1(5),bdns2(5),bdndel(5),scra(4),scdec(4)
      data d1old/0.0d0/,d2old/0.0d0/,deqold/0.0d0/,dyqold/0.0d0/
      call cld(dat,iy,im,id,iwd)
      dyq=iy
      if(im.ge.7) dyq=iy+1
      yq=dyq
      if(dyq.eq.dyqold.and.deq.eq.deqold) go to 10
      dyqold=dyq
      deqold=deq
      if(dyq.eq.deq) go to 20
      call dpre(deq,dyq,dprmat)
   10 if(dyq.eq.deq) go to 20
      call dtrc(dprmat,deqra,deqdec,dyqra,dyqdec)
      go to 30
   20 dyqra =deqra
      dyqdec=deqdec
   30 if(dat.ge.d1old.and.dat.le.d2old) go to 50
      d1old=dat
      d2old=dat+deldat
      call bdn2(d1old,yq,bdns1,psidel,spsi,seps)
      call bdn2(d2old,yq,bdns2,psidel,spsi,seps)
      do 40 k=1,5
   40 bdndel(k)=bdns2(k)-bdns1(k)
   50 call bsc(yq,sngl(dyqra),sngl(dyqdec),scra,scdec)
      deltim=(dat-d1old)/deldat
      dapra =dyqra+bdns1(5)+deltim*bdndel(5)
      dapdec=dyqdec
      do 60  k=1,4
      facsc=bdns1(k)+deltim*bdndel(k)
      dapra=dapra+facsc*scra(k)
   60 dapdec=dapdec+facsc*scdec(k)
      if(dabs(dapdec).le.1.570796326794897d0) go to  70
      dapra=dapra+3.141592653589793d0
   70 dapdec=dred (dapdec,2,3)
      dapra=dred (dapra,2,1)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine dpre(EQUIN0,EQUIN,PREMAT)
C
C  NRAO  16/ 1 S (D)    DPRE       F4S       LCD=  16
C
      double precision a,t,csar                       ,zett,equin,b,
     1t0,cthet,sthet,c,tcb,czeta,szeta,equin0,d,tsq,zeta,czett,
     2premat,thet,szett
      data                 csar/4.848136811095356d-06/
      dimension premat(3,3)
      t0=(equin0-1900.00d0)/100.00d0
      t=(equin-equin0)/100.00d0
      tsq=t*t
      tcb=tsq*t
      zeta=((2304.250d0+1.396d0*t0)*t+0.302d0*tsq+0.018d0*tcb)*csar
      zett=zeta + 0.791d0*tsq*csar
      thet=((2004.682d0-0.853d0*t0)*t-0.426d0*tsq-0.042*tcb)*csar
      szeta=dsin(zeta)
      czeta=dcos(zeta)
      szett=dsin(zett)
      czett=dcos(zett)
      sthet=dsin(thet)
      cthet=dcos(thet)
      a=szeta*szett
      b=czeta*szett
      c=szeta*czett
      d=czeta*czett
      premat(1,1)=d*cthet - a
      premat(1,2)=-c*cthet-b
      premat(1,3)=-sthet*czett
      premat(2,1)= b*cthet + c
      premat(2,2)=-a*cthet + d
      premat(2,3)=-sthet*szett
      premat(3,1)= czeta*sthet
      premat(3,2)=-szeta*sthet
      premat(3,3)= cthet
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine dtrc(TRCMAT,ALPH0,DEL0,ALPHA,DELTA)
C
C  NRAO  14/ 1 S (D)    DTRC       F4S       LCD=  14
C
      double precision del0,alpha     ,alph0,vec,delta,vec0,trcmat
      double precision                   pdist,cosdel
      dimension trcmat(3,3),vec0(3),vec(3)
      cosdel=dcos(del0)
      vec0(1) =dcos(alph0)*cosdel
      vec0(2) =dsin(alph0)*cosdel
      vec0(3) =dsin(del0)
      do 10 i=1,3,1
      vec(i)=0.0
      do 10 j=1,3,1
   10 vec(i)=trcmat(i,j)*vec0(j)+vec(i)
      if(vec(1).ne.0.0d0.or.vec(2).ne.0.0d0)  go to 100
      alpha=0.0d0
      pdist=0.0d0
      go to 20
  100 pdist=dsqrt(vec(1)*vec(1)+vec(2)*vec(2))
      alpha=datan2(vec(2),vec(1))
      if(alpha.lt.0.0d0)alpha=alpha+6.283185307179586d0
   20 delta=datan2(vec(3),pdist)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bdn2(DATJUL,EQUIN,DNS,DELPSI,SPSI,SEPS)
c
c
c
	dimension a(6)
      equivalence (a(1),a1),(a(2),a2),(a(3),a3),(a(4),a4),
     2(a(5),a5),(a(6),a6)
	double precision dred
      double precision a,a3,a7,a23,a44,sa3,tsq,a4,a24    ,a1,a2,
     1alpha,a5,a6,a21,a26,a25
      double precision datjul,djula,dsul,e,dece,t,tcb
      double precision csar
      data             csar/4.848136811095356d-06/
      dimension dns(5)
      data   ck3/0.0027379093/
      data div1/4.8481368e-6/
      dimension sinbbl(23),cosbbl(23),sinbbs(46),cosbbs(46),bbl(23),
     1 bbs(46)
      dimension tbil(9),cbil(9),cbis(3),tbis(3),tcil(5),ccil(5),
     1bil(23),bis(46),cil(16),cis(24)
      equivalence (bbl(1),bbl1),(bbl(2),bbl2),(bbl(3),bbl3)
      equivalence (bbl(4),bbl4),(bbl(5),bbl5),(bbl(6),bbl6)
      equivalence (bbl(7),bbl7),(bbl(8),bbl8),(bbl(9),bbl9)
      equivalence (bbl(10),bbl10),(bbl(11),bbl11),(bbl(12),bbl12)
      equivalence (bbl(13),bbl13),(bbl(14),bbl14),(bbl(15),bbl15)
	equivalence (bbl(16),bbl16),(bbl(17),bbl17),(bbl(18),bbl18)
	equivalence (bbl(19),bbl19),(bbl(20),bbl20),(bbl(21),bbl21)
	equivalence ( bbl(22),bbl22),(bbl(23),bbl23)
	equivalence( bbs(1),bbs1),(bbs(2),bbs2),(bbs(3),bbs3)
	equivalence (bbs(4),bbs4),(bbs(5),bbs5),(bbs(6),bbs6)
	equivalence(bbs(7),bbs7),(bbs(8),bbs8),(bbs(9),bbs9)
	equivalence(bbs(10),bbs10),(bbs(11),bbs11),(bbs(12),bbs12)
	equivalence( bbs(13),bbs13),(bbs(14),bbs14),(bbs(15),bbs15)
	equivalence(bbs(16),bbs16),(bbs(17),bbs17),(bbs(18),bbs18)
	equivalence(bbs(19),bbs19),(bbs(20),bbs20),(bbs(21),bbs21)
	equivalence (bbs(22),bbs22),(bbs(23),bbs23),(bbs(24),bbs24)
	equivalence (bbs(25),bbs25),(bbs(26),bbs26),(bbs(28),bbs28)
      equivalence (bbs(27),bbs27)
	equivalence (bbs(29),bbs29),(bbs(30),bbs30),(bbs(31),bbs31)
	equivalence( bbs(32),bbs32),(bbs(33),bbs33),(bbs(34),bbs34)
	equivalence(bbs(35),bbs35),(bbs(36),bbs36),(bbs(37),bbs37)
	equivalence(bbs(38),bbs38),(bbs(39),bbs39),(bbs(40),bbs40)
	equivalence( bbs(41),bbs41),(bbs(42),bbs42),(bbs(43),bbs43)
	equivalence(bbs(44),bbs44),(bbs(45),bbs45),(bbs(46),bbs46)
      data   tbil   /-17.2327,-1.2729,+0.2088,0.1261,-0.0497,0.0214,
     1 0.0124,0.0016,-0.0015/,cbil   /-0.01737,-0.00013,0.00002,
     2 -0.00031,0.00012,-0.00005,0.00001,-0.00001,0.00001/
     3 ,cbis   /-0.00002,0.00001,-0.00004/,tbis   /-0.2037,0.0675,
     4 -0.0342/,tcil   /9.2100,0.5522,-0.0904,0.0216,-0.0093/,ccil   /
     5 0.00091,-0.00029,0.00004,-0.00006,0.00003/,tcis1/0.0884/
      data tcis3/0.0113/,ccis1/-0.00005/,ccis3/-0.00001/
      data   bil    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0045,0.0045,-0.0021,
     1 0.0,0.0,-0.0015,-0.0010,0.0010,-0.0005,-0.0005,-0.0004,0.0004,
     2 0.0003,-0.0003,-0.0003,-0.0002/
	data bis/0.0,0.0,0.0,-0.0261,
     3 -0.0149,0.0114,0.0060,0.0058,-0.0057,-0.0052,-0.0044,-0.0032,
     4 0.0028,0.0026,-0.0026,0.0025,0.0019,0.0014,-0.0013,-0.0009,0.0007
     5 ,-0.0007,0.0006,2*-0.0006,0.0006,-0.0006,0.0005,2*-0.0005,-0.0004
     7 ,0.0004,2*-0.0004,0.0004,0.0003,2*-0.0003,2*-0.0002,0.0002,
     8 3*-0.0002,0.0002,-0.0002/
	data cil/5*0.0,-0.0066,-0.0024,
     9 0.0007,0.0008,0.0005,2*0.0003,0.0002,2*-0.0002,0.0002/
     9 ,cis    /0.0,0.0183,0.0,-0.0050,-0.0031,0.0030,0.0022,0.0023,
     1 0.0014,-0.0011,0.0011,-0.0010,-0.0007,0.0007,0.0005,-0.0003,
     2 0.0003,0.0003,-0.0002,0.0003,-0.0003,2*0.0003,0.0002/
c-----da13
c------
      t =(datjul-2415020.0d0)/36525.0d0
      tsq=t *t
      tcb=t *tsq
      a1=(1006908.04d0+129602768.13d0*t+1.089d0*tsq)*csar
      a2=(1012395.00d0+6189.03d0*t+1.63d0*tsq+0.012d0*tcb)*csar
      a3=(1290513.04d0+129596579.10d0*t-0.54d0*tsq-0.012d0*tcb)*csar
      a1=dred(a1,2,1)
      a2=dred(a2,2,1)
      a3=dred(a3,2,1)
c------
c----da46
c------
      a4=(973562.99d0+1732564379.31d0*t-4.08d0*tsq+0.0068d0*tcb)*csar
      a5=(933059.79d0-6962911.23d0*t+7.48d0*tsq+0.008d0*tcb)*csar
      a6=(1065976.59d0+1717915856.79d0*t+33.09d0*tsq+0.0518d0*tcb)*csar
      a4=dred(a4,2,1)
      a5=dred(a5,2,1)
      a6=dred(a6,2,1)
c------debut  nut2
      a21=2.0*  a1
      a23=2.0*  a3
      a24=2.0*  a4
      a25=2.0*  a5
      a26=2.0*  a6
      a44=4.0*  a4
      bbl1  =  a5
      bbl2  =a21
      bbl3  =a25
      bbl4  =  a3
      bbl5  =a21+  a3
      bbl6  =a21-  a3
      bbl7  =a21-  a5
      bbl8  =a24-a26-  a5
      bbl9  =a26+a21-a24
      bbl10  =a21-a25
      bbl11  =a23
      bbl12  =a21+a23
      bbl13  =  a3+  a5
      bbl14  =  a5-  a3
      bbl15  =  a5-bbl8
      bbl16  =  a5-bbl9
      bbl17  =a21-  a3-  a5
      bbl18  =bbl17  -  a3
      bbl19  =a26-a24+a21+  a5
      bbl20  =a21-bbl14
      bbl21  =a24-a26
      bbl22  =  a6-  a4+  a1
      bbl23  =bbl22  -  a3
      bbs1  =a24
      bbs2  =  a6
      bbs3  =a24-  a5
      bbs4  =  a6+a24
      bbs5  =  a6-a24+a21
      bbs6  =-  a6+a24
      bbs7  =a24-a21
      bbs8  =  a6+  a5
      bbs9  =  a5-  a6
      bbs10  =a44-  a6-a21
      bbs11  =  a6+a24-  a5
      bbs12  =a44-a21
      bbs13  =a26
      bbs14  =  a6+a21
      bbs15  =a26+a24
      bbs16  =a24-a25
      bbs17  =a24-  a6-  a5
      bbs18  =a24-  a6-a21+  a5
      bbs19  =  a6-a24+a21+  a5
      bbs20  =a44-  a6-a21-  a5
      bbs21  =  a3+a24
      bbs22  =  a6+  a3-a24+a21
      bbs23  =  a6+a24-a21
      bbs24  =a24-  a3
      bbs25  =a44-a21+  a6
      bbs26  =a21+a26
      bbs27  =a24-a21+  a5
      bbs28  =  a6-  a5+a21
      bbs29  =a21-a24+  a5
      bbs30  =a44-  a5-a21
      bbs31  =a24-  a5+a26
      bbs32  =  a6-  a3
      bbs33  =  a3-a24+a21
      bbs34  =  a4-  a1
      bbs35  =  a6-a24+a25
      bbs36  =  a6+a24-a25
      bbs37  =  a6+  a3
      bbs38  =  a6-  a3+a24
      bbs39  =  a5-a26
      bbs40  =a21-  a6-  a5
      bbs41  =a26+  a5
      bbs42  =a44-  a6-  a3-a21
      bbs43  =a44-  a3-a21
      bbs44  =  a6+a25
      bbs45  =  a6+  a3+a24
      bbs46  =3.0*  a6+a24
      do 10 i=1,7
   10 bil(i)=tbil(i)+cbil(i)*t
      do 20 i=11,12
   20 bil(i)=tbil(i-3)+cbil(i-3)*t
      do 30 i=1,3
   30 bis(i)=tbis(i)+cbis(i)*t
      do 40 i=1,5
   40 cil(i)=tcil(i)+ccil(i)*t
      cis(1)=tcis1+ccis1*t
      cis(3)=tcis3+ccis3*t
      do 50 i=1,23
   50 sinbbl(i)=sin(bbl(i))
      do 60 i=1,46
   60 sinbbs(i)=sin(bbs(i))
      do 70 i=1,3
   70 cosbbl(i)=cos(bbl(i))
      do 80 i=4,7
   80 cosbbl(i)=cos(bbl(i+1))
      do 90 i=8,10
   90 cosbbl(i)=cos(bbl(i+4))
      do 100 i=11,16
  100 cosbbl(i)=cos(bbl(i+5))
      cosbbs(1)=cos(bbs(1))
      do 110 i=2,3
  110 cosbbs(i)=cos(bbs(i+1))
      cosbbs(4)=cos(bbs(6))
      do 120 i=5,9
  120 cosbbs(i)=cos(bbs(i+3))
      cosbbs(10)=cos(bbs(14))
      cosbbs(11)=cos(bbs(15))
      do 130 i=12,16
  130 cosbbs(i)=cos(bbs(i+5))
      do 140 i=17,24
  140 cosbbs(i)=cos(bbs(i+7))
      spsi=0.0
      do 150 i=1,46
  150 spsi=spsi+bis(i)*sinbbs(i)
      delpsi=0.0
      do 160 i=1,23
  160 delpsi=delpsi+bil(i)*sinbbl(i)
      delpsi=delpsi+spsi
      seps=0.0
      do 170 i=1,24
  170 seps=seps+cis(i)*cosbbs(i)
      deleps=0.0
      do 180 i=1,16
  180 deleps=deleps+cil(i)*cosbbl(i)
      deleps=deleps+seps
      delpsi=div1*delpsi
      deleps=deleps*div1
      spsi=spsi*div1
      seps=seps*div1
c------
c------fin   nut2
c------
c----
      aber=9.936741e-5
      if(datjul.lt.2439856.5d0)   aber=9.924136e-5
      tt=(equin-1967.0)/100.0
      djula=(equin-1967.0)*365.242195d0+2439491.541d0
      tau=ck3*(datjul-djula)
      epslon=0.40931976-2.2711e-4*(datjul-2415020.0d0)/36525.0
      dns(1)=(9.7161995e-5-4.1209e-8*tt)*tau + sin(epslon)*delpsi
      dns(2)=-deleps
c----
c----------dsul
c-------
      e   =0.01675104d0-(0.00004180d0+0.000000126d0*t )*t
c----dece/decc    dsul=a1+dece(decc
      sa3=dsin(a3)
      dece=e*(2.0d0*sa3+e*1.25d0*dsin(2.0d0*a3)+e*e*(1.0833333d0*
     1 dsin(3.0d0*a3)-0.25d0*sa3))
      dsul=a1+dece
      dsul=dred(dsul,2,1)
      a7=dsul
c	dns(3)=(-aber)*cos(epslon)*dcos(a7)
	dns(3)=-dcos(a7)*cos(epslon)*aber
c	dns(4)=(-aber)*dsin(a7)
	dns(4)=sngl(dsin(a7)*(dble(-aber)) )
      dns(5)=(0.002226d0 - 0.000373d0*tt)*delpsi
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bsc(EQUIN,ALPHA,DELTA,SCRA,SCDEC)
C
C  NRAO  10/ 1 S (D)    BSC        F4S       LCD=  10
C
      dimension scra(4),scdec(4)
      t=(equin-1900.0)/100.0
      sindel=sin(delta)
      cosdel=cos(delta)
      secdel=1.0/cosdel
      sinalf=sin(alpha)
      cosalf=cos(alpha)
      scra(1)=2.29887+0.00237*t + sinalf*sindel*secdel
      scra(2)=cosalf*sindel*secdel
      scra(3)=cosalf*secdel
      scra(4)=sinalf*secdel
      scdec(1)=cosalf
      scdec(2)=-sinalf
      eps=0.40931975-0.00022711*t
      scdec(3)=tan(eps)*cosdel-sinalf*sindel
      scdec(4)=cosalf*sindel
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	double precision function dred(ANGLE,K,L)
C
C******* NRAO ****** GABRIEL *****
C
	dimension cu(4,2),cpi2(4)
	double precision angle,alpha,cu,x,cpi2
      data cu/1.0d0,6.283185307179586d0,24.0d0,360.0d0,0.5d0,
     1  3.141592653589793d0,12.0d0,180.0d0/ 
	data   cpi2/0.25d0,1.570796326794897d0,6.0d0,90.0d0 /
      alpha = angle
      n = idint(alpha/cu(k,1))
      alpha = alpha - n*cu(k,1)
      if(alpha.ge.0.0d0) go to 20
      alpha=alpha+cu(k,1)
      n=n-1
   20 if(l.gt.1) go to 40
      if(l.eq.(-1))   l=n
   30 dred = alpha
      return
   40 if(alpha.le.cu(k,2)) go to 50
      alpha = alpha - cu(k,1)
   50 if(l.eq.2) go to 30
      x = dabs(alpha)
      if(x.gt.cpi2(k))  x = cu(k,2) - x
      alpha = dsign(x,alpha)
      go to 30
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine coord (a0,b0,ap,bp,a1,b1,aa2,bb2)
	implicit none
	double precision sb0,cb0,sbp,cbp,sb1,cb1,sb2,cb2,saa,caa,cbb,
     2	sbb,ta202,b0,bp,b1,a1,ap,a0,bb2,aa2
	sb0=dsin(b0)
	cb0=dcos(b0)
	sbp=dsin(bp)
	cbp=dcos(bp)
	sb1=dsin(b1)
	cb1=dcos(b1)
c
	sb2=sbp*sb1+cbp*cb1*dcos(ap-a1)
	cb2=dsqrt( 1.0d0 - sb2*sb2)
	bb2=datan(sb2/cb2)
c	b2=darsin (sb2)
c modif modif
c  (note b0 is not needed to calculate b2)
c
	saa=dsin(ap-a1)*cb1/cb2
	caa=(sb1-sb2*sbp)/(cb2*cbp)
c
	cbb=sb0/cbp
	sbb=dsin(ap-a0)*cb0
c
	ta202=(1.0-caa*cbb-saa*sbb)/(saa*cbb-caa*sbb)
	aa2=2.0d0  *datan (ta202)
c
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine conv_equatoriales(L2,B2,ASD,DEC)
C
C   Pour convertir des coordonnees galactiques en equatoriales
C   L2= L2 en degres decimaux    <---
C   B2= B2 en degres decimaux    <---
C   ASD= ascension droite en heure decimales	--->
C   DEC= declinaison en degrees decimaux        --->
C
	implicit none
	include 'constante_def.inc'
	real*4 l2,b2,asd,dec
	double precision ri0,si0,ci0,rl2,sl2,cl2,rb2,sb2,cb2,sas,cas,sde
c
	ri0=62.6*cv_deg_rad
	si0=sin(ri0)
	ci0=cos(ri0)
c
	rl2=(l2-33.)*cv_deg_rad
	sl2=sin(rl2)
	cl2=cos(rl2)
c
	rb2=b2*cv_deg_rad
	sb2=sin(rb2)
	cb2=cos(rb2)
c
	sas=cb2*sl2*ci0-sb2*si0
	cas=cb2*cl2
	asd=((atan2(sas,cas)*cv_rad_deg)+282.25d0)/360.d0
	if(asd.lt.0.d0)asd=asd+1.d0
	if(asd.gt.1.d0)asd=asd-1.d0
        asd=asd*24.d0
c
	sde=ci0*sb2+si0*cb2*sl2
	dec=asin(sde)*cv_rad_deg
c
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine conv_galactiques(ASD,DEC,L2,B2)
C
C   Pour convertir des coordonnees equatoriales en galactiques
C   ASD= ascension droite en heure decimale   <---
C   DEC= declinaison en degrees decimaux        <---
C   L2= L2 en degres decimaux    --->
C   B2= B2 en degres decimaux    --->
C
	implicit none
	include 'constante_def.inc'
	real*4 asd,dec,l2,b2
	double precision ras,sas,cas,rde,sde,cde
	double precision rl2,sl2,cl2,rb2,sb2,cb2,ri0,si0,ci0
c
	ri0=62.6*cv_deg_rad
	si0=sin(ri0)
	ci0=cos(ri0)
c
	ras=(asd/24.d0-0.784027791d0)*cv_rot_rad
	sas=sin(ras)
	cas=cos(ras)
c
	rde=dec*cv_deg_rad
	sde=sin(rde)
	cde=cos(rde)
c
	sl2=si0*sde+ci0*cde*sas
	cl2=cde*cas
	rl2=atan2(sl2,cl2)
	l2=rl2*cv_rad_deg+33.d0
	if(l2.lt.0.d0)l2=l2+360.d0
c
	sb2=ci0*sde-si0*cde*sas
	b2=asin(sb2)*cv_rad_deg
c
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_vitsun(asdsun,decsun,asd,dec)
c
c   calcul de la vitesse soleil/lsr 'f_vitsun' en km/s projetee sur
c   la direction de visee en fonction de:
c   ASDSUN=ascension droite reduite du soleil (en heure)
c   DECSUN=declinaison du soleil reduite (en degre decimaux)
c   ASD=ascension droite reduite visee (en heure)
c   DEC=declinaison reduite visee (en degre decimaux)
c
	implicit none
	include 'constante_def.inc'
	real*8 asdsun,decsun,asd,dec
	real*8 vx0,vy0,vz0,rasdr,rdecr,rasun,rdsun
c
c
c
	rasun=asdsun/24.d0*cv_rot_rad
	rdsun=decsun*cv_deg_rad
	rasdr=asd/24.d0*cv_rot_rad
	rdecr=dec*cv_deg_rad
	vx0=20.d0*dcos(rasun)*dcos(rdsun)
	vy0=20.d0*dsin(rasun)*dcos(rdsun)
	vz0=20.d0*dsin(rdsun)
	f_vitsun=-vx0*dcos(rasdr)*dcos(rdecr)
     1           -vy0*dsin(rasdr)*dcos(rdecr)
     2           -vz0*dsin(rdecr)
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_vitbar(datjex,asd,dec)
c
c   DATJEX=date julienne exacte de l'instant ou l'on veut la vitesse
c          du barycentre terre-lune 
c   ASD=ascension droite visee (en heure)
c   DEC=declinaison visee (en degres decimaux)
c   f_vitbar=vitesse du barycentre terre-lune projetee sur la direction de
c        visee    f_vitbar=v+vmon (en km/s)
C
	implicit none
	include 'constante_def.inc'
	real*8 asd,dec
	real*8 datjex
	real*8 asdr,decr,smd,t,t2,t3,am,pir,pu,e,e2,e3
	real*8 ai,vs,v,xlam,alam,along,beta,hop,chop,omgar
	real*8 amonr,gampr,pim,olamm,amm,vsm,em,em2,em3,alamm
	real*8 pid,aim,algm,betam,hopm,chopm,vmon
c
	data pir/0.01745329251994506d0/
	data pid/0.1570796326795055d1/
	data chop/0.2573417731464959d7/
	data chopm/0.88401508171266581d5/
	data em/0.054900489d0/
	data em2/0.3014063692439121d-2/
	data em3/0.16547357059205334d-3/
	data aim/0.89804108500272217d-1/
c
c
c
	asdr=asd/24.d0*cv_rot_rad
	decr=dec*cv_deg_rad
	smd=datjex-2415020.d0
	t=smd/36525.d0
	t2=t*t
	t3=t2*t
	am=(358.47583d0+0.985600267d0*smd-1.5d-4*t2-0.3d-5*t3)*pir
	pu=(101.22083d0+4.70684000d-5*smd+4.53d-4*t2+0.3d-5*t3)*pir
	e=0.01675104d0-4.18d-5*t-1.26d-7*t2
	e2=e*e
	e3=e2*e
	ai=(23.452294d0-1.30125d-2*t-1.64d-6*t2+5.03d-7*t3)*pir
	vs=am+(2.d0*e-0.25d0*e3)*dsin(am)+1.25d0*e2*dsin(2.d0*am)+
     1	13.d0/12.d0*e3*dsin(3.d0*am)
	xlam=pu+vs
	alam=xlam+pi
	call coord(0.d0,0.d0,-pid,pid-ai,asdr,decr,along,beta)
	hop=chop/dsqrt(1.d0-e2)/86400.d0
	v=-hop*dcos(beta)*(dsin(alam-along)-e*dsin(pu-along))
	omgar=(2.59183275d2-5.29539222d-2*smd+2.078d-3*t2+2.d-6*t3)*pir
	amonr=(2.70434164d2+1.3176396527d1*smd-1.133d-3*t2+1.9d-6*t3)
     1	*pir
	gampr=(3.34329556d2+1.114040803d-1*smd-1.0325d-2*t2-1.2d-5*t3)
     1	*pir
	pim=gampr-omgar
	olamm=amonr-omgar
	amm=olamm-pim
	vsm=amm+(2.d0*em-0.25d0*em3)*dsin(amm)+1.25d0*em2*dsin(2.d0*amm)
     1	+1.08333333d0*em3*dsin(3.d0*amm)
	alamm=pim+vsm
	call coord(omgar,0.d0,omgar-pid,pid-aim,along,beta,algm,betam)
	hopm=chopm/dsqrt(1.d0-em2)/86400.d0
	vmon=-hopm/81.3d0*dcos(betam)*(dsin(alamm-algm)-em*dsin(pim-
     1	algm))
	f_vitbar=v+vmon
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_vitspn(ah,dec)
c
c   AH = angle horaire en minute
c   DEC = declinaison de la direction de visee en degre
c   f_vitspin = projection sur la direction de visee du spin de la terre en km/s
c   cat=latitude geocentrique de nancay en radian
c   vrho=rayon vecteur multiplie par vitesse angulaire de la terre
c   w=vrho*cos(cat)=constante pour nancay
c
	implicit none
	include 'constante_def.inc'
	real*8 ah,dec
	real*8 w
c
c
	data w/0.31556992d0/
c
c   nouvelle valeur donnee par F. Biraud le 11/12/95 pour w
c	data w/0.31557d0/
c
	f_vitspn=w*dcos(dec*cv_deg_rad)*dsin(ah/1440.d0*cv_rot_rad)
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_vitobsref(codvit,datjex,asdsun,decsun,asd,dec,ah)
c
c   Calcule la vitesse (projettee sur la direction de visee) de l'observateur
c   par rapport a la reference 'codvit' donnee par l'utilisateur
c   en fonction de:
c   de la date julienne exacte de l'instant 'datjex'
c   des coordonnees reduites du soleil 'asdsun' et 'decsun' (heure et deg.)
c   de la direction de visee 'asd' (heure) et 'dec' (degres decimaux)
c   de l'angle horaire 'ah' (minute TS)
c
	implicit none
	integer*4 codvit,igo
	real*8 datjex,asdsun,decsun,asd,dec,ah,vsun,vbar,vspn,c
        real*8 f_vitsun,f_vitbar,f_vitspn
	data c/299792.4561d0/
c
	f_vitobsref=0.d0
	igo=codvit+1
	go to(10,20,30,20),igo
	return
c
10	continue
	vsun=f_vitsun(asdsun,decsun,asd,dec)
        vbar=f_vitbar(datjex,asd,dec)
	vspn=f_vitspn(ah,dec)
	f_vitobsref=vsun+vbar+vspn
	return
c
20	continue
        vbar=f_vitbar(datjex,asd,dec)
	vspn=f_vitspn(ah,dec)
	f_vitobsref=vbar+vspn
	return
c
30	continue
	vspn=f_vitspn(ah,dec)
	f_vitobsref=vspn
	return
c
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine vitesse_doppler(codvit,datjex,asdsun,decsun,asd,dec,ah,vits,
     1	                           vitd,dopl)
c
c   Calcule la vitesse Doppler en fonction du code de reference 'codvit'
c   de la date julienne exacte contenue dans 'datjex'
c   des coordonnees reduites du soleil 'asdsun' et 'decsun'
c   de la direction de visee 'asd' (heures decimales) et 'dec' (degres decimaux)
c   de l'angle horaire 'ah' (rotations)
c   des 4 vitesses donnees 'vits' (km/s)
c   'vitd' sont les 4 vitesses doppler calculees (en km/s)
c   'dopl' sont les 4 doppler (1-V/C)
c
	implicit none
	integer*4 codvit,igo,i
	real*8 datjex,vsun,vbar,vspn,c
        real*8 f_vitsun,f_vitbar,f_vitspn
        real*4 asdsun,decsun,asd,dec,ah,vits(4),vitd(4),dopl(4)
	data c/299792.4561d0/
c
	igo=codvit+1
	go to(10,20,30,40,50,60),igo
	return
c
10	continue
	vsun=f_vitsun(asdsun,decsun,asd,dec)
        vbar=f_vitbar(datjex,asd,dec)
	vspn=f_vitspn(ah,dec)
	do i=1,4
	vitd(i)=vsun+vbar+vspn+vits(i)
	enddo
	go to 100
c
20	continue
        vbar=f_vitbar(datjex,asd,dec)
	vspn=f_vitspn(ah,dec)
	do i=1,4
	vitd(i)=vbar+vspn+vits(i)
	enddo
	go to 100
c
30	continue
	vspn=f_vitspn(ah,dec)
	do i=1,4
	vitd(i)=vspn+vits(i)
	enddo
	go to 100
c
40	continue
        vbar=f_vitbar(datjex,asd,dec)
	vspn=f_vitspn(ah,dec)
	do i=1,4
        vitd(i)=vbar+vspn+(c*(1.d0-1.d0/(vits(i)/c+1.d0)))
	enddo
	go to 100
c
50	continue
	do i=1,4
        vitd(i)=vits(i)
	enddo
	go to 100
c
60	continue
	do i=1,4
	vitd(i)=0.0
        enddo
	go to 100
c
100	continue
	do i=1,4
	dopl(i)=1.d0-vitd(i)/c
	enddo
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc









