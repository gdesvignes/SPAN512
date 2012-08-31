	subroutine calpou(ier,stopcode,ia,im,ij,ih,iu,is,ic,xw,zw,yw,rw,xcw,ycw,incw,tural,tupou,tutot,rtsdman,rtsfpou)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Calcule la poursuite complete a partir des parametres contenus dans
c   COMSOUMAP. Les coordonnees de la sources doivent etre reduites
c   au jour avant l'appel de ce programme.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	implicit none
	include 'constante_def.inc'
        include 'compou.inc'
	include 'comsoumap.inc'
	include 'comcalper.inc'
        include 'position.inc'
	real*8 durp(10),vasd(10),dasd(10),vdec(10),ddec(10),drot(10)
	real*8 dural,dapou,ddpou,dafcyc,ddfcyc,drfcyc
	real*8 zmin,zmax,zcal
	real*8 ddelta,difz,tral,coefcv
	real*8 f_ahc,colat
	real*8 f_tpos,f_zcodeur,f_zlocal
	real*8 ahdsec,alphadsec,deltadsec,dadsec,dddsec,xdsec,zdsec,ydsec,
     1	  xcdsec,ycdsec,rdsec,drdsec,dydsec,dxcdsec,dycdsec,incdsec,zminsec,
     2    zmaxsec,tscourant
	real*8 ahfcyc,alphafcyc,deltafcyc,xfcyc,zfcyc,yfcyc,
     &	       xcfcyc,ycfcyc,rfcyc,dyfcyc,incfcyc
	integer*2 niter,nitermax
	integer*4 nphasp,nph,numcyc,numpha,numcycsec,numpersec
	integer*4 kodral,ier,stopcode,numperfcyc,koderr
        integer*4 ia,im,ij,ih,iu,is,ic,js,ihs,ius,iss,ics
        integer*8 temponano,f_tumerid70,f_cvrotd70,f_cvgregd70,f_rndsd70
        integer*8 tufpoud70cyc,tufpoud70sec
        real*8    f_ts0,f_djl0,f_cvd70rot,dureepremper
        real*8    f_ahdeptheo,f_ahfintheo,dah,f_alpha
	logical section_valable,z_out,f_depassement_z,fin_de_manip
	logical passage_superieur,offpx,f_recalzdep
        character*2 litpou(7)
        data litpou/'PC','PE','PX','DR','L3','L5','NS'/
        integer*4 ipass
        real*8 ts,tu,f_cvtstu
        real*8 xw,zw,yw,rw,xcw,ycw,incw
        real*8 tural,tupou,tutot,rtsdman,rtsfpou

        write(*,*)'calpou> Start...'
        write(*,*)'calpou> Date/HeureTS ia,im,ij,ih,iu,is,ic= ',ia,im,ij,ih,iu,is,ic
        ts=ih/24.0+iu/1440.0+is/86400.0+ic/86400000.0
        tu=f_cvtstu(ia,im,ij,ts,ipass)
        ih=tu*24.0
        iu=(tu-ih/24.)*1440.0
        is=(tu-ih/24.-iu/1440.0)*86400.0
        write(*,*)'calpou> Conversion en HeureTU ',ih,iu,is 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   'nitermax' est le nombre d'iterations maximum permises sur le calcul
c   d'une section pour ramener le 'Z' minimum de la section a 'zlimb'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	nitermax=10
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   conversion des arguments d'entree en unites utilisee pour le calcul de
c   la poursuite
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	alpha=sou%redjou_asd    	! heures decimales TS

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Si depart immediat on transforme l'alpha
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(imm)then
           call cvtugregtsgreg(ia,im,ij,ih,iu,is,ic,0,ihs,ius,iss,ics)
           tscourant=ihs+(ius/60.)+(iss/3600.)+(ics/360000.)
           alpha=tscourant+0.5
           if(alpha.ge.24.0)alpha=alpha-24.0
           write(*,8)sou%redjou_asd,alpha
 8	   format(' Alpha modifie pour depart immediat: ancien=',f7.1,'   nouveau=',f7.1)
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   correction de l'alpha si on calcul une poursuite drift
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        
	if(map%pou%typou.ge.4.and.map%pou%typou.le.6)then
	   alpha=alpha-((map%pou%phase(1)%duree/2.d0)/3600.d0)
	   if(alpha.lt.0.0d0)alpha=alpha+24.d0
	   if(alpha.gt.24.d0)alpha=alpha-24.d0
	endif

	delta=sou%redjou_dec		! degres decimaux

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   correction du delta si on calcul une poursuite lobe
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        write(*,*)'calpou> Type de poursuite= ',litpou(map%pou%typou)
	if(map%pou%typou .eq. 5 .or. map%pou%typou .eq. 6)then
		ddelta=map%pou%phase(1)%ddec/3600.d0
		if(map%pou%typou .eq. 5)then
			delta=delta-ddelta
		else
			delta=delta-2.d0*ddelta
		endif
	endif
	mvpa=sou%mvtpro_asd     	! sec. alpha/sec. -> sec./sec.
	mvpd=sou%mvtpro_dec     	! sec. d'arc/sec. -> sec. d'arc/sec.
        mvpr=sou%mvtpro_rot/60.d0       ! degre/heure -> degre/minute
        nphasp=map%pou%nphase           !nombre de phases de poursuite
	durcycacq=0.d0                  !init duree de cycle acquisition


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si les 'dasd' sont en secondes d'arc (udasd=1) il faut calculer un
c   coefficient de conversion qui est 15/cos(delta)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        coefcv=1.d0
        if(map%pou%udasd.eq.1)coefcv=15.d0*cos(delta*cv_deg_rad)

	do nph=1,nphasp
 	 durp(nph)=map%pou%phase(nph)%duree         ! secondes -> secondes
         durcycacq=durcycacq+durp(nph)/60.d0        ! en minutes TS
	 vasd(nph)=map%pou%phase(nph)%vasd	    ! sec/sec -> heure/heure
	 dasd(nph)=map%pou%phase(nph)%dasd/3600.d0  ! secondes -> heure
         dasd(nph)=dasd(nph)/coefcv
	 vdec(nph)=map%pou%phase(nph)%vdec          ! arcsec/sec. -> deg/heure
	 ddec(nph)=map%pou%phase(nph)%ddec/3600.d0  ! arcsec -> deg.
	 drot(nph)=map%pou%phase(nph)%dr/10.d0	    ! 1/10 degres  -> degres
	enddo
	fin_de_manip=.false.
	passage_superieur=.false.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   on lit le temps universel actuel, on lui ajoute une temporisation
c   pour tenir compte du temps de calcul et de transfert des periodes aux
c   cibles, ce qui donne 'tudmand70' que l'on arrondi a la seconde superieure.
c   'tudmand70' est pris comme base pour calculer 'tumerid70' qui est
c   la date TU (en d70) du passage au meridien. Ce temps correspond donc
c   a l'angle horaire 0 minute.
c   modif du 30 mai 2007
c   la temporisation du depart est forfaitairement a 20 secondes pour toutes
c   les manips, sauf pour les manips 'rdh' ou cette constante est a 230 secondes
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        tudmand70=f_cvgregd70(ia,im,ij,ih,iu,is,ic,0)   !converti en D70
	temponano=0                            !20s TU en nanos
C	temponano=20000000000                            !20s TU en nanos
C	if(rdh)temponano=230000000000
        tudmand70=f_rndsd70(tudmand70+temponano)         !arrondi a la seconde
        tumerid70=f_tumerid70(tudmand70,alpha,ahdman)
C        write(*,*)'calpou> RA heure decimale TS = ', alpha
c       Ajoute par I.Cognard pour prevenir futurs bugs dans f_tumerid70
        if((tumerid70.eq.0).and.(ahdman.eq.0.0))then
          ier=8
          goto 1000
        endif
        ahdtheo=f_ahdeptheo(delta)
        ahftheo=f_ahfintheo(delta)
        write(*,9)ahdtheo,ahdman,ahftheo
 9	format('calpou> AH debut theorique=',f7.1,'       AH actuel=',f7.1,
     &  '       AH fin theorique=',f7.1)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si l'angle horaire actuel est superieur a 360 minutes et que la source
c   a une declinaison superieure a (90°-latitude) on prend le passage
c   superieur.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        colat=90.d0-(latitude_nancay*cv_rad_deg)
        if((abs(ahdman).gt.360.d0).and.(delta.gt.colat))then
		write(*,*)' --- On tente le passage superieur ---'
		passage_superieur=.true.
	endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   on initialise les parametres de poursuite
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	call init_parampou(passage_superieur,koderr)
        if(koderr.lt.0)then
           ier=10
           go to 1000
        endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   recherche des positions actuelles XC, ZC, YC, XCC, YCC, RC, INCC
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	call get_positions(xc,zc,yc,rc,xcc,ycc,incc)
        xc=xw
        zc=zw
        yc=yw
        rc=rw
        xcc=xcw
        ycc=ycw
        incc=incw
        write(*,*)'calpou> get_positions> xc zc yc rc= ',xc,zc,yc,rc
        write(*,*)'calpou> get_positions> xcc ycc incc= ',xcc,ycc,incc
        xcou=xc
        zcou=zc
        ycou=yc
        xccou=xcc
        yccou=ycc
        icoumin=incc
        icoumax=incc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Si l'angle horaire de depart actuel est plus petit que l'angle horaire
c   de depart theorique, on calcule la duree minimum de la premiere periode
c   de ralliement.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        dureepremper=0.d0
        if(ahlimdeb.gt.ahdman)dureepremper=1.0d0+(ahlimdeb-ahdman)*60.d0
c        write(*,12)ahdman,ahlimdeb,ahlimfin,dureepremper
c 12	format(' AH actuel=',f7.1,' AHD limite=',f7.1,
c     &  ' AHF limite=',f7.1,'    duree premiere periode=',f7.1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   on initialise les positions courantes
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	tufpoud70cyc=0          !tu fin de poursuite
	ahc=ahdman		!ah courant en minutes TS
        dah=mvpa*ahc            !dah en minutes TS
        if((ahc+dah).lt.ahdman)ahc=ahc-dah
 	alphac=alpha+dah/60.d0  !alpha courant en heures TS
	deltac=delta+(mvpd*ahc*60.d0)/3600.d0 !delta courant en degres
 	dafcyc=0.d0		!init. du DA de fin de cycle
	ddfcyc=0.d0		!init. du DD de fin de cycle
	drfcyc=0.d0		!init. du DR de fin de cycle
	nbrsectot=1		!numero de section
	numcyc=1		!numero de cycle
	numper=1		!numero de periode
	nbrper=0		!nombre total de periode
        dr=0                    !premiere variation de R
	dy=-vary/2.d0		!premiere variation du Y
	dxc=xcc-ixcornet	!variation de numero de cornet
	dyc=-varyc/2.d0		!premiere variation du Yc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   debut calcul d'une nouvelle section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
100	continue
	niter=0			!nombre d'iteration sur la section
	ahdsec=ahc		!AH au debut de la section
        tufpoud70sec=tufpoud70cyc
	alphadsec=alphac        !alpha vise au debut de la section
	deltadsec=deltac	!delta vise au debut de la section
	dadsec=dafcyc           !da au debut de la section
	dddsec=ddfcyc           !dd au debut de la section
        drdsec=drfcyc           !dr au debut de la section
	dydsec=dy		!variation de Y en debut de section
	dxcdsec=dxc		!variation en Xc en debut de section
	dycdsec=dyc		!variation en Yc en debut de section
	xdsec=xc		!X en debut de section
	zdsec=zc		!Z en debut de section
	ydsec=yc		!Y en debut de section
        xcdsec=xcc		!Xc en debut de section
	ycdsec=ycc		!Yc en debut de section
	rdsec=rc		!R en debut de section
	incdsec=incc		!I en debut de section
	numcycsec=numcyc	!numero du premier cycle de la section
	numpersec=numper	!numero de la premiere periode de la section
	zcal=0.d0		!Z de depart calcule de la section

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   debut iteration sur une section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
200	continue
	niter=niter+1		!incremente nombre d'iteration sur la section
	if(niter.gt.nitermax)then
		ier=8
		go to 1000
		endif
	kodral=0		!code pour la periode de ralliement:
					! 0=premier ralliement debut section
					! 1=ralliement suivant dans la section
					! 3=ralliement apres phase 1 dans PX
c
	write(*,201)nbrsectot,niter
201	format(/' Section ',i2,' Iteration ',i2/
     &          ' -----------------------')
c
	fin_de_manip=.false.
	section_valable=.false.	!section d'au moins 1 cycle
	ahc=ahdsec		!init. angle horaire courant
        tufpoud70=tufpoud70sec  !init. tu fin de poursuite
        alphac=alphadsec        !init. alpha courant
	deltac=deltadsec	!init. delta courant
	dafcyc=dadsec           !init. da courant
	ddfcyc=dddsec           !init. dd courant
	drfcyc=drdsec           !init. dr courant
	xc=xdsec		!init. X courant
	zc=zdsec		!init. Z courant
	yc=ydsec		!init. Y courant
	xcc=xcdsec		!init. Xc courant
	ycc=ycdsec		!init. Yc courant
	rc=rdsec		!init. R courant
        dr=drdsec               !init. dr courant
	dy=dydsec               !init. dy courant
	incc=incdsec		!init. Inclinaison courante
	dxc=dxcdsec		!init. dxc courant
	dyc=dycdsec		!init. dyc courant
	numcyc=numcycsec	!init. numero de cycle courant
	numper=numpersec	!init. numero de periode courant
	zmin=9.d6		!init. du Z minimum
	zmax=-zmin		!init. du Z maximum
	zminsec=zmin
	zmaxsec=zmax

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul d'un cycle de la section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
300	continue
	da=dafcyc
	dd=ddfcyc
	dr=drfcyc
        offpx=.false.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   boucle sur les phases du cycle
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	do nph=1,nphasp

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul de la periode de ralliement
c   La duree de cette periode est le temps de positionnement
c   du telescope aux positions de depart de la phase de tracking.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	duree=0.d0
        if(numper.eq.1)duree=dureepremper
	zcal=zcal+1d-2
C	write(*,301)numper,duree,kodral,zcal
301	format(/' rallie:periode=',i3,' duree=',f7.3,' kodral=',i2,
     &          ' zcal=',f10.3)
	call periode_ralliement(offpx,kodral,zcal,ier)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   analyse des erreurs pendant le calcul de la periode de ralliement
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(ier.ne.0)then
c           write(*,*)' err=',ier,' Periode ral. no',numper
           go to 400
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si on a un depassement en Z pendant la periode de ralliement on sort
c   de la boucle sur les phases pour traiter la fin de section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	z_out=f_depassement_z(za,zmin,zmax)
	if(z_out)then
           write(*,*)' Periode ral. no',numper,' DEPASSEMENT EN Z=',za
           go to 500
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si c'est la premiere periode de la manip, on annule le DXc
c   puisque le changement de cornet ne se fait qu'au debut de la manip
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(numper.eq.1)dxc=0.d0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   on sauve le 'da' calcule et la periode de ralliement
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	dural=duree
	call range_periode(nbrsectot,numcyc,nph)
	numper=numper+1
	if(numper.gt.nbrpermax)then
	   ier=6
	   go to 400
	endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul de la periode de tracking definie par l'utilisateur
c   Cette periode ne depend que de sa duree, de la vitesse en ascension
c   droite et en declinaison
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	va=vasd(nph)
	vd=vdec(nph)
	duree=durp(nph)
C	write(*,302)numper,duree,kodral,zcal
302	format(/' track:periode=',i3,' duree=',f9.3,' kodral=',i2,
     &          ' zcal=',f10.3)
C        write(*,*)'calpou> va= ',va,' vd= ',vd,' duree= ',duree,' offpx=',offpx
	call periode_tracking(offpx,ier)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   analyse des erreurs pendant le calcul de la periode de tracking
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(ier.ne.0)then
c           write(*,*)' err=',ier,' Periode track. no',numper
           go to 400
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si on a un depassement en Z pendant la periode de poursuite on sort
c   de la boucle sur les phases pour traiter la fin de section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	z_out=f_depassement_z(za,zmin,zmax)
	if(z_out)then
           write(*,*)' Periode track no',numper,' DEPASSEMENT EN Z=',za
           go to 500
        endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   sauvetage du 'da' calcule et de cette periode de tracking
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	dapou=da
	ddpou=dd
	call range_periode(nbrsectot,numcyc,nph)
	numper=numper+1
	if(numper.gt.nbrpermax)then
	   ier=6
	   go to 400
	endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du 'da' pour la phase de ralliement suivante
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	kodral=1
	if(map%pou%typou.eq.3)then
           if(nph.eq.1)then
	      kodral=3
	      da=durp(1)/3600.d0   !le 'da' est en heure TS
              offpx=.true.
	   else
	      da=-(durp(2)+dural)/3600.d0
	   endif
	else
	   da=-dapou+dasd(nph)
	   dd=-ddpou+ddec(nph)
	endif
	dr=dr+drot(nph)
        enddo          !fin de boucle sur les phases du cycle

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   fin d'un cycle complet. Sauvetage des positions a la fin de ce cycle.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	section_valable=.true.
	numperfcyc=numper
	nbrper=numper-1		!mise a jour nombre total periode
	ahfcyc=ahc		!ah vise a la fin du cycle
        tufpoud70cyc=tufpoud70
        alphafcyc=alphac        !alpha vise a la fin du cycle
	deltafcyc=deltac	!delta vise a la fin du cycle
	dafcyc=da		!da a la fin du cycle
	ddfcyc=dd		!dd a la fin du cycle
	drfcyc=dr		!dr a la fin du cycle
	xfcyc=xc		!X en fin du cycle
	zfcyc=zc		!Z en fin du cycle
	yfcyc=yc		!Y en fin du cycle
	rfcyc=rc		!R en fin de cycle
	incfcyc=incc		!I en fin du cycle
	zminsec=min(zmin,zminsec)	!remise a jour Zmin fin de section
	zmaxsec=max(zmax,zmaxsec)	!remise a jour Zmax fin de section
	numcyc=numcyc+1			!incremente le numero de cycle
	if(numcyc .gt. map%par%ncy)then	!depasse t-on le nombre de cycles lus
		ier=7
		go to 400
		endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul des variations en Y et Yc du cycle suivant en fonction de sa parite
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(mod(numcyc,2).eq.0)then
		dy=vary/2.d0
		dyc=varyc/2.d0
		else
		dy=-vary/2.d0
		dyc=-varyc/2.d0
	endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   on passe au calcul du cycle suivant
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	go to 300

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   on vient ici en cas d'erreur detectee au cours du calcul d'une periode
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
400	continue
	fin_de_manip=.true.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   on vient ici en cas de fin de section (limite Z atteinte) ou de manip
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
500	continue
	if(.not.section_valable)go to 580

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   traitement d'une fin de section (si le dernier cycle de cette section est
c   incomplet on n'en tient pas compte). Une section est valable si elle a
c   au moins un cycle.
c
c	   si la section est valable, on regarde si le z est bien compris
c          entre les limites zlimb et zlimh et si le zmin est le plus petit
c	   possible. Si ce n'est pas le cas, on recale 'zcal' et on reitere
c	   le calcul de la section.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	zmin=zminsec
	zmax=zmaxsec
c
c        write(*,501)incfcyc+offseti,f_zcodeur(zminsec),f_zcodeur(zmaxsec)
c 501	format(' I=',f6.0,' zmin=',f6.0,' zmax=',f6.0)
c
	if(.not.f_recalzdep(zmin,zmax,zcal,niter))then
c
c           write(*,502)zcal
c 502	   format(' zcal est pas bon on reitere avec la valeur',f7.1)
c
           go to 200
        endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   le z est bon on passe a la section suivante (en remettant
c   a jour les parametres au dernier cycle complet) ou si le
c   flag 'fin_de manip' est positionne on va a la fin de la poursuite
c   Le flag 'fin_de_manip' est positionne si DMI est .false.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(fin_de_manip)go to 1000
        if(.not.dmi)then
           ier=9
           fin_de_manip=.true.
        endif
	if(fin_de_manip)go to 1000
	ahc=ahfcyc
        tufpoud70=tufpoud70cyc
        alphac=alphafcyc
	deltac=deltafcyc
	dadsec=dafcyc
	dddsec=ddfcyc
	drdsec=drfcyc
	xc=xfcyc
	zc=zfcyc
	yc=yfcyc
	rc=rfcyc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul de la variation en Y du premier cycle de la section suivante
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(mod(numcyc,2).eq.0)then
		dy=-vary/2.d0-yc
		else
		dy=vary/2.d0-yc
		endif
	incc=incfcyc
	numper=numperfcyc
	nbrsectot=nbrsectot+1			!mise a jour numero de section

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   On passe a la section suivante
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	go to 100

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   la section n'est pas valable, on regarde si c'est a cause d'un
c   depassement en Z. Si c'est le cas, on regarde si ce depassement a eu
c   lieu avant le meridien. Dans ce cas on reitere sur la meme section
c   en changeant l'angle horaire de depart de la section.
c   Si le depassement a eu lieu apres le meridien, la source est consideree
c   comme sorti du champ du radiotelescope et la manip est terminee.
c   Dans ce cas il faut diminuer de 1 le nombre de section
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
580	continue
	if(z_out)then
		if(ahc.gt.0.d0)then
		   ier=5
		   nbrsectot=nbrsectot-1
		   go to 1000
		endif
		ahdsec=ahdsec+1.0*niter      !ajoute 60 sec. a l'angle horaire
		zcal=0.d0
		go to 200		  !reitere sur la meme section
	endif
        nbrsectot=nbrsectot-1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   le calcul de la poursuite est termine
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
1000	continue
        nbrcyctot=numcyc-1
	write(*,1001)nbrcyctot,nbrsectot
1001	format(//5x,' Calcul de la poursuite de',i4,' cycle(s) sur ',i2,' section(s) termine par:')
        stopcode=ier
	go to(1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)ier
c
	write(*,1002)ier
1002	format(10x,'-code de fin inconnu=',i4/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1100	write(*,1101)
1101	format(10x,'-sortie du champ de l''objet observe'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1200	write(*,1201)
1201	format(10x,'-AH courant plus petit que AH limite de debut'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1300	write(*,1301)
1301	format(10x,'-Angle Horaire courant plus grand que Angle Horaire fin'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1400	write(*,1401)
1401	format(10x,'-depassement des limites d''inclinaison du miroir'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1500	write(*,1501)
1501	format(10x,'-depassement en Z apres le meridien dans une section de moins d''un cycle'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1600	write(*,1601)
1601	format(10x,'-depassement du nombre maximum de periode'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1700	write(*,1701)
1701	format(10x,'-depassement du nombre de cycle impose'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
1800	write(*,1801)nitermax,nbrsectot
1801	format(10x,'-nombre limite d''iteration (',i2,') sur la section ',i2/)
	go to 9999
c
1900	write(*,1901)
1901	format(10x,'-depointage possible mais non-autorise (NODMI)'/)
	if(nbrcyctot.gt.0)ier=0
	go to 9999
c
2000	write(*,2001)
2001	format(10x,'-AH de fin plus petit ou egal a AH de debut'/)
	go to 9999
c
9999	continue
        call dumpou(1,tural,tupou,tutot,rtsdman,rtsfpou,incw)
	return
	end
