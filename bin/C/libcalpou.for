cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Cette librairie contient les modules utilises pour le calcul de la poursuite
c   Par convention les unites sont les suivantes:
c   - tout ce qui exprime des temps absolus TU est en 'D70'(c'est a dire
c     en nombre de nanosecondes ecoulees depuis le 1/1/1970 a 0H TU)
c   - tout ce qui se rapporte a l'ascension droite est en heure decimale
c   - tout ce qui se rapporte a un angle horaire est en minute de temps
c   - tout ce qui se rapporte a la declinaison est en degre decimaux
c   - tout ce qui se rapporte a l'azimut et au site est en degre decimaux
c   - tout ce qui se rapporte a l'angle de rotation du cornet est en degre
c   - tout ce qui se rapporte a l'inclinaison est en millieme de degre
c   - tout ce qui exprime une distance (X, Z, Y ...) est en millimetre
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        real*8 function f_ahdeptheo(DEC)        
c
c   calcul de l'angle horaire depart theorique (en minute) en fonction 
c   de la declinaison (DEC en degres decimaux), et des limites en X et Z 
c
c   modif du 3/12/2007:
c   l'angle horaire de depart theorique est toujours du meme signe que xmin
        implicit none
	include 'compou.inc'
        include 'comsoumap.inc'
	real*8 dec,x,zlim,z,f_ahdep,f_zlocal,signx,signa

        x=dfloat(map%par%xmin)
        signx=1.0
        if(x.lt.0.0)signx=-1.0
        if(dec.lt.0.0)then
           zlim=dfloat(map%par%zmax)
        else
           zlim=dfloat(map%par%zmin)
        endif
        z=f_zlocal(zlim)
C        write(*,*)' f_ahdeptheo> dec= ',dec,' zlim= ',zlim,' map%par%zmax= ',map%par%zmax,' map%par%zmin= ',map%par%zmin
        f_ahdeptheo=f_ahdep(dec,x,z)
        signa=1.0
        if(f_ahdeptheo.lt.0.0)signa=-1.0
        if(signa.ne.signx)f_ahdeptheo=-f_ahdeptheo
        if(f_ahdeptheo.lt.map%par%ahd)then
           f_ahdeptheo=map%par%ahd
        endif
        return
        end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        real*8 function f_ahfintheo(DEC)        
c
c   calcul de l'angle horaire fin theorique (en minute) en fonction 
c   de la declinaison (DEC en degres decimaux), et des limites en X et Z 
c
c   modif du 3/12/2007:
c   l'angle horaire de fin theorique est toujours du meme signe que xmax
c
        implicit none
	include 'compou.inc'
        include 'comsoumap.inc'
	real*8 dec,x,zlim,z,f_ahdep,f_zlocal,signx,signa

        x=dfloat(map%par%xmax)
        signx=1.0
        if(x.lt.0.0)signx=-1.0
        if(dec.lt.0.0)then
           zlim=dfloat(map%par%zmax)
        else
           zlim=dfloat(map%par%zmin)
        endif
        z=f_zlocal(zlim)
        f_ahfintheo=f_ahdep(dec,x,z)
        signa=1.0
        if(f_ahfintheo.lt.0.0)signa=-1.0
        if(signa.ne.signx)f_ahfintheo=-f_ahfintheo
        if(f_ahfintheo.gt.map%par%ahf)then
           f_ahfintheo=map%par%ahf
        endif
        return
        end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        Real*8 function f_ahdep(DEC,XMM,ZMM)        
c
c   calcul de l'angle horaire (en minute) en fonction 
c   de la declinaison (DEC en degres decimaux), du X (XMM en mm) et du Z 
c   (ZMM en mm)
c
c   formule:
c   cos(D)sin(AH)=cos(Z)sin(X)
c
        implicit none
	include 'constante_def.inc'
	real*8 dec,xmm,zmm,ah,d,x,z,cdec,sign,sah
c
        d=dec*cv_deg_rad
        cdec=cos(d)
        sign=+1.0
        if((cdec.lt.0.09).and.(xmm.lt.0.0))sign=-1.0
        x=xmm*cv_mm_rad
        z=zmm*cv_mm_rad
        sah=cos(z)*sin(x)/cdec
        if(abs(sah).gt.1.d0)sah=1.d0
        ah=asin(sah)
        f_ahdep=ah*cv_rad_minute*sign
        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_zcodeur(ZLOCAL)
c
c    conversion du Z en mm dans le systeme local en Z codeur
c
        implicit none
	include 'compou.inc'
        real*8 zlocal
        f_zcodeur=zlocal-offsetz
        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_zlocal(ZCODEUR)
c
c    conversion du Z codeur en mm en Z dans le systeme local
c
        implicit none
	include 'compou.inc'
        real*8 zcodeur
        f_zlocal=zcodeur+offsetz
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	logical function f_recalzdep(ZMIN,ZMAX,ZCAL,NITER)
c
c   Cette fonction logique prend la valeur .TRUE. si le Z est bien cale
c   au plus bas dans le plan focal.
c   Si la valeur de la fonction est .FALSE., on retourne dans ZCAL une
c   nouvelle valeur du Z de depart pour reiterer sur la meme section.
c   ZMIN et ZMAX sont les minimum et maximum en Z calcules sur la section,
c   ZCAL est a l'entree le Z de depart de la section qui a donne le ZMIN et
c   le ZMAX considere.
c   ZCAL est en sortie, si f_recalzdep est .FALSE., la nouvelle valeur du Z
c   de depart.
c   Si NITER est > 2 et que le ZMIN est plus grand que 'zlimb' on considere
c   que le but est atteint.
c
        implicit none
	include 'compou.inc'
        real*8 zmin,zmax,zcal,zb,zh,f_zlocal
        integer*4 niter,difz,limdif
c
c   Pour eviter un nombre trop grand d'iterations on regarde, apres
c   au moins 5 iterations, si le zmin et le zmax sont dans la plage
c   zb zh et on se contente de ces valeurs. (modif du 22/8/2000)
c
        zb=f_zlocal(zlimb)
        zh=f_zlocal(zlimh)
        f_recalzdep=.false.
        if(niter.ge.5)then
           if((zmin.ge.zb).and.(zmax.le.zh))then
              f_recalzdep=.true.
              go to 100
           endif
        endif
        limdif=10+(niter-1)*10
        difz=nint(zmin-zb)
        if(difz.ge.-40.and.difz.lt.limdif)then
           f_recalzdep=.true.
           go to 100
        endif
        zcal=zcal-difz
 100    continue
c        if(f_recalzdep)then
c           write(*,*)' sortie de recalzdep: zcal est bon'
c        else
c           write(*,*)' sortie de recalzdep: nouvelle valeur de zcal=',zcal
c        endif
        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        real*8 function f_tprepointage(DINC)
        real*8 dinc
        f_tprepointage=abs(dinc)/100.d0+162.d0
        return
        end
        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        real*8 function f_tpointagefinal(DINC)
c
c   rajouter 20 secondes le 19 Aout 2003
c
        real*8 dinc
        f_tpointagefinal=abs(dinc)+15.d0+30.d0
        return
        end
        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        real*8 function f_tpos(DXMM,DZMM,DYMM,DXCUN,DYCMM,DROT,DINC)        
c
c   calcul du temps de positionnement du telescope en secondes TS
c   en fonction des deplacements DXMM, DZMM, DYMM, DYCMM en mm,
c                                DXCUN en unite,DROT en degres,
c                                DINC en millidegres
c   Si on calcule le temps de positionnement de la premiere periode,
c   et si c'est une manip PULSAR (psr=.TRUE.) il faut calculer le temps
c   d'initialisation des 4 voies du pulsaroscope en fonction du nombre
c   de cycles estimes a la louche et prendre ce temps d'initialisation
c   comme duree de positionnement s'il est plus grand que les autres.
c   De toutes facon la duree de la premiere periode de ralliement ne peut
c   etre inferieure a 5 secondes.
c
c   ON SUPPOSE QUE LES VITESSES DE CHAQUE MOUVEMENT SONT DONNEES EN TU
c
        implicit none
	include 'compou.inc'
        include 'comsoumap.inc'
        include 'constante_def.inc'
	include 'comcalper.inc'
        real*8 dxmm,dzmm,dymm,dxcun,dycmm,drot,dinc,
     1	       dxm,dzm,dym,dxcm,dycm,drm,dim
	real*8 vx,vz,vy,vyc,vr,vi,tposx,tposz,tposy
        real*8 tposxc,tposyc,tposr,tposi,tposmax,tpostu
        real*8 ahd,ahf,f_tpointagefinal,f_tprepointage
        integer*4 nbcyc,idelai

	parameter (vx=325.d0)			!en mm/seconde
	parameter (vz=40.d0)			!en mm/seconde
	parameter (vy=40.d0)			!en mm/seconde
        parameter (vyc=5.d0)			!en mm/seconde
	parameter (vr=3.d0)			!en deg/seconde
c	parameter (vi=1.d0)			!en milideg./seconde
c
	dxm=abs(dxmm)
	dzm=abs(dzmm)
	dym=abs(dymm)
        dxcm=abs(dxcun)
        dycm=abs(dycmm)
	drm=abs(drot)
	dim=abs(dinc)
	tposx=0.d0
	tposz=0.d0
	tposy=0.d0
	tposxc=0.d0
	tposyc=0.d0
	tposr=0.d0
	tposi=0.d0
	f_tpos=0.d0
	if(dxm.gt.0.d0)go to 10
	if(dzm.gt.0.d0)go to 10
	if(dym.gt.0.d0)go to 10
	if(dxcm.gt.0.d0)go to 10
        if(dycm.gt.0.d0)go to 10
	if(drm.gt.0.d0)go to 10
	if(dim.gt.0.d0)go to 10
	go to 100

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en X
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
10	continue
        if((.not.cha).and.(numper.eq.1))go to 20
	if(dxm.lt.1.d0)go to 20
cccccc changement de la formule le 29/8/2000
c        tposx=6.d0+dxm/vx
        tposx=10.d0+dxm/vx

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en Z
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
20	continue
        if((.not.cha).and.(numper.eq.1))go to 30
	if(dzm.lt.1.d0)go to 30
        tposz=4.d0+dzm/33.d0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en Y
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
30	continue
        if((.not.cha).and.(numper.eq.1))go to 40
	if(dym.lt.1.d0)go to 40
        tposy=3.d0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en Xc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
40	continue
        if((.not.cha).and.(numper.eq.1))go to 50
	if(dxcm.lt.1.d0)go to 50
	tposxc=60.d0+tposx

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en Yc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
50	continue
        if((.not.cha).and.(numper.eq.1))go to 60
	if(dycm.lt.1.d0)go to 60
	tposyc=dycm/vyc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en R
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
60	continue
        if((.not.cha).and.(numper.eq.1))go to 70
        if(drm.lt.1.d0)go to 70
	tposr=drm/vr

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en I
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
70	continue
        if((.not.mir).and.(numper.eq.1))go to 80
	if(dim.lt.1.d0)go to 80
c
c  cas particulier de la 1ere periode
c
        if(numper.eq.1)then
           if(pno)then
              tposi=f_tprepointage(dim) + 15.d0 + f_tpointagefinal(118.d0)
c              write(*,71)int(tposi)
c 71           format(' Temps de pointage PNO   au debut de manip=',i5, 'secondes')
           else
              tposi=f_tpointagefinal(dim)
c              write(*,72)int(tposi)
c 72           format(' Temps de pointage NOPNO au debut de manip=',i5, 'secondes')
           endif
c
c   cas des autres periodes
c
        else
           tposi=f_tpointagefinal(dim)      
        endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   le temps de positionnement retenu est le maximum des temps precedemment
c   calcules et il est arrondi a la seconde TU entiere avant d'etre converti
c   en seconde TS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
80	continue
	tposmax=max(tposx,tposz,tposy,tposxc,tposyc,tposr,tposi)
        tpostu=int(tposmax+1.0d0)
        if(tposmax.le.0.0d0)tpostu=0.d0
	f_tpos=tpostu*cv_dtu_dts

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   La premiere periode de ralliement ne peut etre inferieure a 15 secondes TU
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
100     continue
        if(numper.eq.1)then
c
c          write(*,101)dxm,dzm,dym,dxcm,dycm,drm,dim
c 101      format(1x,'dx =',f6.0,'dz =',f6.0,'dy =',f6.0,'dxc=',f6.0,
c     &              'dyc=',f6.0,'dr =',f6.0,'di =',f6.0)
c          write(*,102)tposx,tposz,tposy,tposxc,tposyc,tposr,tposi
c 102      format(1x,'tx =',f6.0,'tz =',f6.0,'ty =',f6.0,'txc=',f6.0,
c     &              'tyc=',f6.0,'tr =',f6.0,'ti =',f6.0)
c
          f_tpos=max(f_tpos,15.d0*cv_dtu_dts)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    si on est en pulsar
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
          if(psr)then
             idelai=30             
             f_tpos=max(f_tpos,idelai*cv_dtu_dts)
          endif
        endif
        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_ahcmvpa(AHA,ALPHA,MVPA)
c
c   retourne l'angle horaire courant en minute en fonction:
c   de l'angle horaire actuel AHA en minute
c   de l'ascension droite ALPHA en heure decimale
c   du mouvement propre en ascension droite MVPA en sec. par seconde
c
	implicit none
	real*8 tsc,aha,alpha,mvpa,nalpha,ahc
c
	tsc=alpha+aha/60.d0                !tsc en heures TS
        ahc=aha                            !ahc et aha en minutes TS
	if(mvpa.eq.0.d0)go to 20
	nalpha=alpha+(mvpa*ahc)/60.d0
        ahc=(tsc-nalpha)*60.d0
20	f_ahcmvpa=ahc
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_damvpa(DUREE,MVPA)
c
c   retourne la variation d'alpha en fonction:
c   de la duree de la periode en secondes et
c   du mouvement propre en ascension droite MVPA en heure/heure
c
	implicit none
	real*8 duree,mvpa
c
	f_damvpa=(duree/3600.d0)*mvpa     !  f_damvpa en heures TS
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_decmvpd(AHC,DELTA,MVPD)
c
c   retourne la declinaison courante en degre decimaux en fonction
c   de l'angle horaire courant AHC en minute
c   de la declinaison DELTA en degre decimaux
c   du mouvement propre en declinaison MVPD en sec. d'arc par seconde
c
	implicit none
	real*8 delta,mvpd,ahc,sec
c
        sec=ahc*60.d0
	f_decmvpd=delta+(mvpd*sec)/3600.d0  !  f_decmvpd en degres
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_ddmvpd(DUREE,MVPD)
c
c   retourne la variation de declinaison courante en degre decimaux en fonction
c   de la duree de la periode en secondes et
c   du mouvement propre en declinaison MVPD en sec. d'arc par seconde
c
	implicit none
	real*8 duree,mvpd
c
	f_ddmvpd=duree*mvpd/3600.d0        !  f_ddmvpd est en degres
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_ahcmvpr(AHA,MVPR)
c
c   retourne la variation d'angle du cornet (en degre) en fonction:
c   de l'angle horaire actuel AHA en minute
c   du mouvement propre en rotation MVPR en degre par heure
c
	implicit none
	real*8 aha,mvpr
c
        f_ahcmvpr=aha*mvpr
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_zdepart(AHD,DEC,ZCAL)
c
c   si a l'entree ZCAL=0 on retourne un Z (local) de depart theorique en mm en fonction
c   -de l'angle horaire de depart en minute
c   -de la declinaison en degres decimaux
c
	implicit none
	include 'compou.inc'
	real*8 ahd,dec,zcal,zb,zh,f_zlocal,f_zcodeur
c
	if(zcal.ne.0.d0)go to 1000
        zb=f_zlocal(zlimb)
        zh=f_zlocal(zlimh)
	if(dec.lt.0.d0)then
		if(ahd.lt.0.d0)then
			zcal=zh
			go to 1000
		else
			zcal=zb
			go to 1000
		endif
	else
		if(ahd.lt.0.d0)then
			zcal=zb
			go to 1000
		else
			zcal=zh
			go to 1000
		endif
	endif
1000	continue

c        write(*,1111)ahd,f_zcodeur(zcal)
c 1111   format(' Calcul ZDEP avec ahd=',f6.1,' ==> ',f6.1)

	f_zdepart=zcal
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine periode_ralliement(OFFPX,KODRAL,ZCAL,IER)
c
c   remise a jour des positions et de l'angle horaire courant a la fin d'une
c   periode de ralliement:
c	AHC=angle horaire courant en minute TS remis a jour au retour
c	ALPHA = ascension droite en heures decimales
c	MVPA = mouvement propre en ascension droite
c	DA = variation d'ascension droite pendant la periode mise a jour(heure)
c	DELTA = declinaison en degres decimaux
c	MVPD = mouvement propre en declinaison
c	DD = variation de declinaison pendant la periode
c	DR = variation d'angle du cornet pendant la periode
c	DUREE = duree de la periode de ralliement mis a jour au retour
c	XC,ZC,YC,RC,XCC,YCC,INCC = positions en X,Z,Y,R,Xc,Yc,I mises a jour
c	DX,DZ,DR = variations imposees (kodral=3) en X, Z et R
c	KODRAL = code de ralliement de la periode:
c		0 => ralliement debut d'une section (calcul Z (si ZCAL=0) et I)
c		1 => ralliement entre deux phases
c		3 => ralliement avec DX,DZ,DR imposes (poursuite PX)
c	ZCAL = Z du debut d'une section (sera calcule si nul a l'entree)
c	IER = code d'erreur au retour:
c		 0 => tout est OK
c		 1 => la source n'est pas encore ou plus dans le champ
c		 2 => le TS calcule est plus petit que TSLIMDEB
c		 3 => le TS calcule est plus grand que TSLIMFIN
c		 4 => l'inclinaison calculee sort des limites
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	implicit none
        include 'position.inc'
	include 'compou.inc'
        include 'comsoumap.inc'
	include 'comcalper.inc'
        include 'constante_def.inc'
	logical offpx
	real*8 zcal,delah
	integer*4 kodral,ier,ker,increel,ipass,incduree,from
	real*8 ahf,xf,zf,yf,rf,xcf,ycf,incf,ahd,xd,zd,yd,rd,xcd,ycd,incd
	real*8 tpos,f_damvpa,f_ddmvpd,f_tpos,dah
	real*8 alphad,deltad,alphaf,deltaf,f_zdepart
	real*8 f_parallactique,f_ahcmvpr

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   positions au debut de la periode de ralliement
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	ier=0
	alphad=alphac
	deltad=deltac
	ahd=ahc
	xd=xc
	zd=zc
	yd=yc
	rd=rc
	xcd=xcc
	ycd=ycc
	incd=incc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   positions a la fin de la periode de ralliement
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	yf=posy+dy
        xcf=xcd+dxc
	ycf=posyc+dyc
c
c        write(*,'(/)')
c        write(*,9)da,dd
c 9      format(' DA=',f12.6,'  DD=',f12.6)
c
         from=0

10	continue

	alphaf=alphad+da+f_damvpa(duree,mvpa)
        if(kodral.eq.3.and.offpx)alphaf=alphaf+duree/3600.d0
	deltaf=deltad+dd+f_ddmvpd(duree,mvpd)
	ahf=ahd+duree/60.d0-(alphaf-alphad)*60.d0
c
c        write(*,11)from,ahd,ahf,duree
c 11     format(/' from=',i2,' Ahd=',f10.6,' Ahf=',f10.6,
c     &       ' Duree=',f7.2)
c        write(*,12)alphad,alphaf,deltad,deltaf
c 12     format(' alphad=',f12.6,' alphaf=',f12.6,
c     &       ' deltad=',f12.6,' deltaf=',f12.6)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   'zf', 'incf' et 'xf' dependent de KODRAL
c   si KODRAL=0 on est en debut de section et il faut calculer une inclinaison
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(kodral.eq.0)then

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   pour ne pas trop iterer, on calcul la difference entre l'angle horaire
c   limite au debut de la manip et l'angle horaire de fin de la periode de
c   ralliement. Si cette difference est > 0.1 minute, on la rajoute directement
c   a la duree de la periode et on recommence le calcul.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
           if(ahlimdeb.gt.ahf)then
              delah=ahlimdeb-ahf
              incduree=delah*60.d0         !incduree est en secondes TS
              duree=duree+incduree+1.0d0
c              write(*,15)ahlimdeb,ahf,duree
c 15           format(' ahlim=',f10.6,' ahf=',f10.6,' new duree=',f10.3)
              from=1
              go to 10
           endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si on est en debut de section et que c'est la premiere iteration sur cette
c   section, il faut calculer le z (provisoire) que l'on doit atteindre a la
c   fin de la periode de ralliement.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	   zf=f_zdepart(ahf,deltaf,zcal)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   connaissant le Z a la fin de la periode, on peut calculer l'inclinaison
c   a la fin de la periode en fonction de l'angle horaire apparent et de la
c   declinaison apparente
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	   call calinc(ahf,deltaf,zf,incf)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   connaissant l'inclinaison a la fin de la periode, on peut calculer le X
c   et le Z a la fin de la periode.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	   call calxz(ahf,deltaf,incf,xf,zf,ker)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Le code de retour de calxz nous indique si la source n'est pas encore
c   dans le champ (-1), dans le champ (0), plus dans le champ (1).
c   Si la source n'est pas encore dans le champ, on ajoute 10s TS
c   a la duree et on recommence le calcul. Si la source est dans
c   le champs on termine le calcul de cette periode. Si la source n'est plus
c   dans le champ on passe un code d'erreur a l'appelant pour terminer le
c   calcul de la poursuite.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
           if(ker.eq.0)go to 20
           if(ker.eq.-1)then
              duree=duree+10.d0*cv_dtu_dts
              from=2
              go to 10
           endif
           ier=1
c           write(*,*)' sortie de periode_ralliement avec ier=',ier
           return

	endif   !fin de l'option kodral=0 





cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si KODRAL=1 on est a l'interieur d'une section et l'inclinaison de fin
c   de la periode a deja ete calculee.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(kodral.eq.1)then
	   incf=incd
	   call calxz(ahf,deltaf,incf,xf,zf,ker)
           if(ker.eq.0)go to 20
           ier=1
c           write(*,*)' sortie de periode_ralliement avec ier=',ier
           return
	endif   !fin de l'option kodral=1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si KODRAL=3 on doit rallier les positions xf=xd-dx,  zf=zd-dz
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(kodral.eq.3)then
	   incf=incd
	   xf=xd-dx
	   zf=zd-dz
	   go to 20

	endif   !fin de l'option kodral=3






ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Quelque soit KODRAL on vient ici pour calculer le temps de positionnement
c   et reajuster la duree de la periode de ralliement.
c   D'abord on calcule l'angle du cornet a la fin de la periode en prenant
c   en compte le 'dr', eventuellement la correction de l'effet
c   parallactique qui n'est fonction que de l'angle horaire et de la
c   declinaison visee et enfin le mouvement propre en rotation
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
20	continue
	rf=rotmer+dr+f_parallactique(ahf,deltaf)+f_ahcmvpr(ahf,mvpr)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du temps de positionnement en secondes TS
c   modif du 23/8/2000:
c   Si on traite la premiere periode, le temps de positionnement du miroir
c   depend de abs(incf-incd). Pour que ce temps colle au mieux avec la realite,
c   on prend comme incd l'inclinaison reelle qui nous donnera l'ecart maximum
c   avec incf. D'autre part on doit tenir compte de offseti pour incd.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        if(numper.eq.1)then
           if(pno)then
              incd=icoumin-offseti
              if(abs(incf-(icoumax-offseti)).gt.abs(incf-incd))then
                 incd=icoumax-offseti
              endif
           else
              incd=ith-offseti
              if(abs(incf-incd).ge.300)then
                 map%opt%motopt=map%opt%motopt+16
                 pno=.true.
                 go to 20
              endif
           endif
        endif
	tpos=f_tpos(xf-xd,zf-zd,yf-yd,xcf-xcd,ycf-ycd,rf-rd,incf-incd)
C        write(*,*)'period_ralliement> f-tpos> incf= ',incf,' incd= ',incd,' -> tpos= ',tpos
C        write(*,21)tpos
C21     format(' Temps de positionnement=',f10.3,' sec. TS')

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   si le temps de positionnement est plus grand que la duree de la periode,
c   on augmente la duree de 1 seconde TU et on reitere
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if(duree.lt.tpos)then
	   duree=tpos+cv_dtu_dts
           from=3
	   go to 10
	endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   c'est fini : remise a jour des valeurs courantes
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
30	continue

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   On a le couple (xf,zf) vrai et la declinaison vraie visee a la fin de la
c   periode. On les transforme en couple (xa,za) apparent qui determine
c   la direction que l'on doit viser et qui tient compte de la
c   correction de refraction atmospherique.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        call correfrac(deltaf,incf,xf,zf,xa,za,siteapp,azimapp)

c
c	write(*,31)ahd,ahf
c31	format(' AHd=',f10.0,' AHf=',f10.0)
c	write(*,32)alphad,alphaf,xd,xf,zd,zf
c32	format(' Alphad=',f10.1,' Alphaf=',f10.1,' xd=',f10.1,' xf=',f10.1,
c     &       ' zd=',f10.1,' zf=',f10.1)
c        write(*,33)xa,za
c 33     format(' xa=',f10.1,' za=',f10.1)
c         write(*,34)
c 34      format(' --------------------------------------------------'/)
c
c	if(ker.ne.0)then
c           write(*,*)' xf=',xf,'zf=',zf
c           ier=1
c        endif
c
	if(ahf.lt.ahlimdeb)then
c           write(*,36)ahf,ahlimdeb
c 36        format(' periode_ralliement: ahf=',f10.3,' < ahlimdeb=',f10.3)
           ier=2
        endif
	if(ahf.gt.ahlimfin)then
c           write(*,35)ahf,ahlimfin
c 35        format(' periode_ralliement: ahf=',f10.3,' > ahlimfin=',f10.3)
           ier=3
        endif
	increel=nint(incf)+offseti
	if(increel.lt.incmin.or.increel.gt.incmax)ier=4
	alphac=alphaf
	da=alphaf-alphad
	deltac=deltaf
	dd=deltaf-deltad
        ahc=ahf
	xc=xf
	dx=xf-xd
	zc=zf
	dz=zf-zd
	yc=yf
	xcc=xcf
	ycc=ycf
	rc=rf
	incc=incf
c        write(*,*)' sortie de periode_ralliement avec ier=',ier
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine periode_tracking(OFFPX,IER)
c
c   calcul des positions et de l'angle horaire a la fin d'une periode de
c   poursuite
c	ALPHAC = ascension droite courante mise a jour au retour
c	MVPA = mouvement propre en ascension droite
c	VA = vitesse en ascension droite pendant la periode
c	DELTAC = declinaison courante mise a jour au retour
c	MVPD = mouvement propre en declinaison
c	VD = vitesse en declinaison pendant la periode
c	DUREE = duree de la periode de poursuite (imposee)
c	XC,ZC,YC,XCC,YCC,RC,INCC = positions en X,Z,Y,Xc,Yc,R,I,
c       mises a jour au retour
c       OFFPX est vrai si on est dans une phase OFF d'une poursuite PX
c             Dans ce cas les positions de fin de la periode de tracking
c             doivent etre identique a celles de la phase ON.
c	IER = code d'erreur au retour:
c		 0 => tout est OK
c		 1 => la source n'est pas encore ou plus dans le champ
c		 2 => l'AH calcule est plus petit que AHLIMDEB
c		 3 => l'AH calcule est plus grand que AHLIMFIN
c		 4 => l'inclinaison calculee sort des limites
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	implicit none
	include 'compou.inc'
	include 'comcalper.inc'
	include 'constante_def.inc'
	logical offpx
        integer*4 ier,ker,increel,ipass
	real*8 xd,zd,yd,xcd,ycd,rd,incd
	real*8 alphad,deltad,ahd
	real*8 xf,zf,yf,xcf,ycf,rf,incf
	real*8 alphaf,deltaf,ahf
	real*8 f_damvpa,f_ddmvpd

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   position au debut de la periode
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	ier=0
	alphad=alphac
	deltad=deltac
	ahd=ahc
	xd=xc
	zd=zc
	yd=yc
        xcd=xcc
        ycd=ycc
	rd=rc
	incd=incc
C        write(*,*)'periode_tracking> alphad= ',alphad,' deltad= ',deltad,' ahd= ',ahd
C        write(*,*)'periode_tracking> xd= ',xd,' zd= ',zd,' yd= ',yd,' xcd= ',xcd,' ycd= ',ycd,' rd= ',rd,' incd= ',incd
C        write(*,*)'periode_tracking> zlimb= ',zlimb,' zlimh= ',zlimh

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   position a la fin de la periode
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	alphaf=alphad+va*duree/3600.d0+f_damvpa(duree,mvpa)
	deltaf=deltad+vd*duree/3600.d0+f_ddmvpd(duree,mvpd)
	ahf=ahd+duree/60.d0-(alphaf-alphad)*60.d0
	yf=posy+dy
        xcf=xcd
        ycf=posyc+dyc
	rf=rd
	incf=incd
        if(offpx)then
           xf=xd-dx
           zf=zd-dz
           go to 20
        endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul du X et du Z a la fin de la periode en fonction de l'angle horaire
c   vrai, de la declinaison vraie et de l'inclinaison.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	call calxz(ahf,deltaf,incf,xf,zf,ker)
	if(ker.ne.0)then
           ier=1
        endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   remise a jour des valeurs courantes
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
20	continue
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   On a le couple (xf,zf) vrai et la declinaison vraie visee a la fin de la
c   periode. On les transforme en couple (xa,za) apparent qui determine
c   la direction que l'on doit viser et qui tient compte de la
c   correction de refraction atmospherique.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        call correfrac(deltaf,incf,xf,zf,xa,za,siteapp,azimapp)
C        write(*,*)'periode_tracking> deltaf= ',deltaf,' incf= ',incf,' xf= ',xf,' zf= ',zf,' xa= ',xa,' za= ',za

c
C	write(*,31)xd,xf
31	format('periode_tracking> Xd=',f10.0,' Xf=',f10.0)
C	write(*,34)zd,zf
34      format('periode_tracking> Zd=',f10.0,' Zf=',f10.0)
C	write(*,32)ahd,ahf
32	format('periode_tracking> Ahd=',f10.3,' Ahf=',f10.3)
C	write(*,33)alphad,alphaf
 33     format('periode_tracking> Alphad=',f10.3,' Alphaf=',f10.3)
c
	if(ahf.lt.ahlimdeb)then
c           write(*,36)ahf,ahlimdeb
c 36        format(' periode_tracking: ahf=',f10.3,' < ahlimdeb=',f10.3)
           ier=2
        endif
	if(ahf.gt.ahlimfin)then
c           write(*,35)ahf,ahlimfin
c 35        format(' periode_tracking: ahf=',f10.3,' > ahlimfin=',f10.3)
           ier=3
        endif
	increel=nint(incf)+offseti
	if(increel.lt.incmin.or.increel.gt.incmax)ier=4
	alphac=alphaf
	da=alphaf-alphad-f_damvpa(duree,mvpa)
	deltac=deltaf
	dd=deltaf-deltad-f_ddmvpd(duree,mvpd)
	ahc=ahf
	xc=xf
	dx=xf-xd
	zc=zf
	dz=zf-zd
	yc=yf
	xcc=xcf
	ycc=ycf
	rc=rf
	incc=incf
c        write(*,*)' sortie de periode_tracking avec ier=',ier
	return
	end
cccccccccccc0cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	logical function f_depassement_z(Z,ZMIN,ZMAX)
c
c   cette fonction calcule ZMIN et ZMAX et prend la valeur logique .TRUE.
c   si ZMAX-ZMIN depasse la valeur ZLIMH-ZLIMB
c
	implicit none
	include 'compou.inc'
	real*8 z,zmin,zmax,dbtz
c
	f_depassement_z=.false.
	dbtz=abs(zlimh-zlimb)
	zmin=min(zmin,z)
	zmax=max(zmax,z)
	if((zmax-zmin).gt.dbtz)then
           f_depassement_z=.true.
           write(*,*)'f_depassement_z> Depassement Z= ',z,' ZMIN=',zmin,' ZMAX=',zmax
           write(*,*)'f_depassement_z> zlimb= ',zlimb,' zlimh= ',zlimh
        endif
	return
	end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine range_periode(NUMSEC,NUMCYC,NUMPHA)
c
c   range les parametres d'une periode dans le tableau de periodes globale
c
	implicit none
	include 'constante_def.inc'
	include 'compou.inc'
	include 'comcalper.inc'
        include 'position.inc'
	integer*4 numsec,numcyc,numpha
        integer*4 ia,im,ij,ih,iu,is,ic,js,ihs,ius,iss,ics,ip
        integer*8 f_cvahd70,f_rndcsd70
        real*8 f_zcodeur,datjex,f_vitobsref
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   si c'est la premiere periode que l'on traite ici, sa duree doit etre un
c   nombre entier de seconde TU. D'autre part, cette duree (en TS) correspond
c   au temps de positionnement de l'instrument aux positions de depart.
c   On veut decomposer cette duree en deux temps: 1° le temps de positionnement
c   reel et 2° un temps forfaitaire pour que le chariot
c   atteigne ses vitesses et que le recepteur fasse le CAG.
c   On recalcule donc une heure de debut de manip qui correspond a l'heure de
c   la premiere periode de ralliement.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	if(numper.eq.1)then
           ahdtrk=ahdman+duree/60.d0			!AH debut de tracking
	   duree=15.d0*cv_dtu_dts			!duree 1ere periode
	   ahdpou=ahdtrk-(duree/60.d0)			!AH debut de poursuite
	   tudpoud70=f_cvahd70(tumerid70,ahdpou)	!date debut de pours.
	   tudtrkd70=f_cvahd70(tumerid70,ahdtrk)	!date debut de track.
           ahfpou=ahdpou				!init. AH fin de pours.
           tufpoud70=tudpoud70				!init. TU fin de pours.
        endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   calcul de la vitesse observateur/reference au debut de la periode
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        call cvd70djlex(tufpoud70,datjex)
	vitobsref=f_vitobsref(codvit,datjex,asdrsun,decrsun,
     &		  alphac,deltac,ahc)

c
c   generation de la periode globale
c
	p(numper)%turperiode=f_rndcsd70(tufpoud70-tudpoud70)
	p(numper)%nosection=numsec
	p(numper)%nocycle=numcyc
        p(numper)%nophase=numpha
	p(numper)%noperiode=numper
	p(numper)%ahperiode=ahc
	p(numper)%dperiode=duree

	p(numper)%xperiode=nint(xa)+offsetx
	p(numper)%zperiode=nint(f_zcodeur(za))
        if(.not.dch)then
           p(numper)%xperiode=xfixe
           p(numper)%zperiode=zfixe
        endif
	p(numper)%yperiode=nint(yc)+offsety
	p(numper)%rperiode=nint(rc*10.d0)+offsetr
	p(numper)%xcperiode=ixcornet
	p(numper)%ycperiode=nint(ycc)+offsetyc
	p(numper)%iperiode=nint(incc)+offseti
	p(numper)%vperiode=vitobsref
        p(numper)%siperiode=siteapp
        p(numper)%azperiode=azimapp
        tufpoud70=tufpoud70+duree*1.d9*cv_dts_dtu
        ahfpou=ahfpou+duree/60.d0
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine get_positions(XA,ZA,YA,RA,XCA,YCA,INCA)
c
c   donne les positions actuelles du radiotelescope qui sont passees comme
c   argument a l'appel de 'iniman'
c
	implicit none
       	include 'compou.inc'
        include 'position.inc'
	real*8 xa,za,ya,xca,yca,ra,inca,f_zlocal
        xfixe=xcou
        zfixe=zcou
	xa=xcou
	za=f_zlocal(dfloat(zcou))
	ya=ycou
        ra=rcou
	xca=xccou
	yca=yccou-offsetyc
	inca=icoumin
c
c
c
        write(*,1)xcou,zcou,ycou,rcou,xccou,yccou,icoumin,icoumax,ith
 1      format(/'Positions actuelles:'/' X=',i6,'   Z=',i6,'   Y=',i6,
     &   '   R=',f5.1,'   Xc=',i2,'   Yc=',i4,'   I=',i5,':',i5,
     &   '   Ith=',i5)
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine init_parampou(passup,ier)
c
c   pour initialiser les parametres de la poursuite dans le common COMPOU
c
c   'ier' est un code de retour:
c       ier=0  ==> OK
c       ier=-1 ==> tslimdeb > ou = tslimfin
c    
	implicit none
       	include 'compou.inc'
        include 'comsoumap.inc'
	include 'constante_def.inc'
	include 'offset.inc'
        integer*2 cornet,i
	real*8 tsh,lambda
	real*4 ahdlu,ahflu,xlimolu,xlimelu
        integer*4 tsflu,ihtsf,iutsf,istsf,ier
	logical passup,f_bitest
c
c   changement des limites en X pour tenir compte des fins de course logiciel 24/01/2005
c
	real*8 xlimoc,xlimo/-39950.d0/
	real*8 xlimec,xlime/+39950.d0/
        real*8 ahlimhlt
c
c   les limites en 'Z' sont indicees par le numero de cornet
c
	real*8 zlimbas(2)/0.d0,0.d0/
	real*8 zlimhau(2)/1000.d0,1000.d0/
        real*8 zlimbfx,zlimhfx,zlimblu,zlimhlu
c
c   les limites en 'Yc' sont indicees par le numero de cornet
c
	real*8 yclim(2)/300.d0,500.d0/
c
c   les offsets sont indices par le numero de cornet
c
c	integer*4 offsetxmm(2)/0,0/		!en mm
c	integer*4 offsetzmm(2)/-1650,-1650/	!en mm
c	integer*4 offsetymm(2)/0,0/		!en mm
c	integer*4 offsetrdd(2)/900,900/		!en 0.1 degres
c	integer*4 offsetimd(2)/145,145/		!en 0.001 degres
c	integer*4 offsetycmm(2)/0,0/		!en mm
c
c
c
c
        ier=0
c
c   lecture des offsets
c
        call getoffset(ier)
        if(ier.ne.0)return
c
c   init. de valeur a partir de la source et du map
c
        cornet=map%fre%cornet
c     
c   determination du 'Xc'
c
	ixcornet=cornet
c
c   determination des offsets effectifs pour la manip
c
	offsetx=offsetxmm(cornet)
	offsetz=offsetzmm(cornet)
	offsety=offsetymm(cornet)
	offsetr=offsetrdd(cornet)
	offseti=offsetimd(cornet)
	offsetyc=offsetycmm(cornet)

	ahdlu=map%par%ahd
        ahflu=map%par%ahf
        xlimolu=map%par%xmin
        xlimelu=map%par%xmax
c
c   les limites en z reel donnee par l'utilisateur sont en zcodeur,
c
        zlimblu=dfloat(map%par%zmin)
        zlimhlu=dfloat(map%par%zmax)
	zlimbfx=zlimbas(cornet)
	zlimhfx=zlimhau(cornet)

        rotmer=sou%rotmer !rotation au meridien en degre
	icorpar=0
        if(prl)icorpar=1
c
c   calcul de la variation en 'y' si on a l'option 'may'
c       et de la variation en 'yc' si on a l'option 'myc'         
c   La position en y sera determine par le parametre map%par%posy;
c   c'est la position mediane si on a 'may', c'est a dire que la 
c   position y sera pour les cycles impaires: y = map%par%posy - vary/2
c               et  pour les cycles  paires : y = map%par%posy + vary/2
c   La logique est la meme pour le mouvement alterne en yc c'est a dire
c   que si on a l'option 'myc' la position en yc sera:
c        pour les cycles impaires : yc = map%par%posyc - varyc/2
c     et pour les cycles paires   : yc = map%par%posyc + varyc/2
c
c   Les parametres vary et varyc sont nuls si les options correspondantes
c   (may ou myc) ne sont pas activees. Si les options sont activees, les
c   valeurs de vary et varyc sont prises dans le map (map%par%vdy et
c   map%par%vdyc) et s'ils sont nuls dant le map, ils sont mis par defaut
c   a la valeur lambda/4.
c
        lambda=c/map%fre%frerep(1)
        posy=map%par%posy
        if((cornet.eq.1).and.(map%par%posyc.gt.300))map%par%posyc=300
        posyc=map%par%posyc
        vary=0.d0
        varyc=0.d0
	if(may)then
           vary=map%par%vdy
           if(vary.le.0.d0)vary=lambda/4.d0
        endif
	if(myc)then
           varyc=map%par%vdyc
           if(varyc.le.0.d0)varyc=lambda/4.d0
        endif
        if((posy+vary/2.d0).gt.150.d0)posy=150.d0-vary/2.d0
        if((posy-vary/2.d0).lt.-150.d0)posy=-150.d0+vary/2.d0
        if((posyc+varyc/2.d0).gt.yclim(cornet))posyc=yclim(cornet)-varyc/2.d0
        if((posyc-varyc/2.d0).lt.0.d0)posyc=varyc/2.d0
c
        map%par%posy=posy
        map%par%vdy=vary
        map%par%posyc=posyc
        map%par%vdyc=varyc
c
c   prise en compte des limites en X donnees eventuellement par l'utilisateur
c
	xlimoc=xlimo
	xlimec=xlime
	if(xlimolu.gt.xlimo)xlimoc=xlimolu
	if(xlimelu.lt.xlime)xlimec=xlimelu
	xlimam=xlimoc			!limite en X ante meridienne
	xlimpm=xlimec			!limite en X post meridienne
c
c   inversion des limites en X si on a un passage superieur
c
	if(passup)then
		xlimam=xlimec
		xlimpm=xlimoc
        endif
c
c   prise en compte des limites en Z donnees eventuellement par l'utilisateur
c
	if(zlimblu.gt.zlimbfx.and.zlimblu.lt.zlimhfx)then
           zlimb=zlimblu
        else
           zlimb=zlimbfx
        endif
	if(zlimhlu.lt.zlimhfx.and.zlimhlu.gt.zlimbfx)then
           zlimh=zlimhlu
        else
           zlimh=zlimhfx
        endif
c
c   prise en compte des angles horaires debut et fin donnes eventuellement par
c   l'utilisateur
c
	ahlimdeb=ahdman			!en minutes
	ahlimfin=ahflu			        !en minutes
	if(ahdlu.gt.ahlimdeb)ahlimdeb=ahdlu
c
c   prise en compte des limites theoriques des angles horaire
c
        if(ahdtheo.gt.ahlimdeb)ahlimdeb=ahdtheo
        if(ahftheo.lt.ahlimfin)ahlimfin=ahftheo

c
c   prise en compte du TS de fin (TSFI) donnee par l'utilisateur
c   le TS de fin est donne en heure et minute
c   ce parametre n'est pris en compte que s'il est strictement positif
c
        tsflu=map%par%hlt
        if(tsflu.ge.0)then
           ihtsf=tsflu/100
           iutsf=tsflu-ihtsf*100
           istsf=0
           tsh=ihtsf+iutsf/60.d0+istsf/3600.d0
           if((tsh-alpha).gt.+12.d0)tsh=tsh-24.d0
           if((tsh-alpha).lt.-12.d0)tsh=tsh+24.d0
           ahlimhlt=(tsh-alpha)*60.d0
           if(ahlimhlt.lt.ahlimfin)ahlimfin=ahlimhlt
        endif
        if(ahlimfin.le.ahlimdeb)then
           write(*,111)ahlimdeb,ahlimfin
 111       format(' ERR ahlimdeb=',f10.2,' > ahlimfin=',f10.2)
           ier=-1
        endif
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine init_flagopt()
c
c   pour initialiser les drapeaux optionnels dans le common COMPOU
c
	implicit none
       	include 'compou.inc'
        include 'comsoumap.inc'
        logical f_bitest
c
c
c
	cha=.false.
        if(f_bitest(map%opt%motopt,0))cha=.true.
	dch=.false.
        if(f_bitest(map%opt%motopt,1))dch=.true.
	mir=.false.
        if(f_bitest(map%opt%motopt,2))mir=.true.
	dmi=.false.
        if(f_bitest(map%opt%motopt,3))dmi=.true.
	pno=.false.
        if(f_bitest(map%opt%motopt,4))pno=.true.
	rec=.false.
        if(f_bitest(map%opt%motopt,5))rec=.true.
	acq=.false.
        if(f_bitest(map%opt%motopt,6))acq=.true.
	may=.false.
        if(f_bitest(map%opt%motopt,7))may=.true.
	prl=.false.
        if(f_bitest(map%opt%motopt,8))prl=.true.
	dvr=.false.
        if(f_bitest(map%opt%motopt,9))dvr=.true.
	fil=.false.
        if(f_bitest(map%opt%motopt,10))fil=.true.
	sao=.false.
        if(f_bitest(map%opt%motopt,11))sao=.true.
	nco=.false.
        if(f_bitest(map%opt%motopt,12))nco=.true.
	psr=.false.
        if(f_bitest(map%opt%motopt,13))psr=.true.
	sfr=.false.
        if(f_bitest(map%opt%motopt,14))sfr=.true.
	myc=.false.
        if(f_bitest(map%opt%motopt,15))myc=.true.
	imm=.false.
        if(f_bitest(map%opt%motopt,16))imm=.true.
	nbp=.false.
        if(f_bitest(map%opt%motopt,17))nbp=.true.
	bkr=.false.
        if(f_bitest(map%opt%motopt,18))bkr=.true.
	rdh=.false.
        if(f_bitest(map%opt%motopt,19))rdh=.true.
c
c   si c'est une manip pulsar, on n'accepte pas les depointages
c   plus d'actualite : le pulsaroscope n'est plus en activite -> psr=bon
c        if(psr)dmi=.false.
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine altazimut(AHC,DEC,SITE,AZIMUT)
c
c   ce sous programme retourne le SITE VRAI (la hauteur) et l'AZIMUT
c   d'un objet en degres decimaux en fonction de:
c	AHC= l'angle horaire exprime en minute
c	DEC= la declinaison de l'objet exprime en degres decimaux
c
c   formules :
c
c	sin(site)=sin(dec)sin(lat)+cos(dec)cos(lat)cos(ahc)
c	sin(azimut)=cos(dec)sin(ahc)/cos(site)
c	cos(azimut)=(cos(dec)sin(lat)cos(ahc)-sin(dec)cos(lat))/cos(site)
c	azimut=atan(sin(azimut)/cos(azimut))
c
c   avec la convention des astronomes qui met le SUD a l'azimut=0 et
c   l'OUEST a l'azimut=+90
c
	implicit none
	include 'constante_def.inc'
	real*8 ahc,dec,site,azimut,sahc,cahc,sdec,cdec,slat,clat
	real*8 ssite,csite,sazimut,cazimut
c
	sahc=sin(ahc*cv_minute_rad)
	cahc=cos(ahc*cv_minute_rad)
	sdec=sin(dec*cv_deg_rad)
	cdec=cos(dec*cv_deg_rad)
	slat=sin(latitude_nancay)
	clat=cos(latitude_nancay)
c
	ssite=sdec*slat+cdec*clat*cahc
	site=asin(ssite)
	csite=cos(site)
	if(abs(csite).lt.1.d-6)csite=1.d-6	!pour eviter une division par 0
	sazimut=cdec*sahc/csite
	cazimut=(cdec*slat*cahc-sdec*clat)/csite
	azimut=atan2(sazimut,cazimut)
c
	site=site*cv_rad_deg
	azimut=azimut*cv_rad_deg
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine equator(SITE,AZIMUT,AHC,DEC)
c
c   ce sous programme retourne l'angle horaire AHC en minute TS
c   et la declinaison DEC en degres decimaux en fonction de:
c	SITE en degres decimaux
c	AZIMUT exprime en degres decimaux
c
c   formules :
c
c	sin(dec)=sin(site)sin(lat)-cos(site)cos(lat)cos(azimut)
c	cos(ahc)*cos(dec)=sin(site)cos(lat)+cos(site)sin(lat)cos(azimut)
c
c   avec la convention des astronomes qui met le SUD a l'azimut=0 et
c   l'OUEST a l'azimut=+90
c
	implicit none
	include 'constante_def.inc'
	real*8 ahc,dec,site,azimut,cah,cde,sde,sla,cla
	real*8 ssi,csi,caz
c
	ssi=sin(site*cv_deg_rad)
	csi=cos(site*cv_deg_rad)
	caz=cos(azimut*cv_deg_rad)
	sla=sin(latitude_nancay)
	cla=cos(latitude_nancay)
        sde=ssi*sla-csi*cla*caz
        dec=asin(sde)
        cde=cos(dec)
        if(abs(cde).lt.1.d-9)then
           ahc=0.d0
        else
           cah=(ssi*cla+csi*sla*caz)/cos(dec)
           ahc=sign(abs(acos(cah))*cv_rad_minute,azimut)
        endif
        dec=dec*cv_rad_deg
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine correfrac(DEC,INC,XV,ZV,XA,ZA,SITE,AZIMUT)
c
c   cette routine converti un couple (X,Z) vrai en (X,Z) apparent
c   en tenant compte de la refraction atmospherique.
c   En entree on a:
c         DEC=declinaison vraie en degres decimaux
c         INC=inclinaison du miroir en millidegres
c         Xv,Zv = valeurs vraies en millimetres
c   En sortie on a:
c         Xa,Za = valeurs apparente dans les meme unite qu'en entree
c         SITE et AZIMUT apparents en degres decimaux.
        implicit none
        include 'constante_def.inc'
        integer*4 ker
        real*8 dec,inc,xv,zv,xa,za,sah,ah,aha,deca
        real*8 site,azimut,f_site_apparent
c
        sah=cos(zv*cv_mm_rad)*sin(xv*cv_mm_rad)/cos(dec*cv_deg_rad)
        ah=asin(sah)*cv_rad_minute
        call altazimut(ah,dec,site,azimut)
        site=f_site_apparent(site)
        call equator(site,azimut,aha,deca)
        call calxz(aha,deca,inc,xa,za,ker)
        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine calinc(AHD,DEC,ZMM,INC)
c
c   ce sous programme retourne l'inclinaison INC en millidegres
c   en fonction de:
c    AHC=angle horaire en minute
c    DEC=declinaison en degre decimaux
c    ZMM=le Z en mm
c
	implicit none
	include 'constante_def.inc'
	real*8 ahd,dec,zmm,inc,szmm,cahd,cdec,sdec,spsi,cpsi
        real*8 discrim,numer,denom,tgi
c
c
c
        sdec=sin(dec*cv_deg_rad)
        cdec=cos(dec*cv_deg_rad)
        cahd=cos(ahd*cv_minute_rad)
        szmm=sin(zmm*cv_mm_rad)
        spsi=sin(psi)
        cpsi=cos(psi)
        discrim=sqrt((sdec*sdec)+(cdec*cdec)*(cahd*cahd)-(szmm*szmm))
        numer=-((sdec*spsi) + (cdec*cahd*cpsi))
        denom=szmm - (sdec*cpsi) + (cdec*cahd*spsi)
        if(abs(denom).lt.1.d-9)then
           tgi=-szmm/numer
        else
           tgi=(numer+discrim)/denom
           if(tgi.lt.0.d0)tgi=(numer-discrim)/denom
        endif
        inc=atan(abs(tgi))*cv_rad_deg*1000.d0
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine calxz(AHD,DEC,INC,XMM,ZMM,KER)
c
c   ce sous programme retourne le X et le Z en millimetres
c   en fonction de:
c    AHD=angle horaire en minute
c    DEC=declinaison en degre decimaux
c    INC=l'inclinaison du miroir en millidegres
c    KER est un code d'erreur retourne qui signifie:
c		-1  l'objet n'est pas encore dans le champs
c		0   l'objet est dans le champs
c		+1  l'objet n'est plus dans le champs
	implicit none
	include 'constante_def.inc'
        include 'compou.inc'
        integer*4 ker
	real*8 ahd,dec,inc,xmm,zmm,irad,eps
        real*8 sxmm,szmm,czmm,sahd,cahd,cdec,sdec,seps,ceps
c
        irad=(inc/1000.d0)*cv_deg_rad
        eps=psi-(2.d0*irad)
        seps=sin(eps)
        ceps=cos(eps)
        sdec=sin(dec*cv_deg_rad)
        cdec=cos(dec*cv_deg_rad)
        sahd=sin(ahd*cv_minute_rad)
        cahd=cos(ahd*cv_minute_rad)
        szmm=-sdec*ceps+cdec*cahd*seps
        zmm=asin(szmm)
        czmm=cos(zmm)
        sxmm=cdec*sahd/czmm
        xmm=asin(sxmm)*cv_rad_mm
        zmm=zmm*cv_rad_mm
	ker=0
	if(xlimam.lt.0)then
		if(xmm.lt.xlimam)ker=-1
		if(xmm.gt.xlimpm)ker=1
	else
		if(xmm.gt.xlimam)ker=-1
		if(xmm.lt.xlimpm)ker=1
	endif
c        write(*,*)' sortiee de calxz avec ker=',ker
        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine ixzaltaz(INC,XMM,ZMM,SITE,AZIMUT)
c
c   ce sous programme retourne le site et l'azimut en degres decimaux
c   en fonction de:
c    INC=l'inclinaison du miroir en millidegres
c    XMM=le X local en mm
c    ZMM=le Z local en mm
	implicit none
	include 'constante_def.inc'
	real*8 a,b,ah,de,ahc,dec,inc,xmm,zmm,irad,eps
        real*8 sx,cx,sz,cz,se,ce,site,azimut

        irad=(inc/1000.d0)*cv_deg_rad
        eps=psi-(2.d0*irad)
        se=sin(eps)
        ce=cos(eps)
        sx=sin(xmm*cv_mm_rad)
        cx=cos(xmm*cv_mm_rad)
        sz=sin(zmm*cv_mm_rad)
        cz=cos(zmm*cv_mm_rad)
        a=cz*sx
        b=cz*cx*ce+sz*se
        ah=atan2(a,b)
        if(ah.eq.0.d0)then
           de=acos(b)
        else
           de=acos(a/sin(ah))
        endif
        ahc=ah*cv_rad_minute
        dec=de*cv_rad_deg
        call altazimut(ahc,dec,site,azimut)
        return
        end
        


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_site_apparent(site_vrai)
c
c   Cette function retourne le site apparent (degres decimaux) en fonction
c   du site vrai (degres decimaux) en tenant compte de la refraction calculee
c   en interpolant une table.
c   Cette table de correction de refraction est donnee en seconde d'arc pour
c   des sites variant de 0 degre a 90 degres
c
	implicit none
	real*8 site_vrai,decim,cor
	real*4 t(0:90)
	integer*4 ideg
c
	data t/    
     &	1009.4,1009.4,823.6,683.9,578.4,498.2,436.3,387.9,349.2,317.5,
     1  291.1,268.4,248.7,231.2,215.6,201.6,188.9,177.4,167.1,157.8,
     2	149.4,141.9,135.1,129.0,123.5,118.4,113.8,109.5,105.5,101.8,
     3	98.2,94.7,91.4,88.1,85.0,81.8,78.8,75.8,73.0,70.2,
     4	67.4,64.8,62.4,60.0,57.7,55.6,53.6,51.8,50.0,48.4,
     5	46.8,45.4,44.0,42.7,41.4,40.2,39.0,37.8,36.6,35.4,
     6	34.2,32.9,31.6,30.3,28.9,27.5,26.0,24.5,23.0,21.4,
     7	19.8,18.2,16.7,15.1,13.5,12.0,10.6,9.1,7.8,6.5,
     8	5.4,4.3,3.3,2.5,1.7,1.1,0.6,0.3,0.1,0.0,0.0/
c
	ideg=int(site_vrai)
	if(ideg.lt.0.or.ideg.ge.89)then
		f_site_apparent=site_vrai
		return
		endif
c
c   interpolation de la table sur les dixiemes de degre
c
	decim=site_vrai-ideg
	cor=(t(ideg)+(t(ideg+1)-t(ideg))*decim)/3600.d0
	f_site_apparent=site_vrai+cor
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	real*8 function f_parallactique(ah,dec)
c
c   Cette function retourne l'angle parallactique en degre decimaux en fonction
c   de l'angle horaire 'AH' (en minute) et de 'DEC' vise (en deg. decimaux).
c   Cet angle vaut 0.d0 si l'utilisateur n'a pas demande la correction de
c   l'effet parallactique (icorpar=0)
c
c   formule: tg(a)=tg(ah)*sin(dec)
c	a=angle parallactique
c       ah=angle horaire
c	dec=declinaison visee
c
c
	implicit none
	include 'compou.inc'
	include 'constante_def.inc'
	real*8 ah,dec,tga
c
	f_parallactique=0.d0
	if(icorpar.eq.0)return
	tga=tan(ah*cv_minute_rad)*sin(dec*cv_deg_rad)
	f_parallactique=atan(tga)*cv_rad_deg
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	logical function f_bitest(mot,nbit)
        implicit none
        integer*4 and
        integer*4 mot(*)
        integer*4 mask,ind,nbit,nobit
c
	ind=nbit/32+1
        nobit=nbit-((ind-1)*32)
        mask=2**nobit
        f_bitest=.false.
        if(and(mot(ind),mask).ne.0)f_bitest=.true.
        return
        end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine getoffset(ier)
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c   lit le fichier des offsets sur tous les axes.
cccccccccccccccccccccccccccccccccccccccccccccccccccc
	implicit none
	include 'offset.inc'
	character*32 filename
	integer*4 ier

        offsetxmm(1)=0
        offsetxmm(2)=0
        offsetymm(1)=0
        offsetymm(2)=0
        offsetzmm(1)=-1650
        offsetzmm(2)=-1650
        offsetycmm(1)=0
        offsetycmm(2)=0
        offsetrdd(1)=900
        offsetrdd(2)=900
        offsetimd(1)=143
        offsetimd(2)=138
C	write(filename,601)
C601	format('/data/environ/offset')
C	open(unit=1,file=filename,status='unknown',err=999)
c

C	read(1,*)offsetxmm
C	read(1,*)offsetymm
C	read(1,*)offsetzmm
C	read(1,*)offsetycmm
C	read(1,*)offsetrdd
C	read(1,*)offsetimd
C	close(unit=1)
C	ier=0
C	go to 9999
c
c
c
C999	continue
C	write(*,101)filename
C101	format('getoffset> err open fichier: ',a)
        write(*,*)'getoffset> Using default values'
C	ier=-1
c
9999	continue
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc





























