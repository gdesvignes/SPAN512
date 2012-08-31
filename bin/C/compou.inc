ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	include 'structures.inc'
	integer*4 incmin	!inclinaison minimum miroir mobile
	parameter (incmin=5000)
	integer*4 incmax	!inclinaison maximum miroir mobile
	parameter (incmax=88710)
	integer*4 nbrpermax	!nombre de periodes maximum
	parameter (nbrpermax=4000)
	type (periodechamir) p(nbrpermax)  !tableau des periodes cha/mir
        type (periodeacqrec) q(nbrpermax)  !tableau des periodes acq/rec
        type (periodenewcor) r(nbrpermax)  !tableau des periodes newcor
	real*8 alpha		!ascension droite en heure decimale
        real*8 delta		!declinaison en degre decimaux
	real*8 xlimam		!limite en X avant le meridien
	real*8 xlimpm		!limite en X apres le meridien
	real*8 ahlimdeb		!AH limite debut de poursuite
	real*8 ahlimfin		!AH limite fin de poursuite
	real*8 ahdtheo		!AH debut theorique
	real*8 ahftheo		!AH fin theorique
	real*8 zlimb		!limite en Z ascenseur en bas
	real*8 zlimh		!limite en Z ascenseur en haut
	real*8 posy		!position de reference en Y
	real*8 posyc		!position de reference en Yc
	real*8 vary		!variation en Y pour la frequence visee
	real*8 varyc		!variation en Yc pour la frequence visee
	real*8 rotmer		!angle de rotation du cornet au meridien

	real*8 ahdman		!AH debut de manip (en minutes TS)
	real*8 ahdpou		!AH debut de poursuite (en minutes TS)
	real*8 ahdtrk		!AH debut de tracking (en minutes TS)
	real*8 ahfpou		!AH fin de poursuite (en minutes TS)
	real*8 durcycacq	!Duree du cycle d'acquisition (en minutes TS)

	integer*8 tudmand70	!date TU debut de manip (en d70)
	integer*8 tudpoud70	!date TU debut de poursuite (en d70)
        integer*8 tudtrkd70	!date TU debut de tracking (en d70)
	integer*8 tumerid70	!date TU du meridien (en d70)
	integer*8 tufpoud70	!date TU fin de manip (en d70)

        integer*4 dtu		!correction DTU en 0.01 secondes

	integer*4 icorpar	!correction parallactique (0=non 1=oui)
	integer*4 ixcornet	!Xc=numero de cornet (1 ou 2)
	integer*4 offsetx	!offset a appliquer aux valeurs de X
	integer*4 offsetz	!offset a appliquer aux valeurs de Z
	integer*4 offsety	!offset a appliquer aux valeurs de Y
	integer*4 offsetyc	!offset a appliquer aux valeurs de Yc
	integer*4 offsetr	!offset a appliquer aux valeurs de R
	integer*4 offseti	!offset a appliquer aux valeurs de INC
	integer*4 nbrper	!nombre total de periodes de poursuite
	integer*4 nbrperacq	!nombre total de periodes d'acquisition
	integer*4 nbrcyctot	!nombre total de cycles
	integer*4 nbrsectot	!nombre total de sections
	logical*1 flagopt(32)	!flags optionnels
	logical*1 cha,dch,mir,dmi,pno,rec,acq,may,prl,dvr,fil,sao
	logical*1 nco,psr,sfr,myc,imm,nbp,bkr,rdh
        equivalence (flagopt(1),cha),(flagopt(2),dch)
        equivalence (flagopt(3),mir),(flagopt(4),dmi)
        equivalence (flagopt(5),pno),(flagopt(6),rec)
        equivalence (flagopt(7),acq),(flagopt(8),may)
        equivalence (flagopt(9),prl),(flagopt(10),dvr)
        equivalence (flagopt(11),fil),(flagopt(12),sao)
        equivalence (flagopt(13),nco),(flagopt(14),psr)
        equivalence (flagopt(15),sfr),(flagopt(16),myc)
        equivalence (flagopt(17),imm),(flagopt(18),nbp)
        equivalence (flagopt(19),bkr),(flagopt(20),rdh)
c
	common /compou/p,q,r,alpha,delta,
     &		xlimam,xlimpm,ahlimdeb,ahlimfin,ahdtheo,ahftheo,zlimb,zlimh,
     &		posy,posyc,vary,varyc,rotmer,
     1		ahdman,ahdpou,ahdtrk,ahfpou,durcycacq,
     &		tudmand70,tudpoud70,tudtrkd70,tumerid70,tufpoud70,
     2          dtu,icorpar,
     3		ixcornet,offsetx,offsetz,offsety,offsetyc,offsetr,
     4		offseti,nbrper,nbrperacq,nbrcyctot,nbrsectot,flagopt
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



