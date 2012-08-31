	TYPE time_rt
	SEQUENCE
	integer*4 ia
	integer*4 im
	integer*4 ij
	integer*4 ih
	integer*4 iu
	integer*4 is
	integer*4 ic
        END TYPE
c
        TYPE ident
	SEQUENCE
        character*60 nom
        integer*4 uic
        integer*4 num
        integer*4 datcre
        integer*4 heucre
        integer*4 datmod
        integer*4 heumod
        integer*4 datver
        END TYPE
c
        TYPE cfopt
	SEQUENCE
        integer*4 motopt
        END TYPE
c
        TYPE cfpar
	SEQUENCE
        integer*4 xmin
        integer*4 xmax
        integer*4 zmin
        integer*4 zmax
        integer*4 posy
        integer*4 vdy
        integer*4 mvpr
        integer*4 vdyc
        integer*4 ahd
        integer*4 ahf
        integer*4 ncy
        integer*4 hlt
        integer*4 rotmer
	integer*4 posyc
	integer*4 tcag
        END TYPE
c
        TYPE cfbkr
	SEQUENCE
        integer*4 nofilebkr
        integer*4 uicbkr
        integer*4 bidon(4)
        END TYPE
c
        TYPE cfcad
	SEQUENCE
        integer*4 freqech
        integer*4 modeint
        integer*4 nbfil
        integer*4 numfil(16)
        END TYPE
c
        TYPE cfstk
	SEQUENCE
        integer*4 nstok
        character*4 stok(8)
        integer*4 bidon
        END TYPE
c   
        TYPE cfnco
	SEQUENCE
        integer*4 mode
        integer*4 nbank
        integer*4 codebp
        integer*4 flagtf
        integer*4 nbrcarte
        integer*4 modapod
        integer*4 delaiblkr
        integer*4 bidon(3)
        END TYPE
c
        TYPE cfsfr
	SEQUENCE
        integer*4 tauint
        integer*4 numcan(4)
	END TYPE
c
        TYPE cfcal
	SEQUENCE
        integer*4 numtab
        real*4 durcal
	END TYPE
c
	TYPE cfpol
	SEQUENCE
        character*1 polar(8)
        END TYPE
c
        TYPE cfpsr
	SEQUENCE
        integer*4 jpulsa(10)
        END TYPE
c
        TYPE cffre
	SEQUENCE
        integer*4 cornet
        integer*4 filtrehf
        integer*4 filtremf
        integer*4 vitmin
        integer*4 vitmax
	real*8 frerep(4)
        real*8 vitoffset(4)
	real*8 freoffset(8)
	real*8 freol(8)
	real*8 fretr(8)
	real*8 frespin(8)
        END TYPE
c
        TYPE phasep
	SEQUENCE
        real*4 duree
        real*4 vasd
        real*4 dasd
        real*4 vdec
        real*4 ddec
        integer*4 dr
        END TYPE
c
	TYPE cfpou
	SEQUENCE
        integer*2 typou
        integer*2 udasd
        integer*4 nphase
        TYPE(phasep) phase(10)
        END TYPE
c
        TYPE phaser
	SEQUENCE
        real*4 duree
        real*4 df(8)
        integer*4 nophasp
        integer*4 typacq
        integer*4 nfil
        integer*4 blkr
        integer*4 nsao
        integer*4 nnco
        integer*4 detat
        END TYPE
c
        TYPE cfaqr
	SEQUENCE
        integer*4 nphase
        TYPE(phaser) phase(20)
        END TYPE
c
	TYPE nouvsou
	SEQUENCE
	TYPE(ident) ide
        character*32 prog
	integer*4 ajuste(2)
        integer*2 date_manip(10)
        integer*4 numscan
        integer*4 codcoord
        integer*4 anref
        integer*4 codvit
        real*4 asd
        real*4 offset_asd
        real*4 mvtpro_asd
        real*4 redjou_asd
        real*4 dec
        real*4 offset_dec
        real*4 mvtpro_dec
        real*4 redjou_dec
        real*4 vits(4)
        real*4 vitd(4)
        real*4 rotmer
        real*4 mvtpro_rot
        integer*2 date_dtrak(10)
	real*4 libre(64)
        END TYPE
c
        TYPE nouvmap
	SEQUENCE
        TYPE(ident) ide
        TYPE(cfopt) opt
        TYPE(cfpar) par
        TYPE(cfbkr) bkr
        TYPE(cfcad) cad
        TYPE(cfstk) stk
        TYPE(cfnco) nco
        TYPE(cfsfr) sfr
        TYPE(cfcal) cal
        TYPE(cfpol) pol
        TYPE(cfpsr) psr
        TYPE(cffre) fre
        TYPE(cfpou) pou
        TYPE(cfaqr) aqr
        character*40 cfvsu
        character*40 cfdep
        integer*4 hltunit
        integer*4 libre(48)
        END TYPE
c
	TYPE periacqrec
	SEQUENCE
        TYPE(time_rt) hper
        integer*4 noper
        integer*4 nophase
        integer*4 nocycle
        integer*4 typepha
        integer*4 cag
        integer*4 rgd
        integer*4 tab
        integer*4 fil
        integer*4 sao
        integer*4 nco
        character*1 pol(4)
        real*8 trl(4)
        END TYPE
c
	TYPE periodenewcor
	SEQUENCE
	integer*2   nosection
	integer*2   nocycle
        integer*2   nophase
        integer*2   nophacq
	integer*8   turperiode	!temps universel relatif en nanosecondes
        integer*2   typepha     !Type de la phase
	integer*2   nbrint      !Nombre d'integrations correlateur
	integer*2   durint      !Duree d'une integration en seconde
        character*1 etatpol(4)  !etat des polars pendant la periode
        real*8      etattrl(4)	!etat des translateurs pendant la periode
        END TYPE
c
	TYPE periodeacqrec
	SEQUENCE
	integer*2   nosection
	integer*2   nocycle
        integer*2   nophase
        integer*2   nophacq
	integer*8   turperiode	!temps universel relatif en nanosecondes
        integer*2   typepha     !Type de la phase
	integer*2   nbrint      !Nombre d'integrations filtre
	integer*2   tauint      !Taux d'integration sur les filtres(a 100Hz)
	integer*2   etatcag     
	integer*2   etatrgd
	integer*2   etattab
	integer*2   etatnco
	character*1 etatpol(4)
	real*8      etattrl(4)
	END TYPE
c
        TYPE perichamir
	SEQUENCE
        TYPE(time_rt) hper
        integer*2 noper
        integer*2 nophase
        integer*2 nocycle
        integer*2 z
        integer*2 y
        integer*2 r
        integer*2 yc
        integer*2 xc
        integer*4 x
        integer*4 i
        END TYPE
c
	TYPE periodechamir
	SEQUENCE
	integer*2 nosection
	integer*2 nocycle
        integer*2 nophase
	integer*2 noperiode
	integer*8 turperiode	!temps universel relatif en nanosecondes
	real*8    ahperiode	!angle horaire en minutes TS
	real*8    dperiode	!duree de la periode en secondes TS
	integer*4 xperiode	!X chariot en mm
	integer*4 zperiode	!Z chariot en mm
	integer*4 yperiode	!Y chariot en mm
	integer*4 rperiode	!R cornet en 0.1 degre
	integer*4 xcperiode	!Xc cornet 1 ou 2
	integer*4 ycperiode	!Yc cornet en mm
	integer*4 iperiode	!I miroir en millidegre
	real*8    vperiode	!Vitesse obs/ref en km/s
        real*8    siperiode	!site apparent fin de periode (degres)
        real*8    azperiode	!azimut apparent fin de periode (degres)
	END TYPE
c
	TYPE valparenv
	SEQUENCE
        integer*4 par(50)
        END TYPE

c
	TYPE blanker
	SEQUENCE
        integer*8 retech(2)      !retards echantillons
        integer*4 nofilebkr      !No de fichier de config bkr
        integer*4 uicbkr         !No d'uic du fichier de config bkr
        integer*2 seuil(8)       !seuils blankers internes 4fx2p
        integer*2 allong(8)      !allongeurs (delta T) 7 reels
        integer*2 retard(4)      !1 retard interne et 3 externes
        integer*2 registre(3)    ! selinpol,selrdor,selout
        integer*2 bidon          !pour avoir une taille paire
        END TYPE


