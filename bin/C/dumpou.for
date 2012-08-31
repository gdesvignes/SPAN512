ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine dumpou(kod,turalR,tupouR,tutotR,rtsdmanR,rtsfpouR,inc)
	implicit none
	include 'constante_def.inc'
	include 'compou.inc'
	include 'durcyc.inc'
	character*50 bufper
	character*20 datbuftu,datbufts
	character*11 stural,stupou,stutot
        character*11 stucyc
	real*8 durtsper,tsral,tspou,tural,tupou,tutot,tstot,f_cvd70rot
        real*8 tucyc,tupositrot,rtsdman,rtsdpou,rtsmerid,rtsfpou,turcycle
	integer*2 n,irep,nocyc
        integer*4 kod,xper,zper,yper,durtuper
        real*4 rper
        real*8 turalR,tupouR,tutotR,rtsdmanR,rtsfpouR,inc
c
	call d70asciibref(datbuftu,tudmand70)
        rtsdman=(alpha+ahdman/60.d0)/24.d0
	call rotasciir(rtsdman,datbufts)
	Write(*,5)datbuftu,datbufts
5	format(10x,'TU de debut de manip = ',a,'   TS=',a)
	if(nbrper.le.0)then
		write(*,1)
1		format(//' **** PAS DE POURSUITE CALCULEE ****')
		return
		endif
        tupositrot=f_cvd70rot(tudpoud70-tudmand70)
	call rotasciir(tupositrot,stural)
	Write(*,4)stural
 4      format(10x,'Duree de positionnement',9x,a)

	call d70asciibref(datbuftu,tudpoud70)
        rtsdpou=(alpha+ahdpou/60.d0)/24.d0
	call rotasciir(rtsdpou,datbufts)
	Write(*,7)datbuftu,datbufts
7       format(10x,'TU de debut poursuite= ',a,'   TS=',a)

	call d70asciibref(datbuftu,tumerid70)
        rtsmerid=alpha/24.d0
	call rotasciir(rtsmerid,datbufts)
	Write(*,6)datbuftu,datbufts
 6      format(10x,'TU de pass.  meridien= ',a,'   TS=',a)
c
c   recalcul du 'tufpoud70' reel, des TS de ralliement et de tracking
c
        tufpoud70=tudpoud70
	tsral=0.d0
	tspou=0.d0
	do n=1,nbrper
           durtsper=p(n)%dperiode
           tufpoud70=tufpoud70+(durtsper*1.d9*CV_dts_dtu)
           if(mod(p(n)%noperiode,2).eq.1)then
              tsral=tsral+durtsper
           else
              tspou=tspou+durtsper
           endif
	enddo
        tsral=tsral/86400.d0
        tspou=tspou/86400.d0
	tural=tsral*cv_dts_dtu 		!en rotation
	tupou=tspou*cv_dts_dtu		!en rotation
	tutot=tural+tupou	        !en rotation
        tstot=tsral+tspou               !en rotation

	call d70asciibref(datbuftu,tufpoud70)
	rtsfpou=rtsdpou+tstot
	call rotasciir(rtsfpou,datbufts)
	Write(*,8)datbuftu,datbufts
8       format(10x,'TU de fin poursuite  = ',a,'   TS=',a)
        write(*,9)ixcornet
 9      format(/10x,'Cornet utilise=',i1/)

c
10	continue
	nocyc=0
        turcycle=0.d0
	n=0
20	n=n+1
	if(n.gt.nbrper)go to 80
        if(p(n)%nocycle .ne. nocyc)then
                if(nocyc.gt.0)then
                   tdurcycms(nocyc)=int((p(n)%turperiode-turcycle)/1.d6)
                   turcycle=p(n)%turperiode
                endif
		nocyc=nocyc+1
		write(*,21)nocyc,p(n)%iperiode
 21             format(' Cycle ',i3,' Inclinaison ',i5/
     &                 ' ---------------------------')
	        if(mod(nocyc,5).eq.1)write(*,11)
11	        format('  Per  TUrel.   Duree    Ahf',
     1  '    Xf    Zf  Yf  Rf   Ycf  Site   Azim.   Vdop.'/
     2  '  ---  ------   -----    ---',
     3  '    --    --  --  --   ---  ----   -----   -----')
        endif
        xper=p(n)%xperiode
        zper=p(n)%zperiode
        yper=p(n)%yperiode
        rper=(p(n)%rperiode-offsetr)/10.0
        if(kod.eq.1)then
	Write(*,23)p(n)%noperiode,p(n)%turperiode/1.d9,
     &       p(n)%dperiode,p(n)%ahperiode,xper,zper,
     &       yper,rper,
     &       p(n)%ycperiode,p(n)%siperiode,p(n)%azperiode,p(n)%vperiode
23	format(1x,i4,1x,f7.2,1x,f7.2,1x,f6.1,1x,i6,1x,i4,1x,i4,1x,
     &       f5.1,1x,i3,1x,f6.3,1x,f8.3,1x,f7.2)
	else
	Write(*,24)p(n)%noperiode,p(n)%turperiode/1.d9,
     &       p(n)%dperiode,p(n)%ahperiode,xper,zper,
     &       yper,rper,
     &       p(n)%ycperiode,p(n)%siperiode,p(n)%azperiode,p(n)%vperiode
 24     format(1x,i4,1x,f7.2,1x,f7.2,1x,f6.1,1x,i6,1x,i4,1x,i4,1x,
     &       f5.1,1x,i3,1x,f6.3,1x,f8.3,1x,f10.5)
        endif
        go to 20
80	continue
        durtuper= int(p(nbrper)%dperiode*1.d3*cv_dts_dtu)
        tdurcycms(nocyc)=int((p(nbrper)%turperiode-turcycle)/1.d6)+durtuper
        ndurcycms=nocyc
c
c
c
c
800	continue
        tucyc=tutot/nocyc
	call rotasciir(tural,stural)
	call rotasciir(tupou,stupou)
	call rotasciir(tutot,stutot)
	call rotasciir(tucyc,stucyc)
	write(*,802)stural,stupou,stutot,stucyc
802	format(/20x,'Duree (TU) totale de ralliement=',a/
     1  20x,'Duree (TU) totale de tracking  =',a/
     2  20x,'Duree (TU) totale de la manip  =',a/
     3  20x,'Duree (TU) moyenne du cycle    =',a)
999	continue
        turalR=tupositrot+tural
        tupouR=tupou
        tutotR=turalR+tupouR
        rtsdmanR=rtsdman
        rtsfpouR=rtsfpou
        inc=p(nbrper)%iperiode
	return
	end
