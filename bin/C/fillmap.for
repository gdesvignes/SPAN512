        subroutine fillmap(duree)
        implicit none
C        include 'structures.inc'
        include 'compou.inc'
        include 'comsoumap.inc'

        real*4 duree
        integer*4 i

C        write(*,*)'fillmap> Start...'
        write(*,*)'fillmap>  duree= ',duree

C --    Nom de la SOUrce
        map%ide%nom(1:3)="Map"

C --    Options
C --    logical*1 cha,dch,mir,dmi,pno,rec,acq,may,prl,dvr,fil,sao
C --    logical*1 nco,psr,sfr,myc,imm,nbp,bkr,rdh
C        map%opt%motopt=0
C --    Fait le meme boulot que init_flagopt !!
        cha=.true.
        dch=.true.
        mir=.true.
        dmi=.true.
        pno=.true.
        rec=.true.
        acq=.true.
        may=.false.
        prl=.false.
        dvr=.false.
        fil=.false.
        sao=.false.
        nco=.false.
        psr=.true.
        sfr=.true.
        myc=.false.
        imm=.false.
        nbp=.false.
        bkr=.false.
        rdh=.false.

C --    Parameters
        map%par%xmin=-45000
        map%par%xmax=+45000
        map%par%zmin=0
        map%par%zmax=1000
        map%par%posy=0
        map%par%vdy=0
        map%par%mvpr=0
        map%par%vdyc=0
        map%par%ahd=-35
        map%par%ahf=+35
        map%par%ncy=1
        map%par%hlt=-1
        map%par%rotmer=0
        map%par%posyc=0
        map%par%tcag=0
C --    fre
        map%fre%cornet=1
C --    Poursuite
        map%pou%typou=7
        map%pou%udasd=0
        map%pou%nphase=2
        map%pou%phase(1)%duree=30.0
        map%pou%phase(1)%vasd=0.0
        map%pou%phase(1)%dasd=0.0
        map%pou%phase(1)%vdec=0.0
        map%pou%phase(1)%ddec=0.0
        map%pou%phase(1)%dr=0
        map%pou%phase(2)%duree=duree
        map%pou%phase(2)%vasd=0.0
        map%pou%phase(2)%dasd=0.0
        map%pou%phase(2)%vdec=0.0
        map%pou%phase(2)%ddec=0.0
        map%pou%phase(2)%dr=0


C        write(*,*)'fillmap> End'
        return
        end
