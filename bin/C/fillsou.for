        subroutine fillsou(asdw,decw,verbose)
        implicit none
        include 'structures.inc'
        include 'comsoumap.inc'

        real*4 asdw,decw
        integer*4 i,verbose

C        write(*,*)'fillsou> Start...'
        if(verbose.gt.0)then
         write(*,*)'fillsou>  asdw= ',asdw,' decw= ',decw
        endif

C --    Nom de la SOUrce
        sou%ide%nom(1:3)="Sou"

C --    misc
        sou%numscan=0
        sou%codcoord=1
        sou%anref=2000
        sou%codvit=5
C --    R.A.
        sou%asd=asdw
        sou%offset_asd=0.0
        sou%mvtpro_asd=0.0
        sou%redjou_asd=asdw
C --    DEC.
        sou%dec=decw
        sou%offset_dec=0.0
        sou%mvtpro_dec=0.0
        sou%redjou_dec=decw
        DO i=1,4
         sou%vits(i)=0.0
         sou%vitd(i)=0.0
        ENDDO
        sou%rotmer=0.0
        sou%mvtpro_rot=0.0

C        write(*,*)'fillsou> End'

        return
        end
