c
c   definition de constantes instrumentales et de conversion
c
	real*8 pi
	parameter (pi=3.141592653589793d0)	!constante pi
	real*8 rayon
	parameter (rayon=280520.d0)		!rayon sphere focale en mm
	real*8 latitude_nancay
	parameter (latitude_nancay=0.82670429d0)	!en radian
	real*8 longitude_nancay
	parameter (longitude_nancay=-6.104729938d-3)	!en rotation
	real*8 psi
	parameter (psi=latitude_nancay+pi/2.d0)		!en radian	
	real*8 c
	parameter (c=299792.458d0)			!en km/s	
c
c   toutes les constantes de conversion sont des facteurs multiplicatif
c
	real*8 cv_deg_rad
	parameter (cv_deg_rad=pi/180.d0)	!conversion degre -> radian
	real*8 cv_rad_deg
	parameter (cv_rad_deg=180.d0/pi)	!conversion radian -> degre
	real*8 cv_rot_rad
	parameter (cv_rot_rad=2.d0*pi)		!conversion rotation -> radian
	real*8 cv_rad_rot
	parameter (cv_rad_rot=1.d0/(2.d0*pi))	!conversion radian -> rotation
	real*8 cv_hr_rad
	parameter (cv_hr_rad=pi/12.d0)		!conversion heure -> radian
	real*8 cv_rad_hr
	parameter (cv_rad_hr=12.d0/pi)		!conversion radian -> heure
	real*8 cv_mm_rad
	parameter (cv_mm_rad=1.d0/rayon)	!conversion mm -> radian
	real*8 cv_rad_mm
	parameter (cv_rad_mm=rayon)		!conversion radian -> mm
	real*8 cv_minute_rad
	parameter (cv_minute_rad=pi/720.d0)	!conversion minute(T) -> radian
	real*8 cv_rad_minute
	parameter (cv_rad_minute=720.d0/pi)	!conversion radian -> minute(T)
	real*8 cv_dtu_dts
	parameter (cv_dtu_dts=1.0027379093d0)	!conversion dTU -> dTS
	real*8 cv_dts_dtu
	parameter (cv_dts_dtu=1.d0/cv_dtu_dts)	!conversion dTS -> dTU
	real*8 djl070
        parameter (djl070=2440587.5d0)		!date julienne du 1/1/70 a 0H
	real*8 cv_minuts_nanotu
        parameter (cv_minuts_nanotu=60.d9*cv_dts_dtu)
        real*8 cv_nanotu_minuts
        parameter (cv_nanotu_minuts=1.d0/cv_minuts_nanotu)