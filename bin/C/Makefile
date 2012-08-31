mkpointings:	mkpointings.C
	g++ -O2 -Wall -o mkpointings mkpointings.C -I/usr/include/mysql -lmysqlpp

mkpointings2:	mkpointings.C
	gfortran -ffixed-line-length-132 -fno-range-check -c calpou.for astrolib.for libcvtime.for libcalpou.for fillsou.for fillmap.for dumpou.for
	g++ -c -O2 -Wall mkpointings.C -I/usr/include/mysql -lmysqlpp
#	g++ -o mkpointings mkpointings.o calpou.o astrolib.o libcvtime.o libcalpou.o fillsou.o fillmap.o dumpou.o -I/usr/include/mysql -lmysqlpp -lgfortran
	g++ -O2 -Wall -o mkpointings mkpointings.o calpou.o astrolib.o libcvtime.o libcalpou.o fillsou.o fillmap.o dumpou.o -I/usr/include/mysql -lmysqlpp -lgfortran

mltpoursuite:	mltpoursuite.c
	gfortran -ffixed-line-length-132 -fno-range-check -c calpou.for astrolib.for libcvtime.for libcalpou.for fillsou.for fillmap.for dumpou.for
	gcc -c mltpoursuite.c
	gfortran -o mltpoursuite mltpoursuite.o calpou.o astrolib.o libcvtime.o libcalpou.o fillsou.o fillmap.o dumpou.o

tstpoursuite:	tstpoursuite.c
	gfortran -ffixed-line-length-132 -fno-range-check -c calpou.for astrolib.for libcvtime.for libcalpou.for fillsou.for fillmap.for dumpou.for
	gcc -c tstpoursuite.c
	gfortran -o tstpoursuite tstpoursuite.o calpou.o astrolib.o libcvtime.o libcalpou.o fillsou.o fillmap.o dumpou.o

clean:
	rm mkpointings 
