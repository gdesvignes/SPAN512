#include <mysql++/mysql++.h>
#include <getopt.h>
#include <stdio.h>
#include <time.h>
#include <vector>
#include <functional>
#include <iostream>
#include <fstream>
#include <cmath>
//#include <iomanp>

#include "database.h"
#include "slalib.h"

#define ARCSECTORAD 4.8481368110953599358991410235794797595635330237270e-6
#define RADTOARCSEC 206264.80624709635515647335733077861319665970087963
#define SECTORAD 7.2722052166430399038487115353692196393452995355905e-5
#define RADTOSEC 13750.987083139757010431557155385240879777313391975
#define RADTODEG 57.295779513082320876798154814105170332405472466564
#define DEGTORAD 1.7453292519943295769236907684886127134428718885417e-2
#define RADTOHRS 3.8197186342054880584532103209403446888270314977710
#define HRSTORAD 2.6179938779914943653855361527329190701643078328126e-1

// -- declarations of the fortran functions --
extern "C" {
    void fillsou_(float *, float *, int *);
    void fillmap_(float *, int *);
    void calpou_(int *, int *, int *, int &, int &, int &, int &, int &, int &, int &,
      double &, double &, double &, double &, double &, double &, double *,
      double *, double *, double *, double *, double *);
}

using namespace std;
using namespace mysqlpp;

void usage() {
    cout << "mkpointings - ask for survey beams"<<endl;
    cout << "Usage:    mkpointings [options]"<<endl;
    cout <<endl;
    cout << "Options:"<<endl;
    cout << "    -r, --ras=22:00:00.0     Select Right Ascension above this value"<<endl;
    cout << "    -R, --rae=02:00:00.0     Select Right Ascension under this value"<<endl;
    cout << "    -d, --decs=40:00:00.0    Select DEClination above this value"<<endl;
    cout << "    -D, --dece=50:00:00.0    Select DEClination under this value"<<endl;
    cout << "    -l, --ls=150.0           Select galactic longitude above this value"<<endl;
    cout << "    -L, --le=180.0           Select galactic longitude under this value"<<endl;
    cout << "    -b, --bs=3.5             Select galactic latitude above this value"<<endl;
    cout << "    -B, --be=5.0             Select galactic latitude under this value"<<endl;
    cout << "    -j, --jour=2012:01:24    Select the date of scheduled observations"<<endl;
    cout << "    -g, --grid=555555        Select a specific grid_id beam"<<endl;
    cout << "    -o, --observed           Add the already observed beams to the list"<<endl;
    cout << "    -a, --all                Ask for all possible pointings within the NRT range"<<endl;
    cout << "    -x, --doit               Do it"<<endl;
    cout << "    -v, --verbose            Debug mode"<<endl;

}

void split(const string& s, char c, vector<string>& v) {
    string::size_type i = 0;
    string::size_type j = s.find(c);
    while (j != string::npos) {
        v.push_back(s.substr(i, j-i));
        i = ++j;
        j = s.find(c, j);
        if (j == string::npos)
            v.push_back(s.substr(i, s.length( )));
    }
}

string space2underscore(string text) {
    for (unsigned i=0; i<text.length(); i++) {
        if (isspace(text[i])) text[i] = '_';
    }
    return text;
}    

float dms_to_deg(int deg, int min, float sec) {
    int sign;
    if (deg < 0.0) sign = -1;
    else if (deg==0.0 and (min < 0.0 or sec < 0.0)) sign = -1;
    else sign = 1;	
    return sign * ARCSECTORAD * RADTODEG * (60.0 * (60.0 * fabs(deg) + fabs(min)) + fabs(sec));
}

float hms_to_deg(int hour, int min, float sec) {
    int sign;
    if (hour < 0.0) sign = -1;
    else sign = 1;
    return sign * SECTORAD * RADTODEG * (60.0 * (60.0 * fabs(hour) + fabs(min)) + fabs(sec));
}

/*
 *
 */
int mksou(vector<int> obs_ids, vector<string> ra, vector<string> dec, string filename) {

    vector<string> ra_split, dec_split;

    // try to open the file
    FILE *pfo;
    pfo = fopen(filename.c_str(), "w");  // c_str convertit string en un pointeur

    // file opened	
    if (pfo!= NULL) {
        cout << "Writting sources to " << filename << endl;
	fprintf(pfo,"def=programme=PSRsv\n");
	fprintf(pfo,"def=code_coordonnees=1\n");
	fprintf(pfo,"def=annee_reference=2000\n");
	fprintf(pfo,"def=code_vitesses=5\n");
	for(unsigned ii=0; ii<obs_ids.size();ii++) {
	    // -- Split RA and DEC --
	    split(ra[ii], ':', ra_split);
	    split(dec[ii], ':', dec_split);

	    // DEBUG
	    //cout << ra[ii] << endl;
	    //for (int i = 0; i < ra_split.size( ); ++i) 
	    //      cout << ra_split[i] << '\n';
	    // END DEBUG
	    fprintf(pfo,"%s %s %s 0.0 0.0  %s %s %s 0.0 0.0 0.0 0.0 0.0 0.0 -45.0 0.0 SRV%06d\n",
	        ra_split[0].c_str(), ra_split[1].c_str(), ra_split[2].c_str(),
		dec_split[0].c_str(), dec_split[1].c_str(), dec_split[2].c_str(),obs_ids[ii]);
//	    fprintf(pfo,"source: SRV%06d  RA %s %s %s   DEC %s %s %s\n",
//	        obs_ids[ii], ra_split[0].c_str(), ra_split[1].c_str(), ra_split[2].c_str(),
//		dec_split[0].c_str(), dec_split[1].c_str(), dec_split[2].c_str());

	    ra_split.clear(); // Reset the vector	
	    dec_split.clear();	
	}
	fclose(pfo);
	return 1;
 	
    }else {
        cout << "Unable to open file" << filename << endl;
        return 0;
    }

}


int main(int argc, char *argv[]) {

    // Command line options
    static struct option long_opts[] = {
        {"ras", 1, NULL, 'r'},
        {"rae", 1, NULL, 'R'},
        {"decs", 1, NULL, 'd'},
        {"dece", 1, NULL, 'D'},
        {"ls", 1, NULL, 'l'},
        {"le", 1, NULL, 'L'},
        {"bs", 1, NULL, 'b'},
        {"be", 1, NULL, 'B'},
	{"jour", 1, NULL, 'j'},
	{"grid", 1, NULL, 'g'},
        {"all", 1, NULL, 'a'},
        {"observed", 1, NULL, 'o'},
        {"doit", 1, NULL, 'x'},
        {"verbose", 1, NULL, 'v'},
        {"help", 1, NULL, 'h'},
	{0,0,0,0}
    };	

    int iks,ikc,errorcode,stopcode;	// -- Index, counter, Error codes
    int jyr,jmn,jdy,jhr,jmi,jsc,jcc;	// -- Date and TStime to start observation
    float asdr,decl;			// -- R.A. and DEC. of target
    float duree=1080.0;			// -- Integration Time 18mnx60s=1080s --
    double xc,zc,yc,rc,xcc,ycc,incc;	// -- Receiver positions
    double tural,tupou,tutot;		// -- Durees des ralliement (tural), poursuite (tupou), duree totale (tutot)
    double rtsdman,rtsfpou,rtsfpouo;	// -- Temps Sideral de depart manip (rtsdman) et de fin de manip (rtsfpou)
    float rac,diffra;
    char command[256];
    FILE *pfo;
    int verbose=0;

    int requested_grid_id;
    int rah1=0, ram1=0, rah2=0, ram2=0;
    float ras1=0.0, ras2=0.0, ra1=0.0, ra2=0.0;
    int decd1, decm1, decd2, decm2;
    float decs1, decs2, dec1=0.0, dec2=0.0;
    float l1, l2, b1, b2, l_b_conditions=0;
    int jyrobs, jmnobs, jdyobs;
    bool ras_flag=false, rae_flag=false, decs_flag = false, dece_flag = false;
    bool all_grid=false, already_observed=false, doit=false, request_grid_id=false, gal_coordinates=false;
    bool VERBOSE = false;
    int opt, opt_indx, conditions=2;
    while ((opt=getopt_long(argc,argv,"r:R:d:D:l:L:b:B:j:g:aoxvh",long_opts,&opt_indx))!=-1) {
        switch(opt) {
	    case 'r':
		sscanf(optarg, "%d:%d:%f", &rah1, &ram1, &ras1);
		ra1 = hms_to_deg(rah1, ram1, ras1); // Convertit en degres
		ras_flag = true;
		conditions++;
		break;
	    case 'R':
		sscanf(optarg, "%d:%d:%f", &rah2, &ram2, &ras2);
		ra2 = hms_to_deg(rah2, ram2, ras2); // Convertit en degres
		rae_flag = true;
		conditions++;
		break;
	    case 'd':
		sscanf(optarg, "%d:%d:%f", &decd1, &decm1, &decs1);
		dec1 = dms_to_deg(decd1, decm1, decs1); // Convertit en degres
		decs_flag = true;
		conditions++;
		break;
	    case 'D':
		sscanf(optarg, "%d:%d:%f", &decd2, &decm2, &decs2);
		dec2 = dms_to_deg(decd2, decm2, decs2); // Convertit en degres
		dece_flag = true;
		conditions++;
		break;
	    case 'l':
		sscanf(optarg, "%f", &l1);
		l_b_conditions++;
		break;
	    case 'L':
		sscanf(optarg, "%f", &l2);
		l_b_conditions++;
		break;
	    case 'b':
		sscanf(optarg, "%f", &b1);
		l_b_conditions++;
		break;
	    case 'B':
		sscanf(optarg, "%f", &b2);
		l_b_conditions++;
		break;
	    case 'j':
		sscanf(optarg, "%d:%d:%d", &jyrobs, &jmnobs, &jdyobs);
		break;
	    case 'g':
		sscanf(optarg, "%d", &requested_grid_id);
		request_grid_id = true;
		break;
	    case 'a':
	        all_grid = true;
		conditions--;
		break;
	    case 'o':
	        already_observed = true;
		conditions--;
		break;
	    case 'x':
	        doit = true;
		break;
	    case 'v':
	        VERBOSE = true;
		verbose=1;
		break;
	    case 'h':
	        usage();
		exit(1);
		break;
	    default:
		usage();
		exit(1);
		break;
	}
    }	

    // -- Check the consistency of the galactic coordinates conditions --
    if (l_b_conditions) {
        if (l_b_conditions == 4) {
	    gal_coordinates = true;
	    if(decs_flag) {
	        decs_flag=false;
		conditions--;
            }
	    if(dece_flag) {
	        dece_flag=false;
		conditions--;
	    }	
	    conditions++;
	    printf("Requesting special galactic coordinates\n");
	} else {
	    printf("Missing one or more galactic coordinates (l-L-b-B) conditions\n");
	    exit(-1);
	}
    }	

    // Connect to the database.
    Connection conn(false);
    if (conn.connect(db, server, user, pass)) {
        if (conn.connected()) cout << "Connected to DB: " << db << endl;
    } 	
    else {
        cerr << "DB connection failed: " << conn.error() << endl;
        return 1;
    }

    ostringstream oss;
    // -- Query to the DB for non observed sources: obs_stat='n' --
    Query query = conn.query();
    string QUERY = "SELECT grid_id, right_ascension, declination, ra_deg, dec_deg FROM NRT_grid ";


    string CONDITIONS = "";
    // -- Construct the Conditions QUERY --
    // -- If we want a specific beam --
    if(request_grid_id) {
	oss.str(""); // Reset la chaine oss
	oss << requested_grid_id;
	CONDITIONS += "WHERE grid_id = " + oss.str();
    } else {	
        // -- If we specify a range and let the NRT decide... --
        if (conditions) {
	    CONDITIONS += "WHERE ";

   	    // -- We do not want all grid, restrict to the SPAN survey only --
	    if(!all_grid) {
	        CONDITIONS += "(is_SPAN=true OR is_SPAN2=true) ";
	        conditions--;
	        if (conditions) CONDITIONS += "AND ";
	    }

	    // -- RA inferior limit --
	    if(ras_flag) {
	        oss << ra1;
	        // -- trick to deal with continuous 23->2 hr in RA sessions --
	        if (ra2<ra1 && rae_flag) CONDITIONS += "(ra_deg > " + oss.str() + " ";
	        else CONDITIONS += "ra_deg > " + oss.str() + " ";
	        conditions--;
	        if (conditions) CONDITIONS += "AND ";
	    }

	    // -- RA superior limit --
	    if(rae_flag) {
	        oss.str(""); // Reset la chaine oss
	        oss << ra2;
	        // -- trick to deal with continous  23->2 hr in RA sessions --
	        if(ra2<ra1 && ras_flag) CONDITIONS += "ra_deg < 360.0 || ra_deg > 0.0 AND ra_deg < " + oss.str() + ") ";
	        else CONDITIONS += "ra_deg < " + oss.str() + " ";
	        conditions--;
	        if (conditions) CONDITIONS += "AND ";
	    }

	    // -- DEC inferior limit --
	    if(decs_flag) {
	        oss.str(""); // Reset la chaine oss
	        oss << dec1;
	        CONDITIONS += "dec_deg > " + oss.str() + " ";
	        conditions--;
	        if (conditions) CONDITIONS += "AND ";
	    }

	    // -- DEC superior limit --
	    if(dece_flag) {
	        oss.str(""); // Reset la chaine oss
	        oss << dec2;
	        CONDITIONS += "dec_deg < " + oss.str() + " ";
	        conditions--;
	        if (conditions) CONDITIONS += "AND ";
	    }

            // -- Ask for a longitude-latitude box --
	    if(gal_coordinates) {
	        double ra_tmp1, dec_tmp1;
	        double ra_tmp2, dec_tmp2;
	        slaGaleq(l1*DEGTORAD, b1*DEGTORAD, &ra_tmp1, &dec_tmp1);
	        slaGaleq(l2*DEGTORAD, b2*DEGTORAD, &ra_tmp2, &dec_tmp2);

	        oss.str(""); // Reset la chaine oss
	        if (dec_tmp1 < dec_tmp2) oss << dec_tmp1*RADTODEG;
		else oss << dec_tmp2*RADTODEG;
	        CONDITIONS += "dec_deg > " + oss.str() + " ";

	        slaGaleq(l2*DEGTORAD, b2*DEGTORAD, &ra_tmp2, &dec_tmp2);
	        oss.str(""); // Reset la chaine oss
	        if (dec_tmp1 < dec_tmp2) oss << dec_tmp2*RADTODEG;
		else oss << dec_tmp1*RADTODEG;
	        CONDITIONS += "AND dec_deg < " + oss.str() + " ";

	        conditions--;
	        if (conditions) CONDITIONS += "AND ";
	    }

	    // -- Include the already observed beams
	    if(!already_observed) {CONDITIONS +="grid_id NOT IN (SELECT grid_id FROM processing WHERE proc_stat='p' OR proc_stat='o' OR proc_stat='r' OR proc_stat='f' OR proc_stat='d' OR proc_stat='e') ";}
	}
    }	

    // -- Display the mysql query --
    cout << QUERY + CONDITIONS << endl;

    // -- Run the mysql query --
    query << QUERY + CONDITIONS;
    StoreQueryResult res = query.store();

    // -- Display results --
#if 1
    if (res) {
	for (size_t i = 0; i < res.num_rows(); i++) {
	    cout << "\t #"<< i<<" grid_id: " << res[i][0] << " " << res[i][1] << " " << res[i][2] << endl;
	}
    }
    // -- Error returned --
    else {
	cerr << "Failed to get item list: " << query.error() << endl;
	return -1;
    }
#endif

    // A utiliser plus tard pour marquer une observation comme plannifiee / mise au programme
    // -- Do query --
    // proc_stat: 'p' for plannified
    // 		  'o' will be observed
    // 		  'a' will be analysed
    // 		  'r' will be results available
    
    // -- Res structure
    // 0: grid_id
    // 1: ra (string)
    // 2: dec (string)
    // 3: ra (in degrees, float)
    // 4: dec (in degrees, float)
    
    // -- Do the selection stuff --
    unsigned ii;
    vector<int> obs_ids, grid_ids;
    vector<string> ra, dec; // Create an array of string
    float raf[70000],decf[70000];

    printf("%s> res.num_rows()= %lu\n",argv[0],res.num_rows());

    // Add a fake obs parameters (grid_id=0, ra="ra_test", dec="dec_test") to the 3 arrays
    //grid_ids.push_back(0);
    //ra.push_back("00:00:00.0");
    

    // -- If we requested a specific beam with known grid_id, don't use the NRT functions --
    if(request_grid_id && res.num_rows()==1) {
        grid_ids.push_back(res[0][0]);
        ra.push_back(res[0][1].c_str());
        dec.push_back(res[0][2].c_str());
    } else {

    for(size_t i=0;i<res.num_rows();i++) {
      //printf("%5lu %f %f\n",i,atof(res[i][3]),atof(res[i][4]));
      //raf[i]=(float)res[i][3]/15.0; decf[i]=res[i][4];
      raf[i]=atof(res[i][3])/15.0; decf[i]=atof(res[i][4]);
      // printf("%5lu %f %f\n",i,raf[i],decf[i]);
    }

    //asdr=atof(res[0][3])/15.0; decl=atof(res[0][4]);

    pfo = fopen("mkpointings.log", "w");  // Log de mkpointings
    ikc=0;
    // -- Default value for NRT positions
    yc=rc=xcc=ycc=0.0;
    xc=0.0; zc=0.0; incc=60000.0;
    rac=ra1/15.;
    jyr=jyrobs; jmn=jmnobs; jdy=jdyobs;
    printf("%s> Will start searching beam on %d %d %d within ra1= %.6f ra2= %.6f  and rac= %.6f\n",
      argv[0],jyr,jmn,jdy,ra1/15.0,ra2/15.0,rac);
    rtsfpouo=0.0;

    if(ra2<ra1) {
      ra2=ra2+360.0;
      printf("%s> We had to add 360 to ra2\n",argv[0]);
    }
    // -- LOOP WHILE ENOUGH REMAING TIME TO OBSERVE --
    while( (rac<(ra2/15.0-15.0/60.0)) &&(ikc<20)) {
     jhr=(int)(rac); jmi=(int)((rac-jhr)*60.0); jsc=0; jcc=0;
     // -- Choosing a beam --
     rac=rac+15.0/60.0;
     if(rac>24.0)rac-=24.0;
     printf("%s>\n%s> Will try a new observation with targeted RA rac= %.6f   for TS= %02dh%02d\n",argv[0],argv[0],rac,jhr,jmi); fflush(stdout);
     diffra=12.0; iks=-1;
     for(size_t i=0;i<res.num_rows();i++) {
       // printf("%d rac=%f raf[]=%f diffra=%f iks=%d\n",i,rac,raf[i],diffra,iks);
       if(fabsf(raf[i]-rac)<diffra) { diffra=fabsf(raf[i]-rac); iks=i; }
     }
     printf("%s> Doing NRT simulation for #%d asdr= %.6f decl= %.6f\n",argv[0],iks,raf[iks],decf[iks]);
     asdr=raf[iks]; decl=decf[iks];
#if 1
     // -- Initialize SOU and MAP structures
     printf("%s> Filling SOU/MAP with asdr= %.6f decl= %.6f duree= %.2f\n",argv[0],asdr,decl,duree);
     fillsou_(&asdr,&decl,&verbose); fillmap_(&duree,&verbose);
     // -- Calculate NRT POURSUITE
     printf("%s> Calling callpou...  %04d.%02d.%02d %02dh%02d\n",argv[0],jyr,jmn,jdy,jhr,jmi);
     calpou_(&verbose,&errorcode,&stopcode,jyr,jmn,jdy,jhr,jmi,jsc,jcc,
      xc,zc,yc,rc,xcc,ycc,&incc,&tural,&tupou,&tutot,&rtsdman,&rtsfpou);
     printf("%s> rtsdman= %.6lf rtsfpou= %.6lf  tural= %.6lf tupou= %.6lf tutot= %.6lf  new_incc= %.2lf\n",argv[0],
       rtsdman*24.0,rtsfpou*24.0,tural*24.0,tupou*24.0,tutot*24.0,incc);
     printf("%s> Ratio onSky/Total= %.3lf\n",argv[0],tupou/tutot);

     sprintf(command,"RA=%02dh%02d DEC=%02dd%02d Inc=%05d TS=%02dh%02d-%02dh%02d Posi=%02d:%02d Ratio_Sky/Tot=%.3lf\n",
       (int)asdr,(int)((asdr-(int)asdr)*60.0), (int)(decl),(int)((decl-(int)decl)*60.0),(int)incc,
       (int)(rtsdman*24.0),(int)((rtsdman*24.0-(int)(rtsdman*24.0))*60.0),
       (int)(rtsfpou*24.0),(int)((rtsfpou*24.0-(int)(rtsfpou*24.0))*60.0),
       (int)(tural*24.0*60.0),(int)((tural*24.0*60.0-(int)(tural*24.0*60.0))*60.0),tupou/tutot);
     if(pfo!=NULL)fputs(command,pfo);
#endif
#if 0
     sprintf(command,"./tstpoursuite %f %f %d %d %d %d %d %d %d %d",asdr,decl,jyr,jmn,jdy,jhr,jmi,(int)xc,(int)yc,(int)incc);
     printf("%s> Calling system \" %s \"\n",argv[0],command); fflush(stdout);
     system(command);
#endif
     ikc++;
     // -- Selected grid beam
     printf("Selected grid beam : grid_id= %d RA= %s = %.6f  DEC= %s = %.6f\n",0,res[iks][1].c_str(),asdr,res[iks][2].c_str(),decl);
     // Add an obs parameters (grid_id=0, ra="ra_test", dec="dec_test") to the 3 arrays
     grid_ids.push_back(res[iks][0]);
     ra.push_back(res[iks][1].c_str());
     dec.push_back(res[iks][2].c_str());

     if((tutot*24.0)>0.001) {
       rac=rtsfpou*24.0;
       printf("Changing rac= %.6f\n",rac);
       if((ikc>0) && (rtsfpou<rtsfpouo)) {
         printf("%s> Crossing the 24h00 limit\n",argv[0]);
         ra2=ra2-360.0;
       }
       rtsfpouo=rtsfpou;
     } else {
       printf("%s> Unable to simulate NRT observation... \n",argv[0]);
     }
     printf("%s> Will compare rac=%.6lf and ra2-15mn=%.6lf to decide to continue or not\n",argv[0],rac,ra2/15.0-15.0/60.0);
    }		// while(rac<(ra2/15.0-15.0/60.0))
    } // End of if(!request_grid_id)
    if(pfo!=NULL)fclose(pfo);

    // -- TODO Loop over the requested sources --
    for(ii=0; ii<grid_ids.size();ii++) {
	// Register the pointing to the database
	query << "INSERT INTO processing (grid_id, planned_date, proc_stat) VALUES ("<<grid_ids[ii]<<", NOW(), 'p')";

	if(doit) {
	    if (VERBOSE) cout << query << endl;
	    SimpleResult res2 = query.execute();
	    if(VERBOSE) {
	        if (res2) cout << "OK" << endl;
		else {
		    cerr << "Failed to get item list: " << query.error() << endl;
		    return -1;
  	        }
            }
	    query << "SELECT LAST_INSERT_ID() FROM processing";
	    res = query.store();   // Get the returned obs_id
	    int obs_id = atoi(res[0][0].c_str());
	    obs_ids.push_back(obs_id); // Append the returned obs_id to the obs_ids array

	    char QUERY2[128];
	    sprintf(QUERY2, "UPDATE processing SET pointing_name='SRV%06d' WHERE obs_id=%d", obs_id, obs_id);
	    if (VERBOSE) cout << QUERY2 << endl;
	    query << QUERY2;
	    res2 = query.execute();
	    if (VERBOSE && res2) cout << "OK" << endl;
	}
	else {
	    if (VERBOSE) cout << "DEBUG: "<< query <<endl;
	    cout << "WARNING : Observation not registered in the database. Fake source..." << endl;
	    obs_ids.push_back(999999);
	}    
    }

    // Create the filename source 
    time_t temp = time(NULL);
    struct tm date = *gmtime(&temp);
    string filename;
    // -- 1) Use current time ?
    oss.str(""); // Reset la chaine oss
    oss << asctime(&date) ;
    filename = oss.str(); // transform oss into a string for manipulation
    filename = space2underscore(filename)+"sources"; // replace blank by underscore

    // -- 2) Use the RA start - RA end?
    char shortfilename[64];
    sprintf(shortfilename, "%04d-%02d-%02d_%02dh%02d-%02dh%02d.sources", jyr, jmn, jdy, rah1, ram1, rah2, ram2);

    // -- 3) If we requested a specific grid_id
    if(request_grid_id) {
        sprintf(shortfilename, "grid_id-%d.source",requested_grid_id);
    }

    // Make the source
    filename = shortfilename;
    mksou(obs_ids, ra, dec, filename);
//    if(doit) {
      printf("------------- Content of mkpointings.log ----------------\n");
      sprintf(command,"cat mkpointings.log"); system(command);
      printf("------------- Content of mkpointings.log ----------------\n");
      printf("------------- Content of %s ----------------\n",filename.c_str());
      sprintf(command,"cat %s",filename.c_str()); system(command);
      printf("------------- Content of %s ----------------\n",filename.c_str());
      sprintf(command,"scp -p %s cognard@nrt:",filename.c_str()); system(command);
      printf(" Transfer of file %s to cognard@nrt\n",filename.c_str());
      printf(" Executing \" %s \"\n",command);
//    }


    // -- Close the connection --
    conn.disconnect();
    return 0;
}
