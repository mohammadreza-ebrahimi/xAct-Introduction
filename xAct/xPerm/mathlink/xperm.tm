/**********************************************************************
 *                                                                    *
 * Template file xperm.tm to link the xperm.c code to xPerm.nb        *
 *                                                                    *
 * Jose M. Martin-Garcia (C) 2003-2011                                *
 *                                                                    *
 * jose@xAct.es                                                       *
 * http://www.xAct.es/                                                *
 *                                                                    *
 **********************************************************************/

#include <mathlink.h>

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

/**********************************************************************
 *                            PACKAGE                                 *
 **********************************************************************/

#include "xperm.c"

/********************************************************************** 
 *                           INTERFACE                                *
 **********************************************************************/

/**********************************************************************/

void ML_schreier_sims P(( intp_nt, long, intp_nt, long, int ));

:Begin:
:Function:      ML_schreier_sims
:Pattern:       xAct`xPerm`Private`MLSchreierSims[ xAct`xPerm`Private`base_List, xAct`xPerm`Private`GS_List, xAct`xPerm`Private`n_Integer]
:Arguments:     {xAct`xPerm`Private`base, xAct`xPerm`Private`GS, xAct`xPerm`Private`n}
:ArgumentTypes: {IntegerList, IntegerList, Integer}
:ReturnType:    Manual
:End:

:Evaluate:

void ML_schreier_sims( int *base, long bl, int *GS, long m, int n) {

	int newbase[n];
	int nbl;
	int **newGS = NULL;
	int *pointer = NULL;
	int nm;
	int num=0;

	pointer = malloc(m*n*sizeof(int));
	newGS = &pointer;

	schreier_sims(base, bl, GS, m/n, n, 
		newbase, &nbl, newGS, &nm, &num);
	MLPutFunction(stdlink, "StrongGenSet", 3);
	MLPutIntegerList(stdlink, newbase, nbl);
	MLPutIntegerList(stdlink, *newGS, nm*n);
	MLPutInteger(stdlink, n);

	free(pointer);

	return;

}

/**********************************************************************/

void ML_orbit P(( int, intp_nt, long, int));

:Begin:
:Function:	ML_orbit
:Pattern:	xAct`xPerm`Private`MLOrbit[ xAct`xPerm`Private`point_Integer, xAct`xPerm`Private`GS_List, xAct`xPerm`Private`n_Integer]
:Arguments:	{xAct`xPerm`Private`point, xAct`xPerm`Private`GS, xAct`xPerm`Private`n}
:ArgumentTypes:	{Integer, IntegerList, Integer}
:ReturnType:	Manual
:End:

:Evaluate:

void ML_orbit( int point, int *GS, long m, int n) {

	int orbit[n];
	int ol;

	one_orbit(point, GS, m/n, n, orbit, &ol);
	MLPutIntegerList(stdlink, orbit, ol);
	return;
}

/**********************************************************************/

void ML_canonical_perm P(( 
        intp_nt, long, 
	int,
	int,
        intp_nt, long,
        intp_nt, long,
	intp_nt, long,
        intp_nt, long,
        intp_nt, long,
        intp_nt, long,
        intp_nt, long,
        intp_nt, long ));

:Begin:
:Function:      ML_canonical_perm
:Pattern:       xAct`xPerm`Private`MLCanonicalPerm[ 
        xAct`xPerm`Private`perm_List,
        xAct`xPerm`Private`deg_Integer,
        xAct`xPerm`Private`SGSQ_Integer,
        xAct`xPerm`Private`base_List,
        xAct`xPerm`Private`GS_List,
        xAct`xPerm`Private`freeps_List,
        xAct`xPerm`Private`vds_List,
        xAct`xPerm`Private`dummies_List,
        xAct`xPerm`Private`mQ_List,
        xAct`xPerm`Private`vrs_List,
        xAct`xPerm`Private`repes_List]
:Arguments:     { xAct`xPerm`Private`perm,
                  xAct`xPerm`Private`deg,
                  xAct`xPerm`Private`SGSQ,
                  xAct`xPerm`Private`base,
                  xAct`xPerm`Private`GS,
                  xAct`xPerm`Private`freeps,
                  xAct`xPerm`Private`vds,
                  xAct`xPerm`Private`dummies,
                  xAct`xPerm`Private`mQ,
                  xAct`xPerm`Private`vrs,
                  xAct`xPerm`Private`repes }
:ArgumentTypes: { IntegerList,
                  Integer,
                  Integer,
                  IntegerList,
                  IntegerList,
                  IntegerList,
                  IntegerList,
                  IntegerList,
                  IntegerList,
                  IntegerList,
                  IntegerList }
:ReturnType:    Manual
:End:

:Evaluate:

void ML_canonical_perm(
        int *perm, long nn,
        int deg,
	int SGSQ,
        int *base, long bl,
        int *GS, long m,
	int *freeps, long fl,
        int *vds, long vdsl,
        int *dummies, long dl,
        int *mQ, long mQl,
        int *vrs, long vrsl,
        int *repes, long rl) {

	int *cperm= (int*)malloc(nn*sizeof(int));
	int error;

	canonical_perm_ext(
                perm, deg,
                SGSQ, base, bl, GS, m/deg,
		freeps, fl,
                vds, vdsl, dummies, dl, mQ,
                vrs, vrsl, repes, rl, 
                cperm);

        error = MLError(stdlink);
	if(error) {
		MLPutFunction(stdlink, "Print", 1);
		MLPutString(stdlink, MLErrorMessage(stdlink));
	} else {
		MLPutFunction(stdlink, "xAct`xPerm`Private`ToSign", 2);
		MLPutFunction(stdlink, "Images", 1);
		MLPutIntegerList(stdlink, cperm, deg);
		MLPutInteger(stdlink, deg-2);
	}

	free(cperm);
	return;

}

/**********************************************************************/

void ML_set_stabilizer P(( 
        intp_nt, long, 
	int,
        intp_nt, long,
        intp_nt, long ));

:Begin:
:Function:      ML_set_stabilizer
:Pattern:       xAct`xPerm`Private`MLSetStabilizer[ 
        xAct`xPerm`Private`list_List,
        xAct`xPerm`Private`deg_Integer,
        xAct`xPerm`Private`base_List,
        xAct`xPerm`Private`GS_List]
:Arguments:     { xAct`xPerm`Private`list,
                  xAct`xPerm`Private`deg,
                  xAct`xPerm`Private`base,
                  xAct`xPerm`Private`GS }
:ArgumentTypes: { IntegerList,
                  Integer,
                  IntegerList,
                  IntegerList }
:ReturnType:    Manual
:End:

:Evaluate:

void ML_set_stabilizer(
        int *list, long nn,
        int n,
        int *base, long bl,
        int *GS, long m) {

        int num=0;
        int *charac=NULL, i;
        int *pointer=NULL;
        int **GSK=NULL, mK;
        pointer= malloc(m*sizeof(int));
        GSK = &pointer;
        charac= malloc(n*sizeof(int));

        /* Convert list of points into a characteristic function */
        zeros(charac,n);
        for(i=0; i<nn; i++) {
                charac[list[i]-1]=1;
        }

        /* Note that even though we send a characteristic function of
           n points, we send the length nn of the original list. We
           need both in the computations */
	search(base, bl, GS, m/n, n, 4, charac, nn, 1, GSK, &mK, &num);

	MLPutFunction(stdlink, "StrongGenSet", 3);
	MLPutIntegerList(stdlink, base, bl);
	MLPutIntegerList(stdlink, *GSK, mK*n);
	MLPutInteger(stdlink, n);

        free(pointer);
        free(charac);
        
	return;

}

/**********************************************************************/
/**********************************************************************/



void ML_basechangestabchain P(( 
	int,
	intp_nt, long,
	intp_nt, long,
	intp_nt, long ));

:Begin:
:Function:      ML_basechangestabchain
:Pattern:       xAct`xPerm`Private`MLBaseChangeStabilizerChain[ 
        xAct`xPerm`Private`deg_Integer,
        xAct`xPerm`Private`base_List,
        xAct`xPerm`Private`GS_List,
        xAct`xPerm`Private`newbase_List]
:Arguments:     { xAct`xPerm`Private`deg,
                  xAct`xPerm`Private`base,
                  xAct`xPerm`Private`GS,
			  xAct`xPerm`Private`newbase }
:ArgumentTypes: { Integer,
                  IntegerList,
                  IntegerList,
			  IntegerList }
:ReturnType:    Manual
:End:

:Evaluate:

void ML_basechangestabchain(
	int n,
	int *base, long bl,
	int *GS, long m,
	int *newbase, long nn) {
	
	int i, k;
	int *cl=NULL;
	int **chain=NULL;
	int *GSK=NULL;
	int nbl;
	int *nGS=NULL;
	int gslen=m/n;
	int *nbase = NULL;

	nbase = (int *)malloc(bl*sizeof(int));
	cl=(int *)malloc(bl*sizeof(int));
	chain=(int* *)malloc(bl*sizeof(int*));
	nGS=(int *)malloc(m*sizeof(int));

	nbl=bl;

	zeros(cl,nbl);

	for(i=0; i < nbl; i++) {
		chain[i]=NULL;
	}

	stab_chain(base, nbl, GS, gslen, n, chain, cl); 
	
	memmove(nbase, base, nbl*sizeof(int));
	memmove(nGS, GS, m*sizeof(int));
	
	basechange_chain(&nbase, &nbl, &nGS, &gslen, n, &chain, &cl, newbase, nn);

	GSK=(int *)malloc(gslen*n*sizeof(int));	

	MLPutFunction(stdlink, "List", nbl);

	for(i=0; i < nbl; i++) {
		for(k=0; k < cl[i]; k++) {
			memmove(&GSK[n*k], &nGS[n*(chain[i][k])], n*sizeof(int));
		}

		MLPutFunction(stdlink, "StrongGenSet", 3);
		MLPutIntegerList(stdlink, nbase+i, nbl-i);
		MLPutIntegerList(stdlink, GSK, n*cl[i]);
		MLPutInteger(stdlink, n);
	}

	for(i=0; i < nbl; i++) {
		free(chain[i]);
	}
	free(nbase);
	free(nGS);
	free(GSK);
	free(cl);
	free(chain);

	return;

}

void ML_basechange P(( 
	int,
	intp_nt, long,
	intp_nt, long,
	intp_nt, long ));

:Begin:
:Function:      ML_basechange
:Pattern:       xAct`xPerm`Private`MLBaseChange[ 
        xAct`xPerm`Private`deg_Integer,
        xAct`xPerm`Private`base_List,
        xAct`xPerm`Private`GS_List,
        xAct`xPerm`Private`newbase_List]
:Arguments:     { xAct`xPerm`Private`deg,
                  xAct`xPerm`Private`base,
                  xAct`xPerm`Private`GS,
			  xAct`xPerm`Private`newbase }
:ArgumentTypes: { Integer,
                  IntegerList,
                  IntegerList,
			  IntegerList }
:ReturnType:    Manual
:End:

:Evaluate:

void ML_basechange(
	int n,
	int *base, long bl,
	int *GS, long m,
	int *newbase, long nn) {
	
	int i, k;
	int *cl=NULL;
	int **chain=NULL;
	int *GSK=NULL;
	int nbl;
	int *nGS=NULL;
	int gslen=m/n;
	int *nbase = NULL;

	nbase = (int *)malloc(bl*sizeof(int));
	cl=(int *)malloc(bl*sizeof(int));
	chain=(int* *)malloc(bl*sizeof(int*));
	nGS=(int *)malloc(m*sizeof(int));

	nbl=bl;

	zeros(cl,bl);

	for(i=0; i < nbl; i++) {
		chain[i]=NULL;
	}

	stab_chain(base, nbl, GS, gslen, n, chain, cl); 
	
	memmove(nbase, base, bl*sizeof(int));
	memmove(nGS, GS, m*sizeof(int));
	
	basechange_chain(&nbase, &nbl, &nGS, &gslen, n, &chain, &cl, newbase, nn);

	GSK=(int *)malloc(gslen*n*sizeof(int));

	for(k=0; k < cl[0]; k++) {
		memmove(&GSK[n*k], &nGS[n*(chain[0][k])], n*sizeof(int));
	}
	MLPutFunction(stdlink, "StrongGenSet", 3);
	MLPutIntegerList(stdlink, nbase, nbl);
	MLPutIntegerList(stdlink, GSK, n*cl[0]);
	MLPutInteger(stdlink, n);
	

	for(i=0; i < nbl; i++) {
		free(chain[i]);
	}
	free(nbase);
	free(nGS);
	free(GSK);
	free(cl);
	free(chain);

}



/**********************************************************************/
void ML_stabsgs P(( 
	int,
	intp_nt, long,
	intp_nt, long,
	intp_nt, long ));

:Begin:
:Function:      ML_stabsgs
:Pattern:       xAct`xPerm`Private`MLStabilizerSGS[ 
        xAct`xPerm`Private`deg_Integer,
        xAct`xPerm`Private`base_List,
        xAct`xPerm`Private`GS_List,
        xAct`xPerm`Private`pts_List]
:Arguments:     { xAct`xPerm`Private`deg,
                  xAct`xPerm`Private`base,
                  xAct`xPerm`Private`GS,
			  xAct`xPerm`Private`pts }
:ArgumentTypes: { Integer,
                  IntegerList,
                  IntegerList,
			  IntegerList }
:ReturnType:    Manual
:End:

:Evaluate:

void ML_stabsgs(
	int n,
	int *base, long bl,
	int *GS, long m,
	int *pts, long ptsl) {
	
	int i, k;
	int *cl=NULL;
	int **chain=NULL;
	int *GSK=NULL;
	int nbl;
	int tmpbasel;
	int *tmpbase=NULL;
	int *nGS=NULL;
	int gslen=m/n;
	int *nbase = NULL;

	tmpbase= (int *)malloc((bl+ptsl)*sizeof(int));
	nbase = (int *)malloc(bl*sizeof(int));
	cl=(int *)malloc(bl*sizeof(int));
	chain=(int* *)malloc(bl*sizeof(int*));
	nGS=(int *)malloc(m*sizeof(int));

	nbl=bl;

	zeros(cl,bl);

	for(i=0; i < nbl; i++) {
		chain[i]=NULL;
	}

	stab_chain(base, nbl, GS, gslen, n, chain, cl); 
	
	memmove(nbase, base, bl*sizeof(int));
	memmove(nGS, GS, m*sizeof(int));
	
	/* We want to keep the order of the remaining base points  - updated 2014-09-24 */
	
	memmove(tmpbase, pts, ptsl*sizeof(int));
	tmpbasel=ptsl;
	
	for(k=0; k < bl; k++) {
		if(!position(base[k], pts, ptsl)){
			tmpbase[tmpbasel++]=base[k];
		};
	}
	
	basechange_chain(&nbase, &nbl, &nGS, &gslen, n, &chain, &cl, tmpbase, tmpbasel);

	GSK=(int *)malloc(gslen*n*sizeof(int));

	for(k=0; k < cl[ptsl]; k++) {
		memmove(&GSK[n*k], &nGS[n*(chain[ptsl][k])], n*sizeof(int));
	}
	MLPutFunction(stdlink, "StrongGenSet", 3);
	MLPutIntegerList(stdlink, &(nbase[ptsl]), nbl-ptsl);
	MLPutIntegerList(stdlink, GSK, n*cl[ptsl]);
	MLPutInteger(stdlink, n);
	

	for(i=0; i < nbl; i++) {
		free(chain[i]);
	}
	free(nbase);
	free(nGS);
	free(GSK);
	free(cl);
	free(chain);

}



/**********************************************************************/

/********************************************************************** 
 *                            COMPATIBILITY                           *
 **********************************************************************/

#if MACINTOSH_MATHLINK

int main( int argc, char* argv[])
{
	/* Due to a bug in some standard C libraries that have shipped with
	 * MPW, zero is passed to MLMain below.  (If you build this program
	 * as an MPW tool, you can change the zero to argc.)
	 */
	argc = argc; /* suppress warning */
	return MLMain( 0, argv);
}

#elif WINDOWS_MATHLINK

#if __BORLANDC__
#pragma argsused
#endif

int PASCAL WinMain( HINSTANCE hinstCurrent, HINSTANCE hinstPrevious, LPSTR lpszCmdLine, int nCmdShow)
{
	char  buff[512];
	char FAR * buff_start = buff;
	char FAR * argv[32];
	char FAR * FAR * argv_end = argv + 32;

    hinstPrevious = hinstPrevious; /* suppress warning */

	if( !MLInitializeIcon( hinstCurrent, nCmdShow)) return 1;
	MLScanString( argv, &argv_end, &lpszCmdLine, &buff_start);
	return MLMain( argv_end - argv, argv);
}

#else

int main(argc, argv)
	int argc; char* argv[];
{
	return MLMain(argc, argv);
}

#endif
