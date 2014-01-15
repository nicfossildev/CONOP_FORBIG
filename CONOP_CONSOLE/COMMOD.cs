using System;
//using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CONOP.NET
{
    public class COMMOD
    {
        #region Var. Members
        //---------------------------------------//
        //the limit of levels in a single section//
        //---------------------------------------// 
        public int MXLVL, MXLBL;
        public delegate int INTRAND();
        public int WTF, cutco;
        public int CONTRACT, COXSTF, FB4LF, INIGEN, NABRGEN;
        public int SOLVEF, ONEGRF, SLNF;
        public int JSTART, RUNGRF, TWOGRF, FNCGRF, FIXF, PAUSF, WALKF, GRIDF, GRID2F;
        public int MOVEF, STKF, HOMEF, NOTHR, MAXDIV;
        public int NTRIES, LSTRIES, CNTRIES, NBETR, NSTOP;
        public int NTRAJ, NCALIB, CALIBF, GRAPHF, TRJGRF, CHTGRF, CTRF;
        public int PENF, PEN2F, PENM, PEN2M;
        public int VIDEOF, COMPNO, COMPF, XEVNT, YEVNT, XPOSN, ESTART, LARGE;
        public int COXOUTF, EVTOUTF, SCTOUTF, FITOUTF, RUNOUTF, SEQOUTF, CONTF;
        public int INCOUTF, LOCOUTF, OBSOUTF, CMPOUTF, NEGATF, JSPANF, NEXCL, JEXCL;
        public int NREPS, NROWS, NOBS, NMIDS, NBOXS, NSCT, NSPC, NEVNT, TWOSPC;
        public int TERMF, NCOL;
        public int NFALA, MXFALA, NCOEX, MXCOEX;
        public int MXITAG, MXJTAG;
        public int COEXON, COEXOFF;
        public int NEWRANK;
        public int STOPF, AUTF, ADAF, CDF;
        public int NINNER;
        public double NOUTER;
        public double R, Rx, T, STARTT, KSM, KSQ, KSH, KTS, KMX, CPUTM;
        public double UU;

        //-----------------------------//
        //CPMS  the graphics variables //
        //-----------------------------//                               
        public int maxx, maxy, scl, top, btm, rit, lft;
        public double YSPAN, YROWS, XSPAN, XCOLS;

        //-------------------------------//
        //File names for input and output//
        //-------------------------------//     
        public string INFILE = string.Empty;
        public string RAWFILE = string.Empty;
        public string SCTFILE = string.Empty;
        public string EVTFILE = string.Empty;
        public string ETAGFILE = string.Empty;
        public string STAGFILE = string.Empty;
        public string SCTTAGS = string.Empty;
        public string EVTTAGS = string.Empty;
        public string LBLFILE = string.Empty;
        public string SLNFILE = string.Empty;
        public string TTLFILE = string.Empty;
        public string OUTMAIN = string.Empty;
        public string OUTSECT = string.Empty;
        public string OUTEVNT = string.Empty;
        public string ABFILE = string.Empty;
        public string ALBETFILE = string.Empty;
        public string DELTAFILE = string.Empty;
        public string BESTFILE = string.Empty;
        public string SCNDFILE = string.Empty;
        public string STEPSOL = string.Empty;
        public string STEPTMP = string.Empty;
        public string CMPSTFILE = string.Empty;
        public string COEXFILE = string.Empty;
        public string FB4LFILE = string.Empty;
        public string ORDRFILE = string.Empty;
        public string CNFGFILE = string.Empty;
        public string LOGFILE = string.Empty;
        public string CIRCFILE = string.Empty;
        public string INITSOL = string.Empty;
        public string BESTSOL = string.Empty;
        public string RUNLOG = string.Empty;
        public string TMPFILE = string.Empty;
        public string ASKFILE = string.Empty;
        public string USRFILE = string.Empty;
        public string PLOTFILE = string.Empty;
        public string PROJNAME = string.Empty, PROJTYP1 = string.Empty, PROJTYP2 = string.Empty;
        public string STAKNAME = string.Empty;


        //----------------------------------------//
        //storage for full section and event names//
        //as well as abbreviated nicknames        //
        //----------------------------------------//
        public string[] SECTNAME = null;
        public string[] SECTNICK = null;
        public string[] EVNTNAME = null;
        public string[] EVNTNICK = null;
        public string[] LABLNAME = null;
        public string[] ETAGNAME = null;
        public string[] STAGNAME = null;

        //---------------------------------------------------------------//
        //an array for the plotting order for sections                   //
        //an array for the JEXCL sections with exclusive taxa at the ends//
        //---------------------------------------------------------------//
        public int[] SECTPERM = null, EXCLSECT = null;


        //-------------------------------//
        //an array for section properties//
        //-------------------------------//
        public int[,] SECTPROP = null;


        //-------------------------------//
        //flags for "augmented" annealing//
        //-------------------------------//
        public bool NUDGUP, NUDGDN, NUDGER;


        //-------------------------------------//
        //flags for default options chosen in  //
        //response to unrecognized .CFG entries//
        //-------------------------------------//
        public bool DFLTPEN, DFLTCOMP, DFLTNABR, DFLTSTART;
        public bool DFLTWT, DFLTCOEX, DFLTFb4L;


        //----------------------------------------------------------//
        //"faster" version of RASCAL penalty which uses RASC() array//
        //----------------------------------------------------------//
        public bool RASCon;


        //----------------------------------//
        //counts the records of input events//
        //----------------------------------//
        public int IRCOUNT;

        public int[] PAIRJ = null, HLEVEL = null;
        public int[] RANGE = null, COVER = null;
        public int[,] LIMITS = null;
        public double[,] ZLEVEL = null;
        public int[,] FADLAD = null, PTLORD = null, COEXST = null, RASC = null;
        public int[,] NEGATIVE = null, ELEVEL = null;
        public int[,] MAPSCTLVL = null;

        public int[,] IROWS = null;

        public int[, ,] ISTATIC = null;

        public double[, ,] RSTATIC = null;

        public double[,] VALEVEL = null, COMPLVL = null;

        public double[,] EVERYBST = null, SECNDBST = null, CONFGRID = null;

        public int[,] CULLIST = null;

        public int[] PERM = null, INIPERM = null, PXLPERM = null, TEMPERM = null, BSTPERM = null, LSTPERM = null;

        public double BSTPEN, BS2PEN, NXTPEN, PEN, TOOPEN, TO2PEN, MAXPEN, SMPEN, SQPEN,
        SHPEN, INTPEN, ORDPEN, LVLPEN, RSCPEN, ROYPEN, SEQPEN, SPTPEN, INIPEN,
        SPANPEN, VERYBEST, USERBEST, TWINBEST, LCLBST, SQBST, SHBST, TSPEN,
        BSTPLUS, TSBST, SQLST, SHLST, TSLST, NGTPEN, NGHPEN, AUTPEN, NEAR,
        ASPNPEN, RVLPEN, MOMPEN, CRTPEN;

        public double[] SCJPEN = null, COLPEN = null;

        public double[,] EVTPEN = null;

        public int[,] HSCTSOL = null, SCTSOL = null, BSCTSOL = null;

        public int[, ,] HSCTRNG = null;

        //for rarefaction//
        public double Erare, Srare, up95, dn95;
        public double[] IN = null;//individuals in ith species

        //***************THIS IS VERY IMPORTANT!*******************/
        internal Random RANDOM = new Random(DateTime.Now.Millisecond);
        //*********************************************************/

        //for output
        internal StringBuilder OutmainSB = new StringBuilder();
        internal StringBuilder BestSolnSB = new StringBuilder();
        internal StringBuilder DeltaSB = new StringBuilder();
        internal StringBuilder AlbetSB = new StringBuilder();
        internal StringBuilder AbSB = new StringBuilder();
        internal StringBuilder CmpstSB = new StringBuilder();
        internal StringBuilder TtlSB = new StringBuilder();
        internal StringBuilder SlnSB = new StringBuilder();

        public bool DEBUG_MODE = false;

        #endregion

        #region Singleton

        private static COMMOD g_singleton = null;
        private COMMOD() { }
        public static COMMOD Singleton()
        {
            if (g_singleton == null)
                g_singleton = new COMMOD();

            return g_singleton;
        }

        #endregion
    }
}
