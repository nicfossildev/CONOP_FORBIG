using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CONOP.NET
{
    //**********************************************************************
    // 
    // Sets up run variables in two stages 
    //  1: sets defaults 
    //  2: reads in setup parameters from a configuration file
    //
    //        PROGRAMMERS: PETE SADLER 
    //        LAST UPDATE: August 27th 2007
    //
    //        REWRITED BY: XUDONG HOU  [MAR. 2013]   
    //                                             
    //cpms********************************************************************
    public class PREPARE:IRunIt
    {
        #region Var.

#if SEC7
        public const string CONOPNET_CNFGFILE = @"sec7-setting/CONOP9.CFG";
#elif SEC20
        public const string CONOPNET_CNFGFILE = @"sec20-setting/CONOP9.CFG";
#elif SEC19
        public const string CONOPNET_CNFGFILE = @"sec19-setting/CONOP9.CFG";
#elif SEC50
        public const string CONOPNET_CNFGFILE = @"sec50-setting/CONOP9.CFG";
#elif SEC195
        public const string CONOPNET_CNFGFILE = @"sec195-setting/CONOP9.CFG";
#elif SEC286
        public const string CONOPNET_CNFGFILE = @"sec286-setting/CONOP9.CFG";
#elif SEC287
        public const string CONOPNET_CNFGFILE = @"sec287-setting/CONOP9.CFG";
#else
        public const string CONOPNET_CNFGFILE = "CONOP9.CFG";
#endif

        private Dictionary<string, Dictionary<string, object>> CONOPNET_CNF_NAMELIST = null;

        public string UPPER;

        private int SECTIONS, TAXA, EVENTS, MAX_LEVELS, MAX_LABELS, STEPS, TRIALS,
                    STARTSECT, COLUMNS, COMPOSNMBR, STARTEVENT;
        private string COEX_OUT, EVNT_OUT, SECT_OUT, FITS_OUT, CNFG_OUT,
                       SEQN_OUT, INCR_OUT, LOC_OUT, OBS_OUT, COMP_OUT;
        private double STARTEMP, RATIO, SMOOTHER, SQUEEZER,
                       SHRINKER, TEASER, BESTKNOWN, NEARENOUGH;
        private string LOADFILE, SECTFILE, EVENTFILE, UNLOADMAIN, UNLOADSECT,
               UNLOADEVNT, ORDERFILE, RUNLOGFILE, BESTARTFILE, LABELFILE,
               SECTTAGFILE, SECTTAGS, EVENTTAGFILE, EVENTTAGS,
               STARTFILE, SOLNLIST, PREPFILE, OBSDFILE, PLCDFILE, STEPFILE,
               EXTNFILE, CURVFILE, CRV2FILE,
               COEXISTFILE, FAD_LADFILE, COMPOSFILE, CULLFILE;
        private string PROJECT;
        private string HOODSIZE, STARTYPE, SOLVER, PENALTY, VIDEOMODE;
        private string WEIGHTING, PAUSES;
        private string FORCECOEX, FORCEFB4L, LETCONTRACT,
               STACKER, SHOWMOVIES, TRAJECTORY, COMPOSTYPE,
               USENEGATIVE, HOMERANGE, EXCLUSIVES;

        #endregion

        #region Singleton

        private static PREPARE g_singleton = null;
        private PREPARE() { }
        public static PREPARE Singleton()
        {
            if (g_singleton == null)
                g_singleton = new PREPARE();

            return g_singleton;
        }
        #endregion

        public void RunIt()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            COMMOD9.CNFGFILE = CONOPNET_CNFGFILE;
            COMMOD9.PLOTFILE = "PlotData.txt";
            //*******************************************
            //initialize some COMMOD names
            COMMOD9.PROJNAME = "      ";
            COMMOD9.PROJTYP1 = "      ";
            COMMOD9.PROJTYP2 = "      ";
            COMMOD9.NINNER = 0;
            COMMOD9.NOUTER = 0;
            COMMOD9.DFLTPEN = false;
            COMMOD9.DFLTCOMP = false;
            COMMOD9.DFLTNABR = false;
            COMMOD9.DFLTSTART = false;
            COMMOD9.DFLTWT = false;
            COMMOD9.DFLTCOEX = false;
            COMMOD9.DFLTFb4L = false;
            COMMOD9.RASCon = false;

            //more initializations for Intel 11.1
            COMMOD9.NSCT = 0;
            COMMOD9.NEVNT = 0;
            COMMOD9.NSPC = 0;
            COMMOD9.NOTHR = 0;
            //----------------------------------------------------------------------
            //   "LARGE" IS A MULTIPLIER TO DISCOURAGE RANGE CONTRACTIONS
            COMMOD9.LARGE = 7777;
            //----------------------------------------------------------------------
            COMMOD9.GRAPHF = 1;
            COMMOD9.RUNGRF = 2;
            //----------------------------------------------------------------------
            COMMOD9.TERMF = 1;
            //-----not running control9.exe:
            COMMOD9.CALIBF = 0;

            //read in the run-time parameters:

            if (!File.Exists(COMMOD9.CNFGFILE))
            {
                Helper.Write(" ABORTING: Unable to find {0}\n", COMMOD9.CNFGFILE);
                Helper.Write(" Should be in same directory as CONOP.NET.EXE\n");
                COMMOD9.STOPF = 1;
                goto Label9999;
            }          

            InitConfigNamelist();
            ReadConfigNamelist();
            

            //set the program flags accordingly            
            COMMOD9.NEAR = NEARENOUGH;
            COMMOD9.NSCT = SECTIONS;
            COMMOD9.NSPC = TAXA;
            COMMOD9.MAXDIV = COMMOD9.NSPC;
            COMMOD9.TWOSPC = 2 * TAXA;
            COMMOD9.MXCOEX = (COMMOD9.NSPC * (COMMOD9.NSPC - 1)) / 2;
            COMMOD9.MXFALA = (COMMOD9.NSPC * COMMOD9.NSPC);
            COMMOD9.NEVNT = EVENTS + (2 * TAXA);
            COMMOD9.NOTHR = EVENTS;
            COMMOD9.MXLVL = MAX_LEVELS;
            COMMOD9.MXLBL = MAX_LABELS;
            COMMOD9.NSTOP = STEPS;
            COMMOD9.NOUTER = STEPS;
            COMMOD9.NINNER = TRIALS;
            COMMOD9.NTRIES = 0;
            COMMOD9.NTRAJ = 1;

            if (COMMOD9.NTRAJ > COMMOD9.NINNER)
                COMMOD9.NTRAJ = COMMOD9.NINNER;

            if ((STARTSECT > 0) && (STARTSECT <= COMMOD9.NSCT))//[1,NSCT] IN FORTRAN,BUT [0,NSCT-1] IN C#
                COMMOD9.JSTART = STARTSECT-1;//<=HXD
            else
                COMMOD9.JSTART = 0;//<=HXD 

            //find the start event
            if ((STARTEVENT > 0) && (STARTEVENT <= COMMOD9.NEVNT))
            {
                COMMOD9.ESTART = STARTEVENT - 1;//<=HXD
                COMMOD9.XEVNT = STARTEVENT - 1;//<=HXD
            }
            else
            {
              
                COMMOD9.XEVNT = COMMOD9.RANDOM.Next(COMMOD9.NEVNT);//<=HXD  HACK:NOT SURE == INTRAND(NEVNT)
                COMMOD9.ESTART = -1;//<=HXD
            }

            //cannot be negative!
            COMMOD9.STARTT = STARTEMP;
            if (COMMOD9.STARTT < 0.0) COMMOD9.STARTT = 0.0;

            COMMOD9.R = 0.98;
            if (RATIO <= 1.00) COMMOD9.R = RATIO;

            COMMOD9.Rx = COMMOD9.R;

            COMMOD9.KMX = 100000000.0;
            COMMOD9.KTS = Math.Max(0.0, TEASER);

            if (COMMOD9.KTS > 0.0) COMMOD9.KMX = Math.Min(COMMOD9.KMX, COMMOD9.KTS);

            COMMOD9.KSM = Math.Max(0.0, SMOOTHER);

            if (COMMOD9.KSM > 0.0) COMMOD9.KMX = Math.Min(COMMOD9.KMX, COMMOD9.KSM);

            COMMOD9.KSQ = Math.Max(0.0, SQUEEZER);

            if (COMMOD9.KSQ > 0.0) COMMOD9.KMX = Math.Min(COMMOD9.KMX, COMMOD9.KSQ);

            COMMOD9.KSH = Math.Max(0.0, SHRINKER);

            if (COMMOD9.KSH > 0.0) COMMOD9.KMX = Math.Min(COMMOD9.KMX, COMMOD9.KSH);
            if (COMMOD9.KMX == 100000000.0) COMMOD9.KMX = 1.0;

            //----------------------------------------------------
            COMMOD9.INFILE = LOADFILE;
            COMMOD9.RAWFILE = PREPFILE;
            COMMOD9.SCTFILE = SECTFILE.ToUpper();
            COMMOD9.STAGFILE = SECTTAGFILE;
            COMMOD9.SCTTAGS = SECTTAGS;
            if (COMMOD9.SCTFILE.Substring(0, 3) == "off") COMMOD9.SCTFILE = "OFF" + COMMOD9.SCTFILE.Substring(3);

            COMMOD9.USERBEST = BESTKNOWN;
            COMMOD9.LBLFILE = LABELFILE.ToUpper();

            if (COMMOD9.LBLFILE.Substring(0, 3) == "off") COMMOD9.LBLFILE = "OFF" + COMMOD9.LBLFILE.Substring(3);

            COMMOD9.EVTFILE = EVENTFILE;
            COMMOD9.ETAGFILE = EVENTTAGFILE;
            COMMOD9.EVTTAGS = EVENTTAGS;
            COMMOD9.INITSOL = STARTFILE;
            COMMOD9.STEPSOL = STEPFILE.ToUpper();
            COMMOD9.STEPTMP = "Steptemp.dat";

            if (COMMOD9.STEPSOL.Substring(0, 3) == "off") COMMOD9.STEPSOL = "OFF" + COMMOD9.STEPSOL.Substring(3);

            COMMOD9.BESTSOL = BESTARTFILE;
            COMMOD9.OUTMAIN = UNLOADMAIN;
            COMMOD9.OUTSECT = UNLOADSECT;
            COMMOD9.OUTEVNT = UNLOADEVNT;

            switch (EXCLUSIVES.ToUpper().Trim())
            {
                case "yes":
                case "YES":
                case "ON":
                case "on":
                    COMMOD9.JSPANF = 1;
                    break;
                case "no":
                case "NO":
                case "OFF":
                case "off":
                    COMMOD9.JSPANF = 0;
                    break;
                default:
                    COMMOD9.JSPANF = 0;
                    break;
            }

            //TODO:1
            /*
              SELECT CASE (TRIM(UPPER(VIDEOMODE)))
	          CASE('VGA','vga','EGA','ega','CGA','cga')
	            VIDEOF=1
	          CASE('SVGA_FULL','svga_full')
	            VIDEOF=2
	          CASE('SVGA_CLIP','svga_clip')
	            VIDEOF=3
	          CASE('XVGA','xvga')
	            VIDEOF=4
	          CASE DEFAULT
	            VIDEOF=1
	          END SELECT
            */

            //Turn on the solution file       
            COMMOD9.SLNF = 1;
            //turn off the solution file
            if (SOLNLIST.Substring(0, 3).ToUpper() == "OFF") COMMOD9.SLNF = 0;//HACK: NOT USE SLNF

            //create the file name
            if ((SOLNLIST.Substring(0, 3).ToUpper() == "OFF") ||
             (SOLNLIST.Substring(0, 3).ToUpper() == "NEW"))
            {
                COMMOD9.SLNFILE = SOLNLIST.Substring(3);
                //also build ttlfile from same base            
                COMMOD9.TTLFILE = SOLNLIST.Substring(3, SOLNLIST.IndexOf(".") - 3 + 1) + "ttl";
            }
            else
            {
                COMMOD9.SLNFILE = SOLNLIST;
                //also build ttlfile             
                COMMOD9.TTLFILE = SOLNLIST.Substring(0, SOLNLIST.IndexOf(".") + 1) + "ttl";
            }

            //wipe the old file and delete it
            //the file may not be empty;  DOUTPUT will create a new file if
            //needed but if the file exists it must be able to read records
            //to determine the next section number
            if (SOLNLIST.Substring(0, 3).ToUpper() == "NEW")
            {
                File.Open(COMMOD9.SLNFILE, FileMode.Create).Close();
                File.Open(COMMOD9.TTLFILE, FileMode.Create).Close();
                
            }


            COMMOD9.NCOL = 7;
            COMMOD9.CDF = 0;
            if (COLUMNS > 0) COMMOD9.NCOL = COLUMNS;

            if (COLUMNS <= 0) COMMOD9.CDF = 1;

            COMMOD9.ABFILE = OBSDFILE;
            COMMOD9.ALBETFILE = PLCDFILE;
            COMMOD9.DELTAFILE = EXTNFILE;
            COMMOD9.GRIDF = 0;

            //don't allow curvfile OFF switch
            //it causes runtime errors
            if (CURVFILE.Substring(0, 3).ToUpper() == "OFF") CURVFILE = CURVFILE.Substring(3);

            COMMOD9.BESTFILE = CURVFILE;

            if (COMMOD9.BESTFILE.Substring(0, 3).ToUpper() != "OFF") COMMOD9.GRIDF = 1;

            //don't allow crv2file OFF switch
            if (CRV2FILE.Substring(0, 3).ToUpper() == "OFF") CRV2FILE = CRV2FILE.Substring(3);

            COMMOD9.SCNDFILE = CRV2FILE;
            COMMOD9.CMPSTFILE = COMPOSFILE;
            COMMOD9.COMPNO = COMPOSNMBR;
            if (COMMOD9.COMPNO == 0) COMMOD9.SLNF = 0;

            //abbreviations
            //STANDARD - STD ST S
            //MAXIMUM  - MAX MX M X
            //AVERAGE  - AVG AV A V
            //ZERO     - ZRO Z
            //Z-SCORE  - Z
            //MINIMUM  - MIN
            switch ((COMPOSTYPE.ToUpper().Trim()))
            {
                case "ORD":
                case "ord":
                    COMMOD9.COMPF = 1;
                    break;
                case "STD":
                case "std":
                case "AST":
                case "AVS":
                    COMMOD9.COMPF = 2;
                    break;
                case "ZTD":
                case "ztd":
                case "ZST":
                case "zst":
                case "ZAS":
                case "zas":
                    COMMOD9.COMPF = 3;
                    break;
                case "MAX":
                case "max":
                    COMMOD9.COMPF = 4;
                    break;
                case "ZMX":
                case "zmx":
                case "Zmx":
                    COMMOD9.COMPF = 5;
                    break;
                case "AVG":
                case "avg":
                    COMMOD9.COMPF = 6;
                    break;

                case "MIN":
                case "min":
                    COMMOD9.COMPF = 7;
                    break;
                case "ZAV":
                case "zav":
                case "NIL":
                case "nil":
                case "ZRO":
                case "zro":
                case "ZMN":
                case "zmn":
                    COMMOD9.COMPF = 8;
                    break;
                case "MST":
                case "mst":
                case "XST":
                case "Xst":
                case "xst":
                case "MXS":
                case "mxs":
                    COMMOD9.COMPF = 9;
                    break;
                case "ZXS":
                case "zxs":
                case "ZMS":
                case "zms":
                    COMMOD9.COMPF = 10;
                    break;
                default:
                    COMMOD9.DFLTCOMP = true;
                    COMMOD9.COMPF = 3;
                    break;
            }

            COMMOD9.COEXFILE = COEXISTFILE;
            COMMOD9.FB4LFILE = FAD_LADFILE;
            COMMOD9.ORDRFILE = ORDERFILE;
            COMMOD9.RUNLOG = RUNLOGFILE;
            COMMOD9.CIRCFILE = CULLFILE;
            COMMOD9.PROJNAME = PROJECT;
            COMMOD9.PROJTYP1 = PENALTY.Trim() + "  COEX=" + FORCECOEX.Trim() +
                    "  LTCTRCT=" + LETCONTRACT.Trim();
            COMMOD9.PROJTYP2 = SOLVER.Trim() + "  " + HOODSIZE.Trim();



            COMMOD9.NABRGEN = 1;
            switch (HOODSIZE.ToUpper().Trim())
            {
                case "BIG":
                case "big":
                    COMMOD9.NABRGEN = 1;
                    break;
                case "SMALL":
                case "small":
                case "REOPT"://REOPT removed from version 6.0
                case "reopt":
                    COMMOD9.NABRGEN = 2;
                    break;
                case "DOUBLE":
                case "double":
                    COMMOD9.NABRGEN = 3;
                    break;
                default:
                    COMMOD9.DFLTNABR = true;
                    COMMOD9.NABRGEN = 1;
                    break;
            }

            COMMOD9.CONTF = 0;
            switch (STARTYPE.ToUpper().Trim())
            {
                case "SECT":
                case "sect":
                    //start from one section
                    COMMOD9.INIGEN = 1;
                    break;
                case "FILE":
                case "file":
                    //start from the STARTFILE
                    COMMOD9.INIGEN = 2;
                    break;
                case "RAND":
                case "rand":
                    //start from a random sequence
                    COMMOD9.INIGEN = 3;
                    break;
                case "BEST":
                case "best":
                    //start from the BESTFILE
                    COMMOD9.INIGEN = 4;
                    break;
                case "STEP":
                case "step":
                    //start from the STEPFILE
                    COMMOD9.INIGEN = 5;
                    break;
                case "CONT":
                case "cont":
                    COMMOD9.CONTF = 1;
                    COMMOD9.INIGEN = 5;
                    break;
                case "PART":
                case "part":
                    COMMOD9.INIGEN = 6;
                    break;
                default:
                    COMMOD9.DFLTSTART = true;
                    COMMOD9.INIGEN = 3;
                    break;
            }

            //until a section start with unpaired events
            //can be programmed, STARTYPE must be over-ridden!
            //'PART' should be a file that runs like 'SECT'
            //it only fails if it does not contain all unpaired events
            if ((EVENTS > 0) && ((STARTYPE == "SECT") || (STARTYPE == "sect"))) COMMOD9.INIGEN = 3;

            COMMOD9.WTF = 1;
            COMMOD9.cutco = 0;
            switch (WEIGHTING.ToUpper().Trim())
            {
                case "OFF":
                case "off":
                    COMMOD9.WTF = 0;
                    break;
                case "ON":
                case "FILE":
                case "on":
                case "file":
                    COMMOD9.WTF = 1;
                    break;
                case "COEX":
                case "coex":
                case "COEX^-1":
                case "coex^-1":
                    COMMOD9.WTF = 2;
                    break;
                case "COEX2":
                case "coex2":
                case "COEX^-2":
                case "coex^-2":
                    COMMOD9.WTF = 22;
                    break;
                case "COEX3":
                case "coex3":
                case "COEX^-3":
                case "coex^-3":
                    COMMOD9.WTF = 23;
                    break;
                case "COEX%1":
                case "coex%1":
                    COMMOD9.WTF = 3;
                    COMMOD9.cutco = 1;
                    break;
                case "COEX%2":
                case "coex%2":
                    COMMOD9.WTF = 3;
                    COMMOD9.cutco = 2;
                    break;
                case "COEX%5":
                case "coex%5":
                    COMMOD9.WTF = 3;
                    COMMOD9.cutco = 5;
                    break;
                case "COEX%10":
                case "coex%10":
                    COMMOD9.WTF = 3;
                    COMMOD9.cutco = 10;
                    break;
                case "COEX%15":
                case "coex%15":
                    COMMOD9.WTF = 3;
                    COMMOD9.cutco = 15;
                    break;
                case "COEX%20":
                case "coex%20":
                    COMMOD9.WTF = 3;
                    COMMOD9.cutco = 20;
                    break;
                default:
                    COMMOD9.DFLTWT = true;
                    break;
            }

            switch (FORCECOEX.ToUpper().Trim())
            {
                case "OFF":
                case "off":
                    COMMOD9.COXSTF = 4;
                    break;
                case "SS":
                case "ss":
                    COMMOD9.COXSTF = 3;
                    break;
                case "SL":
                case "sl":
                case "ON":
                case "on":
                    COMMOD9.COXSTF = 2;
                    break;
                case "ALL":
                case "all":
                    COMMOD9.COXSTF = 0;
                    break;
                default:
                    COMMOD9.DFLTCOEX = true;
                    COMMOD9.COXSTF = 2;
                    break;
            }
            if (COMMOD9.NSPC == 0) COMMOD9.COXSTF = 4;

            switch (FORCEFB4L.ToUpper().Trim())
            {
                case "OFF":
                case "off":
                case "no":
                case "NO":
                case "N":
                case "n":
                    //causes SEQUEL to crash (division by 0)
                    //and will be over-ridden later as necessary
                    COMMOD9.FB4LF = 0;
                    break;
                case "ON":
                case "on":
                case "yes":
                case "YES":
                case "y":
                case "Y":
                case "SS":
                case "ss":
                    //CONOP takes its SS/SL cues from COXSTF
                    //SS is the default for Fb4L
                    COMMOD9.FB4LF = 1;
                    break;
                case "SL":
                case "sl":
                    //CONOP takes its SS/SL cues from COXSTF
                    //COXSTF needs to be set to its SL default
                    //then Fb4L adds the dditional coexistences if FAD and LAD are at same level
                    COMMOD9.FB4LF = 1;
                    COMMOD9.COXSTF = 2;//sensu lato
                    break;
                case "FILE":
                case "file":
                case "DISK":
                case "disk":
                    COMMOD9.FB4LF = 2;
                    break;
                default:
                    COMMOD9.DFLTFb4L = true;
                    COMMOD9.FB4LF = 0;
                    break;
            }
            if (COMMOD9.NSPC == 0) COMMOD9.FB4LF = 0;


            switch (LETCONTRACT.ToUpper().Trim())
            {
                case "SL":
                case "sl":
                case "ON":
                case "on":
                    COMMOD9.CONTRACT = 3;
                    break;
                case "SS":
                case "ss":
                    COMMOD9.CONTRACT = 2;
                    break;
                case "FILE":
                case "file":
                    COMMOD9.CONTRACT = 1;
                    break;
                default:
                    COMMOD9.CONTRACT = 0;
                    break;
            }


            switch (USENEGATIVE.ToUpper().Trim())
            {
                case "SS":
                case "ss":
                case "ON":       
                case "on":
                    COMMOD9.NEGATF = 1;
                    break;
                case "SL":
                case "sl":
                case "COEX":
                case "coex":
                    COMMOD9.NEGATF = 2;
                    break;
                case "OFF":
                case "off":
                    COMMOD9.NEGATF = 0;
                    break;
                default:
                    COMMOD9.NEGATF = 0;
                    break;
            }


            COMMOD9.HOMEF = 1;
            switch (HOMERANGE.ToUpper().Trim())
            {
                case "SS":
                case "ss":
                    COMMOD9.HOMEF = 1;
                    break;
                case "SL":
                case "sl":
                case "COEX":
                case "coex":
                    COMMOD9.HOMEF = 2;
                    break;
                default:
                    break;
            }


            switch (SOLVER.ToUpper().Trim())
            {
                //keep the cases that use a secondary penalty
                //in a group at the large values of SOLVEF
                //then they can be flagged by one .gt. call
                case "anneal":
                case "ANNEAL":
                case "reopt":
                case "REOPT":
                    COMMOD9.SOLVEF = 0;
                    break;
                case "annudg":
                case "ANNUDG":
                    COMMOD9.SOLVEF = 0;
                    COMMOD9.NUDGER = true;
                    break;
                case "greedy":
                case "GREEDY":
                    COMMOD9.SOLVEF = 2;
                    break;
                case "temper":
                case "TEMPER":
                    COMMOD9.SOLVEF = 3;
                    break;
                case "squeeze":
                case "SQUEEZE":
                case "squash":
                case "SQUASH":
                    COMMOD9.SOLVEF = 4;
                    break;
                case "squeal":
                case "SQUEAL":
                    COMMOD9.SOLVEF = 5;
                    break;
                case "shrink":
                case "SHRINK":
                    COMMOD9.SOLVEF = 6;
                    break;
                case "shreal":
                case "SHREAL":
                    COMMOD9.SOLVEF = 7;
                    break;
                case "stack":
                case "STACK":
                case "tease":
                case "TEASE":
                    COMMOD9.SOLVEF = 8;
                    break;
                case "steal":
                case "STEAL":
                case "anneas":
                case "ANNEAS":
                    COMMOD9.SOLVEF = 9;
                    break;
                default:
                    COMMOD9.SOLVEF = 0;
                    break;
            }


            //Set flag for any use of secondary penalty
            //except in final output calls
            COMMOD9.PEN2F = 0;
            if (COMMOD9.SOLVEF >= 4) COMMOD9.PEN2F = 1;
            if ((COMMOD9.KSM > 0.0) || (COMMOD9.KSQ > 0.0) ||
            (COMMOD9.KSH > 0.0) || (COMMOD9.KTS > 0.0)) COMMOD9.PEN2F = 1;
            COMMOD9.PEN2M = COMMOD9.PEN2F;
           

            switch (PENALTY.ToUpper().Trim())
            {
                case "EVENTUAL":
                case "eventual":
                    COMMOD9.PENF = -1;
                    break;
                case "INTERVAL":
                case "interval":
                    COMMOD9.PENF = 0;
                    break;
                case "LEVEL":
                case "level":
                    COMMOD9.PENF = 1;
                    break;
                case "ORDINAL":
                case "ordinal":
                    COMMOD9.PENF = 2;
                    break;
                case "SPATIAL":
                case "spatial":
                    COMMOD9.PENF = 3;
                    break;
                case "RASCAL":
                case "rascal":
                    COMMOD9.PENF = 4;
                    break;
                case "RASCER":
                case "rascer":
                    COMMOD9.PENF = 4;
                    COMMOD9.RASCon = true;
                    break;
                case "ROYAL":
                case "royal":
                    COMMOD9.PENF = 5;
                    break;
                case "SEQUEL":
                case "sequel":
                    COMMOD9.PENF = 6;
                    //option crashes without Fb4LF constraint
                    COMMOD9.FB4LF = 1;
                    break;
                case "MOMENTAL":
                case "momental":
                    COMMOD9.PENF = 7;
                    //depends upon Fb4L in NABRLIMIT!	   
                    if (COMMOD9.FB4LF == 0) COMMOD9.FB4LF = 1;
                    break;
                default:
                    COMMOD9.DFLTPEN = true;
                    COMMOD9.PENF = 1;
                    break;
            }
            COMMOD9.PENM = COMMOD9.PENF;


            switch (STACKER.ToUpper().Trim())
            {
                case "OFF":
                case "off":
                    COMMOD9.STKF = 0;
                    break;
                case "THRU":
                case "thru":
                    COMMOD9.STKF = 1;
                    COMMOD9.STAKNAME = "THRU";
                    break;
                case "INCL":
                case "incl":
                    COMMOD9.STKF = 2;
                    COMMOD9.STAKNAME = "INCL";
                    break;
                case "DIST":
                case "dist":
                    COMMOD9.STKF = 3;
                    COMMOD9.STAKNAME = "DIST";
                    break;
                case "FREQ":
                case "freq":
                    COMMOD9.STKF = 4;
                    COMMOD9.STAKNAME = "FREQ";
                    break;
                case "EXIT":
                case "exit":
                    COMMOD9.STKF = 5;
                    COMMOD9.STAKNAME = "EXIT";
                    break;
                case "PROP":
                case "prop":
                    COMMOD9.STKF = 6;
                    COMMOD9.STAKNAME = "PROP";
                    break;
                case "OLAP":
                case "SPAN":
                case "olap":
                case "span":
                    COMMOD9.STKF = 7;
                    COMMOD9.STAKNAME = "SPAN";
                    break;
                case "COEX":
                case "coex":
                    COMMOD9.STKF = 8;
                    COMMOD9.STAKNAME = "COEX";
                    break;
                case "FB4L":
                case "fb4l":
                    COMMOD9.STKF = 9;
                    COMMOD9.STAKNAME = "FB4L";
                    if (COMMOD9.FB4LF == 0) COMMOD9.FB4LF = 1;
                    break;
                default:
                    COMMOD9.STKF = 0;
                    break;
            }


            //--------------------------------------------------------
            //----------ensure PENF and STKF are compatible-----------
            if((COMMOD9.STKF!=0)&&(COMMOD9.PENF>=2)&&(COMMOD9.PENF!=6)&&
                (COMMOD9.STKF<7))COMMOD9.STKF=7;

            if(COMMOD9.PENF==6)COMMOD9.STKF=8;
            //-----if PENF=5 (ROYAL),  COEX(STKF=8) would be redundant
            //-----only SPAN(STKF=7) and COEX(STKF=8) work with the 
            //-----ORDINAL, SPATIAL, and RASCAL penalties, because
            //-----these penalties don't solve the spacing problem
            //--------------------------------------------------------

            COMMOD9.FITOUTF = 1;
            if (FITS_OUT.ToUpper().Trim() == "OFF") COMMOD9.FITOUTF = 0;

            COMMOD9.RUNOUTF = 1;
            if (CNFG_OUT.ToUpper().Trim() == "OFF") COMMOD9.RUNOUTF = 0;

            COMMOD9.SEQOUTF = 1;
            if (SEQN_OUT.ToUpper().Trim() == "OFF") COMMOD9.SEQOUTF = 0;

            COMMOD9.INCOUTF = 1;
            if (INCR_OUT.ToUpper().Trim() == "OFF") COMMOD9.INCOUTF = 0;

            COMMOD9.LOCOUTF = 1;
            if (LOC_OUT.ToUpper().Trim() == "OFF") COMMOD9.LOCOUTF = 0;

            COMMOD9.OBSOUTF = 1;
            if (OBS_OUT.ToUpper().Trim() == "OFF") COMMOD9.OBSOUTF = 0;

            COMMOD9.CMPOUTF = 1;
            if (COMP_OUT.ToUpper().Trim() == "OFF") COMMOD9.CMPOUTF = 0;

            COMMOD9.SCTOUTF = 1;
            if (SECT_OUT.ToUpper().Trim() == "OFF") COMMOD9.SCTOUTF = 0;

            if (SECT_OUT.ToUpper().Trim() == "MIN") COMMOD9.SCTOUTF = 2;

            COMMOD9.COXOUTF = 1;
            if (COEX_OUT.ToUpper().Trim() == "OFF") COMMOD9.COXOUTF = 0;

            if (COEX_OUT.ToUpper().Trim() == "COUNT") COMMOD9.COXOUTF = 2;

            COMMOD9.EVTOUTF = 1;
            if (EVNT_OUT.ToUpper().Trim() == "OFF") COMMOD9.EVTOUTF = 0;


            COMMOD9.AUTF = 0;
            COMMOD9.ADAF = 0;
            COMMOD9.PAUSF = 0;
            COMMOD9.WALKF = 0;

            switch (PAUSES.ToUpper().Trim())
            {
                case "OFF":
                case "off":
                    COMMOD9.PAUSF = 1;
                    break;
                case "RPT":
                case "rpt":
                case "REP":
                case "rep":
                case "REPEAT":
                case "repeat":
                    COMMOD9.PAUSF = 2;
                    break;
                case "ADD":
                case "add":
                    COMMOD9.PAUSF = 3;
                    break;
                case "BAR":
                case "bar":
                    COMMOD9.PAUSF = 4;
                    break;
                case "AUT":
                case "aut":
                case "AUTO":
                case "auto":
                    COMMOD9.PAUSF = 5;
                    COMMOD9.NOUTER = 1;
                    break;
                case "ADA":
                case "ada":
                case "ADAPT":
                case "adapt":
                    COMMOD9.PAUSF = 5;
                    COMMOD9.NOUTER = 1;
                    COMMOD9.ADAF = 1;
                    break;
                case "WLK":
                case "wlk":
                case "WALK":
                case "walk":
                    COMMOD9.PAUSF = 1;
                    COMMOD9.WALKF = 1;
                    break;
            }


            COMMOD9.FIXF = 0;
            COMMOD9.MOVEF = 0;
            switch (SHOWMOVIES.ToUpper().Trim())
            {
                case "OFF":
                case "off":
                    COMMOD9.RUNGRF = 0;
                    break;
                case "DIV":
                case "div":
                    COMMOD9.RUNGRF = 0;
                    COMMOD9.MOVEF = 1;
                    break;
                case "PEN":
                case "pen":
                case "CHT":
                case "cht":
                case "BST":
                case "bst":
                    COMMOD9.RUNGRF = 2;
                    break;
                case "EVT":
                case "evt":
                    COMMOD9.RUNGRF = 4;
                    COMMOD9.GRIDF = 1;
                    break;
                case "LAG":
                case "lag":
                    COMMOD9.RUNGRF = 4;
                    COMMOD9.FIXF = 1;
                    COMMOD9.GRIDF = 1;
                    break;
                case "AIM":
                case "aim":
                    COMMOD9.RUNGRF = 4;
                    COMMOD9.FIXF = 2;
                    COMMOD9.GRIDF = 1;
                    break;
                case "FIX":
                case "fix":
                    COMMOD9.RUNGRF = 4;
                    COMMOD9.FIXF = 3;
                    COMMOD9.GRIDF = 1;
                    break;
                case "END":
                case "end":
                    COMMOD9.RUNGRF = 4;
                    COMMOD9.FIXF = 4;
                    COMMOD9.GRIDF = 1;
                    break;
                case "FAR":
                case "far":
                    COMMOD9.RUNGRF = 4;
                    COMMOD9.FIXF = 5;
                    COMMOD9.GRIDF = 1;
                    break;
                default:
                    COMMOD9.RUNGRF = 2;
                    break;

            }


            //check to see if grid should be updated even though RUNGRF.ne.4
            //why does this have to be arranged through RUNGRF rather than GRIDF
            //	IF((RUNGRF.ne.4).AND.
            //	   ((CURVFILE(1:3).ne."OFF").OR.(CURVFILE(1:3).ne."off")))
            //	   RUNGRF=5

            if (CURVFILE.Substring(0, 3).ToUpper() == "OFF") COMMOD9.GRIDF = 1;

            COMMOD9.TRJGRF = 0;
            switch (TRAJECTORY.ToUpper().Trim())
            {
                case "OFF":
                case "off":
                    COMMOD9.TRJGRF = 0;
                    break;
                case "ALL":
                case "all":
                    COMMOD9.TRJGRF = 4;
                    break;
                case "BAD":
                case "bad":
                    COMMOD9.TRJGRF = 3;
                    break;
                case "GOOD":
                case "good":
                    COMMOD9.TRJGRF = 2;
                    break;
                case "COOL":
                case "cool":
                    COMMOD9.TRJGRF = 1;
                    break;
            }


            COMMOD9.GRID2F = 0;
            if ((COMMOD9.SOLVEF == 8) || (COMMOD9.SOLVEF == 9) || (COMMOD9.KTS > 0.0)) COMMOD9.GRID2F = 1;

            
        //------------------------------------
            if (COMMOD9.CDF != 1)
            {
                TextWriter writer=new StreamWriter(File.Open(COMMOD9.RUNLOG,FileMode.Create));
                //TODO: write the namelists to the run log
                //   write the namelists to the run log
                //WRITE(11,*)	'_______________________________________'
                //WRITE(11,*) '----- PARAMETERS FROM  CONOP9.CFG -----'
                //WRITE(11,*) ' '
                //  WRITE(11, getinn)                                            
                //  WRITE(11, getans)                                            
                //  WRITE(11, getrun)                                            
                //  WRITE(11, getout)                                            
                //WRITE(11,*) '------- END OF INPUT PARAMETERS -------'
                //  WRITE(11,*) '---------------------------------------'
                //WRITE(11,*) ' '
                //  file1 = COMMITQQ(11)

                writer.Close();
            }

        //----------------------------------------------------------------------

    Label9999:
            
            return;
        }

        protected void InitConfigNamelist()
        {
            CONOPNET_CNF_NAMELIST = new Dictionary<string, Dictionary<string, object>>();

            Dictionary<string, object> getinn_namelist=new Dictionary<string,object>();
            getinn_namelist.Add("PROJECT",null);
            getinn_namelist.Add("SECTIONS",null);
            getinn_namelist.Add("TAXA",null);
            getinn_namelist.Add("EVENTS",null);
            getinn_namelist.Add("MAX_LEVELS",null);
            getinn_namelist.Add("MAX_LABELS",null);
            getinn_namelist.Add("LOADFILE",null);
            getinn_namelist.Add("PREPFILE",null);
            getinn_namelist.Add("SECTFILE",null);
            getinn_namelist.Add("SECTTAGFILE",null);
            getinn_namelist.Add("SECTTAGS",null);
            getinn_namelist.Add("LABELFILE",null);
            getinn_namelist.Add("EVENTFILE",null);
            getinn_namelist.Add("EVENTTAGFILE",null);
            getinn_namelist.Add("EVENTTAGS",null);
            getinn_namelist.Add("BESTKNOWN",null);
            CONOPNET_CNF_NAMELIST.Add("getinn", getinn_namelist);

            Dictionary<string, object> getans_namelist = new Dictionary<string, object>();
            getans_namelist.Add("PENALTY",null);
            getans_namelist.Add("LETCONTRACT",null);
            getans_namelist.Add("WEIGHTING",null);
            getans_namelist.Add("USENEGATIVE",null);
            getans_namelist.Add("NEARENOUGH",null);
            getans_namelist.Add("EXCLUSIVES",null);
            getans_namelist.Add("FORCECOEX",null);
            getans_namelist.Add("FORCEFB4L",null);
            getans_namelist.Add("HOMERANGE",null);
            getans_namelist.Add("SMOOTHER",null);
            getans_namelist.Add("SQUEEZER",null);
            getans_namelist.Add("SHRINKER",null);
            getans_namelist.Add("TEASER",null);
            getans_namelist.Add("STACKER",null);
            CONOPNET_CNF_NAMELIST.Add("getans", getans_namelist);

            Dictionary<string, object> getrun_namelist = new Dictionary<string, object>();
            getrun_namelist.Add("SOLVER",null);
            getrun_namelist.Add("STEPS",null);
            getrun_namelist.Add("TRIALS",null);
            getrun_namelist.Add("STARTEMP",null);
            getrun_namelist.Add("RATIO",null);
            getrun_namelist.Add("HOODSIZE",null);
            getrun_namelist.Add("STARTYPE",null);
            getrun_namelist.Add("STARTSECT",null);
            getrun_namelist.Add("STARTEVENT",null);
            getrun_namelist.Add("SHOWMOVIES",null);
            getrun_namelist.Add("TRAJECTORY",null);
            getrun_namelist.Add("VIDEOMODE",null);
            getrun_namelist.Add("PAUSES",null);
            getrun_namelist.Add("CURVFILE",null);
            getrun_namelist.Add("CRV2FILE",null);
            CONOPNET_CNF_NAMELIST.Add("getrun", getrun_namelist);

            Dictionary<string, object> getout_namelist = new Dictionary<string, object>();
            getout_namelist.Add("COLUMNS",null);
            getout_namelist.Add("UNLOADMAIN",null);
            getout_namelist.Add("FITS_OUT",null);
            getout_namelist.Add("CNFG_OUT",null);
            getout_namelist.Add("SEQN_OUT",null);
            getout_namelist.Add("INCR_OUT",null);
            getout_namelist.Add("LOC_OUT",null);
            getout_namelist.Add("OBS_OUT",null);
            getout_namelist.Add("COMP_OUT",null);
            getout_namelist.Add("UNLOADSECT",null);
            getout_namelist.Add("SECT_OUT",null);
            getout_namelist.Add("UNLOADEVNT",null);
            getout_namelist.Add("EVNT_OUT",null);
            getout_namelist.Add("COEX_OUT",null);
            getout_namelist.Add("RUNLOGFILE",null);
            getout_namelist.Add("CULLFILE",null);
            getout_namelist.Add("SOLNLIST",null);
            getout_namelist.Add("STARTFILE",null);
            getout_namelist.Add("STEPFILE",null);
            getout_namelist.Add("BESTARTFILE",null);
            getout_namelist.Add("COMPOSFILE",null);
            getout_namelist.Add("COMPOSNMBR",null);
            getout_namelist.Add("COMPOSTYPE",null);
            getout_namelist.Add("OBSDFILE",null);
            getout_namelist.Add("PLCDFILE",null);
            getout_namelist.Add("EXTNFILE",null);
            getout_namelist.Add("COEXISTFILE",null);
            getout_namelist.Add("FAD_LADFILE",null);
            getout_namelist.Add("ORDERFILE",null);
            CONOPNET_CNF_NAMELIST.Add("getout", getout_namelist);

        }

        protected void ReadConfigNamelist()
        {
            if (!File.Exists(CONOPNET_CNFGFILE))
                throw new FileNotFoundException();

            string[] lines = File.ReadAllLines(CONOPNET_CNFGFILE);

            bool bMaybeEnd1 = false;
            bool bMaybeEnd2 = false;
            string currentNamelistType = string.Empty;

            for (int i = 0; i < lines.Length; i++)
            {
                lines[i] = lines[i].Trim();                

                if ((lines[i].Trim() == string.Empty) || (lines[i].StartsWith("/")))
                {
                    if (bMaybeEnd2) { break; }
                    else if (bMaybeEnd1 == false)
                    {
                        bMaybeEnd1 = true;
                        continue;
                    }
                    else if (bMaybeEnd2 == false)
                    {
                        bMaybeEnd2 = true;
                        continue;
                    }

                }

                if (lines[i].StartsWith("&"))
                {
                    currentNamelistType = lines[i].Substring(1).Trim();
                    bMaybeEnd1 = false;
                    bMaybeEnd2 = false;
                }
                else if (lines[i].StartsWith("_"))//comments
                {
                    continue;
                }
                else
                {
                    object[] kvp = GetKeyValuePair(lines[i]);

                    CONOPNET_CNF_NAMELIST[currentNamelistType][kvp[0] as string] = kvp[1];


                }
            }

            //getinn
            PROJECT = CONOPNET_CNF_NAMELIST["getinn"]["PROJECT"].ToString();
            SECTIONS = int.Parse(CONOPNET_CNF_NAMELIST["getinn"]["SECTIONS"].ToString());
            TAXA = int.Parse(CONOPNET_CNF_NAMELIST["getinn"]["TAXA"].ToString());
            EVENTS = int.Parse(CONOPNET_CNF_NAMELIST["getinn"]["EVENTS"].ToString());
            MAX_LEVELS = int.Parse(CONOPNET_CNF_NAMELIST["getinn"]["MAX_LEVELS"].ToString());
            MAX_LABELS = int.Parse(CONOPNET_CNF_NAMELIST["getinn"]["MAX_LABELS"].ToString());
            LOADFILE = CONOPNET_CNF_NAMELIST["getinn"]["LOADFILE"].ToString();
            PREPFILE = CONOPNET_CNF_NAMELIST["getinn"]["PREPFILE"].ToString();
            SECTFILE = CONOPNET_CNF_NAMELIST["getinn"]["SECTFILE"].ToString();
            SECTTAGFILE = CONOPNET_CNF_NAMELIST["getinn"]["SECTTAGFILE"].ToString();
            SECTTAGS = CONOPNET_CNF_NAMELIST["getinn"]["SECTTAGS"].ToString();
            LABELFILE = CONOPNET_CNF_NAMELIST["getinn"]["LABELFILE"].ToString();
            EVENTFILE = CONOPNET_CNF_NAMELIST["getinn"]["EVENTFILE"].ToString();
            EVENTTAGFILE = CONOPNET_CNF_NAMELIST["getinn"]["EVENTTAGFILE"].ToString();
            EVENTTAGS = CONOPNET_CNF_NAMELIST["getinn"]["EVENTTAGS"].ToString();
            BESTKNOWN = double.Parse(CONOPNET_CNF_NAMELIST["getinn"]["BESTKNOWN"].ToString());

            //getans
            PENALTY = CONOPNET_CNF_NAMELIST["getans"]["PENALTY"].ToString();
            LETCONTRACT = CONOPNET_CNF_NAMELIST["getans"]["LETCONTRACT"].ToString();
            WEIGHTING = CONOPNET_CNF_NAMELIST["getans"]["WEIGHTING"].ToString();
            USENEGATIVE = CONOPNET_CNF_NAMELIST["getans"]["USENEGATIVE"].ToString();
            NEARENOUGH = double.Parse(CONOPNET_CNF_NAMELIST["getans"]["NEARENOUGH"].ToString());
            EXCLUSIVES = CONOPNET_CNF_NAMELIST["getans"]["EXCLUSIVES"].ToString();
            FORCECOEX = CONOPNET_CNF_NAMELIST["getans"]["FORCECOEX"].ToString();
            FORCEFB4L = CONOPNET_CNF_NAMELIST["getans"]["FORCEFB4L"].ToString();
            HOMERANGE = CONOPNET_CNF_NAMELIST["getans"]["HOMERANGE"].ToString();
            SMOOTHER = double.Parse(CONOPNET_CNF_NAMELIST["getans"]["SMOOTHER"].ToString());
            SQUEEZER = double.Parse(CONOPNET_CNF_NAMELIST["getans"]["SQUEEZER"].ToString());
            SHRINKER = double.Parse(CONOPNET_CNF_NAMELIST["getans"]["SHRINKER"].ToString());
            TEASER = double.Parse(CONOPNET_CNF_NAMELIST["getans"]["TEASER"].ToString());
            STACKER = CONOPNET_CNF_NAMELIST["getans"]["STACKER"].ToString();
            
            //getrun
            SOLVER = CONOPNET_CNF_NAMELIST["getrun"]["SOLVER"].ToString();
            STEPS = int.Parse(CONOPNET_CNF_NAMELIST["getrun"]["STEPS"].ToString());
            TRIALS = int.Parse(CONOPNET_CNF_NAMELIST["getrun"]["TRIALS"].ToString());
            STARTEMP = double.Parse(CONOPNET_CNF_NAMELIST["getrun"]["STARTEMP"].ToString());
            RATIO = double.Parse(CONOPNET_CNF_NAMELIST["getrun"]["RATIO"].ToString());
            HOODSIZE = CONOPNET_CNF_NAMELIST["getrun"]["HOODSIZE"].ToString();
            STARTYPE = CONOPNET_CNF_NAMELIST["getrun"]["STARTYPE"].ToString();
            STARTSECT = int.Parse(CONOPNET_CNF_NAMELIST["getrun"]["STARTSECT"].ToString());
            STARTEVENT = int.Parse(CONOPNET_CNF_NAMELIST["getrun"]["STARTEVENT"].ToString());
            SHOWMOVIES = CONOPNET_CNF_NAMELIST["getrun"]["SHOWMOVIES"].ToString();
            TRAJECTORY = CONOPNET_CNF_NAMELIST["getrun"]["TRAJECTORY"].ToString();
            VIDEOMODE = CONOPNET_CNF_NAMELIST["getrun"]["VIDEOMODE"].ToString();
            PAUSES = CONOPNET_CNF_NAMELIST["getrun"]["PAUSES"].ToString();
            CURVFILE = CONOPNET_CNF_NAMELIST["getrun"]["CURVFILE"].ToString();
            CRV2FILE = CONOPNET_CNF_NAMELIST["getrun"]["CRV2FILE"].ToString();

            //getout
            COLUMNS = int.Parse(CONOPNET_CNF_NAMELIST["getout"]["COLUMNS"].ToString());
            UNLOADMAIN = CONOPNET_CNF_NAMELIST["getout"]["UNLOADMAIN"].ToString();
            FITS_OUT = CONOPNET_CNF_NAMELIST["getout"]["FITS_OUT"].ToString();
            CNFG_OUT = CONOPNET_CNF_NAMELIST["getout"]["CNFG_OUT"].ToString();
            SEQN_OUT = CONOPNET_CNF_NAMELIST["getout"]["SEQN_OUT"].ToString();
            INCR_OUT = CONOPNET_CNF_NAMELIST["getout"]["INCR_OUT"].ToString();
            LOC_OUT = CONOPNET_CNF_NAMELIST["getout"]["LOC_OUT"].ToString();
            OBS_OUT = CONOPNET_CNF_NAMELIST["getout"]["OBS_OUT"].ToString();
            COMP_OUT = CONOPNET_CNF_NAMELIST["getout"]["COMP_OUT"].ToString();
            UNLOADSECT = CONOPNET_CNF_NAMELIST["getout"]["UNLOADSECT"].ToString();
            SECT_OUT = CONOPNET_CNF_NAMELIST["getout"]["SECT_OUT"].ToString();
            UNLOADEVNT = CONOPNET_CNF_NAMELIST["getout"]["UNLOADEVNT"].ToString();
            EVNT_OUT = CONOPNET_CNF_NAMELIST["getout"]["EVNT_OUT"].ToString();
            COEX_OUT = CONOPNET_CNF_NAMELIST["getout"]["COEX_OUT"].ToString();
            RUNLOGFILE = CONOPNET_CNF_NAMELIST["getout"]["RUNLOGFILE"].ToString();
            CULLFILE = CONOPNET_CNF_NAMELIST["getout"]["CULLFILE"].ToString();
            SOLNLIST = CONOPNET_CNF_NAMELIST["getout"]["SOLNLIST"].ToString();
            STARTFILE = CONOPNET_CNF_NAMELIST["getout"]["STARTFILE"].ToString();
            STEPFILE = CONOPNET_CNF_NAMELIST["getout"]["STEPFILE"].ToString();
            BESTARTFILE = CONOPNET_CNF_NAMELIST["getout"]["BESTARTFILE"].ToString();
            COMPOSFILE = CONOPNET_CNF_NAMELIST["getout"]["COMPOSFILE"].ToString();
            COMPOSNMBR = int.Parse(CONOPNET_CNF_NAMELIST["getout"]["COMPOSNMBR"].ToString());
            COMPOSTYPE = CONOPNET_CNF_NAMELIST["getout"]["COMPOSTYPE"].ToString();
            OBSDFILE = CONOPNET_CNF_NAMELIST["getout"]["OBSDFILE"].ToString();
            PLCDFILE = CONOPNET_CNF_NAMELIST["getout"]["PLCDFILE"].ToString();
            EXTNFILE = CONOPNET_CNF_NAMELIST["getout"]["EXTNFILE"].ToString();
            COEXISTFILE = CONOPNET_CNF_NAMELIST["getout"]["COEXISTFILE"].ToString();
            FAD_LADFILE = CONOPNET_CNF_NAMELIST["getout"]["FAD_LADFILE"].ToString();
            ORDERFILE = CONOPNET_CNF_NAMELIST["getout"]["ORDERFILE"].ToString();


        }

        private object[] GetKeyValuePair(string line)
        {
            object[] retVal = new object[2];

            if (!line.Contains("=")) throw new Exception("error in reading config file.");

            string[] kvpString = line.Split(new string[]{ "=" },StringSplitOptions.RemoveEmptyEntries);
            retVal[0]=kvpString[0].ToString().ToUpper();

            if (kvpString[1].StartsWith("'"))
            {
                retVal[1] = kvpString[1].Trim(new char[] { '\'' });
            }
            else
            {
                retVal[1] = double.Parse(kvpString[1]);
            }

            return retVal;
        }

        public void PrintConfigNamelist()
        {
            string currentNamelistType="getinn";
            Helper.Write("&{0}\n", currentNamelistType);
            foreach (KeyValuePair<string, object> kvp in CONOPNET_CNF_NAMELIST[currentNamelistType])
            {
                Helper.Write("{0}={1}\n", kvp.Key, kvp.Value);
            }
            Helper.Write("/\n\n");

            currentNamelistType = "getans";
            Helper.Write("&{0}\n", currentNamelistType);
            foreach (KeyValuePair<string, object> kvp in CONOPNET_CNF_NAMELIST[currentNamelistType])
            {
                Helper.Write("{0}={1}\n", kvp.Key, kvp.Value);
            }
            Helper.Write("/\n\n");

            currentNamelistType = "getrun";
            Helper.Write("&{0}\n", currentNamelistType);
            foreach (KeyValuePair<string, object> kvp in CONOPNET_CNF_NAMELIST[currentNamelistType])
            {
                Helper.Write("{0}={1}\n", kvp.Key, kvp.Value);
            }
            Helper.Write("/\n\n");

            currentNamelistType = "getout";
            Helper.Write("&{0}\n", currentNamelistType);
            foreach (KeyValuePair<string, object> kvp in CONOPNET_CNF_NAMELIST[currentNamelistType])
            {
                Helper.Write("{0}={1}\n", kvp.Key, kvp.Value);
            }
            Helper.Write("/\n\n");
        }

       
    }
}
