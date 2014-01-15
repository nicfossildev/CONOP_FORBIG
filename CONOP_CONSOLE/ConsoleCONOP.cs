using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Runtime.InteropServices;
#if SWT_CHECK_VIA_GETPEN
using System.Diagnostics;
#endif

using CONOP.NET;

namespace CONOP
{
    public class ConsoleCONOP
    {
        #region VARs        
        
        //--------------------------------//
        //Variables for system time on 486//
        //--------------------------------//                 
        private int I, J, M, L;
        private int ttlco, ranco, maxco;
        private double rtlco;
        private double topj, btmj, minc;
        private string xtitle;
        private int rngj;

        private TimeSpan TS_START;
        private TimeSpan TS_END;

        /// <summary>
        /// MainInformation for CONOP output
        /// </summary>
        public StringBuilder MSG_SB = new StringBuilder();

        private bool IsRunning = false;
        private bool IsCompleted = false;
        private bool IsBeforeAnneal = true;
        private int Step = -1;

        private int JDelta = 0;
        private int EvtDelta = 0;
        private int LDelta = 0;

        private bool bDrawCHT = false;

        #endregion

        /// <summary>
        /// For initialization preparation and key interaction
        /// </summary>
        public ConsoleCONOP()
        {
            MSG_SB.Append("   CONOP.NET VERSION 1.0 BETA1   -- REWRITTEN FROM CONOP9       \n" +
                  "   Stratigraphic Correlation and Seriation by Inversion        \n" +
                  "   using CONstrained OPtimization                              \n" +
                  "   Copyright  2013    Junxuan Fan , Xudong Hou &  Pete Sadler \n" +
                  "   (  fanjunxuan@gmail.com  houxudonggis@gmail.com peter.sadler@ucr.edu )\n");

            MSG_SB.AppendLine("\r\n    PRESS <ENTER> TO RUN WITH GRAPHICS ON SCREEN" +
                              "\r\n or PRESS <SPACE> TO RUN WITHOUT GRAPHICS BUT FASTER");
        }

        /// <summary>
        /// Start Main Message loop
        /// </summary>
        public void MsgLoop()
        {
            ConsoleColor preConsoleColor = Console.ForegroundColor;
            Console.WriteLine(MSG_SB.ToString());
            ConsoleKey input;
            while ((input = Console.ReadKey(true).Key) != ConsoleKey.Escape)
            {
                Console.ForegroundColor = ConsoleColor.Green;
                if (input == ConsoleKey.Enter || input == ConsoleKey.Spacebar)
                {
                    MSG_SB.Clear();
                    MSG_SB.AppendLine("============ Start Simulated Annealing procedure ============");
                    Console.Write(MSG_SB.ToString());
                    this.Run(false);

                    TS_END = new TimeSpan(DateTime.Now.Ticks);
                    TS_END = TS_END.Subtract(TS_START).Duration();
                    COMMOD COMMOD9 = COMMOD.Singleton();
                    MSG_SB.Length = 0;
                    MSG_SB.AppendFormat("BSTPEN = {0}     ELAPSED {1}h:{2}m:{3}s", (int)COMMOD9.BSTPEN, TS_END.Hours, TS_END.Minutes, TS_END.Seconds);
                    MSG_SB.AppendLine("\r\nPRESS <Enter> TO VIEW CHARTS OR  PRESS <ESC> TO EXIT.");
                    Console.Write(MSG_SB.ToString());

                    Console.ForegroundColor = preConsoleColor;
                }

                MSG_SB.Clear();
                MSG_SB.AppendLine("============ Repeated Simulated Annealing procedure ============");
                Console.Write(MSG_SB.ToString());
            }
            Console.ForegroundColor = preConsoleColor;
        }

        /// <summary>
        /// BeforeAnneal method for preparation of global parameters
        /// </summary>
        protected void BeforeAnneal()
        {
            #region Initialize

            TS_START = new TimeSpan(DateTime.Now.Ticks);

            //Initialize some COMMOD variables
            COMMOD COMMOD9 = COMMOD.Singleton();
            COMMOD9.STOPF = 0;
            COMMOD9.NTRIES = 0;
            COMMOD9.MXLVL = 0;
            COMMOD9.MXLBL = 0;
            COMMOD9.WTF = 0;
            COMMOD9.PENF = 0;
            COMMOD9.PEN2F = 0;
            COMMOD9.PENM = 0;
            COMMOD9.PEN2M = 0;
            COMMOD9.NFALA = 0;
            COMMOD9.MXFALA = 0;
            COMMOD9.NCOEX = 0;
            COMMOD9.MXCOEX = 0;

            //Initialize local variables for Intel 11.1
            minc = 0.0;
            topj = 0.0;
            btmj = 0.0;
            minc = 0.0;
            rtlco = 0.0;
            I = 0;
            J = 0;
            M = 0;
            L = 0;
            ttlco = 0;
            ranco = 0;
            maxco = 0;
            rngj = 0;

            TextReader reader = null;
            string dataline = null;

            #endregion

            //Console.ForegroundColor = ConsoleColor.Green;
            //Console.WriteLine("________________________________________________________________\n" +
            //                  "|   CONOP.NET VERSION 1.0 BETA   -- REWRITTEN FROM CONOP9       |\n" +
            //                  "|   Stratigraphic Correlation and Seriation by Inversion        |\n" +
            //                  "|   using CONstrained OPtimization                              |\n" +
            //                  "|   Copyright C 2013    Pete Sadler, Junxuan Fan & Xudong Hou   |\n" +
            //                  "|   (peter.sadler@ucr.edu  fanjunxuan@gmail.com)                |\n" +
            //                  "----------------------------------------------------------------\n"
            //                  );
            //Console.WriteLine();
            //Console.ForegroundColor = ConsoleColor.Yellow;           

            #region MAIN PROCESS

            PREPARE.Singleton().RunIt();

            if (COMMOD9.STOPF == 1) goto Label998;

            //lock out CONTROL9 options
            COMMOD9.CALIBF = 0;

            //---Set array sizes
            //all the PERM arrays must be allocated the same size 
            COMMOD9.PERM = new int[COMMOD9.NEVNT];
            COMMOD9.INIPERM = new int[COMMOD9.NEVNT];
            COMMOD9.PXLPERM = new int[COMMOD9.NEVNT];
            COMMOD9.TEMPERM = new int[COMMOD9.NEVNT];
            COMMOD9.BSTPERM = new int[COMMOD9.NEVNT];
            COMMOD9.LSTPERM = new int[COMMOD9.NEVNT];


            COMMOD9.HLEVEL = new int[COMMOD9.NSCT + 10];
            COMMOD9.PAIRJ = new int[COMMOD9.NSCT];
            COMMOD9.RANGE = new int[COMMOD9.NSCT];
            COMMOD9.IN = new double[COMMOD9.NSCT];
            //stores base and top level in composite (i.e. section span)
            COMMOD9.LIMITS = new int[COMMOD9.NSCT, 2];

            COMMOD9.COVER = new int[COMMOD9.NEVNT];
            //stores coverage; i.e. number of sections that span each level
            COMMOD9.ZLEVEL = new double[COMMOD9.NSCT, 2];
            COMMOD9.COEXST = new int[COMMOD9.NSPC, COMMOD9.NSPC];
            COMMOD9.PTLORD = new int[COMMOD9.NEVNT, COMMOD9.NEVNT + 2 - (COMMOD9.NSPC * 2)];

            if (COMMOD9.FB4LF != 0)
            {
                COMMOD9.FADLAD = new int[COMMOD9.NSPC, COMMOD9.NSPC];
            }

            if (COMMOD9.PENF == 4)
            {
                COMMOD9.RASC = new int[COMMOD9.NEVNT, COMMOD9.NEVNT];
            }

            //stores number of events at each level
            COMMOD9.ELEVEL = new int[COMMOD9.MXLVL + 2, COMMOD9.NSCT];

            //first NSCT columns in NEGATIVE() record whether a section contains the 
            //taxon (1), or only its coexisters (2), or neither (0)
            //next two columns record IROW for FAD and LAD events
            //last two columns can record PERM positions of FAD and LAD
            COMMOD9.NEGATIVE = new int[COMMOD9.NSPC, COMMOD9.NSCT + 4];

            //RSTATIC holds invariant real values - for each section
            //ISTATIC holds invariant integer values	- for sections and composites
            COMMOD9.IROWS = new int[COMMOD9.NEVNT, 6];
            COMMOD9.RSTATIC = new double[COMMOD9.NEVNT, COMMOD9.NSCT, 2];
            COMMOD9.ISTATIC = new int[COMMOD9.NEVNT, COMMOD9.NSCT + 10, 2];

            //hold the key to level heights
            //valevel for the local sections
            //complvl for the composite(s) has 10 options
            //ORD STD ZTD MAX ZMX AVG MIN ZRO MST ZMS
            COMMOD9.VALEVEL = new double[COMMOD9.MXLVL + 3, COMMOD9.NSCT];
            COMMOD9.COMPLVL = new double[COMMOD9.NEVNT + 3, 10];

            //array for temporary storage of penalties by section
            COMMOD9.SCJPEN = new double[COMMOD9.NSCT];
            COMMOD9.COLPEN = new double[COMMOD9.NSCT];

            //an array to track the penalties by event
            //,1  INTERVAL
            //,2  LEVEL
            //,3  EVENTUAL
            COMMOD9.EVTPEN = new double[COMMOD9.NEVNT, 3];

            //store solutions to locating task
            //all sections plus 7 composite sections
            //SCTSOL is last accepted, BSCTSOL is best, HSCTSOL is current
            COMMOD9.HSCTSOL = new int[COMMOD9.NEVNT, COMMOD9.NSCT + 10];
            COMMOD9.SCTSOL = new int[COMMOD9.NEVNT, COMMOD9.NSCT + 10];
            COMMOD9.BSCTSOL = new int[COMMOD9.NEVNT, COMMOD9.NSCT + 10];

            COMMOD9.HSCTRNG = new int[COMMOD9.NEVNT, COMMOD9.NSCT + 10, 2];

            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                for (J = 0; J < COMMOD9.NSCT + 10; J++)
                {
                    COMMOD9.HSCTRNG[I, J, 0] = 1;
                    COMMOD9.HSCTRNG[I, J, 1] = COMMOD9.NEVNT;
                }
            }

            COMMOD9.SECTNAME = new string[COMMOD9.NSCT];
            COMMOD9.SECTNICK = new string[COMMOD9.NSCT];

            COMMOD9.LABLNAME = new string[COMMOD9.MXLBL];
            COMMOD9.EVNTNAME = new string[COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS];
            COMMOD9.EVNTNICK = new string[COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS];

            COMMOD9.ETAGNAME = new string[COMMOD9.MXLBL];
            COMMOD9.STAGNAME = new string[COMMOD9.MXLBL];
            for (I = 0; I < COMMOD9.MXLBL; I++)
            {
                COMMOD9.ETAGNAME[I] = "not assigned";
                COMMOD9.STAGNAME[I] = "not assigned";
            }

            COMMOD9.SECTPERM = new int[COMMOD9.NSCT];
            Helper.SetVal(COMMOD9.SECTPERM, -1);//<=HXD

            //store section properties  
            //1: inclusion in composite
            //2: exclusive taxa at section top or base
            //3: exclusive singleton taxa at top or base
            //4: tag
            COMMOD9.SECTPROP = new int[COMMOD9.NSCT, 4];

            COMMOD9.CULLIST = new int[COMMOD9.NEVNT, 6];

            if (COMMOD9.RUNGRF >= 4 || COMMOD9.GRIDF == 1)
            {

                COMMOD9.EVERYBST = new double[COMMOD9.NEVNT, COMMOD9.NEVNT];

                if (COMMOD9.PAUSF == 4) COMMOD9.CONFGRID = new double[COMMOD9.NEVNT, COMMOD9.NEVNT];

                if (COMMOD9.BESTFILE.Substring(0, 3) == "ADD" || COMMOD9.BESTFILE.Substring(0, 3) == "add")
                {

                    bool hasFile1 = File.Exists(COMMOD9.BESTFILE.Substring(3));
                    //start the array with the previous best vales
                    if (hasFile1)
                    {
                        Helper.Write("BESTFILE Exists\n");
                        //TODO:CONOP.CS
                        //OPEN(26,FILE=BESTFILE(4:))
                        Helper.Write("  Loading Event/Position Archive . . .\n");

                        for (I = 0; I < COMMOD9.NEVNT; I++)
                        {
                            for (J = 0; J < COMMOD9.NEVNT; J++)
                            {
                                //TODO:CONOP.CS
                                //READ(26,907) (EVERYBST(I,J),J=1,NEVNT)
                                //907   FORMAT(1X,9000(F15.4,1X))

                            }
                        }
                    }
                    else
                    {
                        //start a new array
                        Helper.SetVal(COMMOD9.EVERYBST, 99999999.9999);

                    }
                }
                else
                {
                    //start a new array
                    Helper.SetVal(COMMOD9.EVERYBST, 99999999.9999);


                }

                COMMOD9.VERYBEST = Helper.MinVal(COMMOD9.EVERYBST);
                if ((COMMOD9.VERYBEST - COMMOD9.USERBEST) < -0.01)
                {
                    Helper.Write("  BESTKNOWN value larger than minimum in CURVFILE: {0}\n", COMMOD9.VERYBEST);
                }
                else if ((COMMOD9.VERYBEST - COMMOD9.USERBEST) > 0.01)
                {
                    Helper.Write("  BESTKNOWN value smaller than minimum in CURVFILE: {0}\n", COMMOD9.VERYBEST);
                }


                if (COMMOD9.GRID2F == 1)
                {
                    COMMOD9.SECNDBST = new double[COMMOD9.NEVNT, COMMOD9.NEVNT];

                    if (COMMOD9.SCNDFILE.Substring(0, 3) == "ADD" || COMMOD9.SCNDFILE.Substring(0, 3) == "add")
                    {
                        //TODO:CONOP.CS
                        //INQUIRE(FILE=SCNDFILE(4:),EXIST=file1)
                        //start the array with the previous best vales
                        bool hasFile = true;
                        if (hasFile)
                        {
                            //TODO:CONOP.CS                            
                            //OPEN(27,FILE=SCNDFILE(4:))
                            Helper.Write("  Loading 2nd Event/Position Archive . . .\n");
                            //   DO I=1,NEVNT
                            //     READ(27,909) (SECNDBST(I,J),J=1,NEVNT)
                            //909            FORMAT(1X,9000(F15.4,1X)) 
                            //   END DO
                        }
                        else
                        {
                            //start a new array
                            Helper.SetVal(COMMOD9.SECNDBST, 99999999.9999);
                        }
                    }
                    else
                    {
                        //start a new array
                        Helper.SetVal(COMMOD9.SECNDBST, 99999999.9999);
                    }

                    COMMOD9.TWINBEST = Helper.MinVal(COMMOD9.SECNDBST);
                }
            }



            //----------------------------------------------
            //  WRITE (*,*)   'WE HAVE EXITED PREPARE'
            //  WRITE (*,*)   'INFILE  =  ',INFILE
            //  WRITE (*,*)   'MAIN OUTPUT FILE =  ',OUTMAIN
            //  WRITE (*,*)   'SECT OUTPUT FILE =  ',OUTSECT
            //  WRITE (*,*)   'EVNT OUTPUT FILE =  ',OUTEVNT
            //**********************************************

            //CALL GETTIM(BHR, BMIN, BSEC, BCENT)  
            //CALL GETDAT(BYR, BMON, BDAY)

            DOINPUT.Singleton().RunIt();



            COMMOD9.EXCLSECT = new int[COMMOD9.JEXCL];
            I = -1;//HACK:I=1 IN FORTRAN
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                if (COMMOD9.SECTPROP[J, 1] > 0)
                {
                    I = I + 1;
                    COMMOD9.EXCLSECT[I] = J;
                }
            }


            if (COMMOD9.STOPF == 1) goto Label998;

            //CPMS**************************************************************
            //CPMS   now fill up the COXST PTLORD FADLAD arrays to make constraints 
            //CPMS   suitable for generating the initial permutation 
            //CPMS   coexistences not entered from file 
            if (COMMOD9.COXSTF != 1)
            {

                //CPMS      fill coexistence matrix    
                Helper.Write("    \n");
                Helper.Write("  Building Coexistence Matrix . . . \n");
                //CPMS      with loose coexistence rules
                DOCOEX.Singleton().RunIt(COMMOD9.COXSTF);


                //CPMS-----------------
                if (COMMOD9.STOPF == 1) goto Label998;

                //CPMS   -------------------------
            }
            else if ((COMMOD9.COXSTF == 1) && (COMMOD9.NSPC > 0))
            {

                //CPMS      load coexistence matrix from file               
                //CPMS      this could be dangerous -- matrix on file must be 
                //CPMS      suitable for generating a plausible initial solution
                //CPMS      initial solution should be conservative!
                Helper.Write("  Loading the Coexistence Matrix from file. . . \n");


                //TODO:CONOP.CS
                //CALL TIMEOUT(0.40)
                //OPEN(21,FILE=COEXFILE)
                //Read file into COEXST(I,J),
                reader = new StreamReader(File.Open(COMMOD9.COEXFILE, FileMode.Open));
                dataline = null;

                I = 0;
                while ((dataline = reader.ReadLine()) != null)
                {
                    string[] dataString = dataline.Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries);

                    if (dataString.Length != COMMOD9.NSPC) throw new Exception();

                    for (J = 0; J < COMMOD9.NSPC; J++)
                    {
                        //TODO:CONOP.CS
                        //READ(21,9009, END=9008) (COEXST(I,J),J=1,NSPC)
                        COMMOD9.COEXST[I, J] = int.Parse(dataString[J]);
                    }

                    I++;
                }

                reader.Close();

            }



            //count the number of coexistences proven by observation
            COMMOD9.NCOEX = 0;
            if (COMMOD9.NSPC > 0)
            {
                for (I = 0; I < COMMOD9.NSPC - 1; I++)
                {
                    for (M = I + 1; M < COMMOD9.NSPC; M++)
                    {
                        if (COMMOD9.COEXST[I, M] >= (int)COMMOD9.COXSTF) COMMOD9.NCOEX = COMMOD9.NCOEX + 1;
                    }
                }
            }



            if (COMMOD9.CDF != 1)
            {
                //TODO:CONOP.CS
                //OPEN(11, FILE=RUNLOG, STATUS='OLD', POSITION='APPEND')
                //WRITE(11,*) '    '
                //WRITE(11,*) '------COEX Matrix Loaded-------------'
                //WRITE(11,*) 'Number of observed coexisting pairs: ', NCOEX
                //WRITE(11,*) '   '
                //CLOSE(11, STATUS='KEEP')

                //OPEN(64,FILE="trajectory.txt")
                //   WRITE(64,*) 'Temperature | Current Misfit | Best Fit'
                //CLOSE(64)
            }

            Helper.Write("  Number of observed coexisting pairs: {0}\n", COMMOD9.NCOEX);


            //CPMS--------------------------------       
            //c------Now that the COEX matrix is ready
            //c------correct weights according to WEIGHT (->WTF) 
            switch (COMMOD9.WTF)
            {
                case 0:
                    //no weights used; convert all to 1.00
                    Helper.SetVal(COMMOD9.RSTATIC, 1.0);
                    Helper.Write("  Setting all weights to 1.00 . . .\n");
                    break;
                case 1:
                    //use weights from input file
                    Helper.Write("  Using weights as listed . . .\n");
                    break;
                case 2:
                case 22:
                case 23:
                case 3:
                    //recalculate weights for paired taxa according 
                    //to number of coexistences
                    Helper.Write("  Calculating weights from coexistences . . .\n");

                    for (I = 0; I < COMMOD9.NEVNT; I++)
                    {
                        //1.find paired firsts or lasts
                        if ((COMMOD9.IROWS[I, 1] == 1) || (COMMOD9.IROWS[I, 1] == 2))
                        {
                            //2.calculate number of coexistences
                            ttlco = 0;

                            for (J = 0; J < COMMOD9.NSPC; J++)
                            {
                                if (COMMOD9.IROWS[I, 2] == J) continue;

                                if (COMMOD9.COEXST[COMMOD9.IROWS[I, 2], J] >= (int)COMMOD9.COXSTF)
                                {
                                    ttlco = ttlco + 1;
                                }
                            }

                            COMMOD9.RANGE[COMMOD9.IROWS[I, 2]] = (int)ttlco;

                            if (COMMOD9.WTF != 3)
                            {
                                //3.update RSTATIC()
                                rtlco = Math.Max((double)ttlco, 1.0);
                                if (COMMOD9.WTF == 22) rtlco = Math.Pow(rtlco, 0.5);
                                if (COMMOD9.WTF == 23) rtlco = Math.Pow(rtlco, 0.33);
                                Helper.Write("event - weight: {0} - {1}", I, rtlco);

                                for (J = 0; J < COMMOD9.NSCT; J++)
                                {
                                    COMMOD9.RSTATIC[I, J, 0] = 1.0 / rtlco;
                                    COMMOD9.RSTATIC[I, J, 1] = 1.0 / rtlco;
                                }
                            }
                        }
                    }

                    if (COMMOD9.WTF == 3)
                    {
                        Helper.Write("   % of taxa with reduced weights: {0}\n", COMMOD9.cutco);

                        if (COMMOD9.CDF != 1)
                        {
                            //TODO:CONOP.CS
                            //  OPEN(11, FILE=RUNLOG, STATUS='OLD', POSITION='APPEND')
                            //  WRITE(11,*) '  '
                            //  WRITE(11,*) '---------------------------------------'	
                            //WRITE(xtitle,'(I0)') cutco 
                            //  WRITE(11,*) 'WEIGHT REDUCTION for top '//TRIM(xtitle)//
                            //1		            'th percentile long-ranging taxa,'
                            //  WRITE(11,*) 'as judged by coexistence count'
                        }
                    }

                    COMMOD9.cutco = (int)(((double)COMMOD9.cutco) * ((double)COMMOD9.NSPC) / 100.0);
                    if ((COMMOD9.WTF == 3) && (COMMOD9.cutco > 0))
                    {
                        //find ranks in RANGE
                        ranco = 1;
                        maxco = Helper.MAXVal(COMMOD9.RANGE);

                        for (; ranco <= COMMOD9.cutco; )
                        {
                            for (I = 0; I < COMMOD9.NEVNT; I++)
                            {
                                if ((COMMOD9.IROWS[I, 1] != 1) && (COMMOD9.IROWS[I, 1] != 2)) continue;

                                if (COMMOD9.RANGE[COMMOD9.IROWS[I, 2]] == (int)maxco)
                                {
                                    //3. update RSTATIC()
                                    if ((COMMOD9.CDF != 1) && (COMMOD9.IROWS[I, 1] == 1))
                                    {
                                        Helper.GETEVNT(I, ref xtitle, 0, 1, 0, 1, 0, 0);
                                        //TODO:CONOP.CS
                                        //WRITE(11,*) IROWS(I,1),"  - "//TRIM(xtitle)
                                    }

                                    for (J = 0; J < COMMOD9.NSCT; J++)
                                    {
                                        COMMOD9.RSTATIC[I, J, 0] = COMMOD9.RSTATIC[I, J, 0] / 10;
                                        COMMOD9.RSTATIC[I, J, 1] = COMMOD9.RSTATIC[I, J, 1] / 10;
                                    }

                                    ranco = ranco + 1;
                                    COMMOD9.RANGE[COMMOD9.IROWS[I, 2]] = 0;
                                }
                            }

                            maxco = Helper.MAXVal(COMMOD9.RANGE);
                        }

                        if (COMMOD9.CDF != 1)
                        {
                            //TODO:CONOP.CS
                            //WRITE(11,*) '---------------------------------------'	
                            //WRITE(11,*) '  '
                            //CLOSE(11, STATUS='KEEP')
                        }
                    }

                    break;
            }



            //C----------------------------------
            //CPMS   fill partial ordering matrix        
            if (COMMOD9.NOTHR > 0)
            {
                Helper.Write("  Building the Partial Ordering Matrix . . .\n");

                DOPTLORD.Singleton().RunIt();

                if (COMMOD9.STOPF == 1) goto Label998;

            }



            //c------------------------------------
            //c      fill the RASC matrix if needed
            if (COMMOD9.PENF == 4)
            {
                Helper.Write("    \n");
                Helper.Write("  Building RASC Matrix . . . \n");

                DORASC.Singleton().RunIt();
            }




            //c------------------------------   
            //CPMS    fill FADLAD matrix 
            if (COMMOD9.FB4LF == 1)
            {
                Helper.Write("    \n");
                Helper.Write("  Building FAD\\LAD Matrix . . . \n");


                //with loose coexistence rules
                DOFB4L.Singleton().RunIt();

                if (COMMOD9.STOPF == 1) goto Label998;
            }
            else if ((COMMOD9.FB4LF == 2) && (COMMOD9.NSPC > 0))
            {
                //CPMS      load FADLAD matrix from file               
                //CPMS      this could be dangerous -- matrix on file must be 
                //CPMS      suitable for generating a plausible initial solution
                //CPMS      initial solution should be conservative!
                Helper.Write("  Loading the FAD\\LAD Matrix from file. . . \n");

                //TODO:CONOP.CS
                //CALL TIMEOUT(0.40)
                //OPEN(53,FILE=FB4LFILE)
                //C    Read file into FB4L(I,J)
                for (I = 0; I < COMMOD9.NSPC; I++)
                {
                    //TODO:CONOP.CS
                    //READ(53,9109, END=9108) (FADLAD(I,J),J=1,NSPC)
                }

                //TODO:CONOP.CS
                //         file1 = COMMITQQ(53)
                //9108     CLOSE(53) 

                //CPMS      format matches that used to write unit 53 (see DOFB4L.FOR)     
                //9109     FORMAT(1X,9000(I3,1X)) 

                COMMOD9.FB4LF = 1;   //subsequently treat as FORCEFb4L="ON" 

            }



            //cpms   count the number of FAD\LAD events
            if (COMMOD9.FB4LF != 0)
            {
                COMMOD9.NFALA = 0;

                for (I = 0; I < COMMOD9.NSPC; I++)
                {
                    for (M = 0; M < COMMOD9.NSPC; M++)
                    {
                        if (I == M) continue;

                        if (COMMOD9.FADLAD[I, M] == 1) COMMOD9.NFALA = COMMOD9.NFALA + 1;

                    }
                }

                if (COMMOD9.CDF != 1)
                {
                    //TODO:CONOP.CS
                    //OPEN(11, FILE=RUNLOG, STATUS='OLD', POSITION='APPEND')
                    //WRITE(11,*) '    '
                    //WRITE(11,*) '------FAD\LAD Matrix Built-------------'
                    //WRITE(11,*) 'Number of observed FA\LA pairs: ', NFALA
                    //CLOSE(11, STATUS='KEEP')
                }
            }

            Helper.Write("  Number of observed FA\\LA pairs: {0}\n", COMMOD9.NFALA);
            Helper.Write("  Number of combinations of 2 taxa: {0}\n", COMMOD9.MXCOEX);

            Random rnd = new Random(DateTime.Now.Millisecond);
            //cpms---make sure XEVNT is not zeroed out
            while ((COMMOD9.ESTART == -1) && (Helper.ALLRZERO(COMMOD9.XEVNT)))//<=HXD
            {
                COMMOD9.XEVNT = rnd.Next(COMMOD9.NEVNT);//HACK:NOT SURE == INTRAND(NEVNT)
            }


            //cpms----------------------------------------------------
            //TODO:CONOP.CS
            //CALL TIMEOUT(0.40)

            //cpms    the first call to getstart did not randomize the
            //cpms    FAD's  -  the reason is unclear; it was
            //cpms    another example of DEC FORTRAN's handling of the
            //cpms    uninitialized arrays  --  L() in ASORT!!!
            GETSTART.Singleton().RunIt(1);
            if (COMMOD9.STOPF == 1) goto Label998;

        Label998: ;


        }

        /// <summary>
        /// if run simulated annealing procedure on CONSOLE
        /// </summary>
        /// <param name="bDrawChart"></param>
        protected void Run(bool bDrawChart)
        {
            BeforeAnneal();

            COMMOD COMMOD9 = COMMOD.Singleton();

        Label11:
            if (COMMOD9.NTRIES > COMMOD9.NSTOP) COMMOD9.PAUSF = 0;

            //c  the next set of options writes LSTPERM out to INIPERM
            //c  then reads it back in;  the second step can be cut out.
            if (((COMMOD9.INIGEN >= 2) && (COMMOD9.FIXF > 0) &&
            ((COMMOD9.PAUSF == 2) || (COMMOD9.PAUSF == 4))) ||
            ((COMMOD9.INIGEN >= 2) && (COMMOD9.PAUSF == 3)))
            {
                GETSTART.Singleton().RunIt(0);
            }
            else if ((COMMOD9.INIGEN >= 2) && (COMMOD9.PAUSF == 5))
            {
                //c      this crashes here but works at the time
                //c      LSTPERM is written out via STARTOUT
                //c   -----------------
                //c   INIPERM = LSTPERM
            }


            //HACK:OMIT MANY LINES
            COMMOD9.YSPAN = (double)(COMMOD9.maxy - 50);
            COMMOD9.XSPAN = (double)(COMMOD9.maxx - 100);
            COMMOD9.YROWS = (double)(COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS + 1);


            //cpms  initialize the penalties for exclusive taxa
            //cpms  even if not used (JSPANF.eq.0) they are updated
            //cpms  it is faster than checking the flag every time!
            COMMOD9.SPANPEN = 0.0;
            COMMOD9.ASPNPEN = 0.0;

            IsBeforeAnneal = false;

#if OUTDATA
            WritePara.writeThem();
            Environment.Exit(0);
#endif

#if REPACEPERM
            WritePara.replaceInitPerm();
#endif

#if SWT_CHECK_VIA_GETPEN
            FileStream Log = new FileStream("CheckPenalty.log", FileMode.Create);
            Trace.Listeners.Add(new TextWriterTraceListener(Log) { TraceOutputOptions = TraceOptions.Timestamp | TraceOptions.Callstack });
            Trace.AutoFlush = true;
#endif

            // TODO : input parameter for CONSOLE api -> 
            ANNEAL.Singleton().RunIt(this);

            //cpms  update the near-enough threshold:
            COMMOD9.BSTPLUS = COMMOD9.BSTPEN + (COMMOD9.BSTPEN * COMMOD9.NEAR / (double)COMMOD9.NOBS);

            if (COMMOD9.PAUSF == 5) goto Label22;

            //cpms----------------------
            //cpms     prepare lclbst to notice any improvements
            //cpms     that result from graphical output options
            COMMOD9.LCLBST = COMMOD9.BSTPEN;

            //HACK:OMIT MANY LINES

            //CPMS  prepare VALEVEL for standardized stratigraphic distances

            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                topj = Helper.PLACE(COMMOD9.NEVNT - 1, J);//<=HXD
                btmj = Helper.PLACE(1 - 1, J);//<=HXD

                //CPMS  record range of section in stratigraphic distance from 
                //CPMS  from highest to lowest placement

                COMMOD9.VALEVEL[COMMOD9.MXLVL, J] = topj - btmj;

                if (topj > btmj)
                {
                    //C      TOP
                    //CPMS   count down to first event that is not placed at the section top  
                    I = COMMOD9.NEVNT - 1;

                    while (COMMOD9.VALEVEL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[I], J], J] == topj)
                    {
                        I = I - 1;
                    }

                    rngj = I;
                    COMMOD9.LIMITS[J, 1] = COMMOD9.BSTPERM[I];

                    //c	  count down to first observed FAD
                    L = COMMOD9.NEVNT - 1;
                    while (L >= 0)
                    {
                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], J, 0] >= 0) &&
                            (COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 1))//<=HXD
                        {
                            break;
                        }

                        L = L - 1;
                    }

                    //c	 place lower limit at highest observed FAD
                    //c      or first event placed below top

                    if (L > I) COMMOD9.LIMITS[J, 1] = COMMOD9.BSTPERM[L];

                    //c      BASE:
                    //CPMS   count up to first event that is not placed at bottom of section
                    I = 1 - 1;
                    while (COMMOD9.VALEVEL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[I], J], J] == btmj)
                    {
                        I = I + 1;
                    }

                    rngj = rngj - I;
                    COMMOD9.LIMITS[J, 0] = COMMOD9.BSTPERM[I];

                    //c	 count up to first observed LAD
                    L = 0;
                    while (L < COMMOD9.NEVNT)
                    {
                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], J, 0] >= 0) &&
                            (COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 2))//<=HXD
                        {
                            break;
                        }

                        L = L + 1;
                    }

                    //c	 place lower limit at lowest observed LAD
                    //c      or first event placed above base
                    if (L < I) COMMOD9.LIMITS[J, 0] = COMMOD9.BSTPERM[L];

                    //CPMS       rngj is now the number of events not at top or bottom
                    //CPMS       add two to give the number of inter-event spaces recorded          
                    //CPMS       and record range of section as number of event spacings between
                    //CPMS       top and bottom levels 

                }

                if ((topj - btmj) == 0.0)
                {
                    COMMOD9.VALEVEL[COMMOD9.MXLVL + 1, J] = 0.0;
                }
                else
                {
                    COMMOD9.VALEVEL[COMMOD9.MXLVL + 1, J] = (double)(rngj + 2);
                }

            }//End for J

            //CPMS--------------------------------------------------------       
            //cpms   now load the COVER() array with number of sections 
            //c      that cover each level in ordinal composite sequence

            Helper.SetVal(COMMOD9.COVER, 0);

            //c  tally each section	 
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                //add 1 for all levels from base to top limits
                for (L = COMMOD9.LIMITS[J, 0]; L < COMMOD9.LIMITS[J, 1]; L++)
                {
                    COMMOD9.COVER[L] = COMMOD9.COVER[L] + 1;
                }
            }

            //CPMS   update the cullist with max and min optimal positions
            Helper.EVENTSUM(4, minc);

            //CPMS   restore original baselevels
            Helper.UNBASE();

            //CPMS   calculate the net section span
            Helper.JSPAN(COMMOD9.BSTPERM);

            if (COMMOD9.JSPANF > 0)
            {
                switch (COMMOD9.PENF)
                {
                    case -1:
                        COMMOD9.RVLPEN = COMMOD9.RVLPEN - COMMOD9.SPANPEN;
                        break;
                    case 0:
                        COMMOD9.INTPEN = COMMOD9.INTPEN - COMMOD9.SPANPEN;
                        break;
                    case 1:
                        COMMOD9.LVLPEN = COMMOD9.LVLPEN - COMMOD9.SPANPEN;
                        break;
                    case 2:
                        COMMOD9.ORDPEN = COMMOD9.ORDPEN - COMMOD9.SPANPEN;
                        break;
                    case 3:
                        COMMOD9.SPTPEN = COMMOD9.SPTPEN - COMMOD9.SPANPEN;
                        break;
                    case 4:
                        COMMOD9.RSCPEN = COMMOD9.RSCPEN - COMMOD9.SPANPEN;
                        break;
                    case 5:
                        COMMOD9.ROYPEN = COMMOD9.ROYPEN - COMMOD9.SPANPEN;
                        break;
                    case 6:
                        if (COMMOD9.FB4LF != 0) COMMOD9.SEQPEN = COMMOD9.SEQPEN - COMMOD9.SPANPEN;
                        break;
                    case 7:
                        COMMOD9.MOMPEN = COMMOD9.MOMPEN - COMMOD9.SPANPEN;
                        break;
                }
            }

            //CPMS   prepare to write reports



            if ((COMMOD9.OUTMAIN.Substring(0, 3) != "OFF") && (COMMOD9.OUTMAIN.Substring(0, 3) != "off") &&
                COMMOD9.CDF != 1)
            {
                COMMOD9.OutmainSB.Length = 0;

                COMMOD9.OutmainSB.AppendLine(COMMOD9.PROJNAME);
                COMMOD9.OutmainSB.AppendLine("__________________________________________");
                COMMOD9.OutmainSB.AppendLine(string.Format("           Number of sections: {0,12}", COMMOD9.NSCT));
                COMMOD9.OutmainSB.AppendLine(string.Format("               Number of taxa: {0,12}", COMMOD9.NSPC));
                COMMOD9.OutmainSB.AppendLine(string.Format("   Number of Mid-range events: {0,12}", COMMOD9.NMIDS));
                COMMOD9.OutmainSB.AppendLine(string.Format("    Number of unpaired events: {0,12}", COMMOD9.NOTHR));
                COMMOD9.OutmainSB.AppendLine(string.Format("       Total number of events: {0,12}", COMMOD9.NEVNT));

                if ((COMMOD9.FITOUTF == 1) && (COMMOD9.CDF != 1)) Output.FITOUT();
                if ((COMMOD9.RUNOUTF == 1) && (COMMOD9.CDF != 1)) Output.RUNOUT();
                if ((COMMOD9.SEQOUTF == 1) && (COMMOD9.CDF != 1)) Output.SEQOUT();

                //cpms    write the penalty increments and delta file
                if ((COMMOD9.INCOUTF == 1) && (COMMOD9.CDF != 1)) Output.INCROUT();

                //cpms    write the placed levels and albet file
                if ((COMMOD9.LOCOUTF == 1) && (COMMOD9.CDF != 1)) Output.LOCOUT(1);

                //cpms    write the observed levels and ab file
                if ((COMMOD9.OBSOUTF == 1) && (COMMOD9.CDF != 1)) Output.OBSOUT();

                //cpms    fill the arrays with composite section information
                //cpms    and IF CMPOUTF.eq.1 (flag is in COMPOUT)  
                //cpms    write the composite and compst file

                File.WriteAllText(COMMOD9.OUTMAIN, COMMOD9.OutmainSB.ToString());
                COMMOD9.OutmainSB.Length = 0;
            }

        Label22:

            //CPMS  check to see if solution is best-ever
            if ((COMMOD9.BESTSOL.Substring(0, 3) != "OFF") && (COMMOD9.BSTPEN == COMMOD9.VERYBEST)
                && (COMMOD9.CDF != 1))
            {
                //Output.BESTOUT(); -------------*
            }

            //CPMS  write out starting solution
            if (COMMOD9.PAUSF != 5)
            {
                if (COMMOD9.CDF != 1) Output.STARTOUT(COMMOD9.BSTPERM);
            }
            else
            {
                if (COMMOD9.CDF != 1) Output.STARTOUT(COMMOD9.LSTPERM);
                Helper.CopyArray(COMMOD9.LSTPERM, ref COMMOD9.INIPERM);
                goto Label33;
            }



            //cpms  must make next call even if (CDF.eq.1)
            //cpms  because compout fills the COMPLVL array
            //cpms  COMPOUT has internal check for (CDF.eq.1) 

            Output.COMPOUT();
            File.AppendAllText(COMMOD9.OUTMAIN, COMMOD9.OutmainSB.ToString());

            //Write out the grid of best penalties
        //if(COMMOD9.CDF!=1) CALL CURVSOUT  //TODO:

        Label33:

            //CPMS---------------------------------------------------------
            //CPMS---if PAUSES is set to rpt (repeat), return to solve call
            //CPMS	 after resetting mimimum conditions
            if (COMMOD9.PAUSF >= 2)
            {
                if (COMMOD9.ESTART == -1) COMMOD9.XEVNT = COMMOD9.RANDOM.Next(COMMOD9.NEVNT);//?

            }

            if (COMMOD9.PAUSF == 3) COMMOD9.INIGEN = 2;

            if (COMMOD9.PAUSF == 5)
            {
                COMMOD9.INIGEN = 2;
                if (COMMOD9.NBETR <= 0)
                {
                    //best-fit has not improved
                    COMMOD9.NTRIES++;
                    COMMOD9.AUTF = -1;
                }
                else
                {
                    //restart the count of null trial sets
                    COMMOD9.NTRIES = 0;
                    COMMOD9.AUTF = 1;
                }
            }


            if (COMMOD9.PAUSF >= 2) goto Label11;


            //    CALL EVENTSUM(3,minc)

            #endregion


        Label998:
            ;

        }

        /// <summary>
        /// Clear console and write output message
        /// </summary>
        public void ClearLineAndOutputMessage(string message, bool isEndline)
        {
            Console.SetCursorPosition(0, Console.CursorTop);
            Console.Write(new string(' ', Console.WindowWidth));
            Console.SetCursorPosition(0, Console.CursorTop - 1);
            Console.Write(string.Format("{0}", message));
            if (isEndline)
            {
                Console.Write("\n");
            }
        }
    }
}
