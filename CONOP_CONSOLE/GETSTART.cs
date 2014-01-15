using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CONOP.NET
{
    //C***********************************************************************
    //C     A SUBROUTINE TO GENERATE A STARTING PERMUTATION FOR
    //C       OPTIMIZATION OR CALIBRATION RUNS  
    //C
    //C                       PROGRAMMERS: PETE SADLER & BILL KEMPLE 
    //C                       LAST UPDATE: Aug 19th 2011 - for Intel 11.1
    //C
    //C***********************************************************************
    //C     THIS SUBROUTINE IS ORGANIZED AS FOLLOWS:
    //C
    //C         INIGEN is used to say how the 
    //C             initial perm is to be generated
    //C                 INIGEN = 1  build from existing section
    //C                 INIGEN = 2  READ IN FROM STARTFILE
    //C                 INIGEN = 3  Random start
    //C                 INIGEN = 4  READ IN FROM BESTFILE
    //C                 INIGEN = 5  READ IN FROM STEPFILE
    //C                 if the steptemp is to be used too, STARTT is negative 
    //C                 ----near future developments--------------
    //C                 INIGEN = 6  READ IN PARTIAL SOLUTION FROM STARTFILE
    //C                 ----far future developments---------------
    //C                 INIGEN = 7  READ PARTIAL SOLUTION FROM DIFFERENT DATA SET
    //C                             learn to translate old event numbers to new
    //C                             thus, build starting solution from a file with 
    //C                             solution for a different set of events
    //C                             again it will be a matter of found and missing
    //C                             will need OLD EVENT FILE parameter in .CFG
    //C                             or a translation table
    //C								i.e. build your own in EXCEL
    //C
    //C         THE INITIAL PERM IS BUILT IN "INIPERM"
    //C              first entry is IROWS# of event with rank1
    //C             second entry is IROWS# of event with rank2
    //C             etc.
    //C----------------------------------------------------------------------
    //C         when building from existing section, INIGEN = 1, 
    //C         A SECTION NUMBER IS SPECIFIED IN "JSTART"
    //C             (IT FORMS THE CORE OF THE INITIAL PERMUTATION)
    //C
    //C             events extending down out of JSTART are placed first
    //C             missing firsts are next
    //C             next are the events recovered in JSTART, in that order
    //C             missing lasts are next
    //C             events extending up out of the section are placed last
    //C
    //C             all but the founds are random within their class
    //C
    //C
    //C     WE:
    //C         1. extract "JSTART" (ROW,LEVEL) data from ISTATIC.
    //C             <missings> - LEVELS = 0
    //C             <founds>   - LEVELS > 0
    //C         2. place <missing> firsts above <downs> randomly
    //C         3. sort <founds> and place them above <missing> firsts
    //C         4. place <missing> lasts above <founds> randomly
    //c
    //CPMS-----Counted as firsts and lasts
    //CPMS-----To be stacked at bottom and top respectively
    //CPMS-----ordering by partial ordering matrix does not matter
    //CPMS-----because these events are placed in deirectioon of their allowed/unconstrained moves
    //CPMS       1 paired range event - FAD first - may move down - free to be early in sequence
    //CPMS       2 paired range event - LAD last  - may move up   - free to be late in sequence
    //c
    //CPMS-----Counted among "OTHER" events:
    //CPMS-----To be arranged in middle, between unordered firsts and lasts
    //CPMS-----togther with ordered firsts and lasts
    //CPMS      -3 unpaired BOB                    - free to be late or early
    //CPMS      -2 unpaired disappearance DIS      - free to be late
    //CPMS      -1 unpaired appearance APP         - free to be early
    //CPMS       0 
    //CPMS       3 unpaired range event  e.g. MID acme      - free between FAD and LAD
    //CPMS       4 unique marker horizon e.g. ash bed ASH   - must be ordered wrt 4s and 5s
    //CPMS       5 AGE dated horizon                        - must be ordered wrt 4s and 5s
    //CPMS       6 SEQ non-unique repetitive marker
    //CPMS      10 spacer  e.g. GAP hiatus   ?? should be 0
    //CPMS      11 floor/MAX of BOXed event pair            - must be ordered wrt 4s 5s 12s
    //CPMS      12   top/MIN of BOXed event pair            - must be ordered wrt 4s 5s 11s
    //CPMS-----------
    //*     NOTE: --  we check and adjust to make sure that coexistences
    //*                 observed in the data are honored in INIPERM.
    //C
    //C***********************************************************************
    public class GETSTART
    {
        #region Singleton

        private static GETSTART g_singleton = null;
        private GETSTART() { }
        public static GETSTART Singleton()
        {
            if (g_singleton == null)
                g_singleton = new GETSTART();

            return g_singleton;
        }

        #endregion

        public void RunIt(int OUT)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            //CPMS  arrays and variables for sorting/randomizing:
            double UNIFRN;
            double[] U = new double[COMMOD9.NEVNT];
            int[] L = new int[COMMOD9.NEVNT];
            int[] L1 = new int[COMMOD9.NEVNT];
            int[] X = new int[COMMOD9.NEVNT];

            //CPMS  arrays for events found in starting section:
            int[] FOUNDS = new int[COMMOD9.NEVNT];

            //cpms  if (inigen.eq.5), founds has IROWS order and tracks the found (1)
            //cpms  versus missing (2) events
            //CPMS  arrays for missing events:
            int[] MFIRSTS = new int[COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS];
            int[] MLASTS = new int[COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS];
            int[] MOTHERS = new int[Math.Max(1, COMMOD9.NEVNT - COMMOD9.TWOSPC)];

            int ISWAP, TEMP;
            int SPC, TYP, RNK, ROW;
            int NF, NMF, NML, FILLED, NMO;
            int I, K, M;

            //cpms  out determines whether initial perm is written
            //cpms  to the runlog (1) or not (0)

            UNIFRN = 0.0;

            //cpms  initialize arrays
            Helper.SetVal(U, 0);
            Helper.SetVal(L, 0);
            Helper.SetVal(L1, 0);
            Helper.SetVal(X, 0);//<=HXD

            Helper.SetVal(FOUNDS, 0);
            Helper.SetVal(MFIRSTS, 0);
            Helper.SetVal(MLASTS, 0);
            Helper.SetVal(MOTHERS, 0);

            //c   Initialize more for Intel 11.1
            I = 0;
            K = 0;
            M = 0;
            NF = 0;
            SPC = 0;
            TYP = 0;
            RNK = 0;
            ROW = 0;
            NMO = 0;
            NMF = 0;
            NML = 0;
            TEMP = 0;
            ISWAP = 0;
            FILLED = 0;

            //CPMS---------------------------------------------------------------------
            //C   IF THE STARTING PERM(S) TO BE GENERATED FROM THE DATA (INIGEN = 1)
            //C         we'll perform the rather lengthy procedure following. Otherwise,
            //C         we'll use one of the procedures further down.
            //C----------------------------------------------------------------------- 
            if ((COMMOD9.INIGEN == 1) || (COMMOD9.INIGEN == 3))
            {
                //C   **BUILD FROM EXISTING SECTION (1)*** 
                //C   **OR BUILD AT RANDOM          (3)***
                //C   For a random start, treat initial section as if empty

                if (COMMOD9.INIGEN == 1)
                {
                    if (OUT == 1) Helper.Write("  Building Initial Sequence from Section{0}\n", COMMOD9.JSTART);

                }
                else
                {
                    if (OUT == 1) Helper.Write("  Building Random Initial Sequence  .  .  .\n");

                }


                //C  -------------------------------------      
                //C  1. ZEROIZE INIPERM AND THE COUNTERS
                //C  -------------------------------------
                Helper.SetVal(COMMOD9.INIPERM, 0);

                FILLED = 0;
                //CPMS    counter for developing initial permutation       
                //CPMS    counters for events (TYPE, LEVEL):   
                NF = 0;
                //CPMS    number of found events (>0,>0)      
                NMO = 0;
                //CPMS    number of missing unpaired events (>2,0) and (<1,0)     
                NMF = 0;
                //CPMS    number of missing firsts (1,0)     
                NML = 0;
                //CPMS    Number of missing lasts (2,0)      
                //C   ------------------------------------------      
                //C   --TOP OF LOOP-----------------------------      
                //C   ------------------------------------------      
                //C     READ THE SECTION JSTART data FROM ISTATIC
                //C     fill out the appropriate arrays and
                //C     COUNT THE NUMBER OF each kind AS WE GO
                //C
                //C     this is the top of a large loop that goes through a case
                //C     type routine to determine the status of all events within 
                //C     the section JSTART, extract information on events that are
                //C     present, and classify events that are not. event row#'s are
                //C     placed in arrays by class for subsequent combination.
                //C      
                //C   ----------------------- 


                for (I = 0; I < COMMOD9.IRCOUNT; I++)
                {
                    //C     --OVERVIEW-------------------------------------------      
                    //C     we'll handle found events then missings  
                    //C       in the order of expected frequency then downs and ups
                    //C
                    //C     the LEVELS value in ISTATIC for each event is its classification
                    //C       >  0 - found   (values may be negative but not levels)
                    //C       =  0 - missing
                    //C
                    //C      the first IF is and if-then-else based on this
                    //C      the endif is at the bottom of the loop (just above 2000)    
                    //C
                    //C      -------------------------------------------------------      
                    //C      --FOUNDS-----------------------------------------------
                    //C       founds (record row# in FOUNDS and level in X)
                    //C      ----------------------------------------------

                    //HACK:
                    //NF = -1;
                    //NMF = -1;
                    //NML = -1;
                    //NMO = -1;

                    if (COMMOD9.ISTATIC[I, COMMOD9.JSTART, 0] > -1)//<=HXD
                    {
                        if (COMMOD9.INIGEN == 1)
                        {
                            NF = NF + 1;
                            FOUNDS[NF - 1] = I;
                            X[NF - 1] = COMMOD9.ISTATIC[I, COMMOD9.JSTART, 0];
                            //CPMS   otherwise: INIGEN.EQ.3; 
                            //CPMS   treat the founds as missing to build a random order
                        }
                        else if (COMMOD9.IROWS[I, 1] == 1)
                        {
                            NMF = NMF + 1;
                            MFIRSTS[NMF - 1] = I;
                        }
                        else if (COMMOD9.IROWS[I, 1] == 2)
                        {
                            NML = NML + 1;
                            MLASTS[NML - 1] = I;
                        }
                        else if ((COMMOD9.IROWS[I, 1] > 2) || (COMMOD9.IROWS[I, 1] < 1))
                        {
                            NMO = NMO + 1;
                            MOTHERS[NMO - 1] = I;
                        }

                        //C  ---------------------------      
                        //C  --MISSINGS-----------------      
                        //C  --------------------------- 
                    }
                    else if (COMMOD9.ISTATIC[I, COMMOD9.JSTART, 0] == -1)//<=HXD
                    {
                        //C   -----------------------------------      
                        //C   missing firsts
                        //C   --------------
                        if (COMMOD9.IROWS[I, 1] == 1)
                        {
                            NMF = NMF + 1;
                            MFIRSTS[NMF - 1] = I;
                        }
                        //C   -----------------
                        //C   missing lasts
                        //C   ------------------------------- 
                        else if (COMMOD9.IROWS[I, 1] == 2)
                        {
                            NML = NML + 1;
                            MLASTS[NML - 1] = I;
                        }
                        //C    ----------------------------
                        //C    missing unpaired horizions
                        //C    ---------------------------
                        else if ((COMMOD9.IROWS[I, 1] > 2) || (COMMOD9.IROWS[I, 1] < 1))
                        {
                            NMO = NMO + 1;
                            MOTHERS[NMO - 1] = I;
                        }
                    }
                }

                //C -----------------------------------------------------------------------      
                //C **PLACE EVENTS*********************************************************      
                //C before we place each kind of event, we'll sort them as appropriate
                //C -----------------------------------------------------------------------      
                //C the form of INIPERM is:
                //C   
                //C                    rank        entries are row#'s
                //C                            _______________________________
                //C low                  1    |                               |
                //C                     .     | missing firsts in random order|
                //C                    NMF    |_______________________________|
                //C                  NMF+1    |                               |
                //C                     .     | founds (and unpaireds)        |
                //C                 NMF+NF    |_______________________________|
                //C               NMF+NF+1    |                               |
                //C                     .     | missing lasts in random order |
                //C high        NMF+NF+NML    |_______________________________|
                //C
                //C----------------------------------------------------------  

                //C-----------------------------------------------------------------------      
                //C--MISSING FIRSTS-------------------------------------------------------      
                //C   place missing firsts
                //C-------------------------
                //C   MISSING FIRST PROCESSING. IF NMF IS NOT ZERO WE'LL CREATE "L1"
                //C   A NMF VECTOR OF THE INTEGERS 1 TO NMF IN RANDOM ORDER. 
                //C   THE first NMF entries in INIPERM WILL GET 
                //C   the corresponding entry from MFIRSTS
                //C-----------------------------------------

                Random rnd = new Random(DateTime.Now.Millisecond);
                

                if (NMF > 0)
                {

                    for (I = 0; I < NMF; I++)
                    {
                        UNIFRN = rnd.NextDouble();//HACK: CALL RANDOM_NUMBER(UNIFRN) IN FORTRAN
                        U[I] = UNIFRN;
                    }

                    Helper.ASORT(ref U, ref L1, NMF);

                    for (K = 0; K < NMF; K++)
                    {
                        //Helper.Write("INIPERM[{0}]={1} =>", K, COMMOD9.INIPERM[K]);
                        COMMOD9.INIPERM[K] = MFIRSTS[L1[K]];
                        //Helper.Write("{0}\n",COMMOD9.INIPERM[K]);
                    }


                    FILLED = NMF;
                }

                //C     --FOUNDS---------     
                //C      place founds 
                //C      if INIGEN.EQ.3 there are no founds
                //C      for now, if there are any unpaired events, INIGEN is set to 3
                //C      -------------------------------------------------------------         
                //C      USE IASORT TO CREATE L VECTOR POINTING TO THE ROWS OF X
                //C      BY THEIR RANKS. L(1) CONTAINS THE ROW NUMBER OF THE
                //C      SMALLEST ENTRY IN X AND SO ON. place the NF row numbers in
                //C      INIPERM in this order.
                //C      ---------------------- 
                if (NF > 0)
                {
                    Helper.IASORT(ref X, ref L, NF);

                    for (K = 0; K < NF; K++)
                    {
                        COMMOD9.INIPERM[K + FILLED] = FOUNDS[L[K]];
                    }

                    FILLED = FILLED + NF;

                }

                //C      --OTHERS----------------    
                //C         place missing MOTHERS 
                //C         if INIGEN.EQ.3 there are no founds
                //*         for now, if there are any unpaired events, INIGEN is set to 3
                //C      -----------------------------------------------        
                //CPMS     use PTLORD to determine acceptable sequence
                //C      ---------------------------------------------   
                if (NMO > 0)
                {
                    //CPMS     currently this means "if there are any unpaired events"
                    //CPMS     place all unpaired events 
                    //CPMS     they are at the top of PTLORD and IROWS by INPUT design 
                    //CPMS    -----------
                    //CPMS    ---1. PLACE UNPAIRED EVENTS WITH lower-than > higher-than
                    for (I = COMMOD9.NEVNT - (COMMOD9.NSPC * 2) - 1; I >= 0; I--)
                    {
                        //CPMS       loop down through all possible numbers of "lower-than"s
                        //CPMS       from the maximum down to 1
                        for (ROW = 0; ROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); ROW++)
                        {
                            //loop through all unpaired events
                            if ((COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2)] == I) &&
                                (COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2)] >
                                COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2) + 1]))
                            {
                                //CPMS           if the number of "lower-than"s equals the current count down
                                //CPMS           and exceeds the number of "higher-than"s,  place!
                                FILLED = FILLED + 1;
                                COMMOD9.INIPERM[FILLED - 1] = ROW;//<=HXD
                            }
                        }
                    }


                    //CPMS    -----------
                    //CPMS    ---2. PLACE UNPAIRED EVENTS WITH lower-than = higher-than
                    for (ROW = 0; ROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); ROW++)
                    {

                        //c         loop through all unpaired events 
                        if (COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2)] ==
                            COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2) + 1])
                        {
                            //CPMS         if the number of "lower-than"s equals the 
                            //CPMS         number of "higher-than"s (including 0's),  place!
                            FILLED = FILLED + 1;
                            COMMOD9.INIPERM[FILLED - 1] = ROW;//<=HXD
                        }
                    }

                    //CPMS    ---3. PLACE UNPAIRED EVENTS WITH lower-than < higher-than
                    for (I = 0; I < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); I++)
                    {
                        //CPMS      loop up through all possible numbers of "higher-than"s
                        //CPMS      from 1 up to the maximum     
                        for (ROW = 0; ROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); ROW++)
                        {
                            //c          loop through all unpaired events
                            if ((COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2) + 1] == I) &&
                            (COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2) + 1] >
                            COMMOD9.PTLORD[ROW, COMMOD9.NEVNT - (COMMOD9.NSPC * 2)]))
                            {
                                //CPMS         if the number of "higher-than"s equals the current count up
                                //CPMS         and exceeds the number of "lower-than"s,  place!

                                FILLED = FILLED + 1;
                                COMMOD9.INIPERM[FILLED - 1] = ROW;//<=HXD

                            }
                        }
                    }

                }

                //C --MISSING LASTS-----------     
                //C   place missing lasts
                //C   ----------------------      
                //C   MISSING last PROCESSING. IF NML IS NOT ZERO WE'LL CREATE "L1"
                //C   A NML VECTOR OF THE INTEGERS 1 TO NML IN RANDOM ORDER. 
                //C   THE first NML entries in INIPERM WILL GET 
                //C   the corresponding entry from MLASTS
                //C   ------------------------------------ 
                if (NML > 0)//<=HXD
                {

                    for (I = 0; I < NML; I++)
                    {
                        UNIFRN = rnd.NextDouble();
                        U[I] = UNIFRN;
                    }

                    Helper.ASORT(ref U, ref L1, NML);

                    for (K = 0; K < NML; K++)
                    {
                        COMMOD9.INIPERM[K + FILLED] = MLASTS[L1[K]];
                        //Helper.Write("U[L1[{0}]]={1}\n", K, U[L1[K]]);
                    }

                    FILLED = FILLED + NML;
                }

                //C      ---------------------------------------      
                //C      check to make sure the solution is full
                //C      ---------------------------------------      
                if (FILLED != COMMOD9.IRCOUNT)
                {
                    Helper.Write("ERROR IN GETSTART!!\n");
                    Helper.Write("INIPERM WRONG SIZE\n");
                    Helper.Write("IRCOUNT =   {0}\n", COMMOD9.IRCOUNT);
                    Helper.Write("FILLED =  {0}\n", FILLED);

                    COMMOD9.STOPF = 1;
                    goto Label8888;
                }

            }
            else if ((COMMOD9.INIGEN == 2) || (COMMOD9.INIGEN > 3))
            {
                //C      ---------------------------------------      
                //C      **INITIAL SOLUTION FROM FILE -- INIGEN = 2  ***************************     
                //C      -----------------------------------------------------------------------      
                //*      USE IF WE ARE GOING TO READ IN AN INITIAL SOLUTION FROM FILE
                //C
                //C      the initial solution is in the following format:
                //C
                //C             |species|type|rank|
                //C
                //C       the subroutine FINDROW uses the species and type to find the row#
                //C         in IROWS and other arrays for this event.
                //C
                //C         if there is no match in IROWS, an error is reported in RUNLOG
                //C       -----------------------------------------------------------------------      
                //*         eventually there will probably be yet another section for
                //*         a completely random start (see prepare)
                //C       ----------------------------------------------------------------------- 

                string LOADFILE = string.Empty;

                switch (COMMOD9.INIGEN)
                {
                    case 2:
                    case 6:
                        //TODO:GETSTART.CS
                        // OPEN(14, FILE=INITSOL)
                        LOADFILE = COMMOD9.INITSOL;
                        if ((COMMOD9.AUTF == 0) && (OUT == 1))
                        {
                            Helper.Write("  Loading Last Solution from File . . . \n");
                        }
                        break;
                    case 4:
                        //TODO:GETSTART.CS
                        //OPEN(14, FILE=BESTSOL)
                        Helper.Write("  Loading Best Solution from File . . . \n");
                        break;
                    case 5:
                        if (COMMOD9.CONTF == 1)
                        {
                            Helper.Write("  Recalling Interrupted Run . . . \n");
                            //cpms load the previous best-
                            //cpms but BESTSOL was probably NOT written during run!
                            //TODO:GETSTART.CS
                            //    OPEN(14, FILE=STEPSOL)
                            //12  READ(14,*,END=13) SPC, TYP, RNK
                            Helper.FINDROW(SPC, TYP, ref ROW);

                            if (ROW != -1)
                            {
                                COMMOD9.BSTPERM[RNK] = ROW;
                            }
                            else
                            {
                                //TODO:GETSTART.CS
                                //? unit 11 opened?
                                //IF(CDF.ne.1) WRITE(11, 1011) SPC, TYP, RNK
                            }

                            //TODO:GETSTART.CS
                            //GO TO 12
                            //CLOSE(14)


                            //cpms  reset the best penalty variables
                            if (COMMOD9.PENF <= 1)
                            {
                                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.BSTPEN);
                            }
                            else if (COMMOD9.PENF >= 2)
                            {
                                Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.BSTPEN);
                            }

                            //cpms  next line was missing in previous trial
                            Helper.DOTOO();

                            if (COMMOD9.AUTF == 0)
                            {
                                COMMOD9.BS2PEN = COMMOD9.TOOPEN;
                                COMMOD9.SQBST = COMMOD9.SQPEN;
                                COMMOD9.SHBST = COMMOD9.SHPEN;
                                COMMOD9.TSBST = COMMOD9.TSPEN;
                            }

                            //cpms get the starting temperature from file
                            //TODO:GETSTART.CS
                            //OPEN(52, FILE=STEPTMP)
                            //  READ(52,*) STARTT
                            //CLOSE(52)
                            //cpms  cannot start from Step Solution until the previous
                            //cpms  lines provide a bug-free way to load BSTPERM and BSTPEN
                            //cpms  the problems cause animated range chart to break up

                            //TODO:GETSTART.CS
                            //OPEN(14, FILE=STEPSOL)
                        }
                        else
                        {
                            Helper.Write("  Loading Step Solution from File . . . \n");
                            //TODO:GETSTART.CS
                            //OPEN(14, FILE=STEPSOL)
                        }

                        break;
                }


                //TODO:GETSTART.CS
                //CALL TIMEOUT(0.40)

                //cpms      IF (INIGEN.eq.6)
                //cpms        check that all the unpaired events are present
                //cpms        if not - bail out with message for now
                //cpms        eventually - read PARTLORD and learn to insert
                //cpms        between proper events
                //cpms        
                //cpms        for now - missing events will be paired FAD-LADs
                //cpms      ENDIF

                FILLED = 0;

                //TODO:GETSTART.CS
                //22     READ(14,*,END=23) SPC, TYP, RNK
                TextReader reader = new StreamReader(File.Open(LOADFILE, FileMode.Open));
                string dataLine = null;
                while ((dataLine = reader.ReadLine()) != null)
                {
                    string[] dataRowString = dataLine.Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries);
                    SPC = int.Parse(dataRowString[0].ToString());
                    TYP = int.Parse(dataRowString[1].ToString());
                    RNK = int.Parse(dataRowString[2].ToString());
                    Helper.FINDROW(SPC-1, TYP, ref ROW);

                    FILLED = FILLED + 1;

                    //cpms   IF (INIGEN.eq.6)
                    //cpms   need to maintain checklist of FOUND events 
                    //cpms   1,0 in IROWS order 

                    if (ROW != -1)
                    {
                        COMMOD9.INIPERM[RNK-1] = ROW;//<=hxd
                    }
                    else
                    {
                        if (COMMOD9.CDF != 1)
                        {
                            //TODO:GETSTART.CS
                            //WRITE(11, 1010) SPC, TYP, RNK
                        }
                    }
                }
                reader.Close();

                //TODO:GETSTART.CS
                //       GO TO 22
                //23     CLOSE(14) 


                //cpms   IF (INIGEN.eq.5)
                //cpms     INIPERM will have gaps at bottom for missing events
                //cpms     shift contents down by number of missing FADs
                //cpms     place missing FADs at start in any order
                //cpms     place missing LADs at end in any order
                //cpms     eventually, learn to insert missing unpaired events
                //cpms       shifting up-one is easy (from insert position)
                //cpms       finding insert position from PARTLORD is the trick
                //cmps       ?need to read through INIPERM
                //cpms       compare scores in PARTLORD watch for match or switch from gt to lt
                //cpms   ENDIF


                if (FILLED != COMMOD9.NEVNT)
                {
                    //TODO:GETSTART.CS
                    //            WRITE(*,1009) NEVNT, FILLED
                    // 1009       FORMAT(1X,I6,' events expected, but ',I6, ' in start file')
                    //            WRITE(*,*)'size of start file does not match this problem'
                    //            WRITE(*,*)'  '
                    //c            WRITE(*,*)'Enter key terminates run'
                    //c            READ(*,*)
                    COMMOD9.STOPF = 1;
                    goto Label8888;
                }
            }

            //TODO:GETSTART.CS
            // 1010  FORMAT(1X,'GETSTART - EVENT IN INITSOL FILE NOT IN IROWS, ',/,
            //     1       '  SPC=  ', I4,'  TYP=  ',I4,'  RNK =  ', I4,/)
            // 1011  FORMAT(1X,'GETSTART - EVENT IN BESTSOL FILE NOT IN IROWS, ',/,
            //     1       '  SPC=  ', I4,'  TYP=  ',I4,'  RNK =  ', I4,/)


            //C-----------------------------------------------------------------------      
            //C       **COEXISTENCE CHECKING*************************************************
            //C        6. IF COEXISTENCES OBSERVED IN THE DATA ARE TO BE PRESERVED, WE MUST
            //C         TEST THE INITIAL PERMUTATION. IF ANY COEXISTENCES OBSERVED IN THE
            //C         DATA ARE PRECLUDED, THE LAST OF THE LOWER SPECIES AND THE FIRST
            //C         OF THE UPPER SPECIES ARE INTERCHANGED SO THE SPECIES OVERLAP.
            //C         COEXISTANCE IS PRECLUDED IF THE FIRST OF ONE SPECIES IS
            //C         LATER IN TIME (HIGHER RANK) THAN THE LAST OF THE OTHER
            //C       ---------------------------------------------------------      
            //CPMS     IF((INIGEN.ne.2).and.(INIGEN.lt.4)) THEN
            //CPMS     EVEN IF THE SOLUTION IS LOADED FROM FILE
            //CPMS     IT MUST STILL OBEY THE COEXISTENCE RULES
            //C        FOR AD HOC COEX RULES, MAKE A COEX FILE TO MATCH!!
            //C        for debugging the coexistence checker
            //C        -------------------------------------      
            //c        IF(LOGGETSTART.EQ.1) THEN
            //c          WRITE(12,7372)
            //c        END IF
            //C        -------------------------------- 
            if (COMMOD9.COXSTF != 4)
            {
                //c  If their are any coex rules to check
                for (I = 0; I < COMMOD9.IRCOUNT - 1; I++)
                {
                    //C  --------------------------------------      
                    //C  check each event to see if it's a last
                    //C  ----------------------------------      
                    if (COMMOD9.IROWS[COMMOD9.INIPERM[I], 1] == 2)
                    {
                        //C   --------------------------------      
                        //C   if YES, check each event above to see if it's a first
                        //C   -----------------------------------------------------
                        ISWAP = 0;
                        M = I + 1;

                        while ((M < COMMOD9.IRCOUNT) && (ISWAP == 0))//<=HXD
                        {
                            if (COMMOD9.IROWS[COMMOD9.INIPERM[M], 1] == 1)//<=HXD
                            {
                                //C   -----------------------------------------
                                //C   a FAD above a LAD precludes coexistence, 
                                //C   if coexistence is mandated by COXSTF 
                                //C   make the swap now; also reverse any LADi before FADi
                                //C   situations that may have escaped prior notice
                                //C   ----------------------------------------------
                                if ((COMMOD9.COEXST[COMMOD9.IROWS[COMMOD9.INIPERM[I], 2],
                                    COMMOD9.IROWS[COMMOD9.INIPERM[M], 2]] >= COMMOD9.COXSTF) ||
                                    (COMMOD9.IROWS[COMMOD9.INIPERM[M], 0] ==
                                    COMMOD9.IROWS[COMMOD9.INIPERM[I], 0]))
                                {

                                    TEMP = COMMOD9.INIPERM[M];
                                    COMMOD9.INIPERM[M] = COMMOD9.INIPERM[I];
                                    COMMOD9.INIPERM[I] = TEMP;
                                    ISWAP = 1;
                                }
                                else
                                {
                                    M = M + 1;
                                }
                            }
                            else
                            {
                                M = M + 1;
                            }
                        }
                    }
                }
            }
            else
            {
                //cpms     if there are no coex rules, check only FAD-LAD order
                //cpms     these cannot be violated, even from a file start 
                for (I = 0; I < COMMOD9.IRCOUNT - 1; I++)
                {
                    //C   ----------------------------------      
                    //C   check each event to see if it's a last
                    //C   ---------------------------------- 
                    if (COMMOD9.IROWS[COMMOD9.INIPERM[I], 1] == 2)
                    {
                        //C  -------------------------------------------------------      
                        //C  if YES, check each event above to see if it's the mate
                        //C  ------------------------------------------------------
                        ISWAP = 0;
                        M = I + 1;

                        while ((M < COMMOD9.IRCOUNT) && (ISWAP == 0))//<=HXD
                        {
                            //C  if the mates are out of order, swap them
                            if ((COMMOD9.IROWS[COMMOD9.INIPERM[M], 1] == 1) &&
                                (COMMOD9.IROWS[COMMOD9.INIPERM[M], 0] ==
                                COMMOD9.IROWS[COMMOD9.INIPERM[I], 0]))
                            {
                                TEMP = COMMOD9.INIPERM[M];
                                COMMOD9.INIPERM[M] = COMMOD9.INIPERM[I];
                                COMMOD9.INIPERM[I] = TEMP;
                                ISWAP = 1;
                            }
                            else
                            {
                                M = M + 1;
                            }
                        }//End while
                    }//End if
                }//End for I

            }//End if


            //TODO:GETSTART.CS
            //7372 FORMAT(1X,'coexistence violation in raw initial perm',/,
            //    1   'lower: SPC    TYP    RNK  upper:   SPC    TYP    RNK')
            //7371 FORMAT(6X,3(I4,2x),6X,3(I4,2x))

            //cpms---------------------------------
            //C     end of coexistence checking
            //C     ------------------------------------

            if ((OUT == 1) && (COMMOD9.CDF != 1))
            {
                //TODO:GETSTART.CS
                //   OPEN(11, FILE=RUNLOG, STATUS='OLD', POSITION='APPEND')    
                //c  by event and type
                //   WRITE(11,8001) 

                for (M = 0; M < COMMOD9.NEVNT; M++)
                {
                    //TODO:GETSTART.CS
                    //WRITE(11,8003)M,IROWS(INIPERM(M),1),IROWS(INIPERM(M),2)
                }

                //TODO:GETSTART.CS
                //WRITE(11,8004) NEVNT

                //8001  FORMAT(1X,'THE INITIAL PERM IS',/,1X,'RANK   SPECIES TYPE',/)
                //8003  FORMAT(1X,3I7)    
                //8004  FORMAT(1X, 'NEVNT =  ', I7,/) 

                //TODO:GETSTART.CS
                //CLOSE(11, STATUS='KEEP')
            }


            //for (int n = 0; n < COMMOD9.NEVNT; n++)
            //{
            //    Helper.Write(string.Format(" {0,7}  {1,7}  {2,7}\n",
            //        COMMOD9.IROWS[COMMOD9.INIPERM[n], 0] + 1,
            //        COMMOD9.IROWS[COMMOD9.INIPERM[n], 1],
            //        n + 1));
            //}

           
               

        Label8888:
            return;

        }

      

        private void PrintSectionEvent()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            int N;
            for (N = 0; N < COMMOD9.NEVNT; N++)
            {
                int Lv=COMMOD9.ISTATIC[COMMOD9.INIPERM[N],0,0];
                if(Lv>=0)
                {
                    Helper.Write("Level={0},VALEVEL={1}\n",Lv,COMMOD9.VALEVEL[Lv,0]);
                }
            }

        }

        private void PrintMaxPen()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            int M, N;
            COMMOD9.MAXPEN = 0.0;
            if (COMMOD9.PENF < 2)
            {
                for (M = 0; M < COMMOD9.NSCT; M++)
                {
                    for (N = 0; N < COMMOD9.NEVNT; N++)
                    {
                        //if not found, cycle, else calculate penalty
                        if (Helper.IORZERO(COMMOD9.INIPERM[N], M)) continue;

                        if (Math.Abs(COMMOD9.IROWS[COMMOD9.INIPERM[N], 1]) == 1)
                        {
                            if (COMMOD9.PENF == 0)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[COMMOD9.INIPERM[N], M, 1] *
                                    (COMMOD9.VALEVEL[COMMOD9.ISTATIC[COMMOD9.INIPERM[N], M, 0], M] - 1000));
                            }
                            else if (COMMOD9.PENF == 1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[COMMOD9.INIPERM[N], M, 1] *
                                    (double)(COMMOD9.ISTATIC[COMMOD9.INIPERM[N], M, 0] - 1));//HACK:
                            }
                            else if (COMMOD9.PENF == -1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[COMMOD9.INIPERM[N], M, 1] *
                                    (double)(COMMOD9.ELEVEL[COMMOD9.ISTATIC[COMMOD9.INIPERM[N], M, 0], M] - 1));//HACK:
                            }
                        }

                        if (Math.Abs(COMMOD9.IROWS[COMMOD9.INIPERM[N], 1]) == 2)
                        {
                            if (COMMOD9.PENF == 0)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[COMMOD9.INIPERM[N], M, 0] *
                                    (COMMOD9.VALEVEL[COMMOD9.HLEVEL[M], M] -
                                    COMMOD9.VALEVEL[COMMOD9.ISTATIC[COMMOD9.INIPERM[N], M, 0], M]));//HACK:
                            }
                            else if (COMMOD9.PENF == 1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[COMMOD9.INIPERM[N], M, 0] *
                                    (double)(COMMOD9.HLEVEL[M] - COMMOD9.ISTATIC[COMMOD9.INIPERM[N], M, 0]));//HACK:
                            }
                            else if (COMMOD9.PENF == -1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[COMMOD9.INIPERM[N], M, 0] *
                                    (double)(COMMOD9.ELEVEL[COMMOD9.HLEVEL[M], M] -
                                    COMMOD9.ELEVEL[COMMOD9.ISTATIC[COMMOD9.INIPERM[N], M, 0], M]));//HACK:
                            }
                        }
                    }//End for N
                }//Enf for M
            }//End if

            Helper.Write("MaxPen={0}\n", COMMOD9.MAXPEN);
        }
    }
}
