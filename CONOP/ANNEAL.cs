using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.IO;
#if SWT_CHECK_VIA_GETPEN
using System.Diagnostics;
#endif

using CONOP;

namespace CONOP.NET
{
    //C***********************************************************************
    //C     A SUBROUTINE TO FIND A PERMUTATION (solution) and locations in the
    //C         sections THAT MINIMIZE THE OVERALL PENALTY using the
    //C         SIMULATED ANNEALING ALGORITHM
    //C
    //C                       PROGRAMMERS:  PETE SADLER & BILL KEMPLE
    //C                       LAST UPDATE:  March 14th 2001
    //C
    //C***********************************************************************
    //C   VARIABLES:
    //C         the initial solution is in INIPERM writen by GETSTART
    //C         the SA starting temperature is in STARTT
    //C         the SA cooling ratio is in R
    //C         the number of passes, outer SA loop (# temp drops) is NOUTER             
    //C         the number of passes, inner SA loop (constant temp) is NINNER
    //C      current solution:
    //C         the current solution is in PERM   (see BSTPERM)
    //C         the penalty for the current solution is in PEN   (see BSTPEN)
    //C         the 2ndary penalty for the current solution is in TOOPEN  (see BS2PEN)
    //C         the placements for the current solution are in SCTSOL
    //C      next candidate solution:
    //C         the next candidate solution is built temporarily in PERM
    //C             if it is rejected, PERM is restored to its previous state
    //C         the penalty for the candidate solution is in NXTPEN
    //C             their difference (new - old) is DELTA
    //C         the placements for the candidate solution are in HSCTSOL
    //C      best solution
    //C         the best solution so far is in BSTPERM
    //C         the total penalty for the current best solution is in BSTPEN
    //C         the secondary component of the current best is in BS2PEN 
    //C      last action
    //C         the last action taken by the SA algorithm is in LAST
    //C            LAST = 1, accepted new PERM
    //C            LAST = 0, did not accept new PERM 
    //c
    //cpms   SOLVEF is a flag set by prepare according to CONOP3.CFG
    //cpms      SOLVEF = 0   anneal with partial reoptimization
    //cpms     [SOLVEF = 1]  anneal with full reoptimization
    //cpms                   option only possible with small neighborhood
    //cpms                   OPTION REMOVED WITH VERSION 6.0
    //cpms      SOLVEF = 2   greedy algorithm
    //cpms      SOLVEF = 3   simulated tempering
    //cpms      SOLVEF = 4   squeezing (squashing)
    //cpms		  SOLVEF = 5   squealing
    //cpms      SOLVEF = 6   shrinking
    //cpms      SOLVEF = 7   shrealing
    //cpms      SOLVEF = 8   stacking  (teasing)
    //cpms      SOLVEF = 9   stealing  (anneasing)
    //c
    //c
    //c         ??LST --  used for secondary penalties that can be 
    //c                   updated by partial reoptimization
    //c                   ONLY TEASE(COEX) so far
    //c     
    //c         newpen resets the penalty   HPEN = 0.0
    //c         nwdmpen does not            HPEN = PEN (- TOOPEN when active)
    //c             therefore secondary penalty must be extracted first
    //C
    //C  SUBROUTINES CALLED:
    //C
    //C     the following subroutines need to know problem specific info:
    //C         the penalty for the initial solution and the starting placements
    //C             are found using GETPEN 
    //CPMS         the neighbor (proposed solution) is found using ???NABR
    //CPMS         the penalty and placements for the subsequent solution are
    //CPMS             found using NEWPEN
    //CPMS      NEWPEN can calculate the penalty faster
    //CPMS         but only if a penalty has been determined for the previous perm
    //CMPS      DEMPEN does not make placements
    //CPMS         - it bases the penalty upon sequence changes only  
    //CPMS         - so GETPEN must be used for placements after the anneal is done 
    //CPMS      NWDMPEN can be used after DEMPEN has determined intial penalty
    //CPMS         - it is a reoptimization routine
    //C     the following subroutines are generic:
    //C         the MS rn generator is RANDOM
    //C         the seed is set using SEED
    //C***********************************************************************
    //*     
    //*     several lines test for rungrf.eq.4  OR  gridf.eq.1
    //*     gridf should take care of rungrf.eq.4 (see prepare.for)
    //*     
    //C----------------------------------------------------------------------
    public class ANNEAL : IRunIt
    {
        #region Singleton

        private static ANNEAL g_singleton = null;
        private ANNEAL() { }
        public static ANNEAL Singleton()
        {
            if (g_singleton == null)
                g_singleton = new ANNEAL();

            return g_singleton;
        }

        #endregion


        public void RunIt() { }

        public void RunIt(frmCONOP frm)
        {
             double DELTA;

            double U;
            int INNER, IRANK, JRANK, LAST, OUTER, NCTR, FIXM, NABRM;
            int I, L, X, NCT;

            //CPMS  Declarations for graphics:      
            //CPMS  PCT  Percentage annealed
            //CPMS  INISCOR - extension penalty for initial solution
            //CPMS  BSTSCOR - best extension penalty to date
            //CPMS  SCORes do not include the smoothing factors                          
            //CPMS  I??SCOR - initial scores for scaling the screen plots

            int INISCOR, ISMSCOR, ISQSCOR, ISHSCOR, ITSSCOR, INGSCOR;
            int BSTSCOR, AUTSCOR;
            double PCT;
            int dummy, ulx, uly, lrx, lry, res, siz;
            string SCORE;

            int horiz, vert, div;
            bool rndf, stepf, aimf, redf, trjf, algf, rlxf, rngf, hilf, s45f, s67f, s89f;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //CPMS************************************************
            //CPMS***PRELIMINARIES
            //CPMS	 next section executes only once in a run
            //CPMS------------------------------------------------

            div = 0;

            //C     initialize things for Intel 11.1  
            COMMOD9.UU = 0.0;
            U = 0.0;
            PCT = 0.0;
            DELTA = 0.0;
            INNER = 0;
            IRANK = 0;
            JRANK = 0;
            LAST = 0;
            OUTER = 0;
            NCTR = 0;
            FIXM = 0;
            NABRM = 0;
            I = 0;
            L = 0;
            X = 0;
            NCT = 0;

            //C   initialized for Intel 9.0
            horiz = 0;
            vert = 900;
            rndf = true;
            INISCOR = 0;
            ITSSCOR = 0;
            ISMSCOR = 0;
            ISQSCOR = 0;
            ISHSCOR = 0;
            INGSCOR = 0;
            AUTSCOR = 0;
            BSTSCOR = 0;

            if ((COMMOD9.AUTF == 0) && (COMMOD9.CONTF == 0))
            {
                COMMOD9.BSTPEN = 0.0;
                COMMOD9.BS2PEN = 0.0;
            }

            COMMOD9.TOOPEN = 0.0;
            COMMOD9.TO2PEN = 0.0;
            COMMOD9.TSPEN = 0.0;
            COMMOD9.SMPEN = 0.0;
            COMMOD9.SHPEN = 0.0;
            COMMOD9.CRTPEN = 0.0;
            COMMOD9.NEWRANK = 0;
            //c  NEWRANK is the rank to which the last (BIG|SML) mutation moved an event

            //C--------------------------------
            //C     set loop-accelerator flags
            //C     step-solutions (outer loop):
            stepf = false;
            if ((COMMOD9.STEPSOL.Substring(0, 3) != "OFF") && (COMMOD9.STEPSOL.Substring(0, 3) != "off"))
            {
                stepf = true;
            }

            //C  aimed tempering (outer loop):

            aimf = false;
            if ((COMMOD9.SOLVEF == 3) && (COMMOD9.FIXF == 2)) aimf = true;

            //C  animated ranges:
            rngf = false;
            if ((COMMOD9.RUNGRF == 2) || (COMMOD9.RUNGRF == 5)) rngf = true;

            //C  penalty trajectory:
            trjf = false;
            if (((COMMOD9.RUNGRF == 2) || (COMMOD9.RUNGRF == 0)) && (COMMOD9.TRJGRF > 0)) trjf = true;

            //C  red temp curve (inner loop):
            redf = false;
            if (trjf)
            {
                if ((COMMOD9.RUNGRF != 0) && (COMMOD9.SOLVEF != 2) && (COMMOD9.SOLVEF != 4) &&
                   (COMMOD9.SOLVEF != 6) && (COMMOD9.SOLVEF != 8)) redf = true;
            }

            //C  greedy algorithm:
            algf = false;
            if ((COMMOD9.SOLVEF == 2) || (COMMOD9.SOLVEF == 4) || (COMMOD9.SOLVEF == 6) ||
               (COMMOD9.SOLVEF == 8)) algf = true;

            //C  relaxed fit update:
            rlxf = false;
            if ((COMMOD9.RUNGRF >= 4) || (COMMOD9.GRIDF == 1)) rlxf = true;
            //C  uphill moves in different color:	
            hilf = false;
            if ((COMMOD9.SOLVEF != 2) && (COMMOD9.TRJGRF >= 3)) hilf = true;

            //C  SOLVEF 4 or 5 (squeeze):
            s45f = false;
            if ((COMMOD9.SOLVEF == 4) || (COMMOD9.SOLVEF == 5)) s45f = true;

            //C  SOLVEF 6 or 7 (shrink)
            s67f = false;
            if ((COMMOD9.SOLVEF == 6) || (COMMOD9.SOLVEF == 7)) s67f = true;
            //C     SOLVEF 8 or 9 (stack)
            s89f = false;
            if ((COMMOD9.SOLVEF == 8) || (COMMOD9.SOLVEF == 9)) s89f = true;


            //C--------------------------------
            //C     set the initial temperature
            NCTR = 0;
            COMMOD9.CTRF = 0;
            COMMOD9.NBETR = 0;

            if (COMMOD9.CONTF == 1) goto Label111;

            switch (COMMOD9.AUTF)
            {
                case 0:
                    //cpms  not PAUSES='AUT','ADA'
                    COMMOD9.T = COMMOD9.STARTT;
                    break;
                case 1:
                    //cpms  still improving in last TRIALS - don't cool
                    //COMMOD9.T=COMMOD9.T;
                    COMMOD9.Rx = COMMOD9.R;
                    break;
                case -1:
                    //cpms   nothing better found in last TRIALS - cool	   	
                    //c         The following line cools by a fixed amount
                    //c	        T = T*R           
                    //c         In the alternative the size of the
                    //c         cooling step increases as the  
                    //c         size of the null sets increases 
                    //c         for adaprive cooling
                    COMMOD9.Rx = COMMOD9.R;
                    if ((COMMOD9.ADAF == 1) && (COMMOD9.NTRIES > 0) && (COMMOD9.LSTRIES > 1))
                    {
                        L = COMMOD9.RANDOM.Next(Math.Min(COMMOD9.LSTRIES, 5) + 1);//<=HXD
                        COMMOD9.Rx = Math.Pow(COMMOD9.R, L);
                        COMMOD9.NTRIES = COMMOD9.NTRIES + (L - 1);
                    }

                    COMMOD9.T = COMMOD9.T * COMMOD9.Rx;

                    break;
            }


#if test_initPerm
            //INITPERM
            long _index = 0;

#if SEC7
            string inputFile = "inData/SEC7_INIT_PERM.csv";
#elif SEC19
            string inputFile = "inData/SEC19_INIT_PERM.csv";
#elif SEC20
            string inputFile = "inData/SEC20_INIT_PERM.csv";
#elif SEC50
            string inputFile = "inData/SEC50_INIT_PERM.csv";
#elif SEC195
            string inputFile = "inData/SEC195_INIT_PERM.csv";
#elif SEC286
            string inputFile = "inData/SEC286_INIT_PERM.csv";
#elif SEC287
            string inputFile = "inData/SEC287_INIT_PERM.csv";
#endif

#if  REPACEPERM

            COMMOD9.INIPERM = new int[COMMOD9.NEVNT];
            using (StreamReader reader = new StreamReader(inputFile, Encoding.Default, true))
            {
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    COMMOD9.INIPERM[_index++] = int.Parse(line.Split(',')[1]);
                }
            }
#endif
#endif //test_initPerm

        Label111:
            PCT = COMMOD9.NINNER * COMMOD9.NOUTER;

            //C--------------------------------------------------------
            //CPMS  alter OUTF so that GETPEN does not write to outmain
            //CPMS
            //**      OUTF = OUTF-100
            //CPMS
            //C     COMPUTE THE BEST PLACEMENTS THAT COMPLY WITH STARTING PERM 
            //C       "INIPERM" AND RESULTING PENALTY. 
            //C          PUT THE placements in SCTSOL and the PENALTY IN PEN
            //C
            //C     for the first permutation, we need a complete solution.
            //C         for subsequent perms, we may use reoptimization.    
            //C---------------------------------------------------------    
            if (COMMOD9.PENF <= 1)
            {
                Helper.GETPEN(COMMOD9.INIPERM, ref COMMOD9.NXTPEN);
            }
            else if (COMMOD9.PENF >= 2)
            {
                Helper.DEMPEN(COMMOD9.INIPERM, ref COMMOD9.NXTPEN);
            }

            //CPMS*********************************************
            //CPMS  PRELIMINARIES  -  not included in loops
            Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.PERM);
            Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.LSTPERM);
            Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.PXLPERM);

            if (COMMOD9.CONTF == 0) Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.BSTPERM);

            COMMOD9.PEN = COMMOD9.NXTPEN;
            COMMOD9.INIPEN = COMMOD9.NXTPEN;
            INISCOR = (int)COMMOD9.INIPEN;
            AUTSCOR = (int)COMMOD9.NXTPEN;

            if (COMMOD9.AUTF == 0)
            {
                if (COMMOD9.CONTF == 0) COMMOD9.BSTPEN = COMMOD9.NXTPEN;
            }
            else
            {
                if (COMMOD9.CONTF == 0) COMMOD9.BSTPEN = COMMOD9.AUTPEN;
            }

            BSTSCOR = (int)COMMOD9.BSTPEN;

            //Helper.DOTOO();

            COMMOD9.NGTPEN = COMMOD9.NGHPEN;
            INGSCOR = (int)COMMOD9.NGTPEN;

            if ((COMMOD9.AUTF == 0) && (COMMOD9.CONTF == 0))
            {
                COMMOD9.BS2PEN = COMMOD9.TOOPEN;
                COMMOD9.SQBST = COMMOD9.SQPEN;
                COMMOD9.SHBST = COMMOD9.SHPEN;
                COMMOD9.TSBST = COMMOD9.TSPEN;
            }

            ISQSCOR = (int)COMMOD9.SQPEN;
            ISHSCOR = (int)COMMOD9.SHPEN;
            COMMOD9.TSLST = COMMOD9.TSPEN;
            ITSSCOR = (int)COMMOD9.TSPEN;

            ISMSCOR = (int)COMMOD9.SMPEN;

            if ((COMMOD9.KSM > 0.0) && (COMMOD9.PENF < 2))
            {
                ISMSCOR = (int)(COMMOD9.SMPEN / COMMOD9.KSM);
                INISCOR = (int)(COMMOD9.PEN - COMMOD9.SMPEN);
                BSTSCOR = (int)(COMMOD9.BSTPEN - COMMOD9.SMPEN);
            }

            if (COMMOD9.CTRF == 1)
            {
                NCT = NCT + 1;
                COMMOD9.CTRF = 0;
            }

            if (COMMOD9.GRID2F == 1)
            {
                //cpms	 make sure that the .gr2 file records initial permutation

                for (X = 0; X < COMMOD9.NEVNT; X++)
                {
                    //cpms  update secondary penalty 
                    COMMOD9.SECNDBST[COMMOD9.PERM[X], X] =
                        Math.Min(COMMOD9.SECNDBST[COMMOD9.PERM[X], X], COMMOD9.TO2PEN);
                }
            }


            //C-----------------------------------------------------------------------      
            //C     to speed up reoptimization (in EVALUATE), keep track of 
            //C         the last action taken during annealing. 
            //C     if the new PERM was accepted,
            //C         SCTSOL and HSCTSOL will already be the same, and there is no need 
            //C         to copy SCTSOL to HSCTSOL before reoptimization. 
            //C     if the new permutation is not accepted, copy SCTSOL to HSCTSOL
            //C     use the flag LAST:
            //C        LAST = 1, accepted the new PERM
            //C        LAST = 0, did not accept the new PERM
            //C     since GETPEN calculates HSCTSOL, and we assign that to SCTSOL, 
            //C         initialize it to 1.    
            //C-----------------------------------------------------------------------
            //C     put the initial perm in PERM, the initial penalty in PEN, and       
            //C     USE PERM AND PEN AS THE INITIAL "BEST" VALUES
            //C-----------------------------------------------------------------------

            Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
            Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            LAST = 1;
            COMMOD9.XPOSN = 0;

            if (COMMOD9.FIXF == 3)
            {
                for (COMMOD9.XPOSN = 0; COMMOD9.XPOSN < COMMOD9.NEVNT; COMMOD9.XPOSN++)
                {
                    if (COMMOD9.PERM[COMMOD9.XPOSN] == COMMOD9.XEVNT) break;
                }
            }

            //HACK:OMIT MANY LINES

            //cpms---reset CONTF after first pass through--------------

            COMMOD9.CONTF = 0;

            //CPMS*******************************************************************
            //CPMS  MAIN LOOPS
            //C     The following lines are executed with every trial!!
            //C     Analyse a big sequence of neighbors ('permutations'). 
            //C     Compute the penalty and decide whether to accept the new perm
            //C     according to the SOLVER option
            //C-------------------------------------------------------------      
            //CPMS---Store two run parameters in case the last temper quench is
            //CPMS   to be "aimed" into the optimum with different parameters
            //CPMS   and the program must reset for another "RPT" run.
            FIXM = COMMOD9.FIXF;
            NABRM = COMMOD9.NABRGEN;

            //Helper.Write("\n  ");
            //if (COMMOD9.PAUSF != 5)
            //{
            //    Console.BackgroundColor = ConsoleColor.Blue;
            //}

#if FAKE_IJRANKE_DOUBLE
            WritePara.readIJRnk();          
#endif

            //CPMS**START THE OUTER LOOP***************
            for (OUTER = 0; OUTER < (int)COMMOD9.NOUTER; OUTER++)
            {

                //CPMS	check for "AIM"ed tempering
                if ((aimf) && (OUTER == (int)COMMOD9.NOUTER - 1))
                {
                    COMMOD9.FIXF = 0;
                    COMMOD9.NABRGEN = 2;
                }

                //CPMS  write out best solution so far if requested
                if ((stepf) && (COMMOD9.CDF != 1)) Helper.STEPOUT(COMMOD9.BSTPERM);
                if ((stepf) && (COMMOD9.CDF != 1)) Helper.TRAJOUT();

                //CPMS*****START THE INNER LOOP*********
                for (INNER = 0; INNER < (int)COMMOD9.NINNER; INNER++)
                {


                    PCT = PCT - 1;

                    //CPMS-----------------------------------------------------     
                    //CPMS       GENERATE A NEIGHBOR AS A CANDIDATE PERMUTATION
                    //CPMS       after saving current state of secondary penalty
                    COMMOD9.TSLST = COMMOD9.TSPEN;
                    Helper.GONABR(ref IRANK, ref JRANK);     
#if FAKE_IJRANKE_DOUBLE
                    COMMOD9.UU = WritePara.FAKE_DOUBLEs[OUTER * (int)COMMOD9.NINNER + INNER];
#else
                    //CPMS******    This replaces the old calculation of PROB after penalty totalling
                    //CPMS******    It allows NEWPEN to exit the section loop as soon as the critical
                    //CPMS******    penalty size (CRTPEN) is reached.
                    //CPMS******    NEWPEN takes advantage of this, but NWDMPEN does not.....yet
                    //CPMS******    for a riley test, running time drops from 1.133 to 1.117 mins.
                    COMMOD9.UU = COMMOD9.RANDOM.NextDouble();
#endif

#if OUTDATA_IJRANK
                    WritePara.writeIJRnk(IRANK, JRANK, COMMOD9.UU);
#endif

                    //if(COMMOD9.PEN>4000 && COMMOD9.PEN<5000)
                    //    Helper.Write(" T={0} UU={1} Pen={2} ", COMMOD9.T,COMMOD9.UU,COMMOD9.PEN);
                    COMMOD9.CRTPEN = COMMOD9.PEN - (COMMOD9.T * Math.Log(COMMOD9.UU));
                    //if (COMMOD9.PEN > 4000 && COMMOD9.PEN < 5000)
                    //    Helper.Write("CRTPEN={0}\n", COMMOD9.CRTPEN);



                    if (COMMOD9.PENF <= 1)
                    {
                        Helper.NEWPEN(IRANK, JRANK, COMMOD9.PERM, ref COMMOD9.NXTPEN);

                    }
                    else if (COMMOD9.PENF >= 2)

                    {
                        Helper.NWDMPEN(IRANK, JRANK, COMMOD9.PERM, ref COMMOD9.NXTPEN);
                    }

#if SWT_CHECK_VIA_GETPEN

                    double checkPenalty = 0.0;
                    if (COMMOD9.PENF <= 1)
                    {
                        Helper.GETPEN(COMMOD9.PERM, ref checkPenalty);
                        double delta = Math.Abs(checkPenalty - COMMOD9.NXTPEN);
                        Trace.WriteLineIf(delta > Double.Epsilon, string.Format("{0} - getNwPen={1}, getPen={2}, delta={3}, [COMMOD9.PENF <= 1]", DateTime.Now, COMMOD9.NXTPEN, checkPenalty, delta));
                    }
                    else if (COMMOD9.PENF >= 2)
                    {
                        Helper.GETPEN(COMMOD9.PERM, ref checkPenalty);
                        double delta = Math.Abs(checkPenalty - COMMOD9.NXTPEN);
                        Trace.WriteLineIf(delta > Double.Epsilon, string.Format("{0} - getNwPen={1}, getPen={2}, delta={3}, [COMMOD9.PENF >= 2]", DateTime.Now, COMMOD9.NXTPEN, checkPenalty, delta));
                    }

#endif

                    DELTA = COMMOD9.NXTPEN - COMMOD9.PEN;


                    //C-----------------------------------------------------------------------      
                    //C    DETERMINE WHETHER TO ACCEPT THE NEW PERM OR KEEP THE
                    //C    OLD ONE
                    //C    PEN is the previous penalty
                    //C    NXTPEN is the candidate to replace PEN
                    //C    DELTA is the difference
                    //C----------------------------------------------------------------------- 

                    //C-----------------------------------------------------------------------
                    //c          set the flag LAST to 0, saying that the new PERM 
                    //C          has not been accepted yet      
                    //C
                    //C          IF THE NEW PERM IS BETTER (OR AS GOOD and not "GREEDY"), 
                    //C            1. ACCEPT IT, keep PERM, update PEN, TOOPEN and SCTSOL, AND 
                    //C            2. SEE IF IT IS BEST SO FAR
                    //C             IF IT IS, UPDATE THE BEST SO FAR (BSTPERM AND BSTPEN)
                    //C		      If a "SQUEEZE" run, accept only if squeeze penalty is not worse
                    //C-----------------------------------------------------------------------         

                    LAST = 0;

                    //CPMS       ? replace the next IF with DELTA.LE.0
                    //CPMS       i.e. always accept an equally good move and save the checking time
                    //CPMS       ?? leads to lots of repetitions
                    //CPMS       ??? what is the snag for greedy?  
                    //c          /////////////////////////////////////////      
                    //CPMS       IF NEW SEQUENCE HAS SAME OR LOWER PENALTY
                    if ((DELTA < 0.0) ||
                        ((DELTA == 0.0) && ((COMMOD9.SOLVEF < 2) ||
                        ((s89f) && (COMMOD9.TSPEN <= COMMOD9.TSBST)) ||
                        ((s67f) && (COMMOD9.SHPEN <= COMMOD9.SHBST)) ||
                        ((s45f) && (COMMOD9.SQPEN <= COMMOD9.SQBST)))))
                    {

                        //HACK:OMIT MANY LINES

                        if (COMMOD9.CTRF == 1)
                        {
                            NCT = NCT + 1;
                            COMMOD9.CTRF = 0;
                        }

                        //update the penalties
                        COMMOD9.PEN = COMMOD9.NXTPEN;
                        COMMOD9.ASPNPEN = COMMOD9.SPANPEN;

                        Helper.DOTOO();

                        if (COMMOD9.PENF < 2)
                        {
                            Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
                        }

                        Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                        LAST = 1;

                        //if this is very best so far
                        if ((COMMOD9.PEN < COMMOD9.BSTPEN) || ((COMMOD9.PEN == COMMOD9.BSTPEN) &&
                            (((s89f) && (COMMOD9.TSPEN < COMMOD9.TSBST)) ||
                            ((s67f) && (COMMOD9.SHPEN < COMMOD9.SHBST)) ||
                            ((s45f) && (COMMOD9.SQPEN < COMMOD9.SQBST)))))
                        {
                            siz = 2;
                            if (COMMOD9.PEN == COMMOD9.BSTPEN) siz = 1;


                            COMMOD9.SQBST = COMMOD9.SQPEN;
                            COMMOD9.SHBST = COMMOD9.SHPEN;
                            COMMOD9.TSBST = COMMOD9.TSPEN;
                            COMMOD9.BSTPEN = COMMOD9.PEN;
                            COMMOD9.NBETR = COMMOD9.NBETR + 1;
                            COMMOD9.BS2PEN = COMMOD9.TOOPEN;
                            COMMOD9.NGTPEN = COMMOD9.NGHPEN;
                            BSTSCOR = (int)COMMOD9.BSTPEN;

                            if ((COMMOD9.KSM > 0.00) && (COMMOD9.PENF < 2)) BSTSCOR = BSTSCOR - (int)COMMOD9.SMPEN;

                            Helper.CopyArray(COMMOD9.PERM, ref COMMOD9.BSTPERM);

                            //HACK:OMIT MANY LINES



                        }
                    }
                    else
                    {
                        //C-----------------------------------------------------------------------      
                        //C    LOOK FOR PROBABILISTIC GROUNDS TO ACCEPT
                        //C    EVEN IF THE NEW PERM IS WORSE THAN "PERM",
                        //C    ACCEPT IT WITH PROBABILITY "PROB"
                        //C    PROB replaced by CRTPEN - the critical penalty at PROB
                        //C  ////////////////////////////////////////////////////////////// 

                        //NOT AN IMPROVEMENT
                        if (COMMOD9.NUDGER)
                        {
                            COMMOD9.NUDGUP = false;
                            COMMOD9.NUDGDN = false;
                        }

                        //CPMS   reject and reset for greedy algorithm 
                        //cpms   and shrinker and squeezer and teaser
                        if (algf)
                        {
                            Helper.CopyArray(COMMOD9.LSTPERM, ref COMMOD9.PERM);

                            COMMOD9.TSPEN = COMMOD9.TSLST;

                        }
                        else
                        {
                            //cpms-----------for squeal and anneas and steal--------
                            //cpms	  adjust U downward if 2nd penalty improves
                            //cpms    and up if the 2nd penalty worsens
                            //cpms    adjust according to the new/old ratio squared
                            //cpms	   the adjustments are likely to be small 
                            //cpms    but they increase geometrically as the 
                            //cpms    impact on the secondary penalty increases
                            //cpms
                            //cpms    the efficiency of the promary search is compromised
                            //cpms    but the secondary penalty is not allowed to
                            //cpms    increase uncontrollably

                            switch (COMMOD9.SOLVEF)
                            {
                                case 1:
                                case 2:
                                case 3:
                                case 4:
                                case 6:
                                case 8:
                                    break;
                                case 5:
                                    if (COMMOD9.SQPEN > 0)
                                        U = U * Math.Pow((COMMOD9.SQPEN / COMMOD9.SQBST), 2);
                                    break;
                                case 7:
                                    if (COMMOD9.SHPEN > 0)
                                        U = U * Math.Pow((COMMOD9.SHPEN / COMMOD9.SHBST), 2);
                                    break;
                                case 9:
                                    if (COMMOD9.TSPEN > 0)
                                        U = U * Math.Pow((COMMOD9.TSPEN / COMMOD9.TSBST), 2);
                                    break;
                            }

                            //cpms-------------------------------------------------

                            if (COMMOD9.NXTPEN < COMMOD9.CRTPEN)
                            {
                                //CPMS    ACCEPT UPHILL STEP
                                //CPMS    and check contraction
                                if (COMMOD9.CTRF == 1)
                                {
                                    NCT = NCT + 1;
                                    COMMOD9.CTRF = 0;
                                }

                                switch (COMMOD9.SOLVEF)
                                {
                                    case 1:
                                    case 2:
                                    case 3:
                                    case 4:
                                    case 6:
                                    case 8:
                                        break;
                                    case 5:
                                        COMMOD9.SQBST = COMMOD9.SQPEN;
                                        break;
                                    case 7:
                                        COMMOD9.SHBST = COMMOD9.SHPEN;
                                        break;
                                    case 9:
                                        COMMOD9.TSBST = COMMOD9.TSPEN;
                                        break;
                                }


                                //update total penalty and secondary penalty
                                COMMOD9.PEN = COMMOD9.NXTPEN;
                                COMMOD9.ASPNPEN = COMMOD9.SPANPEN;

                                Helper.DOTOO();

                                if (COMMOD9.PENF < 2)
                                {
                                    Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
                                }

                                Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                                LAST = 1;

                                //Edit the range lines because bad switch is accepted

                                //CPMS	 Load the EVERYBST array and add points to 
                                //CPMS	 the event bathtub - needs to be a separate subroutine
                                if (rlxf)
                                {
                                    FILLBEST(INISCOR, COMMOD9.PEN);
                                    //run through the solution in sequence (EVERYBST cols)
                                    //and enter the penalty for the appropriate
                                    //event (EVERYBST rows), IF the best so far
                                    //i.e. EVERYBST has IROWS order
                                }

                            }
                            else
                            {
                                //C-----------------------------------------------------------------------      
                                //CPMS     REJECT UPHILL STEP reset PERM to previous order (any NABRGEN!)
                                //CPMS	   no boxes plotted on the run-time graphic
                                //CPMS     no update to PEN or TOOPEN
                                //CPMS--------------------------------------------------------
                                Helper.CopyArray(COMMOD9.LSTPERM, ref COMMOD9.PERM);
                                COMMOD9.TSPEN = COMMOD9.TSLST;
                                COMMOD9.CTRF = 0;
                            }
                        }
                    }//END if ELSE DELTA>0

                    //cpms  if tempering:  
                    if (COMMOD9.SOLVEF == 3) COMMOD9.T = COMMOD9.T * COMMOD9.R;

                    System.Windows.Forms.Application.DoEvents();
                    System.Threading.Thread.Sleep(0);

                }//CPMS*****end of inner loop**************

                //C  lower the temperature and do it again if annealing
                //C  heat to random temperature and quench again if tempering (solvef=3)

                if (COMMOD9.SOLVEF == 3)
                {
                    U = COMMOD9.RANDOM.NextDouble();

                    //c	 PCT/NINNER*NOUTER runs from 1.0 to 0.0
                    //c      IF random number is larger, try again
                    //c      reduces frequency of hot tempers later in run
                    COMMOD9.T = COMMOD9.STARTT * PCT / (COMMOD9.NINNER * COMMOD9.NOUTER) * U;

                    if (COMMOD9.PENF < 2)
                    {
                        Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.PEN);
                    }
                    else if ((COMMOD9.PENF == 5) || (COMMOD9.PENF == 6))
                    {
                        Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.PEN);
                    }
                    else if (COMMOD9.PENF > 1)
                    {
                        Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.PEN);
                    }

                    Helper.CopyArray(COMMOD9.BSTPERM, ref COMMOD9.PERM);
                    Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
                    Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
                }
                else
                {
                    if (COMMOD9.AUTF == 0) COMMOD9.T = COMMOD9.T * COMMOD9.R;

                    if ((COMMOD9.AUTF == 0) && (COMMOD9.CDF != 1)) Helper.TRAJOUT();
                }

                if (COMMOD9.PAUSF != 5)
                {
                    if (OUTER == COMMOD9.NOUTER - 1)
                    {
                        frm.MSG_SB.Length = 0;
                        frm.MSG_SB.Append("100%");
                        frm.Invalidate();
                        frm.Update();
                    }
                    //else if (OUTER != 0 && (OUTER % ((int)(COMMOD9.NOUTER / 100))) == 0)
                    else if (OUTER != 0)
                    {
                        frm.MSG_SB.Length = 0;
                        frm.MSG_SB.AppendFormat("{0:0.0}%", ((float)(OUTER + 1) / (float)COMMOD9.NOUTER * 100));
                        frm.Invalidate();
                        frm.Update();
                    }
                }


            }//cpms**end of outer loop*********************************************
#if OUTDATA_IJRANK
            WritePara.closeSW_STATIC();
#endif

            COMMOD9.AUTPEN = COMMOD9.BSTPEN;
            COMMOD9.NUDGUP = false;
            COMMOD9.NUDGDN = false;

            //CPMS  Now turn to tasks completed only once per run, AFTER annealing
            //cpms-----------------
            //CPMS	restore FIXF and NABRGEN
            COMMOD9.FIXF = FIXM;
            COMMOD9.NABRGEN = NABRM;

            //CPMS-----------------------------------------------------------
            //CPMS     write out last solution tried if requested	 
            if ((COMMOD9.STEPSOL.Substring(0, 3) == "LST") ||
                (COMMOD9.STEPSOL.Substring(0, 3) == "lst"))
            {
                if (COMMOD9.CDF != 1) Helper.STEPOUT(COMMOD9.LSTPERM);

                if (COMMOD9.CDF != 1) Helper.TRAJOUT();

            }
            //C-------------------------------------------------------------- 
            //C     save value of PENF
            LAST = COMMOD9.PENF;

            //CPMS  Try to prevent the remaining tasks from
            //c     running during auto-cooling until about to stop
            if ((COMMOD9.PAUSF == 5) && (COMMOD9.NTRIES < COMMOD9.NSTOP - 1))
            {
                COMMOD9.NUDGUP = false;
                COMMOD9.NUDGDN = false;

                goto Label33;
            }

            //C     after all NOUTER*NINNER passes through the loop, 
            //CPMS  make sure that HSCTSOL reflects the best solution
            //CPMS  rather than the last tried
            //CPMS  and that HSCTSOL is last set according to correct penalty
            //CPMS  i.e. lvlpen if PENF=1, intpen for all other cases

            if (COMMOD9.PENF == 1)
            {
                COMMOD9.PENF = -1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.RVLPEN);
                if (LAST == -1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);
                if (LAST == 0) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.LVLPEN);
                if (LAST == 1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            }
            else if (COMMOD9.PENF == 0)
            {
                COMMOD9.PENF = -1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.RVLPEN);
                if (LAST == -1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.LVLPEN);
                if (LAST == 1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);
                if (LAST == 0) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            }
            else if (COMMOD9.PENF == -1)
            {
                COMMOD9.PENF = 1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.LVLPEN);
                if (LAST == 1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);
                if (LAST == 0) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = -1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.RVLPEN);
                if (LAST == -1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            }

            COMMOD9.NGTPEN = COMMOD9.NGHPEN;
            //CPMS  GETPEN places local horizons to fit the best sequence
            //CPMS  but the final penalty needs to be returned to its 
            //CPMS  "democratic" value for an ordinal search

            COMMOD9.PENF = 6;
            if (COMMOD9.FB4LF != 0) Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.SEQPEN);

            COMMOD9.PENF = 5;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.ROYPEN);

            COMMOD9.PENF = 7;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.MOMPEN);
            if (LAST == 7) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            COMMOD9.PENF = 4;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.RSCPEN);
            if (LAST == 4) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            COMMOD9.PENF = 3;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.SPTPEN);
            if (LAST == 3) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            COMMOD9.PENF = 2;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.ORDPEN);
            if (LAST == 2) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            if (LAST == 5) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            if ((LAST == 6) && (COMMOD9.FB4LF != 0)) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            //C   SCJPEN now contains the primary section penalties
            //C   But this is pointless for Royal (PENF=5) 
            //C   and for Sequel (PENF=6) so
            //C   the ordinal penalty is used instead

            if (LAST > 2)
            {
                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);

                //C   COLPEN now contains the ordinal or interval penalties
                //C   whichever contrasts with the primary
            }

            //c  restore PENF 
            COMMOD9.PENF = LAST;
            switch (COMMOD9.PENF)
            {
                case -1://eventual
                    COMMOD9.BSTPEN = COMMOD9.RVLPEN;
                    break;
                case 0://interval
                    COMMOD9.BSTPEN = COMMOD9.INTPEN;
                    break;
                case 1://level
                    COMMOD9.BSTPEN = COMMOD9.LVLPEN;
                    break;
                case 2://ordinal
                    COMMOD9.BSTPEN = COMMOD9.ORDPEN;
                    break;
                case 3://spatial
                    COMMOD9.BSTPEN = COMMOD9.SPTPEN;
                    break;
                case 4://rascal/rascer
                    COMMOD9.BSTPEN = COMMOD9.RSCPEN;
                    break;
                case 5://royal (Coex)
                    COMMOD9.BSTPEN = COMMOD9.ROYPEN;
                    break;
                case 6://sequel (Fb4L)
                    if (COMMOD9.FB4LF != 0) COMMOD9.BSTPEN = COMMOD9.SEQPEN;
                    break;
                case 7://momental
                    COMMOD9.BSTPEN = COMMOD9.MOMPEN;
                    break;
            }

#if WRITESULT
            WritePara.writeBstPerm();
            Environment.Exit(0);
#endif

        Label33:

            //HACK:OMIT MANY LINES


            //CPMS-------------------------------------------------------------
            //CPMS  transfer the number of contractions to a common variable
            COMMOD9.CTRF = NCTR;

        }

        //Draws the x-axis 
        private void XSCALE(frmCONOP frm, Graphics g, int xbig, int mxx, int mxy, int xlft, int xrit, int y)
        {
            //COMMOD COMMOD9=COMMOD.Singleton();

            int stp, jmp, lbl, tck;
            double xreal, fctr;
            //cpms  DEC fortran does not handle reals and integers like MS Fortran
            //cpms  so we scale the x-axis position in reals and then apply INT2()
            //cpms  when performing a lineto or moveto! 
            string xtitle = "";

            //C-------------------------------------------------------------------
            //CPMS	  scale line runs from xlft to maxx-xrit; everywhere at maxy-y
            //CPMS	  the biggest x value is xbig (e.g. NEVNT)
            //C---------------------------------------------
            //CPMS  Draw the x-axis baseline
            g.DrawLine(frm.DRAW_PEN_WHITE, xlft, mxy - y, mxx - xrit, mxy - y);


            //CPMS----------------------------
            //CPMS	Scale the ticks and labels
            if (xbig < 200)
            {
                jmp = 1;
                lbl = 20;
            }
            else if (xbig < 400)
            {
                jmp = 2;
                lbl = 50;
            }
            else if (xbig < 800)
            {
                jmp = 5;
                lbl = 100;
            }
            else if (xbig < 1600)
            {
                jmp = 10;
                lbl = 200;
            }
            else
            {
                jmp = 20;
                lbl = 500;
            }

            //CPMS----------------
            //cpms  determine scale factor to use in next two loops
            fctr = (double)(mxx - xlft - xrit) / (double)(xbig);
            //cpms----------------
            //CPMS  Draw the ticks    
            for (stp = 0; stp <= xbig; stp += jmp)
            {
                xreal = (double)(stp) * fctr;

                //determine tick height
                if (stp % 100 == 0)
                {
                    tck = 8;
                }
                else if (stp % 50 == 0)
                {
                    tck = 6;
                }
                else if (stp % 10 == 0)
                {
                    tck = 4;
                }
                else
                {
                    tck = 2;
                }

                g.DrawLine(frm.DRAW_PEN_WHITE, xlft + (int)(xreal), mxy - y, xlft + (int)(xreal), mxy - y + tck);
            }

            //CPMS-----------------
            //CPMS	Draw the lables
            for (stp = lbl; stp <= xbig; stp += lbl)
            {
                xreal = (double)(stp) * fctr;
                g.DrawString(string.Format("{0}", stp), frm.MSG_FONT_S, frm.MSG_BRUSH_WHITE, xlft - 15 + (int)(xreal), mxy - y + 10);

            }

        }

        private void YSCALE(frmCONOP frm, Graphics g, int ybig, int mxx, int mxy, int ytop, int ybtm, int x, int res, bool rndf)
        {

            int div = 0;
            int stp;
            int tck;
            double yreal, fctr;
            string SCORE;

            //C-------------------------------------------------------------------
            //CPMS	  scale line runs from ytop to maxy-ybtm; everywhere at x
            //CPMS	  the biggest x value is xbig (e.g. NEVNT)
            //mxx = mxx  ??<=HXD
            //C---------------------------------------------
            //CPMS	round the top y value

            if (rndf)
            {
                ROUNDUP(ref ybig, res, ref div);
            }

            SCORE = string.Format("{0}", ybig);
            LBLSCOR(frm, g, SCORE, 0);

            //CPMS----------------------------
            //CPMS  Draw the y-axis baseline
            g.DrawLine(frm.DW_Pen, x, ytop, x, mxy - ybtm);

            //CPMS----------------
            //CPMS  Draw the ticks
            if (!rndf)
            {
                div = res;
            }

            if (div > 0)
            {
                tck = 2;
                fctr = (double)(mxy - ybtm - ytop) / (double)(div * 10);
                for (stp = 0; stp <= div * 10; stp++)
                {
                    tck = 2;
                    if (stp % 10 == 0) tck = 6;
                    yreal = (double)(stp) * fctr;
                    g.DrawLine(frm.DW_Pen, x, ytop + (int)(yreal), x - tck, ytop + (int)(yreal));
                }
            }
        }

        private void WRITEPEN(frmCONOP frm, Graphics g, Brush brush, int flg)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            string ytitle;
            int horiz, vert;
            int scl, top, btm, lft, rit;

            //CPMS-----------------------------------------------
            horiz = 0;
            vert = -90;
            scl = 0;
            top = 0;
            btm = 50;
            lft = 100;
            rit = 0;
            //CPMS-------------------------------------------------------------

            g.TranslateTransform((int)(lft / 9), (int)(COMMOD9.maxy - btm - 125));
            g.RotateTransform(vert);

            if (flg == 1)
            {
                ytitle = "";
                switch (COMMOD9.PENF)
                {
                    case -1:
                        ytitle = "- eventual";
                        break;
                    case 0:
                        ytitle = "- interval";
                        break;
                    case 1:
                        ytitle = "- level";
                        break;
                    case 2:
                        ytitle = "- ordinal";
                        break;
                    case 3:
                        ytitle = "- spatial";
                        break;
                    case 4:
                        ytitle = "- rascal";
                        break;
                    case 5:
                        ytitle = "- royal";
                        break;
                    case 6:
                        ytitle = "- sequel";
                        break;
                    case 7: ytitle = "- momental";
                        break;
                }

                ytitle = "BEST PENALTY" + ytitle;
                g.DrawString(ytitle, frm.MSG_FONT_S, brush, 0, 0);
                g.ResetTransform();
            }
            else if (flg == 2)
            {
                g.DrawString("BEST SUM OF SECONDARY PENALTIES", frm.MSG_FONT_S, brush, 0, 0);
                g.ResetTransform();
            }

            if (COMMOD9.KSM > 0.00)
            {
                brush = frm.MSG_BRUSH_2;
                g.TranslateTransform((int)(lft / 9), (int)(COMMOD9.maxy - btm - 15));
                g.RotateTransform(vert);
                g.DrawString("smoothing - ", frm.MSG_FONT_S, brush, 0, 0);
                g.ResetTransform();
            }

        }

        //---------------------------------------------------------------
        //Subroutine to draw a range chart that illustrates the sequence
        //of observed events in one feasible solution
        //
        //May be "animated" using editcht which alters the range chart to
        //reflect sequence changes made by GONABR.FOR
        //
        //Programmer:        Pete Sadler
        //Last Modified:     January 11 1997
        //---------------------------------------------------------------
        private void DRAWCHT(frmCONOP frm, Graphics g, int trj, int[] HPERM)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            int I, typsc;
            int dinc, horscl, font, topfont, dum1, dum2;
            double xreal, yreal;
            //CPMS--WINDOWS Font Management
            int numfonts, index;
            int horiz, vert;
            int scl, top, btm, lft, rit;

            //C----------------------------------------------------------------------
            //CPMS       limits of screen resolution:   maxx  maxy
            //CPMS       margins outside graph box:   top  btm  lft  rit
            //C----------------------------------------------------------------------
            string ltr;

            string xtitle = "";
            string ytitle = "";
            string etitl = "";

            //CPMS----------------------------------------------
            //CPMS  TRJ.LT.2   redraw whole graph
            //CPMS  TRJ.EQ.2   refresh range lines
            //CPMS  TRJ.EQ.3   skeleton graph for event plots
            //CPMS----------------------------------------------
            scl = 0;
            top = 0;
            btm = 50;
            lft = 100;
            rit = 0;
            //CPMS-----------------------------------------------
            horiz = 0;
            vert = -90;//900 in Fortran

            I = 1 - 1;//<=HXD
            Brush brush = frm.MSG_BRUSH_WHITE;
            Pen pen = new Pen(brush);

        Label100:

            //CPMS-----------------------------------------
            if (trj != 2)
            {
                //pause to allow viewing of previous graph
                //clear the screen
                g.Clear(frm.BackColor);

                //draw the axis with current font
                XSCALE(frm, g, COMMOD9.NEVNT, COMMOD9.maxx, COMMOD9.maxy, lft, rit, btm);

            }

            //CPMS---------------------------------------------------------------------
            if (trj != 3)
            {
                //CPMS    Draw full range lines for paired events only
                //C	  if number of available lines is less than the number of taxa+others
                //C  then the following needs to be rewritten so that
                //C  1.  it allots one line to each event
                //C  2.  it stops drawing event ranges after the bottom line on the screen

                for (scl = 0; scl < COMMOD9.NEVNT; scl++)
                {
                    if ((COMMOD9.IROWS[HPERM[scl], 1] == 1) ||
                        (COMMOD9.IROWS[HPERM[scl], 1] == 11))
                    {
                        //draw full white/gold line for FADs and MAXsonly, to represent
                        //FAD-LAD pairs or FAD-MID-LAD triplets, or MAX-MIN pairs
                        brush = frm.MSG_BRUSH_WHITE;
                        pen = new Pen(brush);

                        if (COMMOD9.IROWS[HPERM[scl], 1] == 11)
                        {
                            brush = frm.MSG_BRUSH_14;
                            pen = new Pen(brush);
                        }

                        //they will be clipped to range length below
                        //other lines are blanks, awaiting a colored dash
                        //if this is the Ith taxon, vchange the line color
                        if ((trj == 0) && (COMMOD9.IROWS[HPERM[scl], 2] == I))
                        {
                            brush = frm.MSG_BRUSH_11;
                            pen = new Pen(brush);
                            Helper.GETEVNT(HPERM[scl], ref etitl, 1, 1, 0, 0, 0, 1);
                            g.TranslateTransform((int)(lft / 2), (int)(COMMOD9.maxy - btm));
                            g.RotateTransform(vert);
                            g.DrawString(etitl, frm.MSG_FONT_S, brush, 0, 0);
                            g.ResetTransform();
                        }

                        yreal = (double)(COMMOD9.IROWS[HPERM[scl], 0] + 1) * COMMOD9.YSPAN / COMMOD9.YROWS;

                        g.DrawLine(pen, lft, top + (int)(yreal), COMMOD9.maxx - rit, top + (int)(yreal));
                    }
                }


                //CPMS---------------------------------------------------------------------
                //CPMS    Clip the range lines
                //CPMS
                //CPMS    the horizontal position is given by IROWS(HPERM(scl),1
                //CPMS    the end to cut is given by IROWS(HPERM(scl),2
                //CPMS    the length of cut is given by position in HPERM (i.e. scl)
                brush = frm.MSG_BRUSH;
                pen = new Pen(brush);

                for (scl = 0; scl < COMMOD9.NEVNT; scl++)
                {
                    typsc = COMMOD9.IROWS[HPERM[scl], 1];
                    xreal = (double)(scl + 1) * COMMOD9.XSPAN / (double)(COMMOD9.NEVNT);
                    yreal = (double)(COMMOD9.IROWS[HPERM[scl], 0] + 1) * COMMOD9.YSPAN / COMMOD9.YROWS;
                    if ((typsc == 1) || (typsc == 11))
                    {
                        //paired event - FAD or MAX - cut from left
                        g.DrawLine(pen, lft, top + (int)(yreal), lft + (int)(xreal), top + (int)(yreal));
                    }
                    else if ((typsc == 2) || (typsc == 12))
                    {
                        // paired event - LAD or MIN - cut to right  
                        g.DrawLine(pen, lft + (int)(xreal), top + (int)(yreal), COMMOD9.maxx - rit, top + (int)(yreal));
                    }
                    else if ((typsc >= 3) || (typsc < 1))
                    {
                        brush = frm.MSG_BRUSH_14;
                        pen = new Pen(brush);

                        if (typsc == 3)
                        {
                            brush = frm.MSG_BRUSH_12;
                            pen = new Pen(brush);
                        }


                        g.DrawLine(pen, lft + (int)(xreal) - 1, top + (int)(yreal), lft + (int)(xreal) + 1, top + (int)(yreal));

                        brush = frm.MSG_BRUSH;
                        pen = new Pen(brush);
                    }
                }
            }

            if (trj != 2)
            {
                //Add the legend and scales
                brush = frm.MSG_BRUSH_WHITE;

                xtitle = "TIME SCALE WITH ONE UNIT FOR EVERY OBSERVED EVENT";

                if (trj == 3)
                {
                    ytitle = "    Penalty vs Position of One Event in Sequence";
                }
                else
                {
                    ytitle = "TAXON NUMBER";
                }

                g.DrawString(xtitle.Trim(), frm.MSG_FONT_XS, brush, (int)(COMMOD9.maxx / 1.7), (int)(COMMOD9.maxy - btm / 1.9));

                if (trj == 3)
                {
                    Helper.GETEVNT(COMMOD9.XEVNT, ref xtitle, 1, 1, 1, 1, 1, 1);
                    brush = frm.MSG_BRUSH_10;
                    g.DrawString(xtitle.Trim(), frm.MSG_FONT_XS, brush, 5, (int)(COMMOD9.maxy - 32));
                }

                brush = frm.MSG_BRUSH_12;
                g.DrawString(COMMOD9.PROJNAME, frm.MSG_FONT_XS, brush, 5, (int)(COMMOD9.maxy - 12));
            }

            if (trj != 2)
            {
                brush = frm.MSG_BRUSH_WHITE;

                if (trj == 3) { brush = frm.MSG_BRUSH_10; }

                g.TranslateTransform((int)(lft / 2.7), (int)(COMMOD9.maxy - btm - 25));
                g.RotateTransform(vert);
                g.DrawString(ytitle.Trim(), frm.MSG_FONT_XS, brush, 0, 0);
                g.ResetTransform();

                if (COMMOD9.TRJGRF > 0 && trj == 1) { brush = frm.MSG_BRUSH_10; }

                WRITEPEN(frm, g, brush, 1);

            }

            if (trj != 2)
            {
                //CALL setgtextvector(INT2(0),INT2(1))??

                switch (COMMOD9.SOLVEF)
                {
                    case 4:
                    case 5:
                        frm.SetColor(1);
                        g.DrawString("Squeeze Penalty", frm.MSG_FONT_XS, frm.DW_Brush,
                            (int)(lft / 2.7), (int)(COMMOD9.maxy - btm - 240));
                        break;
                    case 6:
                    case 7:
                        frm.SetColor(1);
                        g.DrawString("Shrink Penalty", frm.MSG_FONT_XS, frm.DW_Brush,
                            (int)(lft / 2.7), (int)(COMMOD9.maxy - btm - 240));
                        break;
                    case 8:
                    case 9:
                        frm.SetColor(1);
                        g.DrawString("Tease (" + COMMOD9.STAKNAME + ") Penalty", frm.MSG_FONT_XS, frm.DW_Brush,
                            (int)(lft / 2.7), (int)(COMMOD9.maxy - btm - 240));
                        break;
                }

                if ((COMMOD9.SOLVEF != 2) && (COMMOD9.SOLVEF != 4) && (COMMOD9.SOLVEF != 6) &&
                    (COMMOD9.SOLVEF != 8))
                {
                    frm.SetColor(4);
                    g.TranslateTransform((int)(lft / 4), (int)(COMMOD9.maxy - btm - 190));
                    g.RotateTransform(vert);
                    g.DrawString("COOLING SCHEDULE", frm.MSG_FONT_XS, frm.DW_Brush, 0, 0);
                    g.ResetTransform();

                    g.DrawString("zero temp", frm.MSG_FONT_S, frm.DW_Brush, (int)(lft / 10), (int)(COMMOD9.maxy - btm));

                    if (COMMOD9.PAUSF == 5)
                    {
                        //track NTRIES
                        if (COMMOD9.NTRIES > 0)
                        {
                            COMMOD9.CNTRIES = COMMOD9.NTRIES;
                        }
                        else
                        {
                            COMMOD9.LSTRIES = COMMOD9.CNTRIES;
                        }

                        DRAWTEMP(frm, g);
                    }
                }

                frm.SetColor(15);
            }

            if (trj != 2)
            {

                if (trj < 2)
                {
                    //label every 10th taxon  
                    for (scl = 9; scl < COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS; scl += 10)
                    {
                        yreal = (double)(scl + 1) * COMMOD9.YSPAN / COMMOD9.YROWS;

                        g.DrawString(string.Format("{0}", scl), frm.MSG_FONT_S, frm.DW_Brush,
                            lft - 25, top - (int)(yreal) - 5);
                    }
                }

                //CALL moveto(lft,top, xy)
            }

            if ((trj == 0) && (COMMOD9.PAUSF == 0))
            {
                //CALL NAVIKEY(I,NSPC,font,topfont,dinc,horscl,dum1,dum2,ltr)
                //IF((I.eq.0).or.(ltr.eq.'Q'))return;
                // GOTO 100
            }

        }

        //cpms  fills the EVERYBST array for BESTFILE
        protected void FILLBEST(int ISCOR, double RSCOR)
        {
            int X, Y;

            //cpms   - the penalty for the current solution
            //cpms   - goes into the BESTFILE
            //CPMS*****************************************************
            //CPMS	  run through the solution in sequence (EVERYBST cols)
            //CPMS	  and enter the penalty for the appropriate
            //CPMS	  event (EVERYBST rows), IF the best so far
            //CPMS	  i.e. EVERYBST has IROWS order

            COMMOD COMMOD9 = COMMOD.Singleton();

            for (X = 0; X < COMMOD9.NEVNT; X++)
            {
                COMMOD9.EVERYBST[COMMOD9.PERM[X], X] =
                    Math.Min(COMMOD9.EVERYBST[COMMOD9.PERM[X], X], RSCOR);

                //cpms   update secondary penalty if necessary
                //cpms   SECNDBST stores secondary penalty BEFORE scaling: TO2PEN
                if (COMMOD9.GRID2F == 1)
                {
                    COMMOD9.SECNDBST[COMMOD9.PERM[X], X] =
                        Math.Min(COMMOD9.SECNDBST[COMMOD9.PERM[X], X], COMMOD9.TO2PEN);
                }
            }

            if (COMMOD9.VERYBEST == COMMOD9.NXTPEN)
            {
                for (X = 0; X < COMMOD9.NEVNT; X++)
                {
                    for (Y = 0; Y < COMMOD9.NSCT; Y++)
                    {
                        COMMOD9.HSCTRNG[X, Y, 0] =
                            Math.Max(COMMOD9.HSCTRNG[X, Y, 0], COMMOD9.HSCTSOL[X, Y]);

                        COMMOD9.HSCTRNG[X, Y, 1] =
                           Math.Min(COMMOD9.HSCTRNG[X, Y, 1], COMMOD9.HSCTSOL[X, Y]);
                    }
                }
            }
            else if (COMMOD9.VERYBEST > COMMOD9.PEN)
            {
                for (X = 0; X < COMMOD9.NEVNT; X++)
                {
                    for (Y = 0; Y < COMMOD9.NSCT; Y++)
                    {
                        COMMOD9.HSCTRNG[X, Y, 0] = COMMOD9.HSCTSOL[X, Y];
                        COMMOD9.HSCTRNG[X, Y, 1] = COMMOD9.HSCTSOL[X, Y];
                    }
                }

                COMMOD9.VERYBEST = Helper.MinVal(COMMOD9.EVERYBST);

            }

        }

        private int newx(int xcoord)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            double tempx = 0;
            //int top=0;
            //int btm = 50;
            int lft = 100;
            int rit = 0;

            tempx = (COMMOD9.maxx - lft - rit) / 1000.0;
            tempx = xcoord * tempx + 0.5 + lft;
            return (int)tempx;
        }

        private int newy(int ycoord)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            double tempy = 0;
            int top = 0;
            int btm = 50;
            //int lft = 100;
            //int rit=0;

            tempy = (COMMOD9.maxy - top - btm) / 1000.0;
            tempy = ycoord * tempy + 0.5 + top;
            return (int)tempy;
        }

        private void DRAWSCOR(frmCONOP frm, Graphics g, double xone, double xtwo, double yone, double ytwo, int side, int fill)
        {

            int prcnt, pnlty;

            //CPMS----------------------------------------------------------
            //c    xone yone     maximum value on given axis
            //c    xtwo ytwo     plotted value on given axis
            //c    side          symbol size
            //c    fill          filled symbol
            //CPMS----------------------------------------------------------


            prcnt = (int)(1000 * (xone - xtwo) / xone);
            pnlty = (int)(1000 * (yone - ytwo) / yone);



            if (fill == 1)
            {
                if (side == 0)
                {
                    g.DrawLine(frm.DW_Pen, newx(prcnt) - (int)side - 0.5f, newy(pnlty) - (int)side - 0.5f,
                        newx(prcnt) - (int)side + 0.5f, newy(pnlty) - (int)side + 0.5f);
                }
                else
                {
                    g.DrawRectangle(frm.DW_Pen, newx(prcnt) - (int)side, newy(pnlty) - (int)side,
                        (int)side * 2, (int)side * 2);
                }

            }
            else if (fill != 1)
            {
                if (side == 0)
                {
                    g.DrawLine(frm.DW_Pen, newx(prcnt) - (int)side - 0.5f, newy(pnlty) - (int)side - 0.5f,
                        newx(prcnt) - (int)side + 0.5f, newy(pnlty) - (int)side + 0.5f);
                }
                else
                {
                    g.FillRectangle(frm.DW_Brush, newx(prcnt) - (int)side, newy(pnlty) - (int)side,
                        (int)side * 2, (int)side * 2);
                }
            }

        }


        //C***********************************************************************
        //C   Rounds integer to nearest Ix10^C that is larger
        //C   For scaling graph axes to nearest larger round number
        //C
        //C   Pete Sadler    Sept 19 1998
        //C
        //C***************************************************************
        private void ROUNDUP(ref int D, int res, ref int div)
        {
            int I;

            if (res == 0)
            {
                //find next highest power of ten
                for (I = 9; I >= 0; I--)
                {
                    if (D >= (int)(Math.Pow(10, I)))
                    {
                        D += (int)(Math.Pow(10, I)) - (int)(D % (Math.Pow(10, I)));
                        div = (int)(D / (Math.Pow(10, I)));
                        break;
                    }
                }
            }
            else
            {
                //c	find next highest factor of 10 raised to power res
                //c   this allows more sensitive vertical scales
                //c   but makes subdivision harder (div=0 means no ticks!)
                I = res;
                D += (int)(Math.Pow(10, I)) - (int)(D % (Math.Pow(10, I)));
                div = 0;
            }

        }

        //CPMS*****************************************************************
        //CPMC   labels the y-axis for critical scores
        //CPMS*****************************************************************    
        private void LBLSCOR(frmCONOP frm, Graphics g, string score, int pnlty)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            //scl = 0;
            //top = 0;
            //btm = 50;
            int lft = 100;
            //rit = 0;

            g.DrawString(score.Trim(), frm.MSG_FONT_XS, frm.DW_Brush, (int)((double)lft / 2.5),
                (int)(newy(pnlty)));
        }

        public void RunItAndDraw(frmCONOP frm, Graphics g)
        {
            double DELTA;

            double U;
            int INNER, IRANK, JRANK, LAST, OUTER, NCTR, FIXM, NABRM;
            int I, L, X, NCT;

            //CPMS  Declarations for graphics:      
            //CPMS  PCT  Percentage annealed
            //CPMS  INISCOR - extension penalty for initial solution
            //CPMS  BSTSCOR - best extension penalty to date
            //CPMS  SCORes do not include the smoothing factors                          
            //CPMS  I??SCOR - initial scores for scaling the screen plots

            int INISCOR, ISMSCOR, ISQSCOR, ISHSCOR, ITSSCOR, INGSCOR;
            int BSTSCOR, AUTSCOR;
            double PCT;
            int dummy, ulx, uly, lrx, lry, res, siz;
            string SCORE;

            int horiz, vert, div;
            bool rndf, stepf, aimf, redf, trjf, algf, rlxf, rngf, hilf, s45f, s67f, s89f;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //CPMS************************************************
            //CPMS***PRELIMINARIES
            //CPMS	 next section executes only once in a run
            //CPMS------------------------------------------------

            div = 0;
            res = 0;

            int scl = 0;
            int top = 0;
            int btm = 50;
            int lft = 100;
            int rit = 0;

            //C     initialize things for Intel 11.1  
            COMMOD9.UU = 0.0;
            U = 0.0;
            PCT = 0.0;
            DELTA = 0.0;
            INNER = 0;
            IRANK = 0;
            JRANK = 0;
            LAST = 0;
            OUTER = 0;
            NCTR = 0;
            FIXM = 0;
            NABRM = 0;
            I = 0;
            L = 0;
            X = 0;
            NCT = 0;

            //C   initialized for Intel 9.0
            horiz = 0;
            vert = -90;//900  in Fortran
            rndf = true;
            INISCOR = 0;
            ITSSCOR = 0;
            ISMSCOR = 0;
            ISQSCOR = 0;
            ISHSCOR = 0;
            INGSCOR = 0;
            AUTSCOR = 0;
            BSTSCOR = 0;

            if ((COMMOD9.AUTF == 0) && (COMMOD9.CONTF == 0))
            {
                COMMOD9.BSTPEN = 0.0;
                COMMOD9.BS2PEN = 0.0;
            }

            COMMOD9.TOOPEN = 0.0;
            COMMOD9.TO2PEN = 0.0;
            COMMOD9.TSPEN = 0.0;
            COMMOD9.SMPEN = 0.0;
            COMMOD9.SHPEN = 0.0;
            COMMOD9.CRTPEN = 0.0;
            COMMOD9.NEWRANK = 0;
            //c  NEWRANK is the rank to which the last (BIG|SML) mutation moved an event

            //CPMS  set the screen grabber corners
            ulx = 0;
            uly = 0;
            lrx = COMMOD9.maxx - 1;
            lry = COMMOD9.maxy - 1;

            //C--------------------------------
            //C     set loop-accelerator flags
            //C     step-solutions (outer loop):
            stepf = false;
            if ((COMMOD9.STEPSOL.Substring(0, 3) != "OFF") && (COMMOD9.STEPSOL.Substring(0, 3) != "off"))
            {
                stepf = true;
            }

            //C  aimed tempering (outer loop):

            aimf = false;
            if ((COMMOD9.SOLVEF == 3) && (COMMOD9.FIXF == 2)) aimf = true;

            //C  animated ranges:
            rngf = false;
            if ((COMMOD9.RUNGRF == 2) || (COMMOD9.RUNGRF == 5)) rngf = true;

            //C  penalty trajectory:
            trjf = false;
            if (((COMMOD9.RUNGRF == 2) || (COMMOD9.RUNGRF == 0)) && (COMMOD9.TRJGRF > 0)) trjf = true;

            //C  red temp curve (inner loop):
            redf = false;
            if (trjf)
            {
                if ((COMMOD9.RUNGRF != 0) && (COMMOD9.SOLVEF != 2) && (COMMOD9.SOLVEF != 4) &&
                   (COMMOD9.SOLVEF != 6) && (COMMOD9.SOLVEF != 8)) redf = true;
            }

            //C  greedy algorithm:
            algf = false;
            if ((COMMOD9.SOLVEF == 2) || (COMMOD9.SOLVEF == 4) || (COMMOD9.SOLVEF == 6) ||
               (COMMOD9.SOLVEF == 8)) algf = true;

            //C  relaxed fit update:
            rlxf = false;
            if ((COMMOD9.RUNGRF >= 4) || (COMMOD9.GRIDF == 1)) rlxf = true;
            //C  uphill moves in different color:	
            hilf = false;
            if ((COMMOD9.SOLVEF != 2) && (COMMOD9.TRJGRF >= 3)) hilf = true;

            //C  SOLVEF 4 or 5 (squeeze):
            s45f = false;
            if ((COMMOD9.SOLVEF == 4) || (COMMOD9.SOLVEF == 5)) s45f = true;

            //C  SOLVEF 6 or 7 (shrink)
            s67f = false;
            if ((COMMOD9.SOLVEF == 6) || (COMMOD9.SOLVEF == 7)) s67f = true;
            //C     SOLVEF 8 or 9 (stack)
            s89f = false;
            if ((COMMOD9.SOLVEF == 8) || (COMMOD9.SOLVEF == 9)) s89f = true;


            //C--------------------------------
            //C     set the initial temperature
            NCTR = 0;
            COMMOD9.CTRF = 0;
            COMMOD9.NBETR = 0;

            if (COMMOD9.CONTF == 1) goto Label111;

            switch (COMMOD9.AUTF)
            {
                case 0:
                    //cpms  not PAUSES='AUT','ADA'
                    COMMOD9.T = COMMOD9.STARTT;
                    break;
                case 1:
                    //cpms  still improving in last TRIALS - don't cool
                    //COMMOD9.T=COMMOD9.T;
                    COMMOD9.Rx = COMMOD9.R;
                    break;
                case -1:
                    //cpms   nothing better found in last TRIALS - cool	   	
                    //c         The following line cools by a fixed amount
                    //c	        T = T*R           
                    //c         In the alternative the size of the
                    //c         cooling step increases as the  
                    //c         size of the null sets increases 
                    //c         for adaprive cooling
                    COMMOD9.Rx = COMMOD9.R;
                    if ((COMMOD9.ADAF == 1) && (COMMOD9.NTRIES > 0) && (COMMOD9.LSTRIES > 1))
                    {
                        L = COMMOD9.RANDOM.Next(Math.Min(COMMOD9.LSTRIES, 5) + 1);//<=HXD
                        COMMOD9.Rx = Math.Pow(COMMOD9.R, L);
                        COMMOD9.NTRIES = COMMOD9.NTRIES + (L - 1);
                    }

                    COMMOD9.T = COMMOD9.T * COMMOD9.Rx;

                    break;
            }

        Label111:
            PCT = COMMOD9.NINNER * COMMOD9.NOUTER;

            //C--------------------------------------------------------
            //CPMS  alter OUTF so that GETPEN does not write to outmain
            //CPMS
            //**      OUTF = OUTF-100
            //CPMS
            //C     COMPUTE THE BEST PLACEMENTS THAT COMPLY WITH STARTING PERM 
            //C       "INIPERM" AND RESULTING PENALTY. 
            //C          PUT THE placements in SCTSOL and the PENALTY IN PEN
            //C
            //C     for the first permutation, we need a complete solution.
            //C         for subsequent perms, we may use reoptimization.    
            //C---------------------------------------------------------    
            if (COMMOD9.PENF <= 1)
            {
                Helper.GETPEN(COMMOD9.INIPERM, ref COMMOD9.NXTPEN);
            }
            else if (COMMOD9.PENF >= 2)
            {
                Helper.DEMPEN(COMMOD9.INIPERM, ref COMMOD9.NXTPEN);
            }

            //CPMS*********************************************
            //CPMS  PRELIMINARIES  -  not included in loops
            Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.PERM);
            Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.LSTPERM);
            Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.PXLPERM);

            if (COMMOD9.CONTF == 0) Helper.CopyArray(COMMOD9.INIPERM, ref COMMOD9.BSTPERM);

            COMMOD9.PEN = COMMOD9.NXTPEN;
            COMMOD9.INIPEN = COMMOD9.NXTPEN;
            INISCOR = (int)COMMOD9.INIPEN;
            AUTSCOR = (int)COMMOD9.NXTPEN;

            if (COMMOD9.AUTF == 0)
            {
                if (COMMOD9.CONTF == 0) COMMOD9.BSTPEN = COMMOD9.NXTPEN;
            }
            else
            {
                if (COMMOD9.CONTF == 0) COMMOD9.BSTPEN = COMMOD9.AUTPEN;
            }

            BSTSCOR = (int)COMMOD9.BSTPEN;

            //Helper.DOTOO();

            COMMOD9.NGTPEN = COMMOD9.NGHPEN;
            INGSCOR = (int)COMMOD9.NGTPEN;

            if ((COMMOD9.AUTF == 0) && (COMMOD9.CONTF == 0))
            {
                COMMOD9.BS2PEN = COMMOD9.TOOPEN;
                COMMOD9.SQBST = COMMOD9.SQPEN;
                COMMOD9.SHBST = COMMOD9.SHPEN;
                COMMOD9.TSBST = COMMOD9.TSPEN;
            }

            ISQSCOR = (int)COMMOD9.SQPEN;
            ISHSCOR = (int)COMMOD9.SHPEN;
            COMMOD9.TSLST = COMMOD9.TSPEN;
            ITSSCOR = (int)COMMOD9.TSPEN;

            ISMSCOR = (int)COMMOD9.SMPEN;

            if ((COMMOD9.KSM > 0.0) && (COMMOD9.PENF < 2))
            {
                ISMSCOR = (int)(COMMOD9.SMPEN / COMMOD9.KSM);
                INISCOR = (int)(COMMOD9.PEN - COMMOD9.SMPEN);
                BSTSCOR = (int)(COMMOD9.BSTPEN - COMMOD9.SMPEN);
            }

            if (COMMOD9.CTRF == 1)
            {
                NCT = NCT + 1;
                COMMOD9.CTRF = 0;
            }

            if (COMMOD9.GRID2F == 1)
            {
                //cpms	 make sure that the .gr2 file records initial permutation

                for (X = 0; X < COMMOD9.NEVNT; X++)
                {
                    //cpms  update secondary penalty 
                    COMMOD9.SECNDBST[COMMOD9.PERM[X], X] =
                        Math.Min(COMMOD9.SECNDBST[COMMOD9.PERM[X], X], COMMOD9.TO2PEN);
                }
            }


            //C-----------------------------------------------------------------------      
            //C     to speed up reoptimization (in EVALUATE), keep track of 
            //C         the last action taken during annealing. 
            //C     if the new PERM was accepted,
            //C         SCTSOL and HSCTSOL will already be the same, and there is no need 
            //C         to copy SCTSOL to HSCTSOL before reoptimization. 
            //C     if the new permutation is not accepted, copy SCTSOL to HSCTSOL
            //C     use the flag LAST:
            //C        LAST = 1, accepted the new PERM
            //C        LAST = 0, did not accept the new PERM
            //C     since GETPEN calculates HSCTSOL, and we assign that to SCTSOL, 
            //C         initialize it to 1.    
            //C-----------------------------------------------------------------------
            //C     put the initial perm in PERM, the initial penalty in PEN, and       
            //C     USE PERM AND PEN AS THE INITIAL "BEST" VALUES
            //C-----------------------------------------------------------------------

            Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
            Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            LAST = 1;
            COMMOD9.XPOSN = 0;

            if (COMMOD9.FIXF == 3)
            {
                for (COMMOD9.XPOSN = 0; COMMOD9.XPOSN < COMMOD9.NEVNT; COMMOD9.XPOSN++)
                {
                    if (COMMOD9.PERM[COMMOD9.XPOSN] == COMMOD9.XEVNT) break;
                }
            }

            //HACK:OMIT MANY LINES
            //CPMS  --if plotting during run-----------------
            if (rngf)
            {
                if (COMMOD9.CONTF == 1)
                {
                    DRAWCHT(frm, g, 1, COMMOD9.INIPERM);
                }
                else
                {
                    DRAWCHT(frm, g, 1, COMMOD9.BSTPERM);
                }
            }
            else if (COMMOD9.RUNGRF == 4)
            {
                if (COMMOD9.CONTF == 1)
                {
                    DRAWCHT(frm, g, 3, COMMOD9.INIPERM);
                }
                else
                {
                    DRAWCHT(frm, g, 3, COMMOD9.BSTPERM);
                }
            }
            else if (COMMOD9.MOVEF == 1)
            {
                //CALL EDITDIVE(0) ????
            }

            if (((COMMOD9.RUNGRF == 2) || (COMMOD9.RUNGRF >= 4)) && (COMMOD9.TRJGRF > 0))
            {
                //---if filling curves, decrease INISCOR to enlarge lower half of curve
                if ((COMMOD9.RUNGRF == 4) && (COMMOD9.INIGEN != 2)) INISCOR = (int)(INISCOR / 2);

                //---if tracking secondary penalties
                //---scale them to half screen height
                switch (COMMOD9.SOLVEF)
                {
                    case 4:
                    case 5:
                        if (COMMOD9.INIGEN != 2) ISQSCOR = ISQSCOR * 2;
                        break;
                    case 6:
                    case 7:
                        if (COMMOD9.INIGEN != 2) ISHSCOR = ISHSCOR * 2;
                        break;
                    case 8:
                    case 9:
                        if (COMMOD9.INIGEN != 2) ITSSCOR = ITSSCOR * 2;
                        break;
                }

                if (COMMOD9.NEGATF > 0) INGSCOR = INGSCOR * 4;

                //CPMS   ---if starting from a previous solution on file--------------
                if (COMMOD9.INIGEN == 2)
                {
                    //starting from file gives a low initial penalty and the search
                    //frequently climbs higher; so the initial penalty is scaled to
                    //half the screen height! 
                    INISCOR = INISCOR * 2;

                    //and the secondary penalty is scaled to 2/3 screen height
                    ISQSCOR = (int)((double)(ISQSCOR) * 1.3);
                    ISHSCOR = (int)((double)(ISHSCOR) * 1.3);
                    ITSSCOR = (int)((double)(ITSSCOR) * 1.3);

                    //and any negative penalty is scaled to 1/3 height
                    INGSCOR = (int)((double)(INGSCOR) * 3);

                    frm.SetColor(10);

                    if (COMMOD9.AUTF == 0) rndf = false;

                    YSCALE(frm, g, INISCOR, COMMOD9.maxx, COMMOD9.maxy, top, btm, lft - 1, res, rndf);

                    frm.SetColor(8);

                    SCORE = string.Format("{0}", INISCOR);
                    LBLSCOR(frm, g, SCORE, 0);

                    //label the initial penalty
                    if (COMMOD9.AUTF == 0) frm.SetColor(10);
                    SCORE = string.Format("{0}", AUTSCOR);
                    LBLSCOR(frm, g, SCORE, 1000 * (INISCOR - AUTSCOR) / INISCOR);

                    if (COMMOD9.AUTF != 0)
                    {
                        //label the best score (carried forward)
                        frm.SetColor(10);
                        SCORE = string.Format("{0}", BSTSCOR);
                        LBLSCOR(frm, g, SCORE, 1000 * (INISCOR - BSTSCOR) / INISCOR);
                    }
                }
                else//---else if starting from scratch------
                {
                    //make top of y-axis a round number
                    //with resolution "res"
                    res = 0;
                    frm.SetColor(10);
                    YSCALE(frm, g, INISCOR, COMMOD9.maxx, COMMOD9.maxy, top, btm, lft - 1, res, rndf);
                }

                //  plot the initial penalties:
                //  the next line had ninner twice: a mistake?
                if ((COMMOD9.NINNER > 0) && (COMMOD9.NOUTER > 0) && (rngf))
                {
                    DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(INISCOR), (double)(BSTSCOR), 2, 1);

                    if (COMMOD9.NEGATF > 0)
                    {
                        //plot negative penalty in brown
                        frm.SetColor(6);
                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(INGSCOR), (double)(COMMOD9.NGHPEN), 0, 1);
                    }


                    //plot secondary penalties in dark blue
                    frm.SetColor(1);

                    switch (COMMOD9.SOLVEF)
                    {
                        case 1:
                        case 2:
                        case 3:
                            break;
                        case 4:
                        case 5:
                            DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ISQSCOR), (double)(COMMOD9.SQPEN), 2, 1);
                            break;
                        case 6:
                        case 7:
                            DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ISHSCOR), (double)(COMMOD9.SHPEN), 2, 1);
                            break;
                        case 8:
                        case 9:
                            DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ITSSCOR), (double)(COMMOD9.TSPEN), 2, 1);
                            break;
                    }

                    frm.SetColor(15);
                }
                else if ((COMMOD9.NINNER > 0) && rlxf) //ELSE IF((NINNER.GT.0.).AND.(NINNER.GT.0.).AND.rlxf) THEN   <=HXD??
                {
                    FILLBEST(frm, g, INISCOR, (double)BSTSCOR);
                }
                else
                {
                    frm.SetColor(15);
                    return;//???<=HXD
                }
            }

            if (COMMOD9.RUNGRF == 4)
            {
                //plot the xevnt position
                frm.SetColor(8);
                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    if (INISCOR > (int)(COMMOD9.EVERYBST[COMMOD9.XEVNT, I]))
                    {
                        DRAWSCOR(frm, g, (double)COMMOD9.NEVNT, (double)(COMMOD9.NEVNT - (I + 1)),
                            (double)(INISCOR), COMMOD9.EVERYBST[COMMOD9.XEVNT, I], 1, 1);
                    }
                }

                frm.SetColor(0);
            }


            //cpms---reset CONTF after first pass through--------------

            COMMOD9.CONTF = 0;

            //CPMS*******************************************************************
            //CPMS  MAIN LOOPS
            //C     The following lines are executed with every trial!!
            //C     Analyse a big sequence of neighbors ('permutations'). 
            //C     Compute the penalty and decide whether to accept the new perm
            //C     according to the SOLVER option
            //C-------------------------------------------------------------      
            //CPMS---Store two run parameters in case the last temper quench is
            //CPMS   to be "aimed" into the optimum with different parameters
            //CPMS   and the program must reset for another "RPT" run.
            FIXM = COMMOD9.FIXF;
            NABRM = COMMOD9.NABRGEN;

            //Helper.Write("\n  ");
            //if (COMMOD9.PAUSF != 5)
            //{
            //    Console.BackgroundColor = ConsoleColor.Blue;
            //}

            //CPMS**START THE OUTER LOOP***************
            for (OUTER = 0; OUTER < (int)COMMOD9.NOUTER; OUTER++)
            {

                //CPMS	check for "AIM"ed tempering
                if ((aimf) && (OUTER == (int)COMMOD9.NOUTER - 1))
                {
                    COMMOD9.FIXF = 0;
                    COMMOD9.NABRGEN = 2;
                }

                //CPMS  write out best solution so far if requested
                if ((stepf) && (COMMOD9.CDF != 1)) Helper.STEPOUT(COMMOD9.BSTPERM);
                if ((stepf) && (COMMOD9.CDF != 1)) Helper.TRAJOUT();

                //CPMS*****START THE INNER LOOP*********
                for (INNER = 0; INNER < (int)COMMOD9.NINNER; INNER++)
                {

                    PCT = PCT - 1;

                    //Draw the red temperature trajectory on the screen
                    //if not "greedy" or "squeeze" or "shrink" or "tease"
                    if (redf)
                    {
                        frm.SetColor("BB0000");
                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, COMMOD9.STARTT, COMMOD9.T, 0, 1);
                        frm.SetColor(15);
                    }


                    //CPMS-----------------------------------------------------     
                    //CPMS       GENERATE A NEIGHBOR AS A CANDIDATE PERMUTATION
                    //CPMS       after saving current state of secondary penalty
                    COMMOD9.TSLST = COMMOD9.TSPEN;

                    Helper.GONABR(ref IRANK, ref JRANK);


                    //CPMS******    This replaces the old calculation of PROB after penalty totalling
                    //CPMS******    It allows NEWPEN to exit the section loop as soon as the critical
                    //CPMS******    penalty size (CRTPEN) is reached.
                    //CPMS******    NEWPEN takes advantage of this, but NWDMPEN does not.....yet
                    //CPMS******    for a riley test, running time drops from 1.133 to 1.117 mins.
                    COMMOD9.UU = COMMOD9.RANDOM.NextDouble();

                    //if(COMMOD9.PEN>4000 && COMMOD9.PEN<5000)
                    //    Helper.Write(" T={0} UU={1} Pen={2} ", COMMOD9.T,COMMOD9.UU,COMMOD9.PEN);
                    COMMOD9.CRTPEN = COMMOD9.PEN - (COMMOD9.T * Math.Log(COMMOD9.UU));
                    //if (COMMOD9.PEN > 4000 && COMMOD9.PEN < 5000)
                    //    Helper.Write("CRTPEN={0}\n", COMMOD9.CRTPEN);


                    if (COMMOD9.PENF <= 1)
                    {
                        Helper.NEWPEN(IRANK, JRANK, COMMOD9.PERM, ref COMMOD9.NXTPEN);

                    }
                    else if (COMMOD9.PENF >= 2)
                    {
                        Helper.NWDMPEN(IRANK, JRANK, COMMOD9.PERM, ref COMMOD9.NXTPEN);
                    }


                    DELTA = COMMOD9.NXTPEN - COMMOD9.PEN;


                    //C-----------------------------------------------------------------------      
                    //C    DETERMINE WHETHER TO ACCEPT THE NEW PERM OR KEEP THE
                    //C    OLD ONE
                    //C    PEN is the previous penalty
                    //C    NXTPEN is the candidate to replace PEN
                    //C    DELTA is the difference
                    //C----------------------------------------------------------------------- 

                    //C-----------------------------------------------------------------------
                    //c          set the flag LAST to 0, saying that the new PERM 
                    //C          has not been accepted yet      
                    //C
                    //C          IF THE NEW PERM IS BETTER (OR AS GOOD and not "GREEDY"), 
                    //C            1. ACCEPT IT, keep PERM, update PEN, TOOPEN and SCTSOL, AND 
                    //C            2. SEE IF IT IS BEST SO FAR
                    //C             IF IT IS, UPDATE THE BEST SO FAR (BSTPERM AND BSTPEN)
                    //C		      If a "SQUEEZE" run, accept only if squeeze penalty is not worse
                    //C-----------------------------------------------------------------------         

                    LAST = 0;

                    //CPMS       ? replace the next IF with DELTA.LE.0
                    //CPMS       i.e. always accept an equally good move and save the checking time
                    //CPMS       ?? leads to lots of repetitions
                    //CPMS       ??? what is the snag for greedy?  
                    //c          /////////////////////////////////////////      
                    //CPMS       IF NEW SEQUENCE HAS SAME OR LOWER PENALTY
                    if ((DELTA < 0.0) ||
                        ((DELTA == 0.0) && ((COMMOD9.SOLVEF < 2) ||
                        ((s89f) && (COMMOD9.TSPEN <= COMMOD9.TSBST)) ||
                        ((s67f) && (COMMOD9.SHPEN <= COMMOD9.SHBST)) ||
                        ((s45f) && (COMMOD9.SQPEN <= COMMOD9.SQBST)))))
                    {
                        if (COMMOD9.MOVEF == 1)
                        {
                            //CALL EDITDIVE(1) ************************
                        }

                        if (COMMOD9.NEGATF > 0)
                        {
                            frm.SetColor(6);
                            DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(INGSCOR), (double)(COMMOD9.NGHPEN), 0, 1);
                            frm.SetColor(15);
                        }

                        frm.SetColor(1);
                        switch (COMMOD9.SOLVEF)
                        {
                            case 4:
                            case 5:
                                //plot sqpen in dark blue, if sqpen is unchanged
                                //primary penalty is not worse
                                DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ISQSCOR), (double)(COMMOD9.SQPEN), 0, 1);
                                break;
                            case 6:
                            case 7:
                                //plot shpen in dark blue, if shpen is unchanged
                                //primary penalty is not worse
                                DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ISHSCOR), (double)(COMMOD9.SHPEN), 0, 1);
                                break;
                            case 8:
                            case 9:
                                //plot tspen in dark blue, if tspen is unchanged
                                //primary penalty is not worse
                                DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ITSSCOR), (double)(COMMOD9.TSPEN), 0, 1);
                                break;
                        }

                        frm.SetColor(15);


                        if (COMMOD9.CTRF == 1)
                        {
                            NCT = NCT + 1;
                            COMMOD9.CTRF = 0;
                        }

                        //update the penalties
                        COMMOD9.PEN = COMMOD9.NXTPEN;
                        COMMOD9.ASPNPEN = COMMOD9.SPANPEN;

                        Helper.DOTOO();

                        if (COMMOD9.PENF < 2)
                        {
                            Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
                        }

                        Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                        LAST = 1;

                        //if this is very best so far
                        if ((COMMOD9.PEN < COMMOD9.BSTPEN) || ((COMMOD9.PEN == COMMOD9.BSTPEN) &&
                            (((s89f) && (COMMOD9.TSPEN < COMMOD9.TSBST)) ||
                            ((s67f) && (COMMOD9.SHPEN < COMMOD9.SHBST)) ||
                            ((s45f) && (COMMOD9.SQPEN < COMMOD9.SQBST)))))
                        {
                            siz = 2;
                            if (COMMOD9.PEN == COMMOD9.BSTPEN) siz = 1;

                            if (COMMOD9.NEGATF > 0)
                            {
                                frm.SetColor(6);
                                DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(INGSCOR), (double)(COMMOD9.NGHPEN), 0, 1);
                                frm.SetColor(15);
                            }

                            //change color for secondary penalty
                            frm.SetColor(9);
                            switch (COMMOD9.SOLVEF)
                            {
                                case 4:
                                case 5:
                                    if ((COMMOD9.SQPEN < COMMOD9.SQBST) || (COMMOD9.PEN < COMMOD9.BSTPEN))
                                    {
                                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ISQSCOR), (double)(COMMOD9.SQPEN), 1, 1);
                                    }
                                    break;
                                case 6:
                                case 7:
                                    if ((COMMOD9.SHPEN < COMMOD9.SHBST) || (COMMOD9.PEN < COMMOD9.BSTPEN))
                                    {
                                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ISHSCOR), (double)(COMMOD9.SHPEN), 1, 1);
                                    }
                                    break;
                                case 8:
                                case 9:
                                    if ((COMMOD9.TSPEN < COMMOD9.TSBST) || (COMMOD9.PEN < COMMOD9.BSTPEN))
                                    {
                                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(ITSSCOR), (double)(COMMOD9.TSPEN), 1, 1);
                                    }
                                    break;
                            }

                            frm.SetColor(15);

                            COMMOD9.SQBST = COMMOD9.SQPEN;
                            COMMOD9.SHBST = COMMOD9.SHPEN;
                            COMMOD9.TSBST = COMMOD9.TSPEN;
                            COMMOD9.BSTPEN = COMMOD9.PEN;
                            COMMOD9.NBETR = COMMOD9.NBETR + 1;
                            COMMOD9.BS2PEN = COMMOD9.TOOPEN;
                            COMMOD9.NGTPEN = COMMOD9.NGHPEN;
                            BSTSCOR = (int)COMMOD9.BSTPEN;

                            if ((COMMOD9.KSM > 0.00) && (COMMOD9.PENF < 2)) BSTSCOR = BSTSCOR - (int)COMMOD9.SMPEN;

                            Helper.CopyArray(COMMOD9.PERM, ref COMMOD9.BSTPERM);

                            if (trjf)
                            {
                                //update trajectory on range graph
                                frm.SetColor(10);
                                DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)INISCOR, (double)BSTSCOR, siz, 1);

                                frm.SetColor(15);
                                //if smoothing, show smoothing penalty term too
                                if ((COMMOD9.KSM > 0.00) && (COMMOD9.PENF < 2))
                                {
                                    BSTSCOR = (int)(COMMOD9.SMPEN / COMMOD9.KSM);
                                    frm.SetColor(2);
                                    DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)ISMSCOR, (double)BSTSCOR, 0, 1);

                                    BSTSCOR = (int)(COMMOD9.BSTPEN - COMMOD9.SMPEN);
                                    frm.SetColor(15);
                                }
                            }
                        }


                        // END OF penalty updates for acceptance
                        // Now edit the range lines because switch was accepted
                        if (rngf)
                        {
                            EDITCHT(frm, g, IRANK, JRANK);
                            //plot uphill moves in contrasting color!!    
                            if (hilf)
                            {
                                //update trajectory on range graph
                                frm.SetColor(8);
                                DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT, (double)(INISCOR), COMMOD9.PEN, 1, 1);
                                frm.SetColor(15);
                            }
                        }


                        // END OF ACCEPTANCE ROUTINES
                        // update the relaxed fit curves   
                        if (rlxf)
                        {
                            FILLBEST(frm, g, INISCOR, COMMOD9.PEN);
                        }


                    }
                    else
                    {
                        //C-----------------------------------------------------------------------      
                        //C    LOOK FOR PROBABILISTIC GROUNDS TO ACCEPT
                        //C    EVEN IF THE NEW PERM IS WORSE THAN "PERM",
                        //C    ACCEPT IT WITH PROBABILITY "PROB"
                        //C    PROB replaced by CRTPEN - the critical penalty at PROB
                        //C  ////////////////////////////////////////////////////////////// 

                        //NOT AN IMPROVEMENT
                        if (COMMOD9.NUDGER)
                        {
                            COMMOD9.NUDGUP = false;
                            COMMOD9.NUDGDN = false;
                        }

                        //CPMS   reject and reset for greedy algorithm 
                        //cpms   and shrinker and squeezer and teaser
                        if (algf)
                        {
                            Helper.CopyArray(COMMOD9.LSTPERM, ref COMMOD9.PERM);

                            COMMOD9.TSPEN = COMMOD9.TSLST;

                            if (trjf)
                            {
                                //update trajectory on range graph
                                frm.SetColor(8);
                                DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT,
                                    (double)INISCOR, (double)(BSTSCOR), 0, 1);
                                frm.SetColor(15);
                            }


                        }
                        else
                        {
                            //cpms-----------for squeal and anneas and steal--------
                            //cpms	  adjust U downward if 2nd penalty improves
                            //cpms    and up if the 2nd penalty worsens
                            //cpms    adjust according to the new/old ratio squared
                            //cpms	   the adjustments are likely to be small 
                            //cpms    but they increase geometrically as the 
                            //cpms    impact on the secondary penalty increases
                            //cpms
                            //cpms    the efficiency of the promary search is compromised
                            //cpms    but the secondary penalty is not allowed to
                            //cpms    increase uncontrollably

                            switch (COMMOD9.SOLVEF)
                            {
                                case 1:
                                case 2:
                                case 3:
                                case 4:
                                case 6:
                                case 8:
                                    break;
                                case 5:
                                    if (COMMOD9.SQPEN > 0)
                                        U = U * Math.Pow((COMMOD9.SQPEN / COMMOD9.SQBST), 2);
                                    break;
                                case 7:
                                    if (COMMOD9.SHPEN > 0)
                                        U = U * Math.Pow((COMMOD9.SHPEN / COMMOD9.SHBST), 2);
                                    break;
                                case 9:
                                    if (COMMOD9.TSPEN > 0)
                                        U = U * Math.Pow((COMMOD9.TSPEN / COMMOD9.TSBST), 2);
                                    break;
                            }

                            //cpms-------------------------------------------------

                            if (COMMOD9.NXTPEN < COMMOD9.CRTPEN)
                            {
                                //CPMS    ACCEPT UPHILL STEP
                                //CPMS    and check contraction
                                if (COMMOD9.CTRF == 1)
                                {
                                    NCT = NCT + 1;
                                    COMMOD9.CTRF = 0;
                                }

                                if (COMMOD9.NEGATF > 0)
                                {
                                    frm.SetColor(6);
                                    DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT,
                                        (double)INGSCOR, (double)COMMOD9.NGHPEN, 0, 1);
                                    frm.SetColor(15);
                                }

                                frm.SetColor(9);

                                switch (COMMOD9.SOLVEF)
                                {
                                    case 1:
                                    case 2:
                                    case 3:
                                    case 4:
                                    case 6:
                                    case 8:
                                        break;
                                    case 5:
                                        COMMOD9.SQBST = COMMOD9.SQPEN;
                                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT,
                                        (double)ISQSCOR, (double)COMMOD9.SQPEN, 0, 1);
                                        break;
                                    case 7:
                                        COMMOD9.SHBST = COMMOD9.SHPEN;
                                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT,
                                        (double)ISHSCOR, (double)COMMOD9.SHPEN, 0, 1);
                                        break;
                                    case 9:
                                        COMMOD9.TSBST = COMMOD9.TSPEN;
                                        DRAWSCOR(frm, g, COMMOD9.NOUTER * COMMOD9.NINNER, PCT,
                                        (double)ITSSCOR, (double)COMMOD9.TSPEN, 0, 1);
                                        break;
                                }

                                frm.SetColor(15);

                                //update total penalty and secondary penalty
                                COMMOD9.PEN = COMMOD9.NXTPEN;
                                COMMOD9.ASPNPEN = COMMOD9.SPANPEN;

                                Helper.DOTOO();

                                if (COMMOD9.PENF < 2)
                                {
                                    Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
                                }

                                Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                                LAST = 1;

                                //Edit the range lines because bad switch is accepted
                                if (rngf)
                                {
                                    EDITCHT(frm, g, IRANK, JRANK);
                                }

                                //CPMS	 Load the EVERYBST array and add points to 
                                //CPMS	 the event bathtub - needs to be a separate subroutine
                                if (rlxf)
                                {
                                    FILLBEST(frm, g, INISCOR, COMMOD9.PEN);
                                    //run through the solution in sequence (EVERYBST cols)
                                    //and enter the penalty for the appropriate
                                    //event (EVERYBST rows), IF the best so far
                                    //i.e. EVERYBST has IROWS order
                                }

                            }
                            else
                            {
                                //C-----------------------------------------------------------------------      
                                //CPMS     REJECT UPHILL STEP reset PERM to previous order (any NABRGEN!)
                                //CPMS	   no boxes plotted on the run-time graphic
                                //CPMS     no update to PEN or TOOPEN
                                //CPMS--------------------------------------------------------
                                Helper.CopyArray(COMMOD9.LSTPERM, ref COMMOD9.PERM);
                                COMMOD9.TSPEN = COMMOD9.TSLST;
                                COMMOD9.CTRF = 0;
                            }
                        }
                    }//END if ELSE DELTA>0

                    //cpms  if tempering:  
                    if (COMMOD9.SOLVEF == 3) COMMOD9.T = COMMOD9.T * COMMOD9.R;

                    System.Windows.Forms.Application.DoEvents();
                    System.Threading.Thread.Sleep(0);

                }//CPMS*****end of inner loop**************

                //C  lower the temperature and do it again if annealing
                //C  heat to random temperature and quench again if tempering (solvef=3)

                if (COMMOD9.SOLVEF == 3)
                {
                    U = COMMOD9.RANDOM.NextDouble();

                    //c	 PCT/NINNER*NOUTER runs from 1.0 to 0.0
                    //c      IF random number is larger, try again
                    //c      reduces frequency of hot tempers later in run
                    COMMOD9.T = COMMOD9.STARTT * PCT / (COMMOD9.NINNER * COMMOD9.NOUTER) * U;

                    if (COMMOD9.PENF < 2)
                    {
                        Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.PEN);
                    }
                    else if ((COMMOD9.PENF == 5) || (COMMOD9.PENF == 6))
                    {
                        Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.PEN);
                    }
                    else if (COMMOD9.PENF > 1)
                    {
                        Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.PEN);
                    }

                    Helper.CopyArray(COMMOD9.BSTPERM, ref COMMOD9.PERM);
                    Helper.CopyArray(COMMOD9.HSCTSOL, ref COMMOD9.SCTSOL);
                    Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                    //refresh the range lines
                    if (rngf) DRAWCHT(frm, g, 2, COMMOD9.BSTPERM);
                }
                else
                {
                    if (COMMOD9.AUTF == 0) COMMOD9.T = COMMOD9.T * COMMOD9.R;

                    if ((COMMOD9.AUTF == 0) && (COMMOD9.CDF != 1)) Helper.TRAJOUT();
                }



            }//cpms**end of outer loop*********************************************


            COMMOD9.AUTPEN = COMMOD9.BSTPEN;
            COMMOD9.NUDGUP = false;
            COMMOD9.NUDGDN = false;

            //CPMS  Now turn to tasks completed only once per run, AFTER annealing
            //cpms-----------------
            //CPMS	restore FIXF and NABRGEN
            COMMOD9.FIXF = FIXM;
            COMMOD9.NABRGEN = NABRM;

            //CPMS-----------------------------------------------------------
            //CPMS     write out last solution tried if requested	 
            if ((COMMOD9.STEPSOL.Substring(0, 3) == "LST") ||
                (COMMOD9.STEPSOL.Substring(0, 3) == "lst"))
            {
                if (COMMOD9.CDF != 1) Helper.STEPOUT(COMMOD9.LSTPERM);

                if (COMMOD9.CDF != 1) Helper.TRAJOUT();

            }
            //C-------------------------------------------------------------- 
            //C     save value of PENF
            LAST = COMMOD9.PENF;

            //CPMS  Try to prevent the remaining tasks from
            //c     running during auto-cooling until about to stop
            if ((COMMOD9.PAUSF == 5) && (COMMOD9.NTRIES < COMMOD9.NSTOP - 1))
            {
                COMMOD9.NUDGUP = false;
                COMMOD9.NUDGDN = false;

                goto Label33;
            }

            //C     after all NOUTER*NINNER passes through the loop, 
            //CPMS  make sure that HSCTSOL reflects the best solution
            //CPMS  rather than the last tried
            //CPMS  and that HSCTSOL is last set according to correct penalty
            //CPMS  i.e. lvlpen if PENF=1, intpen for all other cases

            if (COMMOD9.PENF == 1)
            {
                COMMOD9.PENF = -1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.RVLPEN);
                if (LAST == -1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);
                if (LAST == 0) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.LVLPEN);
                if (LAST == 1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            }
            else if (COMMOD9.PENF == 0)
            {
                COMMOD9.PENF = -1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.RVLPEN);
                if (LAST == -1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.LVLPEN);
                if (LAST == 1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);
                if (LAST == 0) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            }
            else if (COMMOD9.PENF == -1)
            {
                COMMOD9.PENF = 1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.LVLPEN);
                if (LAST == 1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);
                if (LAST == 0) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

                COMMOD9.PENF = -1;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.RVLPEN);
                if (LAST == -1) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            }

            COMMOD9.NGTPEN = COMMOD9.NGHPEN;
            //CPMS  GETPEN places local horizons to fit the best sequence
            //CPMS  but the final penalty needs to be returned to its 
            //CPMS  "democratic" value for an ordinal search

            COMMOD9.PENF = 6;
            if (COMMOD9.FB4LF != 0) Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.SEQPEN);

            COMMOD9.PENF = 5;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.ROYPEN);

            COMMOD9.PENF = 7;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.MOMPEN);
            if (LAST == 7) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            COMMOD9.PENF = 4;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.RSCPEN);
            if (LAST == 4) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            COMMOD9.PENF = 3;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.SPTPEN);
            if (LAST == 3) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            COMMOD9.PENF = 2;
            Helper.DEMPEN(COMMOD9.BSTPERM, ref COMMOD9.ORDPEN);
            if (LAST == 2) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            if (LAST == 5) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);
            if ((LAST == 6) && (COMMOD9.FB4LF != 0)) Helper.CopyArray(COMMOD9.COLPEN, ref COMMOD9.SCJPEN);

            //C   SCJPEN now contains the primary section penalties
            //C   But this is pointless for Royal (PENF=5) 
            //C   and for Sequel (PENF=6) so
            //C   the ordinal penalty is used instead

            if (LAST > 2)
            {
                COMMOD9.PENF = 0;
                Helper.GETPEN(COMMOD9.BSTPERM, ref COMMOD9.INTPEN);

                //C   COLPEN now contains the ordinal or interval penalties
                //C   whichever contrasts with the primary
            }

            //c  restore PENF 
            COMMOD9.PENF = LAST;
            switch (COMMOD9.PENF)
            {
                case -1://eventual
                    COMMOD9.BSTPEN = COMMOD9.RVLPEN;
                    break;
                case 0://interval
                    COMMOD9.BSTPEN = COMMOD9.INTPEN;
                    break;
                case 1://level
                    COMMOD9.BSTPEN = COMMOD9.LVLPEN;
                    break;
                case 2://ordinal
                    COMMOD9.BSTPEN = COMMOD9.ORDPEN;
                    break;
                case 3://spatial
                    COMMOD9.BSTPEN = COMMOD9.SPTPEN;
                    break;
                case 4://rascal/rascer
                    COMMOD9.BSTPEN = COMMOD9.RSCPEN;
                    break;
                case 5://royal (Coex)
                    COMMOD9.BSTPEN = COMMOD9.ROYPEN;
                    break;
                case 6://sequel (Fb4L)
                    if (COMMOD9.FB4LF != 0) COMMOD9.BSTPEN = COMMOD9.SEQPEN;
                    break;
                case 7://momental
                    COMMOD9.BSTPEN = COMMOD9.MOMPEN;
                    break;
            }

        Label33:

            if (((COMMOD9.RUNGRF == 2) || (COMMOD9.RUNGRF >= 4)) && (COMMOD9.TRJGRF > 0))
            {
                frm.SetColor(10);
                BSTSCOR = (int)COMMOD9.BSTPEN;
                SCORE = string.Format("{0}", BSTSCOR);
                LBLSCOR(frm, g, SCORE, 1000 * (INISCOR - BSTSCOR) / INISCOR);
                frm.SetColor(15);
            }

            //CPMS-------------------------------------------------------------
            //CPMS  transfer the number of contractions to a common variable
            COMMOD9.CTRF = NCTR;

        }

        //cpms  fills the EVERYBST array for BESTFILE
        protected void FILLBEST(frmCONOP frm, Graphics g, int ISCOR, double RSCOR)
        {
            int X, Y;

            //cpms   - the penalty for the current solution
            //cpms   - goes into the BESTFILE
            //CPMS*****************************************************
            //CPMS	  run through the solution in sequence (EVERYBST cols)
            //CPMS	  and enter the penalty for the appropriate
            //CPMS	  event (EVERYBST rows), IF the best so far
            //CPMS	  i.e. EVERYBST has IROWS order

            COMMOD COMMOD9 = COMMOD.Singleton();

            for (X = 0; X < COMMOD9.NEVNT; X++)
            {
                COMMOD9.EVERYBST[COMMOD9.PERM[X], X] =
                    Math.Min(COMMOD9.EVERYBST[COMMOD9.PERM[X], X], RSCOR);

                //cpms   update secondary penalty if necessary
                //cpms   SECNDBST stores secondary penalty BEFORE scaling: TO2PEN
                if (COMMOD9.GRID2F == 1)
                {
                    COMMOD9.SECNDBST[COMMOD9.PERM[X], X] =
                        Math.Min(COMMOD9.SECNDBST[COMMOD9.PERM[X], X], COMMOD9.TO2PEN);
                }

                // plot on screen if selected event          
                if ((ISCOR > 0) && (COMMOD9.PERM[X] == COMMOD9.XEVNT) && (COMMOD9.RUNGRF == 4))
                {
                    frm.SetColor(10);
                    DRAWSCOR(frm, g, (double)(COMMOD9.NEVNT), (double)(COMMOD9.NEVNT - (X + 1)),
                        (double)ISCOR, RSCOR, 1, 2);
                    frm.SetColor(15);
                }

            }

            if (COMMOD9.VERYBEST == COMMOD9.NXTPEN)
            {
                for (X = 0; X < COMMOD9.NEVNT; X++)
                {
                    for (Y = 0; Y < COMMOD9.NSCT; Y++)
                    {
                        COMMOD9.HSCTRNG[X, Y, 0] =
                            Math.Max(COMMOD9.HSCTRNG[X, Y, 0], COMMOD9.HSCTSOL[X, Y]);

                        COMMOD9.HSCTRNG[X, Y, 1] =
                           Math.Min(COMMOD9.HSCTRNG[X, Y, 1], COMMOD9.HSCTSOL[X, Y]);
                    }
                }
            }
            else if (COMMOD9.VERYBEST > COMMOD9.PEN)
            {
                for (X = 0; X < COMMOD9.NEVNT; X++)
                {
                    for (Y = 0; Y < COMMOD9.NSCT; Y++)
                    {
                        COMMOD9.HSCTRNG[X, Y, 0] = COMMOD9.HSCTSOL[X, Y];
                        COMMOD9.HSCTRNG[X, Y, 1] = COMMOD9.HSCTSOL[X, Y];
                    }
                }

                COMMOD9.VERYBEST = Helper.MinVal(COMMOD9.EVERYBST);

            }

        }

        private void CLIP(frmCONOP frm, Graphics g, int rnki, int rnkj, int rnkt)
        {
            double xrl, yrl;
            //CPMS---------------------------------------------------
            //cpms  rnki -  the former x position
            //cpms  rnkj -  the key to the event number and yrow position
            //cpms  rnkt -  the new x position    

            COMMOD COMMOD9 = COMMOD.Singleton();

            xrl = 0.0;
            yrl = 0.0;
            xrl = (double)(rnki + 1) * COMMOD9.XSPAN / (double)COMMOD9.NEVNT;
            yrl = (double)((COMMOD9.IROWS[COMMOD9.LSTPERM[rnkj], 0] + 1) * COMMOD9.YSPAN / COMMOD9.YROWS);

            //int scl = 0;
            int top = 0;
            //int btm = 50;
            int lft = 100;
            //int rit = 0;

            //move to left end (i) of modification
            //line/blank up to right end (j)
            int x1 = lft + (int)xrl;
            xrl = (double)(rnkt + 1) * COMMOD9.XSPAN / (double)COMMOD9.NEVNT;
            int x2 = lft + (int)xrl;
            g.DrawLine(frm.DW_Pen, x1, top + (int)yrl, x2, top + (int)(yrl));

        }

        private void NEWDASH(frmCONOP frm, Graphics g, int rnki, int rnkj, int rnkt, int cl1, int cl2)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            double xrl, yrl;
            //CPMS---------------------------------------------------
            //CPMS  Used for MID events that always lie in a white range!
            //CPMS  other unpaired events must be BLACKED out
            //CPMS  rnki - the old dash position in range
            //CPMS  rnkj - the key to event (row) number
            //CPMS  rnkt - and the new dash position
            xrl = 0.0;
            yrl = 0.0;
            //cpms  fix the row to modify
            yrl = (double)((COMMOD9.IROWS[COMMOD9.LSTPERM[rnkj], 0] + 1) * COMMOD9.YSPAN / COMMOD9.YROWS);
            //white-out/blackout the old dash
            xrl = (double)(rnki + 1) * COMMOD9.XSPAN / (double)COMMOD9.NEVNT;

            frm.SetColor(cl1);
            DASH(frm, g, xrl, yrl);

            //draw the new red dash
            xrl = (double)(rnkt + 1) * COMMOD9.XSPAN / (double)COMMOD9.NEVNT;
            frm.SetColor(cl2);
            DASH(frm, g, xrl, yrl);

            frm.SetColor(15);

        }

        private void DASH(frmCONOP frm, Graphics g, double xrl, double yrl)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            int scl = 0;
            int top = 0;
            int btm = 50;
            int lft = 100;
            int rit = 0;

            g.DrawLine(frm.DW_Pen, lft + (int)xrl - 1, top + (int)yrl, lft + (int)(xrl) + 1, top + (int)(yrl));

        }

        private void EDITCHT(frmCONOP frm, Graphics g, int ranki, int rankj)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            int rankt;
            int typei, typej, typet;
            // limits of screen resolution:   maxx  maxy
            //   margins outside graph box:   top  btm  lft  rit

            int scl = 0;
            int top = 0;
            int btm = 50;
            int lft = 100;
            int rit = 0;

            //CPMS---------------------------------------------------------------------
            //CPMS  Edit the range lines
            typei = COMMOD9.IROWS[COMMOD9.LSTPERM[ranki], 1];
            typej = COMMOD9.IROWS[COMMOD9.LSTPERM[rankj], 1];

            //CPMS---------------------------------------------------------------------
            //CPMS  CASE 1  ranki moves up to j; intermediates move down 1 rank
            //CPMS---------------------------------------------------------------------
            if (ranki < rankj)
            {
                //edit ranki to  rankj regardless of neighborhood size
                if ((typei == 1) || (typei == 2) || (typei == 11) || (typei == 12))
                {
                    //ranki is a paired range end;  edit bar       
                    //if ranki is a FAD or MAX; range base must be blanked up to rankj             
                    //if ranki is a last; range top must be extended in white/gold
                    frm.SetColor(0);
                    if (typei == 2) frm.SetColor(15);
                    if (typei == 12) frm.SetColor(14);

                    CLIP(frm, g, ranki, ranki, rankj);

                }
                else if ((typei > 3) || (typei < 1))
                {
                    //ranki is an unpaired event;  move dash up 
                    NEWDASH(frm, g, ranki, ranki, rankj, 0, 14);
                }
                else if (typei == 3)
                {
                    NEWDASH(frm, g, ranki, ranki, rankj, 15, 12);
                }

                //CPMS   if ranks j and i switch  
                if (COMMOD9.NABRGEN > 1)
                {
                    //edit rankj to ranki
                    if ((typej == 1) || (typej == 2) || (typej == 11) || (typej == 12))
                    {
                        //rankj is a paired range end;  edit bar       
                        //if rankj is a first: extend base of range bar 
                        //if ranki is a last; blank out range end   

                        frm.SetColor(15);

                        if (typej == 11) frm.SetColor(14);
                        if (typej == 2) frm.SetColor(0);
                        if (typej == 12) frm.SetColor(0);

                        CLIP(frm, g, ranki, rankj, rankj);
                    }
                    else if ((typej > 3) || (typej < 1))
                    {
                        //rankj is an unpaired event;  move dash down 
                        NEWDASH(frm, g, rankj, rankj, ranki, 0, 14);
                    }
                    else if (typej == 3)
                    {
                        NEWDASH(frm, g, rankj, rankj, ranki, 15, 12);
                    }
                }


                //CPMS   if making big single moves       
                if (COMMOD9.NABRGEN == 1)
                {
                    rankt = ranki + 1;

                    while (rankt <= rankj)
                    {
                        typet = COMMOD9.IROWS[COMMOD9.LSTPERM[rankt], 1];

                        //move all intermediate events down 1 rank 
                        if ((typet == 1) || (typet == 2) || (typet == 11) || (typet == 12))
                        {
                            //if rankt is a first; extend the range bar down 1 rank       
                            //if rankt is a last; blank range bar down one rank from top  
                            frm.SetColor(15);
                            if (typet == 11) frm.SetColor(14);
                            if (typet == 2) frm.SetColor(0);
                            if (typet == 12) frm.SetColor(0);
                            CLIP(frm, g, rankt - 1, rankt, rankt);
                        }
                        else if ((typet > 3) || (typet < 1))
                        {
                            //rankt is an unpaired event, move dash down 1
                            NEWDASH(frm, g, rankt, rankt, rankt - 1, 0, 14);
                        }
                        else if (typet == 3)
                        {
                            NEWDASH(frm, g, rankt, rankt, rankt - 1, 15, 12);
                        }

                        rankt++;
                    }
                }
            }
            else if (ranki > rankj)
            {
                //CPMS----------------------------------------------------------------------       
                //CPMS   CASE 2  ranki moves down to j and intermediate ranks move up 1 rank   
                //CPMS----------------------------------------------------------------------

                //edit ranki to rankj regardless of neighborhood size   
                if ((typei == 1) || (typei == 2) || (typei == 11) || (typei == 12))
                {
                    //ranki is a paired range end;  edit bar       
                    //if ranki is a first; extend base of range bar 
                    //if ranki is a last; blank out range end       
                    frm.SetColor(15);
                    if (typei == 11) frm.SetColor(14);
                    if (typei == 2) frm.SetColor(0);
                    if (typei == 12) frm.SetColor(0);

                    CLIP(frm, g, rankj, ranki, ranki);
                }
                else if ((typei > 3) || (typei < 1))
                {
                    //ranki is an unpaired event;  move dash down
                    NEWDASH(frm, g, ranki, ranki, rankj, 0, 14);
                }
                else if (typei == 3)
                {
                    NEWDASH(frm, g, ranki, ranki, rankj, 15, 15);
                }

                //CPMS   if ranks j and i switch:     
                if (COMMOD9.NABRGEN > 1)
                {
                    //edit rankj to ranki if making double switch
                    if ((typej == 1) || (typej == 2) || (typej == 11) || (typej == 12))
                    {
                        //rankj is a paired range end;  edit bar       
                        //if rankj is a first; range base must be blanked up to ranki
                        //if rankj is a last; range top must be extended
                        frm.SetColor(0);
                        if (typej == 2) frm.SetColor(15);
                        if (typej == 12) frm.SetColor(14);

                        //move to left end (j) of modification
                        CLIP(frm, g, rankj, rankj, ranki);
                    }
                    else if ((typej > 3) || (typej < 1))
                    {
                        //rankj is an unpaired event;  move dash up 
                        NEWDASH(frm, g, rankj, rankj, ranki, 0, 14);
                    }
                    else if (typej == 3)
                    {
                        NEWDASH(frm, g, rankj, rankj, ranki, 15, 12);
                    }
                }

                //if making big single moves (BIGNABR)
                if (COMMOD9.NABRGEN == 1)
                {
                    rankt = ranki - 1;
                    while (rankt >= rankj)
                    {
                        typet = COMMOD9.IROWS[COMMOD9.LSTPERM[rankt], 1];

                        //intermediate ranks move up 1 rank  
                        if ((typet == 1) || (typet == 2) || (typet == 11) || (typet == 12))
                        {
                            //if rankt is a first; blank out range start  
                            //if rankt is a last; extend end of range bar 
                            frm.SetColor(0);
                            if (typet == 2) frm.SetColor(15);
                            if (typet == 12) frm.SetColor(14);

                            //move to left end (t) of modification
                            CLIP(frm, g, rankt, rankt, rankt + 1);
                        }
                        else if ((typet > 3) || (typet < 1))
                        {
                            //rankt is an unpaired event, move dash up 1
                            NEWDASH(frm, g, rankt, rankt, rankt + 1, 0, 14);
                        }
                        else if (typet == 3)
                        {
                            NEWDASH(frm, g, rankt, rankt, rankt + 1, 15, 12);
                        }

                        rankt--;
                    }
                }
            }

            //CPMS--------------------------------
            //CPMS  restore default graphics mode

            frm.SetColor(15);

        }

        private void DRAWTEMP(frmCONOP frm, Graphics g)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            string cnt;
            string cool;

            frm.SetColor(15);

            cool = string.Format("{0:0.000000000}", COMMOD9.T);
            cnt = string.Format("{0}", COMMOD9.NTRIES);
            cool = "temp: " + cool.Trim() + "  [null: " + cnt.Trim() + "]";
            cnt = string.Format("{0:0.00000}", COMMOD9.Rx);
            cool = cool.Trim() + "  {ratio: " + cnt.Trim() + "}";
            g.DrawString(cool.Trim(), frm.MSG_FONT_S, frm.DW_Brush, (int)(COMMOD9.maxx / 1.5), (int)(COMMOD9.maxy - 12));

        }

    }
}
