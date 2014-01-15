using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CONOP.NET
{
    public class DOCOEX
    {
        #region Singleton

        private static DOCOEX g_singleton = null;
        private DOCOEX() { }
        public static DOCOEX Singleton()
        {
            if (g_singleton == null)
                g_singleton = new DOCOEX();

            return g_singleton;
        }

        #endregion

        public void RunIt(int flg)
        {

            int IROW, ISP, MROW, MSP;
            int IAROW, IBROW, MAROW, MBROW;
            int AIJ, BIJ, AMJ, BMJ, AIK, BMK;
            int I, J, K, M, IJ, IK, MJ, MK;

            //C*********************************************************
            //cpms    IAROW, MAROW: IROWS row for FAD of species I and M
            //cpms    IBROW, MBROW: IROWS row for LAD of species I and M
            //cpms    IROW, MROW:   IROWS row for other events I or M
            //c
            //cpms    flg holds COXSTF       
            //c 
            //C---------------------------------------------------------
            IROW = 0;
            IAROW = 0;
            IBROW = 0;
            MROW = 0;
            MAROW = 0;
            MBROW = 0;
            ISP = 0;
            MSP = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();
            //ZEROIZE THE COEXST MATRIX
            Helper.SetVal(COMMOD9.COEXST, 0);

            if (COMMOD9.NSPC == 0) goto Label9999;

            //coexst loaded from file!
            if (flg == 1) goto Label7400;

            //set the row=col diagonal to one
            //i.e. every taxon coexists with itself
            for (I = 0; I < COMMOD9.NSPC; I++)
            {
                COMMOD9.COEXST[I, I] = 3;
            }

            if (flg == 0) goto Label7350;

            //CPMS-------------------------------------------------------------------
            //CPMS    CALCULATE THE COEXISTENCE MATRIX.
            //CPMS       IF TWO SPECIES COEXIST
            //C          IN ANY SECTION, THEY ARE ASSUMED TO COEXIST AND THE
            //C          APPROPRIATE ENTRIES IN THE COEXST MATRIX ARE SET TO
            //C          2 or 3 IF THEY NEVER COEXIST. THE ENTRIES REMAIN 0,
            //C          WHICH WAS SET IN "ZEROIZE".
            //C
            //C    THE COEXST MATRIX IS USED BY THE PERMUTATION GENERATION
            //C       ROUTINE TO TEST THE FEASIBILITY OF CANDIDATE
            //C          PERMUTATIONS.
            //C
            //C   THERE ARE TWO MAIN ROUTINES
            //C      1. SEE IF COEXISTENCES ARE OBSERVED WITHIN ANY ONE SECTION
            //C      2. SEE IF SPECIES I LIES ENTIRELY ABOVE SPECIES M IN ONE
            //C         SECTION AND ENTIRELY BELOW IT IN ANOTHER
            //C----------------------------------------------------------------------
            //C   1. LOOK FOR COEXISTENCES WITHIN EACH SECTION
            //C   1.1  FIRST, find the two tops and two bottoms
            //C----------------------------------------------------------------------

            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    //cpms        need all event numbers in order to stop at all type 1
                    //cpms        smaller loop number requires very slow "FINDROW"
                    AIJ = -1;//<=HXD
                    BIJ = -1;//<=HXD
                    //CPMS        need to prepare for the possibility that the event
                    //CPMS        is not a range end
                    //CPMS        in that case, FINDROW() will not find type 1 or 2
                    //CPMS        FINDROWS will then give IROW= -1
                    //CPMS        and the program CYCLE's back to the top of the DO loop
                    //CPMS        the length of the loop includes all event numbers
                    if ((COMMOD9.IROWS[I, 1] != 1) && (COMMOD9.IROWS[I, 1] != 3)) continue;

                    if (COMMOD9.IROWS[I, 1] == 1)
                    {
                        IAROW = I;
                        //go in here only if event type is 1 -- a FAD!	
                        //moves.eq.3 means that end is unreliable
                        if (COMMOD9.ISTATIC[IAROW, J, 1] == 3) continue;
                        //at least one range end looks reliable; but
                        //check that events are not zeroed out in this section
                        ISP = COMMOD9.IROWS[IAROW, 2];
                        //find LAD
                        IBROW = COMMOD9.IROWS[IAROW, 3];
                        //if LAD is unreliable -- get out
                        if (COMMOD9.ISTATIC[IBROW, J, 1] == 3) continue;
                        if (COMMOD9.ISTATIC[IAROW, J, 1] == 1)
                        {
                            //FAD and LAD are both reliable  
                            if (!(Helper.RZERO(IAROW, J))) AIJ = COMMOD9.ISTATIC[IAROW, J, 0];
                            if (!(Helper.RZERO(IBROW, J))) BIJ = COMMOD9.ISTATIC[IBROW, J, 0];

                            //---------------------------------------------- 
                            //check later for AIJ or BIJ equals 0
                            //---------------------------------------------- 
                        }
                    }
                    else if (COMMOD9.IROWS[I, 1] == 3)
                    {
                        //CPMS    check for unpaired range end; type=3
                        //CPMS    this should not survive DOINPUT
                        //CPMS    keep in case paired and unpaired events for same taxon
                        IROW = I;
                        if (COMMOD9.ISTATIC[IROW, J, 1] == 1)
                        {
                            if (!(Helper.RZERO(IROW, J))) AIJ = COMMOD9.ISTATIC[IROW, J, 0];
                        }
                        else if (COMMOD9.ISTATIC[IROW, J, 1] == 2)
                        {
                            if (!(Helper.RZERO(IROW, J))) BIJ = COMMOD9.ISTATIC[IROW, J, 0];
                        }
                    }


                    //CPMS-------------------------------------------------------------------
                    //CPMS    if taxon has an FAD above its LAD, warn and 
                    //CPMS    set the flag to terminate after all warnings are listed 
                    if ((BIJ >= 0) && (AIJ > BIJ))//HACK: MAY BE 
                    {
                        COMMOD9.STOPF = 1;
                        //TODO:DOCOEX.CS
                        //               WRITE(*,900) IROWS(I,1), J, AIJ, BIJ
                        //900            FORMAT(1X,'taxon ',I5,' in section ',I5,
                        //   1           '  has FAD level',I3,' above LAD level',I3)
                    }

                    for (M = 0; M < COMMOD9.NEVNT; M++)
                    {
                        AMJ = -1;//<=HXD
                        BMJ = -1;//<=HXD
                        if ((COMMOD9.IROWS[M, 1] != 1) && (COMMOD9.IROWS[M, 1] != 3)) continue;
                        if (COMMOD9.IROWS[M, 1] == 1)
                        {
                            MAROW = M;

                            //CPMS  moves.eq.3 means that both ends are unreliable
                            if (COMMOD9.ISTATIC[MAROW, J, 1] == 3) continue;
                            //CPMS  at least one range end looks reliable; but
                            //CPMS  check that event is not zeroed out in this section
                            MSP = COMMOD9.IROWS[MAROW, 2];
                            //cpms	find LAD
                            MBROW = COMMOD9.IROWS[MAROW, 3];
                            //cpms  if LAD is unreliable -- get out
                            if (COMMOD9.ISTATIC[MBROW, J, 1] == 3) continue;
                            if (COMMOD9.ISTATIC[MAROW, J, 1] == 1)
                            {
                                //CPMS  FAD and LAD are both reliable
                                if (!(Helper.RZERO(MAROW, J))) AMJ = COMMOD9.ISTATIC[MAROW, J, 0];
                                if (!(Helper.RZERO(MBROW, J))) BMJ = COMMOD9.ISTATIC[MBROW, J, 0];
                                //cpms  ---------------------------------------------- 
                                //cpms  check later for AMJ or BMJ equals 0
                                //cpms  ---------------------------------------------- 
                            }
                        }
                        else if (COMMOD9.IROWS[M, 1] == 3)
                        {
                            //CPMS  check for unpaired range end; type=3
                            //CPMS  this should not survive DOINPUT
                            //CPMS  keep in case paired and unpaired events for same taxon
                            MROW = M;
                            if (COMMOD9.ISTATIC[MROW, J, 1] == 1)
                            {
                                if (!(Helper.RZERO(MROW, J))) AMJ = COMMOD9.ISTATIC[MROW, J, 0];
                            }
                            else if (COMMOD9.ISTATIC[MROW, J, 1] == 2)
                            {
                                if (!(Helper.RZERO(MROW, J))) BMJ = COMMOD9.ISTATIC[MROW, J, 0];
                            }
                        }

                        //C----------------------------------------------------------------------
                        //C   Now do the coexistence checking
                        //C----------------------------------
                        //C   1.1A. SEE IF A FIRST FOR SPECIES I WAS FOUND IN SECTION J
                        //CPMS    If it is not found the VALEVEL is 0.0
                        //CPMS    This routine was moved from VALEVEL to LEVEL 
                        //CPMS    to solve problems caused by negative VALEVEL's
                        //CPMS    even though this should be fixed when base levels
                        //CPMS    were standardized; LEVEL is more efficient than VALEVEL here
                        //CPMS    If it is not-found, or is zeroed out, the LEVEL is 0
                        //C----------------------------------------------------------------------
                        if (AIJ >= 0)//<=HXD
                        {
                            //C----------------------------------------------------------------------
                            //C     1.1A1. IF IT WAS, AND THE FIRSTS FOR THE TWO SPECIES ARE
                            //C         FOUND AT THE SAME LEVEL, THEN THEY COEXIST
                            //CPMS      if the firsts are at the same level, the rest of the ranges
                            //CPMS      of the two species must overlap, unless one or both of the 
                            //CPMS      ranges is based on a single find at that level. Then the 
                            //CPMS      coexistence is accepted only if COXSTF (now flg)=2.
                            //CPMS-------------------------------------------------------------------
                            //CPMS      Instead of COXSTF(now flg).EQ.2, use COXSTF(now flg).GT.1
                            //CPMS
                            //CPMS      Then we can simply add a large constant to COXSTF
                            //CPMS      to ensure Sensu Lato treatment, and then subtract it
                            //CPMS      to ensure return to original value	-- this approach replaced by 
                            //CPMS      using flg argument; it can be COXSTF or not.  i.e. can send 
                            //CPMS      another value without changing the COXSTF setting
                            //C----------------------------------------------------------------------

                            if (AIJ == AMJ)
                            {
                                COMMOD9.COEXST[ISP, MSP] = Math.Max(COMMOD9.COEXST[ISP, MSP], 2);
                                COMMOD9.COEXST[MSP, ISP] = Math.Max(COMMOD9.COEXST[MSP, ISP], 2);

                                if ((BIJ >= 0) && (BMJ >= 0) && (AIJ != BIJ) && (AMJ != BMJ))//<=HXD
                                {
                                    COMMOD9.COEXST[ISP, MSP] = 3;
                                    COMMOD9.COEXST[MSP, ISP] = 3;
                                }

                                //C----------------------------------------------------------------------
                                //C      1.1A2. SIMILARLY, IF THE FIRST OF SPECIES I WAS FOUND AT
                                //C          THE SAME LEVEL AS THE LAST OF SPECIES M, THEY COEXIST
                                //CPMS      if we believe that levels may condense time,
                                //CPMS      then we should eliminate this evidence of coexistence!!
                                //CPMS      set FORCECOEX to sensu stricto in CONOP.CFG 
                                //CPMS      COXSTF=4 no coexistences recognized
                                //CPMS      COXSTF=3 strict coexistence evidence - overlap required
                                //CPMS               i.e. some coexistences recognized
                                //CPMS      COXSTF=2 loose coexistence evidence
                                //CPMS               i.e. all coexistences recognized
                                //CPMS               = necessary condition for GETSTART()
                                //cpms      COXSTF=1 load from file
                                //cpms      COXSTF=0 all taxa coexist
                                //C----------------------------------------------------------------------
                            }
                            else if (AIJ == BMJ)
                            {
                                COMMOD9.COEXST[ISP, MSP] = Math.Max(COMMOD9.COEXST[ISP, MSP], 2);
                                COMMOD9.COEXST[MSP, ISP] = Math.Max(COMMOD9.COEXST[MSP, ISP], 2);
                                //C----------------------------------------------------------------------
                                //C   1.1B. NEXT, IF SPECIES I HAS BOTH A FIRST AND A LAST IN SECTION J,
                                //C      AND EITHER A FIRST OR A LAST OF SPECIES M IS BETWEEN
                                //C          THEM, SPECIES I AND M COEXIST
                                //C	 NOTE: lots of checks for zero because this can mean found
                                //C            but zeroed out by weight
                                //C----------------------------------------------------------------------
                            }
                            else if ((AMJ >= 0) && (BIJ >= 0) && (AIJ < AMJ) && (AMJ < BIJ))//<=HXD
                            {
                                COMMOD9.COEXST[ISP, MSP] = 3;
                                COMMOD9.COEXST[MSP, ISP] = 3;
                            }
                            else if ((BMJ >= 0) && (BIJ >= 0) && (AIJ < BMJ) && (BMJ < BIJ))//<=HXD
                            {
                                COMMOD9.COEXST[ISP, MSP] = 3;
                                COMMOD9.COEXST[MSP, ISP] = 3;
                            }
                        }

                        //C----------------------------------------------------------------------
                        //C   1.1D. NEXT WE COVER THE CASE WHERE SPECIES I HAS A LAST IN SECTION J
                        //C----------------------------------------------------------------------

                        if (BIJ >= 0)//<=HXD
                        {
                            //C----------------------------------------------------------------------
                            //C     1.1D1. IF IT DOES AND THE LAST OF SPECIES M IS AT THE SAME LEVEL,
                            //C         THEY COEXIST                        
                            //CPMS      if we admit that a single level may condense time  in
                            //CPMS      the strict rule of coexistence  COXSTF=3, however,
                            //CPMS      then we should eliminate this evidence of coexistence!
                            //C----------------------------------------------------------------------
                            if (BIJ == BMJ)
                            {
                                COMMOD9.COEXST[ISP, MSP] = Math.Max(COMMOD9.COEXST[ISP, MSP], 2);
                                COMMOD9.COEXST[MSP, ISP] = Math.Max(COMMOD9.COEXST[MSP, ISP], 2);

                                if ((AIJ >= 0) && (AMJ >= 0) && (AIJ != BIJ) && (AMJ != BMJ))//<=HXD
                                {
                                    COMMOD9.COEXST[ISP, MSP] = 3;
                                    COMMOD9.COEXST[MSP, ISP] = 3;
                                }

                                //C----------------------------------------------------------------------
                                //C     1.1D2. SIMILARLY, IF THE LAST OF SPECIES I WAS FOUND AT
                                //C          THE SAME LEVEL AS THE FIRST OF SPECIES M, THEY COEXIST 
                                //CPMS        under the loose coexistence rule COXSTF=2
                                //C----------------------------------------------------------------------

                            }
                            else if (BIJ == AMJ)
                            {
                                COMMOD9.COEXST[ISP, MSP] = Math.Max(COMMOD9.COEXST[ISP, MSP], 2);
                                COMMOD9.COEXST[MSP, ISP] = Math.Max(COMMOD9.COEXST[MSP, ISP], 2);
                            }
                        }
                    }
                }//For I
            }//For J

            //CPMS-------------------------------------------------------------------
            //CPMS    if any invalid input has been found, stop the program
            if (COMMOD9.STOPF == 1)
            {
                Helper.Write("___________________\n");
                Helper.Write("INVALID INPUT DATA\n");
                goto Label7400;
            }


            //C----------------------------------------------------------------------
            //C 2   PROOF OF COEXISTENCE FROM COMPARISON OF TWO SECTIONS
            //C-----------------------------------
            //C 2.1  Look for paired events for which one end of range is zeroed out
            //C      of all sections:  cross-over in un-zeroed ends may prove
            //C      overlap and thus coexistence.  But this is not a direct 
            //C      observation of overlap; so write 1 to the cells 
            //C----------------------------------------------------------------------

            

            for (I = 0; I < COMMOD9.NEVNT - 1; I++)
            {
                if ((COMMOD9.IROWS[I, 1] > 2) || (COMMOD9.IROWS[I, 1] < 1)) continue;
                if (Helper.ALLRZERO(I)) continue;

                for (M = I + 1; M < COMMOD9.NEVNT; M++)
                {
                    if ((COMMOD9.IROWS[M, 1] > 2) || (COMMOD9.IROWS[M, 1]) > 2) continue;//HACK:Maybe is a mistake (IF((IROWS(M,2).gt.2).or.(IROWS(M,2).gt.2)) CYCLE) IN FORTRAN

                    if (Helper.ALLRZERO(M)) continue;

                    ISP = COMMOD9.IROWS[I, 2];
                    MSP = COMMOD9.IROWS[M, 2];

                    if (COMMOD9.COEXST[ISP, MSP] > 1) continue;

                    for (J = 0; J < COMMOD9.NSCT - 1; J++)
                    {
                        if ((Helper.RZERO(I, J)) || (Helper.RZERO(M, J))) continue;

                        IJ = -1;//<=HXD
                        MJ = -1;//<=HXD
                        IJ = COMMOD9.ISTATIC[I, J, 0];
                        MJ = COMMOD9.ISTATIC[M, J, 0];

                        if ((IJ == -1) || (MJ == -1)) break;//<=HXD

                        for (K = J + 1; K < COMMOD9.NSCT; K++)
                        {
                            if ((Helper.RZERO(I, K)) || (Helper.RZERO(M, K))) continue;

                            IK = -1;//<=HXD
                            MK = -1;//<=HXD
                            IK = COMMOD9.ISTATIC[I, K, 0];
                            MK = COMMOD9.ISTATIC[M, K, 0];

                            if ((IK == -1) || (MK == -1)) continue;//<=HXD

                            if (((IJ < MJ) && (IK > MK)) ||
                                ((IJ > MJ) && (IK < MK)))
                            {
                                COMMOD9.COEXST[ISP, MSP] = 2;
                                COMMOD9.COEXST[MSP, ISP] = 2;
                                break;
                            }
                        }

                        if (COMMOD9.COEXST[ISP, MSP] > 1) break;
                    }
                }
            }

            //C----------------
            //C----------------
            //C 2.2 NOW LOOK FOR INSTANCES OF I ENTIRELY ABOVE M IN ONE SECTION AND
            //C     ENTIRELY BELOW IT IN ANOTHER. TO SAVE TIME, WE'LL ONLY CHECK
            //C     (I,M) PAIRS THAT HAVE NOT ALREADY BEEN SHOWN TO COEXIST
            //C     
            //C     This could be taken to prove coexistence under strict rules
            //C     because it proves overlap.  But, let us say that the combination
            //C     of evidence from two sections is always less strict than direct 
            //C     observation of overlap
            //C     So set cell to 1, as under loose rules
            //C----------------------------------------------------------------------
            for (I = 0; I < COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS - 1; I++)
            {
                Helper.FINDROW(I, 1, ref IAROW);

                if (IAROW == -1) continue;

                ISP = COMMOD9.IROWS[IAROW, 2];

                for (M = I + 1; M < COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS; M++)
                {
                    //	don't check all combinations of two events
                    //  but do check all combinations of two sections
                    Helper.FINDROW(M, 1, ref MAROW);

                    if (MAROW == -1) continue;

                    MSP = COMMOD9.IROWS[MAROW, 2];

                    if (COMMOD9.COEXST[ISP, MSP] == 0)
                    {
                        for (J = 0; J < COMMOD9.NSCT; J++)
                        {
                            BIJ = -1;//<=HXD
                            AMJ = -1;//<=HXD
                            IBROW = COMMOD9.IROWS[IAROW, 3];

                            if (!(Helper.RZERO(IBROW, J))) BIJ = COMMOD9.ISTATIC[IBROW, J, 0];
                            if (!(Helper.RZERO(MAROW, J))) AMJ = COMMOD9.ISTATIC[MAROW, J, 0];

                            if ((BIJ > 0) && (BIJ < AMJ))
                            {
                                //I entirely below M in section J
                                for (K = 0; K < COMMOD9.NSCT; K++)
                                {
                                    if (J == K) continue;

                                    BMK = -1;//<=HXD
                                    AIK = -1;//<=HXD
                                    MBROW = COMMOD9.IROWS[MAROW, 3];

                                    if (!(Helper.RZERO(MBROW, K))) BMK = COMMOD9.ISTATIC[MBROW, K, 0];
                                    if (!(Helper.RZERO(IAROW, K))) AIK = COMMOD9.ISTATIC[IAROW, K, 0];

                                    if ((BMK > 0) && (BMK < AIK))
                                    {
                                        //M entirely below I in section K
                                        COMMOD9.COEXST[ISP, MSP] = 2;
                                        COMMOD9.COEXST[MSP, ISP] = 2;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            //c***********************************************
        //CPMS  Print out the coexst matrix for the record 
        //CPMS  unless the coexst matrix was already read from this file 
        //CPMS  leave zeroes  -   write out a zero
        //CPMS  convert 2's and 3's to 1's      

            Label7350:

            if ((flg != 1) && (COMMOD9.CDF != 1))
            {
                //TODO:DOCOEX.CS
                //OPEN(21,FILE=COEXFILE)
                for (I = 0; I < COMMOD9.NSPC; I++)
                {
                    if (flg == 2)
                    {
                        //TODO:DOCOEX.CS
                        //WRITE(21,7500) (MIN(COEXST(I,J),INT2(1)),J=1,NSPC)
                    }
                    else if (flg == 3)
                    {
                        //TODO:DOCOEX.CS
                        //WRITE(21,7500) (MAX(COEXST(I,J)-2,INT2(0)),J=1,NSPC)
                    }
                }

                //TODO:DOCOEX.CS
                //7500    FORMAT(1X,9000(I1,1X))  
                //CLOSE(21)



            }


        Label7400:
        Label9999:

            //TextWriter writer = new StreamWriter(File.OpenWrite("test.txt"));
            //for (int xx = 0; xx < COMMOD9.NSPC; xx++)
            //{
            //    for (int yy = 0; yy < COMMOD9.NSPC; yy++)
            //    {
            //        writer.Write(string.Format("[{0},{1}]={2}  ", xx+1, yy+1, COMMOD9.COEXST[xx, yy]));
            //    }
            //    writer.Write("\n");
            //}
            //writer.Close();

            return;

        }
    }
}
