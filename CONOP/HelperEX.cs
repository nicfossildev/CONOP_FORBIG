using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CONOP.NET
{
    public static partial class Helper
    {
        //C***********************************************************************
        //C
        //C             PROGRAMMERS: PETE SADLER
        //C             LAST UPDATE: August 18th 2011 - for Intel 11.1
        //C
        //C     RETURNS THE PENALTY RESULTING FROM SQUEEZING 
        //C***********************************************************************
        public static void SMOOTH(int[] HPERM)
        {
            double X1, X2, X3, Y1, Y2, Y3;
            int J, K, L;

            //C***********************************************************************
            //c      initialize for Intel 11.1
            J = 0;
            K = 0;
            L = 0;
            X1 = 0.0;
            X2 = 0.0;
            X3 = 0.0;
            Y1 = 0.0;
            Y2 = 0.0;
            Y3 = 0.0;

            //CPMS   determine any additional penalty due to the smoothing and squeezing 
            //c      terms;  do this on all runs so that smoothness penalty can be reported
            //c      GETPEN runs only at beginning and end.  NEWPEN does not
            //c      determine smoothness penalty unless part of optimization
            //CPMS	 SMOOTHING
            //CPMS     reset smoothing penalty

            COMMOD COMMOD9 = COMMOD.Singleton();
            COMMOD9.SMPEN = 0.00;

            //CPMS     loop through all sections    
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                //CPMS       pair with all sections of higher number
                for (K = J + 1; K < COMMOD9.NSCT; K++)
                {
                    //CPMS         total departure from straight line for all triplets 
                    for (L = 1; L < COMMOD9.NEVNT - 1; L++)
                    {
                        //CPMS           find coordinates of triangle corners:
                        X1 = COMMOD9.VALEVEL[COMMOD9.HSCTSOL[HPERM[L - 1], J], J];
                        X3 = COMMOD9.VALEVEL[COMMOD9.HSCTSOL[HPERM[L + 1], J], J];

                        //CPMS            jump out if colinear - no penalty increment
                        if (X1 == X3) continue;

                        X2 = COMMOD9.VALEVEL[COMMOD9.HSCTSOL[HPERM[L], J], J];
                        Y1 = COMMOD9.VALEVEL[COMMOD9.HSCTSOL[HPERM[L - 1], K], K];
                        Y3 = COMMOD9.VALEVEL[COMMOD9.HSCTSOL[HPERM[L + 1], K], K];

                        //CPMS            jump out if colinear - no penalty increment
                        if (Y1 == Y3) continue;

                        Y2 = COMMOD9.VALEVEL[COMMOD9.HSCTSOL[HPERM[L], K], K];

                        //CPMS           determine cost of "long way round" 
                        COMMOD9.SMPEN = COMMOD9.SMPEN +
                            Math.Sqrt(Math.Pow((X2 - X1), 2) + Math.Pow((Y2 - Y1), 2)) +
                            Math.Sqrt(Math.Pow((X3 - X2), 2) + Math.Pow((Y3 - Y2), 2)) -
                            Math.Sqrt(Math.Pow((X3 - X1), 2) + Math.Pow((Y3 - Y1), 2));

                    }
                }
            }


        }

        //C***********************************************************************
        //C
        //C                       PROGRAMMERS: PETE SADLER
        //C                       LAST UPDATE: October 1st 1998
        //C
        //C     RETURNS THE PENALTY RESULTING FROM SQUEEZING
        //cpms
        //cpms  The basic CONOP penalties attempt to minimize the extensions
        //cpms  to the observed taxon ranges.  In effect, this minimizes the 
        //cpms  taxon ranges in the composite,  subject to the constraint that
        //cpms  they may not be shorter than in any local section.
        //cpms
        //cpms  The original formulation of squeezer recognizes that sections
        //cpms  have ranges in the composite too.  It keeps the section ranges
        //cpms  as short as possible.  In effect, this deals with problems 
        //cpms  arising when two or more subsets of the taxa have little or 
        //cpms  no between-set constraints on sequence.  They optimize 
        //cpms  independently and then interleave arbitrarily.  Because the 
        //cpms  result inflates the section ranges,  SQUEEZER was the fix;
        //cpms  but SHRINKER and TEASER now handle this.
        //cpms  
        //cpms  The most recent visit to the problem recognizes that sections 
        //cpms  have three ranges!  One, considered above, is the implied range
        //cpms  of all taxa observed and placed within the section.  The 
        //cpms  second is the range of all taxa observed within the section and yet
        //cpms  placed at the top,  i.e. LADs estimated to range out of the section.
        //cpms  The third concerns ranges extended through the base of the section;
        //cpms  it is the mirror image of the second and concerns FADs.
        //cpms  
        //cpms  The second and third ranges extend without control, because there is 
        //cpms  no penalty increment for moving within the range of events placed
        //cpms  at the limit of the section.  When many short sections are being
        //cpms  shingled together into a longer composite, this uncontrolled 
        //cpms  growth is disastrous.  SQUEEZER favors solutions that SQUEEZE these
        //cpms  events back into the section.  It minimizes the "outplaced" events --
        //cpms  tose observed in the section but placed at its ends.
        //cpms
        //cpms  Early squeezer operated on all three section ranges by trying to
        //cpms  to pull in the extreme events.  Unfortunately, this did not 
        //cpms  shrink the individual taxon ranges that were extended out into 
        //cpms  the top and bottom over-run regions.
        //cpms
        //cpms  SHRINKER was written to minimize the "inplaced" events --  those
        //cpms  observed events in the section and placed away from the ends.
        //cpms  TEASER deals with implied "missing" events --  those not observed  
        //cpms  in the section but placed within it.
        //C***********************************************************************
        public static void SQUEEZE(int[] HPERM)
        {
            int I, J, Imax, Imin;
            int Itop, Ibtm;

            Imax = 0;
            Imin = 0;
            Itop = 0;
            Ibtm = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //C     SQUEEZING
            //C     determine any additional penalty due to the squeezing factor
            COMMOD9.SQPEN = 0.0;
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                //cpms      determine the limits of observed events within the section
                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    if (COMMOD9.ISTATIC[HPERM[I], J, 0] > -1) break;//<=HXD
                }

                Imin = I;

                for (I = COMMOD9.NEVNT - 1; I >= 0; I--)
                {
                    if (COMMOD9.ISTATIC[HPERM[I], J, 0] > -1) break;//<=HXD
                }

                Imax = I;

                //cpms------the following block builds a SQPEN increment
                //cpms      for every event placed beyond the section
                //cpms      It is more time consuming than the total
                //cpms      section range approach;  but it is necessary
                //cpms      when the squeeze is applied at the end of the 
                //cpms      run, not throughout.
                //cpms      consider writing squeezer(1|2)  for two cases
                //cpms      determine highest and lowest outplaced events

                for (I = Imax; I >= 0; I--)
                {
                    if (COMMOD9.HSCTSOL[HPERM[I], J] < COMMOD9.HLEVEL[J]) break;
                }

                Itop = I + 1;

                for (I = Imin; I < COMMOD9.NEVNT; I++)
                {
                    if (COMMOD9.HSCTSOL[HPERM[I], J] > 0) break;//<=HXD
                }

                Ibtm = I - 1;

                for (I = Imin; I <= Ibtm; I++)//<=HXD
                {
                    if (!Helper.IORZERO(HPERM[I], J))
                    {
                        COMMOD9.SQPEN = COMMOD9.SQPEN + Math.Max(0, Ibtm - I);
                    }
                }

                for (I = Imax; I >= Itop; I--)
                {
                    if (!Helper.IORZERO(HPERM[I], J))
                    {
                        COMMOD9.SQPEN = COMMOD9.SQPEN + Math.Max(0, I - Itop);
                    }
                }

                //cpms--------------------------------------------
                //cpms    The loop below added a penalty for interleaving
                //cpms    non-observed events into the body of a section
                //cpms    it encouraged disastrously long range extensions
                //cpms    by somehow countermanding the effect of the 
                //cpms    penalty that reduced the outplaced ranges
                //cpms    Its action has been moved to TEASE
                //c		DO I=Ibtm, Itop
                //c	 	  IF(ISTATIC(HPERM(I),J,1).eq.0) THEN
                //c	 	     SQPEN=SQPEN + 1
                //c           ENDIF
                //c	    ENDDO
                //cpms------------------------------------------------
                //cpms      the following line penalized for the total
                //cpms      section range but did not efficiently draw
                //cpms      down the outplaced ranges
                //c
                //c         it is the total extent of the composite 
                //c         sequence occupied by events found in the 
                //c         section but placed beyond;  i.e. the 
                //c         sum of the two extreme cases, plus the
                //c         extent of the section itself.  The 
                //c         new formulation should sum all the individual
                //c         overflow events, and NOT include the 
                //c         section itself.  The former makes the penalty
                //c         bigger;  the latter makes it smaller.
                //c
                //c	    SQPEN=SQPEN+MAX(0,Imax-Imin)
                //c
                //c         we can keep a squeeze on the inner section
                //c         as follows:
                //c
                //c	    SQPEN=SQPEN+MAX(0,Itop-Ibtm)
                //c         This is the essence of SHRINK
                //c
                //cpms------------------------------------------------

            }
        }

        //C***********************************************************************
        //C
        //C                       PROGRAMMERS: PETE SADLER
        //C                       LAST UPDATE: June 12th 1999
        //C
        //C     RETURNS THE PENALTY RESULTING FROM SHRINKING
        //cpms
        //cpms  The basic CONOP penalties attempt to minimize the extensions
        //cpms  to the observed taxon ranges.  In effect, this minimizes the 
        //cpms  taxon ranges in the composite,  subject to the constraint that
        //cpms  they may not be shorter than in any local section.
        //cpms
        //cpms  The original formulation of SQUEEZER recognized that sections
        //cpms  have ranges in the composite too.  It keeps the section ranges
        //cpms  as short as possible.  
        //cpms  It was designed to deal with the problems that arose
        //cpms  when two or more subsets of the taxa have little or 
        //cpms  no between-set constraints on sequence.  They optimize 
        //cpms  independently and then interleave arbitrarily.  Because the 
        //cpms  result inflated the section ranges,  SQUEEZER was the fix.
        //cpms  It minimized the total span, in the composite, of all events 
        //cpms  OBSERVED in the section.  
        //cpms  Unfortunately, a similar problem arises when long ranging taxa
        //cpms  force a similarity between two sections that should not overlap;
        //cpms  i.e. a taxon range that is much longer than the section's range.
        //cpms  Taxon assemblages that include the beginning of the long
        //cpms  range are interleaved with taxon assemblages that include 
        //cpms  the end of the long range (unless there are many short lived
        //cpms  taxa in between).
        //cpms   
        //cpms  SHRINK is a more targeted approach to the long-ranging
        //cpms  taxon problem.  It seeks to minimize the number
        //cpms  of events placed inside the section  --  whether they were
        //cpms  observed in it or not.  Inside means away from the ends.
        //cpms  It favors solutions that stack the preserved pieces of the 
        //cpms  long range end-to-end rather than shingling them.
        //cpms  
        //cpms  SQUEEZE now attempts to minimize the distance that 
        //cpms  an event observed in the section will be placed beyond the section.
        //cpms  It makes sure that events which are observed
        //cpms  in a section are not placed too far beyond the ends of the preserved 
        //cpms  range of the section.  It operates largely, but not precisely,
        //cpms  counter to the purpose of SHRINK.  
        //cpms    
        //C***********************************************************************
        public static void SHRINK(int[] HPERM)
        {
            int I, J;
            int Itop, Ibtm;

            Itop = 0;
            Ibtm = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //C     SHRINKING
            //C     determine any additional penalty due to the shrinking factor
            COMMOD9.SHPEN = 0.0;

            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                //cpms      find the limits of the observed events 
                //cpms      as placed in the composite sequence        
                //c	    DO I=1, NEVNT
                //c		  IF(ISTATIC(HPERM(I),J,1).gt.0) EXIT
                //c	    ENDDO
                //c	    Imin=I
                //cpms      Imin is the oldest event observed in the section
                //c	    DO I=NEVNT, 1, -1
                //c		  IF(ISTATIC(HPERM(I),J,1).gt.0) EXIT
                //c	    ENDDO
                //c	    Imax=I
                //cpms      Imax is the youngest event observed in the section

                //cpms      find the limits of all events placed within
                //cpms      the section

                for (I = COMMOD9.NEVNT - 1; I >= 0; I--)
                {
                    if (COMMOD9.HSCTSOL[HPERM[I], J] < COMMOD9.HLEVEL[J]) break;
                }

                Itop = I;

                //cpms   Itop is the youngest event (per composite)
                //cpms   placed within the section

                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    if (COMMOD9.HSCTSOL[HPERM[I], J] > 0) break;//<=HXD
                }

                Ibtm = I;

                //cpms      Ibtm is the oldest event (per composite)		
                //cpms      placed within the section

                //cpms      the following line penalizes for the total
                //cpms      section range (number of events placed
                //cpms      within the section) and does not try to hold
                //cpms      the outplaced ranges close to the section ends

                COMMOD9.SHPEN = COMMOD9.SHPEN + Math.Max(0, Itop - Ibtm);
                //cpms------------------------------------------------
            }

        }

        //C   Subroutine to determine the number of coexistences      
        //C   implied in the current solution but not observed
        public static void ROYAL(int[] XPERM, ref double XPEN)
        {
            int I, M;
            //C***************************
            //c     COEX(sp1,sp2,Xperm) is a function!
            //c     COEXST(NSPC,NSPC)   is a matrix in IROWS(I,3) order!
            //c     DOCOEX()            is a subroutine!
            //c---------------------------
            XPEN = 0.0;
            COMMOD COMMOD9 = COMMOD.Singleton();

            for (I = 0; I < COMMOD9.NEVNT - 1; I++)
            {
                //c        use FADs to count each species only once
                if (COMMOD9.IROWS[I, 1] != 1) continue;

                for (M = I + 1; M < COMMOD9.NEVNT; M++)
                {
                    if (COMMOD9.IROWS[M, 1] != 1) continue;

                    //c          if observed to coexist, cycle
                    if (COMMOD9.COEXST[COMMOD9.IROWS[I, 2], COMMOD9.IROWS[M, 2]] >= COMMOD9.COXSTF) continue;

                    //c          if implied to coexist, increment penalty
                    if (Helper.COEX(COMMOD9.IROWS[I, 0], COMMOD9.IROWS[M, 0], XPERM)) XPEN = XPEN + 1;
                }
            }

        }

        //CPMS  Logical function set to .TRUE. 
        //CPMS  if species sp1 and sp2 coexist
        //CPMS  in the best sequence
        //CPMS  (NOT FIELD OBSERVATIONS)
        //CPMS---------------------------------
        public static bool COEX(int sp1, int sp2, int[] XPERM)
        {
            int spX, I, Iprm, N;
            bool retVal = false;

            spX = 0;
            N = 0;
            COMMOD COMMOD9 = COMMOD.Singleton();

            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                Iprm = COMMOD9.IROWS[XPERM[I], 0];

                //cpms    count up to FAD of either species 
                if ((Iprm == sp1) || (Iprm == sp2))
                {
                    if (COMMOD9.IROWS[XPERM[I], 1] != 3)
                    {
                        N = N + 1;

                        if (N == 1)
                        {
                            //cpms          first FAD, note which species
                            spX = Iprm;
                        }
                        else if (N == 2)
                        {
                            //cpms   note if beginning of other species
                            //cpms   if so, they coexist
                            if (Iprm != spX) retVal = true;

                            break;
                        }
                    }
                }
            }

            return retVal;
        }

        //C   Subroutine to determine the number of FAD-below-LAD pairs       
        //C   implied in the current solution but not observed
        public static void SEQUEL(int[] XPERM, ref double XPEN)
        {
            int I, M;

            //C***************************
            //c     FB4L(sp1,sp2,Xperm) is a function! that reads sequnce
            //c     FADLAD(NSPC,NSPC)   is a matrix in IROWS(I,3) order!  from observations
            //c     DOFB4L()            is a subroutine! to load FADLAD from observations
            //c---------------------------
            XPEN = 0.0;
            COMMOD COMMOD9 = COMMOD.Singleton();

            for (I = 0; I < COMMOD9.NEVNT - 1; I++)
            {
                //c        use FADs to count each species only once
                if (COMMOD9.IROWS[I, 1] != 1) continue;

                for (M = 0; M < COMMOD9.NEVNT; M++)
                {
                    if (COMMOD9.IROWS[M, 1] != 2) continue;

                    //c          if observed FAD before LAD, OK, cycle
                    if (COMMOD9.FADLAD[COMMOD9.IROWS[I, 2], COMMOD9.IROWS[M, 2]] >= 1) continue;

                    //c          if implied in FAD\LAD order, increment penalty
                    if (Helper.FB4L(COMMOD9.IROWS[I, 0], COMMOD9.IROWS[M, 0], XPERM)) XPEN = XPEN + 1;
                }
            }

        }

        //CPMS  Logical function set to .TRUE. 
        //CPMS  if species sp1 LAD is listed
        //CPMS  before sp2 FAD
        //CPMS  in the best sequence
        //CPMS  (NOT FIELD OBSERVATIONS)
        //CPMS---------------------------------
        public static bool FB4L(int sp1, int sp2, int[] XPERM)
        {
            int I, Iprm;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //cpms  run up the event sequence from the base
            //cpms  IF FADsp1 is encountered first
            //cpms        set to true and exit
            //cpms  If LADsp2 is encountered first
            //cpms        leave false and exit
            bool retVal = false;

            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                Iprm = COMMOD9.IROWS[XPERM[I], 0];

                //cpms    count up to FADsp1 or LADsp2 
                if ((Iprm == sp1) &&
                    (COMMOD9.IROWS[XPERM[I], 1] == 1))
                {
                    retVal = true;
                    break;
                }

                if ((Iprm == sp2) && (COMMOD9.IROWS[XPERM[I], 1] == 2)) break;

            }

            return retVal;
        }


        //C***********************************************************************
        //C
        //C                       PROGRAMMERS: PETE SADLER
        //C                       LAST UPDATE: Jan 26th 2000
        //C
        //C     RETURNS THE PENALTY RESULTING FROM TEASING
        //cpms
        //cpms  The basic CONOP penalties attempt to minimize the extensions
        //cpms  to the observed taxon ranges.  In effect, this minimizes the 
        //cpms  taxon ranges in the composite,  subject to the constraint that
        //cpms  they may not be shorter than in any local section.
        //cpms
        //cpms  TEASER penalizes for extending ranges into sections where the 
        //cpms  taxon has not been observed.  There are several options:
        //cpms
        //cpms  THRU  -  penalize for ranging thru a section where not found
        //cpms  INCL  -  penalize for entering a section where not found
        //cpms  DIST  -  penalize for all levels crossed in section where not found
        //cpms           i.e. penalty = length of range in section
        //cpms  EXIT  -  penalize for distance to preferred exit
        //cpms           i.e. FAD to top and LAD to base
        //cpms           preferred exit is desiged to shorten range in composite
        //cpms ----------
        //cpms Notice that teaser runs after the horizon placements are complete.
        //cpms Thus, it can deal with the FAD and LAD for one event.
        //cpms
        //cpms USE_NEGATIVE tries to influence the horizon placements;  it cannot
        //cpms refer to the FAD when trying to place the LAD.
        //C***********************************************************************
        public static void TEASE(int[] HPERM)
        {
            int I, J;

            //C     TEASING
            //C     determine any additional penalty due to the teasing factor
            COMMOD COMMOD9 = COMMOD.Singleton();

            COMMOD9.TSPEN = 0.0;

            if (COMMOD9.STKF == 7) goto Label7000;
            if (COMMOD9.STKF == 8) goto Label8000;
            if (COMMOD9.STKF == 9) goto Label9000;

            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                for (J = 0; J < COMMOD9.NSCT; J++)
                {
                    //cpms  if not a paired event, cycle
                    if ((COMMOD9.IROWS[HPERM[I], 1] > 2) ||
                        (COMMOD9.IROWS[HPERM[I], 1] < 1)) continue;

                    //cpms  if observed in section, always cycle
                    //cpms  IF(ISTATIC(HPERM(I),J,1).gt.0) CYCLE

                    switch (COMMOD9.HOMEF)
                    {
                        //cpms if in section or section contains a coexistor
                        case 1:
                            //cpms  (SS EVIDENCE OF PRESENCE)
                            //cpms   penalize if not seen, coex does not count as seen
                            //cpms   i.e. negative penaties on 0 or 2
                            //cpms        skip tease on 1 only 
                            if (COMMOD9.NEGATIVE[COMMOD9.IROWS[HPERM[I], 2], J] == 1) continue;
                            break;
                        case 2:
                            //cpms	(SL EVIDENCE OF PRESENCE)
                            //cpms   penalize only if not seen AND coex not seen
                            //cpms   i.e. negative penalties on 0 only
                            //cpms        skip tease on 1 (seen) and 2 (coex)
                            if (COMMOD9.NEGATIVE[COMMOD9.IROWS[HPERM[I], 2], J] > 0) continue;
                            break;
                    }

                    switch (COMMOD9.STKF)
                    {
                        case 1:
                            //cpms  (STACKER = THRU)
                            //cpms    if a FAD, and placed at base
                            if ((COMMOD9.IROWS[HPERM[I], 1] == 1) &&
                                (COMMOD9.HSCTSOL[HPERM[I], J] == 0))//<=HXD
                            {
                                //cpms  if corresponding LAD placed at top
                                if (COMMOD9.HSCTSOL[COMMOD9.IROWS[HPERM[I], 3], J] == COMMOD9.HLEVEL[J])
                                { COMMOD9.TSPEN = COMMOD9.TSPEN + 1; }
                            }

                            break;
                        case 2:
                        case 4:
                            //cpms  (STACKER = INCL,FREQ)
                            //cpms    if a FAD placed below top
                            if (COMMOD9.IROWS[HPERM[I], 1] == 1)
                            {
                                if (COMMOD9.HSCTSOL[HPERM[I], J] < COMMOD9.HLEVEL[J])
                                {
                                    if (COMMOD9.STKF == 4)
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN + (0.5 * COMMOD9.CULLIST[HPERM[I], 2]);
                                    }
                                    else
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN + 0.5;
                                    }
                                }
                                else
                                {
                                    //cpms  placed at top (INCL)
                                    if (COMMOD9.STKF == 4)
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN - (0.5 * COMMOD9.CULLIST[HPERM[I], 2]);
                                    }
                                    else
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN = 0.5;
                                    }
                                }
                            }
                            //cpms  if a LAD
                            else if (COMMOD9.IROWS[HPERM[I], 1] == 2)
                            {
                                if (COMMOD9.HSCTSOL[HPERM[I], J] > 0)//<=HXD
                                {
                                    //cpms  placed above base (INCL)
                                    if (COMMOD9.STKF == 4)
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN + (0.5 * COMMOD9.CULLIST[HPERM[I], 2]);
                                    }
                                    else
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN + 0.5;
                                    }
                                }
                                else
                                {
                                    //cpms  placed at base (INCL)
                                    if (COMMOD9.STKF == 4)
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN - (0.5 * COMMOD9.CULLIST[HPERM[I], 2]);
                                    }
                                    else
                                    {
                                        COMMOD9.TSPEN = COMMOD9.TSPEN = 0.5;
                                    }
                                }
                            }

                            break;

                        case 3:
                        case 6:
                            //cpms   (STACKER = DIST,PROP)
                            //cpms     net penalty is number of intervals (between levels) that implied range
                            //cpms     occupies in section
                            //cpms     cases:
                            //cpms        0 - range thru section
                            //cpms            (HLEVEL-1)
                            //cpms        1a - range entirely within section, FAD and LAD placed apart
                            //cpms            (LAD-FAD) 
                            //cpms        1b - range entirely within section, FAD and LAD at same level
                            //cpms            (0)            
                            //cpms        2 - range from top down into section
                            //cpms            (HLEVEL-FAD) 
                            //cpms        3 - range from base up into section
                            //cpms            (LAD-1)
                            //cpms        4 - range above section (both ends placed at top)
                            //cpms            (HLEVEL-HLEVEL)
                            //cpms        5 - range below section (both ends placed at base)
                            //cpms            (1-1)
                            //cpms
                            //cpms     if a FAD 
                            if (COMMOD9.IROWS[HPERM[I], 1] == 1)
                            {
                                //cpms  subtract penalty equal to the distance up from base
                                if (COMMOD9.STKF == 3)
                                {
                                    COMMOD9.TSPEN = COMMOD9.TSPEN - (double)(COMMOD9.HSCTSOL[HPERM[I], J]+1);//<=HXD
                                }
                                else if (COMMOD9.STKF == 6)
                                {
                                    COMMOD9.TSPEN = COMMOD9.TSPEN - (double)(COMMOD9.HSCTSOL[HPERM[I], J]+1)
                                        / (double)(COMMOD9.HLEVEL[J]+1);//<=HXD
                                }
                            }
                            //cpms  if a LAD
                            else if (COMMOD9.IROWS[HPERM[I], 1] == 2)
                            {
                                //cpms  add penalty equal to distance up from base
                                if (COMMOD9.STKF == 3)
                                {
                                    COMMOD9.TSPEN = COMMOD9.TSPEN + (double)(COMMOD9.HSCTSOL[HPERM[I], J]+1);//<=HXD
                                }
                                else if (COMMOD9.STKF == 6)
                                {
                                    COMMOD9.TSPEN = COMMOD9.TSPEN + (double)(COMMOD9.HSCTSOL[HPERM[I], J]+1)
                                        / (double)(COMMOD9.HLEVEL[J]+1);//<=HXD
                                }
                            }

                            break;

                        //cpms    net effect is to add a penalty equal to the length of the range
                        //cpms    this leads to spurious bonuses for shrinking range within section
                        //cpms    rather than driving it out!
                        //cpms    unfortunately, we do not know which end is the appropriate exit!!
                        case 5:
                            //cpms  (STACKER = EXIT)
                            //cpms    encourage range shortening and exit from section
                            //cpms    penalize for FAD below top, LAD above base, BUT
                            //cpms    IF LAD at base, don't penalize FAD at base
                            //cpms    IF FAD at top, don't penalize LAD at top
                            if (COMMOD9.IROWS[HPERM[I], 1] == 1)
                            {
                                //cpms  - a FAD, if not both at base, penalize by distance from top 
                                if ((COMMOD9.HSCTSOL[HPERM[I], J] != 0) ||
                                    (COMMOD9.HSCTSOL[HPERM[COMMOD9.IROWS[I, 3]], J] != 0))//<=HXD
                                {
                                    COMMOD9.TSPEN = COMMOD9.TSPEN + (double)(COMMOD9.HLEVEL[J] -
                                        COMMOD9.HSCTSOL[HPERM[I], J]);
                                }
                            }
                            else if (COMMOD9.IROWS[HPERM[I], 1] == 2)
                            {
                                //cpms  - a LAD, if not both at top, penalize by distance from base 
                                if ((COMMOD9.HSCTSOL[HPERM[I], J] != COMMOD9.HLEVEL[J]) ||
                                    (COMMOD9.HSCTSOL[HPERM[COMMOD9.IROWS[I, 3]], J] != COMMOD9.HLEVEL[J]))
                                {
                                    COMMOD9.TSPEN = COMMOD9.TSPEN + (double)(COMMOD9.HSCTSOL[HPERM[I], J] - 1+1);//<=HXD
                                }
                            }

                            break;
                    }//End switch

                }//End for
            }//End for

                //cpms---------------------
        Label7000:
            if (COMMOD9.STKF == 7)
            {
                //cpms    penalty built on taxon range in PERM
                //cpms    fastest tease penalty
                //cpms    does not need to check observed or placed levels
                //cpms    but is insensitive to HOMERANGE
                //CPMS    CHECK WHETHER THENEXT LOOP IS NEEDED
                //CPMS    NEGATIVE MAY BE ALREADY FULLY LOADED
                if (COMMOD9.NSPC == 0) goto Label8000;

                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    if (COMMOD9.IROWS[HPERM[I], 1] == 1)
                    {
                        COMMOD9.NEGATIVE[COMMOD9.IROWS[HPERM[I], 2], COMMOD9.NSCT + 2] = I;
                    }
                    else if (COMMOD9.IROWS[HPERM[I], 1] == 2)
                    {
                        COMMOD9.NEGATIVE[COMMOD9.IROWS[HPERM[I], 2], COMMOD9.NSCT + 3] = I;
                    }
                }

                for (I = 0; I < COMMOD9.NSPC; I++)
                {
                    COMMOD9.TSPEN = COMMOD9.TSPEN +
                        COMMOD9.NEGATIVE[I, COMMOD9.NSCT + 3] - COMMOD9.NEGATIVE[I, COMMOD9.NSCT + 2];
                }
            }

//cpms---------------------
        Label8000:
            if (COMMOD9.STKF == 8)
            {
                //cpms   penalty based on the number of coexistences
                //cpms   implied by perm, but not seen
                Helper.ROYAL(HPERM, ref COMMOD9.TSPEN);
            }

//cpms---------------------
        Label9000:
            if (COMMOD9.STKF == 9)
            {
                //cpms   penalty based on the number of coexistences
                //cpms   implied by perm, but not seen
                if (COMMOD9.FB4LF != 0) SEQUEL(HPERM, ref COMMOD9.TSPEN);
            }

        }

        //c  subroutine to calculate
        //c  the scaled total of the secondary penaties  TOOPEN  and 
        //c  the unscaled total of secondary penalties   TO2PEN
        //c
        //c  the scaling factors are KTS, KSM, etc
        //c  the proportionality factors that determine how the 
        //c  penalty terms are added together
        //c
        //CPMS***********************
        public static void DOTOO()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            COMMOD9.TOOPEN = (COMMOD9.SMPEN * COMMOD9.KSM) +
                (COMMOD9.SQPEN * COMMOD9.KSQ) +
                (COMMOD9.SHPEN * COMMOD9.KSH) +
                (COMMOD9.TSPEN * COMMOD9.KTS);

            COMMOD9.TO2PEN = 0.0;
            if (COMMOD9.KTS > 0.0) COMMOD9.TO2PEN = COMMOD9.TO2PEN + COMMOD9.TSPEN;
            if (COMMOD9.KSM > 0.0) COMMOD9.TO2PEN = COMMOD9.TO2PEN + COMMOD9.SMPEN;
            if (COMMOD9.KSQ > 0.0) COMMOD9.TO2PEN = COMMOD9.TO2PEN + COMMOD9.SQPEN;
            if (COMMOD9.KSH > 0.0) COMMOD9.TO2PEN = COMMOD9.TO2PEN + COMMOD9.SHPEN;

        }

        public static void STEPOUT(int[] myperm)
        {
            //TODO:STEPOUT()
            //throw new Exception();
        }

        public static void TRAJOUT()
        {
            //TODO:TRAJOUT()
            //throw new Exception();
        }

        //C***********************************************************************
        //CPMS     A SUBROUTINE TO DETERMINE THE LIMITS
        //C        OF UP (RANKUP) AND DOWN (RANKDN) MOVEMENT
        //C        OF ONE EVENT (IRANK) IN CURRENT PERM
        //C
        //CPMS               PROGRAMMER:  PETE SADLER
        //CPMS               LAST UPDATE: August 16th 2011
        //c                       - removed IF(().AND.()) condtion constructs 
        //c                         to force Intel Compiler 11.1
        //c                         to evaluate conditions in my order
        //C
        //C***********************************************************************
        //C     INPUTS:
        //C         PERM - THE CURRENT PERMUTATION 
        //C              - A (NEVNT) INTEGER VECTOR
        //C 	   IRANK - THE EVENT TO MOVE
        //C
        //C     OUTPUT:
        //CPMS         RANKUP - the highest possible rank
        //CPMS
        //CPMS         RANKDN - the lowest possible rank
        //CPMS   
        //c
        //c     This routine uses event types, not allowed moves.  Therefore,
        //c     MAX and MIN must be treated separately from FAD and LAD
        //c       
        //CPMS-------------------------------------------------------------------
        public static void NABRLIMT(int IRANK, ref int RANKUP, ref int RANKDN)
        {
            int RANK, RTYPE, INUMB;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //CPMS  event at IRANK changes rank;  JRANK is its new rank
            //CPMS  RANK is a temporary rank counter
            //CPMS  RANKDN and RANKUP are limits on the range of JRANK
            //CPMS-------------------------------------------------------------------
            //CPMS      i.e. if IRANK is a first (1)
            //CPMS    RANKUP cannot be above the corresponding LAD
            //CPMS           if IRANK is a last (2)
            //CPMS    RANKDN cannot be below the corresponding FAD
            //cpms--------------------------------------
            //c      Initialize local variables
            RANK = 0;  // internal counter for position in sequence
            RANKUP = 0;  // for output
            RANKDN = 0;  // for output
            RTYPE = 0;  // internal - event type at RANK being tested
            INUMB = 0;//  // internal - event number at given IRANK      
            //cpms   find event number for IRANK
            INUMB = COMMOD9.IROWS[COMMOD9.PERM[IRANK], 0];

           

            //CPMS CASE 1: IRANK is a FAD (type.eq.1)
            //CPMS     walk through vector from base to corresponding LAD or MID
            //CPMS     move RANKDN up to highest/last ptlord violation +1
            //CPMS     move RANKUP up to lowest/first ptlord or coex violation -1
            //CPMS     or to corresponding LAD/MID rank -1 
            if (COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1] == 1)
            {
                //c---------------------------------------------------
                //c      the old 2000 version
                if (COMMOD9.SOLVEF != 8)
                {
                    RANK = 0;
                    RANKDN = 0;
                    RANKUP = COMMOD9.NEVNT - 1;
                }
                else //! SOLVEF.eq.8 i.e. tease/stack trying to move FADs up only
                {
                    //HACK:Check it  //<=HXD input rank >=0
                    RANK = IRANK;
                    RANKDN = IRANK;
                    RANKUP = COMMOD9.NEVNT - 1;
                }

                RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];

                //c  Move test-rank (RANK) up from minimum value (usually 1)
                //c  Do move past MID or LAD of same taxon
                while (!((COMMOD9.IROWS[COMMOD9.PERM[RANK], 0] == INUMB) && (RTYPE != 1)))
                {
                    //c  If not yet up to rank of event that is to move (IRANK)  
                    if (RANK < IRANK)
                    {
                        //FAD moving down, range extension, no COEXST problem
                        //c    check for ptlord violations, fix RANKDN
                        //c    violation means RANK event is TYPE>2 and must be below IRANK event
                        //c    i.e. PTLORD(IRANK,RANK) is 2 
                        //c      don't bother to check if there are no other event types
                        if (COMMOD9.NOTHR > 0)
                        {
                            if ((RTYPE > 0))
                            {
                                if ((RTYPE > 2) || (RTYPE < 1))
                                {
                                    if (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 2)
                                    {
                                        //possible limit, set RANKDN but climb on
                                        RANKDN = RANK + 1;
                                    }
                                }
                            }
                        }
                    }
                    //If past rank at origin (Irank) of move  
                    else if (RANK > IRANK)
                    {
                        //c   range contraction: check for ptlord and coex violations, fix RANKUP
                        //c   violation means RANK event is TYPE>2 and must be above IRANK event
                        //c   i.e. PTLORD(IRANK,RANK) is 1 
                        if (COMMOD9.NOTHR > 0)
                        {
                            if ((RTYPE > 2) || (RTYPE < 1))
                            {
                                if (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 1)
                                {
                                    break;
                                }
                            }

                        }
                        //coex check:
                        else if (COMMOD9.NSPC > 0)
                        {
                            if (COMMOD9.COXSTF != 4)
                            {
                                if (COMMOD9.IROWS[COMMOD9.PERM[RANK], 1] == 2)
                                {
                                    if (COMMOD9.COEXST[COMMOD9.IROWS[COMMOD9.PERM[IRANK], 2], COMMOD9.IROWS[COMMOD9.PERM[RANK], 2]]
                                        >= COMMOD9.COXSTF)
                                    {
                                        break;
                                    }

                                    if (COMMOD9.FB4LF != 0)
                                    {
                                        if (COMMOD9.FADLAD[COMMOD9.IROWS[COMMOD9.PERM[IRANK], 2],
                                            COMMOD9.IROWS[COMMOD9.PERM[RANK], 2]] > 0)
                                        {
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }//! RANK.GT.IRANK condition closed  

                    RANK = RANK + 1;
                    RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];
                }

                RANKUP = RANK - 1;
            }
            //CPMS--------------------------------------
            //CPMS CASE 1.5: IRANK is a MAX (type.eq.11)
            //CPMS         walk through vector from base to corresponding MIN
            //CPMS         move RANKDN up to highest/last ptlord violation +1
            //CPMS         move RANKUP up to lowest/first ptlord -1
            //CPMS         or to corresponding LAD/MID rank -1 
            else if (COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1] == 1)
            {
                //      ELSE IF ( IROWS(PERM(IRANK),2).EQ.1 ) THEN
                //c---------------------------------------------------
                //c      the old 2000 version
                if (COMMOD9.SOLVEF != 8)
                {
                    RANK = 0;
                    RANKDN = 0;
                    RANKUP = COMMOD9.NEVNT - 1;                   
                }
                else
                {
                    RANK = IRANK;
                    RANKDN = IRANK;
                    RANKUP = COMMOD9.NEVNT - 1;
                }

                RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];
                //c  do until same number and type 12  (i.e. not 11)
                while (!((COMMOD9.IROWS[COMMOD9.PERM[RANK], 0] == INUMB) && (RTYPE != 11)))
                {
                    if (RANK < IRANK)
                    {
                        //c  check for ptlord violations, fix RANKDN
                        //c  violation means RANK event is TYPE>2 and must be below IRANK event
                        //c  i.e. PTLORD(IRANK,RANK) is 2 
                        if (COMMOD9.NOTHR > 0)
                        {
                            if (((RTYPE > 2) || (RTYPE < 1)) &&
                            (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 2))
                            {
                                //possible limit, set RANKDN but climb on  
                                RANKDN = RANK + 1;
                            }
                        }
                    }
                    else if (RANK > IRANK)
                    {
                        //c  check for ptlord violations, fix RANKUP
                        //c  violation means RANK event is TYPE>2 and must be above IRANK event
                        //c  i.e. PTLORD(IRANK,RANK) is 1 
                        if (COMMOD9.NOTHR > 0)
                        {
                            if (((RTYPE > 2) || (RTYPE < 1)) &&
                                (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 1))
                            {
                                break;
                            }
                        }
                    }

                    RANK = RANK + 1;
                    RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];

                }

                RANKUP = RANK - 1;
            }
            else if ((COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1]) == 2)
            {
                //CPMS----------------------------------               
                //CPMS CASE 2: IRANK is a LAD (type.eq.2)
                //CPMS         walk through vector from top to corresponding FAD or MID
                //CPMS         move RANKUP down to lowest/last ptlord violation -1
                //CPMS         move RANKDN down to highest/first ptlord or coex violation +1
                //CPMS         or to corresponding LAD rank +1 
                //c
                //c----------------------------------------------------------
                //c      the old 2000 version
                if (COMMOD9.SOLVEF != 8)
                {
                    RANK = COMMOD9.NEVNT - 1;
                    RANKUP = COMMOD9.NEVNT - 1;
                    RANKDN = 0;
                }
                else
                {
                    RANK = IRANK;
                    RANKUP = IRANK;
                    RANKDN = 0;
                }




                RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];
                //do while not same number and type 1 or 3,  i.e. not 2 
                while (!((COMMOD9.IROWS[COMMOD9.PERM[RANK], 0] == INUMB) && (RTYPE != 2)))
                {
                    if (RANK > IRANK)
                    {
                        //c    check for ptlord violations, fix RANKUP
                        //c    violation means RANK event is TYPE>2 and must be above IRANK event
                        //c    i.e. PTLORD(IRANK,RANK) is 1 
                        //CPMS The following combination of conditions does not work in Intel 11.1
                        //c    The compiler would appear to evaluate all conditions, even if one is false.
                        //c    or perhaps it does not evaluate them in the order written
                        //c    snags arise if one condition is designed to prevent checking another
                        //c    So, some combinations of conditions need to be separated

                        if (COMMOD9.NOTHR > 0)
                        {
                            if ((RTYPE > 2) || (RTYPE < 1))
                            {
                                if (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 1)
                                {
                                    RANKUP = RANK - 1;
                                }
                            }
                        }
                    }
                    else if (RANK < IRANK)
                    {
                        //c  check for ptlord and coex violations, fix RANKDN
                        //c  violation means RANK event is TYPE>2 and must be below IRANK event
                        //c  i.e. PTLORD(IRANK,RANK) is 2 
                        if (COMMOD9.NOTHR > 0)
                        {
                            if ((RTYPE > 2) || (RTYPE < 1))
                            {
                                if (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 2)
                                {
                                    break;
                                }
                            }
                        }

                        if (COMMOD9.NSPC > 0)
                        {
                            if (COMMOD9.COXSTF != 4)
                            {
                                if (COMMOD9.IROWS[COMMOD9.PERM[RANK], 1] == 1)
                                {
                                    if (COMMOD9.COEXST[COMMOD9.IROWS[COMMOD9.PERM[RANK], 2],
                                        COMMOD9.IROWS[COMMOD9.PERM[IRANK], 2]] >= COMMOD9.COXSTF)
                                    {
                                        break;
                                    }

                                    if (COMMOD9.FB4LF != 0)
                                    {
                                        if (COMMOD9.FADLAD[COMMOD9.IROWS[COMMOD9.PERM[RANK], 2],
                                            COMMOD9.IROWS[COMMOD9.PERM[IRANK], 2]] > 0)
                                        {
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    RANK = RANK - 1;
                    RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];

                }

                RANKDN = RANK + 1;
            }
            else if ((COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1]) == 12)
            {
                //c-------------------------------------       
                //c------start of new MIN code
                //CPMS----------------------------------               
                //CPMS CASE 2.5: IRANK is a MIN (type.eq.12); i.e. higher of pair
                //CPMS   walk through vector from top to corresponding MAX
                //CPMS   move RANKUP down to lowest/last ptlord violation -1
                //CPMS   move RANKDN down to highest/first ptlord +1
                //CPMS   or to corresponding LAD rank +1 

                //c----------------------------------------------------------
                //c      the old 2000 version
                if (COMMOD9.SOLVEF != 8)
                {
                    RANK = COMMOD9.NEVNT - 1;
                    RANKUP = COMMOD9.NEVNT - 1;
                    RANKDN = 0;
                }
                else
                {
                    RANK = IRANK - 1;
                    RANKUP = IRANK - 1;
                    RANKDN = 0;
                }

                RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];
                //do until same number and type 11,  i.e. not 12
                while (!((COMMOD9.IROWS[COMMOD9.PERM[RANK], 0] == INUMB) && (RTYPE != 12)))
                {
                    if (RANK > IRANK)
                    {
                        //c   check for ptlord violations, fix RANKUP
                        //c   violation means RANK event is TYPE>2 and must be above IRANK event
                        //c   i.e. PTLORD(IRANK,RANK) is 1
                        if (COMMOD9.NOTHR > 0)
                        {
                            if (((RTYPE > 2) || (RTYPE < 1)) &&
                                (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 1))
                            {
                                RANKUP = RANK - 1;
                            }
                        }
                    }
                    else if (RANK < IRANK)
                    {
                        //c   check for ptlord violations, fix RANKDN
                        //c   violation means RANK event is TYPE>2 and must be below IRANK event
                        //c   i.e. PTLORD(IRANK,RANK) is 2 
                        if (COMMOD9.NOTHR > 0)
                        {
                            if (((RTYPE > 2) || (RTYPE < 1)) &&
                                (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[RANK]] == 2))
                            {
                                break;
                            }
                        }
                    }

                    RANK = RANK - 1;
                    RTYPE = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];

                }

                RANKDN = RANK + 1;

            }
            else if (COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1] == 3)
            {
                //CPMS  CASE 3:  IRANK is a MID (type.eq.3)
                //CPMS           need to work up and down
                //CPMS           RANKDN find FAD or other mid -  go down to next same event number
                //CPMS           RANKUP find LAD or other mid -  go up to next same event number
                //CPMS           no need to check coex violations  -  stay within range
                //CPMS           no need to check for PTLORD violations - moves up and down
                //CPMS           any PTLORD determined by own FAD and LAD

                //cpms    count down to rankdn; start 1 below IRANK
                //cpms        a MID cannot be at the base of the sequence (FAD is lower)
                for (RANKDN = IRANK - 1; RANKDN >= 1; RANKDN--)
                {
                    if (COMMOD9.IROWS[COMMOD9.PERM[RANKDN - 1], 0] == INUMB) break;//HACK:IF(IROWS(PERM(RANKDN-1),1).EQ.INUMB) EXIT  -- IN FORTRAN
                }

                //cpms    count up to rankup; start 1 above IRANK
                //cpms    a MID cannot be at top of sequence (LAD is higher)
                for (RANKUP = IRANK - 1; RANKUP < COMMOD9.NEVNT - 1; RANKUP++)
                {
                    if (COMMOD9.IROWS[COMMOD9.PERM[RANKUP + 1], 0] == INUMB) break;
                }

            }
            else if ((COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1] > 3) ||
                (COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1] < 1))
            {
                //CPMS  CASE 4:  IRANK is an unpaired event (type.gt.2 or type.lt.1)
                //CPMS           AFTER CASE 3! So, CHANGE TO type.gt.3
                //CPMS           walk through vector from top to bottom
                //CPMS           move RANKDN up to highest/last ptlord violation +1
                //CPMS           move RANKUP up to lowest/first ptlord violation -1
                RANK = 0;
                RANKUP = COMMOD9.NEVNT - 1;
                RANKDN = 0;


                while (RANK <= COMMOD9.NEVNT - 1)
                {
                    if (RANK < IRANK)
                    {
                        //c  check for ptlord violations, fix RANKDN
                        //c  violation means RANK event must be below IRANK event (TYPE>2!)
                        //c  i.e. PTLORD(RANK,IRANK) is 1 
                        if (COMMOD9.PTLORD[COMMOD9.PERM[RANK], COMMOD9.PERM[IRANK]] == 1)
                        {
                            RANKDN = RANK + 1;
                        }
                    }
                    else if (RANK > IRANK)
                    {
                        //c   check for ptlord violations, fix RANKUP
                        //c   violation means RANK event must be above IRANK event (TYPE>2!)
                        //c   i.e. PTLORD(RANK,IRANK) is 2 
                        if (COMMOD9.PTLORD[COMMOD9.PERM[RANK], COMMOD9.PERM[IRANK]] == 2)
                        {
                            RANKUP = RANK - 1;
                            break;
                        }
                    }

                    RANK = RANK + 1;

                }

            }


        }

        //C***********************************************************************
        //C     A SUBROUTINE TO GENERATE A RANDOM NEIGHBOR OF "PERM"
        //C
        //C      A NEIGHBOR IS A PERMUTATION THAT IS THE SAME AS "PERM" EXCEPT
        //C         that  ONE pair of adjoining ELEMENTs HAS BEEN switched
        //C      ANY switch IS ALLOWED AS LONG AS SPECIES BEGINNINGs STAY
        //C         BEFORE their ENDINGs, and observed coexistances are retained
        //C         if so specified
        //C      THE SCHEME ENSURES THAT THE PROBABILITY OF GOING FROM one PERM TO
        //C         another IS THE SAME AS THE PROBABILITY OF GOING the other way
        //C
        //C                       PROGRAMMERS: BILL KEMPLE & PETE SADLER
        //C                       LAST UPDATE: August 8th 1996
        //C
        //C***********************************************************************
        //C     INPUTS:
        //C         PERM - THE CURRENT PERMUTATION - A (NEVNT INTEGER VECTOR 
        //C             (in the DYNAMIC COMMON)
        //C
        //C         STATIC COMMON
        //C             events are in IROWS
        //C             levels where they were recovered are in ISTATIC
        //C             allowable moves for each event are also in ISTATIC
        //C             weights are in RSTATIC
        //C             data values are cross referenced to levels in VALEVL
        //C
        //C         SETUP COMMON
        //C             the method of getting the initial solution is in INIGEN
        //C             the section to use as a basis, if required, is in JSTART
        //C             the file containing an initial solution to be read in,
        //C                 if required, is in INITSOL
        //C             the initial solution, obtained from GETSTART, is in INIPRM
        //C
        //C             the SA starting temperature is in STARTT
        //C             the SA cooling ratio is in R
        //C             the number of passes, outer SA loop (# temp drops) is NOUTER             
        //C             the number of passes, inner SA loop (constant temp) is NINNER
        //C
        //C             the universal control for range contractions is in CONTRACT
        //C
        //C     OUTPUT:
        //C         IRANK - THE rank of the event to move up 1 (in rank)
        //C             it switches rank with the event currently above it to
        //C             form a NEW CANIDATE PERMUTATION       
        //C
        //C             the actual switching is done in ANNEAL
        //C
        //C             this routine just picks the guy to switch and makes
        //C             sure it's OK
        //C
        //C     SUBROUTINES CALLED: 
        //C
        //C***********************************************************************
        //*     FLIB.FI and FLIB.FD ARE INCLUDED SO WE CAN USE MS ROUTINES LIKE
        //*         RANDOM NUM GEN
        //*     it seems necessary to put the interface include before any other
        //*         statements, even PROGRAM or SUBROUTINE
        //C----------------------------------------------------------------------
        public static void SMLNABR(ref int IRANK)
        {
            int RANKUP, RANKDN;

            RANKUP = 0;
            RANKDN = 0;
            IRANK = 0;

            //C----------------------------------------------------------------------
            //C     get IRANK the rank of the event to move up one rank
            //C----------------------------------------------------------------------
            COMMOD COMMOD9 = COMMOD.Singleton();
           

        Label1000:
            IRANK = COMMOD9.RANDOM.Next(COMMOD9.NEVNT - 1);//HACK:IRANK = INTRAND(NEVNT-1) IN FORTRAN

            //c  retries to meet lag requirements
            if (COMMOD9.FIXF > 0)
            {
                if (COMMOD9.PERM[IRANK] == COMMOD9.XEVNT) goto Label1000;
            }

            if (COMMOD9.FIXF == 3)
            {
                if (COMMOD9.PERM[IRANK + 1] == COMMOD9.XEVNT) goto Label1000;
            }

            //c------testing use of nabrlimt
            RANKUP = IRANK + 1;
            RANKDN = IRANK;

            Helper.NABRLIMT(IRANK, ref RANKUP, ref RANKDN);

            if ((IRANK + 1) > RANKUP) goto Label1000;

            goto Label2000;//HACK: THE FOLLOWING LINES WILL NOT EXECU


            //c-----skip old code that did not use nabrlimt
            //c-----end of test

            //c     consistently passes test
            //c     code from here to end is redundant
            //C----------------------------------------------------------------------
            //C     check whether IRANK and event above are of the same 
            //C     species  --  cannot be switched
            //C----------------------------------------------------------------------
            if ((COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1]) ==
                (COMMOD9.IROWS[COMMOD9.PERM[IRANK + 1], 1])) goto Label1000;

            //C----------------------------------------------------------------------
            //C     check if observed coexistences between fossil events are to be
            //C     retained:  does not matter unless a range end above a
            //C     range beginning,   we can't switch them
            //C----------------------------------------------------------------------
            if (COMMOD9.NOTHR == 0) goto Label2000; //this line seems to belong before next block

            if (((COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1]) == 2) &&
                ((COMMOD9.IROWS[COMMOD9.PERM[IRANK + 1], 1]) == 2))
            {
                if ((COMMOD9.NSPC > 0) && (COMMOD9.COXSTF != 4))
                {
                    if (COMMOD9.COEXST[COMMOD9.IROWS[COMMOD9.PERM[IRANK], 2],
                        COMMOD9.IROWS[COMMOD9.PERM[IRANK + 1], 2]] >= COMMOD9.COXSTF) goto Label1000;
                }
            }


            //C----------------------------------------------------------------------
            //CPMS   check whether a partial ordering will be violated
            //c      violation means either 
            //c         IRANK+1 event is TYPE>2or<1 and must stay above IRANK event
            //c         i.e. PTLORD(IRANK,IRANK+1) is 1 
            if (((COMMOD9.IROWS[COMMOD9.PERM[IRANK + 1], 1] > 2) ||
                (COMMOD9.IROWS[COMMOD9.PERM[IRANK + 1], 1] < 1)) &&
                (COMMOD9.PTLORD[COMMOD9.PERM[IRANK], COMMOD9.PERM[IRANK + 1]] == 1))
            {
                goto Label1000;
            }

            //c   IRANK event is TYPE>2or<1 and must stay below RANK event
            //c   i.e. PTLORD(IRANK+1,IRANK) is 2 
            if (((COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1] > 2) ||
                (COMMOD9.IROWS[COMMOD9.PERM[IRANK], 1] < 1)) &&
                (COMMOD9.PTLORD[COMMOD9.PERM[IRANK + 1], COMMOD9.PERM[IRANK]] == 2))
            {
                goto Label1000;
            }


        Label2000:
            return;

        }

        //C***********************************************************************
        //CPMS     A SUBROUTINE TO GENERATE A RANDOM NEIGHBOR OF "PERM"
        //C
        //C      A NEIGHBOR IS A PERMUTATION THAT IS THE SAME AS "PERM" EXCEPT
        //C      that one random ELEMENT (IRANK) HAS MOVED to a new position (JRANK)
        //C      ANY move IS ALLOWED AS LONG AS SPECIES BEGINNINGs STAY
        //C      BEFORE their ENDINGs, and observed coexistences are retained
        //C      if so specified
        //C      THE SCHEME ENSURES THAT THE PROBABILITY OF GOING FROM one PERM TO
        //C      another IS THE SAME AS THE PROBABILITY OF GOING the other way
        //C
        //CPMS                       PROGRAMMER:  PETE SADLER
        //CPMS                       LAST UPDATE: August 18th 2011  - for Intel ll.1
        //C
        //C***********************************************************************
        //C     INPUTS:
        //C         PERM - THE CURRENT PERMUTATION - A (NEVNT) INTEGER VECTOR 
        //C             (in the DYNAMIC COMMON)
        //C
        //C     OUTPUT:
        //CPMS         IRANK - The rank of the event before the move
        //CPMS
        //CPMS         JRANK - the rank of the event after the move
        //CPMS          
        //C             form a NEW CANIDATE PERMUTATION       
        //C
        //C             the actual switching is done in ANNEAL
        //C
        //CPMS             this routine just picks the event to move
        //CPMS             decides where it moves to
        //CPMS             makes sure the move is OK
        //CPMS
        //CPMS	FIXF = 2  -  "LAG"
        //CPMS         3  -  "FIX"
        //CPMS         4  -  "END"    -     move to opposite end of range
        //CPMS         5  -  "FAR"    -     move to one limit of possible positions
        //C
        //C     SUBROUTINES CALLED: 
        //C
        //CPMS-------------------------------------------------------------------
        public static void BIGNABR(ref int IRANK, ref int JRANK)
        {
            int RANKUP, RANKDN, I, K;
            //CPMS  event at IRANK changes rank;  JRANK is its new rank
            //CPMS  RANKDN and RANKUP are limits on the range of JRANK

            COMMOD COMMOD9 = COMMOD.Singleton();
           
            I = 0;
            K = 0;
            RANKUP = COMMOD9.NEVNT - 1; // highest level
            RANKDN = 0;    // lowest level

            Label1000:
            if ((COMMOD9.NUDGER) && (COMMOD9.NUDGUP || COMMOD9.NUDGDN))
            {
                //c  nudging in direction of last mutation
                //c  choose same event that last moved
                IRANK = COMMOD9.NEWRANK;
            }
            else
            {
                //pick event at random
                IRANK = COMMOD9.RANDOM.Next(COMMOD9.NEVNT);
            }
       

            //c  ----------------------------------------
            //C  for various inefficient/lagged mutations     
            if (((COMMOD9.FIXF == 4) || (COMMOD9.FIXF == 5)) && (COMMOD9.RANDOM.Next(9) != 1))//<=HXD
            {
                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    if ((COMMOD9.PERM[I] == COMMOD9.XEVNT) ||
                        (COMMOD9.PERM[I] == COMMOD9.YEVNT))
                    {
                        if (COMMOD9.RANDOM.Next(2) == 1)//<=HXD
                        {
                            IRANK = I;
                        }
                        else
                        {
                            for (K = I + 1; K < COMMOD9.NEVNT; K++)
                            {
                                if ((COMMOD9.PERM[K] == COMMOD9.XEVNT) ||
                                    (COMMOD9.PERM[K] == COMMOD9.YEVNT))
                                {
                                    IRANK = K;
                                    break;
                                }
                            }
                        }

                        break;
                    }

                }
            }

            if ((COMMOD9.PERM[IRANK] == COMMOD9.XEVNT) &&
                (COMMOD9.FIXF > 0) && (COMMOD9.FIXF < 4)) goto Label1000;

            //CPMS-------------------------------------------------------------------
            //CPMS  NOW find JRANK - the new rank after the move:
            //CPMS  need to limit JRANK to ranks from RANKDN to RANKUP 
            //CPMS  these limits cannot extend beyond
            //CPMS  the other end of a paired event range
            //CPMS  or a coexistence violation
            //CPMS  or a partial ordering violation
            //CPMS  whichever is the closest
            //CPMS      i.e. if IRANK is a FAD (1) or MAX (11)
            //CPMS    RANKUP cannot be above the corresponding LAD|MIN
            //CPMS           if IRANK is a LAD (2) or MIN (12)
            //CPMS    RANKDN cannot be below the corresponding FAD|MAX


            Helper.NABRLIMT(IRANK, ref RANKUP, ref RANKDN);

            //CPMS----find JRANK
            if ((COMMOD9.FIXF == 4) && (COMMOD9.PERM[IRANK] == COMMOD9.XEVNT))
            {
                //move xevnt to yevnt end of range
                if (COMMOD9.IROWS[COMMOD9.XEVNT, 1] == 1) JRANK = RANKUP;
                if (COMMOD9.IROWS[COMMOD9.XEVNT, 1] == 2) JRANK = RANKDN;
            }
            else if ((COMMOD9.FIXF == 4) && (COMMOD9.PERM[IRANK] == COMMOD9.YEVNT))
            {
                //move yevnt away from xevnt as far as possible
                if (COMMOD9.IROWS[COMMOD9.YEVNT, 1] == 1) JRANK = RANKDN;
                if (COMMOD9.IROWS[COMMOD9.YEVNT, 1] == 2) JRANK = RANKUP;
            }
            else if (COMMOD9.FIXF == 5)
            {
                //cpms  move event to one extreme or other
                //cpms  decide which by "coin toss"
                if (COMMOD9.RANDOM.Next(2) == 1)
                {
                    JRANK = RANKUP;
                }
                else
                {
                    JRANK = RANKDN;
                }
            }
            else
            {
                if ((COMMOD9.NUDGER) && (COMMOD9.NUDGUP || COMMOD9.NUDGDN))
                {
                    //if mutating in direction of last mutation
                    if (COMMOD9.NUDGUP)
                    {
                        //option to make another large step
                        JRANK = IRANK + COMMOD9.RANDOM.Next(RANKUP - IRANK);//<=HXD
                    }
                    else if (COMMOD9.NUDGDN)
                    {
                        //option to make another large step
                        JRANK = RANKDN + COMMOD9.RANDOM.Next(IRANK - RANKDN);//<=HXD
                    }
                }
                else
                {
                    //default mutation, may be up or down
                    JRANK = RANKDN + COMMOD9.RANDOM.Next(RANKUP - RANKDN + 1);//<=HXD

                }
            }


            //CPMS***************************************************
            //CPMS----if FIX'd make sure that JRANK and IRANK do
            //CPMS    not straddle XEVNT at XPOSN 
            if (COMMOD9.FIXF == 3)
            {
                if (COMMOD9.PERM[JRANK] == COMMOD9.XEVNT) goto Label1000;
                if ((IRANK > COMMOD9.XPOSN) && (JRANK < COMMOD9.XPOSN)) goto Label1000;
                if ((IRANK < COMMOD9.XPOSN) && (JRANK > COMMOD9.XPOSN)) goto Label1000;
            }

            //CPMS****************************************************************
            //CPMS    Make sure that IRANK and JRANK are indeed different
            //CPMS    if not, start again with new IRANK
            //CPMS    it may be that the original IRANK cannot move up or down!!
            if ((IRANK == JRANK) ||
                ((COMMOD9.IROWS[COMMOD9.PERM[IRANK], 0]) == (COMMOD9.IROWS[COMMOD9.PERM[JRANK], 0])))
            {
                if (COMMOD9.NUDGER)
                {
                    COMMOD9.NUDGUP = false;
                    COMMOD9.NUDGDN = false;
                }

                goto Label1000;
            }

            if (COMMOD9.NUDGER)
            {
                //record direction of mutation---------
                if (JRANK > IRANK) COMMOD9.NUDGUP = true;
                if (JRANK < IRANK) COMMOD9.NUDGDN = true;
                //record position of mutated event-----
                COMMOD9.NEWRANK = JRANK;

            }


        }

        //C***********************************************************************
        //CPMS     A SUBROUTINE TO GENERATE A RANDOM NEIGHBOR OF "PERM"
        //C
        //C      A NEIGHBOR IS A PERMUTATION THAT IS THE SAME AS "PERM" EXCEPT
        //C         that  TWO random ELEMENTs HAVE BEEN switched
        //C      ANY switch IS ALLOWED AS LONG AS SPECIES BEGINNINGs STAY
        //C         BEFORE their ENDINGs, and observed coexistences are retained
        //C         if so specified
        //C      THE SCHEME ENSURES THAT THE PROBABILITY OF GOING FROM one PERM TO
        //C         another IS THE SAME AS THE PROBABILITY OF GOING the other way
        //C
        //CPMS                       PROGRAMMER: PETE SADLER
        //CPMS                       LAST UPDATE: August 18th 2011 - for Intel 11.1
        //C
        //C***********************************************************************
        //CPMS********************************************************************
        //C     INPUTS:
        //C         PERM - THE CURRENT PERMUTATION - A (NEVNT) INTEGER VECTOR 
        //C             (in the DYNAMIC COMMON)
        //C
        //C     OUTPUT:
        //CPMS         IRANK - THE rank of the first random event to move (in rank)
        //CPMS
        //CPMS         JRANK -  second random event
        //CPMS                  it switches rank with IRANK
        //C             form a NEW CANIDATE PERMUTATION       
        //C
        //C             the actual switching is done in ANNEAL
        //C
        //CPMS             this routine just picks the guys to switch and makes
        //CPMS             sure they are OK
        //C
        //C     SUBROUTINES CALLED: 
        //C
        //C***********************************************************************
        //*     FLIB.FI and FLIB.FD ARE INCLUDED SO WE CAN USE MS ROUTINES LIKE
        //*         RANDOM NUM GEN
        //*     it seems necessary to put the interface include before any other
        //*         statements, even PROGRAM or SUBROUTINE
        //CPMS-------------------------------------------------------------------
        public static void TWONABR(ref int IRANK, ref int JRANK)
        {
            int KRANK, RANK, RANKDN, RANKUP;
            int TYPER;

            COMMOD COMMOD9 = COMMOD.Singleton();
           

            RANKDN = 0;
            RANKUP = 0;
            IRANK = 0;
            JRANK = 0;
            KRANK = 0;
            RANK = 0;
            TYPER = 0;

            //C----------------------------------------------------------------------
        //CPMS   get IRANK the rank of the event to move up
        //CPMS-------------------------------------------------------------------
        Label1000:
            IRANK = COMMOD9.RANDOM.Next(COMMOD9.NEVNT);
            if (COMMOD9.FIXF > 0)
            {
                if (COMMOD9.PERM[IRANK] == COMMOD9.XEVNT) goto Label1000;
            }

            RANKUP = COMMOD9.NEVNT - 1;
            RANKDN = 1 - 1;

            Helper.NABRLIMT(IRANK, ref RANKUP, ref RANKDN);

            //cpms  make sure event at Irank can move
            //cpms  if not, pick another IRANK
            if (RANKUP == RANKDN) goto Label1000;

//cpms  fix JRANK within limits
        //cpms  for more FIXF options see BIGNABR    
        Label2000:
            JRANK = RANKDN + COMMOD9.RANDOM.Next(RANKUP - RANKDN + 1) - 1;
            if (IRANK == JRANK) goto Label2000;

            //CPMS*******************************************************************
            //CPMS    make sure that JRANK is permitted to move
            if (COMMOD9.FIXF == 3)
            {
                if (COMMOD9.PERM[JRANK] == COMMOD9.XEVNT) goto Label1000;
            }

            //CPMS    make sure that IRANK and JRANK are indeed different
            //CPMS    if not, start again with new IRANK
            //CPMS    it may be that IRANK cannot move up or down
            //CPMS    because nothing in its range is permitted to swap
            //CPM     so must always look for new IRANK, not new JRANK
            if (IRANK == JRANK) goto Label1000;
            if (COMMOD9.FIXF > 0)
            {
                if (COMMOD9.PERM[JRANK] == COMMOD9.XEVNT) goto Label1000;
            }

            //CPMS******************************************************************** 
            //CPMS  now check all RANKS from JRANK to IRANK to find largest
            //CPMS  switch that will not violate COEXST or PTLORD
            //CPMS  check whether switch will cross JRANK with its partner
            //CPMS  or violate ordering constraints;

            //CPMS####WHY NOT USE NABRLIMT FOR JRANK?####  
            RANKUP = Math.Max(IRANK, JRANK);
            RANKDN = Math.Min(IRANK, JRANK);
            Helper.NABRLIMT(JRANK, ref RANKUP, ref RANKDN);
            if (JRANK < IRANK)
            {
                //CPMS   CASE 1. JRANK moves up 
                //CPMS   NOT OK IF RANKUP lower than IRANK
                if (IRANK > RANKUP) goto Label1000;
            }
            else if (JRANK > IRANK)
            {
                //CPMS  CASE 2. JRANK moves down 
                //CPMS  NOT OK IF RANKDN higher than IRANK 
                if (IRANK < RANKDN) goto Label1000;
            }

            goto Label3000;

            //CPMS###################################      

            if (JRANK < IRANK)
            {
                //CPMS  CASE 1. JRANK moves up 
                //CPMS  no need to worry about LADs, except relative to unpaireds    
                for (RANK = JRANK; RANK < IRANK; RANK++)//HACK:DO RANK=JRANK, IRANK, +1 IN FORTRAN
                {
                    JRANK = RANK;
                    TYPER = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];
                    for (KRANK = RANK + 1; KRANK < IRANK; KRANK++)//HACK:DO KRANK=RANK+1, IRANK, +1 IN FORTRAN
                    {
                        if ((TYPER == 1) || (TYPER == 3))
                        {
                            if (COMMOD9.IROWS[COMMOD9.PERM[KRANK], 0] ==
                                COMMOD9.IROWS[COMMOD9.PERM[RANK], 0])
                            {
                                //RANK FAD would cross its MID or LAD; MID would cross its LAD 
                                JRANK = RANK + 1;
                                break;
                            }
                            else if ((COMMOD9.COXSTF != 4) && (COMMOD9.IROWS[COMMOD9.PERM[KRANK], 1] == 2))
                            {
                                if (COMMOD9.COEXST[COMMOD9.IROWS[COMMOD9.PERM[RANK], 2],
                                    COMMOD9.IROWS[COMMOD9.PERM[KRANK], 2]] >= COMMOD9.COXSTF)
                                {
                                    //RANK would violate a coexistence 
                                    JRANK = RANK + 1;
                                    break;
                                }
                            }
                            else if (COMMOD9.NOTHR > 0)
                            {
                                if ((COMMOD9.IROWS[COMMOD9.PERM[KRANK], 1] > 2) ||
                                    (COMMOD9.IROWS[COMMOD9.PERM[KRANK], 1] < 1))
                                {
                                    if (COMMOD9.PTLORD[COMMOD9.PERM[RANK], COMMOD9.PERM[KRANK]] == 1)
                                    {
                                        //JRANK violates a partial order
                                        JRANK = RANK + 1;
                                        break;
                                    }
                                }
                            }
                        }
                        else if ((TYPER > 2) || (TYPER < 1))
                        {
                            if (COMMOD9.PTLORD[COMMOD9.PERM[KRANK], COMMOD9.PERM[KRANK]] == 2)
                            {
                                //JRANK violates a partial order
                                JRANK = RANK + 1;
                                break;
                            }
                        }
                    }//End for KRANK

                    if (JRANK == RANK)
                    {
                        //current RANK is switchable; accept
                        break;
                    }
                }//End for RANK
            }
            else if (JRANK > IRANK)
            {
                //CPMS  CASE 2. JRANK moves down
                //CPMS  no need to worry about FADs, except relative to unpaireds 
                for (RANK = JRANK; RANK > IRANK; RANK--)//HACK:DO RANK=JRANK, IRANK, -1 IN FORTRAN
                {
                    JRANK = RANK;
                    TYPER = COMMOD9.IROWS[COMMOD9.PERM[RANK], 1];
                    for (KRANK = RANK - 1; KRANK > IRANK; KRANK--)//HACK:DO KRANK=RANK-1, IRANK, -1 IN FORTRAN
                    {
                        if ((TYPER == 2) || (TYPER == 3))
                        {
                            if (COMMOD9.IROWS[COMMOD9.PERM[KRANK], 0] ==
                                COMMOD9.IROWS[COMMOD9.PERM[RANK], 0])
                            {
                                //RANK LAD would cross its FAD or MID; MID would cross FAD  
                                JRANK = RANK - 1;
                                break;
                            }
                            else if ((COMMOD9.COXSTF != 4) && (COMMOD9.IROWS[COMMOD9.PERM[KRANK], 1] == 1))
                            {
                                if (COMMOD9.COEXST[COMMOD9.IROWS[COMMOD9.PERM[RANK], 2],
                                    COMMOD9.IROWS[COMMOD9.PERM[KRANK], 2]] >= COMMOD9.COXSTF)
                                {
                                    //RANK would violate a coexistence
                                    JRANK = RANK - 1;
                                    break;
                                }
                            }
                            else if (COMMOD9.NOTHR > 0)
                            {
                                if ((COMMOD9.IROWS[COMMOD9.PERM[KRANK], 1] > 2) ||
                                    (COMMOD9.IROWS[COMMOD9.PERM[KRANK], 1] < 1))
                                {
                                    if (COMMOD9.PTLORD[COMMOD9.PERM[RANK], COMMOD9.PERM[KRANK]] == 2)
                                    {
                                        //RANK violates a partial order
                                        JRANK = RANK - 1;
                                        break;
                                    }
                                }
                            }
                        }
                        else if ((TYPER > 3) || (TYPER < 1))
                        {
                            if (COMMOD9.PTLORD[COMMOD9.PERM[KRANK], COMMOD9.PERM[RANK]] == 1)
                            {
                                //JRANK violates a partial order
                                JRANK = RANK - 1;
                                break;
                            }
                        }
                    }//End for KRANK

                    if (JRANK == RANK)
                    {
                        //current RANK is switchable; accept
                        break;
                    }

                }//End for RANK

            }


            //CPMS--------------------------------------------------------------------
            //C     make sure that IRANK and JRANK are indeed different
            //C     and not equal fo XEVNT 
            //C----------------------------------------------------------------------

            if (IRANK == JRANK) goto Label1000;

            if (COMMOD9.FIXF == 3)
            {
                if (COMMOD9.PERM[JRANK] == COMMOD9.XEVNT) goto Label1000;
            }

        Label3000:

            return;

        }

        //C***********************************************************************
        //C     A SUBROUTINE TO STEP TO NEXT POSITION IN NEIGHBORHOOD
        //C
        //C                       PROGRAMMER: PETE SADLER
        //C                       LAST UPDATE: August 18th 2011 - fir Intel 11.1
        //C
        //C***********************************************************************
        //c     This block of code is used frequently,
        //c     so it is separated as a subroutine
        //C***********************************************************************
        public static void GONABR(ref int IRANK, ref int JRANK)
        {
            int RANK, ITEMP;
            RANK = 0;
            ITEMP = 0;

            IRANK = 0; //sends out to SMLNABR etc for values, then applies to PERM
            JRANK = 0; //sends out to SMLNABR etc for values, then applies to PERM

            //C   GENERATE A NEIGHBOR 
            //C   send out for mutation and make the changes in PERM(). 
            //CPMS--------------------------------------------------------------------
            //CPMS   CASE 1 of 3: SMALL NEIGHBORHOOD SIZE
            //C             SMLNABR returns IRANK, the rank in PERM for the event that 
            //C             is to move up 1 rank. the event currently assigned that 
            //C             rank is assigned IRANK (one less than its old assignment)

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (COMMOD9.NABRGEN == 2)
            {
                //all small moves are UP    

                Helper.SMLNABR(ref IRANK);
                JRANK = IRANK + 1;
                Helper.CopyArray(COMMOD9.PERM, ref COMMOD9.LSTPERM);
                ITEMP = COMMOD9.PERM[IRANK];
                COMMOD9.PERM[IRANK] = COMMOD9.PERM[IRANK + 1];
                COMMOD9.PERM[IRANK + 1] = ITEMP;
            }

            //CPMS---------------------------------------------------------------------
            //CPMS   CASE 2 of 3: BIG NEIGHBORHOOD SIZE

            if (COMMOD9.NABRGEN == 1)
            {
#if FAKE_IJRANKE_DOUBLE
                WritePara.getIJRand(ref IRANK, ref JRANK);
#else
                Helper.BIGNABR(ref IRANK, ref JRANK);
#endif
             

                Helper.CopyArray(COMMOD9.PERM, ref COMMOD9.LSTPERM);
                RANK = IRANK;
                ITEMP = COMMOD9.PERM[IRANK];
                //if IRANK moves up, move leapfrogged events down 
               

                if (IRANK < JRANK)
                {
                    while (RANK < JRANK)
                    {
                        COMMOD9.PERM[RANK] = COMMOD9.PERM[RANK + 1];
                        RANK = RANK + 1;
                    }
                }
                //if IRANK moves down, move leapfrogged events up
                else if (IRANK > JRANK)
                {
                    while (RANK > JRANK)
                    {
                        COMMOD9.PERM[RANK] = COMMOD9.PERM[RANK - 1];
                        RANK = RANK - 1;
                    }
                }


                //move IRANK to new position                       
                COMMOD9.PERM[JRANK] = ITEMP;

            }

            //CPMS---------------------------------------------------------------------
            //CPMS   CASE 3 of 3: SMALL NEIGHBORHOOD SIZE
            if (COMMOD9.NABRGEN == 3)
            {
                Helper.TWONABR(ref IRANK, ref JRANK);

                //one moves up, other down - no NUDGing

                Helper.CopyArray(COMMOD9.PERM, ref COMMOD9.LSTPERM);
                ITEMP = COMMOD9.PERM[IRANK];
                COMMOD9.PERM[IRANK] = COMMOD9.PERM[JRANK];
                COMMOD9.PERM[JRANK] = ITEMP;
            }


        }
               

        //C***********************************************************************
        //C     A SUBROUTINE TO find section locations that minimize the penalty
        //C         after a permutation has been changed. 
        //C
        //C     Unlike GETPEN, it takes advantage of previous 
        //C     optimizations of the horizon placements when
        //C        1. changes in sequence involve events that are not observed
        //c           - in BIG and SML neighborhoods, only 1 event changes relative position
        //c           - in DBL neighborhoods, 2 events change position relative to others
        //C        2. changes do not reach the bottom of the section
        //c           - the optimization/calculation starts at the lowest of the moving events
        //C
        //C                        PROGRAMMER: PETE SADLER
        //C                       LAST UPDATE: Aug 18 2011 for Intel 11.1
        //C
        //C     INPUTS:
        //C         the given permutation is passed in as HPERM
        //C
        //C             events are in IROWS
        //C             levels where they were recovered are in ISTATIC
        //C             allowable moves for each event are also in ISTATIC
        //C             weights are in RSTATIC
        //C             data values are cross referenced to levels in VALEVL
        //C
        //C             the universal control for range contractions is in CONTRACT
        //C             DOINPUT already adjusted ISTATIC accordingly
        //C
        //C     OUTPUTS: 
        //C         the penalty for HPERM is returned in HPEN
        //C         the event locations within sections are updated in HSCTSOL
        //C       SCTSOL is the last accepted solution
        //C       HSCTSOL is the current candidate solution
        //C       in SCTSOL and HSCTSOL, rows match rows in IROWS,
        //C        one plane per section gives placed levels
        //C       i.e. these are not strat sections  they are event placement levels
        //C       IRNK and JRNK refer to taxa that move in the current sequence
        //C       so, given IRNK and JRNK we must use (H)SCTSOL to find levels in section
        //C                                       and IROWS to find taxa
        //C       i.e. arrays may use composite sequence order e.g.  *PERM
        //C                        or event dictionary order  e.g. ?SCTSOL
        //C                        or local section order    e.g. ISTATIC
        //C
        //C     SUBROUTINES CALLED: 
        //C         the penalty within a section is found using SCTPEN
        //C***********************************************************************
        //CPMS  CTRGET - initially set to 0
        //CPMS         - set to 1 for any CTRSECT = 1,
        //CPMS         - reported via CTRF from SCTPEN
        //CPMS         - finally reported back as CTRF
        //C----------------------------------------------------------------------
        public static void NEWPEN(int IRNK, int JRNK, int[] HPERM, ref double HPEN)
        {
            double PENJ;
            int JOPT, CTRGET, I;
            int TRNK;

            //C***********************************************************************
            //C     note that NEWPEN is not generic. 
            //C     it needs problem specific info, so it has access to the commons
            //C     but it passes info with the calling routine (initially ANNEAL)
            //C     in the CALL statement
            //C**********************************************************************
            //C   HPERM(NEVNT) IS a VECTOR OF events (solution)
            //C         similar to the dynamic INTEGER variable PERM
            //C
            //C         A PROPER ORDERING OF THESE NEVNT EVENTS IS ONE OVERALL
            //C            OBJECTIVE OF THE CORRELATION
            //C----------------------------------------------------------------------
            //C   THE OVERALL MINIMUM (THE PENALTY) FOR A GIVEN PERMUTATION CAN
            //C      BE FOUND BY MINIMIZING WITHIN EACH SECTION AND ADDING
            //C         ACROSS SECTIONS
            //C----------------------------------------------------------------------
            //C   AS THE SOLUTION IS DEVELOPED, THE estimated levels for 
            //C         THE OBSERVED EVENTS ARE STORED IN AN ARRAY NAMED HSCTSOL
            //C            EACH ROW OF HSCTSOL REPRESENTS AN OBSERVED EVENT IN THE
            //C               ORDER SET BY IROWS                                     
            //C----------------------------------------------------------------------
            //C      SUBROUTINE RESCTPEN finds 
            //C         OPTIMUM PLACEMENTS (levels) IN SECTION JOPT 
            //C             FOR THE OBSERVED EVENTS (in the array HSCTSOL)
            //C                 and returns the penalty due to section JOPT (in PENJ)
            //C
            //CPMS   RESCTPEN resembles SCTPEN, but uses some previously 
            //CPMS   optimized horizon placements that fall outside the influence 
            //CPMS   of the new move.  It needs MIN(IRNK,JRNK). Disturbance
            //CPMS   is limited to events placed at the same level as MIN(IRNK,JRNK)
            //CPMS   or higher.
            //c
            //CPMS   UNLIKE NWDMPEN, this recalculation sets HPEN to zero;  the speed is 
            //c      gained from using the old placements, not the old penalty
            //c      DEMPEN and NWDMPEN do not make placements
            //C
            //C   COMPUTE THE PENALTY FOR HPERM
            //C
            //C----------------------------------------------------------------------

            COMMOD COMMOD9 = COMMOD.Singleton();

            COMMOD9.CTRF = 0;
            CTRGET = 0;
            JOPT = 0;
            TRNK = 0;
            I = 0;
            HPEN = 0.0;
            COMMOD9.NGHPEN = 0.0;
            PENJ = 0.0;

            for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
            {
                //C      -----------------------------------------------------
                //C      PLACE EVENTS AND CALCULATE PENALTY FOR SECTION "JOPT"
                //C      ---------------------------------------------------------
                //cpms   check whether events that changed are observed in section
                //cpms   if not, there is no penalty change and all other events 
                //cpms   remain as previously optimized - no need to recalculate
                //cpms   IF negative evidence is to be used for penalties,
                //cpms   THEN there is no free move for unobserved events.
                //cpms   -------------------------------------------------

                if ((COMMOD9.ISTATIC[COMMOD9.LSTPERM[IRNK], JOPT, 0] != -1)//<=HXD
                    || (COMMOD9.NEGATF != 0))
                {
                    //c         ! Irnk taxon in section; must re-optimize/calculate
                    //cpms     -------------------------------------------------------     
                    //cpms     THIS BLOCK IS FOR STACKING UP SECTION (comment one out)
                    //cpms     find lowest of the ranks, in permutation that switch occupants
                    TRNK = Math.Min(IRNK, JRNK);
                    //cpms     find highest undisturbable level in JOPT
                    //cpms     it will be at the first level below RNK
                    if (TRNK > 0)//<=HXD
                    {
                        while (COMMOD9.SCTSOL[COMMOD9.LSTPERM[TRNK], JOPT]
                            >= (COMMOD9.SCTSOL[COMMOD9.LSTPERM[Math.Min(IRNK, JRNK)], JOPT] - 1))//<=HXD
                        {
                            if (TRNK == 0) break;//<=HXD

                            TRNK = TRNK - 1;
                        }
                    }

                    //c     recalculate penalty from TRNK to MAX(IRNKorJRNK)
                    //CPMS  comment next line out when using cdebug lines below: 
                    Helper.RESCTPEN(TRNK, Math.Max(IRNK, JRNK), HPERM, JOPT, ref PENJ);
                    //if (COMMOD9.NXTPEN < 3900)
                    //{
                    //    Helper.Write("  Penj={0}\n", PENJ);
                    //}
                }
                else if ((COMMOD9.NABRGEN < 3))
                {
                    //c   BIG or SML neighborhood; 
                    //c   only Irnk taxon needs consideration, others keep relative position
                    //c   Irnk taxon not seen; no new sectpen to calculate
                    for (I = 0; I < COMMOD9.NEVNT; I++)
                    {
                        //repeat old section solution 
                        COMMOD9.HSCTSOL[I, JOPT] = COMMOD9.SCTSOL[I, JOPT];
                    }

                    //c  make the level switch  
                    COMMOD9.HSCTSOL[COMMOD9.LSTPERM[IRNK], JOPT] =
                        COMMOD9.HSCTSOL[COMMOD9.LSTPERM[JRNK], JOPT];

                    //c  keep old penalty    
                    PENJ = COMMOD9.SCJPEN[JOPT];
                }
                else if ((COMMOD9.NABRGEN == 3) &&
                (COMMOD9.ISTATIC[COMMOD9.LSTPERM[JRNK], JOPT, 0] == -1))//<=HXD
                {
                    //c   DBL!  both Irnk and Jrnk taxa change relative position
                    //c   but neither Irnk nor Jrnk taxa seen; no new sectpen to calculate
                    for (I = 0; I < COMMOD9.NEVNT; I++)
                    {
                        //repeat old section solution
                        COMMOD9.HSCTSOL[I, JOPT] = COMMOD9.SCTSOL[I, JOPT];
                    }

                    //make two level switches, for same penalty        
                    TRNK = COMMOD9.HSCTSOL[COMMOD9.LSTPERM[IRNK], JOPT];
                    COMMOD9.HSCTSOL[COMMOD9.LSTPERM[IRNK], JOPT] =
                        COMMOD9.HSCTSOL[COMMOD9.LSTPERM[JRNK], JOPT];
                    COMMOD9.HSCTSOL[COMMOD9.LSTPERM[JRNK], JOPT] = TRNK;

                    //Keep old penalty 
                    PENJ = COMMOD9.SCJPEN[JOPT];

                }
                else
                {
                    //cpms   only remaining possibility is that NABRGEN.EQ.3,
                    //cpms   DBL! although Irnk taxon not seen, Jrnk taxon is seen
                    //cpms   must recalculate because Jrnk taxon changes relative position
                    //cpms   same exercise as first IF but with J taxon active
                    //cpms     -------------------------------------------------------     
                    //cpms     THIS BLOCK IS FOR STACKING UP SECTION (comment one out)
                    //cpms     find lowest rank that moves
                    TRNK = Math.Min(IRNK, JRNK);
                    //cpms     find highest undisturbable rank
                    //cpms     it will be at the first level below RNK
                    if (TRNK > 0)//<=HXD
                    {
                        while (COMMOD9.SCTSOL[COMMOD9.LSTPERM[TRNK], JOPT]
                            >= (COMMOD9.SCTSOL[COMMOD9.LSTPERM[Math.Min(IRNK, JRNK)], JOPT] - 1))
                        {
                            if (TRNK == 0) break;//<=HXD

                            TRNK = TRNK - 1;
                        }
                    }

                    Helper.RESCTPEN(TRNK, Math.Max(IRNK, JRNK), HPERM, JOPT, ref PENJ);

                    //ccpms     ---------------------------------------------------------  
                    //ccpms     THIS BLOCK IS FOR STACKING DOWN SECTION (comment one out)
                    //ccpms     find HIGHEST rank that moves
                    //c         TRNK=MAX(IRNK,JRNK)
                    //ccpms     find LOWEST undisturbable rank
                    //ccpms     it will be at the first level ABOVE RNK
                    //c         DO WHILE ((TRNK.LT.NEVNT).AND.(SCTSOL(LSTPERM(TRNK),JOPT)
                    //c     1            .LE.SCTSOL(LSTPERM(MAX(IRNK,JRNK)),JOPT)-1))
                    //c            TRNK=TRNK+1
                    //c         END DO 
                    //c         CALL RESCTPEN(TRNK,MIN(IRNK,JRNK),HPERM,JOPT,PENJ)
                    //cpms     -----------------------------------------------------  
                }


                if (COMMOD9.CTRF == 1) CTRGET = 1;

                //C------------------------------------------------
                //C         ACCUMULATE THE SECTION PENALTIES TO GET 
                //C         THE OVERALL PERMUTATION PENALTY
                HPEN = HPEN + PENJ;

                //cpms      determine whether the cost is already too high
                //c         HPEN has been growing with each section
                //c         CRTPEN is  the critical threshold at which anneal abandons mutation
                //c         CRTPEN was set in anneal.for using random number UU

                if (HPEN >= COMMOD9.CRTPEN)
                {                    
                    break;
                }

                //C-----   for DOOUTPUT------- 
                COMMOD9.COLPEN[JOPT] = PENJ;

            }

            //CPMS-------------------------------------------------------------
            if (COMMOD9.JSPANF == 1)
            {
                Helper.JSPAN(HPERM);
                HPEN = HPEN + COMMOD9.SPANPEN;
            }
            //CPMS-------------------------------------------------------------

            if (COMMOD9.PEN2F > 0) { Helper.NEW2PEN(IRNK, JRNK, HPERM, ref HPEN, 0); }

            COMMOD9.CTRF = CTRGET;


        }

        //C***********************************************************************      
        //C                       PROGRAMMERS: PETE SADLER & BILL KEMPLE
        //C                       LAST UPDATE: 16 August 2011 - for Intel 11.1
        //C
        //C   EACH TIME A PERMUTATION IS FIXED, EVENTS ARE
        //C      PLACED AND THE TOTAL PENALTY IS COMPUTED FOR EACH SECTION.
        //C   THIS SUBROUTINE COMPUTES THE MINIMUM PENALTY THAT CAN BE ACHIEVED
        //C      IN SECTION "JOPT" AND STILL COMPLY WITH PERMUTATION "HPERM"
        //C
        //C   UNLIKE SCTPEN, IT MAKES USE OF THE PREVIOUS OPTIMIZATION OF HORIZONS
        //C          SCTPEN can start from scratch, i.e. find initial penalty
        //C        RESCTPEN can only be used after an initial run with SCTPEN
        //C      INTENT(IN) :: RNK gives lower limit for recalculation
        //C                 RESCTPEN deals with undisturbed base before entering big loop
        //C      INTENT(IN) :: RNK2 gives upper limit for recalculation
        //C                 RESCTPEN deals with undisturbed top at and of big loop
        //C                 i.e. loop from RNK to NEVNT but look of RNK2 at each loop
        //C     REAL VARIABLES:
        //C         LPEN - array giving penalty at each level of section JOPT
        //C                 updated as events are placed
        //C         PENJ - THE TOTAL PENALTY FOR SECTION JOPT DUE TO COMPLYING
        //C                 WITH "HPERM"
        //C     INTEGER VARIABLES:
        //CPMS    HSCTSOL - solution as horizon placements;  
        //CPMS              built piecemeal abovelevel of no-disturbance
        //CPMS       RNK  - highest undisturbed horizon placement
        //C         ITGT  - level of current event's datum in section JOPT
        //C             special cases:
        //C                  0 - no data
        //C                 -1 - range extends up out of section
        //C                 -2 - range extends down out of section
        //C         LPREV - level in section JOPT where last event was placed
        //C         INOW  - rank in HPERM of current event (starting at lowest)
        //C         HPERM(INOW) - row number in data arrays for current event    
        //C         PLACED - flag set to 0 before each event is placed
        //C                       set to 1 when current event has been placed 
        //CPMS      CTRF   - flag set to 1 if any range contractions are accepted
        //CPMS             - EPEN sets the flag if LARGE is used
        //CPMS             - SECTPEN must reset flage to 0 if
        //CPMS             - than particular EPEN is not used
        //CPMS             - many event moves terminate when LARGE is used
        //C***********************************************************************
        /// <summary>
        /// 
        /// </summary>
        /// <param name="RNK"> 
        /// RNK and RNK2 come in as events to switch but are used to find
        /// limits of disturbable zone; i.e. changed in subroutine.
        /// IRANK and JRANK are kept separate, so no bad feedback to caller
        ///</param>
        /// <param name="RNK2"></param>
        /// <param name="HPERM">the trial permutation</param>
        /// <param name="JOPT">the given section</param>
        /// <param name="PENJ">the new penalty</param>
        public static void RESCTPEN(int RNK, int RNK2, int[] HPERM, int JOPT, ref double PENJ)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            double[] LPEN = new double[COMMOD9.MXLVL];
            double PENDOWN, PENHERE;
            int DOWN, HERE, INOW, ITGT, LPREV, PLACED, THISONE;
            int CTRSECT, CTRHERE, CTRDOWN;
            int I, RR;

            //C***********************************************************************
            //C   PLACE THE OBS EVENTS TO MINIMIZE THE TOTAL PENALTY IN SECTION JOPT
            //C   AND COMPUTE THE TOTAL PENALTY FOR THIS SECTION - PENJ
            //C         ITGT is the level in section JOPT that the current event
            //C             would be placed at if no other events were in the way
            //C         if the event has a datum in JOPT, the level of the datum 
            //C             is ITGT
            //C         there are three special cases:
            //C             if the event has no datum in JOPT, ITGT = 0
            //C             if the event is the top of a range that extends 
            //C                 up out of JOPT, the level is coded -1, and 
            //C                 ITGT is the highest level in the section
            //C                 (HLEVEL(JOPT))
            //C             if the event is the bottom of a range that extends 
            //C                 down out of JOPT, the level is coded -2, and 
            //C                 ITGT is 1
            //CPMS     INOW - loop counter
            //CPMS  THISONE - temporary INOW
            //CPMS     HERE - temporary LPREV
            //CPMS     DOWN - level below HERE
            //CPMS  PENDOWN - sum of EPEN's 
            //C----------------------------------------------------------------------
            COMMOD9.CTRF = 0;
            CTRSECT = 0;
            CTRHERE = 0;
            CTRDOWN = 0;
            I = 0;
            RR = 0;
            ITGT = 0;
            HERE = 0;
            DOWN = 0;
            THISONE = 0;
            PLACED = 0;
            PENJ = 0.0;
            //CPMS  zero out the array of level penalties:  
            Helper.SetVal(LPEN, 0.0);
            PENDOWN = 0.0;
            PENHERE = 0.0;
            //c     start at base level
            INOW = 0;
            LPREV = 0;

            //C**********************************************************************
            //C     start the overall loop 
            //C         - finding placements for all events in one section
            //C     one pass through this loop for each event in the data set.
            //C         we start with the event with RANK 1 in HPERM and place the
            //C             events in order of their rank
            //C
            //C         as each event is placed, others are moved, as necessary, so
            //C             that the overall penalty for everything placed thus far 
            //C             is minimized
            //C
            //C         first we find the target in this section for the event 
            //C
            //C         then we find the placement (for this event and any others 
            //C             that must move) that minimizes the overall penalty thus far
            //C          
            //C         the penalty for a placement of an event is based on the 
            //C             distance from, and position relative to, its target
            //C
            //C         the placement must also consider any additional penalties that
            //C             result from moving other events to accomodate placing this
            //C             one (the order must be retained) and any constraints 
            //C
            //CPMS------------------------------------------------------------------
            //CPMS     first adopt those previously optimized placements
            //CPMS     that are below any action started here.  I's placement
            //CPMS     cannot affect events previously placed below its old or
            //CPMS     new level, whichever is the lower
            //CPMS     if they wanted to move up, they would have
            //CPMS     if they need to move farther down, they can get pushed down later

            if (RNK > 0)//<=HXD
            {
                //CPMS    loop through all events below disturbable zone
                for (INOW = 0; INOW <= RNK; INOW++)//<=HXD
                {
                    //CPMS  determine level in last solution        
                    LPREV = COMMOD9.SCTSOL[COMMOD9.LSTPERM[INOW], JOPT];
                    //CPMS  assign to same level as before          
                    COMMOD9.HSCTSOL[COMMOD9.LSTPERM[INOW], JOPT] = LPREV;
                    //CPMS  acrue same penalty as before        
                    //CPMS  if INOW is observed in this section
                    if (COMMOD9.ISTATIC[HPERM[INOW], JOPT, 0] > -1)//<=HXD
                    {
                        LPEN[LPREV] = LPEN[LPREV] + Helper.EPEN(HPERM[INOW], LPREV, JOPT);
                    }
                    else if (COMMOD9.NEGATF > 0)
                    {
                        LPEN[LPREV] = LPEN[LPREV] + Helper.NEGPEN(HPERM[INOW], LPREV, JOPT);

                    }
                }

                //CPMS    ensure that HERE is set; CASE C below may not arise!
                //CPMS    HERE is used for final totalling of penalty
                HERE = LPREV;
            }
            else
            {
                RNK = -1;//<=HXD
            }


            for (INOW = RNK + 1; INOW < COMMOD9.NEVNT; INOW++)
            {
                //CPMS     loop through all remaining events 
                //C----------------------------------------------------------------------
                //C        find the target for this event
                //CPMS     look up event level in section J
                //CPMS     0=not observed    >0 gives observed level, so store to target
                //C----------------------------------------------------------------------
                //CPMS     NOT IMPLEMENTED
                //CPMS     -1 means extends up out of section, highest level in section is target
                //CPMS     -2 means extends down out of section, level 1 is target
                //c           IF(ISTATIC(HPERM(INOW), JOPT, 1).GE.0) THEN
                //c               ITGT = ISTATIC(HPERM(INOW), JOPT, 1)
                //c           ELSE IF(ISTATIC(HPERM(INOW), JOPT, 1).EQ.-1) THEN    
                //c               ITGT = HLEVEL(JOPT)
                //c           ELSE IF(ISTATIC(HPERM(INOW), JOPT, 1).EQ.-2) THEN    
                //c               ITGT = 1
                //c           END IF             
                //C----------------------------------------------------------------------

                ITGT = COMMOD9.ISTATIC[HPERM[INOW], JOPT, 0];

                //CPMS##########################################
                //cpms######GIVING A TARGET TO MISSING EVENTS###
                //cpms######CAUSES SEARCH TO STALL##############
                //cpms######FADs FREEZE FIRST###################
                //cpms      if negative evidence is also a basis for penalties,
                //cpms      set the target at the end of the section that penalizes 
                //cpms      for arbitrarily long ranges;  i.e. the target for a FAD
                //cpms      is the section top  --  get it out of sections that are   
                //cpms      too old.  In sections that are too young, the LAD
                //cpms      must do the work of keeping both FAD and LAD lower.
                //*        IF((NEGATF.gt.0).and.(ITGT.eq.0)) THEN
                //*          IF(IROWS(HPERM(INOW),2).eq.1) ITGT = HLEVEL(JOPT)
                //cpms           A FAD  -- target is base or top, whichever is nearest
                //cpms           to a range end
                //*          IF(IROWS(HPERM(INOW),2).eq.2) ITGT = 1
                //cpms	         A LAD --  target is the section base
                //*	    ENDIF
                //CPMS#################################

                if (ITGT == -1)//<=HXD
                {
                    //CPMS   CASE A. NO datum this event this section, PUT IT AT LPREV
                    //CPMS   if not observed, place at highest previously occupied level
                    //CPMS   i.e. tied with last of preceding events; no penalty
                    //CPMS   unless USE_NEGATIVE is activated
                    COMMOD9.HSCTSOL[HPERM[INOW], JOPT] = LPREV;

                    //CPMS  ### CHECK FOR NEGATIVE PENALTIES ##################
                    if (COMMOD9.NEGATF != 0)
                    {
                        LPEN[LPREV] = LPEN[LPREV] + Helper.NEGPEN(HPERM[INOW], HERE, JOPT);
                    }
                }
                else if (LPREV <= ITGT)
                {
                    //CPMS  CASE B. last placed is below this event's datum,
                    //CPMS  PUT IT AT ITS datum AND UPDATE LPREV 
                    //CPMS  i.e. target is above highest used event
                    //CPMS  so place at target with no penalty;  update LPREV
                    //CPMS  order is preserved; later placements lower the placement
                    COMMOD9.HSCTSOL[HPERM[INOW], JOPT] = ITGT;
                    LPREV = ITGT;

                }
                else
                {
                    //CPMS    CASE C. the hard one, one or more events are placed above this 
                    //C       event's datum. FIND THE LOCATION WITH THE SMALLEST 
                    //C       COMBINED PENALTY
                    //CPMS    NOT ELSEIF, because
                    //CPMS    only remaining option - placement of earlier events has already 
                    //CPMS    extended to horizons higher than target.  IF INOW is a LAD,
                    //CPMS    extension penalty arises; if an FAD, contraction penalty arises
                    //CPMS    later try to reduce penalty by moving whole group down.
                    //C       place this event (row HPERM(INOW) in the arrays)
                    //C 
                    //C       AT LPREV AND MOVE it down, 
                    //C       TAKING OTHERS ALONG AS NECESSARY TO PRESERVE HPERM ORDER
                    //*         1. calc pen for all at LPREV, including current
                    //*         2. calc pen for all if moved to LPREV - 1
                    //*         3. if pen moved down is less, decrement LPREV and repeat
                    //*           (temp variables THISONE and HERE are used for 
                    //*            INOW and LPREV so they can be decremented without 
                    //*            losing the current values. 
                    //*            DOWN is the level below HERE)
                    //*
                    //*           LPEN is used to hold the penalty increment at each
                    //*               level in the current section (sum of the penalties
                    //*               for all events placed at this level
                    //*           note that only Case C. placements will cause penalties

                    //C---- INITIAL SETUP -----------------------------------------
                    //C         place current event at LPREV, set some temp variables, 
                    //C         compute the penalty at LPREV, and set a flag  
                    //C         saying the event has not been placed yet
                    //C------------------------------------------------------------
                    COMMOD9.HSCTSOL[HPERM[INOW], JOPT] = LPREV;
                    //CPMS    add INOW to developing horizon solution
                    //CPMS    set up temporary variables to decrement              
                    THISONE = INOW;
                    HERE = LPREV;
                    COMMOD9.CTRF = 0;
                    //CPMS    add INOW penalty to temporary level penalty              
                    //CPMS    PENHERE does not accumulate so it is never emptied
                    //CPMS    it is always replaced in one shot
                    //CPMS    ## CHECK FOR NEGATIVE PENALTIES ##################
                    //cpms    do common case first!
                    if (COMMOD9.ISTATIC[HPERM[INOW], JOPT, 0] != -1)//<=HXD
                    {
                        PENHERE = LPEN[LPREV] + Helper.EPEN(HPERM[INOW], HERE, JOPT);
                    }
                    else if (COMMOD9.NEGATF != 0)
                    {
                        PENHERE = LPEN[LPREV] + Helper.NEGPEN(HPERM[INOW], HERE, JOPT);
                    }

                    //CPMS  ##################			 
                    //CPMS  open option to move
                    PLACED = 0;
                    if (COMMOD9.CTRF == 1) CTRHERE = 1;

                    //C     BIG LOOP -- repeat until the placement with the smallest penalty
                    //C                 is found (PLACED = 1) 
                    //CPMS              or events have moved down to the base (LEVEL 1)
                    //C----------------------------------------------------------------------
                    while ((PLACED == 0) && (HERE >= 1))//<=HXD
                    {
                        //set up temporary variable for the next level down: 
                        DOWN = HERE - 1;
                        //set up 0 variable to accumulate penalty increments
                        //for events moving down to DOWN  
                        PENDOWN = 0.0;

                        //C--------------------------------------------------------
                        //C      SMALL LOOP -- find penalty for moving all at HERE down 1 level
                        //CPMS                 work down through earlier events as long as they 
                        //CPMS                 exist (>0) and have been placed at or above the
                        //CPMS                 current level (i.e. reach up and fetch down)   
                        //C----------------------------------------------------------------------
                        while ((THISONE >= 0) && (COMMOD9.HSCTSOL[HPERM[THISONE], JOPT] >= HERE))//<=HXD
                        {
                            //work back through all earlier events on HPERM
                            //at level HERE     

                            COMMOD9.CTRF = 0;

                            //CPMS  #### CHECK FOR NEGATIVE PENALTIES ##################
                            //cpms       do common case first!
                            if (COMMOD9.ISTATIC[HPERM[INOW], JOPT, 0] != -1)//<=HXD
                            {
                                PENDOWN = PENDOWN + Helper.EPEN(HPERM[THISONE], DOWN, JOPT);
                            }
                            else if (COMMOD9.NEGATF > 0)
                            {
                                PENDOWN = PENDOWN + Helper.NEGPEN(HPERM[THISONE], DOWN, JOPT);
                            }

                            //CPMS    ##################			 
                            //CPMS         - total up penalty at new low level due to events
                            //CPMS           that have just been moved down 
                            THISONE = THISONE - 1;

                            //select earlier event
                            if (COMMOD9.CTRF == 1) CTRDOWN = 1;

                        }

                        //CPMS      see if DOWN is going to be as far as we move:
                        //C         check to see if DOWN is better than HERE (PENDOWN < PENHERE)
                        //C             if it is, 
                        //C                 check if DOWN is bottom (level 1).
                        //C                     if it is,
                        //C                         place group at DOWN and update things
                        //C                     if not, 
                        //C                         get ready for the next pass through big loop
                        //C
                        //C         note that the penalty at the new HERE is the penalty for moving
                        //C         all the guys above down to HERE (PENDOWN) plus the penalty
                        //C         for guys that are already there (LPEN(DOWN))
                        //CWGK----------------------------------------------------------------------
                        //CWGK         20 dec 94 -- I duplicated some code below to correct an error
                        //CWGK         it should be made cleaner
                        //CWGK----------------------------------------------------------------------
                        //CPMS        if penalty has improved, prepare to try going down farther
                        //CPMS        move HERE down
                        if (PENDOWN <= PENHERE)
                        {
                            //penalty has improved
                            HERE = HERE - 1;
                            PENHERE = PENDOWN + LPEN[DOWN];

                            //- add extra penalty and earlier penalty for new HERE
                            //  unless at base; in that case wrap up              
                            if (HERE == 0)//<=HXD
                            {
                                //at base of section!  wrap up!!  
                                for (I = INOW; I >= THISONE + 1; I--)//HACK:
                                {
                                    COMMOD9.HSCTSOL[HPERM[I], JOPT] = HERE;
                                    //- update horizon array for all moved events
                                }


                                //set flag for end:
                                PLACED = 1;

                                if (CTRDOWN == 1) CTRSECT = 1;

                                CTRDOWN = 0;
                                CTRHERE = 0;

                                //update LPEN at HERE:
                                LPEN[HERE] = PENHERE;

                                for (I = HERE + 1; I <= LPREV; I++)//<=HXD
                                {
                                    //CPMS  reset to zero all levels above HERE
                                    //CPMS  up to LPREV  
                                    LPEN[I] = 0;

                                }


                                //move LPREV to HERE; no higher events remain 
                                LPREV = HERE;
                            }
                            else
                            {
                                //not at base, prepare to move on down
                                CTRHERE = CTRDOWN;
                                CTRDOWN = 0;

                                //reset THISONE toINOW (youngest event)
                                THISONE = INOW;
                            }
                        }
                        else
                        {

                            //C----------------------------------------------------------------------
                            //CPMS    if penalty is worse (PENDOWN>PENHERE), prepare to stop
                            //CPMS                                        at HERE
                            //C        if DOWN is not better than HERE, HERE is best. 
                            //C         place all that need to move HERE
                            //C         set PLACED to 1
                            //C         update LPEN(HERE)
                            //C         zeroize LPEN for levels above HERE
                            //C         update LPREV
                            //C----------------------------------------------------------------------

                            //penalty has not improved, don't move on down
                            for (I = INOW; I >= THISONE + 1; I--)
                            {
                                //update horizon array for all moved events
                                COMMOD9.HSCTSOL[HPERM[I], JOPT] = HERE;
                            }


                            //set flag for end:
                            PLACED = 1;
                            //update LPEN at HERE   
                            if (CTRHERE == 1) CTRSECT = 1;

                            CTRHERE = 0;
                            CTRDOWN = 0;
                            LPEN[HERE] = PENHERE;
                            //reset to 0 all levels above HERE, up to LPREV
                            for (I = HERE + 1; I <= LPREV; I++)//<=HXD
                            {
                                LPEN[I] = 0;
                            }

                            //CPMS   update LPREV - move it to HERE, no events go higher 
                            LPREV = HERE;

                        }

                        //C   -----------------------------------------------------
                        //C   go backto top of big loop 
                        //C   if PLACED = 0, this event not placed, repeat the loop
                        //C   if PLACED = 1, this event has been placed -- for now
                        //C   go to the overall loop and place the next guy
                        //C   -----------------------------------------------------
                    }
                    //C------------ end of big loop ----------
                    //C             the event has been placed
                    //C---------------------------------------
                }

                //C----------------------------------------------------------------------
                //C     end of overall loop. if not all events placed,
                //C     go back to the top and place the next event
                //C----------------------------------------------------------------------

                if ((INOW > RNK2) && (COMMOD9.HSCTSOL[HPERM[INOW], JOPT] ==
                    COMMOD9.SCTSOL[COMMOD9.LSTPERM[INOW], JOPT]))
                {
                    RNK2 = INOW + 1;

                    //CPMS  loop through all events above disturbable zone
                    while (RNK2 < COMMOD9.NEVNT)//<=HXD
                    {
                        //determine level in last solution 
                        HERE = COMMOD9.SCTSOL[COMMOD9.LSTPERM[RNK2], JOPT];
                        //assign to same level as before
                        COMMOD9.HSCTSOL[HPERM[RNK2], JOPT] = HERE;

                        //CPMS  acrue same penalty as before        
                        //CPMS  if INOW is observed in this section
                        if (COMMOD9.ISTATIC[HPERM[RNK2], JOPT, 0] > -1)//<=HXD
                        {
                            LPEN[HERE] = LPEN[HERE] + Helper.EPEN(HPERM[RNK2], HERE, JOPT);
                        }
                        else if (COMMOD9.NEGATF > 0)
                        {
                            LPEN[HERE] = LPEN[HERE] + Helper.NEGPEN(HPERM[RNK2], HERE, JOPT);
                        }

                        RNK2 = RNK2 + 1;

                    }

                    goto Label1001;
                }

            }//End for INOW

        //CPMS------------------------------------------------------------------- 
        //CPMS-------------------------------------------------------------------
        //C     all events placed, highest at HERE, now calc PENJ
        //CPMS    - add up all level penalties to get section penalty (JPEN)
        //C----------------------------------------------------------------------
        Label1001:
            for (I = 0; I <= HERE; I++)//<=HXD
            {
                PENJ = PENJ + LPEN[I];
            }

            COMMOD9.CTRF = CTRSECT;
            CTRSECT = 0;


        }

        //C******************************************************
        //C     A SUBROUTINE TO recalculate the secondary penalties
        //C     in the course of a partial re-optimization
        //C     i.e. called from NEWPEN and NWDMPEN 
        //C
        //C          PROGRAMMER: PETE SADLER
        //C          LAST UPDATE: Jan 27th 2000
        //C
        //C------------------------------------------------
        public static void NEW2PEN(int IRNK, int JRNK, int[] HPERM, ref double HPEN, int BACK)
        {
            //cpms  if back=1, remove previous secondary penlties
            //cpms             - needed for NWDMPEN in which HPEN = PEN
            //cpms  if back=0, don't (NEWPEN resets HPEN to 0.0)

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (BACK == 1) HPEN = HPEN - COMMOD9.TOOPEN;/*in TPL_SA_GLB_PARA, initial value?*/;
            if (COMMOD9.KSM /*in TPL_SA_GLB_PARA, initial value?*/> 0.0)
            {
                Helper.SMOOTH(HPERM);
                //update overall penalty
                HPEN = HPEN + (COMMOD9.SMPEN /*in TPL_SA_GLB_PARA, initial value?*/ * COMMOD9.KSM);
            }

            //CPMS--------------------------------------------------------------
            //C     determine any additional penalty due to the squeezing factor
            if ((COMMOD9.KSQ /*in TPL_SA_GLB_PARA, initial value?*/ > 0.0) || (COMMOD9.SOLVEF == 4) || (COMMOD9.SOLVEF == 5))
            {
                Helper.SQUEEZE(HPERM);
                HPEN = HPEN + (COMMOD9.SQPEN /*in TPL_SA_GLB_PARA, initial value?*/ * COMMOD9.KSQ);
            }

            //C   determine any additional penalty due to the shrinking factor
            if ((COMMOD9.KSM > 0.0) || (COMMOD9.SOLVEF == 6) || (COMMOD9.SOLVEF == 7))
            {
                Helper.SHRINK(HPERM);
                HPEN = HPEN + (COMMOD9.SHPEN /*in TPL_SA_GLB_PARA, initial value?*/ * COMMOD9.KSH/*in TPL_SA_GLB_PARA, initial value?*/);
            }

            //C  determine any additional penalty due to the teasing factor
            if ((COMMOD9.KTS /*in TPL_SA_GLB_PARA, initial value?*/ > 0.0) || (COMMOD9.SOLVEF == 8) || (COMMOD9.SOLVEF == 9))
            {
                if (COMMOD9.STKF /*in TPL_SA_GLB_PARA, initial value?*/ == 9)
                {
                    //cpms teaser=FB4L
                    if (COMMOD9.FB4LF != 0)
                    {
                        Helper.NEWSEQUEL(IRNK, JRNK, HPERM, ref COMMOD9.TSPEN /*in TPL_SA_GLB_PARA, initial value?*/);
                    }

                }
                else if (COMMOD9.STKF == 8)
                {
                    //cpms  teaser=COEX
                    Helper.NEWROYAL(IRNK, JRNK, HPERM, ref COMMOD9.TSPEN);
                }
                else if (COMMOD9.STKF == 7)
                {
                    //teaser=SPAN

                    Helper.NEWSPAN(IRNK, JRNK, HPERM, ref COMMOD9.TSPEN);
                }
                else
                {
                    Helper.TEASE(HPERM);
                }

                HPEN = HPEN + (COMMOD9.TSPEN * COMMOD9.KTS);

            }

        }

        public static void MAXSEQ(int MAXTYPE, int K, int IRNK, int JRNK, int[] XPERM, ref double XPEN)
        {
            //c    next line added to stop crashes in data sets with
            //c    large numbers of events - curiously not a problem before
            COMMOD COMMOD9 = COMMOD.Singleton();

            if ((COMMOD9.IROWS[XPERM[K], 1] > 2) ||
                (COMMOD9.IROWS[XPERM[K], 1] < 1)) goto Label999;

            if (COMMOD9.COEXST[COMMOD9.IROWS[XPERM[Math.Max(IRNK, JRNK)], 2],
                COMMOD9.IROWS[XPERM[K], 2]] < COMMOD9.COXSTF)
            {
                //CPMS   compare with all passed events in turn
                //cpms   a FAD rising above a LAD = 1 less FB4L
                //cpms   a LAD rising above a FAD = 1 more FB4L 
                if (MAXTYPE == 1)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 2) XPEN = XPEN - 1.00;
                }
                else if (MAXTYPE == 2)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 1) XPEN = XPEN + 1.00;
                }
            }

        Label999:
            return;
        }

        public static void MINSEQ(int MINTYPE, int K, int IRNK, int JRNK, int[] XPERM, ref double XPEN)
        {
            //c    next line added to stop crashes in data sets with
            //c    large numbers of events - curiously not a problem before

            COMMOD COMMOD9 = COMMOD.Singleton();

            if ((COMMOD9.IROWS[XPERM[K], 1] > 2) ||
                (COMMOD9.IROWS[XPERM[K], 1] < 1)) goto Label888;

            if (COMMOD9.COEXST[COMMOD9.IROWS[XPERM[Math.Min(IRNK, JRNK)], 2],
                COMMOD9.IROWS[XPERM[K], 2]] < COMMOD9.COXSTF)
            {
                //CPMS   compare with all passed events in turn
                //cpms   a FAD falling below a LAD = 1 more FB4L
                //cpms   a LAD falling below a FAD = 1 less FB4L 

                if (MINTYPE == 1)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 2) XPEN = XPEN + 1.00;
                }
                else if (MINTYPE == 2)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 1) XPEN = XPEN - 1.00;
                }
            }


        Label888:
            return;

        }

        //c   so far it is identical to NEWROYAL!!!!!!!
        //c
        //C     
        //C Determines the "FAD\LAD" penalty from the
        //C previous value without a full recalculation.
        //C The equivalent of NWDMPEN and NEWPEN strategy
        //C and may be nested in these programs both for 
        //C the primary SEQUEL penalty and the secondary
        //C STACKER=FB4L penalty.
        //C**********************************************
        public static void NEWSEQUEL(int IRNK, int JRNK, int[] XPERM, ref double XPEN)
        {
            int K;
            int MAXTYPE, MINTYPE;

            //C***************************
            //c     FB4L(sp1,sp2,Xperm) is a function!
            //c     LADLAD(NSPC,NSPC)   is a matrix in IROWS(I,3) order!
            //c     DOFB4L()            is a subroutine!
            //c---------------------------
            //c     event at JRNK has moved from IRNK
            //c     if (NABRGEN = 3) event at IRNK has moved from JRNK too
            //c     don't reset XPEN to zero;  adjust old value
            //c     XPEN is fed in as PEN
            //c     it is updated and fed out as NXTPEN
            //c----------------------------------------------
            K = 0;
            MAXTYPE = 0;
            MINTYPE = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            MAXTYPE = COMMOD9.IROWS[XPERM[Math.Max(IRNK, JRNK)], 1];
            MINTYPE = COMMOD9.IROWS[XPERM[Math.Min(IRNK, JRNK)], 1];

            if ((COMMOD9.NABRGEN == 1) || (COMMOD9.NABRGEN == 2))
            {
                //CPMS     current jrnk is the event that moved (to jrnk) 
                //CPMS     if JRNK not paired, get out;

                if ((COMMOD9.IROWS[XPERM[JRNK], 1] > 2) ||
                    (COMMOD9.IROWS[XPERM[JRNK], 1] < 1)) goto Label9999;

                //cpms	(small neigborhood crashes at first use of JRNK)
                //CPMS  otherwise, check for direction of move 

                if (IRNK < JRNK)
                {
                    //CPMS   one event (JRNK) moved above several others

                    for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        Helper.MAXSEQ(MAXTYPE, K, IRNK, JRNK, XPERM, ref XPEN);
                    }

                }
                else if (IRNK > JRNK)
                {
                    //CPMS   one event moved below several others   
                    for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK); K++)
                    {
                        Helper.MINSEQ(MINTYPE, K, IRNK, JRNK, XPERM, ref XPEN);
                    }
                }

            }
            else if (COMMOD9.NABRGEN == 3)
            {
                //CPMS  with a double neighborhood structure, both events move
                //CPMS  because the next two code blocks are the same as above
                //CPMS  the code should be re-arranged to eliminate them
                //CPMS  i.e. IF NABRGEN=3, do both;  IF NABRGEN = 1,2, pick one

                //CPMS   one event moved above several others
                if ((MAXTYPE == 2) || (MAXTYPE == 1))
                {
                    for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        //compare with all passed events, including other mover  
                        Helper.MAXSEQ(MAXTYPE, K, IRNK, JRNK, XPERM, ref XPEN);
                    }
                }


                //cpms   other event moved below several others  
                if ((MINTYPE == 2) || (MINTYPE == 1))
                {
                    for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        //CPMS   compare with all passed events, but NOT other mover   
                        Helper.MINSEQ(MINTYPE, K, IRNK, JRNK, XPERM, ref XPEN);
                    }
                }
            }

        Label9999:
            return;

        }

        public static void MAXROY(int MAXTYPE, int K, int IRNK, int JRNK, int[] XPERM, ref double XPEN)
        {

            //c   next line added to stop crashes in data sets with
            //c   large numbers of events - curiously not a problem before

            COMMOD COMMOD9 = COMMOD.Singleton();

            if ((COMMOD9.IROWS[XPERM[K], 1] > 2) ||
                (COMMOD9.IROWS[XPERM[K], 1] < 1)) goto Label999;

            if (COMMOD9.COEXST[COMMOD9.IROWS[XPERM[Math.Max(IRNK, JRNK)], 2],
                COMMOD9.IROWS[XPERM[K], 2]] < COMMOD9.COXSTF)
            {
                //CPMS   compare with all passed events in turn
                //cpms   a FAD rising above a LAD = 1 less coex
                //cpms   a LAD rising above a FAD = 1 more coex 

                if (MAXTYPE == 1)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 2) XPEN = XPEN - 1.00;
                }
                else if (MAXTYPE == 2)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 1) XPEN = XPEN + 1.00;
                }
            }

        Label999:
            return;

        }

        public static void MINROY(int MINTYPE, int K, int IRNK, int JRNK, int[] XPERM, ref double XPEN)
        {
            //c   next line added to stop crashes in data sets with
            //c   large numbers of events - curiously not a problem before

            COMMOD COMMOD9 = COMMOD.Singleton();

            if ((COMMOD9.IROWS[XPERM[K], 1] > 2) ||
                (COMMOD9.IROWS[XPERM[K], 1] < 1)) goto Label888;

            if (COMMOD9.COEXST[COMMOD9.IROWS[XPERM[Math.Min(IRNK, JRNK)], 2],
                COMMOD9.IROWS[XPERM[K], 2]] < COMMOD9.COXSTF)
            {
                //CPMS  compare with all passed events in turn
                //cpms  a FAD falling below a LAD = 1 more coex
                //cpms  a LAD falling below a FAD = 1 less coex 

                if (MINTYPE == 1)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 2) XPEN = XPEN + 1.00;
                }
                else if (MINTYPE == 2)
                {
                    if (COMMOD9.IROWS[XPERM[K], 1] == 1) XPEN = XPEN - 1.00;
                }
            }

        Label888:
            return;

        }

        //C Determines the "coexistence" penalty from the
        //C previous value without a full recalculation.
        //C The equivalent of NWDMPEN and NEWPEN strategy
        //C and may be nested in these programs both for 
        //C the primary ROYAL penalty and the secondary
        //C STACKER=COEX penalty.
        //C**********************************************
        public static void NEWROYAL(int IRNK, int JRNK, int[] XPERM, ref double XPEN)
        {
            int K;
            int MAXTYPE, MINTYPE;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //C*********************************************
            //c     COEX(sp1,sp2,Xperm) is a function!
            //c     COEXST(NSPC,NSPC)   is a matrix in IROWS(I,3) order!
            //c     DOCOEX()            is a subroutine!
            //c---------------------------
            //c     event at JRNK has moved from IRNK
            //c     if (NABRGEN = 3) event at IRNK has moved from JRNK too
            //c     don't reset XPEN to zero;  adjust old value
            //c     XPEN is fed in as PEN
            //c     it is updated and fed out as NXTPEN
            //c----------------------------------------------

            K = 0;
            MAXTYPE = 0;
            MINTYPE = 0;
            MAXTYPE = COMMOD9.IROWS[XPERM[Math.Max(IRNK, JRNK)], 1];
            MINTYPE = COMMOD9.IROWS[XPERM[Math.Min(IRNK, JRNK)], 1];

            if ((COMMOD9.NABRGEN == 1) || (COMMOD9.NABRGEN == 2))
            {
                //CPMS     current jrnk is the event that moved (to jrnk) 
                //CPMS     if JRNK not paired, get out;
                if ((COMMOD9.IROWS[XPERM[JRNK], 1] > 2) ||
                    (COMMOD9.IROWS[XPERM[JRNK], 1] < 1)) goto Label9999;

                //cpms	 (small neigborhood crashes at first use of JRNK)
                //CPMS   otherwise, check for direction of move    

                if (IRNK < JRNK)
                {
                    //CPMS   one event (JRNK) moved above several others
                    for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        Helper.MAXROY(MAXTYPE, K, IRNK, JRNK, XPERM, ref XPEN);
                    }
                }
                else if (IRNK > JRNK)
                {
                    //CPMS  one event moved below several others
                    for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK); K++)
                    {
                        Helper.MINROY(MINTYPE, K, IRNK, JRNK, XPERM, ref XPEN);

                    }
                }
            }
            else if (COMMOD9.NABRGEN == 3)
            {

                //CPMS  with a double neighborhood structure, both events move
                //CPMS  because the next two code blocks are the same as above
                //CPMS  the code should be re-arranged to eliminate them
                //CPMS  i.e. IF NABRGEN=3, do both;  IF NABRGEN = 1,2, pick one

                //CPMS    one event moved above several others 
                if ((MAXTYPE == 2) || (MAXTYPE == 1))
                {
                    for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        //CPMS  compare with all passed events, including other mover     
                        Helper.MAXROY(MAXTYPE, K, IRNK, JRNK, XPERM, ref XPEN);
                    }
                }

                //cpms   other event moved below several others      
                if ((MINTYPE == 2) || (MINTYPE == 1))
                {
                    for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        //CPMS  compare with all passed events, but NOT other mover
                        Helper.MINROY(MINTYPE, K, IRNK, JRNK, XPERM, ref XPEN);
                    }
                }
            }


        Label9999:
            return;

        }

        //C Determines the "span" penalty from the
        //C previous value without a full recalculation.
        //C The equivalent of NWDMPEN and NEWPEN strategy
        //C and may be nested in these programs both for 
        //C the primary (not developed) penalty and the 
        //C secondary STACKER=SPAN penalty.
        //C**********************************************
        public static void NEWSPAN(int IRNK, int JRNK, int[] XPERM, ref double XPEN)
        {
            int I, jump;

            I = 0;
            jump = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //c----------------------------
            //c     don't reset XPEN to zero;  adjust old value
            //c     XPEN is fed in as PEN
            //c     it is updated and fed out as NXTPEN or TSPEN
            //c     depending upon the call.
            //c----------------------------------------------
            //c     calculate big move once
            jump = Math.Max(IRNK, JRNK) - Math.Min(IRNK, JRNK);

            //c     with BIGNABR, one event makes a big move, those
            //c     leap-frogged do one step in the opposite direction
            //c     SMLNABR is a trivial case of the same
            if ((COMMOD9.NABRGEN == 1) || (COMMOD9.NABRGEN == 2))
            {
                //CPMS     current jrnk is the event that moved (to jrnk)  
                if (IRNK < JRNK)
                {
                    //CPMS       one event (JRNK) moved up           
                    //cpms       a FAD rising = less span
                    if (COMMOD9.IROWS[XPERM[JRNK], 1] == 1)
                    {
                        XPEN = XPEN - jump;
                    }
                    else if (COMMOD9.IROWS[XPERM[JRNK], 1] == 2)
                    {
                        //a LAD rising = more span
                        XPEN = XPEN + jump;
                    }

                    for (I = IRNK; I < JRNK - 1; I++)
                    {
                        //cpms a FAD falling = more span
                        if (COMMOD9.IROWS[XPERM[I], 1] == 1)
                        {
                            XPEN = XPEN + 1;
                        }
                        else if (COMMOD9.IROWS[XPERM[I], 1] == 2)
                        {
                            //a LAD falling = less span
                            XPEN = XPEN - 1;
                        }
                    }
                }
                else if (IRNK > JRNK)
                {
                    //CPMS  one event moved below several others           
                    //cpms  a FAD falling below a LAD = 1 more coex
                    if (COMMOD9.IROWS[XPERM[JRNK], 1] == 1)
                    {
                        XPEN = XPEN + jump;
                    }
                    else if (COMMOD9.IROWS[XPERM[JRNK], 1] == 2)
                    {
                        //a LAD falling below a FAD = 1 less coex
                        XPEN = XPEN - jump;
                    }

                    for (I = JRNK + 1; I < IRNK; I++)
                    {
                        //cpms  a FAD rising = less span
                        if (COMMOD9.IROWS[XPERM[I], 1] == 1)
                        {
                            XPEN = XPEN - 1;
                        }
                        else if (COMMOD9.IROWS[XPERM[I], 1] == 2)
                        {
                            //a LAD rising = more span
                            XPEN = XPEN + 1;
                        }
                    }
                }
            }
            else if (COMMOD9.NABRGEN == 3)
            {
                //CPMS  with a double neighborhood structure, both events move
                //cpms  those in between do not

                //cpms  a FAD rising above a LAD = 1 less coex
                //cpms  a LAD rising above a FAD = 1 more coex 
                if (COMMOD9.IROWS[XPERM[Math.Max(IRNK, JRNK)], 1] == 1)
                {
                    XPEN = XPEN - jump;
                }
                else if (COMMOD9.IROWS[XPERM[Math.Max(IRNK, JRNK)], 1] == 2)
                {
                    XPEN = XPEN + jump;
                }

                //cpms   a FAD falling below a LAD = 1 more coex
                //cpms   a LAD falling below a FAD = 1 less coex 
                if (COMMOD9.IROWS[XPERM[Math.Min(IRNK, JRNK)], 1] == 1)
                {
                    XPEN = XPEN + jump;
                }
                else if (COMMOD9.IROWS[XPERM[Math.Min(IRNK, JRNK)], 1] == 2)
                {
                    XPEN = XPEN - jump;
                }
            }


        }

        //C***********************************************************************
        //C    A SUBROUTINE TO calculate the penalty based upon sequence mis-matches
        //C
        //C                       PROGRAMMER: PETE SADLER
        //C                       LAST UPDATE: June 10th 1997
        //C
        //C
        //C     INPUTS:
        //C         the given permutation is passed in as HPERM
        //C
        //C         DEMPEN and the subroutines it calls are problem aware
        //C               -  USE COMMOD9
        //C   
        //C             events are in IROWS
        //C             levels where they were recovered are in ISTATIC
        //C             allowable moves for each event are also in ISTATIC
        //C             weights are in RSTATIC
        //C             data values are cross referenced to levels in VALEVL
        //C
        //C             the universal control for range contractions is in CONTRACT
        //C             DOINPUT already adjusted ISTATIC accordingly
        //C
        //C     OUTPUTS: 
        //C         the penalty for HPERM is returned in HPEN
        //C-----------------
        //C     WEIGHTS:
        //C         ordinal penalties do not handle weights well, they work pairs;
        //C		zero-weighted events should not be omitted, this leads to 
        //C		sequences that cannot generate horizons without contractions
        //C***********************************************************************
        public static void NWDMPEN(int IRNK, int JRNK, int[] HPERM, ref double HPEN)
        {
            int JOPT;
            double PENJ;
            int SEP, K, statij;

            K = 0;
            SEP = 0;
            JOPT = 0;
            statij = 0;

            //C***********************************************************************
            //C     note that GETPEN is not generic. 
            //C     it needs problem specific info, so it has access to the commons
            //C     but it passes info with the calling routine (initially ANNEAL)
            //C     in the CALL statement
            //c----------------------------------------------------------------------
            //C   The democratic penalty does not require local placements 
            //C   It does not use stratigraphic distance
            //C----------------------------------------------------------------------
            //C
            //C   COMPUTE THE PENALTY FOR HPERM
            //C
            //C----------------------------------------------------------------------
            //C     HPEN is ANNEAL's previous penalty
            //C     setting HPEN = PEN means that new penalty is generated from old
            //C     the next line does not appear in NEWPEN.FOR!  which sets HPEN = 0.0
            //C     the next line causes the penalty calculation to build upon the 
            //C     former penalty
            //C     this is essential for the calculation of the primary dempen
            //C     it seems to make a mess of the secondary penalty!
            //C     the old secondary penalty is left in,  the new one is added on top!
            //CPMS-------------------------------------------------------------
            //c
            //CPMS--any penalty from JSPAN needs to be removed from PEN
            //cpms--the correction is not the last penalty from JSPAN (SPANPEN)
            //cpms--it is the last ACCEPTED penalty from JSPAN (ASPNPEN)
            //cpms--[just as PEN only updates from NXTPEN if accepted] 
            //CPMS--SPANPEN will be recalculated and added back in at the end

            COMMOD COMMOD9 = COMMOD.Singleton();

            HPEN = COMMOD9.PEN;

            if (COMMOD9.JSPANF == 1) HPEN = HPEN - COMMOD9.ASPNPEN;

            if ((COMMOD9.PENF == 5) || (COMMOD9.PENF == 6)) goto Label5000;

            //CPMS  with a big or small neighborhood structure, only one event moves   
            if ((COMMOD9.NABRGEN == 1) || (COMMOD9.NABRGEN == 2))
            {
                if (COMMOD9.PENF != 4)
                {
                    //small neighborhood crashes in next loop
                    for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                    {
                        PENJ = 0.0;

                        //reject if not seen or zeroed out
                        if (Helper.IORZERO(HPERM[JRNK], JOPT)) goto Label2000;

                        statij = COMMOD9.ISTATIC[HPERM[JRNK], JOPT, 0];

                        //c	   small neigborhood crashes at first use of JRNK
                        //CPMS    current jrnk is the event that moved (to jrnk)

                        if (IRNK < JRNK)
                        {
                            //one event moved above several others
                            for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                            {
                                //CPMS   compare with all passed events in turn
                                //cpms   but reject if not found or zeroed out  
                                if (Helper.IORZERO(HPERM[K], JOPT)) goto Label1000;

                                SEP = statij - COMMOD9.ISTATIC[HPERM[K], JOPT, 0];
                                if ((COMMOD9.PENF == 2) && (SEP > 0))
                                {
                                    PENJ = PENJ - 1;
                                }
                                else if ((COMMOD9.PENF == 2) && (SEP < 0))
                                {
                                    PENJ = PENJ + 1;
                                }
                                else if ((COMMOD9.PENF == 7) && (SEP > 0))
                                {
                                    PENJ = PENJ - (double)(JRNK - K) / (double)COMMOD9.NEVNT;
                                }
                                else if ((COMMOD9.PENF == 7) && (SEP < 0))
                                {
                                    PENJ = PENJ + (double)(JRNK - K) / (double)COMMOD9.NEVNT;
                                }
                                else if (COMMOD9.PENF == 3)
                                {
                                    PENJ = PENJ - SEP;
                                }

                            }//End for K

                        Label1000:
                            HPEN = HPEN + PENJ;

                            COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + PENJ;
                        }
                        else if (IRNK > JRNK)
                        {
                            //one event moved below several others
                            for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK); K++)
                            {
                                if (Helper.IORZERO(HPERM[K], JOPT)) goto Label1500;

                                SEP = COMMOD9.ISTATIC[HPERM[K], JOPT, 0] - statij;

                                if ((COMMOD9.PENF == 2) && (SEP > 0))
                                {
                                    PENJ = PENJ - 1;
                                }
                                else if ((COMMOD9.PENF == 2) && (SEP < 0))
                                {
                                    PENJ = PENJ + 1;
                                }
                                else if (COMMOD9.PENF == 3)
                                {
                                    PENJ = PENJ - SEP;
                                }
                                else if ((COMMOD9.PENF == 7) && (SEP > 0))
                                {
                                    PENJ = PENJ - (double)(K - JRNK) / (double)COMMOD9.NEVNT;
                                }
                                else if ((COMMOD9.PENF == 7) && (SEP < 0))
                                {
                                    PENJ = PENJ + (double)(K - JRNK) / (double)COMMOD9.NEVNT;
                                }

                            }//End For K

                        Label1500:
                            HPEN = HPEN + PENJ;
                            COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + PENJ;
                        }

                    Label2000:
                        ;
                    }
                }
                else if (COMMOD9.PENF == 4)
                {
                    //CPMS  current jrnk is the event that moved (to jrnk)     
                    if (IRNK < JRNK)
                    {
                        //one event moved above several others
                        if (COMMOD9.RASCon) goto Label2100;

                        //Rascal penalty without RASC()
                        for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                        {
                            PENJ = 0.0;
                            SEP = 0;

                            for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                            {
                                //    abandon if either is missin
                                //abandon if either is zeroed out

                                if ((Helper.IORZERO(HPERM[JRNK], JOPT))) continue;

                                //sep counts the number of observed pairs 
                                SEP = SEP + 1;

                                //simple ordinal penalty
                                if (COMMOD9.ISTATIC[HPERM[JRNK], JOPT, 0] >
                                COMMOD9.ISTATIC[HPERM[K], JOPT, 0])
                                {
                                    PENJ = PENJ - 1;
                                    COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] - 1;
                                }
                                else if (COMMOD9.ISTATIC[HPERM[JRNK], JOPT, 0] <
                                    COMMOD9.ISTATIC[HPERM[K], JOPT, 0])
                                {
                                    PENJ = PENJ + 1;
                                    COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                                }

                            }//End for JOPT

                            //CPMS  convert penalty to probability 
                            if (SEP > 0) HPEN = HPEN + (PENJ / SEP);

                        }//End for K

                        goto Label2300;

                    Label2100:

                        for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                        {
                            //c   cycle if never observed together
                            if (COMMOD9.RASC[HPERM[JRNK], HPERM[K]] +
                                COMMOD9.RASC[HPERM[K], HPERM[JRNK]] <= 0) continue;

                            //add penalty for new order and subtract for old order
                            HPEN = HPEN +
                                (double)(COMMOD9.RASC[HPERM[JRNK], HPERM[K]]) /
                                (double)(COMMOD9.RASC[HPERM[JRNK], HPERM[K]] +
                                (double)(COMMOD9.RASC[HPERM[K], HPERM[JRNK]]))
                                - (double)(COMMOD9.RASC[HPERM[K], HPERM[JRNK]]) /
                                (double)(COMMOD9.RASC[HPERM[JRNK], HPERM[K]] +
                                (double)(COMMOD9.RASC[HPERM[K], HPERM[JRNK]]));
                        }

                    Label2300:
                        ;
                    }
                    else if (IRNK > JRNK)
                    {
                        //one event moved below several others
                        if (COMMOD9.RASCon) goto Label2500;

                        for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK); K++)
                        {
                            PENJ = 0.0;
                            SEP = 0;

                            for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                            {
                                //cpms  abandon if either is zeroed out
                                if ((Helper.IORZERO(HPERM[JRNK], JOPT)) ||
                                    (Helper.IORZERO(HPERM[K], JOPT))) continue;

                                SEP = SEP + 1;

                                //simple ordinal penalty

                                if (COMMOD9.ISTATIC[HPERM[K], JOPT, 0] >
                                    COMMOD9.ISTATIC[HPERM[JRNK], JOPT, 0])
                                {
                                    PENJ = PENJ - 1;
                                    COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                                }
                                else if (COMMOD9.ISTATIC[HPERM[K], JOPT, 0] <
                                    COMMOD9.ISTATIC[HPERM[JRNK], JOPT, 0])
                                {
                                    PENJ = PENJ + 1;
                                    COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                                }
                            }//End for JOPT

                            if (SEP > 0) HPEN = HPEN + (PENJ / SEP);
                        }//End for K

                        goto Label2700;

                    Label2500:

                        for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK); K++)
                        {
                            //cycle if never observed together
                            if (COMMOD9.RASC[HPERM[JRNK], HPERM[K]] +
                                COMMOD9.RASC[HPERM[K], HPERM[JRNK]] <= 0) continue;

                            //subtract for old order and add penalty for new order
                            HPEN = HPEN -
                                (double)(COMMOD9.RASC[HPERM[JRNK], HPERM[K]]) /
                                (double)((COMMOD9.RASC[HPERM[JRNK], HPERM[K]]) +
                                (COMMOD9.RASC[HPERM[K], HPERM[JRNK]]))
                                + (double)(COMMOD9.RASC[HPERM[K], HPERM[JRNK]]) /
                                (double)((COMMOD9.RASC[HPERM[JRNK], HPERM[K]]) +
                                (COMMOD9.RASC[HPERM[K], HPERM[JRNK]]));

                        }//End for K

                    Label2700:
                        ;
                    }
                }
            }
            else if (COMMOD9.NABRGEN == 3)
            {
                //with a double neighborhood structure, both events move
                if (COMMOD9.PENF != 4)
                {
                    for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                    {
                        PENJ = 0.0;

                        // abandon if zeroed out
                        if (Helper.IORZERO(HPERM[Math.Max(IRNK, JRNK)], JOPT)) goto Label2900;

                        statij = COMMOD9.ISTATIC[HPERM[Math.Max(IRNK, JRNK)], JOPT, 0];

                        //one event moved above several others    
                        for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                        {
                            //compare with all passed events, including other mover  
                            if (Helper.IORZERO(HPERM[K], JOPT)) goto Label2800;

                            SEP = statij - COMMOD9.ISTATIC[HPERM[K], JOPT, 0];

                            if ((COMMOD9.PENF == 2) && (SEP > 0))
                            {
                                PENJ = PENJ - 1;
                            }
                            else if ((COMMOD9.PENF == 2) && (SEP < 0))
                            {
                                PENJ = PENJ + 1;
                            }
                            else if ((COMMOD9.PENF == 7) && (SEP > 0))
                            {
                                PENJ = PENJ - (double)(JRNK - K) / (double)COMMOD9.NEVNT;
                            }
                            else if ((COMMOD9.PENF == 7) && (SEP < 0))
                            {
                                PENJ = PENJ + (double)(JRNK - K) / (double)COMMOD9.NEVNT;
                            }
                            else if (COMMOD9.PENF == 3)
                            {
                                PENJ = PENJ - SEP;
                            }

                        Label2800:
                            ;
                        }



                        HPEN = HPEN + PENJ;
                        COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + PENJ;

                    Label2900:
                        PENJ = 0.0;

                        //abandon if either is zeroed out
                        if (Helper.IORZERO(HPERM[Math.Min(IRNK, JRNK)], JOPT)) goto Label3000;

                        statij = COMMOD9.ISTATIC[HPERM[Math.Min(IRNK, JRNK)], JOPT, 0];

                        //other event moved below several others        
                        for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK) - 1; K++)
                        {
                            //compare with all passed events, but not other mover       
                            if (Helper.IORZERO(HPERM[K], JOPT)) goto Label2950;

                            SEP = COMMOD9.ISTATIC[HPERM[K], JOPT, 0] - statij;

                            if ((COMMOD9.PENF == 2) && (SEP > 0))
                            {
                                PENJ = PENJ - 1;
                            }
                            else if ((COMMOD9.PENF == 2) && (SEP < 0))
                            {
                                PENJ = PENJ + 1;
                            }
                            else if ((COMMOD9.PENF == 7) && (SEP > 0))
                            {
                                PENJ = PENJ - (double)(K - JRNK) / (double)COMMOD9.NEVNT;
                            }
                            else if ((COMMOD9.PENF == 7) && (SEP < 0))
                            {
                                PENJ = PENJ + (double)(K - JRNK) / (double)COMMOD9.NEVNT;
                            }
                            else if (COMMOD9.PENF == 3)
                            {
                                PENJ = PENJ - SEP;
                            }

                        Label2950:
                            ;

                        }//End for K

                        HPEN = HPEN + PENJ;
                        COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + PENJ;

                    Label3000:
                        ;
                    }//End for JOPT

                }
                else if (COMMOD9.PENF == 4)
                {
                    //CPMS   current jrnk is the event that moved (to jrnk)             
                    //CPMS   one event moved above several others 
                    if (COMMOD9.RASCon) goto Label3100;

                    //Rascal penalty without RASC()
                    for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        PENJ = 0.0;
                        SEP = 0;

                        for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                        {
                            //abandon if either is zeroed out
                            if (Helper.IORZERO(HPERM[Math.Max(IRNK, JRNK)], JOPT)) continue;

                            if (Helper.IORZERO(HPERM[K], JOPT)) continue;

                            //sep counts the number of observed pairs 
                            SEP = SEP + 1;

                            //simple ordinal penalty

                            if (COMMOD9.ISTATIC[HPERM[Math.Max(IRNK, JRNK)], JOPT, 0] >
                                COMMOD9.ISTATIC[HPERM[K], JOPT, 0])
                            {
                                PENJ = PENJ - 1;
                                COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                            }
                            else if (COMMOD9.ISTATIC[HPERM[Math.Max(IRNK, JRNK)], JOPT, 0] <
                                COMMOD9.ISTATIC[HPERM[K], JOPT, 0])
                            {
                                PENJ = PENJ + 1;
                                COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                            }

                        }//End for JOPT

                        //convert penalty to probability
                        if (SEP > 0) HPEN = HPEN + (PENJ / SEP);

                    }//End for K

                    goto Label3300;

                Label3100:
                    ;

                    //Rascal Penalty with RASC()
                    for (K = Math.Min(IRNK, JRNK); K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        //cycle if never observed together
                        if (COMMOD9.RASC[HPERM[Math.Max(IRNK, JRNK)], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[Math.Max(IRNK, JRNK)]] <= 0) continue;

                        //add penalty for new order and subtract for old order
                        HPEN = HPEN +
                            (double)(COMMOD9.RASC[HPERM[Math.Max(IRNK, JRNK)], HPERM[K]]) /
                            (double)(COMMOD9.RASC[HPERM[Math.Max(IRNK, JRNK)], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[Math.Max(IRNK, JRNK)]])
                            - (double)(COMMOD9.RASC[HPERM[K], HPERM[Math.Max(IRNK, JRNK)]]) /
                            (double)(COMMOD9.RASC[HPERM[Math.Max(IRNK, JRNK)], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[Math.Max(IRNK, JRNK)]]);

                    }//End for K

                Label3300:

                    //other event moved below several others
                    if (COMMOD9.RASCon) goto Label3500;

                    //Rascal Penalty without RASC()
                    for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        //compare to all passed events but not other mover 
                        PENJ = 0.0;
                        SEP = 0;

                        for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                        {
                            //abandon if either is zeroed out
                            if (Helper.IORZERO(HPERM[Math.Min(IRNK, JRNK)], JOPT)) continue;

                            if (Helper.IORZERO(HPERM[JRNK], JOPT)) continue;

                            SEP = SEP + 1;

                            //simple ordinal penalty

                            if (COMMOD9.ISTATIC[HPERM[K], JOPT, 0] >
                            COMMOD9.ISTATIC[HPERM[Math.Min(IRNK, JRNK)], JOPT, 0])
                            {
                                PENJ = PENJ - 1;
                                COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                            }
                            else if (COMMOD9.ISTATIC[HPERM[K], JOPT, 0] <
                                COMMOD9.ISTATIC[HPERM[Math.Min(IRNK, JRNK)], JOPT, 0])
                            {
                                PENJ = PENJ + 1;
                                COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                            }
                        }//End for JOPT

                        if (SEP > 0) HPEN = HPEN + (PENJ / SEP);

                    }//End for K

                    goto Label3700;

                Label3500:
                    //Rascal Penalty determination with RASC()
                    for (K = Math.Min(IRNK, JRNK) + 1; K < Math.Max(IRNK, JRNK) - 1; K++)
                    {
                        //cycle if never observed together

                        if (COMMOD9.RASC[HPERM[Math.Min(IRNK, JRNK)], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[Math.Min(IRNK, JRNK)]] <= 0) continue;

                        //subtract for old order and add penalty for new order
                        HPEN = HPEN -
                            (double)(COMMOD9.RASC[HPERM[Math.Min(IRNK, JRNK)], HPERM[K]]) /
                            (double)(COMMOD9.RASC[HPERM[Math.Min(IRNK, JRNK)], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[Math.Min(IRNK, JRNK)]])
                            + (double)(COMMOD9.RASC[HPERM[K], HPERM[Math.Min(IRNK, JRNK)]]) /
                            (double)(COMMOD9.RASC[HPERM[Math.Min(IRNK, JRNK)], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[Math.Min(IRNK, JRNK)]]);

                    }//End for K

                Label3700:
                    ;
                }//End if
            }//End if

            if (COMMOD9.JSPANF == 1)
            {
                Helper.JSPAN(HPERM);
                HPEN = HPEN + COMMOD9.SPANPEN;
            }

        Label5000:

            if (COMMOD9.PENF == 5)
            {
                //HPEN = PEN  done in first line
                Helper.NEWROYAL(IRNK, JRNK, HPERM, ref HPEN);
            }

            if (COMMOD9.PENF == 6)
            {
                //HPEN = PEN  done in first line
                if (COMMOD9.FB4LF != 0) Helper.NEWSEQUEL(IRNK, JRNK, HPERM, ref HPEN);
            }


            if (COMMOD9.PEN2F > 0) Helper.NEW2PEN(IRNK, JRNK, HPERM, ref HPEN, 1);



        }

        //CPMS***********************************
        //CPMS   Returns value of placed level
        //C
        //CPMS      Programmer:  Pete Sadler
        //CPMS      Last Modfied: July 27 2000
        //C
        //CPMS***********************************
        public static double PLACE(int evt, int sec)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            //CPMS   sec is the section
            //CPMS   evt is the IROWS position of the event

            if (sec <= COMMOD9.NSCT - 1)//<=HXD
            {
                return COMMOD9.VALEVEL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[evt], sec], sec];
            }
            else
            {
                return COMMOD9.COMPLVL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[evt], sec], sec - COMMOD9.NSCT];
            }

        }
    }

}
