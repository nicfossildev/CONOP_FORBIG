using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CONOP.NET
{
    /// <summary>
    /// Builds the partial ordering matrix PTLORD
    /// </summary>
    public class DOPTLORD : IRunIt
    {

        #region Singleton

        private static DOPTLORD g_singleton = null;
        private DOPTLORD() { }
        public static DOPTLORD Singleton()
        {
            if (g_singleton == null)
                g_singleton = new DOPTLORD();

            return g_singleton;
        }

        #endregion


        #region IRunIt 成员

        protected void UPDATE(int IRW, int MRW, int rong, int rite, int pass)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            if (COMMOD9.PTLORD[IRW, MRW] != (int)rong)
            {
                COMMOD9.PTLORD[IRW, MRW] = (int)rite;
            }
            else
            {
                if (pass > 0)
                {
                    //TODO:DOPTLORD.CS
                    //WRITE(*,900) IROWS(IRW,1),IROWS(MRW,1),pass
                }

                if ((pass == 0) && (COMMOD9.IROWS[IRW, 0] == COMMOD9.IROWS[MRW, 0]))
                {
                    //TODO:DOPTLORD.CS
                    //WRITE(*,910) IROWS(IRW,1)
                }

                if ((pass == 0) && (COMMOD9.IROWS[IRW, 0] != COMMOD9.IROWS[MRW, 0]))
                {
                    //TODO:DOPTLORD.CS
                    //WRITE(*,909) IROWS(IRW,1),IROWS(MRW,1)
                }

                if (pass < 0)
                {
                    //TODO:DOPTLORD.CS
                    //    WRITE(*,901) IROWS(IRW,1),IROWS(MRW,1),
                    //1                             IROWS(ABS(pass),1)
                }

                Helper.Write("____________________________________\n");
                Helper.Write("INCONSISTENT ORDERING RELATIONSHIPS!\n");

            }

            ////TODO:DOPTLORD.CS
            ////-------------------------
            ////  format statement for errors due to conflicts of sequence in pass 1
            //  900 FORMAT(1X,'order of event ',I5,' and event ',I5,
            //     1          ' in section ',I5, ' may conflict with prior section')
            ////  format statement for errors due to conflicts of sequence
            //  901 FORMAT(1X,'order of events ',I5,' and ',I5,
            //     1          ' and ',I5, ' appears inconsistent ')
            ////  format statement for errors due to conflicts of sequence
            //  909 FORMAT(1X,'superposition of event ',I5,' and event ',I5,
            //     1      ' may conflict with age range or violate taxon range')
            ////  format statement for errors due to conflicts of sequence
            //  910 FORMAT(1X,'mid-range event for taxon ',I5,
            //     1      ' may lie outside taxon range')

        }

        public void RunIt()
        {

            int I, J, K, Jdef;
            int IROW, MROW, KROW;
            int LVLIJ, LVLMJ, IFAD, ILAD;
            double IUP, IDN, MUP, MDN;

            //CPMS-------------------------------------------------------------------
            //CPMS NOTES ON THE PARTIAL ORDERING MATRIX.
            //CPMS--------------------------------------
            //CPMS  
            //CPMS PTLORD partial ordering matrix (NEVNT,NEVNT+2-(2*NSPC))
            //CPMS        for comparing all NEVNT events to orderable events 
            //CPMS        row and column order is the same as IROW
            //CPMS        cell contents:
            //CPMS        0  - no ordering information
            //CPMS        1  - row event must occur before (older) or with column event
            //CPMS        2  - row event must occur after (younger) or with column event
            //CPMS        3  - row and column events must occur at same level
            //CPMS           - this includes comparison of an event with itself
            //CPMS    i.e.
            //CPMS    PTLORD has one row for every event         
            //CPMS    and one column for every non range event (Type .NE. 1 or 2) 
            //CPMS	   - the two extra columns (right hand side) hold the 
            //CPMS    	 count of 1's and count of 2's
            //CPMS
            //CPMS    the rows are in IROWS order
            //CPMS    the columns are more difficult to order!!
            //CPMS    because the "other" events do not have an order except as in
            //CPMS    IROWS.  But in IROWS they occur together
            //CPMS    because we assign "other" events the first block of IROWS
            //CPMS    by two passes through input data!!
            //CPMS
            //C       COEXST tracks the relative ordering of FADs and LADs more
            //C       efficiently than one big PTLORD matrix
            //C
            //C       The cells are filled as follows        
            //C
            //CPMS    There are five main routines, or passes, in DOPTLORD.FOR
            //CPMS      1. compare radiometric dates IN DIFFERENT SECTIONS
            //CPMS      2. compare MID with LAD and FAD
            //CPMS      3. compare MAX with MIN 
            //CPMS   (3)4. pass through each section extracting ordering
            //CPMS   (4)5. check rows and columns in PTLORD to fill more cells 
            //C         on the basis of internal consistency
            //CPMS-------------------------------------------------------------------
            //CPMS NOTES ON EVENT NUMBERS
            //CPMS-----------------------
            //CPMS
            //CPMS   Each event has a number and a type
            //CPMS                1 = paired FAD  must share event number with LAD
            //CPMS                    allowable moves usually 1
            //CPMS                2 = paired LAD  must share event number with FAD
            //CPMS                    allowable moves usually 2
            //CPMS                3 = unpaired bio-event
            //CPMS                        unpaired FAD - allowable moves usually 1
            //CPMS                        unpaired LAD - allowable moves usually 2
            //CPMS                        ACME level - allowable moves usually 3 
            //CPMS                4 = unpaired marker horizon
            //CPMS                5 = dated horizon
            //CPMS               11 = floor (youngest) of BOX
            //CPMS               12 = lid   (oldest)   of BOX
            //CPMS   in each section in which it is observed it has
            //CPMS           a level (with a value)
            //CPMS           allowable moves   0 = none
            //CPMS                             1 = down only
            //CPMS                             2 = up only
            //CPMS                             3 = up or down
            //CPMS   Dated events do not have weights
            //CPMS   instead the input cells are used to record the range of possible
            //CPMS   possible age, typically, plus and minus one sigma
            //CPMS-------------------------------------------------------------------
            //CPMS  NOTES ON THE ARRAYS TO USE
            //CPMS----------------------------
            //c
            //c     IROWS(IROW,1) user's number for the event in row IROW
            //c     IROWS(IROW,2) type of the event in row IROW 
            //cpms  IROWS(IROW,3) CONOP9's number for the event in row IROW - for COEX and PTLORD
            //cpms     CONOP9 renumbers to group unpaired and paired events     
            //cpms
            //c     IROWS cannot record the allowable moves because these vary
            //c     with section.  ISTATIC and RSTATIC distinguish sections
            //c     IROWS has only information that is independent of sections
            //CPMS
            //CPMS  ISTATIC(IROW,J,1) level in section J of the IROW event
            //CPMS  ISTATIC(IROW,J,2) allowable moves in section J of the IROW event
            //C                       0 - no moves away from datum
            //c                         - all moves 0 and 1 below, stay below
            //c                           PTLORD scores 2
            //C                         - all moves 0 and 2 above, stay above
            //c                           PTLORD scores 1
            //C                       1 - moves down below datum only
            //c                           can switch with events below
            //C                         - all moves 0 and 2 above, stay above
            //c                           PTLORD scores 1
            //C                       2 - moves up above datum only 
            //c                           can switch with events above
            //c                         - all moves 0 and 1 below, stay below
            //c                           PTLORD scores 2
            //C                       3 - moves up or down away from datum
            //c                           can switch with any event
            //c                           PTLORD stays 0          
            //CPMS
            //CPMS  VALEVEL(ISTATIC(IROW,J,1),J)
            //CPMS          is the height in section J of event in row IROW
            //CPMS  ------------------------------------------------------------------
            //CPMs  initialize for Intel

            I = 0;
            J = 0;
            K = 0;
            Jdef = 0;
            IROW = 0;
            MROW = 0;
            KROW = 0;
            LVLIJ = 0;
            LVLMJ = 0;
            IFAD = 0;
            ILAD = 0;
            IUP = 0.0;
            IDN = 0.0;
            MUP = 0.0;
            MDN = 0.0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (COMMOD9.NOTHR <= 0) goto Label999;

            //CPMS  ZERO THE PTLORD MATRIX
            Helper.SetVal(COMMOD9.PTLORD, 0);

            //CPMS*****************************
            //CPMS   PASS 1.   Use absolute age as evidence of ordering
            //CPMS   was originally pass 2!
            //CPMS-------------------------------------------------------------------
            //cpms   If TYPE.EQ.5, wtup and wtdn give range of probable age
            //cpms   determine ordering like inverse of coexistence
            //c       
            //cpms	wtup is the younger age - should be IUP,MUP 
            //cpms  wtdn is the older age	- should be IDN,MDN
            //cpms	BUT, use variables seek MIN() and MAX() to reverse entry errors
            //CPMS-------------------------------------------------------------------

            for (int n = 0; n < COMMOD9.IROWS.GetLength(0); n++)
            {
                if (COMMOD9.IROWS[n, 1] > 5)
                {
                    Helper.Write("IROWS[{0},1]={1}\n", n, COMMOD9.IROWS[n, 1]);
                }
            }

            //CPMS  loop through all events
            for (IROW = 0; IROW < COMMOD9.NEVNT; IROW++)
            {
                IUP = 0.00;
                IDN = 0.00;
                Jdef = 0;

                if (COMMOD9.IROWS[IROW, 1] == 5)
                {
                    //the event is a dated horizon - find its age range (search by section!)
                    for (J = 0; J < COMMOD9.NSCT; J++)
                    {
                        if (COMMOD9.ISTATIC[IROW, J, 0] >= 0)//<=HXD
                        {
                            if (IDN == 0.00)
                            {
                                //record its age range
                                IUP = Math.Min(COMMOD9.RSTATIC[IROW, J, 0], COMMOD9.RSTATIC[IROW, J, 1]);
                                IDN = Math.Max(COMMOD9.RSTATIC[IROW, J, 0], COMMOD9.RSTATIC[IROW, J, 1]);

                                Jdef = J;
                            }
                            else
                            {
                                if (Math.Min(COMMOD9.RSTATIC[IROW, J, 0], COMMOD9.RSTATIC[IROW, J, 1]) != IUP)
                                {
                                    //TODO:DOPTLORD.CS
                                    //WRITE(*,930) IROWS(IROW,1), Jdef, J
                                }

                                if (Math.Max(COMMOD9.RSTATIC[IROW, J, 0], COMMOD9.RSTATIC[IROW, J, 1]) != IDN)
                                {
                                    //TODO:DOPTLORD.CS
                                    //WRITE(*,939) IROWS(IROW,1), Jdef, J
                                }
                            }
                        }

                        if (IUP > 0.00) 
                            break;

                        //otherwise, try next section   
                    }// End for J

                    //CPMS  now search for ALL other dated event pairings (only need larger IROWs)      
                    for (MROW = IROW + 1; MROW < COMMOD9.NEVNT; MROW++)
                    {
                        if ((COMMOD9.IROWS[MROW, 1] == 5) &&
                            (IROW != MROW))
                        {
                            MUP = 0.00;
                            MDN = 0.00;
                            //Both M and I have absolute ages
                            for (K = 0; K < COMMOD9.NSCT; K++)
                            {
                                if (COMMOD9.ISTATIC[MROW, K, 0] >= 0)//<=HXD
                                {
                                    //record its age range
                                    MUP = Math.Min(COMMOD9.RSTATIC[MROW, K, 0], COMMOD9.RSTATIC[MROW, K, 1]);
                                    MDN = Math.Max(COMMOD9.RSTATIC[MROW, K, 0], COMMOD9.RSTATIC[MROW, K, 1]);
                                    if (MDN < IUP)
                                    {
                                        //M must be younger than I
                                        //but not if their far limits are equal (to avoid errors of order)
                                        UPDATE(MROW, IROW, 1, 2, 0);
                                        UPDATE(IROW, MROW, 2, 1, 0);
                                    }

                                    if (IDN < MUP)
                                    {
                                        //I must be younger than M
                                        //but not if their far limits are equal (to avoid errors of order)	
                                        UPDATE(IROW, MROW, 1, 2, 0);
                                        UPDATE(MROW, IROW, 2, 1, 0);
                                    }

                                    //if age ranges overlap, do not update 
                                }

                                if (MUP > 0.00) break;
                            }
                        }
                    }//next MROW
                }
            }

            //******************************************************   
            //CPMS  PASS 2. PLACE any MID BETWEEN its FAD and LAD
            //CPMS     
            //CPMS   should be covered by sections?
            //CPMS   NO! because MID can move up or down!
            //CPMS   MID is only restricted by its own FAD and LAD??
            //CPMS
            //CMPS   This loop was mistaken - looking for ANY FAD or LAD
            //CPMS   fortunately, it used IROW instead of MROW
            //CPMS   and did nothing to PTLORD!!!
            //CPMS   added check that species was the same
            IFAD = 0;
            ILAD = 0;
            //CPMS  loop through all events
            for (IROW = 0; IROW < COMMOD9.NEVNT; IROW++)
            {

                //cpms    search every entry using (IROW)
                if (COMMOD9.IROWS[IROW, 1] == 3)
                {
                    //cpms    IROW is a MID 
                    for (MROW = 0; MROW < COMMOD9.NEVNT; MROW++)
                    {
                        //cpms         search every entry again
                        //cpms         to find FAD and LAD of same species
                        if (COMMOD9.IROWS[IROW, 0] != COMMOD9.IROWS[MROW, 0]) continue;

                        if (COMMOD9.IROWS[MROW, 1] == 1)
                        {

                            //cpms            MROW is a FAD  --  below MID
                            IFAD = 1;
                            //cpms            M must be older than I	
                            UPDATE(MROW, IROW, 2, 1, 0);
                        }
                        else if (COMMOD9.IROWS[MROW, 1] == 2)
                        {

                            //cpms         MROW is a LAD  --  above MID
                            ILAD = 1;
                            //cpms         M must be younger than I	  
                            UPDATE(MROW, IROW, 1, 2, 0);
                        }
                    }

                    if (IFAD == 0)
                    {
                        //TODO:DOPTLORD.CS
                        //WRITE(*,919) IROWS(IROW,1)
                    }

                    if (ILAD == 0)
                    {
                        //TODO:DOPTLORD.CS
                        //WRITE(*,920) IROWS(IROW,1)
                    }

                }

                IFAD = 0;
                ILAD = 0;
            }


            //******************************************************   
            //CPMS  PASS 3. PLACE MAX and MIN in order
            //CPMS     
            //CPMS   should be covered by sections?
            //CPMS   NO! because MAX and MIN constrain eachother
            //CPMS
            //CPMS  loop through all events
            for (IROW = 0; IROW < COMMOD9.NEVNT; IROW++)
            {

                //cpms    search every entry, using (IROW), for MAXs.
                //cpms    only MAX OR MIN needed to find each pair
                if (COMMOD9.IROWS[IROW, 1] == 11)
                {
                    //cpms      IROW is a MAX
                    //cpms      corresponding MIN is already in (IROWS,4)
                    MROW = COMMOD9.IROWS[IROW, 3];
                    //cpms      MROW is the MIN, must be above IROW. the MAX 		
                    //cpms      M must be younger than I
                    UPDATE(MROW, IROW, 1, 2, 0);
                    UPDATE(IROW, MROW, 2, 1, 0);

                }
            }


            //****************************************************************  
            //CPMS  PASS 4. (formerly 3) LOOK FOR ORDERING WITHIN EACH SECTION
            //CPMS     was originally pass 1
            //CPMS          (the COEXST search worked by paired event numbers 
            //CPMS           it needed FINDROW() to get into ISTATIC 
            //CPMS           because the COEXST rows are paired event numbers)                      
            //CPMS
            //CPMS      the PTLORD search can work by IROWS and section
            //CPMS              1. for each IROW, find level in section
            //CPMS              2. compare with every OTHR event (TYPE>2)
            //CPMS                 in same section 
            //CPMS              3. check ISTATIC(row,sct,2) for ordering  
            //CPMS           
            //C---------------------------------------------------------------
            //CPMS  loop through all events
            //CPMS  LVLIJ for all events
            //CPMS  LVLMJ of unpaired and boxed events only
            for (IROW = 0; IROW < COMMOD9.NEVNT; IROW++)
            {
                //in each section
                for (J = 0; J < COMMOD9.NSCT; J++)
                {
                    //if IROW is observed in section J 
                    if (!(Helper.IORZERO(IROW, J)))
                    {
                        //record its level
                        LVLIJ = COMMOD9.ISTATIC[IROW, J, 0];

                        //CPMS   and compare it with other events MROW in section J
                        //CPMS   and, for MROW all that are unpaired
                        //CPMS   consider updating the partial ordering matrix
                        for (MROW = 0; MROW < COMMOD9.NEVNT; MROW++)
                        {
                            if (MROW == IROW)
                            {
                                if (((COMMOD9.IROWS[MROW, 1] > 2) || (COMMOD9.IROWS[MROW, 1] < 1)))
                                {
                                    //M and I are the same unpaired or boxed event
                                    COMMOD9.PTLORD[IROW, MROW] = 3;
                                }
                            }
                            else if (((COMMOD9.IROWS[MROW, 1] > 2) || (COMMOD9.IROWS[MROW, 1] < 1)) &&
                                !(Helper.IORZERO(MROW, J)))
                            {
                                //MROW is an unpaired or boxed event and found in J 
                                //cpms       worry about unpaired events that move 
                                //cpms       (APP) and (MIN) move down   types -1 and 12
                                //cpms       (DIS) and (MAX) move up     types -2 and 11
                                //cpms       because they may be compared with like events
                                //cpms             ----------------------------------------------------------------
                                //cpms             It is not necessary to track the event types IROWS(IROW|MROW,2)
                                //cpms             because the algorithm uses the smaller number
                                //cpms             of possible allowed moves (i.e. ISTATIC(IROW|MROW,J,2))
                                //cpms             Thus, new event types are automatically covered
                                //cpms             because there are no other allowed-move combinations 
                                //cpms             that determine pairwise order:
                                //c                (0,0)    0 above 0
                                //c                (0,0)    0 below 0  
                                //c                (0,1)    0 above 1
                                //c                (0,2)    0 below 2
                                //c                (1,0)    1 below 0  
                                //c                (1,2)    1 below 2  (FAD,LAD) in COEXST need (APP|MIN) below (DIS|MAX)
                                //c                (0,2)    2 above 0  
                                //c                (1,2)    2 above 1  (FAD,LAD) in COEXST need (DIS|MAX) below (APP|MIN)


                                LVLMJ = COMMOD9.ISTATIC[MROW, J, 0];
                                //CPMS       ---------------
                                //CPMS       Case 1: IROWS may not move up or down (0)
                                //CPMS               and IROWS is above MROWS, which may not move up (0,1) 
                                if ((COMMOD9.ISTATIC[IROW, J, 1] == 0) && (LVLIJ > LVLMJ) &&
                                    ((COMMOD9.ISTATIC[MROW, J, 1] == 0) || (COMMOD9.ISTATIC[MROW, J, 1] == 1)))
                                {
                                    UPDATE(IROW, MROW, 1, 2, J);
                                }
                                //CPMS       ---------------
                                //CPMS       Case 2: IROWS may not move up or down (0)
                                //CPMS               and IROWS is below MROWS, which may not move down (0,2)  
                                else if ((COMMOD9.ISTATIC[IROW, J, 1] == 0) && (LVLIJ < LVLMJ) &&
                                    ((COMMOD9.ISTATIC[MROW, J, 1] == 0) || (COMMOD9.ISTATIC[MROW, J, 1] == 2)))
                                {
                                    UPDATE(IROW, MROW, 2, 1, J);
                                }
                                //CPMS       ---------------
                                //CPMS       Case 3: IROWS may only move down (1)
                                //CPMS               and IROWS is below MROWS, which may not move down (0,2) 
                                //CPMS               i.e. APP|FAD|MIN below DIS|MAX
                                else if ((COMMOD9.ISTATIC[IROW, J, 1] == 1) && (LVLIJ < LVLMJ) &&
                                    ((COMMOD9.ISTATIC[MROW, J, 1] == 0) || (COMMOD9.ISTATIC[MROW, J, 1] == 2)))
                                {
                                    UPDATE(IROW, MROW, 2, 1, J);
                                }
                                //CPMS       ---------------
                                //CPMS       Case 4: IROWS may only move up (2)
                                //CPMS               and IROWS is above MROWS, which may not move up (0,1)
                                //CPMS               i.e. DIS|LAD|MAX above APP|MIN
                                else if ((COMMOD9.ISTATIC[IROW, J, 1] == 2) && (LVLIJ > LVLMJ) &&
                                    ((COMMOD9.ISTATIC[MROW, J, 1] == 0) || (COMMOD9.ISTATIC[MROW, J, 1] == 1)))
                                {
                                    UPDATE(IROW, MROW, 1, 2, J);
                                }
                                //CPMS       ---------------
                                //CPMS       Case 5: IROWS may move up or down (3)
                                //CPMS               ignore  --  no ordering possible except with own FAD,LAD
                                //CPMS                           and this is already booked in previous pass!
                            }
                        }//next event M
                    }
                }//next section J
            }//next event I


            //CPMS*******************************************************************
            //CPMS   PASS 5. (formerly 4)   Use internal evidence of ordering
            //CPMS      this pass does not need adjustments for new event types
            //CPMS      it uses combinations of pairwise orders already established
            //CPMS-------------------------------------------------------------------
            //c    If an event IROW must occur below an event MROW
            //c       then it must occur below all events KROW that are fixed above MROW
            //c
            //c    If an event IROW must occur above an event MROW
            //c       then it must occur above all events KROW that are fixed below MROW
            //CPMS-------------------------------------------------------------------
            //c     Outer loop through all IROW rows  (1, NEVNT)
            for (IROW = 0; IROW < COMMOD9.NEVNT; IROW++)
            {
                //Middle loop through all MROW columns in row IROW
                for (MROW = 0; MROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); MROW++)
                {
                    if (COMMOD9.PTLORD[IROW, MROW] != 0)
                    {
                        //Inner loop through all KROW columns in row MROW 
                        for (KROW = 0; KROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); KROW++)
                        {
                            //if I-below-M and M-below-K  then I-below-K
                            if ((COMMOD9.PTLORD[IROW, MROW] == 1) &&
                                (COMMOD9.PTLORD[MROW, KROW] == 1))
                            {
                                UPDATE(IROW, KROW, 2, 1, 0 - IROW);
                            }
                            //if I-above-M and M-above-K  then I-above-K
                            else if ((COMMOD9.PTLORD[IROW, MROW] == 2) &&
                                (COMMOD9.PTLORD[MROW, KROW] == 2))
                            {
                                UPDATE(IROW, KROW, 1, 2, 0 - IROW);
                            }
                        }//next event K
                    }
                }//next event M
            }//next event I

            //CPMS  REPEAT TO COMPLETE
            //CPMS  two passes seem to do enough
            //CPMS  missing constraints might not be fatal
            for (IROW = 0; IROW < COMMOD9.NEVNT; IROW++)
            {
                for (MROW = 0; MROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); MROW++)
                {
                    if (COMMOD9.PTLORD[IROW, MROW] != 0)
                    {
                        for (KROW = 0; KROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); KROW++)
                        {
                            if ((COMMOD9.PTLORD[IROW, MROW] == 1) &&
                                (COMMOD9.PTLORD[MROW, KROW] == 1))
                            {
                                UPDATE(IROW, KROW, 2, 1, 0 - IROW);
                            }
                            else if ((COMMOD9.PTLORD[IROW, MROW] == 2) &&
                                (COMMOD9.PTLORD[MROW, KROW] == 2))
                            {
                                UPDATE(IROW, KROW, 1, 2, 0 - IROW);
                            }
                        }
                    }
                }
            }

            //CPMS*******************************************************************
            //CPMS   PASS 6. (formerly 5)  Complete totals of 1's and 2's
            //CPMS
            //CPMS  loop through all unpaired and paired events (rows)
            for (IROW = 0; IROW < COMMOD9.NEVNT; IROW++)
            {
                //loop through all unpaired events (columns)
                for (MROW = 0; MROW < COMMOD9.NEVNT - (COMMOD9.NSPC * 2); MROW++)
                {
                    if (COMMOD9.PTLORD[IROW, MROW] == 1)
                    {
                        COMMOD9.PTLORD[IROW, COMMOD9.NEVNT  - (COMMOD9.NSPC * 2)] =
                            COMMOD9.PTLORD[IROW, COMMOD9.NEVNT  - (COMMOD9.NSPC * 2)] + 1;//HACK:May be 
                    }
                    else if (COMMOD9.PTLORD[IROW, MROW] == 2)
                    {
                        COMMOD9.PTLORD[IROW, COMMOD9.NEVNT + 1- (COMMOD9.NSPC * 2)] =
                               COMMOD9.PTLORD[IROW, COMMOD9.NEVNT + 1 - (COMMOD9.NSPC * 2)] + 1;//HACK:May be 
                    }
                }//next unpaired event
            }//CPMS  next event


            //CPMS-------------------------------------------------------
            //CPMS  Print out the partial ordering matrix for the record 
            if (COMMOD9.CDF != 1)
            {
                //TODO:DOPTLORD.CS
                //OPEN(23,FILE=ORDRFILE)
                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    //TODO:DOPTLORD.CS
                    //        WRITE(23,7500) (PTLORD(I,J),J=1,NEVNT+2-(NSPC*2))
                    //7500       FORMAT(1X,1000(I3,1X))  
                    //7900    CONTINUE
                }
                //        file1 = COMMITQQ(23)
                //        CLOSE(23)
            }

//CPMS---------------------------------------------------
        //CPMS  format statement for MID without FAD or LAD
        //TODO:DOPTLORD.CS
        //919 FORMAT(1X,'mid-range event for taxon ',I5,
        //   1       'may be missing FAD')
        //920 FORMAT(1X,'mid-range event for taxon ',I5,
        //   1        'may be missing LAD')

//CPMS  format statement for contradictory ages
        //TODO:DOPTLORD.CS
        //930 FORMAT(1X,'younger age limit for event ',I5,
        //   1       'different in sections ',I5,' and ',I5)
        //939 FORMAT(1X,'older age limit for event ',I5,
        //   1       'different in sections ',I5,' and ',I5)


        Label999:
            return;
        }

        #endregion
    }
}