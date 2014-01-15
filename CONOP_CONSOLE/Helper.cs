using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CONOP.NET
{
    public static partial class Helper
    {
        private static System.Windows.Forms.Form mainForm = null;

        public static int MinVal(int[,] T)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1: return T[0, 0];
                default:
                    int min = T[0, 0];
                    foreach (int t in T)
                    {
                        if (t < min) min = t;
                    }

                    return min;
            }
        }

        public static double MinVal(double[,] T)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1: return T[0, 0];
                default:
                    double min = T[0, 0];
                    foreach (double t in T)
                    {
                        if (t < min) min = t;
                    }

                    return min;
            }
        }

        public static int MAXVal(int[] T)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1: return T[0];
                default:
                    int max = T[0];
                    foreach (int t in T)
                    {
                        if (t > max) max = t;
                    }


                    return max;
            }
        }

        public static int MAXVal(int[,] T)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1: return T[0, 0];
                default:
                    int max = T[0, 0];
                    foreach (int t in T)
                    {
                        if (t > max) max = t;
                    }


                    return max;
            }
        }

        public static double MAXVal(double[] T)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1: return T[0];
                default:
                    double max = T[0];
                    foreach (double t in T)
                    {
                        if (t > max) max = t;
                    }


                    return max;
            }
        }

        public static double MAXVal(double[,] T)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1: return T[0, 0];
                default:
                    double max = T[0, 0];
                    foreach (double t in T)
                    {
                        if (t > max) max = t;
                    }

                    return max;
            }
        }

        public static void SetVal(int[] T, int val)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1:
                    T[0] = val;
                    break;
                default:
                    for (int i = 0; i < T.GetLength(0); i++)
                        T[i] = val;
                    break;
            }
        }

        public static void SetVal(int[,] T, int val)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1:
                    T[0, 0] = val;
                    break;
                default:
                    for (int i = 0; i < T.GetLength(0); i++)
                        for (int j = 0; j < T.GetLength(1); j++)
                            T[i, j] = val;
                    break;
            }
        }

        public static void SetVal(int[, ,] T, int val)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1:
                    T[0, 0, 0] = val;
                    break;
                default:
                    for (int i = 0; i < T.GetLength(0); i++)
                        for (int j = 0; j < T.GetLength(1); j++)
                            for (int k = 0; k < T.GetLength(2); k++)
                                T[i, j, k] = val;
                    break;
            }
        }

        public static void SetVal(double[] T, double val)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1:
                    T[00] = val;
                    break;
                default:
                    for (int i = 0; i < T.GetLength(0); i++)
                        T[i] = val;
                    break;
            }
        }

        public static void SetVal(double[,] T, double val)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1:
                    T[0, 0] = val;
                    break;
                default:
                    for (int i = 0; i < T.GetLength(0); i++)
                        for (int j = 0; j < T.GetLength(1); j++)
                            T[i, j] = val;
                    break;
            }
        }

        public static void SetVal(double[, ,] T, double val)
        {
            switch (T.Length)
            {
                case 0: throw new InvalidOperationException();
                case 1:
                    T[0, 0, 0] = val;
                    break;
                default:
                    for (int i = 0; i < T.GetLength(0); i++)
                        for (int j = 0; j < T.GetLength(1); j++)
                            for (int k = 0; k < T.GetLength(2); k++)
                                T[i, j, k] = val;
                    break;
            }
        }

        public static void CopyArray(int[] srcArray, ref int[] destArray)
        {
            if (destArray == null) destArray = new int[srcArray.Length];
            Array.Copy(srcArray, destArray, srcArray.Length);
        }

        public static void CopyArray(int[,] srcArray, ref int[,] destArray)
        {
            if (destArray == null) destArray = new int[srcArray.GetLength(0), srcArray.GetLength(1)];
            Array.Copy(srcArray, destArray, srcArray.Length);
        }

        public static void CopyArray(double[] srcArray, ref double[] destArray)
        {
            if (destArray == null) destArray = new double[srcArray.Length];
            Array.Copy(srcArray, destArray, srcArray.Length);
        }

        public static void CopyArray(double[,] srcArray, ref double[,] destArray)
        {
            if(destArray==null)destArray = new double[srcArray.GetLength(0), srcArray.GetLength(1)];
            Array.Copy(srcArray, destArray, srcArray.Length);
        }

        public static void Write(string s, params object[] arg)
        {
            //Console.Write(s, arg);
        }

        public static void Write(string s)
        {
            if (mainForm != null)
            {
#if GUIMODE
                frmCONOP frm = mainForm as frmCONOP;
                frm.MSG_SB.Append(s);
                frm.Invalidate();
                frm.Update();
#endif
            }
            //TODO : write into console for new output
            Console.Write(s);
        }

        public static void SetWriterControl(System.Windows.Forms.Form form)
        {
            mainForm = form;
        }

        

        #region Special Functions

        /// <summary>
        /// Counts event properties by section
        /// </summary>
        /// <param name="flg"></param>
        /// <param name="fac"></param>
        public static void EVENTSUM(int flg, double fac)
        {
            int I, J;
            int Nj, Nmax, Nmin;
            double NEARBEST, mrg;

            //C----------------------------------------------------------------------
            //CPMS  flg determines nature of count
            //CPMS  1  observed in section    CULLIST( ,3)
            //CPMS  2  observed away from section ends    CULLIST( ,4)
            //CPMS  3  placed away from section ends    CULLIST( ,5)
            //CPMS  4  max and min optimal positions    CULLIST( ,1)    CULLIST( ,2)
            //cpms
            //CPMS  mrg determines the margin between the very best
            //CPMS      and the cut-off for considering a solution
            //CPMS      near enough
            //c
            //c      LEVEL FOUND  = ISTATIC(I,J,1)
            //c      LEVEL PLACED = HSCTSOL(I,J)
            //CPMS--------------------------------------------------------------------

            COMMOD COMMOD9 = COMMOD.Singleton();

            NEARBEST = 0.0;
            mrg = 0.0;
            mrg = fac + 1.0;

            if (flg == 4)
            {
                COMMOD9.VERYBEST = Helper.MinVal(COMMOD9.EVERYBST);
                NEARBEST = COMMOD9.VERYBEST * Math.Max(1.0, mrg);
            }


            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                Nj = 0;
                Nmin = COMMOD9.NEVNT;
                Nmax = 1;

                if (flg == 4)
                {
                    for (J = 0; J < COMMOD9.NEVNT; J++)
                    {
                        //inner (position) loop
                        COMMOD9.NXTPEN = COMMOD9.EVERYBST[I, J];
                        if (COMMOD9.NXTPEN <= NEARBEST)
                        {
                            Nmin = Math.Min(Nmin, J);
                            Nmax = Math.Max(Nmax, J);
                        }

                    }

                    COMMOD9.CULLIST[I, 0] = Nmax;
                    COMMOD9.CULLIST[I, 1] = Nmin;

                }
                else
                {
                    for (J = 0; J < COMMOD9.NSCT; J++)
                    {
                        if (flg == 1)
                        {
                            if (COMMOD9.ISTATIC[I, J, 0] > -1) Nj = Nj + 1;//<=HXD
                        }

                        if (flg == 2)
                        {
                            //<=HXD
                            if ((COMMOD9.ISTATIC[I, J, 0] >= 0) && (COMMOD9.ISTATIC[I, J, 0] < COMMOD9.HLEVEL[J])) Nj = Nj + 1;
                        }

                        if (flg == 3)
                        {
                            //<=HXD
                            if ((COMMOD9.HSCTSOL[I, J] >= 0) && (COMMOD9.HSCTSOL[I, J] < COMMOD9.HLEVEL[J])) Nj = Nj + 1;
                        }
                    }

                    if (flg == 1) COMMOD9.CULLIST[I, 2] = Nj;
                    if (flg == 2) COMMOD9.CULLIST[I, 3] = Nj;
                    if (flg == 3) COMMOD9.CULLIST[I, 4] = Nj;
                }


            }

        }


        //CPMS  Logical function set to .TRUE. if event has all 0.00 weights
        //CPMS   in given section or is missing from the section
        //CPMS   used to eliminate events from contradiction count
        public static bool IORZERO(int evtrow, int sct)
        {

            bool retVal = false;
            COMMOD COMMOD9 = COMMOD.Singleton();
            if ((COMMOD9.ISTATIC[evtrow, sct, 0] == -1) ||
                ((COMMOD9.RSTATIC[evtrow, sct, 0] == 0.0) &&
                (COMMOD9.RSTATIC[evtrow, sct, 1] == 0.0)))//<=HXD
            {
                retVal = true;
            }

            return retVal;
        }

        //CPMS  Logical function set to .TRUE. if event has all 0.00 weights
        //CPMS   in given section
        //CPMS   used to eliminate events from coexistence rules
        public static bool RZERO(int evtrow, int sct)
        {
            bool retVal = false;
            COMMOD COMMOD9 = COMMOD.Singleton();
            if ((COMMOD9.RSTATIC[evtrow, sct, 0] == 0.0) &&
                (COMMOD9.RSTATIC[evtrow, sct, 1] == 0.0))
            {
                retVal = true;
            }
            return retVal;

        }

        //CPMS  Logical function set to .TRUE. if event has all 0.00 weights
        //CPMS   in all sections
        //CPMS   used to eliminate events from coexistence rules
        public static bool ALLRZERO(int evtrow)
        {
            bool retVal = true;
            int sct;
            COMMOD COMMOD9 = COMMOD.Singleton();

            for (sct = 0; sct < COMMOD9.NSCT; sct++)
            {
                if ((COMMOD9.RSTATIC[evtrow, sct, 0] > 0.0) &&
                    (COMMOD9.RSTATIC[evtrow, sct, 1] > 0.0))
                {
                    return false;
                }
            }

            return retVal;
        }

        /// <summary>
        /// ACCEPT SPECIES AND TYPE AS INPUTS, AND FIND THE
        ///ROW NUMBER IN IROWS AND OTHER ARRAYS FOR THAT EVENT
        /// </summary>
        /// <param name="EVT"></param>
        /// <param name="TYP"></param>
        /// <param name="ROW"></param>
        public static void FINDROW(int EVT, int TYP, ref int ROW)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            for (ROW = 0; ROW < COMMOD9.NEVNT; ROW++)
            {
                if ((COMMOD9.IROWS[ROW, 0] == EVT) && (COMMOD9.IROWS[ROW, 1] == TYP)) break;
            }

            if (ROW == COMMOD9.NEVNT) ROW = -1;
        }

        /// <summary>
        /// Makes a label for the Ith event in Irows
        /// </summary>
        /// <param name="I"></param>
        /// <param name="xtitle"></param>
        /// <param name="flg1"></param>
        /// <param name="flg2"></param>
        /// <param name="flg3"></param>
        /// <param name="flg4"></param>
        /// <param name="flg5"></param>
        /// <param name="flg6"></param>
        public static void GETEVNT(int I, ref string xtitle, int flg1, int flg2, int flg3, int flg4, int flg5, int flg6)
        {
            int Nj;
            string xsct, xevent;
            string xtag;

            //C----------------------------------------------------------------------
            //C	 Flags switch name components on (1) or off (0)
            //C      flg1  nickname
            //C      flg2  name
            //C      flg3  FAD / LAD
            //C      flg4  number of sections
            //C      flg5  conop evnt number
            //C      flg6  tag name
            //C-----------------------------------------------------
            //c     initialize for Intel 11.1
            Nj = 0;
            xsct = "  ";
            xtag = "  ";
            xevent = "  ";
            xtitle = "  ";
            //C------------------------

            COMMOD COMMOD9 = COMMOD.Singleton();

            if ((flg1 != 0) || (flg2 != 0))
            {
                if (COMMOD9.EVTFILE.Substring(0, 3) != "OFF")
                {
                    if (flg2 == 0)
                    {
                        xtitle = " ";
                    }
                    else
                    {
                        xtitle = COMMOD9.EVNTNAME[COMMOD9.IROWS[I, 0]].Trim();
                    }

                    if (flg1 != 0) xtitle = "[" + COMMOD9.EVNTNICK[COMMOD9.IROWS[I, 0]].Trim() +
                            "]   " + xtitle.Trim();
                }
                else
                {
                    xtitle = (I+1).ToString();//<=HXD
                    xtitle = "Event " + xtitle.Trim();
                }
            }


            if (flg3 != 0)
            {
                if (COMMOD9.IROWS[I, 1] == 1)
                {
                    xtitle = xtitle.Trim() + " FAD";
                }
                else if (COMMOD9.IROWS[I, 1] == 2)
                {
                    xtitle = xtitle.Trim() + " LAD";
                }
                else if (COMMOD9.IROWS[I, 1] == -1)
                {
                    xtitle = xtitle.Trim() + " APP";
                }
                else if (COMMOD9.IROWS[I, 1] == -2)
                {
                    xtitle = xtitle.Trim() + " DIS";
                }
                else if (COMMOD9.IROWS[I, 1] == 3)
                {
                    xtitle = xtitle.Trim() + " MID";
                }
                else if (COMMOD9.IROWS[I, 1] == -3)
                {
                    xtitle = xtitle.Trim() + " BOB";
                }
                else if (COMMOD9.IROWS[I, 1] == 4)
                {
                    xtitle = xtitle.Trim() + " ASH";
                }
                else if (COMMOD9.IROWS[I, 1] == 5)
                {
                    xtitle = xtitle.Trim() + " AGE";
                }
                else if (COMMOD9.IROWS[I, 1] == 11)
                {
                    xtitle = xtitle.Trim() + " MAX";
                }
                else if (COMMOD9.IROWS[I, 1] == 12)
                {
                    xtitle = xtitle.Trim() + " MIN";
                }
            }

            if (flg4 != 0)
            {
                Nj = COMMOD9.CULLIST[I, 2];

                if (Nj > 0)
                {

                    xsct = Nj.ToString();
                    xtitle = xtitle.Trim() + " {in " + xsct.Trim() + " sections}";
                }

            }

            if (flg5 != 0)
            {
                xevent = (I + 1).ToString();//<=HXD
                xtitle = xtitle.Trim() + "  (C# " + " " + xevent.Trim() + ")";
            }


            if (flg6 != 0)
            {
                xtag = COMMOD9.ETAGNAME[COMMOD9.IROWS[I, 5]].Trim();//HACK:xtag = TRIM(ETAGNAME(IROWS(I,6)+1)) IN FORTRAN
                xtitle = xtitle.Trim() + "  (tag:" + " " + xtag.Trim() + ")";
            }


        }


        //C***********************************************************************
        //C   IMPROVED SHELL SORT
        //C      DOES NOT REARRANGE THE ORIGINAL VALUES
        //C      VARIABLES
        //C         N: THE NUMBER OF VALUES IN THE VECTOR TO BE SORTED
        //C         X: N VECTOR OF REALS TO BE SORTED
        //C         L: index to sorted X
        //C         D: COMPARISON DISTANCE
        //C             COMPARE  X(L(I)) AND X(L(I+D)) FOR I = 1 TO N - D,
        //C            START WITH
        //C              D = 2**INT(LOG(N)/LOG(2)) -1 (OPTIMIZES SORT TIME)
        //C            THEN DECREMENT D TO THE INTEGER PART OF (D / 2).
        //C             OUTER WHILE () DO EXECUTES UNTIL THIS IS LESS THAN
        //C             OR EQUAL ZERO
        //C         TEMP: TEMPORARY STORAGE FOR L(I+D) WHEN SWITCH(ES)
        //C                NECESSARY
        //C         FLAG: INDICATOR FOR INNER WHILE () DO LOOP WHICH MOVES
        //C              THE ORIGINAL RANK OF X(L(I+D)) AS FAR UP AS
        //C              NECESSARY IN THE SEQUENCE L(I), L(I+D), L(I+2D),
        //C              ETC.
        //C
        //C         UPDATED 8OCT89
        //C***************************************************************
        public static void ASORT(ref double[] X, ref int[] L, int N)
        {
            int I, J;
            int D, TEMP, FLAG;

            //C***************************************************************
            //cpms  initialize array
            Helper.SetVal(L, 0);
            //cpms  don't initialize X() it is pre-loaded by calling routine!!
            //C      INITALIZE D - REMEMBER, LOG REQUIRES A REAL ARGUMENT
            D = (int)Math.Pow(2, (int)(Math.Log((double)N) /Math.Log(2.0))) - 1;

            //C  CREATE INDEX VECTOR L TO BE SORTED AND REFLECT THE RANKS
            //C  OF THE CORRESPONDING X VALUES
            for (I = 0; I < N; I++)
            {
                L[I] = I;
            }

        //C  SORT L ACCORDING TO THE X VALUES
        //C  WHILE D GREATER THAN ZERO, DO
        Label15:
            if (D > 0)
            {

                //C  COMPARE X(L(I)) AND X(L(I+D) VALUES TO SEE IF A SWITCH IS
                //C  INDICATED. IF SO, MOVE L(I) DOWN
                for (I = 0; I < N - D; I++)
                {
                    if (X[L[I]] > X[L[I + D]])
                    {
                        TEMP = L[I + D];
                        L[I + D] = L[I];

                        //C      SINCE WERE MOVING L(I) DOWN, SEE IF WE NEED TO MOVE L(I+D)

                        //C         UP MORE THAN ONE INCREMENT OF D. IF WE PASS THE FIRST
                        //C            TEST, THE ANSWER IS NO. JUST SWITCH THEM

                        if (I <= D)
                        {
                            L[I] = TEMP;
                        }
                        //C      WE DIDN'T, SO SEE HOW FAR UP THE L(I), L(I+D), ...
                        //C        SEQUENCE IT GOES.
                        //C         USE FLAG = 0 TO SAY WE'VE GONE FAR ENOUGH
                        else
                        {
                            FLAG = 1;
                            J = I - D;

                        //C      WHILE FLAG IS NOT ZERO, DO
                        Label25:
                            if (FLAG != 0)
                            {

                                //C      IF WE PASS THIS, TEMP (THE OLD L(I+D)) GOES HERE (L(J))
                                //C      NOTE: THE BASIC PROGRAM I STARTED WITH HAD .GT. HERE
                                if (X[TEMP] > X[L[J]])
                                {
                                    L[J + D] = TEMP;
                                    FLAG = 0;
                                }
                                //C      OTHERWISE MOVE L(J) DOWN ONE D INCREMENT AND TRY THE
                                //C         NEXT ONE UP THE SEQUENCE - IF THERE IS ANOTHER
                                else
                                {
                                    L[J + D] = L[J];
                                    J = J - D;

                                    //C      IF J IS .LT. ONE, WE CAN GO NO FARTHER, SO PUT TEMP HERE
                                    if (J < 1)
                                    {
                                        FLAG = 0;
                                        L[J + D] = TEMP;
                                    }
                                }

                                goto Label25;
                            }
                        }
                    }
                }



                //C      DECREMENT D IF THE RESULT WILL BE .GT. ZERO

                D = (int)(D / 2);

                goto Label15;
            }
        }

        //C***********************************************************************
        //C   IASORT - ASCENDING SORT SUBROUTINE FOR INTEGERS     27 MAR 1990
        //C***********************************************************************
        //C   IMPROVED SHELL SORT
        //C      DOES NOT REARRANGE THE ORIGINAL VALUES
        //C
        //C      VARIABLES
        //C         N: THE NUMBER OF VALUES IN THE VECTOR TO BE SORTED
        //C         IX: N VECTOR OF INTEGERS TO BE SORTED
        //C         D: COMPARISON DISTANCE
        //C             COMPARE  IX(L(I)) AND IX(L(I+D)) FOR I = 1 TO N - D,
        //C
        //C            START WITH
        //C              D = 2**INT(LOG(N)/LOG(2)) -1 (OPTIMIZES SORT TIME)
        //C
        //C            THEN DECREMENT D TO THE INTEGER PART OF (D / 2).
        //C             OUTER WHILE () DO EXECUTES UNTIL THIS IS LESS THAN
        //C             OR EQUAL ZERO
        //C         TEMP: TEMPORARY STORAGE FOR L(I+D) WHEN SWITCH(ES)
        //C                NECESSARY
        //C         FLAG: INDICATOR FOR INNER WHILE () DO LOOP WHICH MOVES
        //C              THE ORIGINAL RANK OF IX(L(I+D)) AS FAR UP AS
        //C              NECESSARY IN THE SEQUENCE L(I), L(I+D), L(I+2D),
        //C              ETC.
        //C
        //C***************************************************************
        public static void IASORT(ref int[] X, ref int[] L, int N)
        {
            int I, J;
            int D, TEMP, FLAG;

            //C***************************************************************
            //cpms  initialize array
            Helper.SetVal(L, 0);
            //cpms  don't initialize X() it is pre-loaded by calling routine!!
            //C      INITALIZE D - REMEMBER, LOG REQUIRES A REAL ARGUMENT
            D = (int)Math.Pow(2, (int)(Math.Log((double)N) / Math.Log(2.0))) - 1;

            //C  CREATE INDEX VECTOR L TO BE SORTED AND REFLECT THE RANKS
            //C  OF THE CORRESPONDING X VALUES
            for (I = 0; I < N; I++)
            {
                L[I] = I;
            }

        //C  SORT L ACCORDING TO THE X VALUES
        //C  WHILE D GREATER THAN ZERO, DO
        Label15:
            if (D > 0)
            {

                //C  COMPARE X(L(I)) AND X(L(I+D) VALUES TO SEE IF A SWITCH IS
                //C  INDICATED. IF SO, MOVE L(I) DOWN
                for (I = 0; I < N - D; I++)
                {
                    if (X[L[I]] > X[L[I + D]])
                    {
                        TEMP = L[I + D];
                        L[I + D] = L[I];

                        //C      SINCE WERE MOVING L(I) DOWN, SEE IF WE NEED TO MOVE L(I+D)

                        //C         UP MORE THAN ONE INCREMENT OF D. IF WE PASS THE FIRST
                        //C            TEST, THE ANSWER IS NO. JUST SWITCH THEM

                        if (I <= D)
                        {
                            L[I] = TEMP;
                        }
                        //C      WE DIDN'T, SO SEE HOW FAR UP THE L(I), L(I+D), ...
                        //C        SEQUENCE IT GOES.
                        //C         USE FLAG = 0 TO SAY WE'VE GONE FAR ENOUGH
                        else
                        {
                            FLAG = 1;
                            J = I - D;

                        //C      WHILE FLAG IS NOT ZERO, DO
                        Label25:
                            if (FLAG != 0)
                            {

                                //C      IF WE PASS THIS, TEMP (THE OLD L(I+D)) GOES HERE (L(J))
                                //C      NOTE: THE BASIC PROGRAM I STARTED WITH HAD .GT. HERE
                                if (X[TEMP] > X[L[J]])
                                {
                                    L[J + D] = TEMP;
                                    FLAG = 0;
                                }
                                //C      OTHERWISE MOVE L(J) DOWN ONE D INCREMENT AND TRY THE
                                //C         NEXT ONE UP THE SEQUENCE - IF THERE IS ANOTHER
                                else
                                {
                                    L[J + D] = L[J];
                                    J = J - D;

                                    //C      IF J IS .LT. ONE, WE CAN GO NO FARTHER, SO PUT TEMP HERE
                                    if (J < 1)
                                    {
                                        FLAG = 0;
                                        L[J + D] = TEMP;
                                    }
                                }

                                goto Label25;
                            }
                        }
                    }
                }



                //C      DECREMENT D IF THE RESULT WILL BE .GT. ZERO

                D = (int)(D / 2);

                goto Label15;
            }
        }

        //C***********************************************************************
        //C    A SUBROUTINE TO find section locations that minimize the penalty
        //C         for a given permutation. 
        //C         a complete solution algorithm - no reoptimizations
        //C         used primarily to find the locations and penalties for an 
        //C             initial permutation.
        //C         may also be used to find the average delta to use in 
        //C             calculating the initial Simulated Annealing parameters
        //C         may also be used to do all solutions, to examine new ideas,
        //C             but it will be SLOW
        //C
        //C                       PROGRAMMERS: BILL KEMPLE & PETE SADLER
        //C                       LAST UPDATE: Aug 18th 2011 - for Intell 11.1
        //C
        //C
        //C     INPUTS:
        //C         the given permutation is passed in as HPERM
        //C
        //C         GETPEN and the subroutines it calls are problem aware. they
        //C             use info in the COMMONS
        //C   
        //C         STATIC COMMON
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
        //C
        //C     SUBROUTINES CALLED: 
        //C         the penalty within a section is found using SCTPEN
        //C***********************************************************************
        //CPMS  CTRGET - initially set to 0
        //CPMS         - set to 1 for any CTRSECT = 1,
        //CPMS         - reported via CTRF from SCTPEN
        //CPMS         - finally reported back as CTRF
        //C----------------------------------------------------------------------
        public static void GETPEN(int[] HPERM, ref double HPEN)
        {
            int JOPT, CTRGET;
            double PENJ;

            //C***********************************************************************
            //C     note that GETPEN is not generic. 
            //C     it needs problem specific info, so it has access to the commons
            //C     but it passes info with the calling routine (initially ANNEAL)
            //C     in the CALL statement
            //C**********************************************************************
            //C   HPERM(NEVNT) IS a VECTOR OF events (solution)
            //*         similar to the dynamic INTEGER variable PERM
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
            //C            SUBROUTINE "SCTPEN" finds 
            //C                 OPTIMUM PLACEMENTS (levels) IN SECTION JOPT 
            //C                     FOR THE OBSERVED EVENTS (in the array HSCTSOL)
            //C                 and returns the penalty due to section JOPT (in PENJ)
            //C
            //C   COMPUTE THE PENALTY FOR HPERM
            //C
            //C----------------------------------------------------------------------
            COMMOD COMMOD9 = COMMOD.Singleton();

            JOPT = 0;
            COMMOD9.CTRF = 0;
            CTRGET = 0;
            HPEN = 0.0;
            COMMOD9.NGHPEN = 0.0;
            Helper.SetVal(COMMOD9.COLPEN, 0.0);          

            //C----------------------------------------------------------------------
            for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
            {

                //C  PLACE EVENTS AND CALCULATE PENALTY FOR SECTION "JOPT"               

                Helper.SCTPEN(HPERM, JOPT, out PENJ);

                if (COMMOD9.CTRF == 1)
                {
                    CTRGET = 1;
                }

                //C   ACCUMULATE SECTION PENALTIES TO OVERALL PENALTY
                HPEN = HPEN + PENJ;

                COMMOD9.COLPEN[JOPT] = PENJ;

            }

           
            //CPMS-------------------------------------------------------------------
            //cpms  getpen is called only outside anneal
            //cpms  so we can afford to call JSPAN every time

            JSPAN(HPERM);

            if (COMMOD9.JSPANF == 1) HPEN = HPEN + COMMOD9.SPANPEN;
                     

            //CPMS-------------------------------------------------------------------
            //CPMS   determine any additional penalty due to the smoothing and squeezing 
            //c      terms;  do this on all runs so that smoothness penalty can be reported
            //c      GETPEN runs only at beginning and end.  NEWPEN does not
            //c      determine smoothness penalty unless part of optimization

            //TODO:
            Helper.TWOPEN(HPERM, ref HPEN);
                     

            COMMOD9.CTRF = CTRGET;


        }

        //C***********************************************************************
        //C                       PROGRAMMER: BILL KEMPLE & PETE SADLER
        //C                       LAST UPDATE: AUG 16th 2011
        //C***********************************************************************
        //C   EACH TIME A PERMUTATION IS FIXED, EVENTS ARE
        //C      PLACED AND THE TOTAL PENALTY IS COMPUTED FOR EACH SECTION.
        //C   THIS SUBROUTINE COMPUTES THE MINIMUM PENALTY THAT CAN BE ACHIEVED
        //C      IN SECTION "JOPT" AND STILL COMPLY WITH PERMUTATION "HPERM"
        //C
        //C     REAL VARIABLES:
        //C         LPEN - array giving penalty at each level of section JOPT
        //C                 updated as events are placed
        //C         PENJ - THE TOTAL PENALTY FOR SECTION JOPT DUE TO COMPLYING
        //C                 WITH "HPERM"
        //C     INTEGER VARIABLES:
        //CPMS    HSCTSOL - solution as horizon placements;  built piecemeal 
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
        //CPMS
        //CPMS      EPEN   - calculates penalty for events in sections where they
        //CPMS               have been found
        //CPMS      NEGPEN - calculates penalty for events in sections where they
        //CPMS               have not been found
        //CPMS               i.e.  if NEGATF.gt.0
        //CPMS
        //C***********************************************************************
        public static void SCTPEN(int[] HPERM, int JOPT, out double PENJ)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            int I;
            double PENDOWN, PENHERE;
            double[] LPEN = new double[COMMOD9.MXLVL];
            int DOWN, HERE, INOW, ITGT, LPREV, PLACED, THISONE, CTRSECT, CTRHERE, CTRDOWN;


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
            //C-------------------------------
            //c     initialize:
            COMMOD9.CTRF = 0;
            CTRSECT = 0;
            CTRHERE = 0;
            CTRDOWN = 0;
            //c     initialize more for Intel:
            DOWN = 0;
            HERE = 0;
            INOW = 0;
            ITGT = 0;
            //c     not JOPT  ! intent is INPUT!            
            PLACED = 0;
            THISONE = 0;
            I = 0;
            //c
            //c     not HPERM     ! intent is INPUT!

            //CPMS  blank the section penalty:        
            PENJ = 0.0;  //OK it is passed for OUTPUT
            //CPMS  initialize for Intel
            PENDOWN = 0.0;
            PENHERE = 0.0;
            //CPMS  start at base level:        
            INOW = 0;//<=HXD
            LPREV = 0;//<=HXD
            THISONE = 0;//<=HXD
            //CPMS  zero out the array of level penalties:     
            Helper.SetVal(LPEN, 0.0);

            //C*********************************************
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
            //C----------------------------------------------------------------------

            for (INOW = 0; INOW < COMMOD9.NEVNT; INOW++)
            {
                //CPMS     loop through all events in HPERM order from base
                //C        ----------------------------------------------------------------------
                //CPMS     THE FOLLOWING OPTIONS ARE NOT IMPLEMENTED:
                //CPMS     -1 means extends up out of section, highest level in section is target
                //CPMS     -2 means extends down out of section, level 1 is target
                //cpms          IF(ISTATIC(HPERM(INOW), JOPT, 1).GE.0) THEN
                //cpms              ITGT = ISTATIC(HPERM(INOW), JOPT, 1)
                //cpms          ELSE IF(ISTATIC(HPERM(INOW), JOPT, 1).EQ.-1) THEN    
                //cpms              ITGT = HLEVEL(JOPT)
                //cpms          ELSE IF(ISTATIC(HPERM(INOW), JOPT, 1).EQ.-2) THEN    
                //cpms              ITGT = 1
                //cpms          END IF             
                //C        ------------------------------------------------------------
                //C        find the target for this event
                //CPMS     look up event level in section J
                //CPMS     0=not observed    >0 gives observed level, so store to target

                ITGT = COMMOD9.ISTATIC[HPERM[INOW], JOPT, 0];

                //CPMS#########################################################
                //cpms######GIVING A TARGET TO MISSING EVENTS##################
                //cpms######CAN CAUSE THE SEARCH TO STALL######################
                //cpms######FADs FREEZE FIRST##################################
                //cpms      if negative evidence is also a basis for penalties
                //cpms      set the target at the nearest section limit
                //cpms      the target should be the section end nearest range end 
                //*         IF((NEGATF.gt.0).and.(ITGT.eq.0)) THEN
                //cpms         FAD target is top of section, because the real problem
                //cpms         arises when FADs sink too far down-sequence
                //cpms         (NOT nearest end of section to LPREV)
                //*             IF(IROWS(HPERM(INOW),2).eq.1) ITGT = HLEVEL(JOPT)
                //cpms	       LAD target is either the FAD level or the section base
                //cpms         (FAD must be earlier in HPERM)
                //cpms         section-base can be found without reference to FAD
                //cpms         only snag arises when FAD is at top of section,
                //cpms         then the LAD should have no penalty for being there
                //*	         IF(IROWS(HPERM(INOW),2).eq.2) ITGT = 1 
                //cpms         if not a FAD or LAD, no negative penalty anyway
                //*         ENDIF
                //cpms      ---------------------------------
                //cpms      Even if missing events are not given targets, the negative
                //cpms      evidence still influences the placements because the net
                //cpms      negative penalty will change as the events are pushed
                //cpms      downwards.
                //cpms      This is more proactive than TEASE
                //cpms      which does not count the negative
                //cpms      penalties until all horizon
                //cpms      placements have been decided on
                //cpms      the basis of positive evidence
                //CPMS#######################################

                if (ITGT == -1)//<=HXD
                {
                    //CPMS  ----CASE A. NO datum this event this section, PUT IT AT LPREV
                    //CPMS              if not observed, [and negatf=0]
                    //CPMS              place at highest previously occupied level
                    //CPMS              i.e. tied with last of preceding events;
                    //CPMS              no penalty: if negatf=o, or ITGT has been set
                    //CPMS              if TGT's are dropped, do penalty for negatf>0

                    COMMOD9.HSCTSOL[HPERM[INOW], JOPT] = LPREV;
                    //cpms        next line would not be necessary, if TGTs were set for negatives
                    //cpms        but it should be safe to leave in
                    if (COMMOD9.NEGATF != 0) PENHERE = LPEN[LPREV] + Helper.NEGPEN(HPERM[INOW], HERE, JOPT);

                }
                else if (LPREV <= ITGT)
                {
                    //CPMS   -----CASE B. last placed is below this event's datum,
                    //CPMS                PUT THIS EVENT AT ITS datum AND UPDATE LPREV 
                    //CPMS                i.e. target is above highest used event
                    //CPMS                so place at target with no penalty;  update LPREV
                    //CPMS                order is preserved; later placements lower the placement
                    COMMOD9.HSCTSOL[HPERM[INOW], JOPT] = ITGT;
                    LPREV = ITGT;
                }
                else
                {
                    //CPMS    -----CASE C. the hard one, one or more events are placed above this 
                    //C                    event's datum. FIND THE LOCATION WITH THE SMALLEST 
                    //C            COMBINED PENALTY
                    //CPMS   NOT ELSEIF, because it is the only remaining option
                    //CPMS       placement of earlier events has already extended to
                    //CPMS       horizons higher than target.  IF INOW is an LAD,
                    //CPMS       extension penalty arises; if an FAD, contraction penalty arises
                    //CPMS       later try to reduce penalty by moving whole group down.
                    //C
                    //C      Place this event (row HPERM(INOW) in the arrays) 
                    //C      AT LPREV AND MOVE it down, 
                    //C      TAKING OTHERS ALONG AS NECESSARY TO PRESERVE HPERM ORDER
                    //c         1. calc pen for all at LPREV, including current
                    //c         2. calc pen for all if moved to LPREV - 1
                    //c         3. if pen moved down is less, decrement LPREV and repeat
                    //c
                    //c       THISONE and HERE are temp variables for INOW and LPREV
                    //c       so they can be decremented without losing the current values.
                    //c
                    //c       DOWN is the level below HERE
                    //c
                    //c       LPEN is used to hold the penalty increment at each
                    //c       level in the current section (sum of the penalties
                    //c       for all events placed at this level
                    //c
                    //c       Only Case-C placements will cause penalties
                    //cpms    IF NEGATF>0 some of the C placements will need EPEN
                    //cpms    and others will need NEGPEN -- so we need to check
                    //cpms    NEGATF and ISTATIC before any EPEN/NEGPEN call
                    //cpms
                    //C-------------initial setup
                    //C             place current event at LPREV, set some temp variables, 
                    //C             compute the penalty at LPREV, and set a flag saying the event 
                    //C             has not been placed yet
                    //C----------------------------------------------------------------------
                    COMMOD9.HSCTSOL[HPERM[INOW], JOPT] = LPREV;
                    //CPMS   add INOW to developing horizon solution
                    //CPMS   set up temporary variables to decrement              
                    THISONE = INOW;
                    HERE = LPREV;
                    COMMOD9.CTRF = 0;
                    //CPMS    add INOW penalty to temporary level penalty              
                    //CPMS         PENHERE does not accumulate so it is never emptied
                    //CPMS         it is always replaced in one shot
                    //CPMS    ####### CHECK FOR NEGATIVE PENALTIES ##################
                    //cpms    do common case first!
                    if (COMMOD9.ISTATIC[HPERM[INOW], JOPT, 0] != -1)//<=HXD
                    {
                        PENHERE = LPEN[LPREV] + Helper.EPEN(HPERM[INOW], HERE, JOPT);
                    }
                    else if (COMMOD9.NEGATF > 0)
                    {
                        PENHERE = LPEN[LPREV] + Helper.NEGPEN(HPERM[INOW], HERE, JOPT);
                    }

                    //CPMS    ##################			 
                    //CPMS    open option to move
                    PLACED = 0;
                    if (COMMOD9.CTRF == 1) CTRHERE = 1;

                    //C----- BIG LOOP ----------------------------------------------
                    //CPMS          repeat until the placement with the smallest penalty
                    //CPMS          is found (PLACED = 1) 
                    //CPMS          or events have moved down to the base (LEVEL 1)
                    //C-----------------------------------------------------------------
                    while (HERE > 0)//<=HXD
                    {

                        //CPMS          set up temporary variable for the next level down:              
                        DOWN = HERE - 1;
                        //CPMS          set up 0 variable to accumulate penalty increments
                        //CPMS          for events moving down to DOWN                  
                        PENDOWN = 0.0;

                        //CPMS          SMALL LOOP ------------------------------------- 
                        //CPMS               find penalty for moving all at HERE down 1 level
                        //CPMS               work down through earlier events as long as they 
                        //CPMS               exist (>0) and have been placed at or above the
                        //CPMS               current level (i.e. reach up and fetch down)   
                        //C-------------------------------------------------------------------
                        if (THISONE >= 0)//HACK: May be 0
                        {
                            while ((COMMOD9.HSCTSOL[HPERM[THISONE], JOPT] >= HERE))
                            {
                                //CPMS   - work back through all earlier events on HPERM
                                //CPMS     at level HERE     
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
                                //CPMS    - total up penalty at new low level due to events
                                //CPMS      that have just been moved down    
                                if (THISONE == 0)
                                    break;//<=0

                                //CPMS    select earlier event if there is one i.e. THISONE.gt.1 
                                THISONE = THISONE - 1;

                                if (COMMOD9.CTRF == 1) CTRDOWN = 1;

                            }
                        }//the THISONE condition  

                        //CPMS     see if DOWN is going to be as far as we move:
                        //C        check to see if DOWN is better than HERE (PENDOWN < PENHERE)
                        //C        if it is, 
                        //C         check if DOWN is bottom (level 1).
                        //C         if it is,
                        //C          place group at DOWN and update things
                        //C          if not, 
                        //C           get ready for the next pass through big loop
                        //C
                        //C        note that the penalty at the new HERE is the penalty for moving
                        //C        all the guys above down to HERE (PENDOWN) plus the penalty
                        //C        for guys that are already there (LPEN(DOWN))
                        //C----------------------------------------------------------------------
                        //CWGK       20 dec 94 -- I duplicated some code below to correct an error
                        //CWGK       it should be made cleaner
                        //C----------------------------------------------------------------------
                        //CPMS       if penalty has improved, prepare to try going down farther
                        //CPMS       move HERE down  
                        if (PENDOWN <= PENHERE)
                        {                           

                            //penalty has improved 
                            HERE = HERE - 1;
                            PENHERE = PENDOWN + LPEN[DOWN];

                            //add extra penalty and earlier penalty for new HERE
                            //unless at base; in that case wrap up  

                            if (HERE == 0)//<=HXD
                            {
                                //at base of section!  wrap up!! 
                                for (I = INOW; I >= THISONE; I--)//HACK:DO I=INOW,THISONE+1,-1 IN FORTRAN
                                {
                                    //update horizon array for all moved events
                                    COMMOD9.HSCTSOL[HPERM[I], JOPT] = HERE;
                                }

                                //set flag for end:
                                PLACED = 1;
                                if (CTRDOWN == 1) CTRSECT = 1;
                                CTRDOWN = 0;
                                CTRHERE = 0;
                                //update LPEN at HERE: 
                                LPEN[HERE] = PENHERE;

                                for (I = HERE + 1; I <= LPREV; I++)//HACK:DO I = HERE+1, LPREV IN FORTRAN
                                {
                                    //reset to zero all levels above HERE up to LPREV
                                    LPEN[I] = 0.0;
                                }


                                //move LPREV to HERE; no higher events remain 
                                LPREV = HERE;
                                //Helper.Write("PENDOWN <= PENHERE LPREV={0}\n", LPREV);
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
                            //CPMS  if penalty is worse (PENDOWN>PENHERE), prepare to stop
                            //CPMS                                      at HERE
                            //C     if DOWN is not better than HERE, HERE is best. 
                            //C        place all that need to move HERE
                            //C        set PLACED to 1
                            //C        update LPEN(HERE)
                            //C        zeroize LPEN for levels above HERE
                            //C        update LPREV
                            //CPMS       penalty has not improved, don't move on down
                            for (I = INOW; I >= THISONE+1; I--)//HACK: DO I = INOW,THISONE+1,-1 IN FORTRAN
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
                            for (I = HERE + 1; I <= LPREV; I++)//HACK:DO I = HERE+1, LPREV IN FORTRAN
                            {
                                LPEN[I] = 0.0;
                            }

                            //update LPREV - move it to HERE, no events go higher
                            LPREV = HERE;                           
                        }


                        //C-----------------------------------------------------------------
                        //C             go back to top of big loop 
                        //C             if PLACED = 0, this event not placed, repeat the loop
                        //C             not if PLACED = 1, this event has been placed -- for now
                        //C             go to the overall loop and place the next guy
                        //C----------------------------------------------------------
                        if (PLACED == 1) break; //moved from DO WHILE (().and.()) condition


                    }//while HERE>0
                    //C-----------end of big loop ---------
                    //C           the event has been placed
                    //C------------------------------------

                }

            }//End for(INOW=0;INOW<COMMOD9.NEVNT;INOW++)

            //C-------------------------------------------------------------
            //C-----all events placed, highest at HERE, now calc PENJ
            //CPMS--add up all level penalties to get section penalty (JPEN)
            //C-------------------------------------------------------------
            for (I = 0; I <= HERE; I++)//<=HXD
            {
                PENJ = PENJ + LPEN[I];

                //Helper.Write("LPEN[{0}]={1}\n",I,LPEN[I]);
            }

            /*
            for (int t = 0; t < COMMOD9.NEVNT; t++)
            {
                string FADLAD = (COMMOD9.ISTATIC[t, 0, 1] == 1 ? "FAD" : (COMMOD9.ISTATIC[t, 0, 1] == 2 ? "LAD" : "--"));
                Helper.Write("IROWS[{0},0]={1}[E] ISTATIC[{0},0,0]={2}[L] {3}\n", t, COMMOD9.IROWS[t, 0], COMMOD9.ISTATIC[t, 0, 0], FADLAD);

            }

            Helper.Write("\n");

            for (int t = 0; t < COMMOD9.NEVNT; t++)
            {
                int newp = COMMOD9.INIPERM[t];
                string FADLAD = (COMMOD9.ISTATIC[newp, 0, 1] == 1 ? "FAD" : (COMMOD9.ISTATIC[newp, 0, 1] == 2 ? "LAD" : "--"));
                Helper.Write("IROWS[{0},0]={1}[E] ISTATIC[{0},0,0]={2}[L] {3}\n", newp, COMMOD9.IROWS[newp, 0], COMMOD9.ISTATIC[newp, 0, 0], FADLAD);

            }

            Helper.Write("\n");

            for (I = 0; I < COMMOD9.NEVNT; I++)//<=HXD
            {
                int newp = COMMOD9.INIPERM[I];
                Helper.Write("HSCTSOL[{0},0]={1}\n", newp, COMMOD9.HSCTSOL[newp, 0]);

                //Helper.Write("LPEN[{0}]={1}\n",I,LPEN[I]);
            }

            Helper.Write("\n");

           */
           
            //C-------------------------

            COMMOD9.CTRF = CTRSECT;
            CTRSECT = 0;

        }

        //C***********************************************************************
        //C   REAL FUNCTION EPEN               
        //C
        //C                       PROGRAMMERS: BILL KEMPLE & PETE SADLER
        //C                       LAST UPDATE: January 18th  2011 - for Intel 11.1
        //C
        //CPMS  RETURNS penalty for given event in given section at given level
        //C
        //CPMS  RETURNS PENALTIES FOR POSITIVE EVIDENCE
        //CPMS  NEGPEN.FOR will replace EPEN for violations of negative evidence
        //CPMS     
        //C
        //C     RETURNS THE PENALTY RESULTING FROM PLACING EVENT in EVENTROW of 
        //C         the static arrays AT Level "LEVEL" IN SECTION "JOPT"
        //C     IF THE EVENT IS NOT OBSERVED IN SECTION "JOPT", 
        //C         IT DOESN'T CONTRIBUTE TO THE PENALTY
        //C     IF THE event is PLACEd above or below its DATA, THE PENALTY IS 
        //*         THE PRODUCT OF THE appropriate WEIGHT (weight up or down) 
        //*         AND THE magnitude of the displacement (perhaps augmented, 
        //*         see following)
        //C     EMOVES is loaded by checking ISTATIC(EVENTROW,JOPT,2)
        //C         to see what kind of moves are allowed for this
        //C         event. if the placement is not allowed, the multiplyer LARGE
        //C         is used to discourage it.  
        //cpms  THUS the allowed moves are not assumed from event type
        //cpms       THIS SHOULD MEAN THAT NEW EVENT TYPES ARE AUTOMATICALLY OK
        //cpms  FOUR allowed moves are recognized
        //cpms       0   no moves
        //cpms       1   down only
        //cpms       2   up only
        //cpms       3   up and down
        //cpms
        //cpms
        //CPMS  CTRF is set to 1 if a contraction (LARGE) is involved
        //CPMS  SECTPEN must undo the flag if the contraction is not accepted
        //C***********************************************************************
        public static double EPEN(int EVENTROW, int LEVEL, int JOPT)
        {
            double retVal = 0.0;

            int ELVL;      // observed level of given event in given section
            int EMOVES;    // permissible adjustments of given event
            double DIST;   // separation of observed and placed levels
            double WEIGHT; // weight factor for premitted adjustments

            //c     Intialize internal variables
            ELVL = 0;
            EMOVES = 0;
            WEIGHT = 0.0; //weight factor for event adjustments
            DIST = 0.0;  //separation of observed and placed levels in JOPT
            //c     Zero out some COMMOD variables
            COMMOD COMMOD9 = COMMOD.Singleton();
            COMMOD9.CTRF = 0;        // flag for contractions
            //c     Get observed level
            ELVL = COMMOD9.ISTATIC[EVENTROW, JOPT, 0];

            if ((ELVL == -1) || (ELVL == LEVEL))//<=HXD
            {
                //cpms     no penalty
                //cpms     event not seen in section JOPT  
                //cpms     or event's datum level = LEVEL

                // penalty for this event in this section at this level
                retVal = 0.0;
                DIST = 0.0;
            }
            else
            {
                //C  there is a penalty of type determined by PENF
                //C  1. compute distance from observed to placed 
                if ((COMMOD9.PENF == 0) || (COMMOD9.PENF > 1))
                {
                    //c   report interval penalty after run even if
                    //c   optimization used an ordinal, spatial, or rascal penalty
                    DIST = COMMOD9.VALEVEL[ELVL, JOPT] - COMMOD9.VALEVEL[LEVEL, JOPT];
                   
                    //cpms  i.e. DIST = observed minus placed
                    //cpms       negative if displaced up
                    //cpms       positive if displaced down
                }
                else if (COMMOD9.PENF == 1)
                {
                    DIST = (double)(ELVL - LEVEL);
                }
                else if (COMMOD9.PENF == -1)
                {
                    //c          --------larger than ORDINAL-------------
                    //c          ----counts events at home level---------
                    //c	     DIST = REAL(ELEVEL(elvl,JOPT) - ELEVEL(LEVEL,JOPT)) 
                    //c          --------smaller than ORDINAL-------------
                    //c          ----does not count home level------------
                    if ((ELVL - LEVEL) > 1)
                    {
                        DIST = 1.0 + (double)(COMMOD9.ELEVEL[ELVL - 1, JOPT] - COMMOD9.ELEVEL[LEVEL, JOPT]);
                    }
                    else if ((ELVL - LEVEL) < -1)
                    {
                        DIST = (double)(COMMOD9.ELEVEL[ELVL + 1, JOPT] - COMMOD9.ELEVEL[LEVEL, JOPT]) - 1.0;
                    }
                    else if (Math.Abs(ELVL - LEVEL) == 1)
                    {
                        DIST = (double)(ELVL - LEVEL);
                    }
                    else if (ELVL == LEVEL)
                    {
                        DIST = 0.0;
                    }
                }


                //cpms     2. find right weight
                if (DIST < 0.0)
                {

                    //cpms       negative: need weight for moving up
                    WEIGHT = COMMOD9.RSTATIC[EVENTROW, JOPT, 0];
                }
                else
                {
                    //cpms       positive: need weight for moving down
                    WEIGHT = COMMOD9.RSTATIC[EVENTROW, JOPT, 1];
                }
                //C-----   --------------------------------------------
                //C        for debugging
                //C-----   --------------------------------------------
                //c        IF(LOGEPEN.EQ.1) THEN   
                //c           WRITE(*,*)'IN EPEN after dist and weight'
                //c        END IF    
                //C-----   --------------------------------------------
                //C        next, compute the penalty 
                //C        based on the allowable moves -  EMOVES
                EMOVES = COMMOD9.ISTATIC[EVENTROW, JOPT, 1];

                switch (EMOVES)
                {
                    case -1://<=HXD
                    case 0:
                        //no moves are allowed, use LARGE 
                        retVal = Math.Abs(DIST) * WEIGHT * COMMOD9.LARGE;
                        break;
                    case 1:
                        //only down moves allowed (DIST > 0),
                        //if otherwise, use LARGE 
                        if (DIST >= 0)
                        {
                            retVal = DIST * WEIGHT;
                        }
                        else
                        {
                            retVal = Math.Abs(DIST) * COMMOD9.LARGE;
                          
                            if (WEIGHT > 0.0) retVal = retVal * WEIGHT;
                            COMMOD9.CTRF = 1;
                        }
                        break;
                    case 2:
                        //only up moves allowed (DIST < 0),
                        //if otherwise, use LARGE
                        if (DIST <= 0)
                        {
                            retVal = Math.Abs(DIST) * WEIGHT;
                        }
                        else
                        {
                            retVal = DIST * COMMOD9.LARGE;
                            if (WEIGHT > 0.0) retVal = retVal * WEIGHT;
                            COMMOD9.CTRF = 1;
                        }
                        break;
                    case 3:
                        //all moves are allowed
                        retVal = Math.Abs(DIST) * WEIGHT;
                        break;
                }
            }


            //if (retVal >= 7777)
            //{
            //    Helper.Write("EVENTROW={0}, LEVEL={1}, JOPT={2}, EPEN={3}\n", EVENTROW, LEVEL, JOPT, retVal);
            //}

            return retVal;
        }

        //CPMS*****************
        //CPMS  A version of EPEN designed to handle penalties for 
        //CPMS  negative evidence
        //CPMS  It should be called only for events that can incur
        //CPMS  negative-evidence penalties
        //CPMS  This reduces the complexity of EPEN, which is the most frequently
        //CPMS  called module in the set and should not be bogged down checking 
        //CPMS  for negative evidence at every call
        //C***********************************************************************
        //C   REAL FUNCTION NEGPEN (EVENTROW PENALTY)               
        //C
        //C                       PROGRAMMERS: PETE SADLER
        //C                       LAST UPDATE: Nov 9th 1999
        //C
        //C***********************************************
        public static double NEGPEN(int EVENTROW, int LEVEL, int JOPT)
        {
            double retVal = 0.0;

            double DIST = 0.0;
            //C**************************************************************
            //cpms---- NEGATIVE() CELL CODES
            //cpms     0 = neither event nor coex events found in section  
            //cpms            (penalize if NEGATF = 1{SS}, or NEGATF = 2{SL})
            //cpms     1 = event found in section  (positive penalties)
            //cpms     2 = coex events found in section  
            //cpms       (penalize only if NEGATF = 1{SS})
            //cpms-----------------------------------------
            //cpms---- STACKER CODES
            //cpms     0   OFF
            //cpms     1   THRU
            //cpms     2   INCL
            //cpms     3   DIST
            //cpms     4   FREQ
            //cpms     5   EXIT
            //cpms     6   PROP
            //cpms---------------------------
            COMMOD COMMOD9 = COMMOD.Singleton();

            COMMOD9.CTRF = 0;

            //cpms--NOTE: --------------------------------------
            //cpms        we do not get into this routine unless 
            //cpms        NEGATF.gt.0    AND
            //cpms        NEGATIVE(taxon,section).ne.INT2(0)
            //cpms        event is not observed in the section JOPT
            //cpms-------------------------------------------------
            //cpms  first eliminate cases with no penalty
            if ((COMMOD9.IROWS[EVENTROW, 1] > 2) || (COMMOD9.IROWS[EVENTROW, 1] < 1) ||
                (((COMMOD9.IROWS[EVENTROW, 1] < 3) || (COMMOD9.IROWS[EVENTROW, 1] > 0)) &&
                ((COMMOD9.NEGATIVE[COMMOD9.IROWS[EVENTROW, 2], JOPT] == 2) &&
                (COMMOD9.NEGATF == 1))))
            {
                //cpms     i.e. unpaired event,
                //cpms     or paired event, coex-seen-but-negative-criterion-is-SS
                DIST = 0.0;
            }
            else
            {
                //cpms  then calculate negative penalty for other cases
                //cpms  i.e. those that are FAD or LAD and those 
                //cpms       that qualify under NEGATF=1 or NEGATF=2
                //cpms       i.e. neither event nor coex seen
                //cpms            or  coex seen and negative criterion is SS

                if (COMMOD9.IROWS[EVENTROW, 1] == 1)
                {
                    //cpms    if a FAD, then penalize distance to section-top
                    //cpms              when ranges are too long, a FAD extends
                    //cpms              down into too many sections where it
                    //cpms              has not been observed (STACKER=EXIT)

                    switch (COMMOD9.STKF)
                    {
                        case 2:
                            //INCL - penalty of 1 for every section entered
                            if (LEVEL < COMMOD9.HLEVEL[JOPT])
                            {
                                DIST = 0.5;
                            }
                            else
                            {
                                DIST = -0.5;
                            }
                            break;
                        case 5:
                            //EXIT - penalty grows with exit difference
                            if (COMMOD9.PENF == 1)
                            {
                                //DIST = REAL(HLEVEL(JOPT)-LEVEL)/REAL(HLEVEL(JOPT))
                                DIST = 1.0 - (((double)LEVEL) / (double)(COMMOD9.HLEVEL[JOPT]));//<=HXD
                            }
                            else if ((COMMOD9.PENF == 0) || (COMMOD9.PENF > 1))
                            {
                                DIST = 1.0 - (COMMOD9.VALEVEL[LEVEL, JOPT]+1)
                                    /( COMMOD9.VALEVEL[COMMOD9.HLEVEL[JOPT], JOPT]+1);
                            }
                            else if (COMMOD9.PENF == -1)
                            {
                                DIST = 1.0 - ((double)(COMMOD9.ELEVEL[LEVEL, JOPT]+1)
                                    / (double)(COMMOD9.ELEVEL[COMMOD9.HLEVEL[JOPT], JOPT]+1));//?HXD
                            }
                            break;
                    }

                }
                else if (COMMOD9.IROWS[EVENTROW, 1] == 2)
                {
                    //L = HSCTSOL(IROWS(EVENTROW,4),JOPT)

                    switch (COMMOD9.STKF)
                    {
                        case 2:
                            //INCL - penalize for extending into section
                            if (LEVEL > 1)
                            {
                                DIST = 0.5;
                            }
                            else
                            {
                                DIST = -0.5;
                            }

                            break;
                        case 5:
                            //EXIT - penalize by distance to base
                            if (COMMOD9.PENF == 1)
                            {
                                DIST = (double)(LEVEL - 1+1) / ((double)COMMOD9.HLEVEL[JOPT]+1);//<=HXD
                            }
                            else if ((COMMOD9.PENF == 0) || (COMMOD9.PENF > 1))
                            {
                                DIST = (COMMOD9.VALEVEL[LEVEL, JOPT] - 1.0) / (double)(COMMOD9.HLEVEL[JOPT]+1);//<=HXD
                            }
                            else if (COMMOD9.PENF == -1)
                            {
                                DIST = (double)(COMMOD9.ELEVEL[LEVEL, JOPT] - 1+1)
                                    / (double)(COMMOD9.ELEVEL[COMMOD9.HLEVEL[JOPT], JOPT]+1);//?HXD
                            }
                            break;
                    }


                }
            }


            //cpms  no weights for negatives

            retVal = DIST;
            COMMOD9.NGHPEN = COMMOD9.NGHPEN + DIST;

            return retVal;

        }

        //CPMS***************************************************
        //CPMS  Calculates span of all the sections that have 
        //CPMS  exclusive events at the limits of the section.
        //CPMS
        //CPMS  Programmer:          Pete Sadler
        //CPMS  Last Modification:   Apr 8th 2001
        //CPMS***************************************************
        public static void JSPAN(int[] XPERM)
        {
            int topx, btmx, J;

            COMMOD COMMOD9 = COMMOD.Singleton();

            COMMOD9.SPANPEN = 0.0;

            //CPMS*************************************
            //c    Determine whether event is
            //c    exclusive and seen at top or base of section. 
            //c    Then count range of that section in numbers of 
            //c    events in the permutation.
            //c    i.e. need to identify problem sections
            //c    during input and count only those
            //c    sections into penalty
            //c    Look for events seen in only one section
            //c    and with FADs at HLEVEL or LADs at level 1
            //c    It does not matter that there might be more
            //c    exclusive events near the end.  If there is
            //c    one, the problem can start.
            //c    This adds work at the begining; but it 
            //c    speeds up the penalty calculation with
            //c    lots of CYCLEs and it allows for the 
            //c    option of resetting the JSPANF to 0
            //c    because none of the exclusives cause trouble.
            //c
            //c    This penalty does not use HSCTSOL, so it
            //c    can work with an ordinal penalty function.
            //c
            //c    The raw penalty is effectively based in "levels" as
            //c    measured in the ORDINAL COMPOSITE section.
            //c    It is not easily taylored to INTERVAL unless one
            //c    uses the mean level spacing in a section.
            //c    To do this, would add time for little good.
            //c
            //c    If a FAD and LAD are at section limit,
            //c    it is hard to move the inner event of the pair.
            //CPMS**************************************************

            for (J = 0; J < COMMOD9.JEXCL; J++)
            {
                //*      DO J=1,NSCT
                //cpms    if no exclusives at section end, cycle
                //cpms    otherwise get range of section in PERM
                //*	  IF(SECTPROP(J,2).le.0) CYCLE
                //*       EXCLSECT contains only sections that satisfy line above
                topx = COMMOD9.NEVNT-1;//<=HXD
                //CPMS    count down to first highest event that is seen in the section
                //CPMS    topx is a big number - counting down from NEVNT
                while (COMMOD9.ISTATIC[XPERM[topx], COMMOD9.EXCLSECT[J], 0] == -1)//<=HXD
                {
                    topx = topx - 1;
                }

                btmx = 0;//<=HXD
                //CPMS    count up through PERM to first observed event that in section
                //CPMS    is not a FAD  (criterion removed !)
                //CPMS    btmx is a small number - counting up from 1
                while (COMMOD9.ISTATIC[XPERM[btmx], COMMOD9.EXCLSECT[J], 0] == -1)//<=HXD
                {
                    btmx = btmx + 1;
                }

                //CPMS    increment the JSPAN penalty 
                if (topx > btmx) COMMOD9.SPANPEN = COMMOD9.SPANPEN + (double)(topx - btmx);

            }

            if (COMMOD9.JEXCL > 0)
            {
                COMMOD9.SPANPEN = COMMOD9.SPANPEN / COMMOD9.JEXCL;//<=HXD  HACK: 
            }

        }

        //C******************************************************
        //C     A SUBROUTINE TO calculate the secondary penalties
        //C     in full at beginning and end of optimization
        //C     i.e. called from GETPEN and DEMPEN 
        //C
        //C     If the main penalty is ordinal, it does not solve 
        //C     the placement problem and so has no basis (HSCTSOL)
        //C     for secondary penalties that need horizons     
        //C
        //C          PROGRAMMER: PETE SADLER
        //C          LAST UPDATE: Jan 27th 2000
        //C
        //C------------------------------------------------
        public static void TWOPEN(int[] HPERM, ref double HPEN)
        {
            double XPEN = 0.0;

            //C****************************************************************
            COMMOD COMMOD9 = COMMOD.Singleton();

            if (COMMOD9.PENF <= 1)
            {
                //CPMS     determine any additional penalty due to the smoothing term
                Helper.SMOOTH(HPERM);

                //CPMS     update overall penalty
                if (COMMOD9.KSM > 0.00) HPEN = HPEN + (COMMOD9.SMPEN * COMMOD9.KSM);

                //CPMS--------------------------------------------------------------
                //C        determine any additional penalty due to the squeezing factor
                Helper.SQUEEZE(HPERM);

                //CPMS     update overall penalty
                if (COMMOD9.KSQ > 0.00) HPEN = HPEN + (COMMOD9.SQPEN * COMMOD9.KSQ);

                //CPMS--------------------------------------------------------------
                //C        determine any additional penalty due to the shrinking factor
                Helper.SHRINK(HPERM);

                //CPMS     update overall penalty
                if (COMMOD9.KSH > 0.00) HPEN = HPEN + (COMMOD9.SHPEN * COMMOD9.KSH);

            }

            //CPMS--------------------------------------------------------------
            //C     determine any additional penalty due to the teasing factor
            //C     options 'OLAP'(STKF.eq.7) and 'COEX'(8) do not require HSCTSOL
            if (COMMOD9.STKF == 8)
            {
                Helper.ROYAL(HPERM, ref XPEN);
                COMMOD9.TSPEN = XPEN;
            }
            else if (COMMOD9.STKF == 9)
            {
                if (COMMOD9.FB4LF != 0) SEQUEL(HPERM, ref XPEN);
                COMMOD9.TSPEN = XPEN;
            }
            else if ((COMMOD9.KTS > 0.00) || (COMMOD9.STKF == 7))
            {
                TEASE(HPERM);
            }


            //CPMS     update overall penalty
            if (COMMOD9.KTS > 0.00) HPEN = HPEN + (COMMOD9.TSPEN * COMMOD9.KTS);

        }

        //C***********************************************************************
        //C    A SUBROUTINE TO calculate the penalty based upon sequence mis-matches
        //C
        //C                       PROGRAMMER: PETE SADLER
        //C                       LAST UPDATE: June 4th 1997
        //C
        //C
        //C     INPUTS:
        //C         the given permutation is passed in as HPERM
        //C
        //C         DEMPEN and the subroutines it calls are problem aware. they
        //C             use info in the COMMONS
        //C   
        //C         STATIC COMMON
        //C             events are in IROWS
        //C             levels where they were recovered are in ISTATIC
        //C             allowable moves for each event are also in ISTATIC
        //C             weights are in RSTATIC
        //C             data values are cross referenced to levels in VALEVEL
        //C
        //C             the universal control for range contractions is in CONTRACT
        //C             DOINPUT already adjusted ISTATIC accordingly
        //C
        //C         PENF = 2  ordinal penalty   (sum of pairwise mismatches)
        //C              = 3  spatial penalty   (sum of mismatch separations)
        //C              = 4  rascal penalty    (sum of mismatch fractions)
        //C              = 5  royal penalty     (excess coexistences)
        //C              = 6  sequel penalty    (excess FAD-b4_LADs)
        //C              = 7  momental penalty  (number of skipped events as fraction of total events)
        //C              < 2  call should have gone to GETPEN  
        //C                   except at end of run (then behaves as if PENF=2)
        //C
        //C     OUTPUTS: 
        //C         the penalty for HPERM is returned in HPEN
        //C
        //C-----------------
        //C     WEIGHTS:
        //C         ordinal penalties do not handle weights well, they work pairs;
        //C		zero-weighted events should not be omitted, this leads to 
        //C		sequences that cannot generate horizons without contractions
        //C***********************************************************************
        //*     FLIB.FI and FLIB.FD ARE INCLUDED SO WE CAN USE MS ROUTINES LIKE
        //*         RANDOM NUM GEN
        //*     it seems necessary to put the interface include before any other
        //*         statements, even PROGRAM or SUBROUTINE
        //C----------------------------------------------------------------------
        public static void DEMPEN(int[] HPERM, ref double HPEN)
        {
            double PENJ;
            int JOPT, SEP, I, K, statij;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //C***********************************************************************
            //C     note that DEMPEN is not generic. 
            //C     it needs problem specific info, so it has access to the commons
            //C     but it passes info with the calling routine (initially ANNEAL)
            //C     in the CALL statement
            //C**********************************************************************
            //C   HPERM(TMXSPC+MXOTHR) IS a VECTOR OF events (solution)
            //*         similar to the dynamic INTEGER variable PERM
            //C
            //C         A PROPER ORDERING OF THESE NEVNT EVENTS IS ONE OVERALL
            //C            OBJECTIVE OF THE CORRELATION
            //C----------------------------------------------------------------------
            //C   THE OVERALL PENALTY FOR A GIVEN PERMUTATION CAN
            //C      BE FOUND BY SUMMING ACROSS ALL SECTIONS
            //c----------------------------------------------------------------------
            //C   The democratic penalty does not require local placements 
            //C   It does not use stratigraphic distance
            //C----------------------------------------------------------------------
            //C
            //C   COMPUTE THE PENALTY FOR HPERM
            //C
            //C----------------------------------------------------------------------
            HPEN = 0.0;
            statij = -1;//<=HXD
            Helper.SetVal(COMMOD9.COLPEN, 0.0);

            if (COMMOD9.PENF <= 3 || COMMOD9.PENF == 7)
            {
                for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                {
                    PENJ = 0.0;

                    for (I = 0; I < COMMOD9.NEVNT - 1; I++)
                    {
                         //ignore if not found or zeroed out (up and down)
                        if(Helper.IORZERO(HPERM[I],JOPT))continue;
                        statij=COMMOD9.ISTATIC[HPERM[I],JOPT,0];

                        for (K = I + 1; K < COMMOD9.NEVNT; K++)
                        {
                            //cpms  ignore if not found or zeroed out (up and down)
                            if (Helper.IORZERO(HPERM[K], JOPT)) continue;

                            SEP = (statij) - (COMMOD9.ISTATIC[HPERM[K], JOPT, 0]);
                            //CPMS  simple ordinal penalty
                            if ((COMMOD9.PENF < 3) && (SEP > 0))
                            {
                                //CPMS   needs to work at end of program if PENF.LT.2
                                //CPMS   called to get simple ordinal penalty!     
                                PENJ = PENJ + 1;
                                //CPMS   spatial penalty
                            }
                            else if ((COMMOD9.PENF == 3) && (SEP > 0))
                            {
                                PENJ = PENJ + SEP;
                                //c   momental penalty
                            }
                            else if ((COMMOD9.PENF == 7) && (SEP > 0))
                            {
                                PENJ = PENJ + (double)(K - I) / (double)(COMMOD9.NEVNT);
                            }
                        }
                    }
                    HPEN = HPEN + PENJ;
                    COMMOD9.COLPEN[JOPT] = PENJ;
                }
            }
            else if (COMMOD9.PENF == 4)
            {
                if (COMMOD9.RASCon) goto Label143;

                //c  Rascal Penalty without RASC()
                for (I = 0; I < COMMOD9.NEVNT - 1; I++)
                {
                    for (K = I + 1; K < COMMOD9.NEVNT; K++)
                    {
                        PENJ = 0.0;
                        SEP = 0;

                        for (JOPT = 0; JOPT < COMMOD9.NSCT; JOPT++)
                        {
                            if (Helper.IORZERO(HPERM[I], JOPT)) continue;
                            if (Helper.IORZERO(HPERM[K], JOPT)) continue;

                            SEP = SEP + 1;

                            //CPMS  simple ordinal penalty
                            if (COMMOD9.ISTATIC[HPERM[I], JOPT, 0] >
                            COMMOD9.ISTATIC[HPERM[K], JOPT, 0])
                            {
                                PENJ = PENJ + 1;
                                COMMOD9.COLPEN[JOPT] = COMMOD9.COLPEN[JOPT] + 1;
                            }
                        }

                        if (SEP > 0) HPEN = HPEN + (PENJ / (double)SEP);
                    }
                }

                goto Label144;

            Label143:
                //c   Rascal penalty with RASC() 
                for (I = 0; I < COMMOD9.NEVNT - 1; I++)
                {
                    for (K = I + 1; K < COMMOD9.NEVNT; K++)
                    {
                        //c  cycle if no contradictions    
                        if (COMMOD9.RASC[HPERM[K], HPERM[I]] <= 0) continue;
                        //c  cycle if never observed together
                        if (COMMOD9.RASC[HPERM[I], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[I]] <= 0) continue;

                        HPEN = HPEN + (double)(COMMOD9.RASC[HPERM[K], HPERM[I]]) /
                            (double)(COMMOD9.RASC[HPERM[I], HPERM[K]] +
                            COMMOD9.RASC[HPERM[K], HPERM[I]]);
                    }
                }

            Label144:
                ;
            }
            else if (COMMOD9.PENF == 5)
            {
                Helper.ROYAL(HPERM, ref HPEN);
            }
            else if (COMMOD9.PENF == 6)
            {
                //c  this case should already have forced Fb4L=1 or 2
                //c  check anyway!	 
                if (COMMOD9.FB4LF != 0) Helper.SEQUEL(HPERM, ref HPEN);
            }

            Helper.JSPAN(HPERM);

            if (COMMOD9.JSPANF == 1) HPEN = HPEN + COMMOD9.SPANPEN;

            Helper.TWOPEN(HPERM, ref HPEN);

        }

        //C**********************************************************************
        //CPMS     A SUBROUTINE TO WRITE SELECTED OUTPUT TO TEXT and DATA FILES
        //CPMS     DURING AND AFTER CORRELATION OF A STRATIGRAPHIC DATA SET
        //CPMS     USING CONOP.FOR
        //C             
        //C         PROGRAMMERS:  PETE SADLER  
        //C         LAST UPDATE:  April 3rd 1999
        //C**********************************************************************
        public static void UNBASE()
        {
            int I, J;
            double fctr;

            COMMOD COMMOD9 = COMMOD.Singleton();

            //CPMS  Loop through all sections and return base levels to input value
            for (J = 0; J < COMMOD9.NSCT; J++)
            {

                //CPMS     Check correction factor
                fctr = COMMOD9.VALEVEL[COMMOD9.MXLVL + 2, J];

                if (fctr != 0.00)
                {
                    //Correct all levels
                    for (I = 0; I <= COMMOD9.HLEVEL[J]; I++)//<=HXD
                    {
                        COMMOD9.VALEVEL[I, J] = COMMOD9.VALEVEL[I, J] - fctr;
                    }
                }
            }

        }
               

        #endregion
    }
}