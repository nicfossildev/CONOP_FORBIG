using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CONOP.NET
{
    public class DORASC : IRunIt
    {

        #region Singleton

        private static DORASC g_singleton = null;
        private DORASC() { }
        public static DORASC Singleton()
        {
            if (g_singleton == null)
                g_singleton = new DORASC();

            return g_singleton;
        }

        #endregion

        #region IRunIt 成员

        public void RunIt()
        {
            int IJ, MJ;
            int I, J, M;            

            COMMOD COMMOD9 = COMMOD.Singleton();

            //ZEROIZE THE RASC MATRIX
            Helper.SetVal(COMMOD9.RASC, 0);

            if (COMMOD9.NEVNT == 0) goto Label9999;

            //cpms  loop through all events but one (pairwise tests!)
            for (I = 0; I < COMMOD9.NEVNT - 1; I++)
            {
                if (Helper.ALLRZERO(I)) continue;

                // loop through all other events
                for (M = I + 1; M < COMMOD9.NEVNT; M++)
                {
                    //if all zeroed out, try again
                    if (Helper.ALLRZERO(M)) continue;

                    //loop through all sections
                    for (J = 0; J < COMMOD9.NSCT; J++)
                    {
                        IJ = 0;

                        //make sure I is observed in section J 
                        if (!(Helper.RZERO(I, J))) IJ = COMMOD9.ISTATIC[I, J, 0];

                        if (IJ == 0) continue;

                        MJ = 0;

                        //make sure M is observed in section J
                        if (!(Helper.RZERO(M, J))) MJ = COMMOD9.ISTATIC[M, J, 0];

                        if (MJ == 0) continue;

                        //RECORD ORDER
                        if (IJ < MJ)
                        {
                            COMMOD9.RASC[I, M] = COMMOD9.RASC[I, M] + 1;
                        }
                        else if (MJ < IJ)
                        {
                            COMMOD9.RASC[M, I] = COMMOD9.RASC[M, I] + 1;
                        }
                    }
                }
            }


            if (COMMOD9.CDF != 1)
            {
                //TODO:DORASC.CS
                //   OPEN(11, FILE=RUNLOG, STATUS='OLD', POSITION='APPEND')
                //    WRITE(11,*) 'Contents of RASC() array'
                //  WRITE(11,*) '-------------------------'
                //  DO I=1, NEVNT
                //    WRITE(11,*) (RASC(I,J),J=1,NEVNT)
                //  ENDDO
                //CLOSE(11, STATUS='KEEP')
            }



        Label9999:
            return;


        }

        #endregion
    }
}
