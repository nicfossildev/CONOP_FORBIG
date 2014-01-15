using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CONOP.NET
{
    public class DOFB4L : IRunIt
    {

        #region Singleton

        private static DOFB4L g_singleton = null;
        private DOFB4L() { }
        public static DOFB4L Singleton()
        {
            if (g_singleton == null)
                g_singleton = new DOFB4L();

            return g_singleton;
        }

        #endregion



        #region IRunIt

        public void RunIt()
        {
            int ISP, MSP;
            int IAROW, MBROW;
            int AIJ, BMJ;
            int I, J, M;

            //C------------------------------------------
            IAROW = 0;
            MBROW = 0;
            ISP = 0;
            AIJ = 0;
            BMJ = 0;
            I = 0;
            J = 0;
            M = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();
            //CPMS    ZEROIZE THE FADLAD MATRIX
            Helper.SetVal(COMMOD9.FADLAD, 0);

            COMMOD9.NFALA = 0;

            if (COMMOD9.NSPC == 0) goto Label9999;

            //CPMS  set the row=col diagonal to one
            //CPMS  i.e. every taxon FAD is before its own LAD
            for (I = 0; I < COMMOD9.NSPC; I++)
            {
                COMMOD9.FADLAD[I, I] = 1;
            }

            //cpms  loop through all events looking for FADs
            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                //CPMS    if not a FAD, try again 

                if (COMMOD9.IROWS[I, 1] != 1) continue;
                //cpms    if all zeroed out, try again
                if (Helper.ALLRZERO(I)) continue;

                IAROW = I;
                ISP = COMMOD9.IROWS[IAROW, 2];

                //cpms    loop through all events again looking for LADs

                for (M = 0; M < COMMOD9.NEVNT; M++)
                {

                    //CPMS      if not a LAD, try again 
                    if (COMMOD9.IROWS[M, 1] != 2) continue;

                    //cpms      if all zeroed out, try again
                    if (Helper.ALLRZERO(M)) continue;

                    MBROW = M;
                    MSP = COMMOD9.IROWS[MBROW, 2];

                    //cpms      quit if FAD\LAD already established
                    if (COMMOD9.FADLAD[ISP, MSP] == 1) continue;

                    //cpms      loop through all sections
                    for (J = 0; J < COMMOD9.NSCT; J++)
                    {
                        AIJ = -1;//<=HXD

                        //cpms        make sure that FADi moves down only
                        if (COMMOD9.ISTATIC[IAROW, J, 1] != 1) continue;

                        //cmps        make sure FADi is observed in section J   
                        if (!(Helper.RZERO(IAROW, J))) AIJ = COMMOD9.ISTATIC[IAROW, J, 0];

                        if (AIJ == -1) continue;//<=HXD

                        BMJ = -1;//<=HXD

                        //cpms        make sure that LADm moves up only
                        if (COMMOD9.ISTATIC[MBROW, J, 1] != 2) continue;

                        //cmps        make sure LADm is observed in section J    
                        if (!(Helper.RZERO(MBROW, J))) BMJ = COMMOD9.ISTATIC[MBROW, J, 0];

                        if (BMJ == -1) continue;//<=HXD

                        //cpms        check for FAD below LAD
                        if (AIJ < BMJ)
                        {
                            COMMOD9.FADLAD[ISP, MSP] = 1;
                        }
                        else if ((AIJ == BMJ) && (COMMOD9.COXSTF == 2))
                        {
                            COMMOD9.FADLAD[ISP, MSP] = 1;
                            COMMOD9.FADLAD[MSP, ISP] = 1;
                        }
                    }
                }
            }


        Label9999:

            //TextWriter writer = new StreamWriter(File.OpenWrite("test.txt"));
            //for (int xx = 0; xx < COMMOD9.NSPC; xx++)
            //{
            //    for (int yy = 0; yy < COMMOD9.NSPC; yy++)
            //    {
            //        writer.Write(string.Format("[{0},{1}]={2}\n", yy + 1, xx + 1, COMMOD9.FADLAD[yy, xx]));
            //    }
                
            //}
            //writer.Close();


            return;
        }

        #endregion
    }
}
