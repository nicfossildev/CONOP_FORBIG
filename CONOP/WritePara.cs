using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CONOP.NET;
using System.IO;
using System.Diagnostics;

namespace CONOP
{
    class WritePara
    {

        public static StreamWriter sw_static= null;
        public static int[] FAKE_IRANKs;
        public static int[] FAKE_JRANKs;
        public static double[] FAKE_DOUBLEs;
        public static int index = 0;
        /// <summary>
        /// replaceInitPerm for preparation of new csv input case for HANA CONOP
        /// </summary>
        public static void replaceInitPerm()
        {
            //Second Project
            COMMOD COMMOD9 = COMMOD.Singleton();


//            //INITPERM
//            long _index = 0;
//#if SEC7
//            string inputFile = "inData/SEC7_INIT_PERM.csv";
//#elif SEC19
//            string inputFile = "inData/SEC19_INIT_PERM.csv";
//#elif SEC50
//            string inputFile = "inData/SEC50_INIT_PERM.csv";
//#elif SEC195
//            string inputFile = "inData/SEC195_INIT_PERM.csv";
//#elif SEC286
//            string inputFile = "inData/SEC286_INIT_PERM.csv";
//#elif SEC287
//            string inputFile = "inData/SEC287_INIT_PERM.csv";
//#endif

//#if  REPACEPERM 
 
//            COMMOD9.INIPERM = new int[COMMOD9.NEVNT];
//            Trace.Assert(File.Exists(inputFile));
//            using (StreamReader reader = new StreamReader(inputFile, Encoding.Default, true))
//            {
//                while (!reader.EndOfStream)
//                {
//                    string line = reader.ReadLine();
//                    int a = int.Parse(line.Split(',')[1]);
//                    COMMOD9.INIPERM[_index++] = int.Parse(line.Split(',')[1]);
//                }
//                Trace.Assert(_index == COMMOD9.INIPERM.LongLength);
//            }
//#endif
        }

        /// <summary>
        /// write out all of context in SimulatedAnnealling algorithm into csv for HANA CONOPs
        /// </summary>

        public static void writeThem()
        {
            //Second Project
            COMMOD COMMOD9 = COMMOD.Singleton();
            FileStream fs;
            StreamWriter sw;
            String inputFile;

            //delete all the data files
            if (Directory.Exists("outData"))
            {
                foreach (string file in Directory.EnumerateFiles("outData"))
                {
                    File.Delete(file);
                }
            }
            else
            {
                Directory.CreateDirectory("outData");
            }

            //irows
            inputFile = "outData/IROWS.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            int index = 0;
            // add row index
            int _index = 0;
            for (int i = 0; i < COMMOD9.IROWS.GetLength(0); i++, _index++)
            {

                sw.WriteLine(_index.ToString() + ","  +COMMOD9.IROWS[i, 0].ToString() + "," + COMMOD9.IROWS[i, 1].ToString() + "," + COMMOD9.IROWS[i, 2].ToString() + "," + COMMOD9.IROWS[i, 3].ToString());
            }
            sw.Close();
            Helper.Write("finish writing the IROWS");

            //COEXST
            _index = 0;
            inputFile = "outData/COEXST.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            foreach (int i in COMMOD9.COEXST)
            {
                sw.WriteLine(_index.ToString() + "," + i);
                _index++;
            }
            sw.Close();
            Helper.Write("finish writing the COEXST");

            //ELEVEL
            _index = 0;
            inputFile = "outData/ELEVEL.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            foreach (int i in COMMOD9.ELEVEL)
            {
                sw.WriteLine(_index.ToString() + "," + i);
                _index++;
            }
            sw.Close();
            Helper.Write("finish writing the ELEVEL");

            //FADLAD
            _index = 0;
            inputFile = "outData/FADLAD.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            foreach (int i in COMMOD9.FADLAD)
            {
                sw.WriteLine(_index.ToString() + "," + i);
                _index++;
            }
            sw.Close();
            Helper.Write("finish writing the FADLAD");

            //HLEVEL
            _index = 0;
            inputFile = "outData/HLEVEL.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            foreach (int i in COMMOD9.HLEVEL)
            {
                sw.WriteLine(_index.ToString() + "," + i);
                _index++;
            }
            sw.Close();
            Helper.Write("finish writing the HLEVEL");

            //ISTATIC
            _index = 0;
            inputFile = "outData/ISTATIC.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            index = 0;
            int iFirst = COMMOD9.ISTATIC.GetLength(0), iSecond = COMMOD9.ISTATIC.GetLength(1);
            for (int i = 0; i < iFirst; i++){
                for (int j = 0; j < iSecond - 10; j++){
                    sw.WriteLine(_index.ToString() + "," + COMMOD9.ISTATIC[i, j, 0].ToString() + "," + COMMOD9.ISTATIC[i, j, 1].ToString());
                    _index++;
                }
            }
            sw.Close();
            Helper.Write("finish writing the ISTATIC");

            //RSTATIC
            index = 0;
            _index = 0;
            inputFile = "outData/RSTATIC.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            for (int i = 0; i < iFirst; i++)
            {
                for (int j = 0; j < iSecond - 10; j++)
                {
                    sw.WriteLine(_index.ToString() + "," + COMMOD9.RSTATIC[i, j, 0].ToString() + "," + COMMOD9.RSTATIC[i, j, 1].ToString());
                    _index++;
                }
            }
            sw.Close();
            Helper.Write("finish writing the RSTATIC");

            //VALEVEL
            _index = 0;
            inputFile = "outData/VALEVEL.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            foreach (double di in COMMOD9.VALEVEL)
            {
                sw.WriteLine(_index.ToString() + "," + di.ToString());
                _index++;
            }
            sw.Close();
            Helper.Write("finish writing the VALEVEL");

            //EXCLSECT
            _index = 0;
            inputFile = "outData/EXCLSECT.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            foreach (int di in COMMOD9.EXCLSECT)
            {
                sw.WriteLine(_index.ToString() + "," + di.ToString());
                _index++;
            }
            sw.Close();
            Helper.Write("finish writing the VALEVEL");

            //INITPERM
            _index = 0;
            inputFile = "outData/INIT_PERM.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            foreach (int di in COMMOD9.INIPERM)
            {
                sw.WriteLine(_index.ToString() + "," + di.ToString());
                _index++;
            }
            sw.Close();
            Helper.Write("finish writing the INIT_PERM");

            //CFG
            inputFile = "outData/ORG_CFG.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            sw.WriteLine(COMMOD9.NSCT.ToString() + "," + COMMOD9.NSPC.ToString() + "," + (COMMOD9.NEVNT - COMMOD9.NEVNT).ToString() + ","
                + COMMOD9.MXLVL.ToString() + "," + COMMOD9.STARTT.ToString() + "," + COMMOD9.COXSTF.ToString() + ","
                + COMMOD9.NINNER.ToString() + "," + COMMOD9.R.ToString());
            sw.Close();
            Helper.Write("finish writing the ORG_CFG");

            //GLOBAL_PARA
            inputFile = "outData/GLOBAL_PARA.csv";
            fs = new FileStream(inputFile, FileMode.CreateNew);
            fs.Close();
            sw = new StreamWriter(inputFile, true, Encoding.Default);
            int NUDGER = 0;
            if(COMMOD9.NUDGER)NUDGER = 1;
            sw.WriteLine(COMMOD9.NEXCL.ToString() + "," + COMMOD9.NROWS.ToString() + "," + COMMOD9.NEVNT.ToString() + ","
                + COMMOD9.NSPC.ToString() + "," + COMMOD9.NMIDS.ToString() + "," + COMMOD9.INIGEN.ToString() + ","
                + COMMOD9.JSTART.ToString() + "," + COMMOD9.AUTF.ToString() + "," + COMMOD9.CONTF.ToString() + ","
                + COMMOD9.SOLVEF.ToString() + "," + COMMOD9.FIXF.ToString() + "," + COMMOD9.RUNGRF.ToString() + ","
                + COMMOD9.TRJGRF.ToString() + "," + COMMOD9.GRIDF.ToString() + "," + COMMOD9.NOUTER.ToString() + ","
                + COMMOD9.PENF.ToString() + "," + COMMOD9.MXLVL.ToString() + "," + COMMOD9.JEXCL.ToString() + ","
                + COMMOD9.JSPANF.ToString() + "," + COMMOD9.NABRGEN.ToString() + "," + COMMOD9.NEGATF.ToString() + ","
                + COMMOD9.XEVNT.ToString() + "," + COMMOD9.FB4LF.ToString() + "," + COMMOD9.COXSTF.ToString() + ","
                + NUDGER.ToString());
            sw.Close();
            Helper.Write("finish writing the GLOBAL_PARA");

        }

        /// <summary>
        /// write IRank JRank from csv
        /// </summary>
        public static void writeIJRnk(int IRnk, int JRnk, double double_num)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
#if SEC7
            string inputFile = "inData/SEC7_IJRNK_DOUBLE.csv";
#elif SEC19
            string inputFile = "inData/SEC19_IJRNK_DOUBLE.csv";
#elif SEC50
            string inputFile = "inData/SEC50_IJRNK_DOUBLE.csv";
#elif SEC195
            string inputFile = "inData/SEC195_IJRNK_DOUBLE.csv";
#elif SEC286
            string inputFile = "inData/SEC286_IJRNK_DOUBLE.csv";
#elif SEC287
            string inputFile = "inData/SEC287_IJRNK_DOUBLE.csv";
#endif
#if  OUTDATA_IJRANK
            if (sw_static == null)
            {
                FileStream fs = new FileStream(inputFile, FileMode.CreateNew);
                fs.Close();
                sw_static = new StreamWriter(inputFile, true, Encoding.Default);
            }
            sw_static.WriteLine(IRnk.ToString() + "," + JRnk.ToString() + "," + double_num.ToString());
#endif
        }


        /// <summary>
        /// close the sw_static stream
        /// </summary>
        public static void closeSW_STATIC()
        {
            sw_static.Close();
            Helper.Write("finish writing the IJRnk_DOUBLE");
        }

        /// <summary>
        /// read IRank JRank from csv
        /// </summary>
        public static void readIJRnk()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            FAKE_IRANKs = new int[(int)COMMOD9.NOUTER * COMMOD9.NINNER];
            FAKE_JRANKs = new int[(int)COMMOD9.NOUTER * COMMOD9.NINNER];
            FAKE_DOUBLEs = new double[(int)COMMOD9.NOUTER * COMMOD9.NINNER];
#if SEC7
            string inputFile = "inData/SEC7_IJRNK_DOUBLE.csv";
#elif SEC19
            string inputFile = "inData/SEC19_IJRNK_DOUBLE.csv";
#elif SEC20
            string inputFile = "inData/SEC20_IJRNK_DOUBLE.csv";
#elif SEC50
            string inputFile = "inData/SEC50_IJRNK_DOUBLE.csv";
#elif SEC195
            string inputFile = "inData/SEC195_IJRNK_DOUBLE.csv";
#elif SEC286
            string inputFile = "inData/SEC286_IJRNK_DOUBLE.csv";
#elif SEC287
            string inputFile = "inData/SEC287_IJRNK_DOUBLE.csv";
#endif
#if  FAKE_IJRANKE_DOUBLE
            Trace.Assert(File.Exists(inputFile));
            using (StreamReader reader = new StreamReader(inputFile, Encoding.Default, true))
            {
                int i = 0;
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    string[] split_result = line.Split(',');
                    if (split_result.Length < 3)
                        continue;
                    FAKE_IRANKs[i] = int.Parse(split_result[0]);
                    FAKE_JRANKs[i] = int.Parse(split_result[1]);
                    FAKE_DOUBLEs[i] = double.Parse(split_result[2]);
                    i++;
                }
                index = 0;
            }
#endif

        }

        /// <summary>
        /// get IRank JRank double from the array
        /// </summary>
        public static void getIJRand(ref int IRank, ref int JRank)
        {
            IRank = FAKE_IRANKs[index];
            JRank = FAKE_JRANKs[index];
            index++;
        }

        /// <summary>
        /// write out BstPerm in soln.dat format
        /// </summary>
        public static void writeBstPerm()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            //TODO : output BestPerm.dat
            int i = 0;
            if (!File.Exists("BestPerm.dat"))
            {
                File.Create("BestPerm.dat").Close();
            }
            using (StreamWriter fs = new StreamWriter(new FileStream("BestPerm.dat", FileMode.Truncate)))
            {
                for (; i < COMMOD9.BSTPERM.Length; i++)
                {
                    fs.WriteLine(string.Format("      {0}      {1}     {2}", COMMOD9.IROWS[COMMOD9.BSTPERM[i], 0] + 1 /*evt_number*/, COMMOD9.IROWS[COMMOD9.BSTPERM[i], 1] /*evt_type*/, i+1));
                }
            }
            //TODO : output Penalty.dat
            if (!File.Exists("Penalty.dat"))
            {
                File.Create("Penalty.dat").Close();
            }
            using (StreamWriter fs = new StreamWriter(new FileStream("Penalty.dat", FileMode.Truncate)))
            {
                fs.WriteLine(COMMOD9.BSTPEN);
            }
        }

        /// <summary>
        /// 1, reset context of control parameter
        /// 2, getPen for current bestPerm
        /// </summary>
        /// <returns></returns>
        public static bool validationBestPerm()
        {
            throw new NotImplementedException();
        }

    }
}
