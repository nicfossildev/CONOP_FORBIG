using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace CONOP
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
#if GUIMODE
        [STAThread]
#endif
        static void Main()
        {
#if GUIMODE
            //TODO : to run GUI mode
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new frmCONOP());
#else
            //TODO : to run CONSOLE mode
            ConsoleCONOP consoleProxy = new ConsoleCONOP();
            consoleProxy.MsgLoop();
#endif
        }
    }
}
