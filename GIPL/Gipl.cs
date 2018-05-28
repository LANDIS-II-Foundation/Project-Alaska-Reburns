using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Remoting.Messaging;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace GIPL
{
    public static partial class Gipl
    {
        private static BcModule _bcModule;
        private static NumericsModule _numericsModule;
        private static PropertiesModule _propertiesModule;

        private static Regex _whiteSpaceRegex = new Regex(@"\s+");

        private static void Abort()
        {
            Console.WriteLine();
            Console.WriteLine(" ===> Simulation aborted; Press Enter to end");
            Console.ReadLine();
        }

        public static void Main(string[] args)
        {
            var wd = args[0];

            Program(wd);
        }

        private static void Program(string wd)
        {
            int lx, sx;
            IndexedArray<double> t, w;
            double wcrit;
            double dummy1, dummy2;

            List<string> lines;
            List<string> line;
            int rc;  // row counter

            // **
            // read the 'initial.txt' file

            if (!ReadTextFile(Path.Combine(wd, "initial.txt"), out lines))
                return;

            rc = 0;

            line = ParseLine(lines[rc++]);
            lx = int.Parse(line[0]);
            sx = int.Parse(line[1]);

            t = new IndexedArray<double>(sx, lx + 1);
            w = new IndexedArray<double>(sx, lx + 1);

            _numericsModule = new NumericsModule(sx, lx);

            for (var i = sx; i <= 0; ++i)
            {
                line = ParseLine(lines[rc++]);
                _numericsModule.LayerShg[i] = int.Parse(line[1]);
                _numericsModule.Xref[i] = double.Parse(line[2]);
                t[i] = double.Parse(line[3]);
            }

            for (var i = 1; i <= lx; ++i)
            {
                line = ParseLine(lines[rc++]);
                _numericsModule.LayerShg[i + 1] = int.Parse(line[1]);
                _numericsModule.Xref[i] = double.Parse(line[2]);
                t[i + 1] = double.Parse(line[3]);
            }

            // **
            // read the 'properties.txt' file

            if (!ReadTextFile(Path.Combine(wd, "properties.txt"), out lines))
                return;

            int nLayers;
            if (!int.TryParse(lines[0], out nLayers) || nLayers != _numericsModule.LayerShg.Max)
            {
                Console.WriteLine("The number of layers is inconsistent.");
                return;
            }

            _propertiesModule = new PropertiesModule(nLayers);

            rc = 2;  // skip nLayers row and headers

            for (var i = 1; i <= nLayers; ++i)
            {
                line = ParseLine(lines[rc++]);
                _propertiesModule.SoilPorosity[i] = double.Parse(line[0]);
                _propertiesModule.SoilLm[i] = double.Parse(line[1]);
                _propertiesModule.SoilCm[i] = double.Parse(line[2]);

                // 4th column refers to another file
                List<string> flines;
                if (!ReadTextFile(Path.Combine(wd, line[3]), out flines))
                    return;

                var frc = 0;   // file row counter
                for (var j = 1; j <= PropertiesModule.UnfrNdata; ++j)
                {
                    var fline = _whiteSpaceRegex.Split(flines[frc++]);
                    _propertiesModule.UnfrXdata[i][j] = double.Parse(fline[0]);
                    _propertiesModule.UnfrFdata[i][j] = double.Parse(fline[1]);
                    _propertiesModule.UnfrDfdata[i][j] = double.Parse(fline[2]);
                }

                // create Hermite polynomial
                SplineHermite.SplineHermiteSet(PropertiesModule.UnfrNdata, _propertiesModule.UnfrXdata[i], _propertiesModule.UnfrFdata[i], _propertiesModule.UnfrDfdata[i], _propertiesModule.UnfrC[i]);
            }

            ++rc;   // Geothermal heat flux header
            var geothermalHeatFlux = double.Parse(lines[rc++]);
            ++rc;   // W crit header
            wcrit = double.Parse(lines[rc++]);
            ++rc;   // Snow thermal conductivity header
            _propertiesModule.SnowThcnd = double.Parse(lines[rc++]);
            ++rc;   // Snow volumetric heat capacity header
            _propertiesModule.SnowHeatc = double.Parse(lines[rc++]);

            // **
            // read the 'bc.txt' file

            if (!ReadTextFile(Path.Combine(wd, "bc.txt"), out lines))
                return;

            rc = 1;  // skip headers

            line = ParseLine(lines[rc++]);
            var npoints = int.Parse(line[0]);
            _numericsModule.Tscale = 1.0e6 / double.Parse(line[1]);

            _bcModule = new BcModule(npoints, geothermalHeatFlux);

            for (var i = 1; i <= _bcModule.Npoints; ++i)
            {
                line = ParseLine(lines[rc++]);
                _bcModule.Timepoints[i][1] = double.Parse(line[0]);
                _bcModule.Timepoints[i][2] = double.Parse(line[1]);
                _bcModule.Timepoints[i][3] = double.Parse(line[2]);
            }


            var snowHeight = _bcModule.Timepoints[1][3];
            //var time = _bcModule.Timepoints[1][1];          // automatically update time
            //var dtime = NumericsModule.MaxDt / 2.0;         // automatically updated timestep
            double time;         // automatically update time
            double dtime;         // automatically updated timestep
            int gc;                                     // automatically updated counter of converged iterations

            Console.WriteLine("Initializing");
            InitializeSoil(out dtime, out gc, out time);
            UpdateProperties(snowHeight, time);

            for (var i = 2; i <= _numericsModule.Lx + 1; ++i)
            {
                double y;
                Soilsat(t[i], out y, out dummy1, out dummy2, _numericsModule.LayerShg[i]);
                w[i] = y;
            }

            for (var i = _numericsModule.Sn; i <= 0; ++i)
            {
                w[i] = 0.0;
            }

            var file = "ModeledGroundTemperatureCs.txt";
            var writer = new StreamWriter(Path.Combine(wd, file));

            //writer.Write($"{time,12:F5}");
            writer.Write($"{time,15:F8}");
            for (var j = 1; j <= _numericsModule.Lx; ++j)
                //writer.Write($"{_numericsModule.X[j],12:F5}");
                writer.Write($"{_numericsModule.X[j],15:F8}");
            writer.WriteLine();

            for (var i = 2; i <= _bcModule.Npoints; ++i)
            {
                var timeEnd = _bcModule.Timepoints[i][1];
                snowHeight = _bcModule.Timepoints[i][3];

                var surfaceTemp = SurfaceTemperature(timeEnd);
                Console.WriteLine($"t={timeEnd,6:F1}; AirT={surfaceTemp,5:F1}; SnowHGT={snowHeight,5:F1}; Ground Temp={t[71],6:F2} @ Z={_numericsModule.X[71],4:F2}");

                UpdateProperties(snowHeight, time);

                Stemperature(t, w, ref time, timeEnd, ref dtime, ref gc);

                //writer.Write($"{time,12:F5}");
                writer.Write($"{time,15:F8}");
                for (var j = 2; j <= _numericsModule.Lx + 1; ++j)
                    //writer.Write($"{t[j],12:F5}");
                    writer.Write($"{t[j],15:F8}");
                writer.WriteLine();
            }

            writer.Close();
        }

        private static double SurfaceTemperature(double time)
        {
            double surface_temperature;

            var dtimepoints = _bcModule.Timepoints[2][1] - _bcModule.Timepoints[1][1];
            var i = (int)(time / dtimepoints) + 1;   // FORTRAN code uses dint() which truncates
            var di = time - (i - 1) * dtimepoints;

            if (i < _bcModule.Npoints)
            {
                surface_temperature = _bcModule.Timepoints[i][2] + di * (_bcModule.Timepoints[i + 1][2] - _bcModule.Timepoints[i][2]) / dtimepoints;
            }
            else
            {
                surface_temperature = _bcModule.Timepoints[_bcModule.Npoints][2];
            }

            return surface_temperature;
        }

        private static bool ReadTextFile(string filePath, out List<string> lines)
        {
            lines = null;
            if (!File.Exists(filePath))
            {
                Console.WriteLine($"Cannot find file: '{filePath}'.");
                return false;
            }

            lines = File.ReadAllLines(filePath).ToList();

            if (!lines.Any())
            {
                Console.WriteLine($"File: '{filePath}' is empty.");
                return false;
            }

            return true;
        }

        private static List<string> ParseLine(string s)
        {
            return _whiteSpaceRegex.Split(s.Trim()).ToList();
        }
    }
}
