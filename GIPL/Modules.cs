using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GIPL
{
    public static partial class Gipl
    {
        private static T[][] RecJagged<T>(int rowCount, int colCount)
        {
            var a = new T[rowCount][];
            for (var i = 0; i < rowCount; ++i)
                a[i] = new T[colCount];

            return a;
        }

        private class BcModule
        {
            public BcModule(int npoints, double geothermal)
            {
                Npoints = npoints;
                Geothermal = geothermal;
                Timepoints = RecJagged<double>(npoints + 1, 4);
            }

            public int Npoints;
            public double Geothermal;
            public double[][] Timepoints;
        }

        private class NumericsModule
        {
            public NumericsModule(int sx, int lx)
            {
                Sx = sx;
                Lx = lx;
                Dx1 = new IndexedArray<double>(sx, lx);
                Diag = new IndexedArray<double>(sx, lx);
                X = new IndexedArray<double>(sx, lx);
                Dxref = new IndexedArray<double>(sx, lx);
                Xref = new IndexedArray<double>(sx, lx);
                LayerW = new IndexedArray<double>(sx, lx + 1);
                LayerLf = new IndexedArray<double>(sx, lx + 1);
                LayerCf = new IndexedArray<double>(sx, lx + 1);
                LayerShg = new IndexedArray<int>(sx, lx + 1);
            }

            public int Lx; //Number of soil layer
            public int Sx; //Maximal number of snow layer
            public int Sn; //Number of the first node in the grid
            public const double Cw = 4.181E0; //[MJ/m/C]       !Heat capacity of liqiud water
            public const double Ci = 2.114E0; //[MJ/m/C]       !Heat capacity of ice
            public const double Ca = 1.003E-3; //[MJ/m/C]       !Heat capacity of ice
            public const double Kw = 0.56E0; //[W/m/C]       !Thermal conductivity of liqiud water
            public const double Ki = 2.2E0; //[W/m/C]       !Thermal conductivity of ice
            public const double Ka = 0.025E0; //[W/m/C]       !Thermal conductivity of air
            public const double La = 333.2E0; //[MJ/?]              !Latent heat of fusion
            public double Tscale; //[sec/10^6]       !Time scale
            public const int NumMaxIter = 20; //[5-9]              !Maximal number of allowed iterations before decreasing the time step
            public const double MaxDt = 0.5E0; //[days]              !Maximal time step
            public const double MaxDelta = 1.0E-5; //[C]              !Accuracy at each time step
            public const double Dlt = 1.0E-10; //[C]              !Regularization of computing a numerical derivative of the ethalpy
            public const double Lkwki = -1.36827585561721230E0; //Natural logarithm of KW/KI
            public const double Dpi = 3.141592653589793E0;
            public IndexedArray<double> Dx1;
            public IndexedArray<double> Diag;
            public IndexedArray<double> X;
            public IndexedArray<double> Dxref;
            public IndexedArray<double> Xref;
            public IndexedArray<double> LayerW;
            public IndexedArray<double> LayerLf;
            public IndexedArray<double> LayerCf;
            public IndexedArray<int> LayerShg;
            public double InitialDepth;
        }

        private class PropertiesModule
        {
            // note: the indexing of all 2D and 3D arrays in this module have been altered from the FORTRAN code
            //  so that the layer index comes first

            public PropertiesModule(int nLayers)
            {
                UnfrXdata = RecJagged<double>(nLayers + 1, UnfrNdata + 1);

                UnfrC = new double[nLayers + 1][][];
                for (var i = 1; i <= nLayers; ++i)
                    UnfrC[i] = RecJagged<double>(5, UnfrNdata + 1);

                UnfrDfdata = RecJagged<double>(nLayers + 1, UnfrNdata + 1);
                UnfrFdata = RecJagged<double>(nLayers + 1, UnfrNdata + 1);

                SoilPorosity = new double[nLayers + 1];
                SoilLm = new double[nLayers + 1];
                SoilCm = new double[nLayers + 1];
            }

            public int Nlayers;
            public const int UnfrNdata = 630;
            public const int UnfrNintv = UnfrNdata - 1;
            public double[][] UnfrXdata;
            public double[][][] UnfrC;
            public double[][] UnfrDfdata;
            public double[][] UnfrFdata;
            public double[] SoilPorosity;
            public double[] SoilLm;
            public double[] SoilCm;
            public double SnowThcnd;
            public double SnowHeatc;
        }
    }
}
