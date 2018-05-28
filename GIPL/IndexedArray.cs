using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GIPL
{
    public static partial class Gipl
    {
        private class IndexedArray<T>
        {
            private readonly T[] _vals;

            public IndexedArray(int minIndex, int maxIndex)
            {
                MinIndex = minIndex;
                MaxIndex = maxIndex;
                _vals = new T[maxIndex - minIndex + 1];
            }

            public T this[int index] { get { return _vals[index - MinIndex]; } set { _vals[index - MinIndex] = value; } }

            public int MinIndex { get; }
            public int MaxIndex { get; }

            public int Count => MaxIndex - MinIndex + 1;
            public T Max => _vals.Max();
            public T Min => _vals.Min();

            public IndexedArray<T> Copy()
            {
                var g = new IndexedArray<T>(MinIndex, MaxIndex);
                for (var i = MinIndex; i <= MaxIndex; ++i)
                    g[i] = this[i];

                return g;
            }
        }

        //private class IndexedDoubleArray
        //{
        //    private readonly double[] _vals;

        //    public IndexedDoubleArray(int minIndex, int maxIndex)
        //    {
        //        MinIndex = minIndex;
        //        MaxIndex = maxIndex;
        //        _vals = new double[maxIndex - minIndex + 1];
        //    }

        //    public double this[int index] { get { return _vals[index - MinIndex]; } set { _vals[index - MinIndex] = value; } }

        //    public int MinIndex { get; }
        //    public int MaxIndex { get; }

        //    public int Count => MaxIndex - MinIndex + 1;
        //    public double Max => _vals.Max();
        //    public double Min => _vals.Min();

        //    public IndexedDoubleArray Copy()
        //    {
        //        var g = new IndexedDoubleArray(MinIndex, MaxIndex);
        //        for (var i = MinIndex; i <= MaxIndex; ++i)
        //            g[i] = this[i];

        //        return g;
        //    }
        //}

        //private class IndexedIntArray
        //{
        //    private readonly int[] _vals;

        //    public IndexedIntArray(int minIndex, int maxIndex)
        //    {
        //        MinIndex = minIndex;
        //        MaxIndex = maxIndex;
        //        _vals = new int[maxIndex - minIndex + 1];
        //    }

        //    public int this[int index] { get { return _vals[index - MinIndex]; } set { _vals[index - MinIndex] = value; } }

        //    public int MinIndex { get; }
        //    public int MaxIndex { get; }

        //    public int Count => MaxIndex - MinIndex + 1;
        //    public int Max => _vals.Max();
        //    public int Min => _vals.Min();

        //    public IndexedIntArray Copy()
        //    {
        //        var g = new IndexedIntArray(MinIndex, MaxIndex);
        //        for (var i = MinIndex; i <= MaxIndex; ++i)
        //            g[i] = this[i];

        //        return g;
        //    }
        //}
    }
}
