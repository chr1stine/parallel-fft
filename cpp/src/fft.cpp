#include <complex>
#include <valarray>

#include <vector>
#include <thread>

#include "./include/utils.h"

const double PI = 3.141592653589793238460;

using namespace std;

CArray fft_iter(CArray &x)
{
    int N = x.size();

    CArray y = CArray(N);

    // complex computations
    Complex wN = polar(1., -1 * double(2) * PI / double(N));
    Complex wN2 = polar(1., -1 * double(2) * PI / double(N / 2));

    for (int k = 0; k < N / 2; k++) // цикл выполняется в одном потоке
    {
        Complex wkN = pow(wN, k);
        Complex even_sum = 0;
        Complex odd_sum = 0;
        for (int n = 0; n < N / 2; n++) // цикл подлежит распараллеливанию на 4(?) потока
        {
            Complex wkN2 = pow(wN2, k * n);
            even_sum += (x[2 * n]) * wkN2;
            odd_sum += (x[2 * n + 1]) * wkN2;
        }
        y[k] = even_sum + (odd_sum * wkN);
        y[k + N / 2] = even_sum - (odd_sum * wkN);
    }
    return y;
}

void task(CArray x, Complex &even_sum, Complex &odd_sum, int k, int N, int th_index, int th_count, Complex wkN, Complex wN2)
{
    for (int n = th_index * (N / 2) / th_count;
         n < (th_index + 1) * (N / 2) / th_count;
         n++) // цикл подлежит распараллеливанию на 4(?) потока
    {
        Complex wkN2 = pow(wN2, k * n);
        even_sum += (x[2 * n]) * wkN2;
        odd_sum += (x[2 * n + 1]) * wkN2;
    }
}

CArray fft_iter_parallel(CArray &x, int p)
{
    int N = x.size();

    CArray y = CArray(N);

    // complex computations
    Complex wN = polar(1., -1 * double(2) * PI / double(N));
    Complex wN2 = polar(1., -1 * double(2) * PI / double(N / 2));

    for (int k = 0; k < N / 2; k++) // цикл выполняется в одном потоке
    {
        Complex wkN = pow(wN, k);
        // поровну разделить N/2 итераций между p потоками
        // запустить их
        // join в конце тела цикла
        vector<thread> threads;

        Complex even_sum = 0;
        Complex odd_sum = 0;

        for (int i = 0; i < p; ++i)
        {
            threads.push_back(std::thread(
                &task, // function to execute
                x,     // input array
                ref(even_sum),
                ref(odd_sum),
                k,   // output array element index
                N,   // size of output and input arrays
                i,   // thread index
                p,   // total processor count
                wkN, // wkN
                wN2  // wN2
                ));
        }

        for (int i = 0; i < p; ++i)
        {
            threads[i].join();
        }

        y[k] = even_sum + (odd_sum * wkN);
        y[k + N / 2] = even_sum - (odd_sum * wkN);
    }

    return y;
}