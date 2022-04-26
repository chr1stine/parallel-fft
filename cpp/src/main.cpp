#include <iostream>
#include <string>
#include "./include/utils.h"
#include "./include/fft.h"

const double PI = 3.141592653589793238460;

typedef std::complex<double> Complex;
typedef std::valarray<Complex> CArray;

using namespace std;

int threads_count = 4; // default if not overwritten by cli arg

int main(int argc, char **argv)
{
    CArray input;
    if (argc >= 2)
    {
        try
        {
            input = read_array(argv[1]);
            cout << "Successfully parsed input" << endl;

            if (argc > 2)
            {
                threads_count = stoi(argv[2]);
            }
        }
        catch (...)
        {
            cout << "Error parsing input" << endl;
            return 1;
        }
    }
    else
    {
        cout << "No file with input specified" << endl;
        return 1;
    }

    CArray result = fft_iter_parallel(input, threads_count);

    cout << endl
         << "fft in parallel" << endl;
    for (int i = 0; i < result.size(); ++i)
    {
        cout << result[i] << endl;
    }

    cout << endl;

    result = fft_iter(input);
    cout << "fft in sequential" << endl;
    for (int i = 0; i < result.size(); ++i)
    {
        cout << result[i] << endl;
    }

    return 0;
}