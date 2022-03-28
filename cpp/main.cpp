#include <complex>
#include <iostream>
#include <valarray>
#include <string>
#include <fstream>

const double PI = 3.141592653589793238460;

typedef std::complex<double> Complex;
typedef std::valarray<Complex> CArray;

using namespace std;

unsigned int reverseBits(unsigned int n, int bits)
{
    int reverse = 0;
    int pos = 0;
    while (pos < bits)
    {
        reverse |= ((n & (1 << pos)) >> pos) << (bits - pos - 1);
        pos++;
    }
    return reverse;
}

CArray fft_iter(CArray &x)
{
    // rearranging original indices
    int N = x.size();
    int *new_indices = new int[N];
    for (int i = 0; i < N / 2; i++)
    {
        int new_index = reverseBits(i, log2(N));
        new_indices[i] = new_index;
        new_indices[new_index] = i;
    }

    CArray y = CArray(N);

    // complex computations
    Complex wn = Complex(double(2) * PI * 1i / double(N));

    for (int i = 0; i < N / 2; i++)
    {
        y[i] = 0;
        y[i + N / 2] = 0;
        y[i + 1] = 0;
        y[i + N / 2 + 1] = 0;
        for (int j = 0; j < N / 2; j++)
        {
            auto t1 = pow(wn, i * j);
            y[i] += (x[j] + x[j + N / 2]) * t1;
            y[i + N / 2] += (x[j] + x[j + N / 2]) * t1;
            y[i + 1] += (x[j] - x[j + N / 2]) * pow(wn, j) * t1;
        }
    }

    // CArray wns = CArray(N / 2);
    // for (int i = 0; i < N / 2; i++)
    // {
    //     wns[i] = pow(wn_const, i);
    // }
    // for (size_t k = 0; k < N / 2; ++k)
    // {
    //     Complex t = std::polar(1.0, -2 * PI * k / N) * x[k + N / 2];
    //     y[k] = x[k] + t;
    //     y[k + N / 2] = x[k] - t;
    // }

    // swapping indices
    // x = new_indices;
    return y;
}

bool parse_one_line(string line, double &number)
{
    try
    {
        number = stof(line);
        return true;
    }
    catch (const invalid_argument &e)
    {
        return false;
    }
}

bool read_input_file(char *file_name, CArray &input_array, int &n)
{
    ifstream myfile(file_name);
    string line;
    if (myfile.is_open())
    {
        getline(myfile, line, '\n');
        n = stoi(line);

        input_array = CArray(n);

        int i = 0;
        while (getline(myfile, line, '\n'))
        {
            double parsed_umber;
            if (parse_one_line(line, parsed_umber))
                input_array[i] = parsed_umber;
            else
                return false;
            i++;
        }
        myfile.close();
    }

    return true;
}

int main(int argc, char **argv)
{
    CArray input;
    int n;
    if (argc == 2)
    {
        if (read_input_file(argv[1], input, n))
        {
            std::cout << "Successfully parsed input" << std::endl;
        }
        else
        {
            std::cout << "Error parsing input" << std::endl;
            return 1;
        }
    }

    // forward fft
    CArray result = fft_iter(input);

    std::cout << "fft" << std::endl;
    for (int i = 0; i < n; ++i)
    {
        std::cout << result[i] << std::endl;
    }

    return 0;
}