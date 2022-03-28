#include <complex>
#include <iostream>
#include <valarray>
#include <string>
#include <fstream>

const double PI = 3.141592653589793238460;

typedef std::complex<double> Complex;
typedef std::valarray<Complex> CArray;

using namespace std;

unsigned int reverse_bits(unsigned int n, int bits)
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
    int *rearranged_indices = new int{N};
    for (int i = 0; i < N / 2; i++)
    {
        int rearranged_index = reverse_bits(i, log2(N));
        rearranged_indices[i] = rearranged_index;
        rearranged_indices[rearranged_index] = i;
    }

    CArray y = CArray(N);

    // complex computations
    Complex wN = polar(1., double(2) * PI / double(N));
    Complex wN2 = polar(1., double(2) * PI / double(N / 2));

    for (int k = 0; k < N; k++)
    {
        Complex wkN = pow(wN, k);
        Complex even_sum = 0;
        Complex odd_sum = 0;
        for (int n = 0; n < N / 2; n++)
        {
            Complex t1 = pow(wN2, -k * n);
            even_sum += (x[2 * n]) * t1;
            odd_sum += (x[2 * n + 1]) * t1;
        }
        y[k] = even_sum + odd_sum * wkN;
    }
    delete rearranged_indices;
    // swapping indices
    // x = new_indices;
    return y;
}

double parse_one_line(string line)
{
    double number;
    try
    {
        number = stof(line);
        return number;
    }
    catch (const invalid_argument &e)
    {
        throw "Error when parsing number";
    }
}

CArray read_input_file(char *file_name, int &n)
{
    ifstream myfile{file_name};
    string line;
    CArray input_array{};

    if (myfile.is_open())
    {
        getline(myfile, line, '\n');
        n = stoi(line);

        input_array = CArray(n);

        int i = 0;
        while (getline(myfile, line, '\n'))
        {
            double parsed_umber;
            input_array[i] = parse_one_line(line);
            i++;
        }
        myfile.close();
    }

    return input_array;
}

int main(int argc, char **argv)
{
    CArray input;
    int n;
    if (argc == 2)
    {
        try
        {
            input = read_input_file(argv[1], n);
            std::cout << "Successfully parsed input" << std::endl;
        }
        catch (...)
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