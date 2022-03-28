#include <complex>
#include <valarray>
#include <fstream>

using namespace std;

typedef complex<double> Complex;
typedef valarray<Complex> CArray;

double parse_one_line(std::string line)
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
