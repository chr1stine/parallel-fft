#include <string>
#include <valarray>
#include <complex>
#include <fstream>

#include "./include/utils.h"

using namespace std;

Complex parse_one_line(string s)
{
    try
    {
        std::string real = std::string("");
        std::string imag = std::string("");
        if (s[0] != '(' || s[s.size() - 1] != ')')
        {
            return Complex(stof(s));
        }

        int i = 1;
        while (i < s.size() - 1 && s[i] != ',')
        {
            real += s[i];
            i++;
        }
        double realPart = std::stof(real);
        if (i >= s.size() - 1)
        {
            throw "no imaginary part";
        }

        i++;
        while (i < s.size() - 1)
        {
            imag += s[i];
            i++;
        }
        double imagPart = std::stof(imag);
        Complex number = Complex(realPart, imagPart);
        return number;
    }
    catch (const invalid_argument &e)
    {
        throw "Error when parsing number";
    }
}

CArray read_array(const char *file_name)
{
    ifstream myfile{file_name};
    string line;
    CArray input_array{};

    int n = 0;
    if (myfile.is_open())
    {
        getline(myfile, line, '\n');
        n = stoi(line);
        input_array = CArray(n);

        int i = 0;
        while (getline(myfile, line, '\n'))
        {
            input_array[i] = parse_one_line(line);
            i++;
        }
        myfile.close();
    }

    if (n == 0)
    {
        throw "file is empty";
    }

    return input_array;
}

bool operator==(const CArray &lhs, const CArray &rhs)
{
    if (lhs.size() != rhs.size())
    {
        return false;
    }

    for (int i = 0; i < lhs.size(); i++)
    {
        if (abs(lhs[i] - rhs[i]) > 1e-2)
        {
            return false;
        }
    }
    return true;
}

string comparison_failed_message(CArray y, CArray x)
{
    string s = "";

    s += "\nExpected array: \n";
    for (int i = 0; i < x.size(); i++)
    {
        s += "(";
        s += to_string(x[i].real());
        s += ",";
        s += to_string(x[i].imag());
        s += ")\n";
    }

    s += "\nActual array: \n";
    for (int i = 0; i < y.size(); i++)
    {
        s += "(";
        s += to_string(y[i].real());
        s += ",";
        s += to_string(y[i].imag());
        s += ")\n";
    }

    s += "\nAbsolute difference:\n";
    for (int i = 0; i < x.size(); i++)
    {
        s += to_string(abs(x[i] - y[i]));
        s += "\n";
    }

    return s;
}
