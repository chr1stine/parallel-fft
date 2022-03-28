#include <complex>
#include <valarray>

typedef std::complex<double> Complex;
typedef std::valarray<Complex> CArray;

double parse_one_line(std::string line);
CArray read_input_file(char *file_name, int &n);