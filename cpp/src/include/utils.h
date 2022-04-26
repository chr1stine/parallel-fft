#pragma once

#include <string>
#include <complex>
#include <valarray>

typedef std::complex<double> Complex;
typedef std::valarray<Complex> CArray;

std::string comparison_failed_message(CArray x, CArray y);
bool operator==(const CArray &lhs, const CArray &rhs);

Complex parse_one_line(std::string line);
CArray read_array(const char *file_name);