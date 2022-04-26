#include "gtest/gtest.h"
// #include <iostream>
#include <string>

#include "../src/include/utils.h"
#include "../src/include/fft.h"

TEST(handlesSequentialFFT, SequentialFftReturnsCorrectValueForExample1)
{
    CArray x = read_array("../tests_data/input1");
    CArray y = fft_iter(x);
    CArray y_expected = read_array("../tests_data/expected_output1");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesSequentialFFT, SequentialFftReturnsCorrectValueForExample2)
{
    CArray x = read_array("../tests_data/input2");
    CArray y = fft_iter(x);
    CArray y_expected = read_array("../tests_data/expected_output2");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesSequentialFFT, SequentialFftReturnsCorrectValueForExample3)
{
    CArray x = read_array("../tests_data/input3");
    CArray y = fft_iter(x);
    CArray y_expected = read_array("../tests_data/expected_output3");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesSequentialFFT, SequentialFftReturnsCorrectValueForExample4)
{
    CArray x = read_array("../tests_data/input4");
    CArray y = fft_iter(x);
    CArray y_expected = read_array("../tests_data/expected_output4");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesSequentialFFT, SequentialFftReturnsCorrectValueForExample7)
{
    CArray x = read_array("../tests_data/input7");
    CArray y = fft_iter(x);
    CArray y_expected = read_array("../tests_data/expected_output7");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesSequentialFFT, SequentialFftReturnsCorrectValueForExample9)
{
    CArray x = read_array("../tests_data/input9");
    CArray y = fft_iter(x);
    CArray y_expected = read_array("../tests_data/expected_output9");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesParallelFFT, ParallelFftReturnsCorrectValueForExample1)
{
    CArray x = read_array("../tests_data/input1");
    CArray y = fft_iter_parallel(x, 4);
    CArray y_expected = read_array("../tests_data/expected_output1");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesParallelFFT, ParallelFftReturnsCorrectValueForExample2)
{
    CArray x = read_array("../tests_data/input2");
    CArray y = fft_iter_parallel(x, 4);
    CArray y_expected = read_array("../tests_data/expected_output2");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesParallelFFT, ParallelFftReturnsCorrectValueForExample3)
{
    CArray x = read_array("../tests_data/input3");
    CArray y = fft_iter_parallel(x, 4);
    CArray y_expected = read_array("../tests_data/expected_output3");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesParallelFFT, ParallelFftReturnsCorrectValueForExample4)
{
    CArray x = read_array("../tests_data/input4");
    CArray y = fft_iter_parallel(x, 4);
    CArray y_expected = read_array("../tests_data/expected_output4");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesParallelFFT, ParallelFftReturnsCorrectValueForExample7)
{
    CArray x = read_array("../tests_data/input7");
    CArray y = fft_iter_parallel(x, 4);
    CArray y_expected = read_array("../tests_data/expected_output7");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

TEST(handlesParallelFFT, ParallelFftReturnsCorrectValueForExample9)
{
    CArray x = read_array("../tests_data/input9");
    CArray y = fft_iter_parallel(x, 4);
    CArray y_expected = read_array("../tests_data/expected_output9");
    EXPECT_TRUE(y == y_expected) << comparison_failed_message(y, y_expected);
}

int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
