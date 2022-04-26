#include "gtest/gtest.h"
#include "../src/include/fft.h"
#include "../src/include/utils.h"
#include <benchmark/benchmark.h>
#include <iostream>

// size 4
static void BM_Sequential_size4(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input1");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size4(benchmark::State &state)
{
    CArray x = read_array("../../../tests_data/input1");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size4(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input1");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size4(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input1");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size4(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input1");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 8
static void BM_Sequential_size8(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input2");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size8(benchmark::State &state)
{
    CArray x = read_array("../../../tests_data/input2");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size8(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input2");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size8(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input2");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size8(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input2");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 16
static void BM_Sequential_size16(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input3");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size16(benchmark::State &state)
{
    CArray x = read_array("../../../tests_data/input3");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size16(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input3");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size16(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input3");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size16(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input3");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 32
static void BM_Sequential_size32(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input4");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size32(benchmark::State &state)
{
    CArray x = read_array("../../../tests_data/input4");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size32(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input4");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size32(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input4");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size32(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input4");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 64
static void BM_Sequential_size64(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input7");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size64(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input7");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size64(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input7");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size64(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input7");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size64(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input7");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 128
static void BM_Sequential_size128(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input8");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size128(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input8");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size128(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input8");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size128(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input8");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size128(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input8");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 256
static void BM_Sequential_size256(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input9");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size256(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input9");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size256(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input9");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size256(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input9");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size256(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input9");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 512
static void BM_Sequential_size512(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input10");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size512(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input10");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size512(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input10");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size512(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input10");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size512(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input10");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 1024
static void BM_Sequential_size1024(benchmark::State &state)
{
    // std::cout << "HERE" << std::endl;
    CArray x = read_array("../tests_data/input5");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size1024(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input5");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size1024(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input5");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size1024(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input5");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size1024(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input5");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 4096
static void BM_Sequential_size4096(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input6");
    for (auto _ : state)
        CArray y = fft_iter(x);
}

static void BM_Parallel_1thread_size4096(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input6");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 1);
}

static void BM_Parallel_4threads_size4096(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input6");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 4);
}

static void BM_Parallel_8threads_size4096(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input6");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 8);
}

static void BM_Parallel_16threads_size4096(benchmark::State &state)
{
    CArray x = read_array("../tests_data/input6");
    for (auto _ : state)
        CArray y = fft_iter_parallel(x, 16);
}

// size 4
BENCHMARK(BM_Sequential_size4);
BENCHMARK(BM_Parallel_4threads_size4);
BENCHMARK(BM_Parallel_8threads_size4);
BENCHMARK(BM_Parallel_16threads_size4);

// size 8
BENCHMARK(BM_Sequential_size8);
BENCHMARK(BM_Parallel_4threads_size8);
BENCHMARK(BM_Parallel_8threads_size8);
BENCHMARK(BM_Parallel_16threads_size8);

// size 16
BENCHMARK(BM_Sequential_size16);
BENCHMARK(BM_Parallel_4threads_size16);
BENCHMARK(BM_Parallel_8threads_size16);
BENCHMARK(BM_Parallel_16threads_size16);

// size 32
BENCHMARK(BM_Sequential_size32);
BENCHMARK(BM_Parallel_4threads_size32);
BENCHMARK(BM_Parallel_8threads_size32);
BENCHMARK(BM_Parallel_16threads_size32);

// size 64
BENCHMARK(BM_Sequential_size64);
BENCHMARK(BM_Parallel_4threads_size64);
BENCHMARK(BM_Parallel_8threads_size64);
BENCHMARK(BM_Parallel_16threads_size64);

// size 128
BENCHMARK(BM_Sequential_size128);
BENCHMARK(BM_Parallel_4threads_size128);
BENCHMARK(BM_Parallel_8threads_size128);
BENCHMARK(BM_Parallel_16threads_size128);

// size 256
BENCHMARK(BM_Sequential_size256);
BENCHMARK(BM_Parallel_4threads_size256);
BENCHMARK(BM_Parallel_8threads_size256);
BENCHMARK(BM_Parallel_16threads_size256);

// size 512
BENCHMARK(BM_Sequential_size512);
BENCHMARK(BM_Parallel_4threads_size512);
BENCHMARK(BM_Parallel_8threads_size512);
BENCHMARK(BM_Parallel_16threads_size512);

// size 1024
BENCHMARK(BM_Sequential_size1024);
BENCHMARK(BM_Parallel_4threads_size1024);
BENCHMARK(BM_Parallel_8threads_size1024);
BENCHMARK(BM_Parallel_16threads_size1024);

// size 4096
BENCHMARK(BM_Sequential_size1024);
BENCHMARK(BM_Parallel_4threads_size4096);
BENCHMARK(BM_Parallel_8threads_size4096);
BENCHMARK(BM_Parallel_16threads_size4096);

// int setup(int argc, char **argv)
// {
//     // ::testing::InitGoogleTest(&argc, argv);
//     x = read_array("../../../tests_data/input1");
//     // return RUN_ALL_TESTS();
// }

BENCHMARK_MAIN();
