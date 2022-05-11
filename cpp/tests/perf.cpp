#include "gtest/gtest.h"
#include "../src/include/fft.h"
#include "../src/include/utils.h"
#include <benchmark/benchmark.h>
#include <iostream>

static void BM_sequential(benchmark::State &state)
{
    for (auto _ : state)
    {
        int i = state.range(0);

        char path[] = "../test_data/input";
        char testN[6];
        sprintf(testN, "%d", i + 1);
        strcat(path, testN);

        CArray x = read_array(path);
        CArray y = fft_iter(x);
    }
}

static void BM_parallel(benchmark::State &state)
{
    for (auto _ : state)
    {
        int i = state.range(0);

        char path[] = "../test_data/input";
        char testN[6];
        sprintf(testN, "%d", i + 1);
        strcat(path, testN);

        CArray x = read_array(path);
        CArray y = fft_iter_parallel(x, state.threads());
    }
}

// sequential
BENCHMARK(BM_sequential)->Arg(8)->Arg(9)->Arg(10)->Arg(11)->Arg(12)->Arg(13)->Arg(14)->Arg(15)->Threads(1);

// 4 threads
BENCHMARK(BM_parallel)->Arg(8)->Arg(9)->Arg(10)->Arg(11)->Arg(12)->Arg(13)->Arg(14)->Arg(15)->Threads(4);

// 8 threads
BENCHMARK(BM_parallel)->Arg(8)->Arg(9)->Arg(10)->Arg(11)->Arg(12)->Arg(13)->Arg(14)->Arg(15)->Threads(8);

// 16 threads
BENCHMARK(BM_parallel)->Arg(8)->Arg(9)->Arg(10)->Arg(11)->Arg(12)->Arg(13)->Arg(14)->Arg(15)->Threads(16);

// 32 threads
BENCHMARK(BM_parallel)->Arg(8)->Arg(9)->Arg(10)->Arg(11)->Arg(12)->Arg(13)->Arg(14)->Arg(15)->Threads(32);

BENCHMARK_MAIN();
