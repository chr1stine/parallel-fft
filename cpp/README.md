### структура

- `main.cpp` - реализация лабораторной. Там есть две функции: `fft_iter` - итеративный последовательный вариант и `fft_iter_parallel` - с использованием потоков.
- `utils.cpp` - вспомогательные функции вроде чтения из файла или операотр сравнения для комплексных массивов

### compile

*prerequisites*: [googletest](https://github.com/google/googletest/blob/main/googletest/README.md),[benchmark for googletest](https://github.com/google/benchmark)

1. `cd cpp`
2. `cmake .`
3. `make`

### run

Первым аргументом передается путь к файлу с входным вектором. Вторым опциональным аргументом передается количество потоков для распараллеливания.

- `./main ./tests/input1` 
- `./cpp/main ./tests/input1 2`

## test

- `./runUnitTests`
- `./runPerfomanceTests`

### план по коду

1. <s>парсинг действительных чисел</s>
2. парсинг комплексных чисел
3. <s>функция вычисления fft последовательно</s>
4. <s>исправить итеративный вариант fft</s>
5. <s>распараллелить</s>
6. реализовать padding входного вектора до степени двойки(??)

### random

O(log(N)) - time complexity of built-in pow function implementation in c/c++ [сурс](https://discuss.codechef.com/t/built-in-power-function-complexity/8901)

сложность функции `reverse_bits`: O(log(n)), т.к. битовые операции константны(О(1)), и они применяются в цикле по количеству бит в бинарном представлении количества элементов в массиве
