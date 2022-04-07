### compiling

`g++ -pthread ./cpp/main.cpp ./cpp/reading_input.cpp -o ./cpp/main`

### running

`./cpp/main ./tests/input1`

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
