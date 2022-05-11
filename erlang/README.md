### структура

- `main.erl` - реализация лабораторной. Там есть две функции: `fft_iter` - итеративный последовательный вариант и `fft_iter_parallel.erl` - с использованием потоков.
- `utils.erl` - вспомогательные функции вроде чтения из файла или операотр сравнения для комплексных массивов

### compile

*prerequisites*: [googletest](https://github.com/google/googletest/blob/main/googletest/README.md),[benchmark for googletest](https://github.com/google/benchmark)

1. `cd erlang`
2. `docker build -t erlang-otp .`
3. `docker run --rm -it -v ${PWD}/app:/usr/src/app -w /usr/src/app erlang-otp`
4. `c(main).`

### run

Первым аргументом передается путь к файлу с входным вектором. Вторым опциональным аргументом передается количество потоков для распараллеливания.

- `main:fft("../tests./input1").`

## test

- `./runUnitTests`
- `./runPerfomanceTests`