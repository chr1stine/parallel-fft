### структура

- `main` - реализация лабораторной. Там есть две функции: `sequential` - итеративный последовательный вариант и `parallel` - с использованием потоков.
- `utils` - вспомогательные функции вроде чтения из файла или операотр сравнения для комплексных массивов
- `complex` - реализация необходимой комплексной арифметики

### compile

*prerequisites*: [googletest](https://github.com/google/googletest/blob/main/googletest/README.md),[benchmark for googletest](https://github.com/google/benchmark)

1. `cd erlang`
2. `docker build -t erlang-otp .`
3. `docker run --rm -it -v ${PWD}/app:/usr/src/app -w /usr/src/app erlang-otp`
4. `c(main).`

или

1. `cd erlang`
2. `bash start.sh`
3. `c(utils),c(complex),c(fft),c(main).`

### run

Первым аргументом передается путь к файлу с входным вектором. Вторым аргументом передается количество потоков для распараллеливания.

- `main:run("../tests_data/input1").`