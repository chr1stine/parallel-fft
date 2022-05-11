docker build -t erlang-otp .;
docker run --rm -it -v ${PWD}/app:/usr/src/app -w /usr/src/app erlang-otp