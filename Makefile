LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
APP_NAME="fermal"
VSN="pre 0.1"

all: compile

docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '$(VSN)' -s init stop

compile:
	@mkdir -p ebin
	@erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
