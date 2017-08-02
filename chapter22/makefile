.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = job_center4 worker4

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump