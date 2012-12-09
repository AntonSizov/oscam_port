NAME=oscam_port
OSCAM-REP=oscam-light
OSCAM-BIN=$(OSCAM-REP)/Distribution/oscam-1.20-unstable_svn0-x86_64-linux-gnu
OSCAM-PRIV="./priv/oscam"

all: compile

compile: oscam get-deps
	@./rebar compile

oscam: $(OSCAM-PRIV)

$(OSCAM-PRIV): $(OSCAM-BIN) priv
	@cp $(OSCAM-BIN) $(OSCAM-PRIV)

priv:
	@mkdir priv

$(OSCAM-BIN): $(OSCAM-REP)
	@cp ./etc/oscam-port.c ./$(OSCAM-REP)/
	@cp ./etc/oscam-Makefile ./$(OSCAM-REP)/Makefile
	@make -C ./$(OSCAM-REP)

$(OSCAM-REP):
	@git clone https://github.com/AntonSizov/$(OSCAM-REP).git

rebuild-c-port:
	@cp ./priv/oscam-port.c ./$(OSCAM-REP)/
	@make -C $(OSCAM-REP) Distribution/oscam-1.20-unstable_svn0-x86_64-linux-gnu

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

test: compile
	@erl -env ERL_LIBS "../" -pa ebin/ \
		-eval 'application:start($(NAME))' \
		-eval 'oscam:test()' \
		-s init stop