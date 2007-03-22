
APPNAME=eunit

SUB_DIRECTORIES = src

include vsn.mk

DOC_OPTS=[{def,{version,"$(EUNIT_VSN)"}}]


all:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE)); \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) clean); \
	done

docs:
	erl -noshell -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop

test: all
	erl -noshell -pa ebin -s eunit test -s init stop
