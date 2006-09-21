
#DEBUG = -DDEBUG=true
#TEST = -DNOTEST=true

EMULATOR=beam
# must have "eunit/ebin" as part of the path for include_lib to work, for now
EBIN=../../eunit/ebin
#EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	eunit_autoexport.erl \
	eunit.erl \
	eunit_server.erl \
	eunit_proc.erl \
	eunit_serial.erl \
	eunit_test.erl \
	eunit_lib.erl \
	eunit_data.erl \
	eunit_tty.erl \
	code_monitor.erl \
	file_monitor.erl \
	autoload.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

all: $(OBJECTS)

$(OBJECTS): eunit_internal.hrl \
	$(INCLUDE)/eunit.hrl 

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN)  $(DEBUG) $(TEST) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
