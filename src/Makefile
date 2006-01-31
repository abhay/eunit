
EMULATOR=beam
EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	eunit.erl \
	eunit_lib.erl \
	eunit_autoexport.erl \
	eunit_examples.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

all: $(OBJECTS)

$(OBJECTS): $(INCLUDE)/eunit.hrl


clean:
	rm -f $(OBJECTS)


$(EBIN)/%.$(EMULATOR):%.erl
	erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
