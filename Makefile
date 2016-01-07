.PHONY: pipe cal env doc userdoc opendoc unit help clean

FLAGS=
QUIET=0

REVISION:=$(shell git rev-parse --short HEAD)
PHONE=$(shell cat $(HOME)/.phonenumber 2> /dev/null)

ifeq ($(QUIET), 1)
  ECHO_PREFIX=@
else
  ECHO_PREFIX=
endif

IDL=idl82

OS:=$(shell uname)

ifeq ($(OS), Darwin)
  OPEN=open
else
  OPEN=firefox
endif


MACHINE=$(shell comp_get_hostname.sh)
CONFIG=config/comp.$(USER).$(MACHINE)$(FLAGS).cfg

SSW_DIR=$(PWD)/ssw
GEN_DIR=$(PWD)/gen
LIB_DIR=$(PWD)/lib
COMP_SRC_DIR=$(PWD)/src

MGLIB_DIR=+$(HOME)/software/mglib/lib
MGUNIT_DIR=$(HOME)/software/mgunit/lib
IDLDOC_DIR=+$(HOME)/projects/idldoc/src
FULL_SSW_DIR=/hao/contrib/ssw

COMP_PATH=+$(COMP_SRC_DIR):$(SSW_DIR):$(GEN_DIR):$(LIB_DIR):"<IDL_DEFAULT>"
DOC_PATH=$(MGLIB_DIR):$(IDLDOC_DIR):$(COMP_PATH)
UNIT_PATH=$(PWD)/unit:$(MGUNIT_DIR):$(COMP_PATH)

help:
	@echo "Running on $(MACHINE) by $(USER), using $(CONFIG)"
	@echo
	@echo "Targets:"
	@echo " pipe             run the CoMP pipeline"
	@echo " cal              run the example calibration"
	@echo " env              start IDL with the CoMP pipeline paths"
	@echo " doc              generate the CoMP pipeline API documentation"
	@echo " userdoc          generate the user-level CoMP pipeline API documentation"
	@echo " opendoc          open the CoMP pipeline API docs in a web browser"
	@echo " unit             run the CoMP pipeline unit tests"
	@echo " clean            clean API documentation"

pipe:
	$(ECHO_PREFIX)$(IDL) -IDL_STARTUP "" -IDL_PATH $(COMP_PATH) -e "comp_run_pipeline, config_filename='$(CONFIG)'"
	@if [ "$(PHONE)" ]; then \
	echo "Sending message to $(PHONE)..."; \
	sms -n $(PHONE) -m "Done processing new pipeline on $(MACHINE)"; \
	else \
	echo "Put phone number in $(HOME)/.phonenumber to be notified when done"; \
	fi

cal:
	$(ECHO_PREFIX)$(IDL) -IDL_STARTUP "" -IDL_PATH calibration:$(COMP_PATH):"+$(FULL_SSW_DIR)" comp_cal_example_script

env:
	$(ECHO_PREFIX)$(IDL) -IDL_STARTUP "" -IDL_PATH $(COMP_PATH)

doc:
	$(ECHO_PREFIX)$(IDL) -IDL_STARTUP "" -IDL_PATH $(DOC_PATH) -e "comp_make_docs"

userdoc:
	$(ECHO_PREFIX)$(IDL) -IDL_STARTUP "" -IDL_PATH $(DOC_PATH) -e "comp_make_docs, /user"

opendoc:
	$(ECHO_PREFIX)$(OPEN) api-docs/index.html

unit:
	$(ECHO_PREFIX)$(IDL) -IDL_STARTUP "" -IDL_PATH $(UNIT_PATH) -e "comp_run_unittests"

clean:
	$(ECHO_PREFIX)rm -rf api-docs api-userdocs
