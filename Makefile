###========================================================================
### File: Makefile
###
###
### Author(s):
###     - xionglikarl <xiongli.karl@gmail.com>
###
###-- LICENSE -------------------------------------------------------------
### The MIT License (MIT)
###
### Copyright (c) 2016, xionglikarl <xiongli.karl@gmail.com>
###
### Permission is hereby granted, free of charge, to any person obtaining a
### copy of this software and associated documentation files (the
### "Software"), to deal in the Software without restriction, including
### without limitation the rights to use, copy, modify, merge, publish,
### distribute, sublicense, and/or sell copies of the Software, and to
### permit persons to whom the Software is furnished to do so, subject to
###  the following conditions:
###
### The above copyright notice and this permission notice shall be included
### in all copies or substantial portions of the Software.
###
### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
### OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
### IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
### CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
### TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
### SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
###========================================================================
.PHONY: all compile test eunit test_debug clean clean-all


## Macros
##-------------------------------------------------------------------------
REBAR = ./rebar3
TEST_DIR = ./_build/test/lib/esproto


## Settings
##-------------------------------------------------------------------------
EUNIT_OPTS ?=
## you can change test case(like "test1_test") to see details
DEBUG_TEST_CASE ?= test1_test	

## Targets
##-------------------------------------------------------------------------
all: compile

compile:
	$(REBAR) compile

test: eunit

eunit:
	$(REBAR) eunit $(EUNIT_OPTS)

test_debug:
	$(REBAR) eunit $(EUNIT_OPTS)
	erl -pa $(TEST_DIR)/ebin $(TEST_DIR)/test -noshell -s test $(DEBUG_TEST_CASE) -s init stop

clean:
	$(REBAR) clean

clean-all: clean
	$(REBAR) unlock
	rm -rf */*~
	rm -rf _build
