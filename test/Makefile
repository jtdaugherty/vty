VERIF_TESTS := \
verify_attribute_ops \
verify_display_attributes \
verify_parse_terminfo_caps \
verify_eval_terminfo_caps \
verify_utf8_width \
verify_image_ops \
verify_image_trans \
verify_empty_image_props \
verify_picture_ops \
verify_span_ops \
verify_debug_terminal \
verify_inline \


TESTS :=\
benchmark \
Test \
Test2 \
yi_issue_264 \
vty_issue_18 \
$(VERIF_TESTS)

$(shell mkdir -p objects )

# TODO: Tests should also be buildable referencing the currently installed vty
GHC_ARGS=--make -i../src \
		 -package parallel \
		 -package deepseq-1.3.0.1 \
		 -hide-package transformers \
		 -package QuickCheck \
		 -ignore-package vty \
		 ../cbits/gwinsz.c ../cbits/set_term_timing.c  ../cbits/mk_wcwidth.c \
		 -O -funbox-strict-fields -Wall -threaded -fno-full-laziness -fspec-constr -fspec-constr-count=10 \
		 -rtsopts \
		 -odir objects -hidir objects

GHC_PROF_ARGS=-prof -auto-all $(GHC_ARGS)

SOURCE := $(shell find ../src ../cbits -name '*.hs' -print -or -name '*.c' -print -or -name '*.h' -print)

.PHONY: all
all : $(VERIF_TESTS)

.PHONY: $(TESTS)
.SECONDEXPANSION:
$(TESTS) : 
	@echo running test $@
	@mkdir -p results/$@
	( ghc $(GHC_PROF_ARGS) $@ \
	&& time ./$@ +RTS -p -sresults/$@/mem_report \
	&& cp $@.prof results/$@/profile \
	)

.PHONY: interactive_terminal_test
interactive_terminal_test : 
	ghc $(GHC_ARGS) $@ && ./$@

.PHONY: core
core :
	ghc-core --no-asm --no-cast -- $(GHC_ARGS) Bench

