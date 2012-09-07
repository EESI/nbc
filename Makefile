%.grm.sig %.grm.sml: %.grm
	$(MLYACC) $^
%.lex.sml: %.lex
	$(MLLEX) $^
%: %.mlb
	$(MLTON) $(MLTONFLAGS) -output $@ $^
count: count.mlb
all: score tabulate
score: score.mlb
tabulate: tabulate.mlb
