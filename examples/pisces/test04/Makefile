# Fri Aug 26 11:29:23 PDT 1988
#
# Run the examples from the PISCES-IIb manual.


# ------------------------------------------------------------

#PDEV=DEFPDEV=sumq
#PDEV= DEFPDEV=save DEFPFIL='x??????.gp'
PDEV=
P2=pisc2

PR=	enscript -2rv
# ------------------------------------------------------------
IN=      n3egrid.p2 n3esolve.p2 n3edrain.p2
OUT=     n3egrid.out n3esolve.out n3edrain.out

.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

n3egrid.out: n3egrid.p2
n3esolve.out: n3egrid.out n3esolve.p2
n3edrain.out: n3egrid.out n3esolve.out n3edrain.p2


# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.sl[v-z] *.sm[0-9] *.out

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
