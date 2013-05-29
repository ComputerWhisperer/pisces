# Fri Aug 26 11:29:23 PDT 1988
#
# Run the examples from the PISCES-IIb manual.


# ------------------------------------------------------------

#PDEV=DEFPDEV=sumq
# PDEV= DEFPDEV=save DEFPFIL='x??????.gp'
PDEV=
P2=pisc2

PR=     enscript -2rv
# ------------------------------------------------------------
IN=      nmos.p2 mosIV.p2
OUT=     nmos.out mosIV.out

.SUFFIXES: .out .slv .log .msh .p2

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1

# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

nmos.out: nmos.p2

mosIV.out: nmos.out mosIV.p2


# ------------------------------------------------------------
print: $(IN)
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.slv *.out
