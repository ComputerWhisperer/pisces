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
IN=     shot.p2 schot.p2
OUT=    shot.out schot.out

.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

shot.out: shot.p2
schot.out: schot.p2

# shot.log: shot.out
#IV-nr.log: pn2aa.out

# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.slv $(OUT)

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
