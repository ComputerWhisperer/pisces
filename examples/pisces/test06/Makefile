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
IN=      ex13ig.p2 exn13g37.p2 
OUT=     ex13ig.out exn13g37.out 

.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

ex13ig.out: ex13ig.p2
exn13g37.out: ex13ig.out exn13g37.p2


# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.mshtt *.log *.slv *.out DOP07 G37LOG1

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
