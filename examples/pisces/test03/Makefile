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
IN=      nbLgrid.p2 nbLsolve.p2 nbLdrain.p2
OUT=     nbLgrid.out nbLsolve.out nbLdrain.out

.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

nbLgrid.out: nbLgrid.p2
nbLsolve.out: nbLgrid.out nbLsolve.p2
nbLdrain.out: nbLgrid.out nbLsolve.out nbLdrain.p2

#IV-r.log: pn2a.out
#IV-nr.log: pn2aa.out

# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.sl[v-z] *.sm[0-9] *.out

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
