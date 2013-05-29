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
IN=      mesh.p2 lowbias.p2
OUT=     mesh.out lowbias.out

.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

mesh.out: mesh.p2
lowbias.out:  lowbias.p2 mesh.out


# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.slv *.out bip.*

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
