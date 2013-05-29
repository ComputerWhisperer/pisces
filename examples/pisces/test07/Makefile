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
IN=      mesh098.p2 idvg098.p2 
OUT=     mesh098.out idvg098.out 

.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

mesh098.out: mesh098.p2
idvg098.out: mesh098.out idvg098.p2


# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.mshtt *.log *.slv *.out DOP232

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
