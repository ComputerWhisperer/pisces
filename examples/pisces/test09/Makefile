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
IN=	pn1a.p2  pn2a.p2  pn2aa.p2  pn3a.p2  pn4.p2  pn8.p2  pn9a.p2
OUT=	pn1a.out pn2a.out pn2aa.out pn3a.out pn4.out pn8.out pn9a.out

.SUFFIXES:
.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------
pn2a.out:	pn1a.out
pn2aa.out:	pn1a.out
pn3a.out:	pn1a.out pn2a.out pn2aa.out IV-r.log IV-nr.log
pn4.out:	pn1a.out pn2a.out
pn8.out:	pn1a.out pn2a.out
pn9.out:	pn1a.out pn2a.out

IV-r.log: pn2a.out
IV-nr.log: pn2aa.out

# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.slv *.out

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
