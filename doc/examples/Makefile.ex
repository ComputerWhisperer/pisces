# Fri Aug 26 11:29:23 PDT 1988
#
# Run the examples from the PISCES-IIb manual.


# ------------------------------------------------------------

PDEV=
#PDEV= DEFPDEV=lw DEFPFIL='x??????.ps'
P2=pisc2

PR=	enscript -2rv
# ------------------------------------------------------------
IN=	pn1a.in  pn2a.in  pn2aa.in  pn3a.in  pn4.in  pn8.in  pn9a.in
OUT=	pn1a.out pn2a.out pn2aa.out pn3a.out pn4.out pn8.out pn9a.out

.SUFFIXES:
.SUFFIXES: .out .slv .log .msh .in


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------
pn2a.out:	pn1a.out
pn2aa.out:	pn1a.out
pn3a.out:	pn1a.out pn2a.out pn2aa.out IV-r.out IV-nr.out
pn4.out:	pn1a.out pn2a.out
pn8.out:	pn1a.out pn2a.out
pn9.out:	pn1a.out pn2a.out

IV-r.out: pn2a.out
IV-nr.out: pn2aa.out

# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.slv

# ------------------------------------------------------------
.in.out:
	$(PDEV) $(P2) $*.in  > $*.out
