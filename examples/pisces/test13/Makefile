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
IN=      skel.out makegrid.p2 pulse2ns.p2 pulse1.75ns.p2
OUT=     tri1 makegrid.out pulse2ns.out pulse1.75ns.out \
	IV1.75ns.data IV2ns.data


.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT) plot

# ------------------------------------------------------------

makegrid.out: makegrid.p2
pulse2ns.out:  makegrid.out mesh2a
pulse1.75ns.out:  makegrid.out mesh2a


# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.log *.slv mesh* *.out

# ------------------------------------------------------------
tri1: skel.out
	tri skel.out -p tri1

.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1

IV1.75ns.data: IV1.75ns
	sed -e 1d IV1.75ns > IV1.75ns.data

IV2ns.data: IV2ns
	sed -e 1d IV2ns > IV2ns.data

plot: IV1.75ns.data IV2ns.data
	dplot latch.dplot
