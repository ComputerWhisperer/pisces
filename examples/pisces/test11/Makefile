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
IN=     fig03.p2 fig04.p2 fig05.p2 fig08.p2 fig15.p2 fig19.p2
OUT=    fig03.out fig04.out fig05.out fig08.p2 fig15.out fig19.out

.SUFFIXES:
.SUFFIXES: .out .slv .log .msh .p2


# ------------------------------------------------------------
all: $(OUT)

# ------------------------------------------------------------

fig15.out:        fig03.out fig08.out
fig19.out:        fig03.out fig08.out

IVsub.log: fig03.out
IVdrain.log: fig03.out

# ------------------------------------------------------------
print:
	$(PR) $(IN)

clean:
	rm -f *.msh *.mshtt *.log *.slv *.out

# ------------------------------------------------------------
.p2.out:
	$(PDEV) $(P2) $*.p2  > $*.out 2>&1
