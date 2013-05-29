# Wed Sep 14 12:44:42 PDT 1988 (dredge -- Stanford)

NROFF=	nroff -ms

TDEV= -Tpsc
TROFF=	ptroff -ms -t > paper-tr
GROFF=  groff -ms -t -e

TMAC=	../macros.tmac


ALL= overview.ms $(CARDS) end.ms

CARDS= \
	c_check.ms \
	c_comment.ms \
	c_contact.ms \
	c_contour.ms \
	c_doping.ms \
	c_elec.ms \
	c_elim.ms \
	c_end.ms \
	c_extract.ms \
	c_impact.ms \
	c_include.ms \
	c_interface.ms \
	c_load.ms \
	c_log.ms \
	c_mater.ms \
	c_mesh.ms \
	c_method.ms \
	c_models.ms \
	c_option.ms \
	c_plot1d.ms \
	c_plot2d.ms \
	c_print.ms \
	c_region.ms \
	c_regrid.ms \
	c_solve.ms \
	c_spread.ms \
	c_symb.ms \
	c_title.ms \
	c_vector.ms \
	c_xmesh.ms


paper-ps: $(ALL) toc.gr
	$(GROFF) $(TMAC) toc.gr $(ALL) > paper-ps

paper-nr.cat: $(ALL) toc.tr
	tbl $(TMAC) toc.tr $(ALL) | neqn | $(NROFF) > paper-nr.cat

paper-tr: $(ALL) toc.tr
	tbl $(TMAC) toc.tr $(ALL) | eqn | $(TROFF)

dit: $(ALL) toc.tmp
	tbl $(TMAC) $(ALL) | dieqn $(TDEV) | ditroff $(TDEV) -t -ms > dit

pre: $(ALL) toc.gr
	gtbl $(TMAC) toc.gr $(ALL) | geqn | groff -TX75 -ms 

# Table of contents.
#toc.tr: $(ALL) toc.tmp
#	tbl $(TMAC) toc.tmp $(ALL) | dieqn $(TDEV) | \
#			ditroff $(TDEV) -ms -t >xx
#	toc-make toc-pages.tr > toc.tr

toc.tr: $(ALL) toc.tmp
	groff -e -t -ms $(TMAC) toc.tmp $(ALL)  > xx
	toc-make toc-pages.tr > toc.tr

toc.gr: toc.tr
	tbl ../macros.tmac toc.tr | eqn > toc.gr

toc.nr: $(ALL) toc.tmp
	tbl $(TMAC) toc.tmp $(ALL) | neqn | $(NROFF) > paper-nr.cat
	toc-nr.filt paper-nr.cat > toc-nr.tmp
	toc-make toc-nr.tmp > toc.nr

toc.tmp:
	echo Overview 1 | toc-make > toc.tmp


please:
release: please paper-nr.cat
	rm -rf release
	mkdir release
	cp $(ALL) release
	cp Makefile release
	cp toc-make toc-page release
	cp paper-nr.cat toc.tr release


