c Wed Apr  6 15:57:59 PDT 1988
c
c "p1com0": PLOT 1D common variables.
c
c ------------------------------------------------------------------------
c
c .. Physical Origin, Lengths and Device sizes
	real	xaorg, yaorg
	real	xalen, yalen
	real	xsize, ysize
	    common/p1com0/ xaorg, yaorg
	    common/p1com0/ xalen, yalen
	    common/p1com0/ xsize, ysize
c
c .. Min and Max values to be plotted.
	real	vxmin, vymin, vxmax, vymax
	    common/p1com0/ vxmin, vymin, vxmax, vymax
c
c .. Axis Title(label) height, Axis TicLabel Height, Tic Height
c .. Main title size, by.symbol symbol size
	real	tlhite, lhite, thite, titlsz
	real    symbsz
	    common/p1com0/ tlhite, lhite, thite, titlsz
	    common/p1com0/ symbsz

c .. Header (Name/Title), and version string
	character header*9, hdvers*4
	    common/p1com0/ header, hdvers
