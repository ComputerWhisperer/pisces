Title pn diode (Fig 1a)
$
$ Generate and plot the mesh for the diode example
$
mesh	inf=pn.msh
plot.2	bound no.tic no.fill grid pause
$ Make FIGURE 1c
plot.1 log dop abs a.x=0 b.x=5 b.y=0.5 a.y=0.5 points

end
