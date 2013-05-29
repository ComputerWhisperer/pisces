ADD_EXECUTABLE(pisces 
acanal.f
breakdown.f
clock.f
disct.f
doping.f
erite.f
extract.f
filopncls.f
flowline.f
impact.f
incfil.f
interface.f
loadsol.f
machplot.f
mc.f
mcintfc.f
md.f
meshgen.f
meshio.f
meshrect.f
method.f
misc.f
mobil.f
options.f
pcrchol.f
pisc2.f
pisces.f
plotter.f
plotutil.f
plotvec.f
printer.f
regrid.f
s3read.f
s4read.f
setmat.f
smobl.f
smooth.f
solasmb1.f
solasmb2.f
solgummel.f
sollud.f
solnewton.f
solver.f
stats.f
symbol.f
trefld.f
trfldmob.f
usrmob1.f
version.f
volume.f
)

TARGET_LINK_LIBRARIES(pisces genii3 sysf77lib plotcap_f77links plotcap_gplot plotcap_high2 edline -lreadline)
GET_TARGET_PROPERTY(PISCES_BIN pisces LOCATION)
ADD_CUSTOM_COMMAND(TARGET pisces
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BIN_DIR}
          COMMAND ${CMAKE_COMMAND} -E copy ${PISCES_BIN} ${PROJECT_BIN_DIR}/pisces.bin
)

#use tests to generate key file


