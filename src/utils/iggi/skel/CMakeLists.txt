INCLUDE_DIRECTORIES(../misc
${PLOTCAP_GPLOT_INCLUDE}
)
ADD_EXECUTABLE(skel
main.c
dbase.c
alpha.c
check.c
geom.c
griph.c
iltty.c
menu.c
meshio.c
stalloc.c
u2.c
udb.c
user.c
)

ADD_LIBRARY(skellib STATIC
skelp.c
sys.c
)
TARGET_LINK_LIBRARIES(skel skellib misc
plotcap_gplot
plotcap_high2
-lncurses
-lm)

GET_TARGET_PROPERTY(SKEL_BIN skel LOCATION)
ADD_CUSTOM_COMMAND(TARGET skel
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BIN_DIR}
          COMMAND ${CMAKE_COMMAND} -E copy ${SKEL_BIN} ${PROJECT_BIN_DIR}/skel
)

