INCLUDE_DIRECTORIES(../misc ../sparse ../skel 
${PLOTCAP_GPLOT_INCLUDE}
)
ADD_EXECUTABLE(tri
bfill.c
ckmesh.c
dbase.c
flip.c
geom.c
gn_asm.c
gn_func.c
gn_opt.c
hinit.c
laplace.c
list.c
main.c
matrix.c
meshio.c
opt.c
plot.c
score.c
triang.c
tridata.c
triheur.c
)
TARGET_LINK_LIBRARIES(tri misc sparse skellib
plotcap_gplot
plotcap_high2
-lncurses
-lm)

GET_TARGET_PROPERTY(TRI_BIN tri LOCATION)
ADD_CUSTOM_COMMAND(TARGET tri
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BIN_DIR}
          COMMAND ${CMAKE_COMMAND} -E copy ${TRI_BIN} ${PROJECT_BIN_DIR}/tri
)
