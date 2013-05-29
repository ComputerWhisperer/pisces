#INCLUDE_DIRECTORIES()
ADD_EXECUTABLE(mosmesh
asciicalc.c
calc_junc.c
calcnode.c
calcratio.c
calcspacing.c
computesubstratetype.c
depl_width.c
dmax.c
dmin.c
doublenode.c
dump_info.c
eval.c
generate_deck.c
getdefmosdope.c
getdefprofiles.c
getinfo.c
getmosdope.c
getprofiles.c
lexical.c
longMOSnodegen.c
mosmesh.c
panic.c
parse.c
preal.c
read_deffloat.c
read_defreal.c
readfloat.c
readreal.c
reads3as.c
reads3.f
reduce.c
shortMOSnodegen.c
writeanaldoping.c
writedrainIV.c
writegateIV.c
writemosmesh.c
writesup3ascii.c
writesup3export.c
xdmax.c
)
#TARGET_LINK_LIBRARIES(mosmesh)

GET_TARGET_PROPERTY(MOSMESH_BIN mosmesh LOCATION)
ADD_CUSTOM_COMMAND(TARGET mosmesh
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BIN_DIR}
          COMMAND ${CMAKE_COMMAND} -E copy ${MOSMESH_BIN} ${PROJECT_BIN_DIR}/mosmesh
)

