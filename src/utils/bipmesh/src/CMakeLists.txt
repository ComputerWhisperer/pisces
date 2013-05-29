ADD_EXECUTABLE(bipmesh
reads3.f
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
dump_stride.c
eval.c
findindex.c
generate_deck.c
get3dinfo.c
get3dprofiles.c
getdef3dprofiles.c
getdefprofiles.c
get_info.c
getprofiles.c
lexical.c
main.c
panic.c
parse.c
piscesnodegen.c
preal.c
read_defdop.c
read_deffloat.c
read_defreal.c
read_dop.c
read_float.c
read_real.c
reads3as.c
reduce.c
stride_node.c
writeanaldoping.c
writebaseIV.c
writecollectorIV.c
write_pisces.c
write_stride.c
writesup3ascii.c
writesup3export.c
xdmax.c
)

GET_TARGET_PROPERTY(BIPMESH_BIN bipmesh LOCATION)
ADD_CUSTOM_COMMAND(TARGET bipmesh
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BIN_DIR}
          COMMAND ${CMAKE_COMMAND} -E copy ${BIPMESH_BIN} ${PROJECT_BIN_DIR}/bipmesh
)
