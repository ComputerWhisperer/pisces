INCLUDE_DIRECTORIES(../s4stuff)
ADD_EXECUTABLE(ig2image
fill_grid.c
get_graphsize.c
incfil.c
main.c
makeframe.c
make_grid.c
mylog.c
newget_solval.c
s4incf.c
scan_s4file.c
window.c
write_imagetool.c
readp2.f
)
TARGET_LINK_LIBRARIES(ig2image s4)

GET_TARGET_PROPERTY(IG2IMAGE_BIN ig2image LOCATION)
ADD_CUSTOM_COMMAND(TARGET ig2image
          POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BIN_DIR}
          COMMAND ${CMAKE_COMMAND} -E copy ${IG2IMAGE_BIN} ${PROJECT_BIN_DIR}/ig2image
)

