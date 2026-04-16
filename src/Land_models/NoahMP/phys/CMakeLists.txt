add_subdirectory("surfex")

add_library(noahmp_phys STATIC
        module_sf_noahmpdrv.F
        module_sf_noahmp_glacier.F
        module_sf_noahmp_groundwater.F
        module_sf_noahmplsm.F
)

target_link_libraries(noahmp_phys noahmp_submodule snowcro crocus_surfex)
add_dependencies(noahmp_phys noahmp_submodule)
