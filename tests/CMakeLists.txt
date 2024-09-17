
# tests for getini tool

if ( HAVE_BUILD_TOOLS )
    
    ecbuild_configure_file(
        test_getini1c.sh.in
        test_getini1c.sh @ONLY
    )

    ecbuild_add_test(
        TYPE     SCRIPT
        COMMAND  test_getini1c.sh
    )

endif()
