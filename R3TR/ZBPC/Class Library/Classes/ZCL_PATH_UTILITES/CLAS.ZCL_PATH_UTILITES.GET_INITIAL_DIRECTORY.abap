  method get_initial_directory.
    cl_gui_frontend_services=>get_desktop_directory(
   changing
     desktop_directory    = directory ).
    cl_gui_cfw=>flush( ).

    concatenate directory `\` into directory.

  endmethod.
