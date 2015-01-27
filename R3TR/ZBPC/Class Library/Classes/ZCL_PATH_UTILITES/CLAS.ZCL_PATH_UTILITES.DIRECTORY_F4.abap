  method directory_f4.

    cl_gui_frontend_services=>directory_browse(
      changing
        selected_folder      = directory
      exceptions
        others               = 4 ).

    find regex `\A([a-z]):\\$` in directory ignoring case.
    if sy-subrc <> 0.
      concatenate directory `\` into directory.
    endif.
  endmethod.
