  method file_f4.

    data
      : lt_file_table type filetable
      , ls_file_table like line of lt_file_table.

    data: lv_rc type sy-subrc.

    cl_gui_frontend_services=>file_open_dialog(
      changing
        file_table = lt_file_table
        rc         = lv_rc ).
    clear ls_file_table .
    read table lt_file_table into ls_file_table index 1.
    if sy-subrc  = 0.
      file = ls_file_table-filename.
    endif.

  endmethod.
