  method upload_from_pc.


    data
    : ld_v__retcode     type sy-subrc
    , ld_v__filesize    type i
    .


    gd_t__delimeter = i_v__delimeter.

    call method cl_gui_frontend_services=>gui_upload
      exporting
        filename        = i_v__filename
        filetype        = `ASC`
      importing
        filelength      = ld_v__filesize
      changing
        data_tab        = gd_t__strdata
      exceptions
        access_denied   = 1
        file_open_error = 2
        file_read_error = 3
        others          = 13.

    case sy-subrc.
      when 0.
      when 1.
        raise access_denied.
      when 2.
        raise file_open_error.
      when 3.
        raise file_read_error.
      when others.
        raise other.
    endcase.

  endmethod.
