  method upload_from_bpcmgr.


    data
    : ld_v__retcode     type sy-subrc
    , ld_v__filesize    type i
    .


    gd_t__delimeter = i_v__delimeter.

    try.

        call method cl_ujp_ut_util=>get_document
          exporting
            i_appset_id   = i_v__appset_id
            i_docname     = i_v__docname
          importing
            et_data_table = data(content).

      catch cx_ujf_file_service_error.
        raise access_denied.
    endtry.

    loop at content assigning field-symbol(<content>).
      append <content>-line to gd_t__strdata.
    endloop.

  endmethod.
