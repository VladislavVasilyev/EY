  method path_choose.

    data
    : ld_s__function        type svalp
    , ld_t__functions       type standard table of svalp
    , ld_t__choosefile      type standard table of ty_s__choose_file
    , ld_s__pathlist        type ty_s__filelist
    , ld_s__choosefile      type ty_s__choose_file
    .



    move
    : 'OK'            to ld_s__function-func_name
    , sy-cprog        to ld_s__function-prog_name
    , i_v__form_name  to ld_s__function-form_name.

    append ld_s__function to ld_t__functions.


    loop at i_t__pathlist assigning field-symbol(<ld_s__pathlist>).
      ld_s__choosefile-index = sy-tabix.
      ld_s__choosefile-name = <ld_s__pathlist>-name.
      ld_s__choosefile-path = <ld_s__pathlist>-path.
      ld_s__choosefile-type = <ld_s__pathlist>-type.
      append ld_s__choosefile to ld_t__choosefile.
    endloop.


    call function 'POPUP_GET_SELECTION_FROM_LIST'
      exporting
        display_only                 = 'X'
        table_name                   = `ZBPC_ST_FILE`
        title_bar                    = 'Выберите файлы для загрузки'
      tables
        list                         = ld_t__choosefile
        functions                    = ld_t__functions
      exceptions
        no_tablefields_in_dictionary = 1
        no_table_structure           = 2
        no_title_bar                 = 3
        others                       = 4.

    if sy-subrc <> 0.
      message id sy-msgid type 'E' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    refresh i_t__pathlist.

    check cd_f__popup = abap_true.
    loop at ld_t__choosefile assigning field-symbol(<ld_s__coosefile>) where checkbox = `X`.
      move-corresponding <ld_s__coosefile> to ld_s__pathlist.
      append ld_s__pathlist to i_t__pathlist.
    endloop.

  endmethod.
