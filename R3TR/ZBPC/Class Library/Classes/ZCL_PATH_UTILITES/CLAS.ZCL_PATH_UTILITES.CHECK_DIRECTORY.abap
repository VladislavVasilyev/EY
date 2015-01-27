  method check_directory.
    data
    : l_regex type string
    .

    if ext is not supplied.
      l_regex = '\A([a-z]):\\([^/:*?"<>$ \r\n]*\\)$'.
      find first occurrence of regex l_regex in directory ignoring case.

      if sy-subrc = 0.
        check = abap_true.
      else.
        check = abap_false.
      endif.
    else.
      l_regex = '\A([a-z]):\\([^/:*?"<>$ \r\n]*\\)([^/:*?"<>$ \r\n]*).' && ext.
      find first occurrence of regex l_regex in directory ignoring case.

      if sy-subrc = 0.
        check = abap_true.
      else.
        check = abap_false.
      endif.
    endif.

  endmethod.
