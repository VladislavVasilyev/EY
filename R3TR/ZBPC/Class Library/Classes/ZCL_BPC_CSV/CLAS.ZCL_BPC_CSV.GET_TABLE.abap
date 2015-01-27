  method get_table.

    data
    : ld_v__str_r               type  i
    , ld_v__end_r               type  i
    , ld_v__str_c               type  i
    , ld_v__end_c               type  i
    , ld_t__value               type standard table of string
    , ld_ht__comp               type ty_ht__comp "
    , lr_s__data                type ref to data
    , ld_v__cnt                 type i
    , ld_v__tabix               type i
    , ld_v__qcol                type string
    , ld_v__str                 type string
    .

    field-symbols
    : <ld_s__csvdata>           type string
    , <ld_t__data>              type standard table
    , <ld_s__namecomp>          type string
    , <ld_v__value>             type string
    .

    if i_v__str_r is not supplied or i_v__str_r <= 0.
      ld_v__str_r = 1.
    else.
      ld_v__str_r = i_v__str_r.
    endif.

    ld_v__end_r = i_v__end_r.

    loop at  gd_t__strdata assigning <ld_s__csvdata> from ld_v__str_r.
      ld_v__tabix = sy-tabix.

      if ld_v__str_r = sy-tabix.
        if i_f__first_header = abap_true.
          replace all  occurrences of regex `\s+` in <ld_s__csvdata> with space.
          split <ld_s__csvdata> at gd_t__delimeter into table e_t__comp.
          delete e_t__comp where table_line is initial.
          insert lines of e_t__comp into table ld_ht__comp.
          e_r__data = create_ref_table( ld_ht__comp ).
          assign e_r__data->* to <ld_t__data>.
          continue.
        else.
          ld_v__qcol = i_v__end_c - i_v__str_c + 1.

          if ld_v__qcol < 1.
            return.
          endif.

          do ld_v__qcol times.
            ld_v__str = sy-index.
            ld_v__str = `C` && ld_v__str.
            condense ld_v__str no-gaps.
            insert ld_v__str into table ld_ht__comp.
            e_t__comp = ld_ht__comp.
          enddo.
          e_r__data = create_ref_table( ld_ht__comp ).
          assign e_r__data->* to <ld_t__data>.
        endif.
      endif.

      split <ld_s__csvdata> at gd_t__delimeter into table ld_t__value.
      append initial line to <ld_t__data> reference into lr_s__data.

      ld_v__cnt = 0.

      try.
          loop at e_t__comp assigning <ld_s__namecomp>.
            add 1 to ld_v__cnt.
            assign lr_s__data->(<ld_s__namecomp>) to <ld_v__value>.
            <ld_v__value> = ld_t__value[ ld_v__cnt ].

          endloop.
        catch cx_sy_itab_line_not_found.
          continue.
      endtry.

      check ld_v__end_r > 0.

      if ld_v__tabix = ld_v__end_r.
        exit.
      endif.
    endloop.

  endmethod.
