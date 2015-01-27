  method searchfiles.

    call method search
      exporting
        i_v__dir      = i_v__dir
        i_v__regex    = i_v__regex
      importing
        e_t__filelist = e_t__filelist.

  endmethod.
