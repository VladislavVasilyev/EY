  method SEARCH.

  data
  : file_table type standard table of sdokpath
  , dir_table  type standard table of sdokpath
  , file       type string
  , dirchar        type c length 255
  , dirstring  type string
  , filename   type string
  , ld_t__filelist type ty_t__filelist
  , ld_s__filelist type line of ty_t__filelist
  , ld_v__regex type string
  .

  dirchar = i_v__dir.

  call function 'TMP_GUI_DIRECTORY_LIST_FILES'
     exporting
       directory        = dirchar
       filter           = '*.*'
*     IMPORTING
*       FILE_COUNT        =
*       DIR_COUNT         =
     tables
       file_table       = file_table
       dir_table        = dir_table.
*     EXCEPTIONS
*       CNTL_ERROR       = 1
*       OTHERS           = 2.

  loop at file_table into file.
    if i_v__regex is initial.
      ld_v__regex = `\A`.
    else.
      ld_v__regex = i_v__regex.
    endif.

    find regex ld_v__regex in file ignoring case.
    if sy-subrc = 0.
      ld_s__filelist-path = i_v__dir.
      ld_s__filelist-name = file.

      find regex `\.([[:alnum:]]+)$` in file submatches ld_s__filelist-type.
      translate ld_s__filelist-type to upper case.

      concatenate `.` ld_s__filelist-type `$` into ld_v__regex.
      replace regex ld_v__regex in ld_s__filelist-name with `` ignoring case.

      insert ld_s__filelist into table e_t__filelist.
    endif.
  endloop.

  loop at dir_table into dirchar.
    refresh ld_t__filelist.
    concatenate i_v__dir dirchar `\` into dirchar.
    dirstring = dirchar.
    call method search
      exporting
        i_v__dir      = dirstring
        i_v__regex    = i_v__regex
      importing
        e_t__filelist = ld_t__filelist.

    insert lines of ld_t__filelist into table e_t__filelist.
  endloop.

  endmethod.
