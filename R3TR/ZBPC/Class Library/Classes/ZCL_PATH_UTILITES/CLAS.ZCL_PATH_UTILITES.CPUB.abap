class zcl_path_utilites definition
  public
  final
  create public .

  public section.
    type-pools abap .

    types:
      begin of ty_s__filelist
          , path type string
          , name type string
          , type type string
        , end of ty_s__filelist .
    types:
      ty_t__filelist type sorted table of ty_s__filelist with non-unique key path name .

    types:
      begin of ty_s__choose_file.
            include type zbpc_st_file.
    types: checkbox(1) type c
    , index(4) type c
    , end of ty_s__choose_file.

    class-data cd_f__popup type rs_bool.

    class-methods files_choose .
    class-methods path_choose
      importing
        !i_v__form_name type form_name
      changing
        !i_t__pathlist  type ty_t__filelist .
    class-methods searchfiles
      importing
        !i_v__dir            type string default 'C:\TEMP\'
        !i_v__regex          type string default ''
      returning
        value(e_t__filelist) type ty_t__filelist .
    class-methods check_directory
      importing
        !directory   type string
        !ext         type string optional
      returning
        value(check) type rs_bool .
    class-methods get_initial_directory
      returning
        value(directory) type string .
    class-methods file_f4
      returning
        value(file) type string .
    class-methods directory_f4
      returning
        value(directory) type string .
