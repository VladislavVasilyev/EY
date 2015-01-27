class ZCL_BPC_CSV definition
  public
  final
  create public .

public section.

  types:
    ty_ht__comp type hashed table of string with unique default key .

  type-pools ABAP .
  type-pools ZBPCT .
  methods GET_TABLE
    importing
      !I_V__STR_R type I optional
      !I_V__END_R type I optional
      !I_V__STR_C type I optional
      !I_V__END_C type I optional
      !I_F__FIRST_HEADER type RS_BOOL default ABAP_TRUE
    exporting
      !E_R__DATA type ref to DATA
      !E_T__COMP type ZBPCT_ST__STRING .
  methods UPLOAD_FROM_BPCMGR
    importing
      !I_V__DOCNAME type UJ_DOCNAME
      !I_V__DELIMETER type C default ';'
      !I_V__APPSET_ID type UJ_APPSET_ID
    exceptions
      ACCESS_DENIED
      FILE_OPEN_ERROR
      FILE_READ_ERROR
      OTHER .
  methods UPLOAD_FROM_PC
    importing
      !I_V__FILENAME type STRING
      !I_V__DELIMETER type C default ';'
    exceptions
      ACCESS_DENIED
      FILE_OPEN_ERROR
      FILE_READ_ERROR
      OTHER .
