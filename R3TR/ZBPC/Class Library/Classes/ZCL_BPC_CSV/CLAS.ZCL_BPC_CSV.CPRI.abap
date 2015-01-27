private section.

  data:
    gd_t__strdata     type table of string .
  data GD_T__DELIMETER type C .

  methods CREATE_REF_TABLE
    importing
      !I_T__COMP type TY_HT__COMP
    returning
      value(E_R__TABLE) type ref to DATA .
