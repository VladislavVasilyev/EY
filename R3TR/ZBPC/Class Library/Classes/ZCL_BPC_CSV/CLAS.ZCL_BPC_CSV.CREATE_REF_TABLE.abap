  method create_ref_table.

    data
    : ld_t__comp                type abap_component_tab
    , ld_s__comp                type abap_componentdescr
    , lr_o__structdescr         type ref to cl_abap_structdescr
    , lr_o__abap_table          type ref to cl_abap_tabledescr
    .

    field-symbols
    : <ld_v__comp>              type string
    .

    ld_s__comp-type ?= cl_abap_datadescr=>describe_by_name( `STRING` ).

    loop at i_t__comp assigning <ld_v__comp>.
      ld_s__comp-name = <ld_v__comp>.
      append ld_s__comp to ld_t__comp.
    endloop.


    lr_o__structdescr ?= cl_abap_structdescr=>create( p_components = ld_t__comp p_strict = abap_false ).

    lr_o__abap_table  ?= cl_abap_tabledescr=>create( p_line_type  = lr_o__structdescr
                                                     p_unique     = abap_false
                                                     p_key_kind   = cl_abap_tabledescr=>keydefkind_default
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std   ).

    create data e_r__table  type handle lr_o__abap_table.

  endmethod.
