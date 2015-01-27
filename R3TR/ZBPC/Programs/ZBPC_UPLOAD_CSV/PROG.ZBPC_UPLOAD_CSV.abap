****************************************************************
* ABAP Name:      ZBPC_UPLOAD_CSV
* Created by:     Vladislav Vasilyev (EY)
* Created on:     04.12.2014 14:28:25
* Version:        1.0
* Change request: BADK909195
*---------------------------------------------------------------
* Comments:
* Обработка файла CSV и запись в его прозрачные таблицы
* BPC_TUFS_DATA или BPC_TCOR_DATA согласно выбору пользователя.
*
*---------------------------------------------------------------
* Changes History:
* Date        Programmer              Correction    Marker    Description
*---------------------------------------------------------------
REPORT ZBPC_UPLOAD_CSV.

DEFINE STATUS_BAR>.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING TEXT        = &2
            PERCENTAGE  = &1.
END-OF-DEFINITION.
*--------------------------------------------------------------------*

CONSTANTS
  : CS_TUFS TYPE STRING VALUE `ZBPC_TUFS_DATA`
  , CS_TCOR TYPE STRING VALUE `ZBPC_TCOR_DATA`.
*--------------------------------------------------------------------*

DATA
: GD_T__ENTITY            TYPE ZCL_BPC_STATE_MANAGER=>TT_ENTITY
, GD_V__PERIOD            TYPE /BI0/OICALMONTH
, GD_V__NUMLINE           TYPE STRING
, GD_V__TIME_STAMP        TYPE TIMESTAMPL
, GD_V__TIMESTMNUM        TYPE TZNTSTMPLL
, GD_V__PATH              TYPE STRING
, GD_V__FILE              TYPE STRING
, GD_V__MODE              TYPE STRING
, GD_V__RCODE             TYPE /BIC/OIZBPCCHCOD
, GD_T__ZBPC_TUFS_DATA    TYPE STANDARD TABLE OF ZBPC_TUFS_DATA WITH EMPTY KEY
, GD_T__ZBPC_TCOR_DATA    TYPE STANDARD TABLE OF ZBPC_TCOR_DATA WITH EMPTY KEY
.

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE T_BO1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS DOWN1 RADIOBUTTON GROUP R01 DEFAULT 'X' USER-COMMAND UC MODIF ID A01.
SELECTION-SCREEN COMMENT (50) TDOWN1 FOR FIELD DOWN1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS DOWN2 RADIOBUTTON GROUP R01 MODIF ID A01.
SELECTION-SCREEN COMMENT (50) TDOWN2 FOR FIELD DOWN2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS ONEFILE RADIOBUTTON GROUP R02 DEFAULT 'X' USER-COMMAND DC MODIF ID A03.
SELECTION-SCREEN COMMENT (32) TONEFILE FOR FIELD ONEFILE.
PARAMETERS MOREFILE RADIOBUTTON GROUP R02 MODIF ID A03.
SELECTION-SCREEN COMMENT (32) TMOREFLE FOR FIELD MOREFILE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (32) TFILE FOR FIELD DOWN2.
PARAMETERS: FILE TYPE STRING LOWER CASE VISIBLE LENGTH 70  MODIF ID 002.
SELECTION-SCREEN END OF LINE.
PARAMETERS MODE TYPE STRING MODIF ID A02.
SELECTION-SCREEN END OF BLOCK B01.
*--------------------------------------------------------------------*

INITIALIZATION.
  MOVE
  : `Укажите режим загрузки и выберите файл`          TO T_BO1
  , `Загрузка УФС`                                    TO TDOWN1
  , `Загрузка Ручных Трансформационных Корректировок` TO TDOWN2
  , `Загрузить один файл`                             TO TONEFILE
  , `Загрузить несколько файлов`                      TO TMOREFLE
  .

  FILE = ZCL_PATH_UTILITES=>GET_INITIAL_DIRECTORY( ).

AT SELECTION-SCREEN OUTPUT.
  TFILE   = COND STRING( WHEN ONEFILE  = ABAP_TRUE THEN `Файл`
                         WHEN MOREFILE = ABAP_TRUE THEN `Директория` ).


  IF MODE =  `UFS` OR MODE = `TRM`.
*    onefile = abap_true.
*    morefile = abap_false.
  ENDIF.

  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.
      WHEN `A01`.
        IF MODE =  `UFS` OR MODE = `TRM`.
          SCREEN-INPUT = 0.
        ENDIF.
      WHEN `A02`.
        SCREEN-ACTIVE = 0.
      WHEN `A03`.
        IF MODE =  `UFS` OR MODE = `TRM`.
*          screen-active = 0.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE.

  FILE = COND STRING( WHEN ONEFILE  = ABAP_TRUE THEN ZCL_PATH_UTILITES=>FILE_F4( )
                      WHEN MOREFILE = ABAP_TRUE THEN ZCL_PATH_UTILITES=>DIRECTORY_F4( ) ).

*--------------------------------------------------------------------*
* start of slection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  GET TIME STAMP FIELD GD_V__TIME_STAMP.
  UNPACK GD_V__TIME_STAMP TO GD_V__TIMESTMNUM.

  CASE ABAP_TRUE.
    WHEN DOWN1. GD_V__MODE = CS_TUFS.
    WHEN DOWN2. GD_V__MODE = CS_TCOR.
  ENDCASE.

  CASE ABAP_TRUE.
    WHEN ONEFILE.
      IF ZCL_PATH_UTILITES=>CHECK_DIRECTORY( DIRECTORY = FILE EXT = `(csv|txt)` ) NE ABAP_TRUE.
        MESSAGE `Не верно указан путь к файлу (файл должен иметь расширение CSV или TXT)` TYPE `E`.
      ENDIF.

      PERFORM DOWN       USING FILE FILE.
      PERFORM LOADTOCUBE CHANGING GD_V__RCODE.

    WHEN MOREFILE.
      DATA(LD_T__FILES) = ZCL_PATH_UTILITES=>SEARCHFILES( I_V__DIR = FILE I_V__REGEX = `(CSV|TXT)` ).

      ZCL_PATH_UTILITES=>PATH_CHOOSE( EXPORTING I_V__FORM_NAME = `CHOOSE` CHANGING I_T__PATHLIST = LD_T__FILES ).

      LOOP AT LD_T__FILES ASSIGNING FIELD-SYMBOL(<LD_S__FILES>).
        GD_V__PATH = <LD_S__FILES>-PATH && `\` && <LD_S__FILES>-NAME && `.` && <LD_S__FILES>-TYPE.
        GD_V__FILE = <LD_S__FILES>-NAME && `.` && <LD_S__FILES>-TYPE.
        PERFORM DOWN  USING GD_V__PATH GD_V__FILE .
      ENDLOOP.

      PERFORM LOADTOCUBE CHANGING GD_V__RCODE.

  ENDCASE.

END-OF-SELECTION.
*--------------------------------------------------------------------*




*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
FORM DOWN USING PATH TYPE STRING
                FILE TYPE STRING.

  DATA
  : LR_T__CSV_DATA          TYPE REF TO DATA
  , LD_V__LOW               TYPE STRING
  , LD_F__CHECKVERS         TYPE RS_BOOL
  , LD_F__CHECKPER          TYPE RS_BOOL
  , LD_F__CHECKPF           TYPE RS_BOOL
  , LD_F__CHECKBU           TYPE RS_BOOL

  , LR_O__AUTH              TYPE REF TO ZCL_BPC_CHECK_AUTH

  , LD_S__ZBPC_TUFS_DATA    LIKE LINE OF GD_T__ZBPC_TUFS_DATA
  , LD_S__ZBPC_TUFS_SDATA   LIKE LINE OF GD_T__ZBPC_TUFS_DATA
  , LD_S__ZBPC_TCOR_DATA    LIKE LINE OF GD_T__ZBPC_TCOR_DATA
  , LD_S__ZBPC_TCOR_SDATA   LIKE LINE OF GD_T__ZBPC_TCOR_DATA
*  , lr_o__state             type ref to zcl_bpc_state_manager
  , LD_V__VERSTYPE          TYPE STRING
  , LD_V__BU                TYPE RSCHAVL
  , LD_F__CHECKUSER         TYPE RS_BOOL
*  , lr_i__iobj              type ref to zif_bpc_prc_md
  , LD_V__POPUPMESS         TYPE STRING
  , LD_T__POPUPMESS         TYPE STRING
  , LR_X__ROOT              TYPE REF TO CX_ROOT
  .

  FIELD-SYMBOLS
  : <LD_T__SCV_DATA>        TYPE STANDARD TABLE
  , <LD_S__SCV_DATA>        TYPE ANY
  , <LD_S__RESDATA>         TYPE ANY
  .

  DATA(LR_O__CSV) = NEW ZCL_BPC_CSV(  ).

  LR_O__CSV->UPLOAD_FROM_PC( EXPORTING I_V__FILENAME = PATH EXCEPTIONS OTHERS = 1 ).

  IF SY-SUBRC NE 0.
    MESSAGE `не удалось произвести чтние из файла ` && FILE TYPE 'E'.
  ENDIF.

  CASE GD_V__MODE.
    WHEN CS_TUFS.
      UNPACK GD_V__TIME_STAMP TO LD_S__ZBPC_TUFS_DATA-TIMESTMP.
      LD_S__ZBPC_TUFS_DATA-USER_NAME = SY-UNAME.
    WHEN CS_TCOR.
      UNPACK GD_V__TIME_STAMP TO LD_S__ZBPC_TCOR_DATA-TIMESTMP.
      LD_S__ZBPC_TCOR_DATA-USER_NAME = SY-UNAME.
  ENDCASE.

  " чтение таблицы
  STATUS_BAR> 5 `Чтение заголовка файла`.

  CALL METHOD LR_O__CSV->GET_TABLE
    EXPORTING
      I_V__STR_R = 1
      I_V__END_R = 2
    IMPORTING
      E_R__DATA  = LR_T__CSV_DATA.

  ASSIGN LR_T__CSV_DATA->* TO <LD_T__SCV_DATA>.

  CASE GD_V__MODE.
    WHEN CS_TUFS.
      LD_V__VERSTYPE = `VERSIONUFS`.
      ASSIGN
      : LD_S__ZBPC_TUFS_DATA-ZBPCCHPF   TO FIELD-SYMBOL(<LD_V__ZBPCCHPF>)
      , LD_S__ZBPC_TUFS_DATA-ZBPCCHPER  TO FIELD-SYMBOL(<LD_V__ZBPCCHPER>)
      , LD_S__ZBPC_TUFS_DATA-ZBPCCHBU   TO FIELD-SYMBOL(<LD_V__ZBPCCHBU>)
      , LD_S__ZBPC_TUFS_DATA            TO <LD_S__RESDATA>
      .
    WHEN CS_TCOR.
      LD_V__VERSTYPE = `VERSIONCOR`.
      ASSIGN
      : LD_S__ZBPC_TCOR_DATA-ZBPCCHPF   TO <LD_V__ZBPCCHPF>
      , LD_S__ZBPC_TCOR_DATA-ZBPCCHPER  TO <LD_V__ZBPCCHPER>
      , LD_S__ZBPC_TCOR_DATA-ZBPCCHBU   TO <LD_V__ZBPCCHBU>
      , LD_S__ZBPC_TCOR_DATA            TO <LD_S__RESDATA>
      .
  ENDCASE.

*--------------------------------------------------------------------*
* Обработка заголовка
*--------------------------------------------------------------------*

  MOVE-CORRESPONDING <LD_T__SCV_DATA>[ 1 ] TO <LD_S__RESDATA>.

  ASSIGN COMPONENT `FILE_VERSION` OF STRUCTURE <LD_T__SCV_DATA>[ 1 ] TO FIELD-SYMBOL(<LD_V__VC1>).
  IF SY-SUBRC = 0.

    SELECT SINGLE LOW
       FROM TVARVC
       INTO LD_V__LOW
       WHERE   TYPE = `P`
         AND  NAME = LD_V__VERSTYPE
    AND  LOW  = <LD_V__VC1>.

    IF SY-SUBRC <> 0.
      LD_F__CHECKVERS = ABAP_FALSE.
    ELSE.
      LD_F__CHECKVERS = ABAP_TRUE.
    ENDIF.
  ENDIF.

  ASSIGN COMPONENT `ZBPCCHBU` OF STRUCTURE <LD_T__SCV_DATA>[ 1 ] TO <LD_V__VC1>.
  IF <LD_V__VC1> IS NOT INITIAL.
    LD_F__CHECKBU  = ABAP_TRUE.
    LD_V__BU = <LD_V__VC1>.

    " проверка полномочий
    CREATE OBJECT LR_O__AUTH
      EXPORTING
        I_DIM     = 'ZBPCCHBU'
        I_KEYDATE = SY-DATUM.

    IF LR_O__AUTH->CHECK_MEMBER( LD_V__BU ) NE ABAP_TRUE.
      MESSAGE `Нет полномочий на элемент ` && LR_O__AUTH->A_ERR_MEMBER TYPE 'E'.
    ENDIF.

    LD_V__POPUPMESS = ZCL_BPC_IOBJ=>READ_MASTER_DATA( `ZBPCCHBU` )->GET_TEXT( LD_V__BU )  && `(` && <LD_V__VC1> && `)`.

    PERFORM FILLSTRING USING `Компания группы` LD_V__POPUPMESS CHANGING LD_V__POPUPMESS.
    LD_T__POPUPMESS = LD_T__POPUPMESS && LD_V__POPUPMESS.

  ENDIF.

  ASSIGN COMPONENT `ZBPCCHPER` OF STRUCTURE <LD_T__SCV_DATA>[ 1 ] TO <LD_V__VC1>.
  IF <LD_V__VC1> IS NOT INITIAL.
    LD_F__CHECKPER = ABAP_TRUE.

    LD_V__POPUPMESS = `"` && <LD_V__VC1> && `"`.
    PERFORM FILLSTRING USING `Период` LD_V__POPUPMESS CHANGING LD_V__POPUPMESS.
    LD_T__POPUPMESS = LD_T__POPUPMESS && LD_V__POPUPMESS.

  ENDIF.

  ASSIGN COMPONENT `ZBPCCHPF` OF STRUCTURE <LD_T__SCV_DATA>[ 1 ] TO <LD_V__VC1>.
  IF <LD_V__VC1> IS NOT INITIAL.
    LD_F__CHECKPF  = ABAP_TRUE.

    LD_V__POPUPMESS = `"` && <LD_V__VC1> && `"`.
    PERFORM FILLSTRING USING `Категория` LD_V__POPUPMESS CHANGING LD_V__POPUPMESS.
    LD_T__POPUPMESS = LD_T__POPUPMESS && LD_V__POPUPMESS.

  ENDIF.

  IF LD_F__CHECKPER = ABAP_FALSE.
    MESSAGE `Файл не содержит поля ZBPCCHPER (период)` TYPE `E`.
  ENDIF.

  IF LD_F__CHECKPF = ABAP_FALSE.
    MESSAGE `Файл не содержит поля ZBPCCHPF (план / факт)` TYPE `E`.
  ENDIF.

  IF LD_F__CHECKBU = ABAP_FALSE.
    MESSAGE `Файл не содержит поля ZBPCCHBU (компания группы)` TYPE `E`.
  ENDIF.

  IF LD_F__CHECKVERS = ABAP_FALSE.
    CASE MODE.
      WHEN CS_TUFS.
        MESSAGE `Версия файла УФС не совпадает с контрольной версией` TYPE `E`.
      WHEN CS_TCOR.
        MESSAGE `Версия файла Ручных трансформационных корректировок не совпадает с контрольной версией` TYPE `E`.
    ENDCASE.
  ENDIF.


*--------------------------------------------------------------------*
* POPUP
*--------------------------------------------------------------------*
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Выберите действие'
*     DIAGNOSE_OBJECT       = ' '
      TEXT_QUESTION         = LD_T__POPUPMESS
      TEXT_BUTTON_1         = 'Загрузить'(001)
      ICON_BUTTON_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = 'Отменить'(002)
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DEFAULT_BUTTON        = '2'
      DISPLAY_CANCEL_BUTTON = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 28
*     START_ROW             = 1
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      ANSWER                = LD_F__CHECKUSER
*    tables
*     parameter             = ld_t__popupparam
* EXCEPTIONS
*     TEXT_NOT_FOUND        = 1
*     OTHERS                = 2.
    .

  CASE LD_F__CHECKUSER.
    WHEN 2.
      MESSAGE `Операция отменена пользователем` TYPE `S`.
      RETURN.
  ENDCASE.
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Заполнение таблицы
*--------------------------------------------------------------------*
  STATUS_BAR> 10 `Чтение основных данных`.
  CALL METHOD LR_O__CSV->GET_TABLE
    EXPORTING
      I_V__STR_R = 3
    IMPORTING
      E_R__DATA  = LR_T__CSV_DATA.
*      e_t__comp  = ld_t__comp.

  ASSIGN LR_T__CSV_DATA->* TO <LD_T__SCV_DATA>.


  STATUS_BAR> 25 `Копирование основных данных`.
  LOOP AT <LD_T__SCV_DATA> ASSIGNING <LD_S__SCV_DATA>." reference into lr_s__scv_data.

    TRY.
        CASE GD_V__MODE.
          WHEN CS_TUFS.
            MOVE-CORRESPONDING
            : LD_S__ZBPC_TUFS_DATA TO LD_S__ZBPC_TUFS_SDATA
            , <LD_S__SCV_DATA>     TO LD_S__ZBPC_TUFS_SDATA
            .

            APPEND LD_S__ZBPC_TUFS_SDATA TO GD_T__ZBPC_TUFS_DATA.
          WHEN CS_TCOR.
            MOVE-CORRESPONDING
            : LD_S__ZBPC_TCOR_DATA TO LD_S__ZBPC_TCOR_SDATA
            , <LD_S__SCV_DATA>     TO LD_S__ZBPC_TCOR_SDATA
            .

            APPEND LD_S__ZBPC_TCOR_SDATA TO GD_T__ZBPC_TCOR_DATA.
        ENDCASE.
      CATCH CX_ROOT INTO LR_X__ROOT.
        PERFORM CLEAR_TABLE.

*        case gd_v__mode.
*          when cs_tufs.
*            write: / ld_s__zbpc_tufs_sdata-zbpcchsht, `|`, ld_s__zbpc_tufs_sdata-numrow, `|`, ld_s__zbpc_tufs_sdata-numcol.
*          when cs_tcor.
*            write: / ld_s__zbpc_tcor_sdata-sheet, `|`, ld_s__zbpc_tcor_sdata-numrow, `|`, ld_s__zbpc_tcor_sdata-numcol.
*        endcase.

        MESSAGE `Ошибка при копировании данных файла: ` && FILE && `.    ` && LR_X__ROOT->GET_TEXT( ) TYPE `E`.
    ENDTRY.
  ENDLOOP.

  STATUS_BAR> 50 `Сохранение данных в прозрачную таблицу`.
  DATA(LD_V__LINES) = SWITCH I( GD_V__MODE
                         WHEN CS_TUFS THEN LINES( GD_T__ZBPC_TUFS_DATA )
                         WHEN CS_TCOR THEN LINES( GD_T__ZBPC_TCOR_DATA ) ).

  GD_V__PERIOD = SWITCH /BI0/OICALMONTH( GD_V__MODE
                         WHEN CS_TUFS THEN LD_S__ZBPC_TUFS_DATA-ZBPCCHPER
                         WHEN CS_TCOR THEN LD_S__ZBPC_TCOR_DATA-ZBPCCHPER ).

  APPEND SWITCH /BIC/OIZBPCCHBU( GD_V__MODE
                          WHEN CS_TUFS THEN LD_S__ZBPC_TUFS_DATA-ZBPCCHBU
                          WHEN CS_TCOR THEN LD_S__ZBPC_TCOR_DATA-ZBPCCHBU )
                          TO GD_T__ENTITY.

  TRY.
      CASE GD_V__MODE.
        WHEN CS_TUFS.
          INSERT ZBPC_TUFS_DATA FROM TABLE GD_T__ZBPC_TUFS_DATA.
        WHEN CS_TCOR.
          INSERT ZBPC_TCOR_DATA FROM TABLE GD_T__ZBPC_TCOR_DATA.
      ENDCASE.

    CATCH CX_ROOT INTO LR_X__ROOT.
      PERFORM CLEAR_TABLE.

      MESSAGE `Ошибка при загрузке файла: ` && FILE && `.    ` && LR_X__ROOT->GET_TEXT( ) TYPE `E`.
  ENDTRY.
*--------------------------------------------------------------------*

  ADD LD_V__LINES TO GD_V__NUMLINE.

  FREE
  : GD_T__ZBPC_TUFS_DATA
  , GD_T__ZBPC_TCOR_DATA
  .

ENDFORM.

*--------------------------------------------------------------------*
* loadtocube
*--------------------------------------------------------------------*
FORM LOADTOCUBE CHANGING R_CODE TYPE /BIC/OIZBPCCHCOD.

  DATA(LR_O__STATE) = NEW ZCL_BPC_STATE_MANAGER( ).

  STATUS_BAR> 85 `Прогрузка данных до кубов`.
  R_CODE = LR_O__STATE->RUN_LOAD( I_SRCSYS  = SWITCH /BIC/OIZBPCCHSYS( GD_V__MODE WHEN CS_TUFS THEN 'UFS' WHEN CS_TCOR THEN 'TRM')
                                  IT_ENTITY = GD_T__ENTITY
                                  I_PERIOD  = GD_V__PERIOD ).

  IF R_CODE IS NOT INITIAL.
    CALL FUNCTION 'DB_ROLLBACK'.
  ELSE.
    MESSAGE `Сохранено строк: ` &&  GD_V__NUMLINE TYPE `S`.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------*
* fillstring
*--------------------------------------------------------------------*
FORM FILLSTRING USING NAME TYPE STRING VALUE TYPE STRING CHANGING STR TYPE STRING.
  DATA
  : LD_V__TIMES   TYPE I
  , LD_V__FILL    TYPE STRING VALUE `....................................................`
  , LD_V__STR     TYPE STRING
  .

  LD_V__TIMES = 48 - STRLEN( NAME  ) - STRLEN( VALUE && `. ` ).

  IF LD_V__TIMES > 0.
    STR = NAME && LD_V__FILL(LD_V__TIMES) && VALUE && `. `.

  ELSE.
    LD_V__TIMES = 48 - STRLEN( VALUE && `. ` ).
    LD_V__STR = LD_V__FILL(LD_V__TIMES) && VALUE && `. `.

    STR = NAME && LD_V__FILL(47) && ` `.
    STR = STR(48) && ` ` && LD_V__STR.

  ENDIF.

ENDFORM.

*--------------------------------------------------------------------*
* Clear Table
*--------------------------------------------------------------------*

FORM CLEAR_TABLE .
  CALL FUNCTION 'DB_ROLLBACK'.
  CASE GD_V__MODE." проверка не прошло ли скрытых коммитов
    WHEN CS_TUFS.
      SELECT * FROM ZBPC_TUFS_DATA
        INTO CORRESPONDING FIELDS OF TABLE GD_T__ZBPC_TUFS_DATA
        WHERE TIMESTMP = GD_V__TIMESTMNUM.
      IF SY-SUBRC = 0.
        DELETE ZBPC_TUFS_DATA FROM TABLE GD_T__ZBPC_TUFS_DATA.
      ENDIF.

    WHEN CS_TCOR.
      SELECT * FROM ZBPC_TCOR_DATA
         INTO CORRESPONDING FIELDS OF TABLE GD_T__ZBPC_TCOR_DATA
         WHERE TIMESTMP = GD_V__TIMESTMNUM.
      IF SY-SUBRC = 0.
        DELETE ZBPC_TCOR_DATA FROM TABLE GD_T__ZBPC_TCOR_DATA.
      ENDIF.
  ENDCASE.
ENDFORM.


*--------------------------------------------------------------------*
* choose
*--------------------------------------------------------------------*
##CALLED FORM CHOOSE TABLES SELECTION STRUCTURE SHVALUE.

  ZCL_PATH_UTILITES=>CD_F__POPUP = ABAP_TRUE.

ENDFORM.
