&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
          mgmov            PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Temp Tables ---                                       */
DEF TEMP-TABLE tt-parada
    FIELD cod-ctrab LIKE rep-parada-ctrab.cod-ctrab LABEL "teste"
    FIELD cod-parada AS CHAR //LIKE rep-parada-ctrab.cod-parada
    FIELD des-parada LIKE motiv-parada.des-parada
    FIELD log-alter-eficien LIKE motiv-parada.log-alter-eficien
    FIELD dat-inic-parada LIKE rep-parada-ctrab.dat-inic-parada
    FIELD qtd-segs-inic LIKE rep-parada-ctrab.qtd-segs-inic
    FIELD dat-fim-parada LIKE rep-parada-ctrab.dat-fim-parada
    FIELD qtd-segs-fim LIKE rep-parada-ctrab.qtd-segs-fim
    FIELD it-codigo LIKE split-operac.it-codigo
    FIELD inicio AS DATETIME
    FIELD fim AS DATETIME.

/* Local Variable Definitions ---                                       */
DEF VAR inconsistencias AS INT.
DEF VAR par AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-centros

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ctrab rep-oper-ctrab split-operac tt-parada

/* Definitions for BROWSE br-centros                                    */
&Scoped-define FIELDS-IN-QUERY-br-centros ctrab.cod-ctrab 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-centros 
&Scoped-define QUERY-STRING-br-centros FOR EACH ctrab NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-centros OPEN QUERY br-centros FOR EACH ctrab NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-centros ctrab
&Scoped-define FIRST-TABLE-IN-QUERY-br-centros ctrab


/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 split-operac.it-codigo rep-oper-ctrab.nr-ord-produ rep-oper-ctrab.qtd-operac-aprov rep-oper-ctrab.qtd-operac-refgda rep-oper-ctrab.qtd-operac-reptda DATETIME(rep-oper-ctrab.dat-inic-reporte, int(rep-oper-ctrab.qtd-segs-inic-reporte * 1000)) DATETIME(rep-oper-ctrab.dat-fim-reporte, int(rep-oper-ctrab.qtd-segs-fim-reporte * 1000))   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH rep-oper-ctrab NO-LOCK WHERE rep-oper-ctrab.cod-ctrab = "", ~
            FIRST split-operac WHERE split-operac.nr-ord-prod = rep-oper-ctrab.nr-ord-prod
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME}      FOR EACH rep-oper-ctrab NO-LOCK WHERE rep-oper-ctrab.cod-ctrab = "", ~
            FIRST split-operac WHERE split-operac.nr-ord-prod = rep-oper-ctrab.nr-ord-prod.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 rep-oper-ctrab split-operac
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 rep-oper-ctrab
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-4 split-operac


/* Definitions for BROWSE BROWSE-5                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-5 tt-parada.cod-parada tt-parada.des-parada tt-parada.inicio tt-parada.fim tt-parada.it-codigo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-5   
&Scoped-define SELF-NAME BROWSE-5
&Scoped-define QUERY-STRING-BROWSE-5 FOR EACH tt-parada NO-LOCK      WHERE tt-parada.cod-ctrab = ""
&Scoped-define OPEN-QUERY-BROWSE-5 OPEN QUERY {&SELF-NAME} FOR EACH tt-parada NO-LOCK      WHERE tt-parada.cod-ctrab = "".
&Scoped-define TABLES-IN-QUERY-BROWSE-5 tt-parada
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-5 tt-parada


/* Definitions for FRAME centros                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-centros ~
    ~{&OPEN-QUERY-br-centros}

/* Definitions for FRAME paradas                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-paradas ~
    ~{&OPEN-QUERY-BROWSE-5}

/* Definitions for FRAME producao                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-producao ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 RECT-9 RECT-10 edt-centro ~
edt-data-ini btn-pesquisar edt-data-fim 
&Scoped-Define DISPLAYED-OBJECTS edt-centro edt-data-ini edt-data-fim ~
edt-inconsistencias EDITOR-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-pesquisar 
     LABEL "BUSCAR APONTAMENTOS" 
     SIZE 31 BY 1.13
     FONT 4.

DEFINE VARIABLE EDITOR-2 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 3 BY .75
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE edt-centro AS CHARACTER FORMAT "X(256)":U 
     LABEL "Centro de Trabalho" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE edt-data-fim AS DATE FORMAT "99/99/99":U 
     LABEL "Data Final" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE edt-data-ini AS DATE FORMAT "99/99/99":U 
     LABEL "Data Inicio" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE edt-inconsistencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Total de inconsistencias" 
     VIEW-AS FILL-IN 
     SIZE 19.86 BY 1
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 134 BY 1.75.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 2.

DEFINE VARIABLE edt-motivo AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 39 BY 9.75
     BGCOLOR 15 FGCOLOR 12  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-centros FOR 
      ctrab SCROLLING.

DEFINE QUERY BROWSE-4 FOR 
      rep-oper-ctrab, 
      split-operac SCROLLING.

DEFINE QUERY BROWSE-5 FOR 
      tt-parada SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-centros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-centros C-Win _STRUCTURED
  QUERY br-centros NO-LOCK DISPLAY
      ctrab.cod-ctrab FORMAT "x(16)":U WIDTH 16.72
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 20.29 BY 15
         FONT 4 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 NO-LOCK DISPLAY
      split-operac.it-codigo WIDTH 15
rep-oper-ctrab.nr-ord-produ WIDTH 15
rep-oper-ctrab.qtd-operac-aprov FORMAT "->,>>>,>>9.9999":U WIDTH 15
rep-oper-ctrab.qtd-operac-refgda FORMAT "->,>>>,>>9.9999":U WIDTH 15
rep-oper-ctrab.qtd-operac-reptda FORMAT "->,>>>,>>9.9999":U  WIDTH 15
DATETIME(rep-oper-ctrab.dat-inic-reporte, int(rep-oper-ctrab.qtd-segs-inic-reporte * 1000)) LABEL "Inicio" WIDTH 25.5
DATETIME(rep-oper-ctrab.dat-fim-reporte, int(rep-oper-ctrab.qtd-segs-fim-reporte * 1000)) LABEL "Fim" WIDTH 25.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 132 BY 14.25
         FONT 4 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-5 C-Win _FREEFORM
  QUERY BROWSE-5 NO-LOCK DISPLAY
      tt-parada.cod-parada LABEL "Cod." WIDTH 6
tt-parada.des-parada LABEL "Descri‡Æo Parada" WIDTH 30
tt-parada.inicio LABEL "Horario Inicial" WIDTH 20
tt-parada.fim LABEL "Horario Final" WIDTH 20
tt-parada.it-codigo LABEL "Item" WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 11.25
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     edt-centro AT ROW 1.67 COL 20 COLON-ALIGNED WIDGET-ID 2
     edt-data-ini AT ROW 1.71 COL 54 COLON-ALIGNED WIDGET-ID 28
     btn-pesquisar AT ROW 1.71 COL 103.72 WIDGET-ID 14
     edt-data-fim AT ROW 1.75 COL 83 COLON-ALIGNED WIDGET-ID 10
     edt-inconsistencias AT ROW 32.88 COL 113 COLON-ALIGNED WIDGET-ID 24
     EDITOR-2 AT ROW 33.04 COL 3 NO-LABEL WIDGET-ID 20
     "Apontamentos Inconsistentes" VIEW-AS TEXT
          SIZE 30 BY .67 AT ROW 33.04 COL 7 WIDGET-ID 22
          FONT 4
     RECT-7 AT ROW 1.25 COL 2 WIDGET-ID 4
     RECT-8 AT ROW 1.25 COL 44 WIDGET-ID 8
     RECT-9 AT ROW 1.25 COL 102 WIDGET-ID 12
     RECT-10 AT ROW 32.5 COL 2 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136 BY 33.5 WIDGET-ID 100.

DEFINE FRAME producao
     BROWSE-4 AT ROW 1.25 COL 2 WIDGET-ID 400
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.5
         SIZE 134 BY 15.75
         FONT 4
         TITLE "APONTAMENTOS DE PRODU€ÇO" WIDGET-ID 200.

DEFINE FRAME paradas
     BROWSE-5 AT ROW 1.25 COL 2 WIDGET-ID 500
     edt-motivo AT ROW 2.75 COL 95 NO-LABEL WIDGET-ID 6
     "Motivo da Inconsistencia" VIEW-AS TEXT
          SIZE 29 BY 1.5 AT ROW 1.25 COL 101 WIDGET-ID 8
          FONT 0
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 19.5
         SIZE 134 BY 12.75
         FONT 4
         TITLE "PARADAS" WIDGET-ID 300.

DEFINE FRAME centros
     br-centros AT ROW 1 COL 1 WIDGET-ID 700
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 2.75
         SIZE 20.5 BY 15.1 WIDGET-ID 600.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Conferencia de Apontamento X Paradas"
         HEIGHT             = 33.5
         WIDTH              = 136
         MAX-HEIGHT         = 33.5
         MAX-WIDTH          = 180
         VIRTUAL-HEIGHT     = 33.5
         VIRTUAL-WIDTH      = 180
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME centros:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME paradas:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME producao:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME centros
                                                                        */
/* BROWSE-TAB br-centros 1 centros */
ASSIGN 
       FRAME centros:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR EDITOR EDITOR-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN edt-inconsistencias IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME paradas
                                                                        */
/* BROWSE-TAB BROWSE-5 TEXT-2 paradas */
ASSIGN 
       edt-motivo:READ-ONLY IN FRAME paradas        = TRUE.

/* SETTINGS FOR FRAME producao
                                                                        */
/* BROWSE-TAB BROWSE-4 1 producao */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-centros
/* Query rebuild information for BROWSE br-centros
     _TblList          = "mgcad.ctrab"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > mgcad.ctrab.cod-ctrab
"ctrab.cod-ctrab" ? ? "character" ? ? ? ? ? ? no ? no no "16.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-centros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
     FOR EACH rep-oper-ctrab NO-LOCK WHERE rep-oper-ctrab.cod-ctrab = "",
     FIRST split-operac WHERE split-operac.nr-ord-prod = rep-oper-ctrab.nr-ord-prod.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-5
/* Query rebuild information for BROWSE BROWSE-5
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-parada NO-LOCK
     WHERE tt-parada.cod-ctrab = "".
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-5 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Conferencia de Apontamento X Paradas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Conferencia de Apontamento X Paradas */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-centros
&Scoped-define FRAME-NAME centros
&Scoped-define SELF-NAME br-centros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-centros C-Win
ON MOUSE-SELECT-DBLCLICK OF br-centros IN FRAME centros
DO:
  edt-centro:SCREEN-VALUE IN FRAME DEFAULT-FRAME = string(br-centros:GET-BROWSE-COLUMN(1):SCREEN-VALUE IN FRAME centros).
  FRAME centros:VISIBLE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-5
&Scoped-define FRAME-NAME paradas
&Scoped-define SELF-NAME BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-5 C-Win
ON MOUSE-SELECT-CLICK OF BROWSE-5 IN FRAME paradas
DO:
  IF BROWSE-5:GET-BROWSE-COLUMN(2):SCREEN-VALUE <> ? THEN DO:

      edt-motivo:SCREEN-VALUE = "".

      IF tt-parada.log-alter-eficien = TRUE AND tt-parada.it-codigo = "" THEN DO:
          edt-motivo:SCREEN-VALUE = 'HORARIO DE PARADA FORA DO HORARIO PRODUZINDO: Toda parada que "ALTERA" a eficiencia deve estar dentro de horarios de produ‡Æo'.
      END.
    
      IF tt-parada.log-alter-eficien = FALSE AND tt-parada.it-codigo <> "" THEN DO:
          edt-motivo:SCREEN-VALUE = 'HORARIO DE PARADA DENTRO DO HORARIO PRODUZINDO: NÆo pode existir apontamentos de produ‡Æo no mesmo horario de paradas que "NÇO ALTERAM" a eficiencia'.
      END.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-5 C-Win
ON ROW-DISPLAY OF BROWSE-5 IN FRAME paradas
DO:
  IF tt-parada.log-alter-eficien = TRUE AND tt-parada.it-codigo = "" THEN DO:
      tt-parada.cod-parada:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.des-parada:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.inicio:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.fim:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.it-codigo:BGCOLOR IN BROWSE BROWSE-5 = 14.
     ASSIGN inconsistencias = inconsistencias + 1.
  END.

  IF tt-parada.log-alter-eficien = FALSE AND tt-parada.it-codigo <> "" THEN DO:
      tt-parada.cod-parada:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.des-parada:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.inicio:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.fim:BGCOLOR IN BROWSE BROWSE-5 = 14.
      tt-parada.it-codigo:BGCOLOR IN BROWSE BROWSE-5 = 14.
     ASSIGN inconsistencias = inconsistencias + 1.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btn-pesquisar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-pesquisar C-Win
ON CHOOSE OF btn-pesquisar IN FRAME DEFAULT-FRAME /* BUSCAR APONTAMENTOS */
DO:
    //VALIDA CAMPOS
    //Verifica se todos os campos estao preenchidos
    IF edt-centro:SCREEN-VALUE = "" OR edt-data-ini:SCREEN-VALUE = "" OR edt-data-fim:SCREEN-VALUE = "" THEN
        MESSAGE "Por favor preencha todos os campos." VIEW-AS ALERT-BOX.
    ELSE IF edt-data-ini:SCREEN-VALUE > edt-data-fim:SCREEN-VALUE THEN
        MESSAGE "Data inicial maior que data final."  VIEW-AS ALERT-BOX.
    ELSE DO:

        FIND FIRST tt-parada NO-ERROR.

        IF AVAIL tt-parada THEN DO:
           EMPTY TEMP-TABLE tt-parada.
        END.

        
        FOR EACH rep-parada-ctrab NO-LOCK
           WHERE rep-parada-ctrab.cod-ctrab = edt-centro:SCREEN-VALUE
             AND rep-parada-ctrab.dat-inic-parada >= date(edt-data-ini:SCREEN-VALUE)
             AND rep-parada-ctrab.dat-fim-parada <= date(edt-data-fim:SCREEN-VALUE),

             FIRST motiv-parada WHERE motiv-parada.cod-parada = rep-parada-ctrab.cod-parada.

             CREATE tt-parada.
             ASSIGN tt-parada.cod-ctrab = string(rep-parada-ctrab.cod-ctrab)
                    tt-parada.cod-parada = rep-parada-ctrab.cod-parada
                    tt-parada.des-parada = motiv-parada.des-parada
                    tt-parada.log-alter-eficien = motiv-parada.log-alter-eficien
                    tt-parada.inicio = DATETIME(rep-parada-ctrab.dat-inic-parada, INT(rep-parada-ctrab.qtd-segs-inic * 1000))
                    tt-parada.fim = DATETIME(rep-parada-ctrab.dat-fim-parada, INT(rep-parada-ctrab.qtd-segs-fim * 1000))
                    tt-parada.dat-inic-parada = rep-parada-ctrab.dat-inic-parada
                    tt-parada.qtd-segs-inic = rep-parada-ctrab.qtd-segs-inic
                    tt-parada.dat-fim-parada = rep-parada-ctrab.dat-fim-parada
                    tt-parada.qtd-segs-fim = rep-parada-ctrab.qtd-segs-fim.

             FOR FIRST rep-oper-ctrab 
                 WHERE rep-oper-ctrab.cod-ctrab = rep-parada-ctrab.cod-ctrab
                   AND rep-oper-ctrab.dat-inic-reporte = rep-parada-ctrab.dat-inic-parada
                   AND rep-oper-ctrab.qtd-segs-inic-reporte <= rep-parada-ctrab.qtd-segs-inic
                   AND rep-oper-ctrab.qtd-segs-fim-reporte >= rep-parada-ctrab.qtd-segs-fim,
    
                 FIRST split-operac WHERE split-operac.nr-ord-prod = rep-oper-ctrab.nr-ord-prod.
                 
                 ASSIGN tt-parada.it-codigo = split-operac.it-codigo.

             END.
        END.

        //BUSCA APONTAMENTO PRODU€ÇO
        OPEN QUERY BROWSE-4 FOR EACH rep-oper-ctrab NO-LOCK
             WHERE rep-oper-ctrab.cod-ctrab = edt-centro:SCREEN-VALUE
               AND rep-oper-ctrab.dat-inic-report >= date(edt-data-ini:SCREEN-VALUE)
               AND rep-oper-ctrab.dat-fim-report <= date(edt-data-fim:SCREEN-VALUE),

             FIRST split-operac WHERE split-operac.nr-ord-prod = rep-oper-ctrab.nr-ord-prod.


      OPEN QUERY BROWSE-5
            FOR EACH tt-parada NO-LOCK
             WHERE tt-parada.cod-ctrab = edt-centro:SCREEN-VALUE
               AND tt-parada.dat-inic-parada >= date(edt-data-ini:SCREEN-VALUE)
               AND tt-parada.dat-fim-parada <= date(edt-data-fim:SCREEN-VALUE).
    END.

    edt-inconsistencias:SCREEN-VALUE = string(inconsistencias).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edt-centro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edt-centro C-Win
ON MOUSE-SELECT-DBLCLICK OF edt-centro IN FRAME DEFAULT-FRAME /* Centro de Trabalho */
DO:
  IF FRAME centros:VISIBLE = TRUE THEN
      FRAME centros:VISIBLE = FALSE.
  ELSE
      FRAME centros:VISIBLE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-centros
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

edt-centro:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME {&FRAME-NAME}.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

  FRAME centros:VISIBLE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY edt-centro edt-data-ini edt-data-fim edt-inconsistencias EDITOR-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 RECT-9 RECT-10 edt-centro edt-data-ini btn-pesquisar 
         edt-data-fim 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE br-centros 
      WITH FRAME centros IN WINDOW C-Win.
  VIEW FRAME centros IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-centros}
  ENABLE BROWSE-4 
      WITH FRAME producao IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-producao}
  DISPLAY edt-motivo 
      WITH FRAME paradas IN WINDOW C-Win.
  ENABLE BROWSE-5 edt-motivo 
      WITH FRAME paradas IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-paradas}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

