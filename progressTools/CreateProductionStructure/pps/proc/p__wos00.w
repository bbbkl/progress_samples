&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Dialog-Frame 
routine-level on error undo, throw.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Update Information" Dialog-Frame _INLINE
/* Actions: ? ? ? ? adm/support/proc/ds_pa_01.w */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/******************************************************************************/
/*                                      (c) 2015 proALPHA Software GmbH       */
/*                                                Auf dem Immel 8             */
/*                                                67685 Weilerbach            */
/* Project: proALPHA                                                          */
/*                                                                            */
/* Name   : p__wos00.w                                                        */
/* Product: PPS          - Produktionsplanung & -steuerung                    */
/* Module : KERN         - Kernfunktionen (alter Standard)                    */
/*                                                                            */
/* Created: 6.1g01 as of 19.03.2015/Peter Köhler                              */
/* Current: 6.1g01 as of 19.03.2015/Peter Köhler                              */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* DESCRIPTION                                                                */
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Create production structures                                               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* PARAMETERS                                                                 */
/*----------------------------------------------------------------------------*/
/* Name                      Description                                      */
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* HISTORY                                                                    */
/*----------------------------------------------------------------------------*/
/* Date     SC  Description                                                   */
/*----------------------------------------------------------------------------*/
/******************************************************************************/

create widget-pool.

/* Procedure Information -----------------------------------------------------*/

&GLOBAL-DEFINE pa-Autor            Peter Köhler
&GLOBAL-DEFINE pa-Version          6.1g01
&GLOBAL-DEFINE pa-Datum            19.03.2015
&GLOBAL-DEFINE pa-Letzter          Peter Köhler

&GLOBAL-DEFINE pa-GenVersion       UIB
&GLOBAL-DEFINE pa-ProgrammTyp      PrintDialog
&GLOBAL-DEFINE pa-Template         adm/template/dt_dlg02.w
&GLOBAL-DEFINE pa-TemplateVersion  3.01
&GLOBAL-DEFINE pa-XBasisName       p__wos00_w
&GLOBAL-DEFINE pa-NameSpace        2.10
&GLOBAL-DEFINE pa-ZielProgramm '':U

/* Parameters ----------------------------------------------------------------*/

/* Globals -------------------------------------------------------------------*/

{adm/template/incl/dt_dlg01.df}

/* SCOPEDs -------------------------------------------------------------------*/

&SCOPED-DEFINE K_StructureType_Single     1
&SCOPED-DEFINE K_StructureType_Separate   2
&SCOPED-DEFINE K_StructureType_Structural 3

&SCOPED-DEFINE K_AssemblyCreationType_None   1
&SCOPED-DEFINE K_AssemblyCreationType_Create 2
&SCOPED-DEFINE K_AssemblyCreationType_Load   3

&SCOPED-DEFINE K_ObjectCountType_WO       1
&SCOPED-DEFINE K_ObjectCountType_BOM      2
&SCOPED-DEFINE K_ObjectCountType_OnHand   3
&SCOPED-DEFINE K_ObjectCountType_Activity 4

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable goUtilClass         as stamm.base.cls.SBCPersonalUtilsSvo no-undo.
define variable giWOProcessingCount as integer                            no-undo.


/* Temp-Tables ---------------------------------------------------------------*/

define temp-table ttCreatedWOs no-undo
  field WorkOrderObj like PP_Auftrag.PP_Auftrag_Obj
  field DataEntryNo  like PP_Auftrag.Rueckmeldenr
  
  index WorkOrderObj is primary unique
    WorkOrderObj
  
  index DataEntryNo
    DataEntryNo
  . 

define temp-table ttCreatedBaseStructures no-undo
  field BaseStructurePart as character
  field BaseStrucNumber   as integer
  
  index BaseStrucNumber is primary unique
    BaseStrucNumber
  . 

/* Buffers -------------------------------------------------------------------*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS gcUsedCompany giCreateAssemblies ~
giStructureType glCreateOnHandQty giWorkOrderProcessing gdOnHandQty ~
gdStandardPrice glSingleAssemblyOHQ giWorkOrderCount giAssemblyCount ~
btnRemoveBOMStructure giStartingDENo giPurchasePartCount ~
btnCreateBOMStructure glUseSpecificProcess gcAssemblyPrefix giActivityCount ~
glSchedulePWO gcLinePrefix gcResourcePrefix glReleaseWO gcProcessPrefix ~
giPartType glDataEntryBOM gcActivityPrefix glRandomDepth gdBOMDEQty ~
giOrderMaxDepth glDEOperation gdTSTime gdTUTime gdOperationQty glLogTime ~
glUpdatescreenLog glWriteLogfile gcLogFile Btn_Start Btn_Cancel ~
btnDeleteOrders btnClearData gbResetServer 
&Scoped-Define DISPLAYED-OBJECTS gcUsedCompany giCreateAssemblies ~
giStructureType glCreateOnHandQty giWorkOrderProcessing gdOnHandQty ~
gdStandardPrice glSingleAssemblyOHQ giWorkOrderCount giAssemblyCount ~
giStartingDENo giPurchasePartCount glUseSpecificProcess gcAssemblyPrefix ~
giActivityCount glSchedulePWO gcLinePrefix gcResourcePrefix glReleaseWO ~
gcProcessPrefix giPartType glDataEntryBOM gcActivityPrefix glRandomDepth ~
gdBOMDEQty giOrderMaxDepth glDEOperation gdTSTime gdTUTime gdOperationQty ~
glLogTime glUpdatescreenLog glWriteLogfile gcLogFile gcStatusLine ~
gdCurrentCount gdMaxCount 

/* Custom List Definitions                                              */
/* pa-YearFields,pa-ResetFields,List-3,List-4,List-5,List-6             */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "InfoFields" Dialog-Frame _INLINE
/* Actions: ? ? ? ? adm/support/proc/ds_inf01.p */
/* STRUCTURED-DATA
<Build-Information>
1.02
</Build-Information>
<Constants></Constants>
<InfoFields></InfoFields> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Dynamic InfoFields" Dialog-Frame _INLINE
/* Actions: ? ? ? ? adm/support/proc/ds_viw09.p */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "FieldDefinitions" Dialog-Frame _INLINE
/* Actions: ? ? ? ? adm/support/proc/ds_dlg01.p */
/* STRUCTURED-DATA
<DATA></DATA>
<FRAME>
****/
define frame f_dummy
with side-labels width 255 stream-io.
/****
</FRAME> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pa_cGetTimeFormattedString Dialog-Frame 
FUNCTION pa_cGetTimeFormattedString returns character private
  ( pi64Time as int64 ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pa_cGetTimeSummaryString Dialog-Frame 
FUNCTION pa_cGetTimeSummaryString returns character private
  ( pcStep            as character,
    piTotalStepTime   as int64,
    piTotalTime       as int64,
    piObjectCountType as integer ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pa_iGetRandomAssemblyNo Dialog-Frame 
FUNCTION pa_iGetRandomAssemblyNo returns integer
  ( iCurrentAssemblyPosition as integer) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Abbruch":T8 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Start AUTO-GO 
     LABEL "Start":T8 
     SIZE 16 BY 1.

DEFINE BUTTON btnClearData 
     LABEL "STD löschen" 
     SIZE 16 BY 1.

DEFINE BUTTON btnCreateBOMStructure 
     LABEL "Struktur erzeugen" 
     SIZE 20 BY 1.

DEFINE BUTTON btnDeleteOrders 
     LABEL "PPA löschen" 
     SIZE 16 BY 1.

DEFINE BUTTON btnRemoveBOMStructure 
     LABEL "Struktur entfernen" 
     SIZE 20 BY 1.

DEFINE BUTTON gbResetServer 
     LABEL "Delete APS" 
     SIZE 16 BY 1.

DEFINE VARIABLE gcActivityPrefix AS CHARACTER FORMAT "X(256)":U INITIAL "TestAct" 
     LABEL "Activity Prefix":R18 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE gcAssemblyPrefix AS CHARACTER FORMAT "X(256)":U INITIAL "TestAsb" 
     LABEL "Assembly Prefix":R18 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE gcLinePrefix AS CHARACTER FORMAT "X(256)":U INITIAL "TestPPrt" 
     LABEL "Line Prefix" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE gcLogFile AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58.5 BY 1 NO-UNDO.

DEFINE VARIABLE gcProcessPrefix AS CHARACTER FORMAT "X(256)":U INITIAL "TestPrc" 
     LABEL "Process Prefix" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE gcResourcePrefix AS CHARACTER FORMAT "X(256)":U INITIAL "TestRes" 
     LABEL "Resource Prefix":R18 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE gcStatusLine AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 128 BY 1 NO-UNDO.

DEFINE VARIABLE gcUsedCompany AS CHARACTER FORMAT "X(256)":U 
     LABEL "Used Company" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE gdBOMDEQty AS DECIMAL FORMAT "->>,>>9":U INITIAL 0 
     LABEL "Buchungsmenge" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE gdCurrentCount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE gdMaxCount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE gdOnHandQty AS DECIMAL FORMAT ">>,>>9":U INITIAL 50 
     LABEL "On Hand Qty" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE gdOperationQty AS DECIMAL FORMAT "->>,>>9":U INITIAL 0 
     LABEL "Gutmenge" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE gdStandardPrice AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 1 
     LABEL "Standard Price" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE gdTSTime AS DECIMAL FORMAT "->>,>>9":U INITIAL 0 
     LABEL "TR-Zeit" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE gdTUTime AS DECIMAL FORMAT "->>,>>9":U INITIAL 0 
     LABEL "TE-Zeit" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE giActivityCount AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 1 
     LABEL "# Activities":R18 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE giAssemblyCount AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 1 
     LABEL "# Assemblies" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE giOrderMaxDepth AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 16 
     LABEL "Order Depth":R18 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE giPartType AS INTEGER FORMAT ">9":U INITIAL 5 
     LABEL "Part Type Assembly":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE giPurchasePartCount AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 1 
     LABEL "# Purchase Parts":R18 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE giStartingDENo AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Rückmeldenummer" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE giWorkOrderCount AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 10 
     LABEL "# WorkOrders" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE giCreateAssemblies AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "None", 1,
"Create Base Structures", 2,
"Load Base Structures", 3
     SIZE 23.5 BY 3 NO-UNDO.

DEFINE VARIABLE giStructureType AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Single Assembly", 1,
"Separate Assemblies", 2,
"Strucutral Assemblies", 3
     SIZE 27 BY 3 NO-UNDO.

DEFINE VARIABLE giWorkOrderProcessing AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "None", 1,
"Create Orders", 2,
"Load Orders", 3
     SIZE 20 BY 3 NO-UNDO.

DEFINE VARIABLE glCreateOnHandQty AS LOGICAL INITIAL no 
     LABEL "Create On Hand Qty":L20 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE glDataEntryBOM AS LOGICAL INITIAL no 
     LABEL "Stückliste melden" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE glDEOperation AS LOGICAL INITIAL no 
     LABEL "Aktivität melden" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE glLogTime AS LOGICAL INITIAL yes 
     LABEL "Log Time" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE glRandomDepth AS LOGICAL INITIAL no 
     LABEL "Random Depth" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE glReleaseWO AS LOGICAL INITIAL no 
     LABEL "PPA freigeben" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .79 NO-UNDO.

DEFINE VARIABLE glSchedulePWO AS LOGICAL INITIAL no 
     LABEL "PPA einplanen" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.5 BY .79 NO-UNDO.

DEFINE VARIABLE glSingleAssemblyOHQ AS LOGICAL INITIAL no 
     LABEL "Single Line Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE glUpdatescreenLog AS LOGICAL INITIAL no 
     LABEL "Update Screen Log" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE glUseSpecificProcess AS LOGICAL INITIAL no 
     LABEL "Use specific Process":L20 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE glWriteLogfile AS LOGICAL INITIAL no 
     LABEL "Write Logfile" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     gcUsedCompany AT ROW 1.5 COL 21 COLON-ALIGNED
     giCreateAssemblies AT ROW 3 COL 23 NO-LABEL
     giStructureType AT ROW 3 COL 47.5 NO-LABEL
     glCreateOnHandQty AT ROW 3 COL 92
     giWorkOrderProcessing AT ROW 3 COL 136 NO-LABEL
     gdOnHandQty AT ROW 4 COL 90 COLON-ALIGNED
     gdStandardPrice AT ROW 5 COL 90 COLON-ALIGNED
     glSingleAssemblyOHQ AT ROW 6 COL 92
     giWorkOrderCount AT ROW 6.5 COL 134 COLON-ALIGNED
     giAssemblyCount AT ROW 7 COL 21 COLON-ALIGNED
     btnRemoveBOMStructure AT ROW 7 COL 47
     giStartingDENo AT ROW 7.5 COL 134 COLON-ALIGNED
     giPurchasePartCount AT ROW 8 COL 21 COLON-ALIGNED
     btnCreateBOMStructure AT ROW 8 COL 47
     glUseSpecificProcess AT ROW 8 COL 92
     gcAssemblyPrefix AT ROW 9 COL 21 COLON-ALIGNED
     giActivityCount AT ROW 9 COL 90 COLON-ALIGNED
     glSchedulePWO AT ROW 9 COL 136
     gcLinePrefix AT ROW 10 COL 21 COLON-ALIGNED
     gcResourcePrefix AT ROW 10 COL 90 COLON-ALIGNED
     glReleaseWO AT ROW 10 COL 136
     gcProcessPrefix AT ROW 11 COL 90 COLON-ALIGNED
     giPartType AT ROW 11.5 COL 21 COLON-ALIGNED
     glDataEntryBOM AT ROW 11.5 COL 136
     gcActivityPrefix AT ROW 12 COL 90 COLON-ALIGNED
     glRandomDepth AT ROW 12.5 COL 23
     gdBOMDEQty AT ROW 12.5 COL 134 COLON-ALIGNED
     giOrderMaxDepth AT ROW 13.5 COL 21 COLON-ALIGNED
     glDEOperation AT ROW 14 COL 136
     gdTSTime AT ROW 15 COL 134 COLON-ALIGNED
     gdTUTime AT ROW 16 COL 134 COLON-ALIGNED
     gdOperationQty AT ROW 17 COL 134 COLON-ALIGNED
     glLogTime AT ROW 18.5 COL 43
     glUpdatescreenLog AT ROW 18.5 COL 57
     glWriteLogfile AT ROW 18.5 COL 78.5
     gcLogFile AT ROW 18.5 COL 91.5 COLON-ALIGNED NO-LABEL
     gcStatusLine AT ROW 20 COL 22 COLON-ALIGNED
     gdCurrentCount AT ROW 21 COL 22 COLON-ALIGNED NO-LABEL
     gdMaxCount AT ROW 21 COL 38 COLON-ALIGNED NO-LABEL
     Btn_Start AT ROW 21.5 COL 63.5
     Btn_Cancel AT ROW 21.5 COL 80
     btnDeleteOrders AT ROW 21.5 COL 103
     btnClearData AT ROW 21.5 COL 119.5
     gbResetServer AT ROW 21.5 COL 136
     "/":T1 VIEW-AS TEXT
          SIZE 1 BY 1 AT ROW 21 COL 39
     SPACE(116.00) SKIP(1.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Production Structure Generator":T60
         DEFAULT-BUTTON Btn_Start CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,DB-Fields
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{adm/method/incl/dm_dlg00.lib}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN gcStatusLine IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN gdCurrentCount IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN gdMaxCount IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON window-close OF FRAME Dialog-Frame /* Production Structure Generator */
do:
  apply "END-ERROR":U to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Start Dialog-Frame
ON choose OF Btn_Start IN FRAME Dialog-Frame /* Start */
do:
  
  /* Variables                                                                */
  /*--------------------------------------------------------------------------*/

  define variable i64ProcessCreation    as int64 no-undo.
  define variable i64CreateAssStruct    as int64 no-undo.
  define variable i64CreateOnHand       as int64 no-undo.
  define variable i64CreateWOs          as int64 no-undo.
  define variable i64ScheduleWOs        as int64 no-undo.
  define variable i64releaseWOs         as int64 no-undo.
  define variable i64DataEntryBOMLines  as int64 no-undo.
  define variable i64DataEntryOperation as int64 no-undo.
  
  /* Processing                                                               */
  /*--------------------------------------------------------------------------*/
  
  /* assign all screen values first                                           */
  
  do
    on error undo, throw:
      
    &IF DEFINED(DISPLAYED-OBJECTS) > 0 &THEN
      do with frame {&FRAME-NAME}
        on error undo, throw:
        
        assign
          {&DISPLAYED-OBJECTS}
          .
          
        catch oError as Progress.Lang.Error:
        
          (new adm.method.cls.DMCErrorFrw(oError)):display().
          
          undo, throw oError.
        
        end. /* catch oError as Progress.Lang.Error */
        
      end. /* do with frame {&FRAME-NAME} */
    
    &ENDIF  

    /* set the wait-state to avoid screen-flickering                          */
    
    {fn
      pa_lUISvcSetWaitState}. 
    
    /* init the util class, time logging and output file                      */
    
    goUtilClass = new stamm.base.cls.SBCPersonalUtilsSvo().
    goUtilClass:prplUseTimestamp = yes.

    if glWriteLogfile then
      goUtilClass:outFileOpen(gcLogFile,no).
    
    if glLogTime then
      etime(yes).
    
    /* process each step of the structure creation                            */
    
    if glUseSpecificProcess then
      run createProcess.
    if glLogTime then
      i64ProcessCreation = etime.    
    
    
    if giCreateAssemblies = {&K_AssemblyCreationType_Create} then
    do:
      
      run createAssemblyStructure.
      run createAssemblyReferences.
      
    end. /* if giCreateAssemblies = {&K_AssemblyCreationType_Create} */
    else if giCreateAssemblies = {&K_AssemblyCreationType_Load} then
      run loadAssemblyStructures.
    if glLogTime then
      i64CreateAssStruct = etime.      
    
    
    if glCreateOnHandQty then
      run createOnHandQty.
    if glLogTime then
      i64CreateOnHand = etime.


    if giWorkOrderProcessing = 2 then
      run createWorkOrders.
    else if giWorkOrderProcessing = 3 then
      run loadWorkOrders.
    if glLogTime then
      i64CreateWOs = etime.    
      
      
    if glSchedulePWO then
      run scheduleWOs.
    if glLogTime then
      i64ScheduleWOs = etime.    
      
      
    if glReleaseWO then
      run releaseWOs.
    if glLogTime then
      i64releaseWOs = etime.
    
    
    if glDataEntryBOM then
      run dataEntryBOMLine.
    if glLogTime then
      i64DataEntryBOMLines = etime.

    
    if glDEOperation then
      run dataEntryOperation.
    if glLogTime then
      i64DataEntryOperation = etime.
    
    /* finally display time information if necessary                          */

    if glLogTime then
      message 'Processed Times: ':U
         skip '--------------------------------------':U
         skip pa_cGetTimeSummaryString('Prozesserzeugung: ':U,
                                        i64ProcessCreation,
                                        i64ProcessCreation,
                                        {&K_ObjectCountType_Activity})
         skip pa_cGetTimeSummaryString('Erzeugung Stammdaten: ':U,
                                        i64CreateAssStruct - i64ProcessCreation,
                                        i64CreateAssStruct,
                                        {&K_ObjectCountType_BOM})
         skip pa_cGetTimeSummaryString('Buchung Lagerbestand: ':U,
                                        i64CreateOnHand - i64CreateAssStruct,
                                        i64CreateOnHand,
                                        {&K_ObjectCountType_OnHand})
         skip pa_cGetTimeSummaryString('Anlage Produktionsaufträge: ':U,
                                        i64CreateWOs - i64CreateOnHand,
                                        i64CreateWOs,
                                        {&K_ObjectCountType_WO})
         skip pa_cGetTimeSummaryString('Einplanung Produktionsaufträge: ':U,
                                        i64ScheduleWOs - i64CreateWOs,
                                        i64ScheduleWOs,
                                        {&K_ObjectCountType_WO})
         skip pa_cGetTimeSummaryString('Freigabe Produktionsaufträge: ':U,
                                        i64releaseWOs - i64ScheduleWOs,
                                        i64releaseWOs,
                                        {&K_ObjectCountType_WO})
         skip pa_cGetTimeSummaryString('Rückmeldung Materialien: ':U,
                                        i64DataEntryBOMLines - i64releaseWOs,
                                        i64DataEntryBOMLines,
                                        {&K_ObjectCountType_WO})
         skip pa_cGetTimeSummaryString('Rückmeldung Aktivitäten: ':U,
                                        i64DataEntryOperation - i64DataEntryBOMLines,
                                        i64DataEntryOperation,
                                        {&K_ObjectCountType_WO})                                        
        view-as alert-box
        title program-name(1).
    
    return no-apply.
    
    catch oError as Progress.Lang.Error:
    
      /* Only display progress errors. All other errors were already be         */
      /* displayed                                                              */
    
      (new adm.method.cls.DMCErrorMessageErr(oError)):displayProgressErrorsOnly().
      
      assign
        gcStatusLine                                       = '':U
        gdCurrentCount                                     = ?
        gdMaxCount                                         = ?
        gcStatusLine:screen-value in frame {&FRAME-NAME}   = '':U
        gdCurrentCount:screen-value in frame {&FRAME-NAME} = '':U
        gdMaxCount:screen-value in frame {&FRAME-NAME}     = '':U
        .
      
      return no-apply.
      
    end catch. /* catch oError as Progress.Lang.Error */
    
    finally:
      
      {fn
        pa_lUISvcResetWaitState}.
      
      goUtilClass:outFileclose().
      
    end finally.
    
  end. /* do on error undo, throw */
  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearData Dialog-Frame
ON choose OF btnClearData IN FRAME Dialog-Frame /* STD löschen */
do:
  
  define variable cH as character no-undo.
  define variable c  as character no-undo.
  
  
  run assignScreenValues.
  
  if glWriteLogfile then
    goUtilClass:outFileOpen(gcLogFile,no).

  run writeLogInformation('Clear BOM structures...':U,'':U,yes,'':U).

&IF {&PA_VERSION} >= '7.1':U &THEN

  for each PMM_BomHeadMaster 
    where PMM_BomHeadMaster.Part begins gcAssemblyPrefix
    exclusive-lock
    on error undo, throw:
    
    pps.base.cls.PMCBomSvc:prpoInstance:delete(PMM_BomHeadMaster.PMM_BomHeadMaster_Obj).
    
  end. /* for each P_StkKopf ... */

  run writeLogInformation('Clear assembly part records ...':U,'':U,yes,'':U).
  
  for each S_Artikel 
    where S_Artikel.artikel begins gcAssemblyPrefix
    exclusive-lock
    on error undo, throw:
    
    delete S_Artikel.
    
  end. /* for each S_Artikel ... */

  run writeLogInformation('Clear line part and reservation records ...':U,'':U,yes,'':U).
  
  for each S_Artikel 
    where S_Artikel.artikel begins gcLinePrefix
    exclusive-lock
    on error undo, throw:
    
    for each MMR_Reservation
      where MMR_Reservation.Company       = gcUsedCompany
        and MMR_Reservation.S_Artikel_Obj = S_Artikel.S_Artikel_Obj
      exclusive-lock
      on error undo, throw:
      
      delete MMR_Reservation.
      
    end. /* for each MMR_Reservation ... */
    
    delete S_Artikel.
    
  end. /* for each S_Artikel ... */
  
  run writeLogInformation('Clear routing records ...':U,'':U,yes,'':U).
  
  for each PMM_RoutingMaster 
    where PMM_RoutingMaster.PMM_Routing_ID begins gcProcessPrefix
    exclusive-lock
    on error undo, throw:
      message
         skip
        view-as alert-box
        title program-name(1).
    pps.base.cls.PMCRoutingSvc:prpoInstance:delete(PMM_RoutingMaster.PMM_RoutingMaster_Obj).
      
  end. /* for each MA_Prozess ... */
  
  run writeLogInformation('Clear resource master records ...':U,'':U,yes,'':U).
  
  for each M_Ressource
    where M_Ressource.Ressource begins gcResourcePrefix
    exclusive-lock
    on error undo, throw:
    
    delete m_ressource.
    
  end.
  
  finally:

    goUtilClass:outFileclose().
    
  end finally.  
&ELSE

  for each P_StkKopf 
    where P_StkKopf.Artikel begins gcAssemblyPrefix
    exclusive-lock
    on error undo, throw:
    
    delete P_StkKopf.
    
  end. /* for each P_StkKopf ... */

  for each P_StkKopf 
    where P_StkKopf.Artikel begins gcLinePrefix
    exclusive-lock
    on error undo, throw:
    
    delete P_StkKopf.
    
  end. /* for each P_StkKopf ... */

  run writeLogInformation('Clear part records ...':U,'':U,yes,'':U).
  
  for each S_Artikel 
    where S_Artikel.artikel begins gcAssemblyPrefix
    exclusive-lock
    on error undo, throw:
    
    delete S_Artikel.
    
  end. /* for each S_Artikel ... */

  run writeLogInformation('Clear reservation records ...':U,'':U,yes,'':U).
  
  for each S_Artikel 
    where S_Artikel.artikel begins gcLinePrefix
    exclusive-lock
    on error undo, throw:
    
    for each MMR_Reservation
      where MMR_Reservation.Company       = gcUsedCompany
        and MMR_Reservation.S_Artikel_Obj = S_Artikel.S_Artikel_Obj
      exclusive-lock
      on error undo, throw:
      
      delete MMR_Reservation.
      
    end. /* for each MMR_Reservation ... */
    
    delete S_Artikel.
    
  end. /* for each S_Artikel ... */
  
  run writeLogInformation('Clear routing records ...':U,'':U,yes,'':U).
  
  for each MA_Prozess 
    where MA_Prozess.Prozess begins gcProcessPrefix
    exclusive-lock
    on error undo, throw:
      
      delete MA_Prozess.
      
  end. /* for each MA_Prozess ... */
  
  run writeLogInformation('Clear resource records ...':U,'':U,yes,'':U).
  
  for each M_Ressource 
    where M_Ressource.Ressource begins gcResourcePrefix
    exclusive-lock
    on error undo, throw:
      
      delete M_Ressource.
      
  end. /* for each M_Ressource ... */

&ENDIF
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCreateBOMStructure
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCreateBOMStructure Dialog-Frame
ON CHOOSE OF btnCreateBOMStructure IN FRAME Dialog-Frame /* Struktur erzeugen */
DO:

  /* open the output file if it is not open yet                               */
  /*--------------------------------------------------------------------------*/

  run assignScreenValues.

  if glWriteLogfile then
  do:
    
    if valid-object(goUtilClass) = no then
      assign      
        goUtilClass                  = new stamm.base.cls.SBCPersonalUtilsSvo()
        goUtilClass:prplUseTimestamp = yes
        .    
      
    if goUtilClass:prplOutFileOpened = no then
      goUtilClass:outFileOpen(gcLogFile,no).
  
  end. /* if glWriteLogfile:checked in frame {&FRAME-NAME} */

  run createAssemblyReferences.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteOrders Dialog-Frame
ON CHOOSE OF btnDeleteOrders IN FRAME Dialog-Frame /* PPA löschen */
DO:
  
  define variable iCounter        as integer no-undo.
  define variable iCounterMax     as integer no-undo.
  define variable tzLastStartTime as int64   no-undo.
  define variable tzOverallTime   as int64   no-undo.
  
  define buffer bPP_Auftrag for PP_Auftrag.
  
  /* open the output file if it is not open yet                               */
  /*--------------------------------------------------------------------------*/

  run assignScreenValues.

  if glWriteLogfile then
  do:
    
    if valid-object(goUtilClass) = no then
      assign      
        goUtilClass                  = new stamm.base.cls.SBCPersonalUtilsSvo()
        goUtilClass:prplUseTimestamp = yes
        .    
      
    if goUtilClass:prplOutFileOpened = no then
      goUtilClass:outFileOpen(gcLogFile,no).
  
  end. /* if glWriteLogfile:checked in frame {&FRAME-NAME} */
  
  for each bPP_Auftrag
    where bPP_Auftrag.Firma        = gcUsedCompany
      and bPP_Auftrag.nicht_erster = no
      and bPP_Auftrag.Artikel begins gcAssemblyPrefix
    no-lock
    on error undo, throw:
    
    iCounterMax = iCounterMax + 1.
    
  end. /* for each bPP_Auftrag ... */

  run writeLogInformation('Deleting work orders...':U,'':U,yes,'':U).

  for each bPP_Auftrag
    where bPP_Auftrag.Firma        = gcUsedCompany
      and bPP_Auftrag.nicht_erster = no
      and bPP_Auftrag.Artikel begins gcAssemblyPrefix
    no-lock
    on error undo, throw:

    iCounter = iCounter + 1.
  
    if iCounter <> 1 then
      tzOverallTime = tzOverallTime + etime - tzLastStartTime.

    run writeLogInformation(string(iCounter),
                            string(iCounterMax),
                            no,
                            pa_cGetTimeFormattedString(int64((iCounterMax - iCounter + 1) * tzOverallTime / (iCounter - 1)))).
    
    tzLastStartTime = etime.
    
    pps.prod.cls.PPCProdWorkOrderSvc:prpoInstance:deleteWorkOrder(bPP_Auftrag.PP_Auftrag_Obj).
    
  end. /* for each bPP_Auftrag ... */
  
  finally:
    goUtilClass:outFileclose().    
  end. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveBOMStructure
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveBOMStructure Dialog-Frame
ON CHOOSE OF btnRemoveBOMStructure IN FRAME Dialog-Frame /* Struktur entfernen */
DO:

&IF {&PA_VERSION} >= '7.1':U &THEN  
  define buffer bPMM_BOMLine for PMM_BOMLine.  

  for each bPMM_BOMLine
    where bPMM_BOMLine.Company = gcUsedCompany
      and bPMM_BOMLine.Part    begins gcAssemblyPrefix
    exclusive-lock
    on error undo, throw:
    
    pps.base.cls.PMCBomLineSvc:prpoInstance:delete(bPMM_BOMLine.PMM_BOMLine_Obj).
    
  end. /* for each bPMM_BOMLine ... */
  
&ELSE  
  define buffer bP_StkZeile for P_StkZeile.
&ENDIF

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gbResetServer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gbResetServer Dialog-Frame
ON CHOOSE OF gbResetServer IN FRAME Dialog-Frame /* Delete APS */
DO:
  
  define buffer bM_RessGruppe   for M_RessGruppe.
  define buffer bml_lagergruppe for ml_lagergruppe.


  FOR each bM_RessGruppe where bM_RessGruppe.firma = '110':
    bM_RessGruppe.SERVER = ''.
  END.

find bml_lagergruppe 
  where bml_lagergruppe.lagergruppe = 0 
    and bml_lagergruppe.firma = '110'.
  bml_lagergruppe.SERVER = ''.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giCreateAssemblies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giCreateAssemblies Dialog-Frame
ON VALUE-CHANGED OF giCreateAssemblies IN FRAME Dialog-Frame
DO:
  
  run updateEnabledStatus.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giStructureType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giStructureType Dialog-Frame
ON VALUE-CHANGED OF giStructureType IN FRAME Dialog-Frame
DO:

  run updateEnabledStatus.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giWorkOrderProcessing
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giWorkOrderProcessing Dialog-Frame
ON VALUE-CHANGED OF giWorkOrderProcessing IN FRAME Dialog-Frame
DO:

  run updateEnabledStatus.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glCreateOnHandQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCreateOnHandQty Dialog-Frame
ON value-changed OF glCreateOnHandQty IN FRAME Dialog-Frame /* Create On Hand Qty */
do:
  
  if glCreateOnHandQty:checked in frame {&FRAME-NAME} then
  do:
    {fnarg
      pa_lUISvcEnableWidget
       "gdOnHandQty:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcEnableWidget
       "gdStandardPrice:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcEnableWidget
       "glSingleAssemblyOHQ:handle in frame {&Frame-Name}"}.
  end.
  else
  do:
    
    assign
      gdStandardPrice     = 1
      glSingleAssemblyOHQ = no
      .
    
    {fnarg
      pa_lUISvcDisableWidget
       "gdOnHandQty:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcDisableWidget
       "gdStandardPrice:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcDisableWidget
       "glSingleAssemblyOHQ:handle in frame {&Frame-Name}"}.
       
  end.    
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glDataEntryBOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glDataEntryBOM Dialog-Frame
ON VALUE-CHANGED OF glDataEntryBOM IN FRAME Dialog-Frame /* Stückliste melden */
DO:
  
  run updateEnabledStatus.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glDEOperation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glDEOperation Dialog-Frame
ON VALUE-CHANGED OF glDEOperation IN FRAME Dialog-Frame /* Aktivität melden */
DO:

  if glDEOperation:checked in frame {&FRAME-NAME} then
  do:
  
    {fnarg
      pa_lUISvcEnableWidget
       "gdTSTime:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcEnableWidget
       "gdTUTime:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcEnableWidget
       "gdOperationQty:handle in frame {&Frame-Name}"}.
  end.
  else
  do:
    
    assign
      gdTSTime       = 0
      gdTUTime       = 0
      gdOperationQty = 0
      .

    {fnarg
      pa_lUISvcDisableWidget
       "gdTSTime:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcDisableWidget
       "gdTUTime:handle in frame {&Frame-Name}"}.
    {fnarg
      pa_lUISvcDisableWidget
       "gdOperationQty:handle in frame {&Frame-Name}"}.
    
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glRandomDepth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glRandomDepth Dialog-Frame
ON VALUE-CHANGED OF glRandomDepth IN FRAME Dialog-Frame /* Random Depth */
DO:
  
  run updateEnabledStatus.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glReleaseWO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glReleaseWO Dialog-Frame
ON VALUE-CHANGED OF glReleaseWO IN FRAME Dialog-Frame /* PPA freigeben */
DO:

  run updateEnabledStatus.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glSchedulePWO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glSchedulePWO Dialog-Frame
ON VALUE-CHANGED OF glSchedulePWO IN FRAME Dialog-Frame /* PPA einplanen */
DO:

  run updateEnabledStatus.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glUseSpecificProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glUseSpecificProcess Dialog-Frame
ON value-changed OF glUseSpecificProcess IN FRAME Dialog-Frame /* Use specific Process */
do:

  if glUseSpecificProcess:checked in frame {&FRAME-NAME} then
  do:

    {fnarg
      pa_lUISvcEnableWidget
       "giActivityCount:handle in frame {&Frame-Name}"}.

    {fnarg
      pa_lUISvcEnableWidget
       "gcActivityPrefix:handle in frame {&Frame-Name}"}.

    {fnarg
      pa_lUISvcEnableWidget
       "gcProcessPrefix:handle in frame {&Frame-Name}"}.

    {fnarg
      pa_lUISvcEnableWidget
       "gcResourcePrefix:handle in frame {&Frame-Name}"}.

  end.
  else
  do:
    
    {fnarg
      pa_lUISvcDisableWidget
       "giActivityCount:handle in frame {&Frame-Name}"}.    
    
    {fnarg
      pa_lUISvcDisableWidget
       "gcActivityPrefix:handle in frame {&Frame-Name}"}.    
    
    {fnarg
      pa_lUISvcDisableWidget
       "gcProcessPrefix:handle in frame {&Frame-Name}"}.    
    
    {fnarg
      pa_lUISvcDisableWidget
       "gcResourcePrefix:handle in frame {&Frame-Name}"}.    
    
  end.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glWriteLogfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glWriteLogfile Dialog-Frame
ON VALUE-CHANGED OF glWriteLogfile IN FRAME Dialog-Frame /* Write Logfile */
DO:
  
  run updateEnabledStatus.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{adm/template/incl/dt_dlg00.i}

run initialize in this-procedure.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
  
  gcUsedCompany = pACConnectionSvc:prpcCompany.
  
  run enable_UI.
  
  /* diable the value fields first                                            */

  {fnarg
    pa_lUISvcDisableWidget
     "gdOnHandQty:handle in frame {&Frame-Name}"}.  

  {fnarg
    pa_lUISvcDisableWidget
     "gdstandardPrice:handle in frame {&Frame-Name}"}.  

  {fnarg
    pa_lUISvcDisableWidget
     "glSingleAssemblyOHQ:handle in frame {&Frame-Name}"}.

  {fnarg
    pa_lUISvcDisableWidget
     "giWorkOrderCount:handle in frame {&Frame-Name}"}. 
  
  {fnarg
    pa_lUISvcDisableWidget
     "giActivityCount:handle in frame {&Frame-Name}"}.    
  
  {fnarg
    pa_lUISvcDisableWidget
     "gcActivityPrefix:handle in frame {&Frame-Name}"}.    
  
  {fnarg
    pa_lUISvcDisableWidget
     "gcProcessPrefix:handle in frame {&Frame-Name}"}.    
  
  {fnarg
    pa_lUISvcDisableWidget
     "gcResourcePrefix:handle in frame {&Frame-Name}"}.   


  {fnarg
    pa_lUISvcDisableWidget
     "glDEOperation:handle in frame {&Frame-Name}"}.
  {fnarg
    pa_lUISvcDisableWidget
     "gdTSTime:handle in frame {&Frame-Name}"}.
  {fnarg
    pa_lUISvcDisableWidget
     "gdTUTime:handle in frame {&Frame-Name}"}.
  {fnarg
    pa_lUISvcDisableWidget
     "gdOperationQty:handle in frame {&Frame-Name}"}.

  run updateEnabledStatus.
  
  wait-for go of frame {&FRAME-NAME}.
  
end.
run disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _initialize Dialog-Frame 
PROCEDURE _initialize :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

goUtilClass = new stamm.base.cls.SBCPersonalUtilsSvo().
goUtilClass:prplUseTimestamp = yes.

end procedure. /* _initialize */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignData Dialog-Frame  adm/support/proc/ds_dlg10.p
PROCEDURE assignData :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* Assign screen values to parameter string                                   */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* pcParamListe  List with data in proALPHA String format                     */
/*                                                                            */
/*----------------------------------------------------------------------------*/

define       output parameter pcParamListe as character     no-undo.

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

/* Assign Screen Buffer                                                       */

&IF DEFINED(DISPLAYED-OBJECTS) > 0 &THEN
  do with frame {&FRAME-NAME}
    on error undo, throw:
    assign
      {&DISPLAYED-OBJECTS}
      .
    catch oError as Progress.Lang.Error:
      (new adm.method.cls.DMCErrorFrw(oError)):display().
      return error oError.
    end.
  end. /* do with frame {&FRAME-NAME} */
&ENDIF

/* Assign Default Values                                                      */



/* Default Code                                                               */

{adm/template/incl/dt_dlg07.if}

return.

end procedure. /* assignData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(EXCLUDE-assignScreenValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignScreenValues Method-Library
procedure assignScreenValues private:
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* assigns all visible screen values                                          */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

&IF DEFINED(DISPLAYED-OBJECTS) > 0 &THEN
  do with frame {&FRAME-NAME}
    on error undo, throw:
    
    assign
      {&DISPLAYED-OBJECTS}
      .
      
    catch oError as Progress.Lang.Error:
    
      (new adm.method.cls.DMCErrorFrw(oError)):display().
      
      undo, throw oError.
    
    end. /* catch oError as Progress.Lang.Error */
    
  end. /* do with frame {&FRAME-NAME} */

&ENDIF  

end procedure. /* assignScreenValues */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createAssemblyReferences Dialog-Frame 
PROCEDURE createAssemblyReferences PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable iCurrentAssembly    as integer   no-undo.
define variable iCurrentDepthLevel  as integer   no-undo.

define variable iBaseStructureCount as integer   no-undo.
define variable iRandomBgr          as integer   no-undo.
define variable iCurrBgrPos         as integer   no-undo.
define variable cCurrBgr            as character no-undo.
define variable iCurrBgrHead        as integer no-undo.

define variable tzLastStartTime     as int64     no-undo.
define variable tzOverallTime       as int64     no-undo.

/* Buffers -------------------------------------------------------------------*/

&IF {&PA_VERSION} >= '7.1':U &THEN

define buffer bPMM_BOMHead-Head for PMM_BOMHead.
define buffer bPMM_BOMHead-Curr for PMM_BOMHead.
define buffer bPMM_BOMLine-Last for PMM_BOMLine.

&ELSE

define buffer bP_StkKopf-Head  for P_StkKopf.
define buffer bP_StkZeile      for P_StkZeile.
define buffer bP_StkZeile-Last for P_StkZeile.
define buffer bP_StkZeileSpr   for P_StkZeileSpr.

&ENDIF

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

empty temp-table ttCreatedBaseStructures.

run writeLogInformation('Creating subassembly references...':U,'':U,yes,'':U).

&IF {&PA_VERSION} >= '7.1':U &THEN

find first bPMM_BOMHead-Head
  where bPMM_BOMHead-Head.Company = gcUsedCompany 
    and bPMM_BOMHead-Head.Part    = gcAssemblyPrefix + ' ':U + string(1)
  no-lock.

AssemblyLoop:
do iCurrentAssembly = 1 to giAssemblyCount
  transaction
  on error undo, throw:

  if iCurrentAssembly <> 1 then
    tzOverallTime = tzOverallTime + etime - tzLastStartTime.
  
  run writeLogInformation(string(iCurrentAssembly),
                          string(giAssemblyCount),
                          no,
                          pa_cGetTimeFormattedString(int64((giAssemblyCount - iCurrentAssembly + 1) * tzOverallTime / (iCurrentAssembly - 1)))).
  
  tzLastStartTime = etime.
  
  case giStructureType:
    
    when {&K_StructureType_Single} then
    do:

      if iCurrentAssembly = 1 then
      do:
        
        iBaseStructureCount = 1.
        
        create ttCreatedBaseStructures.
        
        assign
          ttCreatedBaseStructures.BaseStructurePart = bPMM_BOMHead-Head.Part
          ttCreatedBaseStructures.BaseStrucNumber   = 1
          .
        
        validate ttCreatedBaseStructures.      

        iRandomBgr = (if not glRandomDepth then 
                        giOrderMaxDepth - 1
                      else
                        pa_iGetRandomAssemblyNo(giOrderMaxDepth)). 
        
        next AssemblyLoop.
        
      end. /* if  = 1 */

      find first PMM_BOMHead
        where PMM_BOMHead.Company = gcUsedCompany 
          and PMM_BOMHead.Part    = (if iRandomBgr <= 0 then
                                       gcAssemblyPrefix + ' ' + string(1)
                                     else
                                       gcAssemblyPrefix + ' ' + string(iCurrentAssembly - 1))
        no-lock.
      
      find last bPMM_BOMLine-Last
        where bPMM_BOMLine-Last.Company   = PMM_BOMHead.Company
          and bPMM_BOMLine-Last.Owning_Obj   = PMM_BOMHead.PMM_BOMHead_Obj
        no-lock no-error.
      
      assign
        iCurrBgrPos = (if available bPMM_BOMLine-Last then
                         bPMM_BOMLine-Last.BOMLinePosition + 1
                       else
                         1)
        cCurrBgr    = gcAssemblyPrefix + ' ' + (if iRandomBgr <= 0 then
                                                  string(1)
                                                else
                                                  string(iCurrentAssembly - 1))
        .

      if iRandomBgr <= 0 then
        iRandomBgr = (if not glRandomDepth then 
                        giOrderMaxDepth - 1
                      else
                        pa_iGetRandomAssemblyNo(giOrderMaxDepth)).
      else
        iRandomBgr = iRandomBgr - 1.  
      
      
      find first bPMM_BOMHead-Curr
        where bPMM_BOMHead-Curr.Company = gcUsedCompany 
          and bPMM_BOMHead-Curr.Part    = cCurrBgr
        no-lock.      
      
      /* create the assembly line                                             */
      
      pps.base.cls.PMCBomLineSvc:prpoInstance:cCreate
       (new pps.base.cls.PMCBomLinePao(bPMM_BOMHead-Curr.PMM_BOMHead_Obj    ,
                                       iCurrBgrPos,
                                       gcAssemblyPrefix + ' ' + string(iCurrentAssembly))).
      
    end. /* when 'Single Assembly':U */ 
    
    when {&K_StructureType_Separate} then
    do:

      find first PMM_BOMHead
        where PMM_BOMHead.Company = gcUsedCompany 
          and PMM_BOMHead.Part    = gcAssemblyPrefix + ' ' + string(iCurrentAssembly)
        no-lock.
      
      iBaseStructureCount = iBaseStructureCount + 1.
      
      create ttCreatedBaseStructures.
      
      assign
        ttCreatedBaseStructures.BaseStructurePart = PMM_BOMHead.Part
        ttCreatedBaseStructures.BaseStrucNumber   = iBaseStructureCount
        .
      
      validate ttCreatedBaseStructures. 
      
    end.
    
    when {&K_StructureType_Structural} then
    do:

      if iRandomBgr <= 0 then
      do:
        
        find first bPMM_BOMHead-Head
          where bPMM_BOMHead-Head.Part = gcAssemblyPrefix + ' ' + string(iCurrentAssembly)
          no-lock.        
        
        assign
          iCurrBgrHead        = iCurrentAssembly
          iBaseStructureCount = iBaseStructureCount + 1
          .
        
        create ttCreatedBaseStructures.
        
        assign
          ttCreatedBaseStructures.BaseStructurePart = bPMM_BOMHead-Head.Part
          ttCreatedBaseStructures.BaseStrucNumber   = iBaseStructureCount
          .
        
        validate ttCreatedBaseStructures.      

        iRandomBgr = (if not glRandomDepth then 
                        giOrderMaxDepth
                      else
                        pa_iGetRandomAssemblyNo(giOrderMaxDepth)).  
        
        next AssemblyLoop.
        
      end. /* if iRandomBgr = 0 */
      
      
      find first PMM_BOMHead
        where PMM_BOMHead.Company = gcUsedCompany 
          and PMM_BOMHead.Part    = gcAssemblyPrefix + ' ' + string(iCurrentAssembly - 1)
        no-lock.
      
      find last bPMM_BOMLine-Last
        where bPMM_BOMLine-Last.Company    = gcUsedCompany
          and bPMM_BOMLine-Last.Owning_Obj = PMM_BOMHead.PMM_BOMHead_Obj
        no-lock no-error.
      
      assign
        iCurrBgrPos = (if available bPMM_BOMLine-Last then
                         bPMM_BOMLine-Last.BOMLinePosition + 1
                       else
                         1)
        cCurrBgr    = gcAssemblyPrefix + ' ' + string(iCurrentAssembly - 1)
        iRandomBgr  = iRandomBgr - 1
        .

      find first bPMM_BOMHead-Curr
        where bPMM_BOMHead-Curr.Company = gcUsedCompany 
          and bPMM_BOMHead-Curr.Part    = cCurrBgr
        no-lock.    
        
      /* create the assembly line                                             */

      pps.base.cls.PMCBomLineSvc:prpoInstance:cCreate
       (new pps.base.cls.PMCBomLinePao(bPMM_BOMHead-Curr.PMM_BOMHead_Obj    ,
                                       iCurrBgrPos,
                                       gcAssemblyPrefix + ' ' + string(iCurrentAssembly))).
      
    end.
    
  end case. /* case giStructureType */
  
end. /* do i = 1 to giAssemblyCount */

&ELSE

find first bP_StkKopf-Head
  where bP_StkKopf-Head.Artikel = gcAssemblyPrefix + ' ' + string(1)
  no-lock.

AssemblyLoop:
do iCurrentAssembly = 1 to giAssemblyCount
  transaction
  on error undo, throw:

  if iCurrentAssembly <> 1 then
    tzOverallTime = tzOverallTime + etime - tzLastStartTime.
  
  run writeLogInformation(string(iCurrentAssembly),
                          string(giAssemblyCount),
                          no,
                          pa_cGetTimeFormattedString(int64((giAssemblyCount - iCurrentAssembly + 1) * tzOverallTime / (iCurrentAssembly - 1)))).
  
  tzLastStartTime = etime.
  
  case giStructureType:
    
    when {&K_StructureType_Single} then
    do:

      if iCurrentAssembly = 1 then
      do:
        
        iBaseStructureCount = 1.
        
        create ttCreatedBaseStructures.
        
        assign
          ttCreatedBaseStructures.BaseStructurePart = bP_StkKopf-Head.Artikel
          ttCreatedBaseStructures.BaseStrucNumber   = 1
          .
        
        validate ttCreatedBaseStructures.      

        iRandomBgr = (if not glRandomDepth then 
                        giOrderMaxDepth - 1
                      else
                        pa_iGetRandomAssemblyNo(giOrderMaxDepth)). 
        
        next AssemblyLoop.
        
      end. /* if  = 1 */

      find first P_StkKopf
        where P_StkKopf.Firma   = gcUsedCompany 
          and P_StkKopf.Art     = 'SSt':U   
          and P_StkKopf.Artikel = (if iRandomBgr <= 0 then
                                     gcAssemblyPrefix + ' ' + string(1)
                                   else
                                     gcAssemblyPrefix + ' ' + string(iCurrentAssembly - 1))
        no-lock.
      
      find last bP_StkZeile-Last
        where bP_StkZeile-Last.Firma     = gcUsedCompany
          and bP_StkZeile-Last.Art       = 'SSt':U
          and bP_StkZeile-Last.Baugruppe = P_StkKopf.Artikel
          and bP_StkZeile-Last.BgrArtVar = P_StkKopf.ArtVar
          and bP_StkZeile-Last.IndexNr   = P_StkKopf.IndexNr
          and bP_StkZeile-Last.Nummer    = 0
        no-lock no-error.
      
      assign
        iCurrBgrPos = bP_StkZeile-Last.Position + 1
        cCurrBgr    = gcAssemblyPrefix + ' ' + (if iRandomBgr <= 0 then
                                                  string(1)
                                                else
                                                  string(iCurrentAssembly - 1))
        .

      if iRandomBgr <= 0 then
        iRandomBgr = (if not glRandomDepth then 
                        giOrderMaxDepth - 1
                      else
                        pa_iGetRandomAssemblyNo(giOrderMaxDepth)).
      else
        iRandomBgr = iRandomBgr - 1.  
      
        
      /* create the assembly line                                             */
      
      create bP_StkZeile.
      
      assign
        bP_StkZeile.Firma     = gcUsedCompany
        bP_StkZeile.Art       = 'sst'
        bP_StkZeile.Baugruppe = cCurrBgr                           
        bP_StkZeile.BgrArtVar = ''
        bP_StkZeile.IndexNr   = ''
        bP_StkZeile.Nummer    = 0
        bP_StkZeile.Position  = iCurrBgrPos
        .
      
      validate bP_StkZeile.
      
      bP_StkZeile.Artikel = gcAssemblyPrefix + ' ' + string(iCurrentAssembly). 
      
      validate bP_StkZeile.
    
      /* create line spr record                                               */
    
      create bP_StkZeileSpr.
      
      assign
        bP_StkZeileSpr.Firma    = gcUsedCompany
        bP_StkZeileSpr.IdentStk = bP_StkZeile.IdentStk 
        bP_StkZeileSpr.Sprache  = 'D'
        .
        
      validate bP_StkZeileSpr.        
      
    end. /* when 'Single Assembly':U */ 
    
    when {&K_StructureType_Separate} then
    do:

      find first P_StkKopf
        where P_StkKopf.Firma   = gcUsedCompany 
          and P_StkKopf.Art     = 'SSt':U   
          and P_StkKopf.Artikel = gcAssemblyPrefix + ' ' + string(iCurrentAssembly)
        no-lock.
      
      iBaseStructureCount = iBaseStructureCount + 1.
      
      create ttCreatedBaseStructures.
      
      assign
        ttCreatedBaseStructures.BaseStructurePart = P_StkKopf.Artikel
        ttCreatedBaseStructures.BaseStrucNumber   = iBaseStructureCount
        .
      
      validate ttCreatedBaseStructures. 
      
    end.
    
    when {&K_StructureType_Structural} then
    do:

      if iRandomBgr <= 0 then
      do:
        
        find first bP_StkKopf-Head
          where bP_StkKopf-Head.Artikel = gcAssemblyPrefix + ' ' + string(iCurrentAssembly)
          no-lock.        
        
        assign
          iCurrBgrHead        = iCurrentAssembly
          iBaseStructureCount = iBaseStructureCount + 1
          .
        
        create ttCreatedBaseStructures.
        
        assign
          ttCreatedBaseStructures.BaseStructurePart = bP_StkKopf-Head.Artikel
          ttCreatedBaseStructures.BaseStrucNumber   = iBaseStructureCount
          .
        
        validate ttCreatedBaseStructures.      

        iRandomBgr = (if not glRandomDepth then 
                        giOrderMaxDepth
                      else
                        pa_iGetRandomAssemblyNo(giOrderMaxDepth)).  
        
        next AssemblyLoop.
        
      end. /* if iRandomBgr = 0 */
      
      
      find first P_StkKopf
        where P_StkKopf.Firma   = gcUsedCompany 
          and P_StkKopf.Art     = 'SSt':U   
          and P_StkKopf.Artikel = gcAssemblyPrefix + ' ' + string(iCurrentAssembly - 1)
        no-lock.
      
      find last bP_StkZeile-Last
        where bP_StkZeile-Last.Firma     = gcUsedCompany
          and bP_StkZeile-Last.Art       = 'SSt':U
          and bP_StkZeile-Last.Baugruppe = P_StkKopf.Artikel
          and bP_StkZeile-Last.BgrArtVar = P_StkKopf.ArtVar
          and bP_StkZeile-Last.IndexNr   = P_StkKopf.IndexNr
          and bP_StkZeile-Last.Nummer    = 0
        no-lock no-error.
      
      assign
        iCurrBgrPos = bP_StkZeile-Last.Position + 1
        cCurrBgr    = gcAssemblyPrefix + ' ' + string(iCurrentAssembly - 1)
        iRandomBgr  = iRandomBgr - 1
        .
        
      /* create the assembly line                                             */
      
      create bP_StkZeile.
      
      assign
        bP_StkZeile.Firma     = gcUsedCompany
        bP_StkZeile.Art       = 'sst'
        bP_StkZeile.Baugruppe = cCurrBgr                           
        bP_StkZeile.BgrArtVar = ''
        bP_StkZeile.IndexNr   = ''
        bP_StkZeile.Nummer    = 0
        bP_StkZeile.Position  = iCurrBgrPos
        .
      
      validate bP_StkZeile.
      
      bP_StkZeile.Artikel = gcAssemblyPrefix + ' ' + string(iCurrentAssembly). 
      
      validate bP_StkZeile.
    
      /* create line spr record                                               */
    
      create bP_StkZeileSpr.
      
      assign
        bP_StkZeileSpr.Firma    = gcUsedCompany
        bP_StkZeileSpr.IdentStk = bP_StkZeile.IdentStk 
        bP_StkZeileSpr.Sprache  = 'D'
        .
        
      validate bP_StkZeileSpr. 
      
    end.
    
  end case. /* case giStructureType */
  
end. /* do i = 1 to giAssemblyCount */

&ENDIF

end procedure. /* createAssemblyReferences */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createAssemblyStructure Dialog-Frame 
PROCEDURE createAssemblyStructure PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i                   as integer   no-undo.
define variable j                   as integer   no-undo.
define variable iRandomBgr          as integer   no-undo.
define variable iCurrBgrPos         as integer   no-undo.
define variable cCurrBgr            as character no-undo.
define variable iBaseStructureCount as integer   no-undo.
define variable tzLastStartTime     as int64     no-undo.
define variable tzOverallTime       as int64     no-undo.

&IF {&PA_VERSION} >= '7.1':U &THEN
define variable cBOMHeadMasterObj   as character no-undo.
&ENDIF
/* Buffers -------------------------------------------------------------------*/

define buffer bS_Artikel-Bgr    for S_Artikel.
define buffer bS_Artikel-Line   for S_Artikel.
define buffer bML_Lagergruppe   for ML_Lagergruppe.
define buffer bML_Ort           for ML_Ort.
define buffer bS_ArtikelSpr     for S_ArtikelSpr.
define buffer bMLM_StorPartData for MLM_StorPartData.
define buffer bMD_Artikel       for MD_Artikel.

&IF {&PA_VERSION} >= '7.1':U &THEN
define buffer bPMM_BOMHead      for PMM_BOMHead.
&ELSE
define buffer bP_StkKopf-Head  for P_StkKopf.
define buffer bP_StkZeile-Last for P_StkZeile.
define buffer bP_StkZeile      for P_StkZeile.
define buffer bP_StkZeileSpr   for P_StkZeileSpr.
define buffer bP_StkKopf       for P_StkKopf.
define buffer bP_StkProzess    for P_StkProzess.
&ENDIF

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('Creating assembly strucutres...':U,'':U,yes,'':U).

/* load common information                                                    */
/*----------------------------------------------------------------------------*/

find bML_Lagergruppe
  where bML_Lagergruppe.Firma       = gcUsedCompany
    and bML_Lagergruppe.Lagergruppe = 0
  no-lock.

find bML_Ort 
  where bML_Ort.Firma    = gcUsedCompany
    and bML_Ort.LagerOrt = 50
  no-lock.

/* create the assembly structures                                             */
/*----------------------------------------------------------------------------*/

do i = 1 to giAssemblyCount
  transaction
  on error undo, throw:

  if i <> 1 then
    tzOverallTime = tzOverallTime + etime - tzLastStartTime.
  
  run writeLogInformation(string(i),
                          string(giAssemblyCount),
                          no,
                          pa_cGetTimeFormattedString(int64((giAssemblyCount - i + 1) * tzOverallTime / (i - 1)))).
  
  tzLastStartTime = etime.
  
  /* create part for assembly                                                 */
  /*--------------------------------------------------------------------------*/
   
  create bS_Artikel-Bgr.

  assign
    bS_Artikel-Bgr.Firma      = gcUsedCompany
    bS_Artikel-Bgr.Artikel    = gcAssemblyPrefix + ' ' + string(i)
    bS_Artikel-Bgr.ArtikelArt = giPartType
    .

  validate bS_Artikel-Bgr.

  assign
    bS_Artikel-Bgr.SME_Faktor         = 1
    bS_Artikel-Bgr.ArtikelGruppe      = '10'
    bS_Artikel-Bgr.Sparte             = '200'  /* > 6.1e */
    bS_Artikel-Bgr.WBZ                = 10
    bS_Artikel-Bgr.sbm_valueflowgroup = 'PA2220:xupd:c27911cff1d1f7b3e713a46b08c9c559'  /* > 6.1e */
    .

  validate bS_Artikel-Bgr.
  
  /* create the parts language record                                         */

  create bS_ArtikelSpr.
  assign
     bS_ArtikelSpr.Firma   = gcUsedCompany
     bS_ArtikelSpr.Artikel = bS_Artikel-Bgr.Artikel
     bS_ArtikelSpr.Sprache = 'D'
     .
  
  validate bS_ArtikelSpr.
  
  /* add storage area                                                         */
  
  create bMLM_StorPartData.
  
  assign
    bMLM_StorPartData.Company            = gcUsedCompany
    bMLM_StorPartData.Part               = bS_Artikel-Bgr.Artikel
    bMLM_StorPartData.ArtVar             = ''
    bMLM_StorPartData.Storagearea        = 50
    bMLM_StorPartData.ML_Lagergruppe_Obj = bML_Lagergruppe.ML_Lagergruppe_Obj
    .
  
  validate bMLM_StorPartData.
  
  bMLM_StorPartData.ML_Ort_Obj = bML_Ort.ML_Ort_Obj.
  
  validate bMLM_StorPartData.
  
  /* add mrp seetings                                                         */
  
  find bMD_Artikel
    where bMD_Artikel.Firma       = gcUsedCompany
      and bMD_Artikel.Artikel     = bS_Artikel-Bgr.Artikel
      and bMD_Artikel.Lagergruppe = 0
    exclusive-lock.
    
  assign
    bMD_Artikel.Prodlager_Ab         = 50
    bMD_Artikel.Prodlager_Zu         = 50
    bMD_Artikel.Basislager           = 50
    bMD_Artikel.OptimizationRelevant = yes
    bMD_Artikel.MRPRelevant          = yes
    bMD_Artikel.DispoArt             = 0
    bMD_Artikel.Horizont             = 120
    .
  
  validate bMD_Artikel.
  
  /* create the BOM head for the assembly                                     */
  /*--------------------------------------------------------------------------*/

&IF {&PA_VERSION} >= '7.1':U &THEN

  cBOMHeadMasterObj = pps.base.cls.PMCBomSvc:prpoInstance:cCreate
                       (new pps.base.cls.PMCBomPao(bS_Artikel-Bgr.Artikel)).
  
  find bPMM_BOMHead
    where bPMM_BOMHead.PMM_BOMHead_Obj = cBOMHeadMasterObj
    no-lock.

&ELSE
  create bP_StkKopf.
  
  assign
    bP_StkKopf.Firma   = gcUsedCompany
    bP_StkKopf.Art     = 'sst'
    bP_StkKopf.Artikel = bS_Artikel-Bgr.Artikel
    bP_StkKopf.ArtVar  = ''
    bP_StkKopf.IndexNr = ''
    .
  
  validate bP_StkKopf.
  
  assign
    bP_StkKopf.AusfassListe = yes
    .
  
  validate bP_StkKopf.

&ENDIF  

  /* add the specific process to the new created BOM                          */
  /*--------------------------------------------------------------------------*/

  if glUseSpecificProcess then
  do:

  &IF {&PA_VERSION} >= '7.1':U &THEN
   
   find first PMM_Routing
    where PMM_Routing.Company        = gcUsedCompany
      and PMM_Routing.PMM_Routing_ID = gcProcessPrefix 
    no-lock.
   
   pps.base.cls.PMCBomHeadRoutingRelSvc:prpoInstance:cCreate
    (new pps.base.cls.PMCBomRoutingRelPao(bPMM_BOMHead.PMM_BOMHead_Obj,
                                          PMM_Routing.PMM_Routing_ID)).

  &ELSE

   create bP_StkProzess.
   
   assign
     bP_StkProzess.Firma        = gcUsedCompany
     bP_StkProzess.Art          = 'SSt':U
     bP_StkProzess.Artikel      = bS_Artikel-Bgr.Artikel
     bP_StkProzess.ArtVar       = '':U
     bP_StkProzess.IndexNrStk   = '':U
     bP_StkProzess.Prozess      = gcProcessPrefix
     bP_StkProzess.IndexNrPro   = '':U
     bP_StkProzess.BasisProzess = yes
     .
   
   validate bP_StkProzess.

  &ENDIF
   
  end. /* if lUseSpecificProcess */
  
  /* create the BOM lines for the new BOM head record                         */
  /*--------------------------------------------------------------------------*/
  
  do j = 1 to giPurchasePartCount
    on error undo, throw:
    
    /* create part for line                                                   */
    /*------------------------------------------------------------------------*/
    
    create bS_Artikel-Line.
    
    assign
      bS_Artikel-Line.Firma      = gcUsedCompany
      bS_Artikel-Line.Artikel    = gcLinePrefix + ' ' + string(i) + '-' + string(j)
      bS_Artikel-Line.ArtikelArt = 10
      . 
    
    validate bS_Artikel-Line.

    assign
      bS_Artikel-Line.SME_Faktor         = 1
      bS_Artikel-Line.ArtikelGruppe      = '10'
      bS_Artikel-Line.Sparte             = '200'  /* > 6.1e */
      bS_Artikel-Line.sbm_valueflowgroup = 'PA2220:xupd:c27911cff1d1f7b3e713a46b08c9c559'  /* > 6.1e */
      .
  
    validate bS_Artikel-Line.

    
    create bS_ArtikelSpr.
    assign
       bS_ArtikelSpr.Firma   = gcUsedCompany
       bS_ArtikelSpr.Artikel = bS_Artikel-Line.Artikel
       bS_ArtikelSpr.Sprache = 'D'
       .
    
    validate bS_ArtikelSpr.
    
    /* add storage area                                                       */
    
    create bMLM_StorPartData.
    
    assign
      bMLM_StorPartData.Company            = gcUsedCompany
      bMLM_StorPartData.Part               = bS_Artikel-Line.Artikel
      bMLM_StorPartData.ArtVar             = ''
      bMLM_StorPartData.Storagearea        = 50
      bMLM_StorPartData.ML_Lagergruppe_Obj = bML_Lagergruppe.ML_Lagergruppe_Obj
      .
    
    validate bMLM_StorPartData.

    bMLM_StorPartData.ML_Ort_Obj = bML_Ort.ML_Ort_Obj.
    
    validate bMLM_StorPartData.
    
    /* add mrp seetings                                                       */
    
    find bMD_Artikel
      where bMD_Artikel.Firma       = gcUsedCompany
        and bMD_Artikel.Artikel     = bS_Artikel-Line.Artikel
        and bMD_Artikel.Lagergruppe = 0
      exclusive-lock.
      
    assign
      bMD_Artikel.Prodlager_Ab = 50
      bMD_Artikel.Prodlager_Zu = 50
      bMD_Artikel.Basislager   = 50
      .
    
    validate bMD_Artikel.    
    
    /* create the line record                                                 */
    /*------------------------------------------------------------------------*/

  &IF {&PA_VERSION} >= '7.1':U &THEN
    
    pps.base.cls.PMCBomLineSvc:prpoInstance:cCreate
     (new pps.base.cls.PMCBomLinePao(bPMM_BOMHead.PMM_BOMHead_Obj,
                                     j,
                                     bS_Artikel-Line.Artikel)).

  &ELSE

    create bP_StkZeile.
    
    assign
      bP_StkZeile.Firma     = gcUsedCompany
      bP_StkZeile.Art       = 'sst'
      bP_StkZeile.Baugruppe = bP_StkKopf.Artikel
      bP_StkZeile.BgrArtVar = ''
      bP_StkZeile.IndexNr   = ''
      bP_StkZeile.Nummer    = 0
      bP_StkZeile.Position  = j
      .
      
    validate bP_StkZeile.
    
    bP_StkZeile.Artikel = bS_Artikel-Line.Artikel. 
    
    validate bP_StkZeile.

    /* create line spr record                                                 */

    create bP_StkZeileSpr.
    
    assign
      bP_StkZeileSpr.Firma     = gcUsedCompany
      bP_StkZeileSpr.IdentStk  = bP_StkZeile.IdentStk 
      bP_StkZeileSpr.Sprache   = 'D'
     .
      
    validate bP_StkZeileSpr.

  &ENDIF

  end. /* do j = 1 to iPurchasePartCount */  
  
end. /* do i = 1 to iAssemblyCount */

end procedure. /* createAssemblyStructure */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createOnHandQty Dialog-Frame 
PROCEDURE createOnHandQty PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i                      as integer                          no-undo.
define variable j                      as integer                          no-undo.
define variable k                      as integer                          no-undo.
define variable cTemp                  as character                        no-undo.
define variable lTemp                  as logical                          no-undo.
                                                                           
define variable cCostAccObjectOID      as character                        no-undo.
define variable iPrimaryIncDriverGroup as integer                          no-undo.
define variable cPrimaryDriverID       as character                        no-undo.
define variable cDimensionArrayObj     like S_Artikel.S_Artikel_Obj 
                                       extent {&pa_SB_IncDimension_Extent} no-undo.

define variable tzLastStartTime     as int64     no-undo.
define variable tzOverallTime       as int64     no-undo.

define variable oMaterialPosting as class mawi.base.cls.MMCMaterialPostingSvo no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bS_KoTraeger   for S_KoTraeger.
define buffer bML_Ort        for ML_Ort.
define buffer bS_Artikel     for S_Artikel.
define buffer bS_ArtikelKalk for S_ArtikelKalk.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('Creating on hand quantities...':U,'':U,yes,'':U).

/* load common information                                                    */
/*----------------------------------------------------------------------------*/

oMaterialPosting = new mawi.base.cls.MMCMaterialPostingSvo().
&IF {&PA_VERSION} < '6.2':U &THEN
find first bS_KoTraeger
  where bS_KoTraeger.firma            = {firma/skotrae.fir pACConnectionSvc:prpcCompany}
    and bS_KoTraeger.direkt_bebuchbar = yes
  no-lock.
&ENDIF

find bML_Ort 
  where bML_Ort.Firma    = gcUsedCompany
    and bML_Ort.LagerOrt = 50
  no-lock.

/* create on hand quantities                                                  */
/*----------------------------------------------------------------------------*/

do i = 1 to giAssemblyCount:
  
  if glSingleAssemblyOHQ:checked in frame {&FRAME-NAME} then
  do
    transaction
    on error undo, throw:
      
      k = k + 1.
      
      if i <> 1 then
        tzOverallTime = tzOverallTime + etime - tzLastStartTime.
  
      run writeLogInformation
           (k,
            string(giAssemblyCount),
            no,
            pa_cGetTimeFormattedString(int64((giAssemblyCount - k + 1) * tzOverallTime / (k - 1)))).
  
      find bS_Artikel 
        where bS_Artikel.Artikel = gcLinePrefix + ' ' + string(i) + '-' + string(1)
        no-lock.

    &IF {&PA_VERSION} >= '6.2':U &THEN
  
      stamm.base.cls.SBCDriverDeterminationSvc:prpoInstance:DetermineDriver
        (       yes,
                bS_Artikel.S_Artikel_Obj,
                bS_Artikel.Driver_Obj,
                stamm.base.cls.SBCMasterFilesAccountingSvc:prpoInstance:cDriverObj('1-10001',{&pa_S_Traeger}),
                '':U,
                '':U,
                '':U,
              &IF {&PA_VERSION} >= '7.1':U &THEN
                '':U,
              &ENDIF
         output cCostAccObjectOID,
         output iPrimaryIncDriverGroup,
         output cPrimaryDriverID,
         output cDimensionArrayObj).
  
    &ENDIF
      oMaterialPosting:emptyMaterialPosting().
  
      mawi.base.cls.MMCReservationSvc:prpoInstance:PostReservationWithoutDocument
        (input         bML_Ort.ML_Ort_Obj,
         input         bS_Artikel.S_Artikel_Obj,
         input         '',
         input         '',
         input         today,
         input         (-1 * gdOnHandQty),
         input         1,
         input         bS_Artikel.LagerME,
         input         'lzu',
         input         '':U,
         input-output ctemp,
               output ltemp).
  
  
    &IF {&PA_VERSION} >= '6.2':U &THEN
      oMaterialPosting:StockReceipt(bS_Artikel.S_Artikel_Obj,
                                    '',
                                    'LZU',
                                    bML_Ort.ML_Ort_Obj,
                                    today,
                                    gdOnHandQty,
                                    ctemp,
                                    '',
                                    cCostAccObjectOID,
                                    iPrimaryIncDriverGroup,
                                    cPrimaryDriverID,
                                    cDimensionArrayObj,
                                    'LZU Part ' + bS_Artikel.Artikel,
                                    '':U,
                                  &IF {&PA_VERSION} >= '7.1':U &THEN
                                    '':U,
                                  &ENDIF
                                    20,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    '').
    &ELSE
      oMaterialPosting:StockReceipt(bS_Artikel.S_Artikel_Obj,
                                    '',
                                    'LZU',
                                    bML_Ort.ML_Ort_Obj,
                                    today,
                                    gdOnHandQty,
                                    cTemp,
                                    0,
                                    bS_KoTraeger.S_Kotraeger_Obj,
                                    'LZU Part ' + bS_Artikel.Artikel,
                                    '':U,
                                    20,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    '').
    &ENDIF
      oMaterialPosting:saveMaterialPosting().
  
      /* set standard price                                                   */
      
      create bS_ArtikelKalk.
      
      assign
        bS_Artikelkalk.Firma      = '110'
        bS_Artikelkalk.Artikel    = bS_Artikel.Artikel
        bS_Artikelkalk.gueltig_ab = today
        .
        
      validate bS_ArtikelKalk.
      
      bS_Artikelkalk.Standardpreis = gdStandardPrice.  
      
      validate bS_ArtikelKalk.
    
  end. /* if glSingleAssemblyOHQ:checked in frame {&FRAME-NAME} ... */
  else
  do j = 1 to giPurchasePartCount
  transaction
  on error undo, throw:

    k = k + 1.
  
    if   i <> 1 
      or j <> 1 then
      tzOverallTime = tzOverallTime + etime - tzLastStartTime.

    run writeLogInformation
         (k,
          string(giAssemblyCount * giPurchasePartCount),
          no,
          pa_cGetTimeFormattedString(int64((giAssemblyCount * giPurchasePartCount - k + 1) * tzOverallTime / (k - 1)))).

    find bS_Artikel 
      where bS_Artikel.Artikel = gcLinePrefix + ' ' + string(i) + '-' + string(j)
      no-lock.

&IF {&PA_VERSION} >= '6.2':U &THEN

    stamm.base.cls.SBCDriverDeterminationSvc:prpoInstance:DetermineDriver
      (       yes,
              bS_Artikel.S_Artikel_Obj,
              bS_Artikel.Driver_Obj,
              stamm.base.cls.SBCMasterFilesAccountingSvc:prpoInstance:cDriverObj('1-10001',{&pa_S_Traeger}),
              '':U,
              '':U,
              '':U,
&IF {&PA_VERSION} >= '7.1':U &THEN
              '':U,
&ENDIF
       output cCostAccObjectOID,
       output iPrimaryIncDriverGroup,
       output cPrimaryDriverID,
       output cDimensionArrayObj).

&ENDIF
    oMaterialPosting:emptyMaterialPosting().

    mawi.base.cls.MMCReservationSvc:prpoInstance:PostReservationWithoutDocument
      (input         bML_Ort.ML_Ort_Obj,
       input         bS_Artikel.S_Artikel_Obj,
       input         '',
       input         '',
       input         today,
       input         (-1 * gdOnHandQty),
       input         1,
       input         bS_Artikel.LagerME,
       input         'lzu',
       input         '':U,
       input-output ctemp,
             output ltemp).


  &IF {&PA_VERSION} >= '6.2':U &THEN
    oMaterialPosting:StockReceipt(bS_Artikel.S_Artikel_Obj,
                                  '',
                                  'LZU',
                                  bML_Ort.ML_Ort_Obj,
                                  today,
                                  gdOnHandQty,
                                  ctemp,
                                  '',
                                  cCostAccObjectOID,
                                  iPrimaryIncDriverGroup,
                                  cPrimaryDriverID,
                                  cDimensionArrayObj,
                                  'LZU Part ' + bS_Artikel.Artikel,
                                  '':U,
                                &IF {&PA_VERSION} >= '7.1':U &THEN
                                  '':U,
                                &ENDIF
                                  20,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  '').
  &ELSE
    oMaterialPosting:StockReceipt(bS_Artikel.S_Artikel_Obj,
                                  '',
                                  'LZU',
                                  bML_Ort.ML_Ort_Obj,
                                  today,
                                  gdOnHandQty,
                                  cTemp,
                                  0,
                                  bS_KoTraeger.S_Kotraeger_Obj,
                                  'LZU Part ' + bS_Artikel.Artikel,
                                  '':U,
                                  20,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  '').
  &ENDIF
    oMaterialPosting:saveMaterialPosting().

    /* set standard price                                                     */
    
    create bS_ArtikelKalk.
    
    assign
      bS_Artikelkalk.Firma      = '110'
      bS_Artikelkalk.Artikel    = bS_Artikel.Artikel
      bS_Artikelkalk.gueltig_ab = today
      .
      
    validate bS_ArtikelKalk.
    
    bS_Artikelkalk.Standardpreis = gdStandardPrice.  
    
    validate bS_ArtikelKalk.

  end. /* else do j = 1 to giPurchasePartCount ... */
  
end. /* do i = 1 to iAssemblyCount */

finally:
  
  if valid-object(oMaterialPosting) then
    delete object oMaterialPosting.
  
end finally.

end procedure. /* createOnHandQty */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createProcess Dialog-Frame 
PROCEDURE createProcess PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

&IF {&PA_VERSION} >= '7.1':U &THEN
define variable i                as integer   no-undo.
define variable cLastActivityObj as character no-undo.
define variable cRoutingObj      as character no-undo.
define variable cOperationObj    as character no-undo.
define variable oPPAO            as class pps.base.cls.PMCOperationPao  no-undo.


/* Buffers -------------------------------------------------------------------*/

define buffer bPMM_OperationRes for PMM_OperationRes.
define buffer bPMM_Routing      for PMM_Routing.

&ELSE

define buffer bMA_Prozess    for MA_Prozess.
define buffer bMA_ProzessSpr for MA_ProzessSpr.
define buffer bMA_Aktivitaet for MA_Aktivitaet.
define buffer bMA_Ressource  for MA_Ressource.
define buffer bMA_AktAob     for MA_AktAob.

define variable i               as integer no-undo.
define variable iLastActivityID as integer no-undo.

&ENDIF

define buffer bM_RessGruppe   for M_RessGruppe.
define buffer bM_RessourceSpr for M_RessourceSpr.
define buffer bM_Ressource    for M_Ressource.


/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('Creating Process...':U,'':U,yes,'':U).

do 
  transaction 
  on error undo, throw:

  /* create the specific standard process record                              */
  /*--------------------------------------------------------------------------*/  
  
  &IF {&PA_VERSION} >= '7.1':U &THEN
  
  cRoutingObj = pps.base.cls.PMCRoutingSvc:prpoInstance:cCreate(new pps.base.cls.PMCRoutingPao(gcProcessPrefix)).   
  
  find bPMM_Routing
    where bPMM_Routing.PMM_Routing_Obj = cRoutingObj
    no-lock.
  
  &ELSE
  
  create bMA_Prozess.
  
  assign
    bMA_Prozess.Firma          = gcUsedCompany
    bMA_Prozess.ProzessBereich = 'PPA':U
    bMA_Prozess.Prozess        = gcProcessPrefix
    bMA_Prozess.IndexNr        = '':U
    .
  
  validate bMA_Prozess.
    
  /* create process language record                                           */
  
  create bMA_ProzessSpr.
  assign
    bMA_ProzessSpr.Firma          = gcUsedCompany
    bMA_ProzessSpr.ProzessBereich = 'PPA':U
    bMA_ProzessSpr.Prozess        = gcProcessPrefix
    bMA_ProzessSpr.IndexNr        = '':U
    bMA_ProzessSpr.Sprache        = 'D':U
    .
    
  validate bMA_ProzessSpr.    
  
  &ENDIF
  /* create the resource used in each of the created activities               */
  /*--------------------------------------------------------------------------*/
  
  if not can-find(M_Ressource
                    where M_Ressource.Firma     = gcUsedCompany
                      and M_Ressource.RessArt   = 7
                      and M_Ressource.Ressource = gcResourcePrefix ) then
  do:
  
    find first bM_RessGruppe
      where bM_RessGruppe.Firma = {firma/mawi.fir gcUsedCompany}
      no-lock.
    
    create bM_Ressource.
    
    assign
      bM_Ressource.Firma     = gcUsedCompany
      bM_Ressource.RessArt   = 7
      bM_Ressource.Ressource = gcResourcePrefix
      .
    
    validate bM_Ressource.
    
    assign  
      bM_Ressource.RessGruppe   = bM_RessGruppe.RessGruppe
      bM_Ressource.Kostenstelle = 10
      bM_Ressource.Kalender     = '10'
      .
    
    validate bM_Ressource.
    
    /* create the resource language record                                      */
    
    create bM_RessourceSpr.
    
    assign
      bM_RessourceSpr.Firma     = gcUsedCompany
      bM_RessourceSpr.RessArt   = 7
      bM_RessourceSpr.Ressource = gcResourcePrefix
      bM_RessourceSpr.Sprache   = 'D':U
      .
    
    validate bM_RessourceSpr.  
  
  end. /* if not can-find(bM_Ressource ... */
  
  /* create the specified number of activities containing the new created     */
  /* resource                                                                 */
  /*--------------------------------------------------------------------------*/
  
  run writeLogInformation('Creating Activities...':U,'':U,yes,'':U).
  
  do i = 1 to giActivityCount
    on error undo, throw:
    
    run writeLogInformation(string(i),string(giActivityCount),no,'':U).
  
  &IF {&PA_VERSION} >= '7.1':U &THEN
    
    oPPAO = new pps.base.cls.PMCOperationPao(bPMM_Routing.PMM_Routing_Obj,
                                             0,
                                             i * 10,
                                             gcActivityPrefix + ' ' + string(i)).
    
    assign
      oPPAO:prpcCostItemGroup         = '70':U
      oPPAO:prpiBasic_ResourcCategory = 7
      oPPAO:prpcBasic_M_Ressource_ID  = gcResourcePrefix
      oPPAO:prplResourceGroup         = no
      oPPAO:prpcAlternateGroup        = '':U
      .
    
    cOperationObj = pps.base.cls.PMCOperationSvc:prpoInstance:cCreate(oPPAO).
         
    /* create the activity connection between the previously created activity */
    /* and the currently new created activity                                 */
    
    if i > 1 then
      pps.base.cls.PMCOperationRelationSvc:prpoInstance:cCreate
       (new pps.base.cls.PMCOperationRelationPao(cLastActivityObj,
                                                 cOperationObj,
                                                 bPMM_Routing.PMM_Routing_Obj)).
        
    cLastActivityObj = cOperationObj.
    
  &ELSE
  
    create bMA_Aktivitaet.
    
    assign
      bMA_Aktivitaet.Firma             = gcUsedCompany
      bMA_Aktivitaet.ProzessBereich    = 'PPA':U
      bMA_Aktivitaet.Prozess           = gcProcessPrefix
      bMA_Aktivitaet.IndexNr           = '':U
      bMA_Aktivitaet.AktPos            = i * 10
      bMA_Aktivitaet.AktArt            = 0
      bMA_Aktivitaet.Aktivitaet        = gcActivityPrefix + ' ' + string(i)
      .
    
    validate bMA_Aktivitaet.   
          
    assign
      bMA_Aktivitaet.KostenArtenGruppe = '20':U
      bMA_Aktivitaet.BasisRessArt      = 7
      bMA_Aktivitaet.BasisRessource    = gcResourcePrefix
      .
    
    validate bMA_Aktivitaet.
    
    /* assign the resource to the activity */
    
    create bMA_Ressource.
     
    assign
      bMA_Ressource.Firma    = gcUsedCompany
      bMA_Ressource.IdentAkt = bMA_Aktivitaet.IdentAkt
      bMA_Ressource.RessPos  = 0
      .    
    
    validate bMA_Ressource.
    
    assign
      bMA_Ressource.Ressource = gcResourcePrefix
      bMA_Ressource.RessArt   = 7
      .    
    
    /* create the activity connection between the previously created activity */
    /* and the currently new created activity                                 */
    
    if i > 1 then
    do:
      
      create bMA_AktAob.
      
      assign
        bMA_AktAob.Firma          = gcUsedCompany
        bMA_AktAob.IdentAktVon    = iLastActivityID
        bMA_AktAob.IdentAktNach   = bMA_Aktivitaet.IdentAkt
        bMA_AktAob.ProzessBereich = 'PPA':U
        bMA_AktAob.Prozess        = gcProcessPrefix      
        .
      
      validate bMA_AktAob.
        
    end. /* if i > 1 */
    
    iLastActivityID = bMA_Aktivitaet.IdentAkt.
  
  &ENDIF  
  
  end. /* do i = 1 to iActivityCount */

end. /* do transaction */

end procedure. /* createProcess */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createWorkOrders Dialog-Frame 
PROCEDURE createWorkOrders PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i                       as integer   no-undo.
define variable j                       as integer   no-undo.
define variable k                       as integer   no-undo.
define variable cWOObj                  as character no-undo.
define variable iWorkOrderCreationCount as integer   no-undo.
define variable tzLastStartTime         as int64     no-undo.
define variable tzOverallTime           as int64     no-undo.

define variable oWorkOrderCreation as class pps.prod.cls.PPCProdCreateWorkOrderSvo no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bttCreatedBaseStructures-Last for temp-table ttCreatedBaseStructures.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('Creating work orders...':U,'':U,yes,'':U). 
 
oWorkOrderCreation = new pps.prod.cls.PPCProdCreateWorkOrderSvo().

find last bttCreatedBaseStructures-Last.

if available bttCreatedBaseStructures-Last then
do:

  giWOProcessingCount = giWorkOrderCount * bttCreatedBaseStructures-Last.BaseStrucNumber.
  
  do i = 1 to giWorkOrderCount
    on error undo, throw:
      
    do j = 1 to bttCreatedBaseStructures-Last.BaseStrucNumber
      on error undo, throw:
      
      find ttCreatedBaseStructures
        where ttCreatedBaseStructures.BaseStrucNumber = j.
      
      k = k + 1.
  
      if k <> 1 then
        tzOverallTime = tzOverallTime + etime - tzLastStartTime.
      
      run writeLogInformation(string(k),
                              string(giWOProcessingCount),
                              no,
                              pa_cGetTimeFormattedString(int64((giWOProcessingCount - k + 1) * tzOverallTime / (k - 1)))).
      
      tzLastStartTime = etime.
      
      assign
        oWorkOrderCreation:prpcPart          = ttCreatedBaseStructures.BaseStructurePart
        oWorkOrderCreation:prpcPartVariant   = ''
        oWorkOrderCreation:prpdQuantity      = 10
        oWorkOrderCreation:prpiMRPArea       = 0
        oWorkOrderCreation:prptRequestedDate = today
        .
      
      if glUseSpecificProcess then
        oWorkOrderCreation:prpcProcess = gcProcessPrefix.
      
      cWOObj = oWorkOrderCreation:cCreateWorkOrder().
      
      create ttCreatedWOs.
      
      ttCreatedWOs.WorkOrderObj = cWOObj.
      
      validate ttCreatedWOs.
      
      oWorkOrderCreation:ResetObject().
    
    end. /* do j = 1 to giAssemblyCount */
    
  end. /* do i = 1 to iWorkOrderCount */

end. /* if available bttCreatedBaseStructures-Last */

finally:

  if valid-object(oWorkOrderCreation) then
    delete object oWorkOrderCreation.

end finally.

end procedure. /* createWorkOrders */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataEntryBOMLine Dialog-Frame 
PROCEDURE dataEntryBOMLine PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i as integer no-undo.
&IF {&PA_VERSION} < '6.2':U &THEN
define variable iTemp as integer no-undo.
&ENDIF
/* Buffers -------------------------------------------------------------------*/

define buffer bPP_Auftrag  for PP_Auftrag.
define buffer bPP_StkZeile for PP_StkZeile.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('perform BOM line withdrawals...':U,'':U,yes,'':U).

for each ttCreatedWOs
  use-index DataEntryNo
  transaction
  on error undo, throw:

  i = i + 1.
  
  run writeLogInformation(string(i),string(giWOProcessingCount),no,'':U).
  
  find bPP_Auftrag
    where bPP_Auftrag.PP_Auftrag_Obj = ttCreatedWOs.WorkOrderObj
    no-lock.
  
  find first bPP_StkZeile
    where bPP_StkZeile.Firma = bPP_Auftrag.Firma
      and bPP_StkZeile.RueckMeldeNr = bPP_Auftrag.RueckMeldeNr
    no-lock no-error.

&IF {&PA_VERSION} >= '6.2':U &THEN  
  if available bPP_StkZeile then
    pps.prod.cls.PPCMaterialBookingCoreSvc:prpoInstance:dataEntryBOMLine
     (bPP_StkZeile.PP_StkZeile_Obj,
      gdBOMDEQty,
      no,
      bPP_StkZeile.Lagerort,
      {&pa_ML_BS_Materialschein},
      today,
      no).  

&ELSE

  if available bPP_StkZeile then
    run pps/prod/proc/ppvrms00.p
         (       bPP_StkZeile.Firma,
                 rowid(bPP_StkZeile),
                 today,
                 ( - gdBOMDEQty),
                 bPP_StkZeile.Lagerort,
                 {&pa_ML_BS_Materialschein},
          output iTemp).

&ENDIF

end. /* for each ttCreatedWOs ... */

end procedure. /* dataEntryBOMLine */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataEntryOperation Dialog-Frame 
PROCEDURE dataEntryOperation PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i as integer no-undo.
&IF {&PA_VERSION} >= '7.1':U &THEN 
define variable oPostingObject as class pps.prod.cls.PPCDataEntryOperationPao no-undo.

&ELSE
define variable lLastRess as logical       no-undo.
&ENDIF

/* Buffers -------------------------------------------------------------------*/

define buffer bPP_Auftrag    for PP_Auftrag.
define buffer bMB_Aktivitaet for MB_Aktivitaet.

&IF {&PA_VERSION} < '7.1':U &THEN
define buffer bMB_Ressource for MB_Ressource.
&ENDIF

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('perform BOM line withdrawals...':U,'':U,yes,'':U).

for each ttCreatedWOs
  use-index DataEntryNo
  transaction
  on error undo, throw:

  i = i + 1.
  
  run writeLogInformation(string(i),string(giWOProcessingCount),no,'':U).
  
  find bPP_Auftrag
    where bPP_Auftrag.PP_Auftrag_Obj = ttCreatedWOs.WorkOrderObj
    no-lock.
  
  find first bMB_Aktivitaet
    where bMB_Aktivitaet.Firma          = bPP_Auftrag.Firma
      and bMB_Aktivitaet.ProzessBereich = 'PPA':U
      and bMB_Aktivitaet.Prozess        = bPP_Auftrag.Auftrag
      and bMB_Aktivitaet.Teilprozess    = bPP_Auftrag.RueckMeldeNr
    no-lock no-error.

  if available bMB_Aktivitaet then
  do:

&IF {&PA_VERSION} >= '7.1':U &THEN  
    
    if   gdTSTime       <> 0
      or gdTUTime       <> 0
      or gdOperationQty <> 0 then
    
      oPostingObject = new pps.prod.cls.PPCDataEntryOperationPao
                            (bMB_Aktivitaet.Teilprozess,
                             bMB_Aktivitaet.AktPos,
                             'tr').

    if gdTSTime <> 0 then
    do:
  
      assign
        oPostingObject:prptPostingDate        = today
        oPostingObject:prpcLaborCategory      = bMB_Aktivitaet.Lohnart
        oPostingObject:prpiEmployeeNo         = 0
        oPostingObject:prpcPostingOrigin      = 'MAN':U
        oPostingObject:prpcPostingReference   = '':U
        oPostingObject:prpcPostingText        = '':U
        oPostingObject:prpdPostingTime        = gdTSTime
        .
  
      pps.prod.cls.PPCOperationBookingCoreSvc:prpoInstance:dataEntryOperation
       (oPostingObject).  
    
    end.
    
    if   gdTUTime       <> 0
      or gdOperationQty <> 0 then    
    do:
      
      assign
        oPostingObject:prpcPostingType        = 'te':U
        oPostingObject:prptPostingDate        = today
        oPostingObject:prpcLaborCategory      = bMB_Aktivitaet.Lohnart
        oPostingObject:prpiEmployeeNo         = 0
        oPostingObject:prpcPostingOrigin      = 'MAN':U
        oPostingObject:prpcPostingReference   = '':U
        oPostingObject:prpcPostingText        = '':U
        oPostingObject:prpdPostingTime        = gdTUTime
        oPostingObject:prpdPostingQty         = gdOperationQty
        .
  
      pps.prod.cls.PPCOperationBookingCoreSvc:prpoInstance:dataEntryOperation
       (oPostingObject). 
    
    end.
    
&ELSE

    if gdTSTime <> 0 then
      for each bMB_Ressource
        where bMB_Ressource.Firma    = bMB_Aktivitaet.Firma
          and bMB_Ressource.IdentAkt = bMB_Aktivitaet.IdentAkt
        no-lock
        break by bMB_Ressource.RessPos
        on error undo, throw:
       
        run pps/prod/proc/ppvrrs00.p(bMB_Aktivitaet.Firma,
                                     rowid(bMB_Aktivitaet),
                                     today,
                                     'tr':U,
                                     bMB_Ressource.Ressource,
                                     bMB_Ressource.RessArt,
                                     bMB_Ressource.RessPos,
                                     bMB_Aktivitaet.Lohnart,
                                     'MAN':U,
                                     '':U,
                                     '':U,
                                     0,
                                     gdTSTime,
                                     bMB_Aktivitaet.soll_tr,
                                     bMB_Aktivitaet.soll_te
                                      / 10,
                                     0,
                                     0,
                                     last(bMB_Ressource.RessPos),           /* Mengenkorrekturen nur bei der letzten Ressource */
                                     0,            /* Wert bei Fremdarbeit */
                                     no,
                                     no,
                                     ?,                   /* Kostenstelle */
                                     1      /* MehrMaschinenFaktor pro Buchung */
        ).
  
      end.
    
    if   gdTUTime       <> 0
      or gdOperationQty <> 0 then    
      for each bMB_Ressource
        where bMB_Ressource.Firma    = bMB_Aktivitaet.Firma
          and bMB_Ressource.IdentAkt = bMB_Aktivitaet.IdentAkt
        no-lock
        break by bMB_Ressource.RessPos
        on error undo, throw:
       
        run pps/prod/proc/ppvrrs00.p(bMB_Aktivitaet.Firma,
                                     rowid(bMB_Aktivitaet),
                                     today,
                                     'te':U,
                                     bMB_Ressource.Ressource,
                                     bMB_Ressource.RessArt,
                                     bMB_Ressource.RessPos,
                                     bMB_Aktivitaet.Lohnart,
                                     'MAN':U,
                                     '':U,
                                     '':U,
                                     0,
                                     gdTUTime,
                                     bMB_Aktivitaet.soll_tr,
                                     bMB_Aktivitaet.soll_te
                                      / 10,
                                     gdOperationQty,
                                     0,
                                     last(bMB_Ressource.RessPos),           /* Mengenkorrekturen nur bei der letzten Ressource */
                                     0,            /* Wert bei Fremdarbeit */
                                     no,
                                     no,
                                     ?,                   /* Kostenstelle */
                                     1      /* MehrMaschinenFaktor pro Buchung */
        ).
  
      end.

&ENDIF

  end.
  
end. /* for each ttCreatedWOs ... */

end procedure. /* dataEntryOperation */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame 
PROCEDURE disable_UI :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* DISABLE the User Interface                                                 */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/* Here we clean-up the user-interface by deleting dynamic widgets we have    */
/* created and/or hide frames.  This procedure is usually called when we are  */
/* ready to "clean-up" after running                                          */
/*                                                                            */
/* Code is changed to be able to create new objects in versions prior to 5.1  */
/* that are compatible and do not need to be changed when a migration to 5.1  */
/* is made                                                                    */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

{adm/template/incl/dt_dlg06.if}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame 
PROCEDURE enable_UI :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* ENABLE the User Interface                                                  */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/* Here we display/view/enable the widgets in the user-interface.             */
/* In addition, OPEN all queries associated with each FRAME and BROWSE.       */
/* These statements here are based on the "Other Settings" section of the     */
/* widget Property Sheets.                                                    */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

{adm/template/incl/dt_dlg03.if}

end procedure. /* enable_UI */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize Dialog-Frame  adm/support/proc/ds_dlg02.p
PROCEDURE initialize :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* Initialization                                                             */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/* - Initialization of variables                                              */
/* - Assign format                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/* lExternalValueAvailable     Flag                                           */
/*----------------------------------------------------------------------------*/

define variable lExternalValueAvailable as logical       no-undo.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

/* Initialize maximum values (use external data, if available)                */



/* Copy setup from dummy frame                                                */

if frame {&FRAME-NAME}:hidden then
  assign
    
    no-error.

/* Default Code                                                               */

{adm/template/incl/dt_dlg01.if}

end procedure. /* initialize */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadAssemblyStructures Dialog-Frame 
PROCEDURE loadAssemblyStructures PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable iBaseStructureCount as integer no-undo.

/* Buffers -------------------------------------------------------------------*/

&IF {&PA_VERSION} >= '7.1':U &THEN
define buffer bPMM_BOMHead for PMM_BOMHead.
&ELSE
define buffer bP_StkKopf for P_StkKopf.
&ENDIF

define buffer bttCreatedBaseStructures for temp-table ttCreatedBaseStructures.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('load BOM structures (' + gcAssemblyPrefix + ')...':U ,'':U,yes,'':U).

&IF {&PA_VERSION} >= '7.1':U &THEN

for each bPMM_BOMHead
  where bPMM_BOMHead.Company     = gcUsedCompany
    and bPMM_BOMHead.bomtype     = 'production':U
    and bPMM_BOMHead.Part begins gcAssemblyPrefix
    and not can-find(first PMM_BOMLine
                       where PMM_BOMLine.Company = {firma/pps.fir pACConnectionSvc:prpcCompany}
                         and PMM_BOMLine.Part    = bPMM_BOMHead.Part)
  no-lock
  on error undo, throw:

  iBaseStructureCount = iBaseStructureCount + 1.
  
  create bttCreatedBaseStructures.
  
  assign
    bttCreatedBaseStructures.BaseStructurePart = bPMM_BOMHead.Part
    bttCreatedBaseStructures.BaseStrucNumber   = iBaseStructureCount
    .
  
  validate bttCreatedBaseStructures. 

end. /* for each bPMM_BOMHead ... */

&ELSE

for each bP_StkKopf
  where bP_StkKopf.Firma   = {firma/pps.fir pACConnectionSvc:prpcCompany}
    and bP_StkKopf.Art     = 'SSt':U
    and bP_StkKopf.Artikel begins gcAssemblyPrefix
    and not can-find(first P_StkZeile
                       where P_StkZeile.Firma   = {firma/pps.fir pACConnectionSvc:prpcCompany}
                         and P_StkZeile.Artikel = bP_StkKopf.Artikel)
  no-lock
  on error undo, throw:

  iBaseStructureCount = iBaseStructureCount + 1.
  
  create bttCreatedBaseStructures.
  
  assign
    bttCreatedBaseStructures.BaseStructurePart = bP_StkKopf.Artikel
    bttCreatedBaseStructures.BaseStrucNumber   = iBaseStructureCount
    .
  
  validate bttCreatedBaseStructures. 

end. /* for each bP_StkKopf ... */

&ENDIF

end procedure. /* loadAssemblyStructures */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadWorkOrders Dialog-Frame 
PROCEDURE loadWorkOrders PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i as integer no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bPP_Auftrag for PP_Auftrag.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

giWOProcessingCount = 0.

empty temp-table ttCreatedWOs.

for each bPP_Auftrag
  where bPP_Auftrag.Firma        =  {firma/ppauftra.fir pACConnectionSvc:prpcCompany}
    and bPP_Auftrag.RueckmeldeNr >= giStartingDENo
  use-index RueckmeldeNr
  no-lock
  on error undo, throw:
  
  giWOProcessingCount = giWOProcessingCount + 1.
  
  create ttCreatedWOs.
  
  assign
    ttCreatedWOs.WorkOrderObj = bPP_Auftrag.PP_Auftrag_Obj
    ttCreatedWOs.DataEntryNo  = bPP_Auftrag.RueckMeldeNr
    .
    
  validate ttCreatedWOs.
  
end. /* for each bPP_Auftrag ... */

end procedure. /* loadWorkOrders */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-data Dialog-Frame  adm/support/proc/ds_dlg04.p
PROCEDURE process-data :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* Save data into parameter string and start processing                       */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/* c_ParamListe        Parameter string                                       */
/* c_Options           Options of target program                              */
/* c_ZielProgramm      Target Program                                         */
/*----------------------------------------------------------------------------*/

define variable c_ParamListe   as character     no-undo.
define variable c_Options      as character     no-undo.
define variable c_ZielProgramm as character     no-undo.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run assignData in this-procedure (output c_ParamListe).

{adm/template/incl/dt_dlg02.if}

end procedure. /* process-data */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE releaseWOs Dialog-Frame 
PROCEDURE releaseWOs PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i as integer no-undo.

define variable tzLastStartTime     as int64     no-undo.
define variable tzOverallTime       as int64     no-undo.

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('Releasing work orders...':U,'':U,yes,'':U).

for each ttCreatedWOs
  use-index DataEntryNo
  transaction
  on error undo, throw:
  
  i = i + 1.
  
  if i <> 1 then
    tzOverallTime = tzOverallTime + etime - tzLastStartTime.
  
  run writeLogInformation(string(i),
                          string(giWOProcessingCount),
                          no,
                          pa_cGetTimeFormattedString(int64((giWOProcessingCount - i + 1) * tzOverallTime / (i - 1)))).

  tzLastStartTime = etime.
  
  pps.prod.cls.PPCProdWorkOrderSvc:prpoInstance:setStatus(ttCreatedWOs.WorkOrderObj,
                                                          'F':U,
                                                          yes).

end. /* for each ttCreatedWOs ... */

end procedure. /* releaseWOs */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scheduleWOs Dialog-Frame 
PROCEDURE scheduleWOs PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable i as integer no-undo.

define variable tzLastStartTime     as int64     no-undo.
define variable tzOverallTime       as int64     no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bttCreatedWOs for temp-table ttCreatedWOs.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

run writeLogInformation('Scheduling work orders...':U,'':U,yes,'':U).

for each bttCreatedWOs
  use-index DataEntryNo
  transaction
  on error undo, throw:
  
  i = i + 1.

  if i <> 1 then
    tzOverallTime = tzOverallTime + etime - tzLastStartTime.
  
  run writeLogInformation(string(i),
                          string(giWOProcessingCount),
                          no,
                          pa_cGetTimeFormattedString(int64((giWOProcessingCount - i + 1) * tzOverallTime / (i - 1)))).

  tzLastStartTime = etime.
  
  if can-find(PP_Auftrag
                where PP_Auftrag.PP_Auftrag_Obj =  bttCreatedWOs.WorkOrderObj
                  and PP_Auftrag.Auftragsstatus = 'B':U) then
    pps.prod.cls.PPCProdWorkOrderSvc:prpoInstance:scheduleRoughStructure(bttCreatedWOs.WorkOrderObj).

end. /* for each ttCreatedWOs ... */

end procedure. /* scheduleWOs */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateEnabledStatus Dialog-Frame 
PROCEDURE updateEnabledStatus PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable lCreateAssemblies as logical no-undo.
define variable lCreateWO         as logical no-undo.
define variable lAssemblyDepth    as logical no-undo.
define variable lAssemblyPrefix   as logical no-undo.
define variable lRandomAssembly   as logical no-undo.
define variable lStructureType    as logical no-undo.
define variable lLoadWO           as logical no-undo.
define variable lSchedule         as logical no-undo.
define variable lRelease          as logical no-undo.
define variable lDataEntry        as logical no-undo.
define variable lDataEntryOpe     as logical no-undo.
define variable lBookingQty       as logical no-undo.
define variable lBookingOpeQty    as logical no-undo.
define variable lCreateOnHand     as logical no-undo.

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

if giCreateAssemblies:screen-value in frame {&FRAME-NAME} = '{&K_AssemblyCreationType_Create}' then
  assign
    lCreateAssemblies = yes
    lStructureType    = yes
    lAssemblyPrefix   = yes
    lRandomAssembly   = not giStructureType:screen-value in frame {&FRAME-NAME} = '{&K_StructureType_Separate}'
    lAssemblyDepth    = giStructureType:screen-value in frame {&FRAME-NAME} <> '{&K_StructureType_Separate}':U
    .
else if giCreateAssemblies:screen-value in frame {&FRAME-NAME} = '{&K_StructureType_Structural}' then
  lAssemblyPrefix = yes.

lCreateOnHand = giCreateAssemblies:screen-value in frame {&FRAME-NAME} <> '{&K_StructureType_Single}'.

if integer(giWorkOrderProcessing:screen-value in frame {&FRAME-NAME}) > 1 then
do:
  
  assign
    lCreateWO = giWorkOrderProcessing:screen-value in frame {&FRAME-NAME} = '2':U
    lLoadWO   = not lCreateWO 
    lSchedule = yes
    .
  
  if glSchedulePWO:checked in frame {&FRAME-NAME} then
  do:
    
    lRelease = yes.
    
    if glReleaseWO:checked in frame {&FRAME-NAME} then
    do:
      
      lDataEntry = yes.
      
      if glDataEntryBOM:checked in frame {&FRAME-NAME} then
        lBookingQty = yes.

      if glDEOperation:checked in frame {&FRAME-NAME} then
        lBookingOpeQty = yes.
      
    end. /* if glReleaseWO:checked in frame {&FRAME-NAME */


    
  end. /* if glSchedulePWO:checked in frame {&FRAME-NAME} */

end. /* if glCreateWorkOrders:checked in frame {&FRAME-NAME} */


if lCreateOnHand then
  {fnarg
    pa_lUISvcEnableWidget
     "giAssemblyCount:handle in frame {&Frame-Name}"}.
else
  {fnarg
    pa_lUISvcDisableWidget
     "giAssemblyCount:handle in frame {&Frame-Name}"}. 

if lCreateAssemblies then
do:
  
  {fnarg
    pa_lUISvcEnableWidget
     "giAssemblyCount:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcEnableWidget
     "giPurchasePartCount:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcEnableWidget
     "gcLinePrefix:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcEnableWidget
     "glRandomDepth:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcEnableWidget
     "giPartType:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcEnableWidget
     "btnCreateBOMStructure:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcEnableWidget
     "btnRemoveBOMStructure:handle in frame {&Frame-Name}"}. 

end.
else
do:

  {fnarg
    pa_lUISvcDisableWidget
     "giAssemblyCount:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcDisableWidget
     "giPurchasePartCount:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcDisableWidget
     "gcAssemblyPrefix:handle in frame {&Frame-Name}"}. 
  {fnarg
    pa_lUISvcDisableWidget
     "gcLinePrefix:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcDisableWidget
     "glRandomDepth:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcDisableWidget
     "giOrderMaxDepth:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcDisableWidget
     "giPartType:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcDisableWidget
     "btnCreateBOMStructure:handle in frame {&Frame-Name}"}. 

  {fnarg
    pa_lUISvcDisableWidget
     "btnRemoveBOMStructure:handle in frame {&Frame-Name}"}.

end.


if lAssemblyPrefix then
  {fnarg
    pa_lUISvcEnableWidget
     "gcAssemblyPrefix:handle in frame {&Frame-Name}"}.
else
  {fnarg
    pa_lUISvcDisableWidget
     "gcAssemblyPrefix:handle in frame {&Frame-Name}"}. 

 
if lStructureType then
  {fnarg
    pa_lUISvcEnableWidget
     "giStructureType:handle in frame {&Frame-Name}"}. 
else
  {fnarg
    pa_lUISvcDisableWidget
     "giStructureType:handle in frame {&Frame-Name}"}. 


if lRandomAssembly then 
  {fnarg
    pa_lUISvcEnableWidget
     "glRandomDepth:handle in frame {&Frame-Name}"}. 
else
do:
  
  glRandomDepth:checked in frame {&FRAME-NAME} = no.
  
  {fnarg
    pa_lUISvcDisableWidget
     "glRandomDepth:handle in frame {&Frame-Name}"}. 

end.


if lAssemblyDepth then 
  {fnarg
    pa_lUISvcEnableWidget
     "giOrderMaxDepth:handle in frame {&Frame-Name}"}. 
else
do:

  giOrderMaxDepth:screen-value in frame {&FRAME-NAME} = '1':U.
  
  {fnarg
    pa_lUISvcDisableWidget
     "giOrderMaxDepth:handle in frame {&Frame-Name}"}. 

end.

if not lLoadWO then
  {fnarg
    pa_lUISvcDisableWidget
     "giStartingDENo:handle in frame {&Frame-Name}"}. 
else
  {fnarg
    pa_lUISvcEnableWidget
     "giStartingDENo:handle in frame {&Frame-Name}"}. 

if not lCreateWO then
do:
  
  giWorkOrderCount:screen-value in frame {&FRAME-NAME} = '0':U.
  
  {fnarg
    pa_lUISvcDisableWidget
     "giWorkOrderCount:handle in frame {&Frame-Name}"}.
     
end. 
else
  {fnarg
    pa_lUISvcEnableWidget
     "giWorkOrderCount:handle in frame {&Frame-Name}"}. 

if not lSchedule then
do:
  
  assign
    glSchedulePWO                                = no
    glSchedulePWO:checked in frame {&FRAME-NAME} = no 
    .
    
  {fnarg
    pa_lUISvcDisableWidget
     "glSchedulePWO:handle in frame {&Frame-Name}"}.  

end.
else
  {fnarg
    pa_lUISvcEnableWidget
     "glSchedulePWO:handle in frame {&Frame-Name}"}.  


if not lRelease then
do:
  
  assign
    glReleaseWO                                = no
    glReleaseWO:checked in frame {&FRAME-NAME} = no 
    .
    
  {fnarg
    pa_lUISvcDisableWidget
     "glReleaseWO:handle in frame {&Frame-Name}"}.
     
end.  
else
  {fnarg
    pa_lUISvcEnableWidget
     "glReleaseWO:handle in frame {&Frame-Name}"}.  


if not lDataEntry then
do:
  
  assign
    glDataEntryBOM                                = no
    glDataEntryBOM:checked in frame {&FRAME-NAME} = no
    glDEOperation                                 = no
    glDEOperation:checked in frame {&FRAME-NAME}  = no
     
    .
    
  {fnarg
    pa_lUISvcDisableWidget
     "glDataEntryBOM:handle in frame {&Frame-Name}"}.  

  {fnarg
    pa_lUISvcDisableWidget
     "glDEOperation:handle in frame {&Frame-Name}"}.      

end.
else
do:
  
  {fnarg
    pa_lUISvcEnableWidget
     "glDataEntryBOM:handle in frame {&Frame-Name}"}.  

  {fnarg
    pa_lUISvcEnableWidget
     "glDEOperation:handle in frame {&Frame-Name}"}. 

end.

if not lBookingQty then
do:
  
  assign
    gdBOMDEQty                                     = 0
    gdBOMDEQty:screen-value in frame {&FRAME-NAME} = '0':U
    .
  
  {fnarg
    pa_lUISvcDisableWidget
     "gdBOMDEQty:handle in frame {&Frame-Name}"}.  
     
end.
else
  {fnarg
    pa_lUISvcEnableWidget
     "gdBOMDEQty:handle in frame {&Frame-Name}"}.  


if not lBookingOpeQty then
do:
  
  assign
    gdTSTime
    gdTSTime:screen-value in frame {&FRAME-NAME} = '0':U
    gdTUTime                                     = 0
    gdTUTime:screen-value in frame {&FRAME-NAME} = '0':U
    gdOperationQty                                     = 0
    gdOperationQty:screen-value in frame {&FRAME-NAME} = '0':U
    .
  
  {fnarg
    pa_lUISvcDisableWidget
     "gdTSTime:handle in frame {&Frame-Name}"}.
  {fnarg
    pa_lUISvcDisableWidget
     "gdTUTime:handle in frame {&Frame-Name}"}.
  {fnarg
    pa_lUISvcDisableWidget
     "gdOperationQty:handle in frame {&Frame-Name}"}.

     
end.
else
do:
  
  {fnarg
    pa_lUISvcEnableWidget
     "gdTSTime:handle in frame {&Frame-Name}"}.
  {fnarg
    pa_lUISvcEnableWidget
     "gdTUTime:handle in frame {&Frame-Name}"}.
  {fnarg
    pa_lUISvcEnableWidget
     "gdOperationQty:handle in frame {&Frame-Name}"}.

end.

if glWriteLogfile:checked in frame {&FRAME-NAME} then
do:
  
  gcLogFile:screen-value in frame {&FRAME-NAME} = 'c:\HOS.log':U.
  
  {fnarg
    pa_lUISvcEnableWidget
     "gcLogFile:handle in frame {&Frame-Name}"}.
     
end.  
else
do:
  
  gcLogFile:screen-value in frame {&FRAME-NAME} = '':U.
  
  {fnarg
    pa_lUISvcDisableWidget
     "gcLogFile:handle in frame {&Frame-Name}"}.  

end.  


end procedure. /* updateEnabledStatus */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeLogInformation Dialog-Frame 
PROCEDURE writeLogInformation PRIVATE :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

define input parameter pcLogText1    as character no-undo.
define input parameter pcLogText2    as character no-undo.
define input parameter plWriteHeader as logical   no-undo.
define input parameter pcETA         as character no-undo.

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable cLineString as character no-undo.

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

if plWriteHeader then
do:
  
  if glUpdateScreenLog then
    gcStatusLine:screen-value in frame {&FRAME-NAME} = pcLogText1.
  
  if glWriteLogFile then
    goUtilClass:writeHeader(pcLogText1).

end. /* if plWriteHeader */
else
do:
  
  if glUpdateScreenLog then
    assign
      gdCurrentCount:screen-value in frame {&FRAME-NAME} = pcLogText1
      gdMaxCount:screen-value in frame {&FRAME-NAME}     = pcLogText2
      .
  
  if glWriteLogFile then
  do:
    
    cLineString = pcLogText1 
                           + ' / ' 
                           + pcLogText2 
                           + ' (':U 
                   + trim(string(integer(pcLogText1) / integer(pcLogText2) * 100,'zz9.99':U)). 
    if pcETA <> '':U then
      cLineString = cLineString  
                     + '% - ETA: ':U
                     + pcETA
                     + ')':U.  
    else
      cLineString = cLineString + '%)':U.
    
    goUtilClass:writeLine(cLineString).
  
  end. /* if glWriteLogFile */
  
end. /* else if plWriteHeader */

end procedure. /* writeLogInformation */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pa_cGetTimeFormattedString Dialog-Frame 
FUNCTION pa_cGetTimeFormattedString returns character private
  ( pi64Time as int64 ):
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable cTimeString as character no-undo.

define variable iSeconds as integer       no-undo.
define variable iMinutes as integer       no-undo.
define variable iHours   as integer       no-undo.

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/                

if pi64Time = 0  then
  cTimeString = '':U.

if pi64Time < 1000 then
  cTimeString = string(pi64Time,'zzz9':U) + 'ms':U.

if    pi64Time <  60000 
  and pi64Time >= 1000 then
  cTimeString = string(pi64Time / 1000,'z9':U) + 's':U.

if    pi64Time <  3600000 
  and pi64Time >= 60000 then
  assign
    iMinutes    = truncate(pi64Time / 60000,0)
    iSeconds    = truncate((pi64Time - (iMinutes * 60000)) / 1000,0)
    cTimeString = string(iMinutes,'z9':U) + 'm ':U
    cTimeString = cTimeString 
                    + string(iSeconds,'z9':U) 
                    + 's':U 
    .

if    pi64Time <  86400000 
  and pi64Time >= 3600000 then
  assign
    iHours      = truncate(pi64Time / 3600000,0)
    iMinutes    = truncate((pi64Time - (iHours * 3600000)) / 60000,0)
    iSeconds    = truncate((pi64Time - (iMinutes * 60000) - (iHours * 3600000)) / 1000,0)
    cTimeString = string(iHours,'z9':U) + 'h ':U
    cTimeString = cTimeString
                    + string(iMinutes,'z9':U) 
                    + 'm ':U
    cTimeString = cTimeString 
                    + string(iSeconds,'z9':U) 
                    + 's':U 
    .
  
return cTimeString.

end function. /* pa_cGetTimeFormattedString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pa_cGetTimeSummaryString Dialog-Frame 
FUNCTION pa_cGetTimeSummaryString returns character private
  ( pcStep            as character,
    piTotalStepTime   as int64,
    piTotalTime       as int64,
    piObjectCountType as integer ):
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable cTimeString as character     no-undo.

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/                

cTimeString = pcStep
               + trim(string(piTotalStepTime / 1000,'>>>,>>>,>>9.999':U)) 
               + '/':U.

case piObjectCountType:

  when {&K_ObjectCountType_WO} then
    cTimeString = cTimeString 
                   + trim(string(piTotalStepTime 
                                  / giWOProcessingCount
                                  / 1000,'>>>,>>>,>>9.999':U)).

  when {&K_ObjectCountType_BOM} then
    cTimeString = cTimeString 
                   + trim(string(piTotalStepTime 
                                  / giAssemblyCount
                                  / 1000,'>>>,>>>,>>9.999':U)).

  when {&K_ObjectCountType_OnHand} then
    cTimeString = cTimeString 
                   + trim(string(piTotalStepTime 
                                  / (giAssemblyCount * giPurchasePartCount)
                                  / 1000,'>>>,>>>,>>9.999':U)).

  when {&K_ObjectCountType_Activity} then
    cTimeString = cTimeString 
                   + trim(string(piTotalStepTime 
                                  / giActivityCount
                                  / 1000,'>>>,>>>,>>9.999':U)).


end case.

cTimeString = cTimeString
               + '(' 
               + trim(string(piTotalTime / 1000,'>>>,>>>,>>9.999')) 
               + ')'.

return cTimeString.

end function. /* pa_cGetTimeSummaryString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pa_iGetRandomAssemblyNo Dialog-Frame 
FUNCTION pa_iGetRandomAssemblyNo returns integer
  ( iCurrentAssemblyPosition as integer):
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Notes ---------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* Parameters ----------------------------------------------------------------*/
/*                                                                            */
/* <none>                                                                     */
/*                                                                            */
/* Examples ------------------------------------------------------------------*/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* Variables -----------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

define variable oRandom as System.Random.

define variable iRandomNo as integer no-undo.

/* Buffers -------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/                

oRandom = new System.Random().

iRandomNo = oRandom:Next(1,iCurrentAssemblyPosition).

delete object oRandom.

return iRandomNo.

end function. /* pa_iGetRandomAssemblyNo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

