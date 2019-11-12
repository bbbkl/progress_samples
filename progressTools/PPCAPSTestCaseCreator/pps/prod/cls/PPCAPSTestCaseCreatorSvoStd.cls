&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r2 GUI
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS V-table-Win

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Update Information" Procedure _INLINE
/* Actions: ? ? ? ? adm/support/proc/ds_pa_01.w */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
/******************************************************************************/
/*                                 (c) 2017 proALPHA Business Solutions GmbH  */
/*                                          Auf dem Immel 8                   */
/*                                          67685 Weilerbach                  */
/* Project: proALPHA                                                          */
/*                                                                            */
/* Name   : PPCAPSTestCaseCreatorSvoStd.cls                                   */
/* Product: PPS - Produktionsplanung & -steuerung                             */
/* Module : PROD - Produktion                                                 */
/*                                                                            */
/* Created: 7.1b01 as of 30.01.2017/Jens Leoff                                */
/* Current: 7.1b01 as of 30.01.2017/Jens Leoff                                */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* DESCRIPTION                                                                */
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Standard Class for PPCAPSTestCaseCreatorSvo.cls                            */
/*
define variable oTCCreator as
    class pps.prod.cls.PPCAPSTestCaseCreatorSvo no-undo.
 oTCCreator = new pps.prod.cls.PPCAPSTestCaseCreatorSvo().

// prepare parts
//                          PART    TA    WBZ  MWBZ   DispH  TempEx  stock
oTCCreator:preparePart('5000052',    6,    10,    0,     90,   true,     0).

// creation instructions, use CAPITAL work order names!
//                              NAME         PART   Qty    Due
oTCCreator:createWorkOrder(  'VERB1',   '5000042',    5,    15).
oTCCreator:createWorkOrder(  'VERB2',   '5000042',    5,    40).
oTCCreator:createWorkOrder('DECKER1',   '5000052',    5,    30).
oTCCreator:createWorkOrder('DECKER2',   '5000052',    5,    70).
oTCCreator:createWorkOrder('DECKER3',   '5000052',    5,    150).
oTCCreator:createWorkOrder('DECKER4',   '5000052',    5,    200).

// return string for stripping with message_file_reader.py
message
 oTCCreator:prpcWorkOrderNumbers skip
 view-as alert-box
 title program-name(1).

if valid-object(oTCCreator) then
  delete object oTCCreator.
                                                                              */
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
/* Date       SC  Description                                                 */
/*----------------------------------------------------------------------------*/
/* 30.01.2017 jl  Version 7.1b01/7.1b011  Task PP-U-APS_-082                  */
/*                APSTestCaseCreator                                          */
/******************************************************************************/

/* Procedure Information -----------------------------------------------------*/

&GLOBAL-DEFINE pa-Autor             Jens Leoff
&GLOBAL-DEFINE pa-Version           7.1b011
&GLOBAL-DEFINE pa-Datum             30.01.2017
&GLOBAL-DEFINE pa-Letzter           Jens Leoff

&GLOBAL-DEFINE pa-GenVersion       OEA
&GLOBAL-DEFINE pa-ProgrammTyp      Class
&GLOBAL-DEFINE pa-Template         paooablstdclass.clsjet
&GLOBAL-DEFINE pa-TemplateVersion  1.00
&GLOBAL-DEFINE pa-XBasisName       PPCAPSTestCaseCreatorSvoStd_cls
&GLOBAL-DEFINE pa-NameSpace        1.00

/*----------------------------------------------------------------------------*/
/* Definitions (Preprocessor only)                                            */
/*----------------------------------------------------------------------------*/

/* Globals -------------------------------------------------------------------*/

{adm/template/incl/dt_cls00.df}

/* SCOPEDs -------------------------------------------------------------------*/

&SCOPED-DEFINE TimeUnitCalendarDays '4':U

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure


/* ***************************  Main Block  *************************** */

/*----------------------------------------------------------------------------*/
/* Class Definition                                                           */
/*----------------------------------------------------------------------------*/

/* Imports                                                                    */

using Progress.Lang.*.

/*----------------------------------------------------------------------------*/
/* Class Definition                                                           */
/*----------------------------------------------------------------------------*/

class pps.prod.cls.PPCAPSTestCaseCreatorSvoStd
  :



  define public property prpiMRPArea as integer initial 0 no-undo
  get. set.


  define public property prpcVariant as character no-undo
  get. set.


  define public property prpcWorkOrderNumbers as character no-undo
  get. set.


  define public property prpiStorageArea as integer initial 50 no-undo
  get. set.


  /*--------------------------------------------------------------------------*/
  /* Definitions                                                              */
  /*--------------------------------------------------------------------------*/

  /* Datasets ----------------------------------------------------------------*/

  /* Temp-Tables -------------------------------------------------------------*/

  /* Buffers -----------------------------------------------------------------*/

  /* Variables ---------------------------------------------------------------*/

  /*--------------------------------------------------------------------------*/
  /* Properties                                                               */
  /*--------------------------------------------------------------------------*/

  /*--------------------------------------------------------------------------*/
  /* Constructors                                                             */
  /*--------------------------------------------------------------------------*/

  /*--------------------------------------------------------------------------*/
  /* Destructors                                                              */
  /*--------------------------------------------------------------------------*/

  /*--------------------------------------------------------------------------*/
  /* Methods                                                                  */
  /*--------------------------------------------------------------------------*/

  method public void preparePart
    (pcPart                 as character,
     piPartType             as integer,
     piReplenishmentTime    as integer,
     piMinReplenishmentTime as integer,
     piMRPHorizon           as integer,
     plExplodeTemporarily   as logical,
     pdStock                as decimal ):
    /* Description -----------------------------------------------------------*/
    /*                                                                        */
    /*                                                                        */
    /*                                                                        */
    /* Notes -----------------------------------------------------------------*/
    /*                                                                        */
    /*                                                                        */
    /*                                                                        */
    /* Parameters ------------------------------------------------------------*/
    /*                                                                        */
    /* <none>                                                                 */
    /*                                                                        */
    /* Examples --------------------------------------------------------------*/
    /*                                                                        */
    /*                                                                        */
    /*                                                                        */
    /*------------------------------------------------------------------------*/
    /* Variables -------------------------------------------------------------*/

    /*------------------------------------------------------------------------*/

    /* Buffers ---------------------------------------------------------------*/
    define buffer bS_Artikel  for S_Artikel.
    define buffer bMD_Artikel for MD_Artikel.
    define buffer bML_Ort     for ML_Ort.
    define buffer bMLA_OnHand for MLA_OnHand.
    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/

    // adjust Part
    find bS_Artikel
      where bS_Artikel.Firma   = {firma/sartikel.fir pACConnectionSvc:prpcCompany}
        and bS_Artikel.Artikel = pcPart
      exclusive-lock no-wait no-error.

    if not available bS_Artikel then
      return.

    assign
      bS_Artikel.ArtikelArt     = piPartType
      bS_Artikel.ZeiteinheitWBZ = {&TimeUnitCalendarDays}
      bS_Artikel.WBZFaktor      = piMinReplenishmentTime
      bS_Artikel.WBZ            = piReplenishmentTime
      .
    release bS_Artikel.

    // adjust MRP parameters
    find bMD_Artikel
      where bMD_Artikel.Firma       = {firma/mlartort.fir pACConnectionSvc:prpcCompany}
        and bMD_Artikel.Artikel     = pcPart
        and bMD_Artikel.Lagergruppe = prpiMRPArea
      exclusive-lock no-wait no-error.

    if not available bMD_Artikel then
      return.

    assign
      bMD_Artikel.HorizontEinheit = {&TimeUnitCalendarDays}
      bMD_Artikel.Horizont        = piMRPHorizon
      bMD_Artikel.MRPRelevant     = plExplodeTemporarily
      .
    release bMD_Artikel.

    // set desired stock quantity
    find first bMLA_OnHand
      where bMLA_OnHand.Company     = {firma/dt_fir00.fir pACConnectionSvc:prpcCompany}
        and bMLA_OnHand.Part        = pcPart
        and bMLA_OnHand.StorageArea = prpiStorageArea
      exclusive-lock no-wait no-error.

    if not available bMLA_OnHand then
      return.

    bMLA_OnHand.OnHand = pdStock.
    release bMLA_OnHand.


  end method. /* preparePart */


  method public void createWorkOrder
    (pcWorkOrderNumber     as character,
     pcPart                as character,
     piQuantity            as integer,
     piRequestedDateOffset as integer):
    /* Description -----------------------------------------------------------*/
    /*                                                                        */
    /*                                                                        */
    /*                                                                        */
    /* Notes -----------------------------------------------------------------*/
    /*                                                                        */
    /*                                                                        */
    /*                                                                        */
    /* Parameters ------------------------------------------------------------*/
    /*                                                                        */
    /* <none>                                                                 */
    /*                                                                        */
    /* Examples --------------------------------------------------------------*/
    /*                                                                        */
    /*                                                                        */
    /*                                                                        */
    /*------------------------------------------------------------------------*/
    /* Variables -------------------------------------------------------------*/
    define variable oWorkOrderCreation as class pps.prod.cls.PPCProdCreateWorkOrderSvo no-undo.
    define variable cWOObj as character     no-undo.
    /*------------------------------------------------------------------------*/

    /* Buffers ---------------------------------------------------------------*/

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/

    oWorkOrderCreation = new pps.prod.cls.PPCProdCreateWorkOrderSvo().

    assign
      oWorkOrderCreation:prpcOrderNumber   = pcWorkOrderNumber
      oWorkOrderCreation:prpcPart          = pcPart
      oWorkOrderCreation:prpcPartVariant   = prpcVariant
      oWorkOrderCreation:prpdQuantity      = piQuantity
      oWorkOrderCreation:prpiMRPArea       = prpiMRPArea
      oWorkOrderCreation:prptRequestedDate = today + piRequestedDateOffset
      .

    cWOObj = oWorkOrderCreation:cCreateWorkOrder().

    if prpcWorkOrderNumbers <> '':U then
      prpcWorkOrderNumbers = prpcWorkOrderNumbers + ',':U.

    prpcWorkOrderNumbers = prpcWorkOrderNumbers + pcWorkOrderNumber.

    finally:

      if valid-object(oWorkOrderCreation) then
        delete object oWorkOrderCreation.
    end.

  end method. /* createWorkOrder */



end class. /* pps.prod.cls.PPCAPSTestCaseCreatorSvoStd */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

