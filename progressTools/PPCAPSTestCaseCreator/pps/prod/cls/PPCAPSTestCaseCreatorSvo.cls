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
/* Name   : PPCAPSTestCaseCreatorSvo.cls                                      */
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
/* create testing data for APS.                                               */
/* Never use this class outside of a test environment!                        */
/*                                                                            */
/* usage example:                                                             */
/*  // set ACM Parameters.. does not work for company parameters..            */
/*  //adm.config.cls.DCCAppConfigSvc:prpoInstance:setParameterValue           */
/*  //  ('PP_DynamicReplenishmentTime':U, 4).                                 */
/*  //adm.config.cls.DCCAppConfigSvc:prpoInstance:setParameterValue           */
/*  //  ('PP_OptPartRLTDeterminationType':U, 'DeterminedRLT':U).              */
/*  //adm.config.cls.DCCAppConfigSvc:prpoInstance:                            */
/*  //  setParameterValue('PP_AdvanceCoverages':U, 1).                        */
/*                                                                            */
/* define variable oTCCreator as                                              */
/*    class pps.prod.cls.PPCAPSTestCaseCreatorSvo no-undo.                    */
/* oTCCreator = new pps.prod.cls.PPCAPSTestCaseCreatorSvo().                  */
/*                                                                            */
/* prepare parts                                                              */
/* //                          PART    TA    WBZ  MWBZ   DispH  TempEx  stock */
/* oTCCreator:preparePart('5000052',    6,    10,    0,     90,   true,     0).*/
/*                                                                            */
/* // creation instructions, use CAPITAL work order names!                    */
/* //                              NAME         PART   Qty    Due             */
/* oTCCreator:createWorkOrder(  'VERB1',   '5000042',    5,    15).           */
/* oTCCreator:createWorkOrder(  'VERB2',   '5000042',    5,    40).           */
/* oTCCreator:createWorkOrder('DECKER1',   '5000052',    5,    30).           */
/* oTCCreator:createWorkOrder('DECKER2',   '5000052',    5,    70).           */
/* oTCCreator:createWorkOrder('DECKER3',   '5000052',    5,    150).          */
/* oTCCreator:createWorkOrder('DECKER4',   '5000052',    5,    200).          */
/*                                                                            */
/* // return string for stripping with message_file_reader.py                 */
/* message                                                                    */
/*  oTCCreator:prpcWorkOrderNumbers skip                                      */
/*  view-as alert-box                                                         */
/*  title program-name(1).                                                    */
/*                                                                            */
/* if valid-object(oTCCreator) then                                           */
/*   delete object oTCCreator.                                                */
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
&GLOBAL-DEFINE pa-Template         paooablclass.clsjet
&GLOBAL-DEFINE pa-TemplateVersion  1.00
&GLOBAL-DEFINE pa-XBasisName       PPCAPSTestCaseCreatorSvo_cls
&GLOBAL-DEFINE pa-NameSpace        1.00

/*----------------------------------------------------------------------------*/
/* Definitions (Preprocessor only)                                            */
/*----------------------------------------------------------------------------*/

/* Globals -------------------------------------------------------------------*/

{adm/template/incl/dt_cls00.df}

/* SCOPEDs -------------------------------------------------------------------*/


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

class pps.prod.cls.PPCAPSTestCaseCreatorSvo
  inherits
    &IF DEFINED(PPCAPSTestCaseCreatorSvo_cls_Y_INHERITS) = 1 &THEN
      {&PPCAPSTestCaseCreatorSvo_cls_Y_INHERITS}
    &ELSEIF DEFINED(PPCAPSTestCaseCreatorSvo_cls_INHERITS) = 1 &THEN
      {&PPCAPSTestCaseCreatorSvo_cls_INHERITS}
    &ELSEIF DEFINED(PPCAPSTestCaseCreatorSvo_cls_Q_INHERITS) = 1 &THEN
      {&PPCAPSTestCaseCreatorSvo_cls_Q_INHERITS}
    &ELSEIF DEFINED(PPCAPSTestCaseCreatorSvo_cls_U_INHERITS) = 1 &THEN
      {&PPCAPSTestCaseCreatorSvo_cls_U_INHERITS}
    &ELSEIF DEFINED(PPCAPSTestCaseCreatorSvo_cls_C_INHERITS) = 1 &THEN
      {&PPCAPSTestCaseCreatorSvo_cls_C_INHERITS}
    &ELSE
      pps.prod.cls.PPCAPSTestCaseCreatorSvoStd
    &ENDIF
  :

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


end class. /* pps.prod.cls.PPCAPSTestCaseCreatorSvo */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

