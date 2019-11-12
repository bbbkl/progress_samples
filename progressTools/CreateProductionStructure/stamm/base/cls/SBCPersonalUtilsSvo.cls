&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r2 GUI
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS V-table-Win 
routine-level on error undo, throw.

/* _UIB-CODE-BLOCK-END */ 
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Update Information" Procedure _INLINE
/* Actions: ? ? ? ? adm/support/proc/ds_pa_01.w */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure
/******************************************************************************/
/*                                            (c) 2014 proALPHA Software GmbH */
/*                                                     Auf dem Immel 8        */
/*                                                     67685 Weilerbach       */
/* Project: proALPHA                                                          */
/*                                                                            */
/* Name   : SBCPersonalUtilsSvo.cls                                           */
/* Product: STAMM - Übergreifende Stammdaten                                  */
/* Module : BASE - Kernfunktionen                                             */
/*                                                                            */
/* Created: 6.1e01 as of 04.07.2014/Peter Köhler                              */
/* Current: 6.1e01 as of 04.07.2014/Peter Köhler                              */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* DESCRIPTION                                                                */
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* util object                                                                */
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
/******************************************************************************/

/* Procedure Information -----------------------------------------------------*/

&GLOBAL-DEFINE pa-Autor            Peter Köhler
&GLOBAL-DEFINE pa-Version          6.1e01
&GLOBAL-DEFINE pa-Datum            04.07.2014
&GLOBAL-DEFINE pa-Letzter          Peter Köhler

&GLOBAL-DEFINE pa-GenVersion       OEA
&GLOBAL-DEFINE pa-ProgrammTyp      Class
&GLOBAL-DEFINE pa-Template         paooablclass.clsjet
&GLOBAL-DEFINE pa-TemplateVersion  1.00
&GLOBAL-DEFINE pa-XBasisName       SBCPersonalUtilsSvo_cls
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

class stamm.base.cls.SBCPersonalUtilsSvo
  inherits  &IF   DEFINED(SBCPersonalUtilsSvo_cls_Y_INHERITS) = 0
              AND DEFINED(SBCPersonalUtilsSvo_cls_INHERITS)   = 0 
              AND DEFINED(SBCPersonalUtilsSvo_cls_C_INHERITS) = 0 &THEN 
              stamm.base.cls.SBCPersonalUtilsSvoStd
            &ELSE
               &IF DEFINED(SBCPersonalUtilsSvo_cls_Y_INHERITS) = 1 &THEN
               
                   {&SBCPersonalUtilsSvo_cls_Y_INHERITS}
                   
               &ELSEIF DEFINED(SBCPersonalUtilsSvo_cls_INHERITS) = 1 &THEN
                  
                   {&SBCPersonalUtilsSvo_cls_INHERITS}
                   
               &ELSEIF DEFINED(SBCPersonalUtilsSvo_cls_C_INHERITS) = 1 &THEN
                  
                   {&SBCPersonalUtilsSvo_cls_C_INHERITS}
               &ENDIF
                                        
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

  
end class. /* stamm.base.cls.SBCPersonalUtilsSvo */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
