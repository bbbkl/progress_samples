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
/* Name   : SBCPersonalUtilsSvoStd.cls                                        */
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
/* Standard Class for SBCPersonalUtilsSvo.cls                                 */
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
&GLOBAL-DEFINE pa-Template         paooablstdclass.clsjet
&GLOBAL-DEFINE pa-TemplateVersion  1.00
&GLOBAL-DEFINE pa-XBasisName       SBCPersonalUtilsSvoStd_cls
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

class stamm.base.cls.SBCPersonalUtilsSvoStd
  inherits adm.base.cls.DBCproALPHA
  abstract
  :  

  /*--------------------------------------------------------------------------*/
  /* Definitions                                                              */
  /*--------------------------------------------------------------------------*/

  /* Datasets ----------------------------------------------------------------*/

  /* Temp-Tables -------------------------------------------------------------*/

  /* Buffers -----------------------------------------------------------------*/

  /* Variables ---------------------------------------------------------------*/
  
  define stream fileOut.
  define stream fileIn.
  
  /*--------------------------------------------------------------------------*/
  /* Properties                                                               */
  /*--------------------------------------------------------------------------*/

  define public property prplUseTimestamp as logical no-undo get. set.

  /*--------------------------------------------------------------------------*/
  /* Constructors                                                             */
  /*--------------------------------------------------------------------------*/

  /*--------------------------------------------------------------------------*/
  /* Destructors                                                              */
  /*--------------------------------------------------------------------------*/

  /*--------------------------------------------------------------------------*/
  /* Methods                                                                  */
  /*--------------------------------------------------------------------------*/
  
  method protected character getTimestampString
    (  ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/

    return '(':U + string(today,'99/99/9999':U) + ' ':U + string(time,'hh:mm:ss':U) + ') ':U.

  end method. /* getTimestampString */

 

  method public void inFileClose
    (  ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    input stream fileIn close.
    
  end method. /* inFileClose */  
  
  
  
  method public void inFileOpen
    ( pcFileName as character ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    input stream fileIn from pcFileName.
    
  end method. /* inFileOpen */  
  
  
  
  
  
  method public void logDatasetTableStats
    ( phTTHandle    as handle,
      pcDatasetName as character ):
    /* Description -----------------------------------------------------------*/
    /*                                                                        */
    /* logs the stats of the provided temp-table                              */
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

    define variable iTableCount as integer   no-undo.
    define variable iRowCount   as integer   no-undo. 
    
    define variable hQuery      as handle    no-undo.
    define variable hBuffer     as handle    no-undo.
    define variable cBufferName as character no-undo.

    /* Buffers ---------------------------------------------------------------*/

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    this-object:writeHeader('TempTable-Stats for dataset <':U + pcDatasetName + '>':U).
    
    do iTableCount = 1 to phTTHandle:num-buffers:
      
      hBuffer = phTTHandle:get-buffer-handle(iTableCount).
      
      cBufferName = string(hBuffer:name).
      
      create query hQuery.
    
      hQuery:add-buffer(hBuffer).
  
      /* define where statement                                                   */
      
      hQuery:query-prepare('for each ':U + hBuffer:name).
      
      hQuery:query-open().
      
      hQuery:get-first().
      
      iRowCount = 0.
      
      do while hBuffer:available:
        
        iRowCount = iRowCount + 1.
        
        hQuery:get-next().
        
      end. /* do iTableCount = 1 to phTTHandle:num-buffers */
      
      this-object:writeLine(cBufferName + ';' + string(iRowCount)).
      
    end.
    
  end method. /* logDatasetTableStats */
    

  
  method public void logTableIndexInfo
    ( pcTableName          as character,
      plUseWikiTableFormat as logical):
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
    
    define variable cFieldList as character no-undo.
    
    /* Buffers ---------------------------------------------------------------*/
    
    
    define buffer bDRC_Table      for DRC_Table.
    define buffer bDRC_Index      for DRC_Index.
    define buffer bDRC_IndexField for DRC_IndexField.
    define buffer bDRC_Field      for DRC_Field.
    
    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/

    this-object:writeHeader(pcTableName).
     
    find bDRC_Table
      where bDRC_Table.DRC_Table_ID = pcTableName
      no-lock.  
     
    for each bDRC_Index 
      where bDRC_Index.drc_table_obj = bDRC_Table.DRC_Table_Obj
      no-lock
      on error undo, throw:
       
      if plUseWikiTableFormat then
        this-object:writeLine(substitute('| &1<br/>':U,bDRC_Index.DRC_Index_ID)).
      else
        this-object:writeLine(substitute('Index: &1 - Unique: &2 - Primary: &3':U,
                                         bDRC_Index.DRC_Index_ID,
                                         string(bDRC_Index.IndexIsUnique),
                                         string(bDRC_Index.IndexIsPrimary))).
       
      cFieldList = '':U.

      for each bDRC_IndexField
        where bDRC_IndexField.DRC_Index_Obj = bDRC_Index.DRC_Index_Obj
        no-lock
        on error undo, throw:

        find bDRC_Field
          where bDRC_Field.DRC_Field_Obj = bDRC_IndexField.DRC_Field_Obj
          no-lock.
          
          if plUseWikiTableFormat then
            cFieldList = cFieldList + bDRC_Field.DRC_Field_ID + ',':U.
          else
            this-object:writeLine(substitute('  Field: &1':U,bDRC_Index.DRC_Index_ID)).

      end. /* for each bDRC_IndexField ... */

      if plUseWikiTableFormat then
      do:    
        
        cFieldList = right-trim(cFieldList,',':U).
      
        this-object:writeLine(substitute('| &1<br/>',cFieldList)).
        this-object:writeLine(substitute('| &1<br/>',string(bDRC_Index.IndexIsUnique,'Yes/No'))).
        this-object:writeLine(substitute('|-')).
      
      end. /* if plUseWikiTableFormat */
    
    end. /* for each bDRC_Index  ... */
    
  end method. /* logTableIndexInfo */

 

  method public void outFileClose
    (  ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    output stream fileOut close.
    
  end method. /* outFileClose */  
  
  
  
  method public void outFileOpen
    ( pcFileName as character,
      plAppend   as logical ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    if plAppend then
      output stream fileOut to value(pcFileName) unbuffered append.
    else
      output stream fileOut to value(pcFileName) unbuffered.
    
  end method. /* outFileOpen */  
  
  
  
  method public character readLine
    (  ):
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
    
    define variable cTextString as character no-undo.
    
    /* Buffers ---------------------------------------------------------------*/

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    do
      on endkey undo, return ?:
        
      import stream fileIn unformatted cTextString.
      return cTextString.
    
    end.
    
  end method. /* readLine */

 

  method public void writeBlankLine
    (  ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/

    put stream fileOut unformatted
      ' ':U
      skip.

  end method. /* writeBlankLine */

 
  
  method public void writeHeader
    ( pcHeaderText as character ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    this-object:writeHeader(pcHeaderText,'-':U).
    
  end method. /* writeHeader */
  
  
  
  method public void writeHeader
    ( pcHeaderText           as character,
      pcHeaderSeparationChar as character ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    pcHeaderText = fill(pcHeaderSeparationChar,3) + ' ':U + pcHeaderText.
    
    if prplUseTimestamp then
      pcHeaderText = this-object:getTimestampString() + pcHeaderText.
      
    put stream fileOut unformatted
      pcHeaderText + ' ':U + fill(pcHeaderSeparationChar,30) 
      skip.
    
  end method. /* writeHeader */

 

  method public void writeLine
    ( pcLineText as character ):
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

    /*------------------------------------------------------------------------*/
    /* Processing                                                             */
    /*------------------------------------------------------------------------*/
    
    if prplUseTimestamp then
      pcLineText = this-object:getTimestampString() + pcLineText.
    
    put stream fileOut unformatted
      pcLineText
      skip.
    
  end method. /* writeLine */

end class. /* stamm.base.cls.SBCPersonalUtilsSvoStd */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
