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
/* Product: STAMM - �bergreifende Stammdaten                                  */
/* Module : BASE - Kernfunktionen                                             */
/*                                                                            */
/* Created: 6.1e01 as of 04.07.2014/Peter K�hler                              */
/* Current: 6.1e01 as of 04.07.2014/Peter K�hler                              */
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

&GLOBAL-DEFINE pa-Autor            Peter K�hler
&GLOBAL-DEFINE pa-Version          6.1e01
&GLOBAL-DEFINE pa-Datum            04.07.2014
&GLOBAL-DEFINE pa-Letzter          Peter K�hler

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

  define public property prplUseTimestamp  as logical no-undo get. set.
  define public property prplOutFileOpened as logical no-undo get. protected set.
  define public property prplInFileOpened as logical no-undo get. protected set.
  
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

    if this-object:prplInFileOpened then
    do:
    
      input stream fileIn close.
      
      this-object:prplInFileOpened = no.
    
    end. /* if this-object:prplInFileOpened */
    
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
    
    /* close an already opend input file                                      */
    
    if this-object:prplInFileOpened then
      this-object:inFileClose().
    
    /* open the new input file                                                */
      
    input stream fileIn from pcFileName.
    
    this-object:prplInFileOpened = yes.
    
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
      plUseWikiTableFormat as logical,
      plWriteUniqueInfo    as logical,
      plWritePrimaryInfo   as logical):
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
    
    define variable cTemp as character no-undo.
    
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
      do:

        cTemp = substitute('Index: &1':U,
                           bDRC_Index.DRC_Index_ID).
        
        if plWriteUniqueInfo then
          cTemp = cTemp + substitute(' - Unique: &1':U,
                                     string(bDRC_Index.IndexIsUnique)).
        
        if plWritePrimaryInfo then
          cTemp = cTemp + substitute(' - Primary: &1':U,
                                     string(bDRC_Index.IndexIsPrimary)).
        
        this-object:writeLine(cTemp).
        
      end. /* else if plUseWikiTableFormat */
      
      cTemp = '':U.

      for each bDRC_IndexField
        where bDRC_IndexField.DRC_Index_Obj = bDRC_Index.DRC_Index_Obj
        no-lock
        on error undo, throw:

        find bDRC_Field
          where bDRC_Field.DRC_Field_Obj = bDRC_IndexField.DRC_Field_Obj
          no-lock.
          
          if plUseWikiTableFormat then
            cTemp = cTemp + bDRC_Field.DRC_Field_ID + ',':U.
          else
            this-object:writeLine(substitute('  Field: &1':U,bDRC_Field.DRC_Field_ID)).

      end. /* for each bDRC_IndexField ... */

      if plUseWikiTableFormat then
      do:    
        
        cTemp = right-trim(cTemp,',':U).
      
        this-object:writeLine(substitute('| &1<br/>',cTemp)).
        if plWriteUniqueInfo then
          this-object:writeLine(substitute('| &1<br/>',string(bDRC_Index.IndexIsUnique,'Yes/No'))).
        if plWritePrimaryInfo then
          this-object:writeLine(substitute('| &1<br/>',string(bDRC_Index.IndexIsPrimary,'Yes/No'))).
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
    
    if this-object:prplOutFileOpened then
    do:
      
      output stream fileOut close.
      
      this-object:prplOutFileOpened = no.
    
    end. /* if this-object:prplInFileOpened */
    
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
    
    if this-object:prplOutFileOpened then
      this-object:outFileClose().
    
    if plAppend then
      output stream fileOut to value(pcFileName) unbuffered append.
    else
      output stream fileOut to value(pcFileName) unbuffered.
    
    this-object:prplOutFileOpened = yes.
    
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
    
    if this-object:prplInFileOpened then
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
    
    if this-object:prplOutFileOpened then
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
    
    if this-object:prplOutFileOpened then
    do:
    
      pcHeaderText = fill(pcHeaderSeparationChar,3) + ' ':U + pcHeaderText + ' ':U.
      
      if prplUseTimestamp then
        pcHeaderText = this-object:getTimestampString() + pcHeaderText.
        
      put stream fileOut unformatted
        pcHeaderText + fill(pcHeaderSeparationChar,30) 
        skip.
    
    end. /* *if this-object:prplOutFileOpened */
    
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
    
    if this-object:prplOutFileOpened then
    do:
    
      if prplUseTimestamp then
        pcLineText = this-object:getTimestampString() + pcLineText.
      
      put stream fileOut unformatted
        pcLineText
        skip.
    
    end. /* if this-object:prplOutFileOpened */
    
  end method. /* writeLine */

end class. /* stamm.base.cls.SBCPersonalUtilsSvoStd */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
