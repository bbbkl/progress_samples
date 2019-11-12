{adm/template/incl/dt_pro00.df}

DEFINE VARIABLE   gcCompany        AS CHAR INIT "110".
DEFINE VARIABLE   gcOrderNumber    AS CHAR INIT "wb-100-AufPos-01".
DEFINE VARIABLE   giCRO            AS INTEGER INIT 0.
DEFINE VARIABLE   gcPart           AS CHAR INIT "5000042":U.
DEFINE VARIABLE   gcPartVar        AS CHAR INIT "":U.
DEFINE VARIABLE   gdQuantity       AS DECIMAL.
DEFINE VARIABLE   gtRequestedDate  AS DATE.

DEFINE VARIABLE   gcProcessArea    AS CHAR INIT "CTP". 

define temp-table TT_P_SSt no-undo like TD_P_SSt.

{pps/incl/p_proz00.lib}
{pps/incl/p_proz01.lib}

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

/* (1) set values like Werksbestellung need */

gcOrderNumber   = "wb-100-AufPos-01".
giCRO           = 0.
gcPart          = "5000042":U.
gcPartVar       = "":U.
gdQuantity      = 3.
gtRequestedDate = TODAY.
gcProcessArea   = "CTP".


/* (2) delete old tempStructure */

RUN deleteTempStructure ( gcCompany,
                          gcOrderNumber).
       

/* (3) create new tempStructure */

RUN createTempStructure (gcCompany,
                         gcOrderNumber,
                         giCRO,
                         gcPart,
                         gcPartVar,
                         gdQuantity,
                         gtRequestedDate).


/* (4) scheduleRough tempStructure */

RUN scheduleRoughTempStructure (gcCompany,
                                gcOrderNumber).


/* (5) set link from tempstructure to Werksbestellung */

/* ... lSwitchLink ( ... )  */ 


MESSAGE 
  "The temp structure was applied".

/* ========================================================================== */



/*----------------------------------------------------------------------------*/
/* deleteTempStructure                                                        */
/*----------------------------------------------------------------------------*/

PROCEDURE deleteTempStructure :
  DEFINE INPUT PARAMETER pcCompany        as CHARACTER.
  DEFINE INPUT PARAMETER pcOrderNumber    as CHARACTER.

  DEFINE BUFFER bMA_tempProzess  FOR ma_tempProzess.

  FOR EACH bMA_tempProzess
    WHERE bMA_tempProzess.firma   = {firma/mawia.fir pcCompany}
      AND bMA_tempProzess.Prozess = pcOrderNumber
    EXCLUSIVE-LOCK
    on error undo, RETURN ERROR:
  
    DELETE bma_tempProzess.   

  END. /* end for each */

END.


/*----------------------------------------------------------------------------*/
/* scheduleRoughTempStructure                                                 */
/*----------------------------------------------------------------------------*/

PROCEDURE scheduleRoughTempStructure :
  DEFINE INPUT PARAMETER pcCompany        as CHARACTER.
  DEFINE INPUT PARAMETER pcOrderNumber    as CHARACTER.

  DEFINE VARIABLE   hAppServer      AS HANDLE.
  DEFINE BUFFER     bMA_TempProzess  FOR MA_TempProzess.

  {adm/incl/d__run00.if
    &Programm = "pps/prod/proc/ppaauf00.p"
    &Handle   = "hAppServer"
    }


  find first bMA_TempProzess
    where bMA_TempProzess.Firma           = {firma/mawia.fir pcCompany}
      and bMA_TempProzess.Prozess         = pcOrderNumber
      and bMA_TempProzess.ProzessBereich  = gcProcessArea
    NO-LOCK.
    
  if valid-handle(hAppServer) then 
  do:

    {adm/incl/d__run02.if
      &AppSvHandle = "hAppServer"
      &Procedure   = "Terminierung_Temp"
      &Arguments   = "(bMA_TempProzess.Firma,
                       bMA_TempProzess.ProzessBereich,
                       integer(bMA_TempProzess.Teilprozess))"
      }
    
    end.

END. /* end Procedure */


/*----------------------------------------------------------------------------*/
/* createTempStructure                                                        */
/*----------------------------------------------------------------------------*/

PROCEDURE createTempStructure :
  DEFINE INPUT PARAMETER pcCompany        as CHARACTER.
  DEFINE INPUT PARAMETER pcOrderNumber    as CHARACTER.
  DEFINE INPUT PARAMETER piCRO            AS INTEGER.
  DEFINE INPUT PARAMETER pcPart           as CHARACTER.
  DEFINE INPUT PARAMETER pcPartVar        as CHARACTER.
  DEFINE INPUT PARAMETER pdQuantity       as decimal.
  DEFINE INPUT PARAMETER ptRequestedDate  AS DATE.

  DEFINE VARIABLE   hAppServer        AS HANDLE.
  DEFINE VARIABLE   iKopfTeilprozess  AS INTEGER.

  {adm/incl/d__run00.if
    &Programm = "pps/proc/p_aprz01.p"
    &Handle   = "hAppServer"
  }
  

  if valid-handle(hAppServer) then 
  do:

    {adm/incl/d__run02.if
      &procedure   = "CreateTempProzess"
      &AppSvHandle = "hAppServer"
      &Arguments   = "( pcCompany,
                        pcPart,
                        pcPartVar,
                        gcProcessArea,
                        pcOrderNumber,
                        ptRequestedDate,
                        pdQuantity,
                        piCRO,
                        YES,
                        no,
                        'F':U,
                        output iKopfTeilprozess)"
      }

    end. /* if valid-handle(hAppServer) */
  
  END. /* end createTempStructure */
