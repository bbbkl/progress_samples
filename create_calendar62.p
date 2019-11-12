/* run createCalendar1. */

/* run createCalendar2. */

/* run createCalendar3. */

/* run setOverloadIntensity. */

run deleteExceptionCalendar.

run updateResCalendars(4). /* Transport */

run updateResCalendars(5). /* Process (Verfahren) */

/* run updateResCalendars(6). */ /* Area (Fläche) */





/* **********************  Internal Procedures  *********************** */


procedure createCalendar1 private:
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* creates a new standard calendar for 1-shift work in the new DB model       */
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

define variable iLoop               as integer    no-undo.
define variable cCalObj             as character  no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bMMM_CalendarWeeks    for MMM_CalendarWeeks.
define buffer bMMM_StandardWeekHead for MMM_StandardWeekHead.
define buffer bMMM_StandardWeekDay  for MMM_StandardWeekDay.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

disable triggers for load of M_Kalender.
disable triggers for load of MMM_CalendarWeeks.
disable triggers for load of MMM_StandardWeekHead.
disable triggers for load of MMM_StandardWeekDay.


/* create a standard week */
for each S_Firma
  no-lock
  on error undo, throw:
      
  create bMMM_StandardWeekHead.
  
  assign
    bMMM_StandardWeekHead.Company                  = S_Firma.Firma
    bMMM_StandardWeekHead.CreatedBy                = pACConnectionSvc:prpcUserID
    bMMM_StandardWeekHead.CreationDateTime         = now
    bMMM_StandardWeekHead.MMM_StandardWeekHead_ID  = '1-Schicht':U
    bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekHead':U)
    bMMM_StandardWeekHead.NumberOfWeeks            = 1
    bMMM_StandardWeekHead.StandardWeekType         = 'calendar'
    .
  
 
  do iLoop = 2 to 6  /* Monday to Friday */
      on error undo, throw:
  
    create bMMM_StandardWeekDay.
  
    assign
      bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
      bMMM_StandardWeekDay.CreationDateTime         = now
      bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
      bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
      bMMM_StandardWeekDay.TimeFrom                 = '0800':U
      bMMM_StandardWeekDay.TimeTo                   = '1600':U
      bMMM_StandardWeekDay.Weekday                  = iLoop
      bMMM_StandardWeekDay.WeekNumber               = 1
      .
  end. /* do iLoop = 2 to 6 */
  
  /* assign the same standard week to all standard calendars */
  for each M_Kalender
    where M_Kalender.Firma = S_Firma.Firma      
      and M_Kalender.KalArt = 0       
     no-lock
     on error undo, throw:
       
    cCalObj = M_Kalender.M_Kalender_Obj.
    
    create bMMM_CalendarWeeks.
  
    assign
      bMMM_CalendarWeeks.CreatedBy                = pACConnectionSvc:prpcUserID
      bMMM_CalendarWeeks.CreationDateTime         = now
      bMMM_CalendarWeeks.MMM_CalendarWeeks_Obj    = adm.method.cls.DMCSessionSvc:cObjectID('MMM_CalendarWeeks':U)
      bMMM_CalendarWeeks.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
      bMMM_CalendarWeeks.Parent_Obj               = cCalObj
      bMMM_CalendarWeeks.StandardWeekType         = 'calendar'
      bMMM_CalendarWeeks.StartWeek                = 1
      bMMM_CalendarWeeks.ValidFrom                = today
      .
    
  end. /* for each M_Kalender */

end. /* for each S_Firma */

end procedure. /* createCalendar1 */



procedure createCalendar2 private:
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* creates a new standard calendar for 3-shift work in the new DB model       */
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

define variable iLoop               as integer    no-undo.
define variable cCalObj             as character  no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bMMM_CalendarWeeks    for MMM_CalendarWeeks.
define buffer bMMM_StandardWeekHead for MMM_StandardWeekHead.
define buffer bMMM_StandardWeekDay  for MMM_StandardWeekDay.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

disable triggers for load of M_Kalender.
disable triggers for load of MMM_CalendarWeeks.
disable triggers for load of MMM_StandardWeekHead.
disable triggers for load of MMM_StandardWeekDay.


/* create a standard week */
for each S_Firma
  no-lock
  on error undo, throw:
      
  create bMMM_StandardWeekHead.
  
  assign
    bMMM_StandardWeekHead.Company                  = S_Firma.Firma
    bMMM_StandardWeekHead.CreatedBy                = pACConnectionSvc:prpcUserID
    bMMM_StandardWeekHead.CreationDateTime         = now
    bMMM_StandardWeekHead.MMM_StandardWeekHead_ID  = '3-Schicht':U
    bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekHead':U)
    bMMM_StandardWeekHead.NumberOfWeeks            = 1
    bMMM_StandardWeekHead.StandardWeekType         = 'calendar'
    .
  
 
  do iLoop = 1 to 7  /* Sunday to Saturday */
      on error undo, throw:
  
    create bMMM_StandardWeekDay.
  
    assign
      bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
      bMMM_StandardWeekDay.CreationDateTime         = now
      bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
      bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
      bMMM_StandardWeekDay.TimeFrom                 = '0000':U
      bMMM_StandardWeekDay.TimeTo                   = '0800':U
      bMMM_StandardWeekDay.Weekday                  = iLoop
      bMMM_StandardWeekDay.WeekNumber               = 1
      .
      
    create bMMM_StandardWeekDay.
  
    assign
      bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
      bMMM_StandardWeekDay.CreationDateTime         = now
      bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
      bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
      bMMM_StandardWeekDay.TimeFrom                 = '0800':U
      bMMM_StandardWeekDay.TimeTo                   = '1600':U
      bMMM_StandardWeekDay.Weekday                  = iLoop
      bMMM_StandardWeekDay.WeekNumber               = 1
      .
      
    create bMMM_StandardWeekDay.
  
    assign
      bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
      bMMM_StandardWeekDay.CreationDateTime         = now
      bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
      bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
      bMMM_StandardWeekDay.TimeFrom                 = '1600':U
      bMMM_StandardWeekDay.TimeTo                   = '2400':U
      bMMM_StandardWeekDay.Weekday                  = iLoop
      bMMM_StandardWeekDay.WeekNumber               = 1
      .
      
  end. /* do iLoop = 1 to 7 */
  
  /* assign the 3-shift-work standard week to the first standard calendar */
  find first M_Kalender
    where M_Kalender.Firma = S_Firma.Firma      
      and M_Kalender.KalArt = 0       
    no-lock no-error.
    
    if available M_Kalender then
    do:
         
      cCalObj = M_Kalender.M_Kalender_Obj.
      
      create bMMM_CalendarWeeks.
    
      assign
        bMMM_CalendarWeeks.CreatedBy                = pACConnectionSvc:prpcUserID
        bMMM_CalendarWeeks.CreationDateTime         = now
        bMMM_CalendarWeeks.MMM_CalendarWeeks_Obj    = adm.method.cls.DMCSessionSvc:cObjectID('MMM_CalendarWeeks':U)
        bMMM_CalendarWeeks.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
        bMMM_CalendarWeeks.Parent_Obj               = cCalObj
        bMMM_CalendarWeeks.StandardWeekType         = 'calendar'
        bMMM_CalendarWeeks.StartWeek                = 1
        bMMM_CalendarWeeks.ValidFrom                = today + 1
        .

    end. /* if available M_Kalender */

end. /* for each S_Firma */

end procedure. /* createCalendar2 */




procedure createCalendar3 private:
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* creates a new 2-week-periodic standard calendar in the new DB model        */
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

define variable iLoop               as integer    no-undo.
define variable cCalObj             as character  no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bMMM_CalendarWeeks    for MMM_CalendarWeeks.
define buffer bMMM_StandardWeekHead for MMM_StandardWeekHead.
define buffer bMMM_StandardWeekDay  for MMM_StandardWeekDay.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

disable triggers for load of M_Kalender.
disable triggers for load of MMM_CalendarWeeks.
disable triggers for load of MMM_StandardWeekHead.
disable triggers for load of MMM_StandardWeekDay.


/* create a standard week */
for each S_Firma
  no-lock
  on error undo, throw:
      
  create bMMM_StandardWeekHead.
  
  assign
    bMMM_StandardWeekHead.Company                  = S_Firma.Firma
    bMMM_StandardWeekHead.CreatedBy                = pACConnectionSvc:prpcUserID
    bMMM_StandardWeekHead.CreationDateTime         = now
    bMMM_StandardWeekHead.MMM_StandardWeekHead_ID  = '2-Wochen-periodisch':U
    bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekHead':U)
    bMMM_StandardWeekHead.NumberOfWeeks            = 2
    bMMM_StandardWeekHead.StandardWeekType         = 'calendar'
    .
  
  do iLoop = 2 to 6  /* Monday to Friday */
      on error undo, throw:
  
    create bMMM_StandardWeekDay.
  
    assign
      bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
      bMMM_StandardWeekDay.CreationDateTime         = now
      bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
      bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
      bMMM_StandardWeekDay.TimeFrom                 = '0800':U
      bMMM_StandardWeekDay.TimeTo                   = '1800':U
      bMMM_StandardWeekDay.Weekday                  = iLoop
      bMMM_StandardWeekDay.WeekNumber               = 1
      .

    create bMMM_StandardWeekDay.
   
    assign
      bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
      bMMM_StandardWeekDay.CreationDateTime         = now
      bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
      bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
      bMMM_StandardWeekDay.TimeFrom                 = '0800':U
      bMMM_StandardWeekDay.TimeTo                   = '1400':U
      bMMM_StandardWeekDay.Weekday                  = iLoop
      bMMM_StandardWeekDay.WeekNumber               = 2
      .
  end. /* do iLoop = 2 to 6 */
  
  create bMMM_StandardWeekDay.
 
  assign
    bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
    bMMM_StandardWeekDay.CreationDateTime         = now
    bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
    bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
    bMMM_StandardWeekDay.TimeFrom                 = '0500':U
    bMMM_StandardWeekDay.TimeTo                   = '1100':U
    bMMM_StandardWeekDay.Weekday                  = 7 /* Saturday */
    bMMM_StandardWeekDay.WeekNumber               = 2
    .
  
  create bMMM_StandardWeekDay.
 
  assign
    bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
    bMMM_StandardWeekDay.CreationDateTime         = now
    bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
    bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
    bMMM_StandardWeekDay.TimeFrom                 = '1300':U
    bMMM_StandardWeekDay.TimeTo                   = '1900':U
    bMMM_StandardWeekDay.Weekday                  = 7 /* Saturday */
    bMMM_StandardWeekDay.WeekNumber               = 1
    .
    
  create bMMM_StandardWeekDay.
 
  assign
    bMMM_StandardWeekDay.CreatedBy                = pACConnectionSvc:prpcUserID
    bMMM_StandardWeekDay.CreationDateTime         = now
    bMMM_StandardWeekDay.MMM_StandardWeekDay_Obj  = adm.method.cls.DMCSessionSvc:cObjectID('MMM_StandardWeekDay':U)
    bMMM_StandardWeekDay.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
    bMMM_StandardWeekDay.TimeFrom                 = '0800':U
    bMMM_StandardWeekDay.TimeTo                   = '1200':U
    bMMM_StandardWeekDay.Weekday                  = 1 /* Sunday */
    bMMM_StandardWeekDay.WeekNumber               = 2
    .
    
  /* assign the 2-week-periodic standard week to the last standard calendar */
  find last M_Kalender
    where M_Kalender.Firma = S_Firma.Firma      
      and M_Kalender.KalArt = 0       
     no-lock no-error.
       
    if available M_Kalender then
    do:   
         
      cCalObj = M_Kalender.M_Kalender_Obj.
      
      create bMMM_CalendarWeeks.
    
      assign
        bMMM_CalendarWeeks.CreatedBy                = pACConnectionSvc:prpcUserID
        bMMM_CalendarWeeks.CreationDateTime         = now
        bMMM_CalendarWeeks.MMM_CalendarWeeks_Obj    = adm.method.cls.DMCSessionSvc:cObjectID('MMM_CalendarWeeks':U)
        bMMM_CalendarWeeks.MMM_StandardWeekHead_Obj = bMMM_StandardWeekHead.MMM_StandardWeekHead_Obj
        bMMM_CalendarWeeks.Parent_Obj               = cCalObj
        bMMM_CalendarWeeks.StandardWeekType         = 'calendar'
        bMMM_CalendarWeeks.StartWeek                = 2
        bMMM_CalendarWeeks.ValidFrom                = today + 2
        .
        
    end. /* if available M_Kalender */

end. /* for each S_Firma */

end procedure. /* createCalendar3 */




&IF DEFINED(EXCLUDE-deleteExceptionCalendar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteExceptionCalendar Method-Library
procedure deleteExceptionCalendar :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* deletes exception calendar for resources with an assigned std calendar     */
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

define buffer bS_Firma        for S_Firma.
define buffer bM_Ressource    for M_Ressource.
define buffer bM_KalenderZeit for M_KalenderZeit.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/


for each bS_Firma
  no-lock
  on error undo, throw:
    
  for each bM_Ressource
    where bM_Ressource.Firma     = bS_Firma.Firma
      and bM_Ressource.Kalender <> '':U
    no-lock
    on error undo, throw:
      
      for each bM_KalenderZeit
        where bM_KalenderZeit.Firma    = bM_Ressource.Firma
          and bM_KalenderZeit.KalArt   = bM_Ressource.RessArt
          and bM_KalenderZeit.Kalender = bM_Ressource.Ressource
          and bM_KalenderZeit.Origin   = 'standard'
        use-index Main
        exclusive-lock
        on error undo, throw:
          
        delete bM_KalenderZeit.
          
      end. /* for each bM_KalenderZeit */
      
  end. /* for each bM_Ressource */
    
end. /* for each S_Firma */

end procedure. /* deleteExceptionCalendar */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


procedure setOverloadIntensity private:
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* sets the overload intensity of every resource > 0                          */
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

define variable iInt               as integer    no-undo.
define variable dInt               as decimal    no-undo.

/* Buffers -------------------------------------------------------------------*/

define buffer bM_Ressource    for M_Ressource.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

disable triggers for load of M_Ressource.

for each S_Firma
  no-lock
  on error undo, throw:
      
  for each bM_Ressource
    where bM_Ressource.Firma = S_Firma.Firma 
     exclusive-lock
     on error undo, throw:
   
    dInt = 0.5 * bM_Ressource.Intensitaet.   
    iInt = round(dInt,0).
  
    assign
      bM_Ressource.UeberlastIntensitaet = iInt
      bM_Ressource.UeberlastMoeglich = yes
      .
    
  end. /* for each bM_Ressource */

end. /* for each S_Firma */

end procedure.



&IF DEFINED(EXCLUDE-updateResCalendars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateResCalendars Method-Library
procedure updateResCalendars :
/* Description ---------------------------------------------------------------*/
/*                                                                            */
/* updates all calendar entries for resources of type process, transport      */
/* or area                                                                    */
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

define input parameter iResType as integer no-undo.

/* Variables -----------------------------------------------------------------*/

define variable tMinDate      as date      no-undo.
define variable cMinDateObjId as character no-undo.
define variable iDateOffset   as integer   no-undo.

/*----------------------------------------------------------------------------*/

/* Buffers -------------------------------------------------------------------*/

define buffer bS_Firma         for S_Firma.
define buffer bM_Ressource     for M_Ressource.
define buffer bM_KalenderDatum for M_KalenderDatum.

/*----------------------------------------------------------------------------*/
/* Processing                                                                 */
/*----------------------------------------------------------------------------*/

if not( iResType = 4        /* transport           */
  or iResType = 5           /* process (Verfahren) */
  or iResType = 6 ) then    /* area (Fläche)       */
  return.

for each bS_Firma
  no-lock
  on error undo, throw:
    
  for each bM_Ressource
    where bM_Ressource.Firma = bS_Firma.Firma
      and bM_Ressource.RessArt = iResType
    no-lock
    on error undo, throw:
      
      assign
       tMinDate      = today
       cMinDateObjId = '':U
       .
      
      /* find calendar entry with earliest begin date */
      for each bM_KalenderDatum
        where bM_KalenderDatum.Firma    = bM_Ressource.Firma
          and bM_KalenderDatum.KalArt   = bM_Ressource.RessArt
          and bM_KalenderDatum.Kalender = bM_Ressource.Ressource
        use-index Main
        no-lock
        on error undo, throw:
          
        if bM_KalenderDatum.vonDatum < tMinDate then
          assign
            tMinDate      = bM_KalenderDatum.vonDatum
            cMinDateObjId = bM_KalenderDatum.M_KalenderDatum_Obj
            .
        
      end. /* for each bM_KalenderDatum */
      
      /* determin the date offset between the earliest calendar entry and today */
      find bM_KalenderDatum
        where bM_KalenderDatum.M_KalenderDatum_Obj = cMinDateObjId
        no-lock no-error.
        
      if available bM_KalenderDatum then
      do:
        
        iDateOffset = today - bM_KalenderDatum.vonDatum.     
     
       /* update all calendar entries. The earliest entry will be today */
       for each bM_KalenderDatum
          where bM_KalenderDatum.Firma          = bM_Ressource.Firma
            and bM_KalenderDatum.KalArt         = bM_Ressource.RessArt
            and bM_KalenderDatum.Kalender       = bM_Ressource.Ressource
            and bM_KalenderDatum.AenderungDatum < today
          use-index Main
          exclusive-lock
          on error undo, throw:
            
          assign
           bM_KalenderDatum.vonDatum = bM_KalenderDatum.vonDatum + iDateOffset
           bM_KalenderDatum.bisDatum = bM_KalenderDatum.bisDatum + iDateOffset
           .
            
        end. /* for each bM_KalenderDatum */
        
      end. /* if available bM_KalenderDatum then */
      
  end. /* for each bM_Ressource */
    
end. /* for each bS_Firma */


end procedure. /* updateResCalendars */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

