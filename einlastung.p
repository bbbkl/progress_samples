/* FIND FIRST PP_Auftrag WHERE PP_Auftrag.Auftrag = "split" NO-LOCK. */

FOR EACH MB_Aktivitaet 
  WHERE MB_Aktivitaet.Prozess = "CONTINOUS"
  ON ERROR UNDO, RETURN ERROR: 

  DISPLAY MB_Aktivitaet.Teilprozess MB_Aktivitaet.AktPos WITH 2 COLUMNS.

  FOR EACH mbt_ActivityDispatching 
    WHERE mbt_ActivityDispatching.SOURCE_obj = MB_Aktivitaet.MB_Aktivitaet_obj
    ON ERROR UNDO, RETURN ERROR: 
  
    DISPLAY mbt_ActivityDispatching WITH 2 COLUMNS.

  END.

END.


