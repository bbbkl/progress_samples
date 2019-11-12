DEFINE VARIABLE ptStartDate AS DATE NO-UNDO.
DEFINE VARIABLE ptEndDate   AS DATE NO-UNDO.    

FOR EACH M_Ressource 
    WHERE M_Ressource.Firma = '110'
      /* AND M_Ressource.Ressource = '32106' */
      AND M_Ressource.Kalender <> ''
      AND M_Ressource.Kalender <> '10':
    DO:
        MESSAGE 'Yepp: ' + M_Ressource.Kalender
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

        M_Ressource.Kalender = '10'.

        ptStartDate = TODAY.
        ptEndDate = ptStartDate + (2 * 365).

        mawi.base.cls.MMCCalendarSvc:prpoInstance:deleteIntensityCalendar(
            M_Ressource.Kalender,
            M_Ressource.RessArt,
            M_Ressource.Ressource,
            ptStartDate,
            ptEndDate).

        mawi.base.cls.MMCCalendarSvc:prpoInstance:createIntensityCalendar(
            M_Ressource.Kalender,
            M_Ressource.RessArt,
            M_Ressource.Ressource,
            M_Ressource.Intensitaet,
            M_Ressource.UeberlastIntensitaet,
            ptStartDate,
            ptEndDate).
  
    END.
END.
