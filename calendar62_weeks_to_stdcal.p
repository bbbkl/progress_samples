/* for each standard calendar of firma=110 without a calendarweek assign a calendarweek */

FIND FIRST mmm_standardweekhead WHERE mmm_standardweekhead_id='1-Schicht(1Woche)'.

DEFINE VARIABLE dt AS datetime NO-UNDO.
dt = now.
/* display dt. */

FOR EACH m_Kalender WHERE firma='110' AND KalArt=0:
    IF NOT CAN-FIND (MMM_CalendarWeeks WHERE parent_obj=m_kalender_obj) THEN 
    DO:
 
        /* MESSAGE m_Kalender.kalender VIEW-AS ALERT-BOX INFO BUTTONS OK. */

        
        CREATE MMM_CalendarWeeks.
        ASSIGN
            MMM_CalendarWeeks.parent_obj = m_Kalender.m_kalender_obj
            MMM_CalendarWeeks.VALIDFROM=now
            MMM_CalendarWeeks.mmm_standardweekhead_obj=mmm_standardweekhead.mmm_standardweekhead_obj
            MMM_CalendarWeeks.standardweektype='calendar'
            .
            
    END.
END.
