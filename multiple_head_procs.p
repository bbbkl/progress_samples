DISABLE TRIGGERS FOR LOAD OF PP_Auftrag.

FIND PP_Auftrag
  WHERE PP_Auftrag.Firma = '110'
    AND PP_Auftrag.RueckMeldeNr = 13042
  EXCLUSIVE-LOCK NO-ERROR.

PP_Auftrag.nicht_erster = NO.
