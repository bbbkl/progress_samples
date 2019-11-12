FOR EACH PP_Auftrag
    WHERE PP_Auftrag.Auftragsstatus = 'E' OR
          PP_Auftrag.Auftragsstatus = 'F':
    /* PP_Auftrag.Auftragsstatus = 'B'. */
    pps.prod.cls.PPCProdWorkOrderSvc:prpoInstance:setStatus(
        PP_Auftrag.PP_Auftrag_Obj,
        'b',
        no) NO-ERROR.
END.
