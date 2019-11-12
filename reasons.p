DEFINE VARIABLE  cCompany           AS CHARACTER    INIT "110".
DEFINE VARIABLE  cWorkOrderNumber   AS CHARACTER    INIT "LA-80211821".
DEFINE VARIABLE  cHeadWorkOrder     AS CHARACTER. 

DEFINE BUFFER    bMBL_SchedReason   FOR MBL_SchedReason.

FIND LAST M_ServerKennZ
  WHERE M_ServerKennZ.firma = cCompany
  NO-LOCK.

cHeadWorkOrder = pps.prod.cls.PPCProdWorkOrderSvr:prpoInstance:cGetHeadWorkOrder(cWorkOrderNumber).

/* --------------------------------------------------------------------------------------------- */
/* zeige die letzten Gründe an                                                                   */
/* --------------------------------------------------------------------------------------------- */

find last bMBL_SchedReason
  where bMBL_SchedReason.M_ServerKennZ_Obj = M_ServerKennZ.M_ServerKennZ_obj
    and bMBL_SchedReason.workOrder_Obj     = cHeadWorkOrder
  no-lock no-error.

FOR EACH MBL_SchedReason
  where MBL_SchedReason.M_ServerKennZ_Obj = M_ServerKennZ.M_ServerKennZ_obj
    and MBL_SchedReason.workOrder_Obj     = cHeadWorkOrder
    AND MBL_SchedReason.ctp_ID            = bMBL_SchedReason.ctp_ID
  NO-LOCK
  on error undo, next:

  DISPLAY 
    "cause_key   =" MBL_SchedReason.cause_key   SKIP
    "Reason      =" MBL_SchedReason.Reason      SKIP
    "Quantity    =" MBL_SchedReason.Quantity    SKIP
    "fromDate    =" MBL_SchedReason.fromDate    SKIP
    "toDate      =" MBL_SchedReason.toDate      SKIP
    "ctp_ID      =" MBL_SchedReason.ctp_ID      SKIP
    "-----------------------------------------" SKIP
    .
	
  if MBL_SchedReason.cause_key = "xml" then
	copy-lob from MBL_SchedReason.structure to file 'd:/temp/blob.txt':U no-convert.

END.

