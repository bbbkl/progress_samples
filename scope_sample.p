DEFINE TEMP-TABLE TT NO-UNDO
    FIELD nr AS INTEGER.
    
DEFINE VARIABLE iVal AS INTEGER NO-UNDO.
define buffer btTT for temp-table TT.

PROCEDURE find7:
  find first btTT 
    where btTT.nr = 7
    no-error.    
END PROCEDURE.

do iVal = 0 to 10:
  create btTT.
  btTT.nr = iVal.
end.

if available btTT then 
  MESSAGE 'tt nr=' btTT.nr  VIEW-AS ALERT-BOX.
  
run find7.  
  
if available btTT then 
  MESSAGE 'tt nr=' btTT.nr  VIEW-AS ALERT-BOX.