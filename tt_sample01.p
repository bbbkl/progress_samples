DEFINE TEMP-TABLE TT NO-UNDO
    FIELD nr AS INTEGER.
    
DEFINE VARIABLE iVal AS INTEGER NO-UNDO.
define buffer btTT for temp-table TT.

do iVal = 0 to 10:
    create btTT.
    btTT.nr = iVal.
end.

if available btTT then
    MESSAGE btTT.nr VIEW-AS ALERT-BOX.
//display available btTT.

find first btTT 
  where btTT.nr = 11 no-error.
  
if not available btTT then
    MESSAGE 'not available (11)'  VIEW-AS ALERT-BOX.