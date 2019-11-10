//connect mySportsDb -H localhost -S 9999.
DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.

for EACH Bill no-lock:
    iCount = iCount + 1.
    display Bill.
    //MESSAGE Bill VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
end.
xxx
MESSAGE 'iCount=' iCount VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
