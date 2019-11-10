DEFINE VARIABLE iTurn AS INTEGER NO-UNDO.
DEFINE VARIABLE iVal AS INTEGER NO-UNDO.

do iTurn = 1 to 3:
    iVal = 2 * iTurn.
    MESSAGE 'turn=' iTurn skip 'iVAL=' iVal skip 'now=' now VIEW-AS ALERT-BOX.
end.    