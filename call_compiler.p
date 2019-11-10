COMPILE D:/projects/progress/progress_samples/db_access.p SAVE NO-ERROR.

DEFINE VARIABLE iError AS INTEGER     NO-UNDO.

MESSAGE  
    "Errors: "   COMPILER:ERROR SKIP
    "Warnings: " COMPILER:WARNING SKIP
    "Messages: " COMPILER:NUM-MESSAGES
    VIEW-AS ALERT-BOX INFORMATION.
