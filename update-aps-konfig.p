FIND FIRST M_Server WHERE firma='110':U EXCLUSIVE-LOCK.

M_Server.SERVERid='apsdemo'.
VALIDATE M_Server.

FIND FIRST BJ_Job WHERE BJ_Job.Jobnr = 1.

BJ_Job.QueueTyp = 'apsdemo'.
