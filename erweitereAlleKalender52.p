{adm/template/incl/dt_dlg00.df}



define variable pcKalender  as character no-undo.
define variable piKalArt    as integer   no-undo.
define variable ptDate      as date      no-undo.

define variable givonjahr    as integer   no-undo.
define variable givonkw    as integer   no-undo.
define variable giAbJahr    as integer   no-undo.
define variable giAbKW    as integer   no-undo.
define variable giAnzahl    as integer   no-undo.




/* LOKALE OBJEKTE -----------------------------------------------------------*/
/* Variable ******************************************************************/
/*   Name               Funktion                                             */
/*   ------------------ ---------------------------------------------------- */
/*****************************************************************************/

define variable hndAppServer as handle no-undo.

  define variable lTemp        as logical no-undo.
  define variable tDatumMin    as date    no-undo.
  define variable tDatumMax    as date    no-undo.
  define variable tDatumAb     as date    no-undo.
  define variable cKWS as character no-undo.
  define variable cKW as character no-undo.

  ptDate = today.

  cKWS = {fnarg
           pa_cDateWeekOfDate
           "date( 6, 20, 2005 )"}.

 cKW  = {fnarg
           pa_cDateWeekOfDate
           "ptDate"}.

  assign
    givonjahr = integer(substring(cKWS,1,4))
    givonkw   = integer(substring(cKWs,5,2))
  .

  assign
    giAbJahr  = integer(substring(ckw,1,4))
    giAbKW    = integer(substring(ckw,5,2))
    giAnzahl  = 500.
  .


  /* bestimme die Datumswerte ( tDatumMin / tDatumMax / tDatumAb) */
  /* ------------------------------------------------------------ */
  tDatumMin = {fnarg
                  pa_tDateDateOfWeek
                  "giVonKW,
                   giVonJahr"}.



  tDatumMax = tDatumMin + 6.


  tDatumAb = {fnarg
               pa_tDateDateOfWeek
               "giAbKW,
                giAbJahr"}.




    /* aufrufen des Kopierprogramms im Server */
    /* -------------------------------------- */
    {adm/incl/d__run00.if
      &Programm = "mawi/proc/m_aaps01.p"
      &Handle   = "hndAppServer"
      }

    if valid-handle(hndAppServer) then do:

        FOR EACH m_kalender :
            ASSIGN 
                piKalArt    = m_Kalender.KalArt
                pcKalender  = m_Kalender.Kalender
                .
    
          {adm/incl/d__run02.if
            &AppSvHandle = "hndAppServer"
            &Procedure   = "kopiereResKalenderWochen"
            &Arguments   = "(pa-Firma,
                             piKalArt,
                             pcKalender,
                             tdatummin,
                             tdatummax,
                             tdatumab,
                             giAnzahl)"
          }
        END.

    end.

  /* Daten werden nicht kopiert */
  /* -------------------------- */
