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



    FOR EACH m_kalender
        WHERE firma = '110' :
        ASSIGN 
            piKalArt    = m_Kalender.KalArt
            pcKalender  = m_Kalender.Kalender
            .


    {adm/incl/d__run03.if
      &Procedure = "mawi/proc/m_ncal05.p"
      &Arguments = "piKalArt,
                    pcKalender,
                    tDatumMin,
                    tDatumMax,
                    tDatumAb,
                    giAnzahl, no, no, no"}

    END.



/* jetzt noch transport und Verfahren erweitern */

    define variable tvonDatum   as date      no-undo.
    define variable tbisDatum   as date      no-undo.
    define variable tStartDatum as date      no-undo.
    define variable tDatum      as date      no-undo.
    define variable iDifferenz  as integer   no-undo.
    define variable i           as integer   no-undo.
    define variable iKalNr      as integer   no-undo init 0.
    define variable lOk         as logical   no-undo.

    define buffer buf_M_KalenderDatum  for M_KalenderDatum.
    define buffer buf2_M_KalenderDatum for M_KalenderDatum.
    define buffer M_Kalender           for M_Kalender.

    assign
      tvonDatum = DATE(6,3,2002)
      tbisDatum = DATE(6,3,2004)
      tStartDatum = TODAY.
      .

      /*
    run mawi/proc/m_pkal20.w (input-output tvonDatum,
                              input-output tbisDatum,
                              output tStartDatum,
                              output lOk).

    RETURN.
        */

    FOR EACH M_Kalender
        where M_Kalender.Firma    = '110'
          and M_Kalender.KalArt   = 4 /* transport */
            OR M_Kalender.KalArt   = 5 /* Verfharen */
        no-lock 
        on error undo , leave :


      iDifferenz = tStartDatum - tvonDatum.

      /* Ermittle den zu prüfenden Betriebskalender */


      if available M_Kalender then
        iKalNr = M_Kalender.Betriebskalender.

      iDifferenz = tStartDatum - tvonDatum.

      /* Alle Kalenderdatenzätze im Zielbereich löschen, auch wenn              */
      /* ein bereits vorhandener Datensatz nicht komplett im Zielbereich liegt, */
      /* sondern sich mit diesem überschneidet.                                 */

      Löschen:
       for each buf_M_KalenderDatum
        where buf_M_KalenderDatum.Firma    = M_Kalender.Firma
          and buf_M_KalenderDatum.KalArt   = M_Kalender.KalArt
          and buf_M_KalenderDatum.Kalender = M_Kalender.Kalender
          and ((buf_M_KalenderDatum.vonDatum <= tvonDatum + iDifferenz
            and buf_M_KalenderDatum.bisDatum >= tvonDatum + iDifferenz)
          or (buf_M_KalenderDatum.vonDatum <= tbisDatum + iDifferenz
            and buf_M_KalenderDatum.bisDatum >= tbisDatum + iDifferenz)
          or (buf_M_KalenderDatum.vonDatum >= tvonDatum + iDifferenz
            and buf_M_KalenderDatum.bisDatum <= tbisDatum + iDifferenz)
          or (buf_M_KalenderDatum.vonDatum <= tvonDatum + iDifferenz
            and buf_M_KalenderDatum.bisDatum >= tbisDatum + iDifferenz))
        exclusive-lock
        on error undo Löschen, leave Löschen:

        delete buf_M_KalenderDatum.

      end. /* Löschen */

      /* Nun werden alle Datensätze die komplett im Bereich liegen kopiert */

      Kopieren:
      for each buf_M_KalenderDatum
        where buf_M_KalenderDatum.Firma    = M_Kalender.Firma
          and buf_M_KalenderDatum.KalArt   = M_Kalender.KalArt
          and buf_M_KalenderDatum.Kalender = M_Kalender.Kalender
          and buf_M_KalenderDatum.vonDatum >= tvonDatum
          and buf_M_KalenderDatum.bisDatum <= tbisDatum
        no-lock
        on error undo Kopieren, leave Kopieren:

        /* Kopiere den Kalender */

        if not can-find(buf2_M_KalenderDatum
          where buf2_M_KalenderDatum.Firma    = buf_M_KalenderDatum.Firma
            and buf2_M_KalenderDatum.KalArt   = buf_M_KalenderDatum.KalArt
            and buf2_M_KalenderDatum.Kalender = buf_M_KalenderDatum.Kalender
            and buf2_M_KalenderDatum.vonDatum = buf_M_KalenderDatum.vonDatum + iDifferenz
            and buf2_M_KalenderDatum.vonZeit  = buf_M_KalenderDatum.vonZeit) then do:

          /* Prüfung auf den Betriebskalender */

          tDatum = (buf_M_KalenderDatum.vonDatum + iDifferenz) - 1.

          do i = 1 to (buf_M_KalenderDatum.bisDatum + iDifferenz) - (buf_M_KalenderDatum.vonDatum + iDifferenz) + 1:

            tDatum = tDatum + 1.


          end. /* do i = 1 to ... */

          /* Datensatz anlegen */

          create buf2_M_KalenderDatum.

          assign
            buf2_M_KalenderDatum.Firma    = buf_M_KalenderDatum.Firma
            buf2_M_KalenderDatum.KalArt   = buf_M_KalenderDatum.KalArt
            buf2_M_KalenderDatum.Kalender = buf_M_KalenderDatum.Kalender
            buf2_M_KalenderDatum.vonDatum = buf_M_KalenderDatum.vonDatum + iDifferenz
            buf2_M_KalenderDatum.vonZeit  = buf_M_KalenderDatum.vonZeit
            .

           validate buf2_M_KalenderDatum.

           assign
             buf2_M_KalenderDatum.bisDatum    = buf_M_KalenderDatum.bisDatum + iDifferenz
             buf2_M_KalenderDatum.bisZeit     = buf_M_KalenderDatum.bisZeit
             buf2_M_KalenderDatum.gebucht     = false
             buf2_M_KalenderDatum.Intensitaet = buf_M_KalenderDatum.Intensitaet
             .

        end. /* if not can-find(buf2_M_KalenderDatum */

      end. /* Kopieren */
    END.
