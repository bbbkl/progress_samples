{adm/template/incl/dt_pro00.df}

define variable pcCompany   as char    init "100".
define variable pcAuftrag   as char    init "MH-00005981-0020". 
                                          /* MH-00005981-0030  MH-00005981-0020 */

define variable plFoundFlag  as logical init false. 


define temp-table ttWorkOrderRecursionCandidates no-undo
  field Artikel          like PP_Auftrag.Artikel
  field ArtVar           like PP_Auftrag.ArtVar
  field BaugruppenFolge  like PP_Auftrag.BaugruppenFolge
  index Main is primary unique
        Artikel
        ArtVar
        BaugruppenFolge.
  .
    

define temp-table ttReservationInfo no-undo
  field Artikel              like PP_Auftrag.Artikel
  field ArtVar               like PP_Auftrag.ArtVar
  field BaugruppenFolge      like PP_Auftrag.BaugruppenFolge
  field AuftragBedarf           like PP_Auftrag.Auftrag
  field StuecklistePos       like PP_StkZeile.Position
  field StuecklisteRNr       like PP_Auftrag.RueckMeldenr
  field AktivitaetRNr        like MB_Aktivitaet.Teilprozess
  field AktivitaetPos        like MB_Aktivitaet.AktPos
    
  field AuftragDeckung       like PP_Auftrag.Auftrag
  field RueckMeldeNrDeckung  like PP_Auftrag.RueckMeldenr

  index Main is primary 
        Artikel ArtVar BaugruppenFolge.
  .


  /* ---------------------------------------------------------------------------------- */
  /* prüfe alle Teile auf Rekursion                                                     */
  /* ---------------------------------------------------------------------------------- */

  if not can-find(first PP_Auftrag 
    where PP_Auftrag.firma   = {firma/ppauftra.fir pcCompany}
      and PP_Auftrag.Auftrag = pcAuftrag) then
  do:
  
    message
      'Falsche Auftragsnummer =' pcAuftrag skip
      view-as alert-box
      title program-name(1).  
      
    return.
      
  end.    


  for each PP_StkZeile 
    where PP_StkZeile.firma   = {firma/ppauftra.fir pcCompany}
      and PP_StkZeile.Auftrag = pcAuftrag
    :

    if can-find(first S_Artikel
      where S_Artikel.Firma   = {firma/sartikel.fir pcCompany}
        and S_Artikel.Artikel = PP_StkZeile.Artikel
        and S_Artikel.ArtikelArt <  10 ) then 

      run testStructureForRecursion ( BUFFER PP_StkZeile ).       

  end.

  define variable cOutputfile as character     no-undo.
  cOutputfile = "d:\temp\reservation_info_":U + pcAuftrag + ".xml":U.

  if can-find (first ttReservationInfo ) then
  do: 
    plFoundFlag = true.
    temp-table ttReservationInfo:WRITE-XML('file':U, cOutputfile, false, ?, ?, false).
    message
      'Es sind relevante Reservierungen vorhanden' skip
      'Prüfen sie bitte die Datei: "' + cOutputfile + '"' skip 
      view-as alert-box
      title program-name(1).
  end.

  if not plFoundFlag then  
    message
      'Keine Probleme erkennbar' skip
      view-as alert-box
      title program-name(1).
 

/* ---------------------------------------------------------------------------------- */
/* testStructureForRecursion                                                          */
/* ---------------------------------------------------------------------------------- */

procedure testStructureForRecursion :
  define parameter buffer pbPP_StkZeile for PP_StkZeile.

    run FillttForRecursionTesting (input-output table ttWorkOrderRecursionCandidates,
                                   yes,
                                   pbPP_StkZeile.Auftrag,
                                   pbPP_StkZeile.Artikel,
                                   pbPP_StkZeile.ArtVar,
                                   pbPP_StkZeile.Baugruppenfolge ).

    run isPartCausingRecursion (input table ttWorkOrderRecursionCandidates,
                                BUFFER pbPP_StkZeile,  
                                pbPP_StkZeile.Artikel,
                                pbPP_StkZeile.ArtVar,
                                pbPP_StkZeile.Baugruppenfolge).

end.


/* ---------------------------------------------------------------------------------- */
/* FillttForRecursionTesting                                                          */
/* ---------------------------------------------------------------------------------- */

procedure FillttForRecursionTesting :
  define input-output parameter table for ttWorkOrderRecursionCandidates.
  define input parameter plEmptyTTforFirstCall as logical.
  define input parameter pcTargetOrderNumber   as character.
  define input parameter pcPart                as character.
  define input parameter pcPartVariant         as character.
  define input parameter piAssemblySequence    as integer.

  /* Buffers ---------------------------------------------------------------*/
  define buffer bttWorkOrderRecursionCandidates for temp-table ttWorkOrderRecursionCandidates.
  define buffer bttReservationInfo              for temp-table ttReservationInfo.
  
  define buffer bPP_StkZeile           for   PP_StkZeile.
  define buffer bPP_StkZeile-Deckung   for   PP_StkZeile.
  define buffer bPP_Auftrag-Deckung    for   PP_Auftrag.
  define buffer bPP_Auftrag-Head       for   PP_Auftrag.
  
  define buffer bM_DispoBew-Bedarf     for   M_DispoBew.
  define buffer bM_DispoBew-Deckung    for   M_DispoBew.
  define buffer bM_DispoVerw           for   M_DispoVerw.
  
  define buffer bPP_MatRess            for   PP_MatRess.
  define buffer bMB_Aktivitaet         for   MB_Aktivitaet.           
  
  
  /*------------------------------------------------------------------------*/
  /* Processing                                                             */
  /*------------------------------------------------------------------------*/
  
  /* only empty the TT if the parameter is yes - this is only to empty the  */
  /* tt for the first call of the method, in the recursive call set to no!  */
  
  if plEmptyTTforFirstCall then
     empty temp-table ttWorkOrderRecursionCandidates.
  
  /* Here we can have multiple Bom lines with this Artikel,ArtVar and BgrNr */
  /* and no existing work order that covers this demand, it has to be       */
  /* created. In the other case the Quantity of the existing Work order     */
  /* that covers this new bomLine where already increased. (see             */
  /* cCreateWorkOrderForAssembly)                                           */
  
  /* get each bom line occurence of this part take its head work order and  */
  /* writes it to our TT. */
  
  for each bPP_StkZeile
    where bPP_StkZeile.Firma           = {firma/ppauftra.fir pcCompany}
      and bPP_StkZeile.Auftrag         = pcTargetOrderNumber
      and bPP_StkZeile.Artikel         = pcPart
      and bPP_StkZeile.ArtVar          = pcPartVariant
      and bPP_StkZeile.BaugruppenFolge = piAssemblySequence
    use-index AufVerw
    no-lock
    on error undo, throw.
  
    /* Find the head work order to the current bom lines without taking the */
    /* assemblysequence into account, therefore the difference between the  */
    /* AssSeqNo of bom line and work order head doesn't stop the recursion. */
  
    find bPP_Auftrag-Head
      where bPP_Auftrag-Head.Firma        = {firma/ppauftra.fir pcCompany}
        and bPP_Auftrag-Head.RueckMeldeNr = bPP_StkZeile.RueckMeldeNr
      no-lock.
  
    /* only add Data if not already there                                   */
    /* Here we do not use the method mergeisPartCausingRecursion because    */
    /* we just want to prevent double entries in the TempTable.             */
  
    if not can-find(first ttWorkOrderRecursionCandidates
      where ttWorkOrderRecursionCandidates.Artikel         = bPP_Auftrag-Head.Artikel
        and ttWorkOrderRecursionCandidates.ArtVar          = bPP_Auftrag-Head.ArtVar
        and ttWorkOrderRecursionCandidates.BaugruppenFolge = bPP_Auftrag-Head.BaugruppenFolge ) then
    do:
  
      /* add it to the Temp-Table */
  
      create bttWorkOrderRecursionCandidates.
      assign
        bttWorkOrderRecursionCandidates.Artikel         = bPP_Auftrag-Head.Artikel
        bttWorkOrderRecursionCandidates.ArtVar          = bPP_Auftrag-Head.ArtVar
        bttWorkOrderRecursionCandidates.BaugruppenFolge = bPP_Auftrag-Head.BaugruppenFolge
        .
  
  
      /*----------------------------------------------------------------------------*/
      /* add links informationen to the temp-table                                  */
      /*----------------------------------------------------------------------------*/
  
      for each bM_DispoVerw   /* code checked by Baumann 07.05.2013 */
        where bM_DispoVerw.Firma             = {firma/ppauftra.fir bPP_StkZeile.Firma}
          and bM_DispoVerw.Artikel           = bPP_StkZeile.Artikel
          and bM_DispoVerw.ArtVar            = bPP_StkZeile.ArtVar
          and bM_DispoVerw.Lagergruppe       = bPP_StkZeile.Lagergruppe

          and bM_DispoVerw.Belegart_Bedarf   = (if bPP_Auftrag-Head.Auftragskennung = 'A':U then 'PPN':U  else 'PPA':U)
          and bM_DispoVerw.Schl_Bedarf       = {pps/prod/incl/ppstkzei.sl &Tabelle = "bPP_StkZeile"}

          and bM_DispoVerw.ResStatus         > 0

        no-lock
        on error undo, return error:

        if can-find(first bttReservationInfo
          where bttReservationInfo.Artikel         = bPP_StkZeile.Artikel
            and bttReservationInfo.ArtVar          = bPP_StkZeile.ArtVar
            and bttReservationInfo.BaugruppenFolge = bPP_StkZeile.BaugruppenFolge ) then
          next.

        create bttReservationInfo.
        assign
          bttReservationInfo.Artikel             = bPP_StkZeile.Artikel
          bttReservationInfo.ArtVar              = bPP_StkZeile.ArtVar
          bttReservationInfo.BaugruppenFolge     = bPP_StkZeile.BaugruppenFolge
          bttReservationInfo.AuftragBedarf       = bPP_StkZeile.Auftrag
          bttReservationInfo.StuecklisteRNr      = bPP_StkZeile.RueckMeldeNr
          bttReservationInfo.StuecklistePos      = bPP_StkZeile.Position
          .
        
        /* find Cover Information */
        
        find bM_DispoBew-Deckung
          where bM_DispoBew-Deckung.Firma             = bM_DispoVerw.Firma
            and bM_DispoBew-Deckung.Artikel           = bM_DispoVerw.Artikel
            and bM_DispoBew-Deckung.ArtVar            = bM_DispoVerw.ArtVar
            and bM_DispoBew-Deckung.BelegArt_Herkunft = bM_DispoVerw.BelegArt_Deckung
            and bM_DispoBew-Deckung.Schl_Herkunft     = bM_DispoVerw.Schl_Deckung
          no-lock no-error.

        if available bM_DispoBew-Deckung then
        do:
        
          find bPP_Auftrag-Deckung 
            where bPP_Auftrag-Deckung.Firma        = {firma/ppauftra.fir pcCompany}
              and bPP_Auftrag-Deckung.RueckMeldeNr = integer( entry(1,bM_DispoBew-Deckung.Schl_Herkunft,{&pa-Delimiter3}) )
            no-lock no-error.

          if available bPP_Auftrag-Deckung then 
            assign
              bttReservationInfo.AuftragDeckung       = bPP_Auftrag-Deckung.Auftrag
              bttReservationInfo.RueckMeldeNrDeckung  = bPP_Auftrag-Deckung.RueckMeldeNr
              .
          
        end. /* cover */


        /* load activity demand                                               */

        find first bPP_MatRess
          where bPP_MatRess.Firma    = {firma/ppauftra.fir pcCompany}
            and bPP_MatRess.IdentStk = bPP_StkZeile.IdentStk
          no-lock no-error.

        if available bPP_MatRess then
        do:
          find bMB_Aktivitaet
            where bMB_Aktivitaet.Firma    = {firma/mawib.fir pcCompany}
              and bMB_Aktivitaet.IdentAkt = bPP_MatRess.IdentAkt
            no-lock no-error.
        
          if available bMB_Aktivitaet then 
            assign
              bttReservationInfo.AktivitaetRNr   = bMB_Aktivitaet.Teilprozess
              bttReservationInfo.AktivitaetPos   = bMB_Aktivitaet.AktPos
              .

        end. 

      end. /* end Link search */
  
    
      /*----------------------------------------------------------------------------*/
      /* get the Bom line that is covered by this Work Order                        */
      /*----------------------------------------------------------------------------*/
      
      run fillTTForRecursionTesting(input-output table ttWorkOrderRecursionCandidates,
                                             no,
                                             pcTargetOrderNumber,
                                             bPP_Auftrag-Head.Artikel,
                                             bPP_Auftrag-Head.ArtVar,
                                             bPP_Auftrag-Head.BaugruppenFolge).
  
    end. /* if not can-find (first ttWorkOrderRecrusionCandidates ... ) */
  
  end. /* for each  bPP_StkZeile */

end. /* end procedure */


/* ---------------------------------------------------------------------------------- */
/* isPartCausingRecursion                                                             */
/* ---------------------------------------------------------------------------------- */

procedure isPartCausingRecursion :
    define input parameter table for ttWorkOrderRecursionCandidates.
    define parameter buffer pbPP_StkZeile for PP_StkZeile.
    define input parameter pcPart               as character.
    define input parameter pcPartVariant        as character.
    define input parameter piAssemblySequenceNo as integer.

    if can-find(first ttWorkOrderRecursionCandidates
                where ttWorkOrderRecursionCandidates.Artikel = pcPart
                  and ttWorkOrderRecursionCandidates.ArtVar  = pcPartVariant) then
    do:              

      message
        'Eine Rekursion wurde festgestellt' skip 
        'AuftragsNr   =' pbPP_StkZeile.Auftrag skip 
        'RueckMeldeNr =' pbPP_StkZeile.RueckMeldeNr skip 
        'Position     =' pbPP_StkZeile.POSITION skip
        'Artikel      =' pcPart skip
        'Var          =' pcPartVariant skip
        'BgrNr        =' string(piAssemblySequenceNo) skip
        .

      plFoundFlag = true.

    end.

end.
