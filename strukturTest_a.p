{adm/template/incl/dt_pro00.df}

DEFINE VARIABLE pcCompany   AS CHAR    INIT "110".
DEFINE VARIABLE pcAuftrag   AS CHAR    INIT "LA-80213016".


DEFINE temp-table ttWorkOrderRecursionCandidates no-undo
  field Artikel          like PP_Auftrag.Artikel
  field ArtVar           like PP_Auftrag.ArtVar
  field BaugruppenFolge  like PP_Auftrag.BaugruppenFolge
  index Main is primary unique
        Artikel
        ArtVar
        BaugruppenFolge.
  .
    

  /* ---------------------------------------------------------------------------------- */
  /* prüfe alle Teile auf Rekursion                                                     */
  /* ---------------------------------------------------------------------------------- */

  FOR EACH PP_StkZeile 
    WHERE PP_StkZeile.firma   = {firma/ppauftra.fir pcCompany}
      AND PP_StkZeile.Auftrag = pcAuftrag
    :

    IF CAN-FIND(FIRST S_Artikel
      where S_Artikel.Firma   = {firma/sartikel.fir pcCompany}
        and S_Artikel.Artikel = PP_StkZeile.Artikel
        and S_Artikel.ArtikelArt <  10 ) THEN 

      RUN testStructureForRecursion ( BUFFER PP_StkZeile ).       

  END.
 

/* ---------------------------------------------------------------------------------- */
/* testStructureForRecursion                                                          */
/* ---------------------------------------------------------------------------------- */

PROCEDURE testStructureForRecursion :
  define PARAMETER BUFFER pbPP_StkZeile FOR PP_StkZeile.

    RUN FillttForRecursionTesting (input-output table ttWorkOrderRecursionCandidates,
                                            yes,
                                            pbPP_StkZeile.Auftrag,
                                            pbPP_StkZeile.Artikel,
                                            pbPP_StkZeile.ArtVar,
                                            pbPP_StkZeile.Baugruppenfolge ).

    RUN isPartCausingRecursion (input table ttWorkOrderRecursionCandidates,
                                BUFFER pbPP_StkZeile,  
                                pbPP_StkZeile.Artikel,
                                pbPP_StkZeile.ArtVar,
                                pbPP_StkZeile.Baugruppenfolge).

END.


/* ---------------------------------------------------------------------------------- */
/* FillttForRecursionTesting                                                          */
/* ---------------------------------------------------------------------------------- */

PROCEDURE FillttForRecursionTesting :
  DEFINE INPUT-OUTPUT parameter table for ttWorkOrderRecursionCandidates.
  DEFINE INPUT PARAMETER plEmptyTTforFirstCall as LOGICAL.
  DEFINE INPUT PARAMETER pcTargetOrderNumber   as CHARACTER.
  DEFINE INPUT PARAMETER pcPart                as CHARACTER.
  DEFINE INPUT PARAMETER pcPartVariant         as CHARACTER.
  DEFINE INPUT PARAMETER piAssemblySequence    as INTEGER.

    /* Buffers ---------------------------------------------------------------*/
    define buffer bttWorkOrderRecursionCandidates for temp-table ttWorkOrderRecursionCandidates.

    define buffer bPP_StkZeile           for   PP_StkZeile.
    define buffer bPP_Auftrag-Head       for   PP_Auftrag.
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

        /* get the Bom line that is covered by this Work Order  */

        RUN fillTTForRecursionTesting(input-output table ttWorkOrderRecursionCandidates,
                                               no,
                                               pcTargetOrderNumber,
                                               bPP_Auftrag-Head.Artikel,
                                               bPP_Auftrag-Head.ArtVar,
                                               bPP_Auftrag-Head.BaugruppenFolge).

      end. /* if not can-find (first ttWorkOrderRecrusionCandidates ... ) */

    end. /* for each  bPP_StkZeile */


END.


/* ---------------------------------------------------------------------------------- */
/* isPartCausingRecursion                                                             */
/* ---------------------------------------------------------------------------------- */

PROCEDURE isPartCausingRecursion :
    DEFINE INPUT parameter table for ttWorkOrderRecursionCandidates.
    define PARAMETER BUFFER pbPP_StkZeile FOR PP_StkZeile.
    DEFINE INPUT PARAMETER pcPart               as CHARACTER.
    DEFINE INPUT PARAMETER pcPartVariant        as CHARACTER.
    DEFINE INPUT PARAMETER piAssemblySequenceNo as INTEGER.

    if can-find(first ttWorkOrderRecursionCandidates
                where ttWorkOrderRecursionCandidates.Artikel = pcPart
                  and ttWorkOrderRecursionCandidates.ArtVar  = pcPartVariant) then

      MESSAGE
        'Eine Rekursion wurde festgestellt' SKIP 
        'AuftragsNr   =' pbPP_StkZeile.Auftrag SKIP 
        'RueckMeldeNr =' pbPP_StkZeile.RueckMeldeNr SKIP 
        'Position     =' pbPP_StkZeile.POSITION SKIP
        'Artikel      =' pcPart SKIP
        'Var          =' pcPartVariant SKIP
        'BgrNr        =' string(piAssemblySequenceNo) SKIP
        .

END.
