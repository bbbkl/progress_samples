
/*------------------------------------------------------------------------
    File        : _call236342.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jul 21 18:32:21 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

block-level on error undo, throw.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
define buffer bS_Firma           for S_Firma.
define buffer bPMM_DrawingMaster for PMM_DrawingMaster.
define buffer bPMM_Drawing       for PMM_Drawing.

for each bS_Firma
  no-lock
  on error undo, throw:

  /* first create the new drawing master record for each found drawing        */

  for each bPMM_DrawingMaster
    where bPMM_DrawingMaster.Company = {firma/pps.fir bS_Firma.Firma}
    no-lock
    on error undo, throw:


      /* Write the Masters ObjectID in each historized version of the Drawing */

      for each bPMM_Drawing
        where bPMM_Drawing.Company        = {firma/pps.fir bS_Firma.Firma}
          and bPMM_Drawing.PMM_Drawing_ID = bPMM_DrawingMaster.PMM_Drawing_ID
        exclusive-lock
        on error undo, throw:

        bPMM_Drawing.PMM_DrawingMaster_Obj = bPMM_DrawingMaster.PMM_DrawingMaster_Obj.

      end. /* for each bPMM_Drawing-Hist */
  end. /*  for each bPMM_DrawingMaster */
end. /* for each bS_Firma */
