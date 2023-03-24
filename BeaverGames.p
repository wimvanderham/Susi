
/*------------------------------------------------------------------------
    File        : BeaverGames.p
    Purpose     : 

    Syntax      :

    Description : Soluzione per Esercizio Beaver Games

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Mar 24 15:21:55 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttPlayer
   FIELD IDPlayer AS INTEGER 
   FIELD Nome     AS CHARACTER FORMAT "X(20)"
INDEX indID IS UNIQUE IDPlayer
INDEX indNome IS PRIMARY Nome.

DEFINE TEMP-TABLE ttImpresa
   FIELD IDImpresa      AS INTEGER 
   FIELD NomeImpresa    AS CHARACTER FORMAT "X(20)"
   FIELD PlayerxSquadra AS INTEGER 
INDEX indID IS UNIQUE IDImpresa.

DEFINE TEMP-TABLE ttPlayerPoints
   FIELD IDPlayer  AS INTEGER 
   FIELD IDImpresa AS INTEGER 
   FIELD Punti     AS INTEGER 
INDEX indID IS UNIQUE IDPlayer IDImpresa.

DEFINE TEMP-TABLE ttImpresaPlayer
   FIELD IDImpresa    AS INTEGER 
   FIELD SquadraVince AS LOGICAL 
   FIELD Posizione    AS INTEGER 
   FIELD IDPlayer     AS INTEGER
   FIELD Punti        AS INTEGER 
   FIELD ListaPlayers AS CHARACTER 
INDEX indID IS UNIQUE IDImpresa SquadraVince Posizione.

DEFINE VARIABLE lOk      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPlayer  AS INTEGER   NO-UNDO.
   
DEFINE BUFFER ttImpresaPlayerVince FOR ttImpresaPlayer.
DEFINE BUFFER ttImpresaPlayerPerde FOR ttImpresaPlayer.
   
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN fillTTs
   (INPUT "Ada:15;20;10,Brown:16;27;14,Candy:19;30;11,Daisy:18;24;15,Eden:17;28;16,Funny:17;28;16,George:19;30;9,Hough:19;30;12",
    INPUT "Impresa 1:4,Impresa 2:2,Impresa 3:1").
    
RUN setPlayer
   (INPUT  "Impresa 1",
    INPUT  TRUE,
    INPUT  1,
    INPUT  "Ada",
    OUTPUT lOk,
    OUTPUT cMessage).
IF lOk EQ FALSE THEN DO:
   MESSAGE cMessage
   VIEW-AS ALERT-BOX WARNING.
   RETURN.
END.
    
RUN setPlayer
   (INPUT  "Impresa 2",
    INPUT  TRUE,
    INPUT  1,
    INPUT  "Ada",
    OUTPUT lOk,
    OUTPUT cMessage).
IF lOk EQ FALSE THEN DO:
   MESSAGE cMessage
   VIEW-AS ALERT-BOX WARNING.
   RETURN.
END.
    
RUN setPlayer
   (INPUT  "Impresa 3",
    INPUT  TRUE,
    INPUT  1,
    INPUT  "Ada",
    OUTPUT lOk,
    OUTPUT cMessage).
IF lOk EQ FALSE THEN DO:
   MESSAGE cMessage
   VIEW-AS ALERT-BOX WARNING.
   RETURN.
END.

FOR EACH ttImpresa
BY ttImpresa.PlayerxSquadra:
   /* Cerca avversario per Impresa 3 */
   DO iPlayer = 1 TO ttImpresa.PlayerxSquadra:
      FOR EACH ttImpresaPlayerVince
      WHERE    ttImpresaPlayerVince.IDImpresa    EQ ttImpresa.IDImpresa
      AND      ttImpresaPlayerVince.SquadraVince EQ TRUE 
      AND      ttImpresaPlayerVince.Posizione    EQ iPlayer:
         FOR EACH ttImpresaPlayerPerde
         WHERE ttImpresaPlayerPerde.IDImpresa    EQ ttImpresa.IDImpresa
         AND   ttImpresaPlayerPerde.SquadraVince EQ (NOT ttImpresaPlayerVince.SquadraVince)
         AND   ttImpresaPlayerPerde.Posizione    EQ ttImpresaPlayerVince.Posizione:
            FOR EACH ttPlayer,
            FIRST ttPlayerPoints OF ttPlayer
            WHERE ttPlayerPoints.IDImpresa EQ ttImpresa.IDImpresa:
               IF ttPlayer.IDPlayer EQ ttImpresaPlayerVince.IDPlayer THEN 
                  NEXT.
               IF ttPlayerPoints.Punti GT ttImpresaPlayerVince.Punti THEN 
                  NEXT.
               ASSIGN 
                  ttImpresaPlayerPerde.ListaPlayers = 
                     SUBSTITUTE ("&1&2&3",
                                 ttImpresaPlayerPerde.ListaPlayers,
                                 (IF ttImpresaPlayerPerde.ListaPlayers EQ "" THEN "" ELSE ","),
                                 ttPlayer.IDPlayer)
               .                                 
            END.
         END.
      END.
   END.
   FOR EACH ttImpresaPlayer
   WHERE ttImpresaPlayer.IDImpresa EQ ttImpresa.IDImpresa
   AND   ttImpresaPlayer.IDPlayer  EQ 0
   AND   NUM-ENTRIES (ttImpresaPlayer.ListaPlayers) EQ 1:
      /* Solo una possibilità per questa posizione, assegnalo */
      FIND  ttPlayer
      WHERE ttPlayer.IDPlayer EQ INTEGER (ttImpresaPlayer.ListaPlayers).
      RUN setPlayer
         (INPUT  ttImpresa.NomeImpresa,
          INPUT  ttImpresaPlayer.SquadraVince,
          INPUT  ttImpresaPlayer.Posizione,
          INPUT  ttPlayer.Nome,
          OUTPUT lOk,
          OUTPUT cMessage).
      IF lOk EQ FALSE THEN DO:
         MESSAGE cMessage
         VIEW-AS ALERT-BOX WARNING.
         RETURN.
      END.
   END.
   
   DISPLAY 
   ttImpresa.NomeImpresa SKIP
   WITH STREAM-IO NO-LABELS.
   FOR EACH ttImpresaPlayer OF ttImpresa,
   FIRST ttPlayer OF ttImpresaPlayer
   BREAK 
   BY ttImpresaPlayer.SquadraVince DESCENDING 
   BY ttImpresaPlayer.Posizione
   WITH FRAME fr-Player DOWN STREAM-IO TITLE " Giocatori ":
      IF FIRST-OF (ttImpresaPlayer.SquadraVince) 
      AND ttImpresaPlayer.SquadraVince EQ FALSE THEN DO:
         DISPLAY 
         "BATTE" @ ttPlayer.Nome
         WITH FRAME fr-Player.
         DOWN 3 WITH FRAME fr-Player.
      END.
      DISPLAY 
      ttPlayer.Nome
      ttImpresaPlayer.Punti (SUB-TOTAL BY ttImpresaPlayer.SquadraVince)
      WITH FRAME fr-Player.
   END.

   LEAVE.
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE fillTTs:
/*------------------------------------------------------------------------------
 Purpose: Riempire Temp-tables con situazione iniziale 
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcListaPlayers  AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER ipcListaImprese  AS CHARACTER NO-UNDO.  

DEFINE VARIABLE iPlayer  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPlayer  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPunti   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPunti   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iID      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iImpresa AS INTEGER   NO-UNDO.
DEFINE VARIABLE cImpresa AS CHARACTER NO-UNDO.
DEFINE VARIABLE lVince   AS LOGICAL NO-UNDO.

DEFINE BUFFER ttPlayer        FOR ttPlayer.
DEFINE BUFFER ttImpresa       FOR ttImpresa.
DEFINE BUFFER ttPlayerPoints  FOR ttPlayerPoints.
DEFINE BUFFER ttImpresaPlayer FOR ttImpresaPlayer.

   iID = 0.
   DO iPlayer = 1 TO NUM-ENTRIES (ipcListaPlayers):
      iID = iID + 1.
      cPlayer = ENTRY (iPlayer, ipcListaPlayers).
      
      CREATE ttPlayer.
      ASSIGN 
         ttPlayer.IDPlayer = iID
         ttPlayer.Nome     = ENTRY (1, cPlayer, ":")
      .
      
      cPunti = ENTRY (2, cPlayer, ":").
      DO iPunti = 1 TO NUM-ENTRIES (cPunti, ";"):
         CREATE ttPlayerPoints.
         ASSIGN 
            ttPlayerPoints.IDImpresa = iPunti
            ttPlayerPoints.IDPlayer  = ttPlayer.IDPlayer
            ttPlayerPoints.Punti     = INTEGER (ENTRY (iPunti, cPunti, ";"))
         .
      END.
   END.
   
   iID = 0.
   DO iImpresa = 1 TO NUM-ENTRIES (ipcListaImprese):
      iID = iID + 1.
      cImpresa = ENTRY (iImpresa, ipcListaImprese).
      CREATE ttImpresa.
      ASSIGN 
         ttImpresa.IDImpresa      = iID
         ttImpresa.NomeImpresa    = ENTRY (1, cImpresa, ":")
         ttImpresa.PlayerxSquadra = INTEGER (ENTRY (2, cImpresa, ":"))
      .
   END.

   FOR EACH ttImpresa:
      lVince = ?.
      REPEAT:
         IF lVince EQ ? THEN 
            lVince = TRUE.
         ELSE 
            IF lVince EQ TRUE THEN 
               lVince = FALSE.
            ELSE
               LEAVE.
         DO iPlayer = 1 TO ttImpresa.PlayerxSquadra:
            CREATE ttImpresaPlayer.
            ASSIGN 
               ttImpresaPlayer.IDImpresa = ttImpresa.IDImpresa
               ttImpresaPlayer.SquadraVince = lVince
               ttImpresaPlayer.Posizione    = iPlayer
            .
         END.
      END.
   END.
   
END PROCEDURE.

PROCEDURE setPlayer:
/*------------------------------------------------------------------------------
 Purpose: Fissa un giocatore in una posizione nell'impresa
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcImpresa   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplVince     AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPosizione AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcNome      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplOk        AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.

DEFINE BUFFER ttImpresa       FOR ttImpresa.
DEFINE BUFFER ttPlayer        FOR ttPlayer.
DEFINE BUFFER ttImpresaPlayer FOR ttImpresaPlayer.

   FIND  ttImpresa       
   WHERE ttImpresa.NomeImpresa EQ ipcImpresa.
   FIND  ttPlayer        
   WHERE ttPlayer.Nome         EQ ipcNome.
   FIND  ttPlayerPoints
   WHERE ttPlayerPoints.IDImpresa EQ ttImpresa.IDImpresa
   AND   ttPlayerPoints.IDPlayer  EQ ttPlayer.IDPlayer.

   FIND  ttImpresaPlayer 
   WHERE ttImpresaPlayer.IDImpresa    EQ ttImpresa.IDImpresa
   AND   ttImpresaPlayer.SquadraVince EQ iplVince
   AND   ttImpresaPlayer.Posizione    EQ ipiPosizione.
   ASSIGN 
      ttImpresaPlayer.IDPlayer = ttPlayer.IDPlayer
      ttImpresaPlayer.Punti    = ttPlayerPoints.Punti      
   .
   
   ASSIGN 
      oplOk = TRUE 
      opcMessage = SUBSTITUTE ("Assigned Player '&1' to '&2' in Posizione '&3'.",
                               ttPlayer.Nome,
                               ttImpresa.NomeImpresa,
                               ttImpresaPlayer.Posizione)
   .                               

   CATCH e AS Progress.Lang.Error :
      ASSIGN 
         oplOk = FALSE 
         opcMessage = SUBSTITUTE ("&1",
                                  e:GetMessage(1))
      .
   END CATCH.

END PROCEDURE.

