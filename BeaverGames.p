
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
   FIELD IDImpresa        AS INTEGER 
   FIELD NomeImpresa      AS CHARACTER FORMAT "X(20)"
   FIELD PlayerxSquadra   AS INTEGER 
   FIELD ListaPlayerVince AS CHARACTER
   FIELD ListaPlayerPerde AS CHARACTER 
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
INDEX indID IS UNIQUE IDImpresa SquadraVince Posizione.

DEFINE VARIABLE lOk      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPlayer  AS INTEGER   NO-UNDO.
   
DEFINE BUFFER ttImpresaPlayerVince FOR ttImpresaPlayer.
DEFINE BUFFER ttImpresaPlayerPerde FOR ttImpresaPlayer.
DEFINE BUFFER ttPrevImpresa        FOR ttImpresa.       
   
DEFINE VARIABLE lvcListaPlayers AS CHARACTER NO-UNDO.

DEFINE VARIABLE iPunteggioVince AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPunteggioPerde AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSquadraOK      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iSquadra        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSquadra        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrevImpresa    AS CHARACTER NO-UNDO.
   
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getCombinazioni RETURNS CHARACTER 
(INPUT ipiNrPlayer     AS INTEGER,
 INPUT ipcListaPlayers AS CHARACTER,
 INPUT ipcListaExclude AS CHARACTER) FORWARD.

FUNCTION getPoints RETURNS INTEGER 
(INPUT ipcImpresa      AS CHARACTER,
 INPUT ipcListaPlayers AS CHARACTER,
 INPUT ipcDelimiter    AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

RUN fillTTs
   (INPUT "Ada:15;20;10,Brown:16;27;14,Candy:19;30;11,Daisy:18;24;15,Eden:17;28;16,Funny:20;24;13,George:19;30;9,Hough:19;30;12",
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


/*/* TEST FUNCTION getCombinazioni */                                       */
/*lvcListaPlayers = getCombinazioni(1, "", "").                             */
/*MESSAGE "1" SKIP                                                          */
/*SUBSTITUTE ("#&1: &2", NUM-ENTRIES (lvcListaPlayers), lvcListaPlayers)    */
/*VIEW-AS ALERT-BOX.                                                        */
/*                                                                          */
/*lvcListaPlayers = getCombinazioni(1, "", "1").                            */
/*MESSAGE "1 exclude 1" SKIP                                                */
/*SUBSTITUTE ("#&1: &2", NUM-ENTRIES (lvcListaPlayers), lvcListaPlayers)    */
/*VIEW-AS ALERT-BOX.                                                        */
/*                                                                          */
/*lvcListaPlayers = getCombinazioni(2, "", "").                             */
/*MESSAGE "2" SKIP                                                          */
/*SUBSTITUTE ("#&1: &2", NUM-ENTRIES (lvcListaPlayers), lvcListaPlayers)    */
/*VIEW-AS ALERT-BOX.                                                        */
/*                                                                          */
/*                                                                          */
/*lvcListaPlayers = getCombinazioni(3, "", "").                             */
/*MESSAGE "3" SKIP                                                          */
/*SUBSTITUTE ("#&1: &2", NUM-ENTRIES (lvcListaPlayers), lvcListaPlayers, "")*/
/*VIEW-AS ALERT-BOX.                                                        */
/*                                                                          */
/*                                                                          */
/*lvcListaPlayers = getCombinazioni(1, "1", "").                            */
/*MESSAGE "Add 1 a 1" SKIP                                                  */
/*SUBSTITUTE ("#&1: &2", NUM-ENTRIES (lvcListaPlayers), lvcListaPlayers, "")*/
/*VIEW-AS ALERT-BOX.                                                        */
/*                                                                          */
/*lvcListaPlayers = getCombinazioni(3, "1", "").                            */
/*MESSAGE "Impresa 1, possibili squadre con Ada" SKIP                       */
/*SUBSTITUTE ("#&1: &2", NUM-ENTRIES (lvcListaPlayers), lvcListaPlayers, "")*/
/*VIEW-AS ALERT-BOX.                                                        */


OUTPUT TO "BeaverGames_soluzione.txt".

FOR EACH ttImpresa
BY ttImpresa.PlayerxSquadra:
   /* Cerca avversari per Impresa, dal "meno" popoloso in su */
   ASSIGN 
      ttImpresa.ListaPlayerVince = ""
      ttImpresa.ListaPlayerPerde = ""
   .
   IF cPrevImpresa NE "" THEN DO:
      /* Spostare tutti i partecipanti dell'impresa precedente
      ** all'impresa da valutare come squadra vincente
      */
      FIND ttPrevImpresa WHERE ttPrevImpresa.NomeImpresa EQ cPrevImpresa.
      FOR EACH ttImpresaPlayer
      WHERE ttImpresaPlayer.IDImpresa EQ ttPrevImpresa.IDImpresa,
      FIRST ttPlayer OF ttImpresaPlayer
      BY ttImpresaPlayer.SquadraVince DESCENDING 
      BY ttImpresaPlayer.Posizione:
         /* Tutti i giocatori vincente dell'Impresa precedente */
         ACCUM "" (COUNT).
         FIND  ttImpresaPlayerVince
         WHERE ttImpresaPlayerVince.IDImpresa    EQ ttImpresa.IDImpresa
         AND   ttImpresaPlayerVince.SquadraVince EQ TRUE
         AND   ttImpresaPlayerVince.Posizione    EQ (ACCUM COUNT "").
         IF ttImpresaPlayerVince.IDPlayer EQ 0 THEN DO:
            /* Player non ancora assegnato */
            RUN setPlayer
               (INPUT  ttImpresa.NomeImpresa,
                INPUT  ttImpresaPlayerVince.SquadraVince,
                INPUT  ttImpresaPlayerVince.Posizione,
                INPUT  ttPlayer.Nome,
                OUTPUT lOk,
                OUTPUT cMessage).         
            IF lOk EQ FALSE THEN DO:
               MESSAGE cMessage
               VIEW-AS ALERT-BOX WARNING.
               RETURN.
            END.               
         END. /* Player non ancora assegnato */
         ELSE DO:
            /* Player già assegnato */
            IF ttImpresaPlayerVince.IDPlayer NE ttPlayer.IDPlayer THEN DO:
               /* Ma non corretto */
               MESSAGE 
               SUBSTITUTE ("Giocatore in posizione &1 nella squadra vincente dell'&2 non corrisponde ad un precedente 'setPlayer'.",
                           (ACCUM COUNT ""),
                           ttImpresa.NomeImpresa) SKIP 
               SUBSTITUTE ("IDGiocatore assegnato: &1, da assegnare: &2.",
                           ttImpresaPlayerVince.IDPlayer,
                           ttPlayer.IDPlayer)                           
               VIEW-AS ALERT-BOX.
               RETURN.
            END. /* Ma non corretto */
         END. /* Player già assegnato */      
      END. /* Tutti i giocatori vincente dell'Impresa precedente */
   END. /* Spostare tutti i partecipanti dell'impresa precedente */      
   
   /* Valutare giocatori della squadra che vince */
   lSquadraOK = TRUE.
   SquadraVinceBlock:
   FOR EACH ttImpresaPlayerVince
   WHERE ttImpresaPlayerVince.IDImpresa    EQ ttImpresa.IDImpresa
   AND   ttImpresaPlayerVince.SquadraVince EQ TRUE:
      IF ttImpresaPlayerVince.IDPlayer EQ 0 THEN DO:
         /* Squadra non completa, impossibile stabilire l'avversario */
         lSquadraOK = FALSE.
         LEAVE SquadraVinceBlock.
      END.
      ttImpresa.ListaPlayerVince = SUBSTITUTE ("&1&2&3",
                                               ttImpresa.ListaPlayerVince,
                                               (IF ttImpresa.ListaPlayerVince NE "" THEN "," ELSE ""),
                                               ttImpresaPlayerVince.IDPlayer).
   END.
    
   IF lSquadraOK EQ FALSE THEN DO:
      MESSAGE "Squadra che Vince non è al completo. Lista dei giocatori scelti:"
      ttImpresa.ListaPlayerVince
      VIEW-AS ALERT-BOX.    
      RETURN.
   END.
   
   iPunteggioVince = getPoints(ttImpresa.NomeImpresa, ttImpresa.ListaPlayerVince, ",").
   
   /* Trovare tutte le possibili combinazioni di squadre avversarie */
   lvcListaPlayers = getCombinazioni(ttImpresa.PlayerxSquadra, "", ttImpresa.ListaPlayerVince).
   
   DO iSquadra = 1 TO NUM-ENTRIES (lvcListaPlayers):
      cSquadra = ENTRY (iSquadra, lvcListaPlayers).
      IF getPoints(ttImpresa.NomeImpresa, cSquadra, ";") LT iPunteggioVince THEN DO:
         /* Trovato una squadra che perde */
         ttImpresa.ListaPlayerPerde = SUBSTITUTE ("&1&2&3",
                                                  ttImpresa.ListaPlayerPerde,
                                                  (IF ttImpresa.ListaPlayerPerde NE "" THEN "," ELSE ""),
                                                  cSquadra).
      END.
   END.

   IF  ttImpresa.ListaPlayerPerde NE ""
   AND NUM-ENTRIES (ttImpresa.ListaPlayerPerde) EQ 1 THEN DO:
      FOR EACH ttImpresaPlayer
      WHERE ttImpresaPlayer.IDImpresa    EQ ttImpresa.IDImpresa
      AND   ttImpresaPlayer.SquadraVince EQ FALSE
      BY ttImpresaPlayer.Posizione:
         FIND  ttPlayer
         WHERE ttPlayer.IDPlayer EQ INTEGER (ENTRY (ttImpresaPlayer.Posizione, ttImpresa.ListaPlayerPerde, ";")).
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
   END.
   ELSE DO:
      MESSAGE "Nessuna squadra avversaria trovata." SKIP 
      ttImpresa.ListaPlayerPerde
      VIEW-AS ALERT-BOX.        
      RETURN.
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
         IF ttImpresa.PlayerxSquadra EQ 1 THEN 
            DISPLAY 
            "BATTE" @ ttPlayer.Nome
            WITH FRAME fr-Player.
         ELSE 
            DISPLAY
            "BATTONO" @ ttPlayer.Nome
             WITH FRAME fr-Player.
         DOWN 3 WITH FRAME fr-Player.
      END.
      DISPLAY 
      ttPlayer.Nome
      ttImpresaPlayer.Punti (SUB-TOTAL BY ttImpresaPlayer.SquadraVince)
      WITH FRAME fr-Player.
      DOWN 1 WITH FRAME fr-Player.
   END.
   
   cPrevImpresa = ttImpresa.NomeImpresa.
   
END.
OUTPUT CLOSE.

RUN sy\win\show-file.w
   (INPUT "BeaverGames_soluzione.txt").
   

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
               ttImpresaPlayer.IDImpresa    = ttImpresa.IDImpresa
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
      opcMessage = SUBSTITUTE ("Assigned Player '&1' to '&2' alla Squadra che &3 in Posizione '&4'.",
                               ttPlayer.Nome,
                               ttImpresa.NomeImpresa,
                               (IF ttImpresaPlayer.SquadraVince THEN "vince" ELSE "perde"),
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


/* ************************  Function Implementations ***************** */

FUNCTION getCombinazioni RETURNS CHARACTER 
(INPUT ipiNrPlayer     AS INTEGER,
 INPUT ipcListaPlayers AS CHARACTER,
 INPUT ipcListaExclude AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Estrazione di un numero di player dal campione di tutti i giocatori 
          con esclusione di una lista (di IDPlayer)
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttPlayer FOR ttPlayer.

DEFINE VARIABLE iLista       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSubLista    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNewLista    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMinIDPlayer AS INTEGER NO-UNDO.

   IF ipcListaPlayers EQ "" THEN DO:
      FOR EACH ttPlayer:
         IF LOOKUP (STRING (ttPlayer.IDPlayer), ipcListaExclude) NE 0 THEN
            /* Escludere questo Player */
            NEXT.
         ipcListaPlayers = SUBSTITUTE ("&1&2&3",
                                       ipcListaPlayers,
                                       (IF ipcListaPlayers NE "" THEN "," ELSE ""),
                                       ttPlayer.IDPlayer).
      END.
   END.
   ELSE DO:
      cNewLista = "".
      ListaBlock:
      DO iLista = 1 TO NUM-ENTRIES (ipcListaPlayers):
         cSubLista = ENTRY (iLista, ipcListaPlayers).
         iMinIDPlayer = INTEGER (ENTRY (NUM-ENTRIES (cSubLista, ";"), cSubLista, ";")).
         FOR EACH ttPlayer
         /* Considerare solo Player più avanti nella lista */
         WHERE ttPlayer.IDPlayer GT iMinIDPlayer:
            IF LOOKUP (STRING (ttPlayer.IDPlayer), ipcListaExclude) NE 0 THEN
               /* Escludere questo Player */
               NEXT.
            
            IF LOOKUP(STRING (ttPlayer.IDPlayer), cSubLista, ";") EQ 0 THEN DO:
               cNewLista = SUBSTITUTE ("&1&2&3",
                                       cNewLista,
                                       (IF cNewLista NE "" THEN "," ELSE ""),
                                       SUBSTITUTE ("&1;&2",
                                                   cSubLista,
                                                   ttPlayer.IDPlayer)).
            END.
         END.                                                                  
      END.
      ipcListaPlayers = cNewLista.
   END.
   IF ipiNrPlayer GT 1 THEN DO:
      ipcListaPlayers = getCombinazioni(ipiNrPlayer - 1, ipcListaPlayers, ipcListaExclude).
   END.
   
   RETURN ipcListaPlayers.      

END FUNCTION. /* getCombinazioni */


FUNCTION getPoints RETURNS INTEGER 
(INPUT ipcImpresa      AS CHARACTER,
 INPUT ipcListaPlayers AS CHARACTER,
 INPUT ipcDelimiter    AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Restituire punteggio di una lista di giocatori in un'impresa
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttPlayer       FOR ttPlayer.
DEFINE BUFFER ttImpresa      FOR ttImpresa.
DEFINE BUFFER ttPlayerPoints FOR ttPlayerPoints.

DEFINE VARIABLE iPlayer    AS INTEGER NO-UNDO.
DEFINE VARIABLE iIDPlayer  AS INTEGER NO-UNDO.
DEFINE VARIABLE iTotPoints AS INTEGER NO-UNDO.

   FIND  ttImpresa
   WHERE ttImpresa.NomeImpresa EQ ipcImpresa.
   DO iPlayer = 1 TO NUM-ENTRIES (ipcListaPlayers, ipcDelimiter):
      iIDPlayer = INTEGER (ENTRY (iPlayer, ipcListaPlayers, ipcDelimiter)).
      FIND  ttPlayer
      WHERE ttPlayer.IDPlayer EQ iIDPlayer.
      FIND  ttPlayerPoints
      WHERE ttPlayerPoints.IDImpresa EQ ttImpresa.IDImpresa
      AND   ttPlayerPoints.IDPlayer  EQ ttPlayer.IDPlayer.
      iTotPoints = iTotPoints + ttPlayerPoints.Punti.
   END.
   
   RETURN iTotPoints.
   
END FUNCTION. /* getPoints */
