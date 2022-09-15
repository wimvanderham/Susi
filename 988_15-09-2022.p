
/*------------------------------------------------------------------------
    File        : 988_15-09-2022.p
    Purpose     : 

    Syntax      :

    Description : 988° Quesito con La Susi

    Author(s)   : Wim van der Ham
    Created     : Thu Sep 15 11:13:40 CEST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION isMultiple RETURNS LOGICAL 
   (INPUT ipiNumero AS INTEGER,
    INPUT ipiBase   AS INTEGER) FORWARD.


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE iNumero           AS INTEGER NO-UNDO.
/* Risposta Corretta della domanda */
DEFINE VARIABLE lRispostaCorretta AS LOGICAL NO-UNDO.
/* Risposta Data */
DEFINE VARIABLE lRispostaData     AS LOGICAL NO-UNDO.
/* Risposte OK */
DEFINE VARIABLE iRisposteOK       AS INTEGER NO-UNDO.

DO iNumero = 1 TO 100:
   ASSIGN 
      lRispostaCorretta = isMultiple(iNumero, 4)
      lRispostaData     = isMultiple(iNumero, 3)
   .
   IF lRispostaCorretta EQ lRispostaData THEN
      iRisposteOK = iRisposteOK + 1.
END.

MESSAGE "Risposte OK:" iRisposteOK
VIEW-AS ALERT-BOX. 
   
/* ************************  Function Implementations ***************** */


FUNCTION isMultiple RETURNS LOGICAL 
   (INPUT ipiNumero AS INTEGER,
    INPUT ipiBase   AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Restituisce TRUE se il numero è un multiplo della base
 Notes:
------------------------------------------------------------------------------*/   

   RETURN (ipiNumero MOD ipiBase EQ 0).
      
END FUNCTION. /* isMultiple */

