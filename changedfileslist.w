&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

    File        : changedfileslist.w
    Description : Create a list of files changed since a given date
    Author      : David Zadrozny
    Created     : 18-Feb-2022
    Notes       :
    
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

/* I/O Stream Definitions ---                                           */
DEFINE STREAM sFileIn.
DEFINE STREAM sFileOut.

/* Temp Table Definitions ---                                           */
DEFINE TEMP-TABLE ttSubDir NO-UNDO
    FIELD DirPath AS CHARACTER
    FIELD Active AS LOGICAL.

DEFINE TEMP-TABLE ttFile NO-UNDO
    FIELD DirPath AS CHARACTER
    FIELD FileName AS CHARACTER
    FIELD ModDate AS DATE
    FIELD ModTime AS DECIMAL
    INDEX Idx1 IS PRIMARY  ModDate ModTime FileName.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cPath dtModSince lSub btnOK btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cPath dtModSince lSub 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 14 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     LABEL "&OK" 
     SIZE 14 BY 1.05
     BGCOLOR 8 .

DEFINE VARIABLE dtModSince AS DATE FORMAT "99/99/9999":U 
     LABEL "Modified Since" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE cPath AS CHARACTER FORMAT "X(256)":U 
     LABEL "Directory" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE lSub AS LOGICAL INITIAL yes 
     LABEL "Scan subdirectories" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fFrame
     cPath AT ROW 1.95 COL 17 COLON-ALIGNED WIDGET-ID 8
     dtModSince AT ROW 3.38 COL 17 COLON-ALIGNED WIDGET-ID 2
     lSub AT ROW 4.81 COL 19 WIDGET-ID 10
     btnOK AT ROW 6.48 COL 19 WIDGET-ID 6
     btnCancel AT ROW 6.48 COL 34 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72 BY 7.24
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Changed Files List"
         HEIGHT             = 7.24
         WIDTH              = 72
         MAX-HEIGHT         = 17.29
         MAX-WIDTH          = 84.6
         VIRTUAL-HEIGHT     = 17.29
         VIRTUAL-WIDTH      = 84.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fFrame
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Changed Files List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Changed Files List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel wWin
ON CHOOSE OF btnCancel IN FRAME fFrame /* Cancel */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK wWin
ON CHOOSE OF btnOK IN FRAME fFrame /* OK */
DO:
    RUN mainLoop.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
    RUN initScreen.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addSubdir wWin 
PROCEDURE addSubdir :
/*------------------------------------------------------------------------------
  Purpose: Add a subdirectory to the search table.    
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pcPath AS CHARACTER NO-UNDO.
    DEFINE BUFFER bfSubDir FOR ttSubDir.


    IF SUBSTRING(pcPath, (LENGTH(pcPath) - 1), 2) = ".." THEN RETURN.
    
    /* Clean up the periods at the end of the path. */
    REPEAT WHILE R-INDEX(pcPath, ".") = LENGTH(pcPath):
        pcPath = SUBSTRING(pcPath, 1, LENGTH(pcPath) - 1).
    END.

    IF SUBSTRING(pcPath, LENGTH(pcPath), 1) <> "\" THEN
        pcPath = pcPath + "\".

    /* See if we should leave. */
    IF lSub = FALSE THEN RETURN.
    IF pcPath = ? THEN RETURN.
    IF CAN-FIND(FIRST bfSubDir WHERE bfSubDir.DirPath = pcPath NO-LOCK) THEN RETURN.
    
    CREATE ttSubDir.

    ASSIGN
        ttSubDir.DirPath = pcPath
        ttSubDir.Active = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignScreen wWin 
PROCEDURE assignScreen :
/*------------------------------------------------------------------------------
  Purpose: Assign fields on the screen.    
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        ASSIGN {&DISPLAYED-OBJECTS}.

    END.  /* DO WITH FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cPath dtModSince lSub 
      WITH FRAME fFrame IN WINDOW wWin.
  ENABLE cPath dtModSince lSub btnOK btnCancel 
      WITH FRAME fFrame IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fFrame}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initScreen wWin 
PROCEDURE initScreen :
/*------------------------------------------------------------------------------
  Purpose: Initialize screen values.
  Notes:
------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
    
        ASSIGN
            cResult = FILE-INFO:PATHNAME
            ENTRY(NUM-ENTRIES(cResult, "\"), cResult, "\") = "".
            cPath:SCREEN-VALUE = cResult.
        
        ASSIGN
            lSub:CHECKED = TRUE.
            
        APPLY "ENTRY":U TO cPath.
    
    END. /* DO WITH FRAME */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE listFiles wWin 
PROCEDURE listFiles :
/*------------------------------------------------------------------------------
  Purpose: Check files in a given directory.    
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pcPath AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPathName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCurrFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFileSize AS INTEGER NO-UNDO.


    DO WITH FRAME fStat FONT 4 TITLE "Status" VIEW-AS DIALOG-BOX:

        DISPLAY TRIM("Searching " + pcPath) FORMAT "X(60)" WITH FRAME fStat.
        
        /* Read file info from OS. */
        INPUT STREAM sFileIn FROM OS-DIR (pcPath).
        
        REPEAT:
            IMPORT STREAM sFileIn cFileName.
            
            /* Check for directory. */
            FILE-INFO:FILE-NAME = TRIM(pcPath + cFileName).
            IF FILE-INFO:PATHNAME = ? THEN NEXT.
  
            IF INDEX(FILE-INFO:FILE-TYPE, "D") <> 0 THEN
                RUN addSubdir (INPUT FILE-INFO:FILE-NAME).
            ELSE
            DO:
                /* Check file's modification date. */
                cPathName = pcPath + "\" + cFileName. 
                FILE-INFO:FILE-NAME = cPathName.
            
                IF FILE-INFO:PATHNAME <> ? THEN
                DO:
                    /* Add file to results table. */
                    IF FILE-INFO:FILE-MOD-DATE >= dtModSince THEN
                    DO:
                        CREATE ttFile.

                        ASSIGN
                            ttFile.DirPath = FILE-INFO:FULL-PATHNAME
                            ttFile.FileName = FILE-INFO:FILE-NAME
                            ttFile.ModDate = FILE-INFO:FILE-MOD-DATE
                            ttFile.ModTime = DECIMAL(REPLACE(STRING(FILE-INFO:FILE-MOD-TIME, "HH:MM"), ":", ".")).
                    END.
                END.
            END.
        END. /* REPEAT */
    
        INPUT STREAM sFileIn CLOSE.

    END. /* DO WITH FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mainLoop wWin 
PROCEDURE mainLoop :
/*------------------------------------------------------------------------------
  Purpose: Main processing loop.
  Notes:
------------------------------------------------------------------------------*/

    RUN assignScreen.
    
    RUN searchDir.

    RUN outputResults.

    MESSAGE "All done!" SKIP(1)
            "Output was sent to changedfileslist.txt."
            VIEW-AS ALERT-BOX INFORMATION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputResults wWin 
PROCEDURE outputResults :
/*------------------------------------------------------------------------------
  Purpose: Send results to a text file.
  Notes:
------------------------------------------------------------------------------*/
    
    OUTPUT STREAM sFileOut TO VALUE("changedfileslist.txt").

    FOR EACH ttFile NO-LOCK:
        PUT STREAM sFileOut UNFORMATTED
            ttFile.DirPath FORMAT "X(80)" AT 1
            STRING(ttFile.ModDate, "99/99/9999") AT 83
            STRING(ttFile.ModTime, "99.99") AT 95
            SKIP.
    END.

    OUTPUT STREAM sFileOut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE searchDir wWin 
PROCEDURE searchDir :
/*------------------------------------------------------------------------------
  Purpose: Search through a subdirectory.   
  Notes:       
------------------------------------------------------------------------------*/

    /* Erase old records. */
    EMPTY TEMP-TABLE ttSubDir.

    /* Make sure the path ends in a slash. */
    IF SUBSTRING(cPath, LENGTH(cPath), 1) <> "\" THEN
        cPath = cPath + "\".

    /* Create a temp record for the subdirectory. */
    CREATE ttSubDir.
    ASSIGN
        ttSubDir.DirPath = cPath
        ttSubDir.Active = TRUE.
        
    /* Loop through subdirectories, setting the Active flag so we don't hit them multiple times. */       
    FOR EACH ttSubDir WHERE ttSubDir.Active = TRUE:
        IF AVAILABLE(ttSubDir) THEN
        DO:
            ttSubDir.Active = FALSE.
            RUN listFiles (INPUT ttSubDir.DirPath).
        END.
    END. /* REPEAT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

