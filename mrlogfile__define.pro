; docformat = 'rst'
;
; NAME:
;    MrLogFile__Define.pro
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   The purpose of this program is to log program errors or text messages during
;   program execution as an aid to debugging said a program at a later date. The
;   MrLogFile program is written as an object so that it will persist in the IDL
;   session until it is destroyed.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Categories:
;    File Utility
;
; :Methods:
;
;        AddError:      Adds an error text string or array to the error log file. By default,
;                       it will add the HELP, LAST_MESSAGE=1, /TRACEBACE traceback 
;                       information to the file. (Procedure)
;
;        AddText:       Adds a text string or array to the error log file. (Procedure)
;
;        AddWarning:    Adds a warning text string or array to the error log file. (Procedure)
;
;        Alert:         Creates a pop-up dialog to alert the user of an error. (Procedure)
;
;        Callstack:     Responsible for parsing the callstack. (Function)
;
;        ClearLog:      Erases all the text currently in the error log file. (Procedure)
;
;        Close:         Closes the currently open error log file. (Procedure)
;
;        Flush:         Forces a write of any current information to the disk (Procedure)
;
;        GetProperty:   Gets properties of the object. (Procedure)
;
;        LastMessage:   Returns the last message text written into the error log file. (Function)
;
;        Open:          Opens the error log file for writing. (Function)
;
;        PrintLastMessage: Writes the last message text written into the error log file to 
;                          standard output. (Procedure)
;
;        GetStatus:     Returns the current status of the error logger. (0 - waiting for input, 
;                       1 - normal operation, 2 - error operation.) (Function)
;
;        SetProperty:   Sets properties of the object. (Procedure)
;
;        SetStatus:     Sets the current status of the error logger. Normally not used by the
;                       user, but used internally. (Procedure)
;
; :See Also:
;   MrCallstack, MrTraceback, MrStdLog, MrStdErr, MrStdOut, MrPrintF
;
; :History:
;   Modification History::
;       2015/10/30  -   Adapted from David Fanning's ErrorLogger__Define.pro by Matthew Argall
;       2015/12/03  -   Added the ADD_FILES and WARN_TRACEBACK properties.
;       2015/12/04  -   Fixed problem with multi-line error messages. - MRA
;       2016/01/16  -   Allocate memory error does not have caller. Handled. - MRA
;       2016/06/11  -   Use Print in demo-mode when possible to by-pass error. - MRA
;       2016/10/06  -   Added the LEVEL parameter to ::AddError and ::AddWarning.
;                           ::Callstack no longer checks for sister program MrPrintF. - MRA
;       2017/03/16  -   LEVEL is silently forced into acceptable range in ::CALLSTACK. - MRA
;       2017/03/18  -   Added the APPEND keyword. Renamed ::Status to ::GetStatus. The
;                           STATUS property can be a range of values and set via ::Add*.
;                           Added the ::SetLun method. - MRA
;-
;*****************************************************************************************
;+
;   Adds error text to the error log file and sets the error log status to 2 (error
;   condition). If the error logger alert flag is set to 1, the method will alert
;   the user to the error with a pop-up message dialog as well as writing the output
;   to standard output.
;
; :Params:
;       THETEXT:     in, required, type=string/strarr
;                    The error message text you wish to add to the file. If not provided,
;                        the text of the last error message (Help, /LAST_MESSAGE) is used and
;                        written to the file.
;
; :Keywords:
;       LEVEL:       in, optional, type=integer, default=3
;                    Level in the callstack at which to report the error. The default
;                        is to report to the program that calls ::AddError.
;       STATUS:      in, optional, type=byte, default=1
;                    Set the error status.
;-
PRO MrLogFile::AddError, theText, $
LEVEL=level, $
STATUS=status
	compile_opt idl2
	on_error, 2
	
	;Error status
	theStatus = N_Elements(status) EQ 0 ? 1 : status

	; If no text was given, assume an IDL error occurred
	;   - ::Traceback will determine on which line and in which program
	;     the last error occurred, regardless of where ::AddError was called.
	IF N_Elements(theText) EQ 0 THEN BEGIN
		theText   = !Error_State.Msg
		traceback = self -> Traceback(caller, line)

	;If text was given, assume the user is logging an error
	;   - ::Callstack will determine on which line ::AddError was called
	;   - Add THETEXT to !Error_State by calling Message.
	ENDIF ELSE BEGIN
		if n_elements(level) eq 0 then level = 3
		traceback = self -> Callstack(level, CALLER=caller, LINE=line)
		Message, theText, /CONTINUE, /NOPRINT, /NONAME, /NOPREFIX
	ENDELSE
	
	;
	; !Error_State.Msg can have the calling routine in it already. E.g.
	;
	;   MMS_EDI_RMT: RMT Failure: Not enough beams in each to/away class
	;
	; If this is the case, remove the calling program from THETEXT. For
	; some errors, there is no calling routine, CALLER is the empty
	; string. E.g.
	;
	;   Unable to allocate memory: to make array.
	;
	
	;The calling program is separated from the text by a colon.
	;   - Object methods are separated from the object class by a double-colon
	;   - Process object methods separately
	doubleColon = strpos(theText, '::')
	if doubleColon eq -1 then begin
		if caller ne '' && strpos(thetext, caller) ne -1 $
			then thetext = strmid(thetext, strpos(thetext, ':')+2)
	endif else begin
		;Get the object class, object method, and message
		;   - CLASS::METHOD: Message String
		class      = strmid(theText, 0, doubleColon+2)
		submessage = strmid(theText, doubleColon+2)
		colon      = strpos(submessage, ':')
		
		;Is there a single colon?
		;   - Compare it to the calling routine. If they match, remove it.
		if colon ne -1 then begin
			if class + strmid(submessage, 0, colon) eq caller $
				then theText = strmid(submessage, colon+1)
		endif
	endelse

	;Add the error
	self -> AddText, 'Error in ' + caller + ': ' + theText + ' (line ' + strtrim(line, 2) + ')', $
	                 STATUS = status

	;Add the traceback report
	IF self.traceback THEN self -> AddText, '   ' + traceback

	; Set the error status
	self -> SetStatus, theStatus
END


;+
;   Adds text to the error log file and sets the error log status to 1 (normal
;   condition). 
;
; :Params:
;       THETEXT:      in, required, type=string/strarr
;                     The message text you wish to add to the file. 
;
; :Keywords:
;       ADD_CALLER:   in, optional, type=boolean, default=0
;                     If this keyword is set, the name of the caller routine is
;                         prepended to the text message.
;       PRINT:        in, optional, type=boolean, default=0
;                     If this keyword is set, the added text is also sent to standard
;                         output.
;       STATUS:       in, optional, type=byte
;                     Sets the error status. By default, the error status is left unchanged.
;-
PRO MrLogFile::AddText, theText, $
ADD_CALLER=add_caller, $
PRINT=print, $
STATUS=status
	Compile_Opt idl2
	On_Error, 2

	; Have to have text to do anything.
	IF N_Elements(theText) EQ 0 THEN RETURN
	
	;Defaults
	tf_print      = keyword_set(print)
	tf_add_caller = keyword_set(add_caller)
	
	; Make sure these are strings we are writing.
	thisType = Size(theText, /TNAME)
	IF thisType NE 'STRING' THEN Message, 'Only strings can be written into the error log file.'

	;Add the caller
	IF tf_add_caller THEN BEGIN
		void    = self -> Callstack(3, CALLER=caller)
		theText = caller + ': ' + theText
	ENDIF

	; Open the file if need be
	IF self.lun EQ 0 THEN BEGIN
		success = self -> Open(self.filename)
		IF ~success THEN Message, 'Cannot successfully open the error log file.'
	ENDIF
	
	;
	; In Demo mode, PrintF is not allowed and an error will be thrown.
	; If it is, return here, set the demo-mode flag, and try again with
	; the regular Print procedure. If that fails, then issue error.
	;
	catch, the_error
	if the_error ne 0 then tf_demo_mode = 1 else tf_demo_mode = 0
	
	; Write each line
	numLines = N_Elements(theText)
	FOR j=0L, N_Elements(theText) -1 DO BEGIN
		
		;DEMO-MODE
		if tf_demo_mode then begin
			catch, /CANCEL

			;Use Print for LUN = -2, -1, otherwise throw error
			if self.lun lt 0 $
				then print, theText[j] $
				else message, /REISSUE_LAST
		
		;NORMAL-MODE
		endif else begin
			PrintF, self.lun, theText[j]
			IF tf_print THEN Print, theText[j]
		endelse
	ENDFOR

	; Write to disk immediately?
	IF self.immediate THEN self -> Flush

	; Update the error logger status to normal. If this method is called
	; from AddError, then when we return to AddError, the status will be
	; set to 2, or error status. But setting to 1 here allows us to add
	; text to the file whenever we like.
	self -> SetStatus, status

	; Save the last message for later recall.
	*self.lastMessage = theText
END 


;+
;   Add a warning message to the log file.
;
;       'Warning: ' + `THETEXT` + '(ROUTINE, LINE)'
;
; :Params:
;       THETEXT:     in, required, type=string
;                    Text to be written to the log file.
;
; :Keywords:
;       LEVEL:       in, optional, type=integer, default=3
;                    Level in the callstack at which to report the error. The default
;                        is to report to the program that calls ::AddError.
;       STATUS:      in, optional, type=byte
;                    Set the error status. The default is to leave the status unchanged.
;-
PRO MrLogFile::AddWarning, theText, $
LEVEL=level, $
STATUS=status
	Compile_Opt idl2
	On_Error, 2

	;Write strings to files
	IF Size(theText, /TNAME) NE 'STRING' THEN Message, 'THETEXT must be a string.'
	if n_elements(level) eq 0 then level = 3

	; Get the call stack and the calling routine's name.
	traceback = self -> Callstack(level, CALLER=caller, LINE=line)

	;Add the error
	self -> AddText, 'Warning: ' + theText + ' (' + caller + ' ' + strtrim(line, 2) + ')', $
	                 STATUS=status

	;Add the traceback report
	IF self.warn_traceback THEN self -> AddText, '    ' + traceback
END


;+
;   Generate a dialog message to alert the user of an error.
;
; :Params:
;       THETEXT:     in, required, type=string
;                    Text to be displayed in the dialog message box.
;-
PRO MrLogFile::Alert, theText
	compile_opt idl2
	on_error, 2
	

	; Are widgets supported?
	IF !D.Name EQ 'PS' $
		THEN widgetsSupported = 1 $
		ELSE widgetsSupported = ((!D.Flags AND 65536L) NE 0)

;---------------------------------------------------------------------
; X-Connection ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

	; It is not enough to know if widgets are supported. In CRON jobs, widgets are
	; supported, but there is no X connection and pop-up dialogs are not allowed.
	; Here is a quick test to see if we can connect to a windowing system. If not,
	; then we are going to assume widgets are not supported.
	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		widgetsSupported = 0
		
		;If we were successful in creating a window
		;   - Close it and set to previous window before proceeding
		IF !D.Window NE theWindow THEN BEGIN
			WDelete, !D.Window
			IF theWindow GE 0 THEN WSet, theWindow
		ENDIF

	;Test X-Connection by creating a window.
	ENDIF ELSE BEGIN
		theWindow = !D.Window
		IF (!D.Flags AND 256) NE 0 THEN Window, /FREE, XSIZE=5, YSIZE=5, /PIXMAP
		Catch, /CANCEL
	ENDELSE

;---------------------------------------------------------------------
; Create Alert ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	IF widgetsSupported THEN BEGIN

	;---------------------------------------------------------------------
	; Trapped Error///////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		; If this is an error produced with the MESSAGE command, it is a trapped
		; error and will have the name "IDL_M_USER_ERR".
		IF !ERROR_STATE.NAME EQ "IDL_M_USER_ERR" THEN BEGIN

			IF N_Elements(title) EQ 0 THEN title = 'Trapped Error'

			; If the message has the name of the calling routine in it,
			; it should be stripped out. Can you find a colon in the string?

			; Is the calling routine an object method? If so, special processing
			; is required. Object methods will have two colons together.
			doublecolon = StrPos(theText, "::")
			IF doublecolon NE -1 THEN BEGIN

				prefix = StrMid(theText, 0, doublecolon+2)
				submessage = StrMid(theText, doublecolon+2)
				colon = StrPos(submessage, ":")
				IF colon NE -1 THEN BEGIN

					; Extract the text up to the colon. Is this the same as
					; the callingRoutine? If so, strip it.
					IF StrMid(theText, 0, colon+StrLen(prefix)) EQ callingRoutine $
						THEN theText = StrMid(theText, colon+1+StrLen(prefix))
				ENDIF
			ENDIF ELSE BEGIN

				colon = StrPos(theText, ":")
				IF colon NE -1 THEN BEGIN

					; Extract the text up to the colon. Is this the same as
					; the callingRoutine? If so, strip it.
					IF StrMid(theText, 0, colon) EQ callingRoutine $
						THEN theText = StrMid(theText, colon+1)
				ENDIF

			ENDELSE

			; Add the calling routine's name.
			void = Dialog_Message(StrUpCase(callingRoutine) + ": " + theText, Title=title)

	;---------------------------------------------------------------------
	; IDL System Error ///////////////////////////////////////////////////
	;---------------------------------------------------------------------
		ENDIF ELSE BEGIN

			; Otherwise, this is an IDL system error.
			IF N_Elements(title) EQ 0 THEN title = 'System Error'

			IF StrUpCase(callingRoutine) EQ "$MAIN$" $
				THEN void = Dialog_Message(theText, Title=title) $
				ELSE void = Dialog_Message(StrUpCase(callingRoutine) + "--> " + theText, Title=title)
		ENDELSE
	ENDIF

;---------------------------------------------------------------------
; Additional Info to Console /////////////////////////////////////////
;---------------------------------------------------------------------
	;Also print the traceback
	IF self.traceback THEN $
		FOR j=0, N_Elements(traceback)-1 DO Print, "     " + traceback[j]
END


;+
;   Parse the Scope_Traceback() report. Output format is:
;       "In ROUTINE at (line ###)"
;
; :Params:
;       LEVEL:       in, optional, type=integer, default=1
;                    Level in the call stack at which to begin reporting traceback
;                        information. If LEVEL=1 (the default), the traceback will
;                        begin here, with MrLogFile::Callstack.
;
; :Keywords:
;       CALLER:      out, optional, type=string
;                    Name of the calling program, identified by `LEVEL`.
;       LINE:        out, optional, type=string
;                    Line of the calling program, identified by `LEVEL`.
;-
function MrLogFile::Callstack, level, $
CALLER=caller, $
LINE=line
	compile_opt idl2
	on_error, 2
	
	
	; Get the call stack and the calling routine's name.
	stack  = Scope_Traceback(/STRUCTURE)
	nstack = n_elements(stack)
	caller = stack[nstack-2].routine
	
	;Level at which to report
	;   - MAIN is 1
	;   - Maximum depth is NSTACK-1
	lvl = n_elements(level) eq 0 ? 1 : level
	lvl = 1 > lvl < nstack
	
	;Get calling routine
	caller = stack[nstack-lvl].routine
	
	;Extract the stack elements
	;   - Eliminate $MAIN$ if possible
	;   - Reverse elements so caller is first.
	if lvl eq nstack $
		then stack = stack[0] $
		else stack = stack[nstack-lvl:1:-1]
	nstack = n_elements(stack)

	;Calling program
	caller = stack[0].routine
	line   = stack[0].line
	
	;Traceback report
	traceback = 'In ' + stack.routine + ' at (line ' + strtrim(stack.line, 2) + ')'
	if self.add_files then traceback += ' --> ' + stack.filename

	return, traceback
END


;+
;   Parse the Help, /LAST_MESSAGE report. Output format is:
;       "In ROUTINE at (line ###)"
;
; :Params:
;       CALLER:      out, optional, type=string
;                    Name of the calling program in which the error occurred.
;       LINE:        out, optional, type=string
;                    Line at which the error occurred in `CALLER`.
;-
function MrLogFile::Traceback, caller, line
	compile_opt idl2
	on_error, 2
	
	;
	;  Example (parse routine name and line number):
	;     % MMS_EDI_RMT: Only MAX_ADDR = {30 | 31 | 32} beams allowed.
	;     % Execution halted at:  MMS_EDI_RMT       304 /home/argall/IDL/MMS/costfn/mms_edi_rmt.pro
	;     %                       MMS_EDI_TEST_COSTFN 1194 /home/argall/IDL/MMS/diagnostics/mms_edi_test_costfn.pro
	;     %                       $MAIN$
	;
	
	;Get the traceback report from the last message
	;   - First element is the error message
	;   - Last element is $MAIN$
	Help, /LAST_MESSAGE, OUTPUT=traceback
	ntrace = n_elements(traceback)
	
	;Find the line that says, "Execution halted ... "
	;   - The error message segment can be multiple lines (elements) long
	;   - This will be the place at which to start parsing
	istart = where(strpos(traceback, 'Execution halted') ne -1, n)
	if n eq 0 then begin
		istart = 0
		iend   = 0
	endif else if n eq 1 then begin
		istart = istart[0]
		iend   = ntrace-2
	endif else begin
		if n ne 1 then message, 'Unexpected traceback format.'
	endelse

	;Error occurred from $MAIN$
	if istart gt iend then begin
		routine = '$MAIN$'
		lines   = 0

	;Error occurred in procedure or function
	endif else begin
		routine = strarr(ntrace-2)
		lines   = lonarr(ntrace-2)
		files   = strarr(ntrace-2)
		for i = 1, ntrace-2 do begin
			info         = stregex(traceback[i], ':?[ ]+([A-Z_0-9:]+)[ ]+([0-9]+)[ ]+(.*)', /SUBEXP, /EXTRACT)
			routine[i-1] = info[1]
			lines[i-1]   = info[2]
			files[i-1]   = info[3]
		endfor
	endelse

	;Caller and line number
	caller = routine[0]
	line   = lines[0]
	
	;Traceback report
	traceback = 'In ' + routine + ' at (line ' + strtrim(lines, 2) + ')'
	if self.add_files then traceback += ' --> ' + files

	return, traceback
END


;+
;   Clears the error log file of text.
;-
PRO MrLogFile::ClearLog

	; Close the current error log file and delete it.
	self -> Close
	File_Delete, self.filename, /ALLOW_NONEXISTENT

	; Open a new error log file with the same name.
	self -> Open, self.filename

	; Set the error logger status to waiting.
	self -> SetStatus, 0

END 


;+
;   Closes the currently open error log file.
;-
PRO MrLogFile::Close
	
	;
	; 100-128 are allocated by /GET_LUN and should be freed
	; 1-99    are chosen by user and can simply be closed
	; -2,-1,0 are standard error, output, and input streams and should not be closed
	;
	IF self.lun GE 100 $
		THEN Free_Lun, self.lun $
		ELSE IF self.lun GT 0 THEN Close, self.lun
END


;+
;   Flushes the current error logger information to the file in case of crash.
;-
PRO MrLogFile::Flush
	IF self.lun GE 100 THEN Flush, self.lun
END 


;+
;   Returns the file name of the error log file.
;
; :Returns:
;       FILENAME:     The name of the error log file.
;-
FUNCTION MrLogFile::GetFileName
	RETURN, self.filename
END 


;+
;   Allows the user to get properties from the object via keywords.
;
; :Keywords:
;       ADD_FILES:      in, optional, type=boolean, default=0
;                       If set, file names will be added to the traceback report. By
;                           default, only the routine names and line numbers are included.
;       ALERT:          out, optional, type=boolean
;                       If set, the user will be alerted of errors via a dialog pop-up.
;       DELETE:         out, optional, type=boolean
;                       The delete on destroy flag in the object.
;       FILENAME:       out, optional, type=string
;                       Name of the log file.
;       LAST_MESSAGE:   out, optional, type=string/strarr
;                       The last message printed to the log file.
;       LUN:            out, optional, type=long
;                       Logical unit number of the log file.
;       NOCLUTTER:      out, optional, type=boolean
;                       If set, automatically sets `ALERT` and `DELETE`.
;       NOTRACEBACK:    out, optional, type=boolean
;                       If set, no traceback report will be added to error messages.
;       STATUS:         out, optional, type=integer
;                       The current error log status.
;       WARN_TRACEBACK: in, optional, type=boolean, default=0
;                       If set, traceback reports will be added to the warning messages.
;                           All warning messages always contain the caller and line
;                           number where the warning occurred.
;-
PRO MrLogFile::GetProperty, $
ADD_FILES=add_files, $
ALERT=alert, $
DELETE=delete, $
FILENAME=filename, $
LAST_MESSAGE=last_message, $
LUN=lun, $
NOCLUTTER=noclutter, $
NOTRACEBACK=notraceback, $
STATUS=status, $
WARN_TRACEBACK=warn_traceback
	on_error, 2

	;Get Properties
	IF Arg_Present(add_files)      THEN add_files      = self.add_files
	IF Arg_Present(alert)          THEN alert          = self.alert
	IF Arg_Present(delete)         THEN delete         = self.delete
	IF Arg_Present(filename)       THEN filename       = self.filename
	IF Arg_Present(last_message)   THEN last_message   = self -> LastMessage()
	IF Arg_Present(lun)            THEN lun            = self.lun
	IF Arg_Present(noclutter)      THEN noclutter      = self.noclutter
	IF Arg_Present(notraceback)    THEN notraceback    = ~self.traceback
	IF Arg_Present(warn_traceback) THEN warn_traceback = self.warn_traceback
	IF Arg_Present(status)         THEN status         = self.status
END 


;+
;   Returns the current status of the error logger.
;
; :Returns:
;       status:     The error log status::
;                      0 - waiting for input
;                      1 - normal operation
;                      2 - error operation
;-
FUNCTION MrLogFile::GetStatus
	RETURN, self.status
END 


;+
;   Returns the last text message written to the error logger.
;
; :Returns:
;       MESSAGE:     The last text written to the error log file.
;-
FUNCTION MrLogFile::LastMessage
	; Returns the last message added to the file.
	IF N_Elements(*self.lastMessage) GT 0 $
		THEN RETURN, *self.lastMessage $
		ELSE RETURN, ""
END 


;+
;   Opens the log file.
;
; :Params:
;       FILENAME:     in, optional, type=string, default='stderr'
;                     The name of the error log file. If "", "stderr", "stdout", 
;                         "<stderr>" or "<stdout>", then output will be directed to IDL's
;                         standard output and error streams (the console). In all cases
;                         the previous log file is closed.
; 
; :Keywords:
;       APPEND:         in, optional, type=boolean, default=0
;                       If set, the file pointer will be moved to the end of the file.
;                           The default is to clear the file and place the file pointer
;                           at the beginning.
;       DELETE_CURRENT: in, optional, type=boolean, default=0
;                       If this keyword is set, the current error log file is closed
;                           and deleted before the new file is opened for writing.
;
; :Returns:
;       STATUS:         out, required, type=integer
;                       Returns 1 if file was opened sucessfully, 0 otherwise.
;-
FUNCTION MrLogFile::Open, newLogFile, $
APPEND=append, $
DELETE_CURRENT=delete_current, $
_REF_EXTRA=extra
	Compile_Opt idl2

	; Error handling
	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		Print, !Error_State.MSG
		Print, '  ' + Transpose(MrTraceback())
		IF N_Elements(lun) NE 0 && lun GT 0 THEN BEGIN
			Free_Lun, lun
			File_Delete, newLogFilename, /ALLOW_NONEXISTENT
		ENDIF
		RETURN, 0
	ENDIF
	
	;Default to standard error
	;   - NEWLOGFILE might be the empty string, in which case choose stderr
	newLog = N_Elements(newLogFile) EQ 0 ? 'stderr' : newLogFile
	newLog = newLog EQ '' ? 'stderr' : newLogFile
	
	;Can we write into the specified directory?
	IF ~StRegEx(newLog, '^(std(out|err)|<std(out|err)>)$', /FOLD_CASE, /BOOLEAN) THEN BEGIN
		dir = file_dirname(newLog)
		IF File_Test(dir, /DIRECTORY, /WRITE) EQ 0 $
			THEN Message, 'Directory does not exist or is not writable: "' + dir + '".' 
	ENDIF

;-----------------------------------------------------
; Previous Log File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	; Close the current file (if any) before opening a new one.
	self -> Close

	; Need to delete the current file?
	IF Keyword_Set(delete_current) THEN File_Delete, self.filename, /ALLOW_NONEXISTENT

;-----------------------------------------------------
; Filename Given \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	; Open the file for writing.
	;    FSTAT(-1) returns "<stdout>" and "<stderr>"
	CASE newLog OF
		'<stdout>': lun = -1
		'stdout':   lun = -1
		'<stderr>': lun = -2
		'stderr':   lun = -2
		ELSE: BEGIN
			;Open the file
			OpenW, lun, newLog, $
			       APPEND = append, $
			       /GET_LUN

			; Write a header into the file.
			IF ~Keyword_Set(append) THEN BEGIN
				PrintF, lun, 'Error log file created: ' + SysTime(/UTC) + ' UTC'
				PrintF, lun, ""
			ENDIF
		ENDCASE
	ENDCASE

	; Store the filename
	self.lun      = lun
	self.filename = newLog

	RETURN, 1
END


;+
;   Prints the last text message written to the error logger to standard output.
;-
PRO MrLogFile::PrintLastMessage

	; Prints the last message in the error logger.
	lastMessage = self -> LastMessage()
	FOR j=0, N_Elements(lastMessage)-1 DO Print, lastMessage[j]

END 


;+
;   Allows the user to set properties of the object via keywords.
;
; :Keywords::
;       ADD_FILES:      in, optional, type=boolean, default=0
;                       If set, file names will be added to the traceback report. By
;                           default, only the routine names and line numbers are included.
;       ALERT:          in, optional, type=boolean
;                       If set, the user will be alerted of errors via a dialog pop-up.
;       DELETE:         in, optional, type=boolean
;                       The delete on destroy flag in the object.
;       NOCLUTTER:      in, optional, type=boolean
;                       If set, automatically sets `ALERT` and `DELETE`.
;       NOTRACEBACK:    in, optional, type=boolean
;                       If set, no traceback report will be added to error messages.
;       STATUS:         in, optional, type=integer
;                       The current error log status.
;       WARN_TRACEBACK: in, optional, type=boolean, default=0
;                       If set, traceback reports will be added to the warning messages.
;                           All warning messages always contain the caller and line
;                           number where the warning occurred.
;-
PRO MrLogFile::SetProperty, $
ADD_FILES=add_files, $
ALERT=alert, $
APPEND=append, $
DELETE=delete, $
NOCLUTTER=noclutter, $
NOTRACEBACK=notraceback, $
STATUS=status, $
WARN_TRACEBACK=warn_traceback
	on_error, 2

	;Set Properties
	IF N_Elements(add_files)      NE 0 THEN self.add_files      =  Keyword_Set(add_files)
	IF N_Elements(alert)          NE 0 THEN self.alert          =  Keyword_Set(alert)
	IF N_Elements(append)         NE 0 THEN self.append         =  Keyword_Set(append)
	IF N_Elements(delete)         NE 0 THEN self.delete         =  Keyword_Set(delete)
	IF N_Elements(notraceback)    NE 0 THEN self.traceback      = ~Keyword_Set(notraceback)
	IF N_Elements(warn_traceback) NE 0 THEN self.warn_traceback =  Keyword_Set(warn_traceback)
	IF N_Elements(status)         NE 0 THEN self -> SetStatus, status
	
	;NoClutter
	IF N_Elements(noclutter) NE 0 THEN BEGIN
		self.noclutter = Keyword_Set(noclutter)
		IF self.noclutter THEN BEGIN
			self.alert = 1
			self.delete = 1
		ENDIF
	ENDIF
END 


;+
;   Set the logical unit number of the log file.
;
; :Params:
;       LUN:                in, required, type=integer
;                           The logical unit number of an open file.
;
; :Keywords:
;       DELETE_CURRENT:     in, optional, type=boolean, default=0
;                           If set, the current log file is deleted.
;-
PRO MrLogFile::SetLUN, lun, $
DELETE_CURRENT=delete_current
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		Print, !Error_State.msg
		Print, '  ' + Transpose(MrTraceback())
		RETURN
	ENDIF
	
	;The LUN must be open
	finfo = FStat(lun)
	IF ~finfo.open THEN Message, 'LUN must be open.'
	
	;Close the current file
	self -> Close
	
	;Delete the current file
	IF Keyword_Set(delete_current) THEN File_Delete, self.filename, /ALLOW_NONEXISTENT
	
	;Set the LUN and filename
	self.filename = finfo.name
	self.lun      = finfo.unit
END


;+
;   Sets the current status of the error logger.
;
; :Params:
;       status:     in, required, type=integer
;                   The error log status::
;                      0     - No error
;                      1-255 - error
;-
PRO MrLogFile::SetStatus, status
	IF N_Elements(status) GT 0 THEN self.status = 0 > status < 255
END


;+
;   Cleans up the object.
;-
PRO MrLogFile::CLEANUP
	compile_opt idl2
	on_error, 2

	; Be sure the file is closed. Otherwise, it can't be deleted.
	self -> Close

	; If the file is not in an error state, and the delete_on_destroy flag
	; is set, delete the error log file.
	IF self.delete THEN File_Delete, self.filename, /ALLOW_NONEXISTENT
	IF self.noclutter THEN BEGIN
		IF self.status LE 0 THEN File_Delete, self.filename, /ALLOW_NONEXISTENT
	ENDIF

	; Free the last message pointer.
	Ptr_Free, self.lastMessage
END 


;+
;
;   The initialization method for the object.
;
; :Params:
;       FILENAME:       in, optional, type=string/int, default='MrLogFile_[date]_[random-numbers].log'
;                       The name or logical unit number (lun) of the error log file. If
;                           not provided, a default name will be created based on the
;                           current system time.
;
; :Keywords:
;      ADD_FILES:       in, optional, type=boolean, default=0
;                       If set, file names will be added to the traceback report. By
;                           default, only the routine names and line numbers are included.
;      ALERT:           in, optional, type=boolean, default=0
;                       The default behavior of the error logger is simply to write text to
;                           a file. But if the ALERT keyword is set, the program will alert
;                           the user via a message dialog that an error has occurred when
;                           using the AddError method. Default is 0. (Input)
;      DELETE:          in, optional, type=boolean, default=0
;                       If set, the log file will be deleted when the MrLogFile object
;                           is destroyed.
;      IMMEDIATE:       in, optional, type=boolean, default=1
;                       All messages will flush to disk as soon as they are logged.
;      NOCLUTTER:       in, optional, type=boolean, defualt=0
;                       If set, then the log file will be delete when the MrLogFile object
;                           is destroyed, but only if no error has occurred (status=0).
;      NOTRACEBACK:     in, optional, type=boolean, default=0
;                       Set this keyword to suppress traceback information in the error log
;                           output and in any alerts issued by the program.
;      TIMESTAMP:       in, optional, type=boolean, default=0
;                       Set this keyword if you wish a time stamp to be appended to the
;                           provided filename. Otherwise, the filename is used as defined.
;                           Default filenames always have a timestamp appended to the file
;                           name.
;      WARN_TRACEBACK:  in, optional, type=boolean, default=0
;                       If set, traceback reports will be added to the warning messages.
;                           All warning messages always contain the caller and line
;                           number where the warning occurred.
;      _REF_EXTRA:      in, optional, type=any
;                       Any keyword accepted by MrLogFile::Open is also accepted here.
;                           This keyword is ignored unless `FILE` is a file name.
;-
FUNCTION MrLogFile::INIT, file, $
ADD_FILES=add_files, $
ALERT=alert, $
APPEND=append, $
DELETE=delete, $
IMMEDIATE = immediate, $
NOCLUTTER=noclutter, $
NOTRACEBACK=notraceback, $
TIMESTAMP=timestamp, $
WARN_TRACEBACK=warn_traceback, $
_REF_EXTRA=extra
	COMPILE_OPT idl2
	
	Catch, theError
	IF theError NE 0 THEN BEGIN
		Catch, /CANCEL
		Print, !Error_State.msg
		Print, '  ' + Transpose(MrTraceback())
		RETURN, 0
	ENDIF
	
	;Time stamp (UTC)
	;   - <date>_<6 random digits>
	;   - Leave SEED undefined to minimize chances that the random digits are the same
	tstamp = StrLowCase(IDL_ValidName(SysTime(/UTC), /CONVERT_ALL)) + '_' + $
	         StrTrim(Fix(randomu(seed, 1) * 1e6, TYPE=3), 2)

;-----------------------------------------------------
; No File Given \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF N_Elements(file) EQ 0 THEN BEGIN
		;Create a file name
		CD, CURRENT=currentDir
		filename  = FilePath( 'MrLogFile_' + tstamp + '.log', $
		                      ROOT_DIR=currentDir )
		
		;Open the file
		status = self -> Open( filename, $
		                       APPEND        = append, $
		                       _STRICT_EXTRA = extra)
		IF status EQ 0 THEN RETURN, 0

;-----------------------------------------------------
; File Name Given \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE IF Size(file, /TNAME) EQ 'STRING' THEN BEGIN
		filename = file
		
		;Fully qualify & add time stamp
		IF ~StRegEx(filename, '^(std(out|err)|<std(out|err)>)$', /FOLD_CASE, /BOOLEAN) THEN BEGIN
			; Is this a fully qualified filename?
			baseName = File_BaseName(filename)
			IF baseName EQ filename THEN BEGIN
				CD, CURRENT=currentDir
				filename = FilePath(ROOT_DIR=currentDir, file)
			ENDIF

			; Does the name need a time stamp?
			IF Keyword_Set(timestamp) THEN BEGIN
				;Directory and base names
				dir      = File_DirName(filename)
				basename = File_BaseName(filename)
				
				;Remove extension
				iExt     = StrPos(basename, '.', /REVERSE_SEARCH)
				ext      = iExt EQ -1 ? ''       : StrMid(basename, iExt+1)
				basename = iExt EQ -1 ? basename : StrMid(basename, 0, iExt)
				
				;Add time stamp
				filename = Filepath(ROOT_DIR=dir, basename + '_' + tstamp)
				IF ext NE '' THEN filename = filename + '.' + ext
			END
		ENDIF
		
		;Open the file
		status = self -> Open( filename, $
		                       APPEND        = append, $
		                       _STRICT_EXTRA = extra)
		IF status EQ 0 THEN RETURN, 0

;-----------------------------------------------------
; LUN Given \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE BEGIN
		;Set the logical unit number
		self -> SetLUN, file
	ENDELSE

;-----------------------------------------------------
; Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	; Initialize the last message pointer.
	self.lastMessage = Ptr_New(/ALLOCATE_HEAP)

	; Set properties.
	tf_noclutter = Keyword_Set(noclutter)
	tf_delete    = Keyword_Set(delete)
	IF tf_noclutter && tf_delete THEN Message, 'DELETE and NOCLUTTER are mutually exclusive.'
	
	self.add_files      = Keyword_Set(add_files)
	self.alert          = Keyword_Set(alert)
	self.delete         = tf_delete
	self.immediate      = N_Elements(immediate) eq 0 ? 1B : Keyword_Set(immediate)
	self.noclutter      = tf_noclutter
	self.traceback      = ~Keyword_Set(notraceback)
	self.warn_traceback = Keyword_Set(warn_traceback)

	; Successful completion.
	RETURN, 1
END 


;+
;   Class definition
;
; :Fields:
;       FIELNAME:       The error log filename.
;       LUN:            The file logical unit number.
;       ADD_FILES:      Add file names to the traceback report.
;       ALERT:          A flag, if set, will give user alerts on errors.
;       APPEND:         A flag, if set, text will be appended to existing file.
;       TRACEBACK:      If set, will include traceback information into the log file.
;       LASTMESSAGE:    The last message written into the file.
;       IMMEDIATE:      A flag causing messages to flush to disk immediately
;       DELETE:         A flag causing log file to be deleted when object is destroyed.
;       NOCLUTTER:      A flag that sets up file deletion on destroy.
;       STATUS:         The current status of the error logger. 0-waiting, 1-normal, 2-error.
;       WARN_TRACEBACK: Add traceback report to the warning messages.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       Class definition structure.
;-
PRO MrLogFile__Define, class

	class = { MrLogFile, $
	          Inherits IDL_Object, $
	          filename:       "", $         ; The error log filename.
	          lun:            0L, $         ; The file logical unit number.
	          add_files:      0B, $         ; Add file names to the traceback report.
	          alert:          0B, $         ; A flag, if set, will give user alerts on errors.
	          append:         0B, $         ; A flag, if set, text will be appended to existing file.
	          traceback:      0B, $         ; If set, will include traceback information into the log file.
	          lastMessage:    Ptr_New(), $  ; The last message written into the file.
	          immediate:      0B, $         ; A flag causing messages to flush to disk immediately
	          delete:         0B, $         ; A flag causing log file to be deleted when object is destroyed.
	          noclutter:      0B, $         ; A flag that sets up file deletion on destroy.
	          status:         0B, $         ; The current status of the error logger. 0-waiting, 1-normal, 2-error.
	          warn_traceback: 0B $          ; Add traceback report to the warning messages.
	        }
END
