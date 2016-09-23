; docformat = 'rst'
;
; NAME:
;    MrPrintF
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
;   A wrapper for IDL's PrintF procedure that can direct output to standard output,
;   error, or log files.
;
; :Categories:
;    File Utility
;
; :Params:
;       LUN:        in, optional, type=string
;                   Logical unit number of the destination file. Special LUNs include::
;                       'stdout'    - File associated with standard output (MrStdOut)
;                       '<stdout>'  - File associated with standard output (MrStdOut)
;                       'stdout'    - File associated with standard error (MrStdErr)
;                       '<stdout>'  - File associated with standard error (MrStdErr)
;                       'stdlog'    - Add an error message to the Standard log file (MrStdLog)
;                       'logwarn'   - Add an warning message to the Standard log file (MrStdLog)
;                       'logout'    - Add text to the Standard log file (MrStdLog)
;                       'logtext'   - Add text to the Standard log file (MrStdLog)
;                   If one of the above is given, ARG2-20 are ignored.
;       ARG2-20:    in, optional, type=any
;                   Up to 20 additional expressions to be output. See IDL's String()
;                       procedure for details. If not provided, then two can things happen:
;                       1) if `LUN` is directed to standard error via any of the special
;                       `LUN` values, then the last error message is written, with line
;                       number and offending program. 2) if `LUN` is directed to standard
;                       output via any of the special `LUN` values, or is a normal logical
;                       unit number, then the empty string is written.
;
; :Keywords:
;       FORMAT:     in, optional, type=string
;                   Allows the format of the output to be specified in precise detail,
;                       using a FORTRAN-style specification. If is not specified,
;                       IDL uses its default rules for formatting the output.
;       _REF_EXTRA: in, optional, type=any
;                   Any keyword accepted by IDL's String() function.
;
; :See Also:
;   MrStdErr, MrStdOut, MrStdLog, MrLogFile__Define, MrTraceback
;
; :Author:
;    Matthew Argall::
;        University of New Hampshire
;        Morse Hall Room 348
;        8 College Road
;        Durham, NH 03824
;        matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2015/10/29  -   Written by Matthew Argall
;       2015/12/03  -   If no string is provided, stderr writes the last error
;                           message. - MRA
;       2016/06/11  -   PrintF is disabled in demo mode, so use Print for LUN of -1
;                           or -2 (stderr/stdout). Issue error otherwise. - MRA
;-
pro MrPrintF, lun,  arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
                   arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
FORMAT=format, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Number of consecutive parameters (not including LUN) with more than 0 elements
	nparams = n_elements(arg1)      eq 0 ?  0 : $
	              n_elements(arg2)  eq 0 ?  1 : $
	              n_elements(arg3)  eq 0 ?  2 : $
	              n_elements(arg4)  eq 0 ?  3 : $
	              n_elements(arg5)  eq 0 ?  4 : $
	              n_elements(arg6)  eq 0 ?  5 : $
	              n_elements(arg7)  eq 0 ?  6 : $
	              n_elements(arg8)  eq 0 ?  7 : $
	              n_elements(arg9)  eq 0 ?  8 : $
	              n_elements(arg10) eq 0 ?  9 : $
	              n_elements(arg11) eq 0 ? 10 : $
	              n_elements(arg12) eq 0 ? 11 : $
	              n_elements(arg13) eq 0 ? 12 : $
	              n_elements(arg14) eq 0 ? 13 : $
	              n_elements(arg15) eq 0 ? 14 : $
	              n_elements(arg16) eq 0 ? 15 : $
	              n_elements(arg17) eq 0 ? 16 : $
	              n_elements(arg18) eq 0 ? 17 : $
	              n_elements(arg19) eq 0 ? 18 : $
	              n_elements(arg20) eq 0 ? 19 : $
	              20

;-----------------------------------------------------
; Conver Inputs to a String \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	case nparams of
		20: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		19: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		18: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		17: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, arg14, arg15, arg16, arg17, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		16: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, arg14, arg15, arg16, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		15: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, arg14, arg15, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		14: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, arg14, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		13: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, arg13, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		12: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, arg12, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		11: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 arg11, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		10: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, arg10, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 9: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 8: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 7: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 6: str = string( arg1,  arg2,  arg3,  arg4,  arg5,  arg6, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 5: str = string( arg1,  arg2,  arg3,  arg4,  arg5, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 4: str = string( arg1,  arg2,  arg3,  arg4, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 3: str = string( arg1,  arg2,  arg3, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 2: str = string( arg1,  arg2, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 1: str = string( arg1, $
		                 FORMAT=format, _STRICT_EXTRA=extra )
		 0: ;Do nothing for now.
		else: message, 'Incorrect number of parameters.'
	endcase

;-----------------------------------------------------
; STDOUT, STDERR, STDLOG, ETC. \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if size(lun, /TNAME) eq 'STRING' then begin
		;No string given?
		if nparams eq 0 then begin
			;For errors, report the error, line, and offending program
			;   - The logging object handles this automatically.
			if stregex(lun, 'stderr', /BOOLEAN, /FOLD_CASE) then begin
				str       = !error_state.msg
				traceback = MrTraceback(caller, line)
				str       = 'Error in ' + caller + ': ' + str + ' (line' + strtrim(line,2) + ')'
			
			;For standard output, use the empty string
			endif else if stregex(lun, '(out|text)', /BOOLEAN, /FOLD_CASE) then begin
				str = ''
			endif
		endif
		
		;Pick the output method.
		case strlowcase(lun) of
			'stdout':   theLUN = MrStdOut()
			'<stdout>': theLUN = MrStdOut()
			'stderr':   theLUN = MrStdErr()
			'<stderr>': theLUN = MrStdErr()
			'stdlog': begin
				oLog = MrStdLog()
				oLog -> AddError, str
			endcase
			'logerr': begin
				oLog = MrStdLog()
				oLog -> AddError, str
			endcase
			'logwarn': begin
				oLog = MrStdLog()
				oLog -> AddWarning, str
			endcase
			'logout': begin
				oLog = MrStdLog()
				oLog -> AddText, str, /ADD_CALLER
			endcase
			'logtext': begin
				oLog = MrStdLog()
				oLog -> AddText, str, /ADD_CALLER
			endcase
			else: message, 'Invalid value for LUN: "' + lun + '".'
		endcase
	endif else begin
		if nparams eq 0 then str = ''
		theLUN = lun
	endelse

;-----------------------------------------------------
; Write to File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(theLUN) gt 0 then begin
		
		;
		; PrintF is disabled in demo mode, so opt for printing
		; to standard out (the command line).
		;
		catch, the_error
		
		;Try using PrintF to print the message, even if text is directed to
		;standard in/out/err
		if the_error eq 0 then begin
			printf, theLUN, str
		
		;Skip using PrintF and go straight to Print.
		endif else begin
			catch, /CANCEL
			
			;Print if THELUN is -1 or -2 (stdout/stderr). Issue an error otherwise.
			if theLUN lt 0 $
				then print, str $
				else message, /REISSUE_LAST
		endelse
	endif
end