; docformat = 'rst'
;
; NAME:
;    MrStdLog
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
;   Assign a file to the standard error logging object.
;
; :Categories:
;    File Utility
;
; :Params:
;       FILE:       in, optional, type=string
;                   The name or logical unit number of a file to be assigned to stderr.
;                       The values '', 'stdout', 'stderr', '<stdout>', '<stderr'>
;                       '<stderr>', -2, or -1, will close any previously opened
;                       file and direct output to IDL's default tandard error (the console).
;                       If FILE is not provided, the current error logging object is
;                       returned.
;
; :Keywords:
;       _REF_EXTRA: in, optional, type=any
;                   Any keyword accepted by MrLogFile::INIT.
;
; :Returns:
;       STDLOG:     The current MrLogFile object.
;
; :Common Blocks:
;       MrStdOut_Comm
;           STDLOG     -  The MrLogFile error logger.
;
; :See Also:
;   MrStdErr, MrStdOut, MrPrintF, MrLogFile__Define
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
;-
function MrStdLog, file, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Create a common block to store the file ID of the standard output file.
	common mrstdlog_comm, stdlog

	;Does the fileID exist yet?
	tf_exist  = n_elements(stdlog) gt 0 && obj_valid(stdlog)

;-----------------------------------------------------
; Return the Current File ID \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(file) eq 0 then begin
		;Default to standard error
		if ~tf_exist $
			then stdlog = obj_new('MrLogFile', '', _STRICT_EXTRA=extra)

;-----------------------------------------------------
; Assign an Existing MrLogFile Object \\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if size(file, /TNAME) eq 'OBJREF' then begin
		;Must be a MrLogFile object
		if ~obj_isa(file, 'MrLogFile') then $
			message, 'Only MrLogFile objects are allowed.'
		
		;Destroy the previous object
		if tf_exist then obj_destroy, stdlog
		
		;Assign the new log object
		stdlog = file
		
;-----------------------------------------------------
; Open a File by Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if size(file, /TNAME) eq 'STRING' then begin
		;If the error logging object already exists, change file names
		if tf_exist then begin
			status = stdlog -> Open(file)
			if status eq 0 then message, 'File cannot be opened: "' + file + '".'
		
			;Set properties
			if n_elements(extra) gt 0 then stdlog -> SetProperty, _STRICT_EXTRA=extra
		
		;Otherwise, create a new object
		endif else begin
			stdlog = obj_new('MrLogFile', file, _STRICT_EXTRA=extra)
		endelse

;-----------------------------------------------------
; Set ID by LUN \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;LUNs are allowed only in the INIT method.
		if tf_exist then obj_destroy, stdlog
		stdlog = obj_new('MrLogFile', file, _STRICT_EXTRA=extra)
	endelse
	
	;Return
	return, stdlog
end