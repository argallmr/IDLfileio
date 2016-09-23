; docformat = 'rst'
;
; NAME:
;    MrStdOut
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
;   Assign a file to be the recipient of standard output.
;
; :Categories:
;    File Utilities
;
; :Params:
;       FILE:       in, optional, type=string
;                   The name of logical unit number of a file to be assigned to stderr.
;                       If the empty string (''), 'stderr', or -1, any previously open
;                       file will be closed and output will be directed to IDL's default
;                       standard stderr (the console). If FILE is not provided, the
;                       logical unit number of the file assigned to standard stderr is
;                       returned.
;
; :Keywords:
;       KEEP_OPEN:  in, optional, type=boolean, default=0
;                   If set, opening a new standard error file will not cause the current
;                       one to be closed. The default is to close the previous file.
;       NAME:       in, optional, type=boolean, default=0
;                   If set, the name of the standard stderr file is returned instead
;                       of the logical unit number.
;
; :Returns:
;       FILEID:     Logial unit number of the standard stderr file.
;
; :Common Blocks:
;       MrStdOut_Comm
;           STDERR_ID     -  The ID of the file assigned to standard stderr
;           STDERR_FNAME  -  The name of the file assigned to standard stderr
;
; :See Also:
;   MrStdOut, MrStdLog, MrPrintF, MrLogFile__Define
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
;       2015/10/28  -   Written by Matthew Argall
;-
function MrStdErr, file, $
KEEP_OPEN=keep_open, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	;Create a common block to store the file ID of the standard output file.
	common mrstderr_comm, stderr_id, stderr_fname

	;Does the fileID exist yet?
	tf_exist  = n_elements(stderr_id) gt 0
	keep_open = keyword_set(keep_open)
	name      = keyword_set(name)

;-----------------------------------------------------
; Return the Current File ID \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(file) eq 0 then begin
		;Default to standard output
		if ~tf_exist then begin
			stderr_id    = -2
			stderr_fname = 'stderr'
		endif

;-----------------------------------------------------
; Open a File by Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if size(file, /TNAME) eq 'STRING' then begin
		;Empty string?
		theFile = file eq '' ? 'stderr' : file
		
		;Close the previous file
		;   - If it exists
		;   - and the LUN is not IDL's standard error (-2)
		;   - Unless the user wants to keep it open
		if ~keep_open && tf_exist && stderr_id > 0 then begin
			
			;Indicate that the old file is being closed
			free_lun, stderr_id
			message, 'Closed previous stdout: "' + stdout_fname + '".', /INFORMATIONAL
			stderr_fname = ''
		endif
			
		;Open the new file, if it exists
		;   - FSTAT(-2) returns "<stderr>", so allow it
		if stregex(theFile, '(stderr|<stderr>)', /BOOLEAN, /FOLDCASE) then begin
			stderr_id    = -2
			stderr_fname = theFile
		endif else begin
			openw, stderr_id, theFile, /GET_LUN
			stderr_fname = file_search(theFile, /FULLY_QUALIFY_PATH)
		endelse

;-----------------------------------------------------
; Set ID by LUN \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Make sure it is a number and an open file
		if ~( file eq -2 || (file gt 0 && file le 128) ) $
			then message, 'FILE must be -1 or a logical unit number between 1 & 128.'
		
		;Check the LUN
		file_stat = fstat(file)
		if ~file_stat.open then message, 'FILE is not open. Cannot assign to stdout.'
		
		;Close the previous file if
		;   - We do not want to keep it open
		;   - It exists
		;   - It is not the same as the input file
		;   - It is not IDL's standard error
		if ~keep_open && tf_exist && file ne stderr_id && stderr_id ne -2 then begin
			;FILE <= 0 or FILE > 128 have been weeded out by logic.
			if file lt 100 $
				then close, stderr_id $
				else free_lun, stderr_id
				
			;Remind user which file was closed.
			message, 'Closed previous stdout: "' + stderr_fname + '".', /INFORMATIONAL
			stderr_fname = ''
		end
		
		;Set the name and file ID
		stderr_fname = file_stat.name
		stderr_id    = file
	endelse
	
	;Return
	if name $
		then return, stderr_fname $
		else return, stderr_id
end