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
;   Set or obtain the logical unit number of the file assigned to standard output.
;
; :Categories:
;    File Utility
;
; :Params:
;       FILE:       in, optional, type=string
;                   The name of logical unit number of a file to be assigned to stdout.
;                       If the empty string (''), 'stdout', or -1, any previously open
;                       file will be closed and output will be directed to IDL's default
;                       standard output (the console). If FILE is not provided, the
;                       logical unit number of the file assigned to standard output is
;                       returned.
;
; :Keywords:
;       KEEP_OPEN:  in, optional, type=boolean, default=0
;                   If set, opening a new standard output file will not cause the current
;                       one to be closed. The default is to close the previous file.
;       NAME:       in, optional, type=boolean, default=0
;                   If set, the name of the standard output file is returned instead
;                       of the logical unit number.
;
; :Returns:
;       FILEID:     Logial unit number of the standard output file.
;
; :Common Blocks:
;       MrStdOut_Comm
;           STDOUT_ID     -  The ID of the file assigned to standard output
;           STDOUT_FNAME  -  The name of the file assigned to standard output
;
; :See Also:
;   MrStdErr, MrStdLog, MrPrintF, MrLogFile__Define
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
function MrStdOut, file, $
KEEP_OPEN=keep_open, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	;Create a common block to store the file ID of the standard output file.
	common mrstdout_comm, stdout_id, stdout_fname

	;Does the fileID exist yet?
	tf_exist  = n_elements(stdout_id) gt 0
	keep_open = keyword_set(keep_open)
	name      = keyword_set(name)

;-----------------------------------------------------
; Return the Current File ID \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(file) eq 0 then begin
		;Default to standard output
		if ~tf_exist then begin
			stdout_id    = -1
			stdout_fname = 'stdout'
		endif

;-----------------------------------------------------
; Open a File by Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if size(file, /TNAME) eq 'STRING' then begin
		;Set empty string equal to "stdout"
		theFile = file eq '' ? 'stdout' : file
		
		;Close the previous file
		;   - If it exists
		;   - and it is not IDL's default standard out (-1)
		;   - unless the user explicitly wants it left open.
		if ~keep_open && tf_exist && stdout_id > 0 then begin
			
			;Indicate that the old file is being closed
			free_lun, stdout_id
			message, 'Closed previous stdout: "' + stdout_fname + '".', /INFORMATIONAL
			stdout_fname = ''
		endif
			
		;Open the new file, if it exists
		;   - FSTAT(-1) returns "<stdout>", so allow brackets
		if stregex(theFile, '(stdout|<stdout>)', /BOOLEAN, /FOLD_CASE) then begin
			stdout_id    = -1
			stdout_fname = theFile
		endif else begin
			openw, stdout_id, theFile, /GET_LUN
			stdout_fname = file_search(theFile, /FULLY_QUALIFY_PATH)
		endelse

;-----------------------------------------------------
; Set ID by LUN \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Make sure it is a number and an open file
		if ~( file eq -1 || (file gt 0 && file le 128) ) $
			then message, 'FILE must be -1 or a logical unit number between 1 & 128.'
		
		;Check the LUN
		file_stat = fstat(file)
		if ~file_stat.open then message, 'FILE is not open. Cannot assign to stdout.'
		
		;Close the previous file if
		;   - We do not want to keep it open
		;   - It exists
		;   - It is not the same file
		;   - It is not IDL's standard out
		if ~keep_open && tf_exist && file ne stdout_id && stdout_id ne -1 then begin
			;FILE <= 0 or FILE > 128 have been weeded out by logic.
			if file lt 100 $
				then close, stdout_id $
				else free_lun, stdout_id
			
			;Remind the user of which file is being closed.
			message, 'Closed previous stdout: "' + stdout_fname + '".', /INFORMATIONAL
			stdout_fname = ''
		end
		
		;Set the name and file ID
		stdout_fname = file_stat.name
		stdout_id    = file
	endelse
	
	;Return
	if name $
		then return, stdout_fname $
		else return, stdout_id
end