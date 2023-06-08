INCLUDE Irvine32.inc
include ExitFromTheMazeData.inc
.data
	BoardT BYTE lengthof Board Dup (?)
	Path BYTE lengthof Board Dup (?)
	path_length dword 0
	info byte "         ****MAZE SOLVER****",10,13,0
	myProject byte "COAL Semester Project BY Muhammad Haseeb",10,13,0
	boardMsg1 byte "This is the representation of Board in Memory:",10,13,0
	sl byte "----------------------------------",10,13,0
	space byte "  ",0
	pathMsg1  byte "The path from S to E is :",0
	pathMsg2  byte "The path length is :",0
	row word 0
	column word  0
	index word 0
	output byte 0
.code
myMain PROC
	call printInfo
	call copyBoard
	call printBoard

	push offset BoardT 
	push n_cols
	push n_rows
	push offset Path
	call find_path
	
	push offset Board
	push n_cols
	push n_rows
	push offset Path
	call check_path

	call printPathMsg
	call exitProcess
myMain ENDP


printInfo PROC
	call crlf
	mov edx,offset info
	call writestring 
	mov edx, offset myProject
	call writestring
	call crlf
	ret 
printInfo ENDP


;-------------------------
;This Function copy the given board to BoardT
copyBoard PROC
	mov ecx,lengthof Board
	dec ecx
	copyBoardToBoardT:
	mov al,Board[ecx]
	mov BoardT[ecx],al
	loop copyBoardToBoardT
	mov al,Board[0]
	mov BoardT[0],al
	ret
copyBoard ENDP


;-------------------------
;This Function print the given board
printBoard PROC
	mov ecx,lengthof Board
	mov esi,0  ; index to travel in the board current index
	mov ebx,0  ; index to know in which col we stand 
	call printBoarDetails
	scan_board:
		movsx edi,Board[esi]  ; to change to Board 
		call printOneByte
		inc esi
		inc ebx
		cmp bx,n_cols
		jz PrintNewLine
		jmp skip
		PrintNewLine:
		call crlf
		mov ebx,0
		skip:
		loop scan_board
		call crlf
	ret
printBoard ENDP


;-------------------------
;This Function print one byte 0=00  1=01  'S'=53  'E'=45
;receives: -Char from the board in edi	
printOneByte PROC
	mov eax,edi
	and eax,000000f0h
	shr eax,4
	call writedec
	mov eax,edi
	and eax,0fh
	call writedec
	mov edx,offset space
	call writestring
	ret
printOneByte ENDP


;-------------------------------
;just for printing msgs to screen
printBoarDetails PROC
	mov edx,offset boardMsg1
	call writestring
	mov eax,offset Board
	call writehex
	call crlf
	mov edx,offset sl
	call writestring
	ret
printBoarDetails ENDP


;-------------------------
;This function calculate board length I'm doesnot use length of board because in function checkpath the arguments passed in stack 
;Using the stack to pass arguments
;return:- BoardLength in ecx
calculateBoardLength PROC
	movsx eax, word ptr[esp+16]
	movsx ebx, word ptr[esp+18]
	mul ebx
	mov ecx,eax
	sub ecx,1 ; length of the board
	ret
calculateBoardLength ENDP

;-----------------------------
;This function find in wich index from the end of the board s exist
;store this index in index lable
findSIndex PROC
	find_start_index: ; this loop find in wich index 'S' found in the Board
	cmp byte ptr [eax][ecx],'S'
	jz exit_find_start_index_loop
		loop find_start_index
	exit_find_start_index_loop:	
		mov index,cx
	ret
findSIndex ENDP

;-----------------------
;This function split the index lable where s located to row and column 
;Uses the parameters from the stack
;store S row's in row and S cloumn's in column
splitIndex PROC
	split_index_to_row_and_column: ; this loop find in wich row and column found the index of 'S'
	cmp word ptr[esp+14],cx
	ja increase_column
		inc row
		sub cx,word ptr[esp+14]
		inc cx
		loop split_index_to_row_and_column
	increase_column:
		mov column,cx
	ret
splitIndex ENDP


;checkpath(Board, n_cols, n_rows, Path)
;This function checks the staus of the path
;Using the stack to pass arguments
;pass the board and the path by reference
;return:- path Validation in eax- al (i,f,s) HomeWork2

check_path PROC

	mov row,0
	mov column,0
	mov path_length,eax  ; after calling to findPath eax store the path length
	push 0 ; just to cause to calculateBoardLength Function work good with another call
	call calculateBoardLength
	pop eax
	
	mov eax,dword ptr[esp+12] ; offset of Board
	mov ebp,dword ptr[esp+4]  ; offset of Path
	;[esp+10] = n_cols , [esp+8] = n_rows

	call findSIndex
	movsx ecx,index 
	call splitIndex

	movsx edi,index ; where we stand yet in the board (index to the current index in the board)
	sub path_length,1
	mov esi,0 ; index to travel on the path
	inc row
	inc column
	inc word ptr [esp+10]
	inc word ptr [esp+8]

checkpath:

	cmp byte ptr[ebp][esi],'L'
	je check_left_step
	cmp byte ptr[ebp][esi],'R'
	je check_right_step
	cmp byte ptr[ebp][esi],'U'
	je check_up_step
	jmp check_down_step ; other case is 'D' no need to compare

	check_left_step:
		dec column
		cmp column,0
		je illegal_path
		dec edi
		cmp byte ptr [eax][edi],1
		je illegal_path
		jmp legal_step

	check_right_step:
		inc column
		movsx ebx,column ; ebx used just to compare (can't compare between two labels)
		cmp bx,word ptr [esp+10]
		je illegal_path
		inc edi
		cmp byte ptr [eax][edi],1
		je illegal_path
		jmp legal_step

	check_up_step:
		dec row
		cmp row,0
		je illegal_path
		sub di,word ptr [esp+10]
		inc di
		cmp byte ptr [eax][edi],1
		je illegal_path
		jmp legal_step

	check_down_step:
		inc row
		movsx ebx,row ; ebx used just to compare (can't compare between two labels)
		cmp bx,word ptr [esp+8]
		je illegal_path
		add di,word ptr [esp+10]
		dec edi
		cmp byte ptr [eax][edi],1
		je illegal_path
		jmp legal_step

	legal_step:
		cmp esi,path_length
		je check_goal_state
		inc esi
		jmp checkpath

	illegal_path:
		mov output,'i'
		jmp skip

	check_goal_state:
		cmp byte ptr [eax][edi],'E'
		je solved
		mov output,'f'
		jmp skip
		solved:
		mov output,'s'
	skip:
	inc path_length
	movsx eax, output
	ret 12
check_path ENDP


;This function print to the screen a Msg according to the path validation 
printPathMsg PROC

	mov ebx,eax
	cmp bl,'s'
	jz solution
	cmp bl,'i'
	jz illegal
	cmp bl,'f'
	jz failed

	solution:
	mov edx,offset pathMsg1
	call writestring
	mov edx,offset Path
	call writestring
	call crlf
	mov edx,offset pathMsg2
	call writestring
	mov eax,path_length
	jmp skip

	illegal:
	mov edx,offset pathMsg2
	call writestring
	mov eax,-1
	jmp skip

	failed:
	mov edx,offset pathMsg1
	call writestring
	mov edx,offset Path
	call writestring
	call crlf
	mov edx,offset pathMsg2
	call writestring
	mov eax,lengthof path
	dec eax

	skip:
	call writeint
	call crlf
	movsx eax,bl
	call writechar
	call crlf
	ret
printPathMsg ENDP


;find_path(Board, n_cols, n_rows, Path)
;This function finds a ligal path from S to E if exist
;Using the stack to pass arguments
;pass the board and the path by reference
;return:- path length in eax

find_path PROC 

	call findSLocation
	push ebp
	mov ebp,esp
	mov index,0
	mov eax,0

	push [ebp+16] ; BoardT offset
	push column   ; S-column
	push row      ; S-row
	push [ebp+8]  ; Path offset
	push index    ; offset (counter where to insert the next letter in path)
	call findpath_r

	mov esp,ebp
	pop ebp
	ret 12
find_path ENDP


;This function finds where 'S' located in the board
;Using the stack to pass arguments
;pass the board and the path by reference
;return:- edi store the S-column
;		  esi store the S-row
findSLocation Proc
	call calculateBoardLength ;ecx now store the Board Length
	mov eax,dword ptr[esp+16] ; offset of Board

	find_start_index: ; this loop find in wich index 'S' found in the Board
		cmp byte ptr [eax][ecx],'S'
		jz exit_find_start_index_loop
			loop find_start_index
		exit_find_start_index_loop:	
			mov index,cx

	movsx ecx,index 
	split_index_to_row_and_column: ; this loop find in wich row and column found the index of 'S'
		cmp word ptr[esp+14],cx
		ja increase_column
			inc row
			sub cx,word ptr[esp+14]
			inc cx
			loop split_index_to_row_and_column
		increase_column:
			mov column,cx

		movsx edi,column
		movsx esi,row
	ret
findSLocation ENDP


; This function convert the (x,y) coordinates to index in the Board
; return this index in ecx
convertLocationToIndex PROC USES EAX
	movsx eax,row
	movsx ebx,n_cols
	mul ebx
	add ax,column
	mov ecx,eax
	ret
convertLocationToIndex ENDP

;this the recursive algorthim as shown in homework page
findpath_r PROC
	jmp skip 
	push_parametrs_to_stack: 
		mov ebx,esp
		mov edx,dword ptr[ebx+14] ; offset boarf
		push edx
		movsx edx,word ptr[ebx+12] ; x
		push dx
		movsx edx,word ptr[ebx+10] ; y
		push dx
		mov edx,dword ptr[ebx+6] ; offset path
		push edx
		movsx edx,word ptr[ebx+4] ; index in path
		push dx
		cmp edi,'R'
		je rightCall
		cmp edi,'L'
		je leftCall
		cmp edi,'D'
		je downCall
		cmp edi,'U'
		je upCall
	skip:

	call checkCondition
	cmp eax,1
	jne return
		call convertLocationToIndex ; now ecx store the index to board
		mov ebx,dword ptr[esp+14]   ; ebx store the offset of board offset =>[ebx] offset of board
		movsx edx,byte ptr[ebx][ecx]
		cmp edx,'E'
		jne continue1
			movsx eax,word ptr[esp+4]
			jmp return
		continue1:
		cmp byte ptr[ebx][ecx],1
		jne continue2
			mov eax,-1
			jmp return
		continue2:
		mov byte ptr[ebx][ecx], 1 ; board[x][y] = 1
		mov esi,dword ptr[esp+6] ; esi store the offset of path ofsset => [esi] offset of path

		mov edi,'R'
		movsx edx,word ptr[esp+4]
		mov [esi][edx],edi
		inc word ptr[esp+4]
		inc word ptr[esp+12]
		inc column
		
		jmp push_parametrs_to_stack
		rightCall:
		call findpath_r

		cmp eax,-1
		je continue3  ;(if eax != -1)
			jmp return
		continue3: ; if(eax = -1) after failed right move
		dec word ptr[esp+4]
		dec word ptr[esp+12]
		dec column
		mov edi,'L'
		movsx edx,word ptr[esp+4]
		mov [esi][edx],edi
		inc word ptr[esp+4]
		dec word ptr[esp+12]
		dec column

		jmp push_parametrs_to_stack
		leftCall:
		call findpath_r

		cmp eax,-1
		je continue4  ;(if eax != -1)
			jmp return
		continue4: ; if(eax = -1) after failed left move
		dec word ptr[esp+4]
		inc word ptr[esp+12]
		inc column
		mov edi,'D'
		movsx edx,word ptr[esp+4]
		mov [esi][edx],edi
		inc word ptr[esp+4]
		inc word ptr[esp+10]
		inc row

		jmp push_parametrs_to_stack
		downCall:
		call findpath_r

		cmp eax,-1
		je continue5 ;(if eax != -1)
			jmp return
		continue5: ; if(eax = -1) after failed Down Move
		dec word ptr[esp+4]
		dec word ptr[esp+10]
		dec row
		mov edi,'U'
		movsx edx,word ptr[esp+4]
		mov [esi][edx],edi
		inc word ptr[esp+4]
		dec word ptr[esp+10]
		dec row

		jmp push_parametrs_to_stack
		upCall:
		call findpath_r

		cmp eax,-1
		je continue6 ;(if eax != -1)
			jmp return
		continue6: ; if(eax = -1) after failed UP move
		dec word ptr[esp+4]
		inc word ptr[esp+10]
		inc row
		mov eax,-1
		cmp word ptr[esp+4],0 ; index in path =0
		je deleteLastLetter
		jmp return
		deleteLastLetter:
		mov path,0
	return:
	ret 14
findpath_r ENDP


;This function get the arguments from the stack 
;this function store in eax 1 if the condition is true and -1 if is not
checkCondition PROC

	movsx eax,sword ptr[esp+16]
	cmp eax,0
	jge check_second
		mov eax,-1 ; if (x<0) then you jump to this line 
		jmp return

	check_second:  ; if (x>=0) then you jump to this line
		movsx ebx,n_cols ; because i can't compere with n_cols

	cmp eax,ebx
	jl check_third
		mov eax,-1 ; if(x>=n_cols)
		jmp return

	check_third:
	movsx eax,word ptr[esp+14]
		
	cmp eax,0
	jge check_fourth  ; was ja
		mov eax,-1   ; if(y<0)
		jmp return

	check_fourth:    ; if(y>=0)
		movsx ebx,n_rows
		
	cmp eax,ebx
	jl all_passed
		mov eax,-1   ; if(y>=n_rows)
		jmp return
	all_passed:   ;if(y<n_rows)
		mov eax,1

	return:
		ret

checkCondition ENDP
END myMain
		
