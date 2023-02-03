INCLUDE "hardware.inc"
INCLUDE "fontCharmap.asm"

DEF BATTERY_RAM= $A000
DEF ARROW_START_X= 40
DEF ARROW_START_Y= 32
DEF HIGHER_BYTE_USED_FOR_LABELS = $E0
DEF HIGHER_BYTE_ADDRESS_CODE_END =$D0



SECTION "VBlankInterrupt", ROM0[$040]
	push af
	call GetButtons
	and a, BUTTON_START
	jp nz, NoStartButtonInterrupt
	ld hl, ContinueAfterCodeRun
	pop af
	push hl
	reti ; return to ContinueAfterCodeRun
NoStartButtonInterrupt:
	pop af
	reti

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; Header will be placed here


; Do not move the following functions until Entry point other wise save cames will not work
; with new revision of the game as this addresses are commonly used by programms
; None of these functions should modify any register
FunctionToWaitForNextVBlank:
	push af
_FunctionToWaitForNextVBlank1:
	ld a, [rLY]
	cp 144
	jp nc, _FunctionToWaitForNextVBlank1
_FunctionToWaitForNextVBlank2:
	ld a, [rLY]
	cp 144
	jp c, _FunctionToWaitForNextVBlank2
	pop af
	ret

DEF BACKGROUND_TILE_IN_ONE_VBLANK=64
ClearBackground:
	call FunctionToWaitForNextVBlank
	push af
	push bc
	push hl
	push de
    ld bc, _SCRN1 - _SCRN0
    ld hl, _SCRN0
	ld d, BACKGROUND_TILE_IN_ONE_VBLANK
_ClearBackgroundLoop:
    ld a, 0
    ld [hli], a 
	dec d
	jp nz, _NoWaitVblankNeeded
	call FunctionToWaitForNextVBlank
	ld d, BACKGROUND_TILE_IN_ONE_VBLANK
_NoWaitVblankNeeded:
    dec bc 
	ld a, b 
	or a, c 
    jp nz, _ClearBackgroundLoop 
	call FunctionToWaitForNextVBlank ; so caller can directly start using vram
	pop de
	pop hl
	pop bc
	pop af
	ret

DEF DPAD_DOWN =%00001000
DEF DPAD_UP =%00000100
DEF DPAD_LEFT =%00000010
DEF DPAD_RIGHT =%00000001
; Return in register a  DPAD_DOWN, DPAD_UP ,DPAD_LEFT ,DPAD_RIGHT bits
; only modifies a
GetDPad:
	ld a, P1F_GET_DPAD
	ld [rP1], a
	call _GetDPadRet ; only to add delay
	ld a , [rP1] ; wait until input stablized
	ld a , [rP1] 
	ld a , [rP1] ; Okay now use this value
_GetDPadRet:
	ret

DEF BUTTON_START =%00001000
DEF BUTTON_SELECT =%00000100
DEF BUTTON_B =%00000010
DEF BUTTON_A =%00000001
GetButtons:
	ld a, P1F_GET_BTN
	ld [rP1], a
	call _GetButtonsRet ; only to add delay
	ld a , [rP1] ; wait until input stablized
	ld a , [rP1] 
	ld a , [rP1] ; Okay now use this value

_GetButtonsRet:
	ret

; Parameter:
; hl destination address
; a value to print (only 0-99)
PrintNumberAsDecimal:
	push bc
	cp a, 100 ; ignore numbers >= 100
	jp nc, _PrintNumberAsDecimalExit
	ld b,0
	ld c, 10
_LoopDivideBy10:
	sub a,c
	inc b
	jp nc, _LoopDivideBy10
	dec b
	add a,c
	ld c, a
	
	; now b has a/10 and c has a%10
	ld a, b
	add a, "0"
	ld [hli], a
	ld a,c
	add a, "0"
	ld [hli], a
_PrintNumberAsDecimalExit:
	pop bc
	ret

; Paramter:
; hl: destination in tilemap
PrintGameOver:
	push af
	push bc
	push de
	push hl
	call ClearBackground
	ld de, GameOverTextLine1
	ld bc, GameOverTextLine1End - GameOverTextLine1
	call printStr
	pop hl
	ld b, 0
	ld c, $3B ; skip two lines but make it centered
	add hl, bc
	ld de, GameOverTextLine2
	ld bc, GameOverTextLine2End - GameOverTextLine2
	call printStr
_WaitForButtonA:
	call GetButtons
	and a, BUTTON_A
	jp nz, _WaitForButtonA

_PrintGameOverExit:
	call FunctionToWaitForNextVBlank
	pop de
	pop bc
	pop af
	ret
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop


GameOverTextLine1:
db "GAME OVER!"
GameOverTextLine1End:
GameOverTextLine2:
db "PRESS A TO CONTINUE"
GameOverTextLine2End:

EntryPoint:
	; stop audio
	ld a, 0
	ld [rNR52], a

	; Turning of LCD outside of VBlank might damage hardware
	call FunctionToWaitForNextVBlank

	; Turn the LCD off
	ld a, 0
	ld [rLCDC], a

	; Copy the font to the tile area
	ld de, fontStart
	ld hl, $9000
	ld bc, fontEnd - fontStart
CopyTiles:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, CopyTiles


 ; Copy the arrow to the object tile area
    ld de, SelectorTile
    ld hl, $8000
    ld bc, SelectorTileEnd - SelectorTile
CopySelectorTile:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, CopySelectorTile


call ClearOam



MACRO printComma
	ld a, ","
	ld [bc],a
	inc bc
ENDM

MACRO printCollon
	ld a, ":"
	ld [bc],a
	inc bc
ENDM

MACRO printSpace
	ld a, " "
	ld [bc],a
	inc bc
ENDM

MACRO printOpenBracket
	ld a, "["
	ld [bc],a
	inc bc
ENDM
MACRO printClosingBracket
	ld a, "]"
	ld [bc],a
	inc bc
ENDM

MACRO print0x
	ld a, "0"
	ld [bc],a
	inc bc
	ld a, "x"
	ld [bc],a
	inc bc
ENDM

MACRO printDollar
	ld a, "$"
	ld [bc],a
	inc bc
ENDM

MACRO printA
	ld a, "A"
	ld [bc],a
	inc bc
ENDM

MACRO resetArrow
	ld hl, _OAMRAM
    ld a, ARROW_START_Y
    ld [hli], a
    ld a, ARROW_START_X
    ld [hli], a
    ld a, 0
    ld [hli], a
    ld [hl], a
ENDM

MACRO moveBit0To2ThreeBitsLeft
	sla a
	sla a
	sla a
	and a,$38
ENDM

MACRO moveBit0To1FourBitsLeft
	sla a
	sla a
	sla a
	sla a
	and a,$30
ENDM

MACRO moveBit0To1SixBitsLeft
	sla a
	sla a
	sla a
	sla a
	sla a
	sla a
	and a, $C0
ENDM

MACRO moveBit6To7FourBitsRight
	sra a
	sra a
	sra a
	sra a
	and a, $0C
ENDM

MACRO moveBit3To5ThreeBitsRight
	sra a
	sra a
	sra a
	and a, $07
ENDM

MACRO moveBit3To5OneBitRight
	sra a
	and a, $1C
ENDM


; Instruction value in c
; Return lenght in bc (b alsway 0)
MACRO getLengthOfInstruction
	ld hl, LengthOfInstructions
	ld b, 0
	add hl,bc
	ld a, [hl]
	ld c, a
ENDM

MACRO getLabelAddress
	ld hl, $9800
	ld de, HeadlineSelectFirstNibble
	ld bc, HeadlineSelectFirstNibbleEnd-HeadlineSelectFirstNibble
	call printStr

	ld de, SelectionHexNibble
	ld hl, $9800
	call ShowSelectionScreen

	ld d,a
	push de

	ld hl, $9800
	ld de, HeadlineSelectSecondNibble
	ld bc, HeadlineSelectSecondNibbleEnd-HeadlineSelectSecondNibble
	call printStr

	ld de, SelectionHexNibble
	ld hl, $9800
	call ShowSelectionScreen
	pop de
	ld b, a
	ld a, d
	call CombineNibbles
ENDM

; Write object
	ld hl, _OAMRAM
    ld a, ARROW_START_Y
    ld [hli], a
    ld a, ARROW_START_X
    ld [hli], a
    ld a, 0
    ld [hli], a
    ld [hl], a


	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON
	ld [rLCDC], a

	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ld [rBGP], a
	;ld a, %11100100
    ld [rOBP0], a

	; Copy to RAM
	ld de, SampleCode1
    ld hl, Cds
    ld bc, SampleCode1End - SampleCode1
CopySample1:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, CopySample1


    ld hl, CdsLabels
    ld bc, CdsLabelsEnd - CdsLabels
	ld a, 0
InitalizeCdsLabelsToZero:
    ld [hli], a
    dec bc
    ld a, b
    or a, c
    jp nz, InitalizeCdsLabelsToZero

	ld de, SampleCode1Labels
    ld hl, CdsLabels
    ld bc, SampleCode1LabelsEnd - SampleCode1Labels
CopySample1Labels:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, CopySample1Labels

; Multiple of 8
DEF ScrollSpeed = 8
Main:
; Init variables
	ld c, $B3
	ld d, $AF
	nop
	ld d, $95
	ld a, 0
	nop
	ld [counter], a
	ld a, 1
	ld [bitmap], a
WaitB10:
	ld a, [rLY]
	cp 144
	jp nc, WaitB10
WaitVBlank10:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank10

Def StartVRAM=$9805

	ld bc, Cds
	ld de, StartVRAM

	ld a, $FF
	ld [FoundLabel],a

printIntrucstionLoop:
	ld a,d
	cp a,$9C
	jp c, NoVramOverflow
	ld d, $98 ; Reset to screen start
NoVramOverflow:	
	ld a,b
	cp a, HIGHER_BYTE_ADDRESS_CODE_END
	jp c, NoRamOverflow
	ld bc, Cds ; start at beginning
NoRamOverflow:
	push de
	push bc
	
	; Print address: 
	ld a, b	;first part of address
	ld h, c ; 
	ld b, d
	ld c, e
	dec c
	dec c
	dec c
	dec c
	dec c
	call PrintNumberAsHex ; does not modify hl
	ld a, h ; second part of address
	call PrintNumberAsHex
	printSpace
	pop bc
	pop de

	push de
	push bc

	ld a, [FoundLabel]
	cp a, $ff
	jp z, NormalInstructionPrint
	ld h, d
	ld l, e
	ld de, LabelText
	ld bc, LabelTextEnd-LabelText
	call printStr
	ld b, h
	ld c, l
	ld a, [FoundLabel]
	call PrintNumberAsHex
	printCollon
	ld a, [bitmap]
	set 5, a ; bitmap signal label was really printed
	ld [bitmap], a
	ld a, 0 ; do not increase pointer to Ram
	jp IncreaseAddressesAfterPrint


NormalInstructionPrint:
	call PrintInstruction
	ld d, a
	ld a, [bitmap]
	res 4, a ; bitmap third bit means down still pressed
	ld [bitmap], a
	ld a, d
IncreaseAddressesAfterPrint:
	ld d, 0
	ld e, a
	pop hl
	add hl, de ; increase Ram with instruction length
	ld b, h
	ld c, l
	pop hl
	ld d, 0
	ld e, $20
	add hl, de ; increase destination vram to next line
	ld d, h
	ld e, l


SkipPrint:
	push de
	push bc

	ld a, [bitmap]
	bit 0, a
	jp nz,NotAnyKey ; If drawing is going on dont evaluate keys. because logic depends on addresses printed to vram

	call GetDPad
	ld b,a
	and a, DPAD_DOWN
	jp z, DownKeyPressed
	ld a, [bitmap]
	res 2, a ; bitmap third bit means down still pressed
	ld [bitmap], a
	jp NoMovementDown
DownKeyPressed:
	ld hl, _OAMRAM
    ld a, [hl] ; get y position of arrow
	cp a, $81
	jp nc, ScrollTextDown
	ld a, [bitmap]
	bit 2, a
	jp nz, NoMovementDown ; ignore holding down as long as arrow not at bottom
	ld a, [hl]
	add a, 8
	ld [hl], a 
	ld a, [bitmap]
	set 2, a ; bitmap third bit means down still pressed
	ld [bitmap], a
	jp NoMovementDown

ScrollTextDown:
	ld a, [rSCY]
	;cp a,$60
	;jp z,NoMovementDown
	add a, ScrollSpeed
	ld [rSCY], a
	ld a, [counter]
	add a, ScrollSpeed
	cp a, 8
	jp nc, SetFlagNextLinePrint
NoFlagNextLinePrint:
	ld [counter],a
	jp NoMovementDown
SetFlagNextLinePrint:
	sub a, 8
	ld [counter], a
	ld a, [bitmap]
	set 1, a ; bitmap second bit means one line to print
	ld [bitmap], a


NoMovementDown:
	ld a,b
	and a, DPAD_UP
	jp z, UpKeyPressed
	ld a, [bitmap]
	res 3, a ; bitmap third bit means down still pressed
	ld [bitmap], a
	jp NoMovementUp
UpKeyPressed:
	ld hl, _OAMRAM
    ld a, [hl] ; get y position of arrow
	cp a, $15
	jp c, ScrollToBeginning
	ld a, [bitmap]
	bit 3, a
	jp nz, NoMovementUp ; ignore holding down as long as arrow not at bottom
	ld a, [hl]
	sub a, 8
	ld [hl], a 
	ld a, [bitmap]
	set 3, a ; bitmap third bit means down still pressed
	ld [bitmap], a
	jp NoMovementUp

ScrollToBeginning:
	ld bc, Cds
	ld de, StartVRAM
	ld a, [bitmap]
	set 0, a ; bitmap first bit means complete screen refresh
	ld [bitmap], a
	ld a, 0
	ld [rSCY], a
	pop hl ; value not needed but to keep stack correct
	pop hl
	jp MovementUpDone

NoMovementUp:
	call GetDPad
	and a, DPAD_LEFT
	jp nz, NoLeftKey
AskToDelete:
	; save arrow position
    ld a, [ _OAMRAM]
	ld d,a
	push de
	; get address top in screen
	call GetAddressInTopScreen
	ld d,e
	ld e,a
	push de

	call GetAddressOfArrow
	push bc
	ld de, SelectionDeleteLine
	ld hl, $9800
	call ShowSelectionScreen
	pop bc
	cp a, 1 ; Option 1 is YES
	jp z, SelectedToDelete
SelecteNotToDelete:
	jp redrawComplete

SelectedToDelete:

	push bc
	call DeleteInstructionAtAddress
	pop bc
	call DecreaseLabelAddress

	call FunctionToWaitForNextVBlank
redrawComplete:	
	pop bc ; set RAM address to addres found in vram top left
	
	pop de
	; set arrow to old position:
	ld hl, _OAMRAM
    ld a, d
    ld [hli], a
    ld a, ARROW_START_X
    ld [hli], a
    ld a, 0
    ld [hli], a
    ld [hl], a

	ld de, StartVRAM

	ld a, [bitmap]
	set 0, a ; bitmap first bit means complete screen refresh
	ld [bitmap], a
	ld a, 0
	ld [rSCY], a
	pop hl ; value not needed but to keep stack correct
	pop hl
	jp MovementUpDone

NoLeftKey:

	call GetDPad
	and a, DPAD_RIGHT
	jp nz, NoRightKey
AskToAdd:
	; save arrow position
    ld a, [ _OAMRAM]
	ld d,a
	push de
	; get address top in screen
	call GetAddressInTopScreen
	ld d,e
	ld e,a
	push de

	call GetAddressOfArrow
	push bc
SelectInstructionType:
	ld de, SelectionAddInstr
	ld hl, $9800
	call ShowSelectionScreen
	pop bc
	cp a, 0
	jp z, ExitSelectInstruction
	cp a, 1
	jp z, SelecteNOP
	cp a, 2 
	jp z, SelectedLDA
	cp a, 3
	jp z, SelectedAluR8
	cp a, 4
	jp z, SelectedAluD8
	cp a, 5
	jp z, SelectedJumpCall
	cp a, 6
	jp z, SelectedPushPop
	cp a, 7
	jp z, SelectedIncDec
	cp a, 8
	jp z,SelectedBitOperation
	cp a, 9
	jp z, SelectedLabelToAdd
	; no option select, should not happen
	call FunctionToWaitForNextVBlank
	jp redrawComplete
ExitSelectInstruction:
	call FunctionToWaitForNextVBlank
	jp redrawComplete
SelectedPushPop:
	resetArrow
	push bc
	ld de, SelectionPushPop
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0
	jp z,SelectInstructionType
	ld h, $C5
	cp a, 1
	jp z, AddPushPopCommon
	ld h, $D5
	cp a, 2
	jp z, AddPushPopCommon
	ld h, $E5
	cp a, 3
	jp z, AddPushPopCommon
	ld h, $F5
	cp a, 4
	jp z, AddPushPopCommon
	; pop instructions
	ld h, $C1
	cp a, 5
	jp z, AddPushPopCommon
	ld h, $D1
	cp a, 6
	jp z, AddPushPopCommon
	ld h, $E1
	cp a, 7
	jp z, AddPushPopCommon
	ld h, $F1
	cp a, 8
	jp z, AddPushPopCommon



AddPushPopCommon:
	ld e, 1
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,1
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete

SelectedIncDec:
	resetArrow
	push bc
	ld de, SelectionIncDec
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0
	jp z,SelectInstructionType
	cp a, 1
	jp z, SelectedInc8
	cp a, 2
	jp z, SelectedInc16
	cp a, 3
	jp z, SelectedDec8
	cp a, 4
	jp z, SelectedDec16


SelectedInc8:
	resetArrow

	call GetSelectionForR8
	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	moveBit0To2ThreeBitsLeft

	or a, $04
	ld h,a
	ld e,1
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,1
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete

SelectedInc16:
	resetArrow

	ld de, SelectionR16
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	moveBit0To1FourBitsLeft

	or a, $03
	ld h,a
	ld e,1
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,1
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete


SelectedDec8:
	resetArrow

	call GetSelectionForR8
	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	moveBit0To2ThreeBitsLeft

	or a, $05
	ld h,a
	ld e,1
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,1
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete

SelectedDec16:
	resetArrow

	ld de, SelectionR16
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	moveBit0To1FourBitsLeft

	or a, $0B
	ld h,a
	ld e,1
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,1
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete

SelectedJumpCall:
	resetArrow
	push bc

	ld de, SelectionAddJumpInstruction
	ld hl, $9800
	call ShowSelectionScreen

	cp a, 0
	jp z,SelectInstructionType
	cp a, 1
	jp z, SelectedToAddConditionalJump
	cp a, 2
	jp z, SelectedToAddUnconditionalJump
	cp a, 3
	jp z, SelectedToAddUnconditionalCall
	cp a, 4
	jp z, SelectedToAddUnconditionalCallAddress
	cp a, 5
	jp z, SelectedToAddRet
	pop bc

	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete
BackToSelectedJumpCallPopBC:
	pop bc
	jp SelectedJumpCall
SelectedToAddConditionalJump:
	resetArrow
	ld de, SelectionCondition
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0
	jp z,BackToSelectedJumpCallPopBC
	dec a
	moveBit0To2ThreeBitsLeft
	push af
InvalidLabelNumerConditionalJump:
	getLabelAddress
	cp a, $32
	jp nc, InvalidLabelNumerConditionalJump
	ld l,a ; second byte of instruction is label address
	pop af
	or a, $C2 ; conditional jump instruction
	ld h, a
	ld d, HIGHER_BYTE_USED_FOR_LABELS; third byte of instruction

	ld e, 3
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,3
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete
SelectedToAddUnconditionalJump:
	resetArrow
InvalidLabelUnconditionalJump:
	getLabelAddress
	cp a, $32
	jp nc, InvalidLabelUnconditionalJump
	ld l,a ; second byte of instruction is label address
	ld h, $C3 ; first byte of instruction
	ld d, HIGHER_BYTE_USED_FOR_LABELS; third byte of instruction

	ld e, 3
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,3
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete

SelectedToAddUnconditionalCall:
	resetArrow
InvalidLabelUnconditionalCall:
	getLabelAddress
	cp a, $32
	jp nc, InvalidLabelUnconditionalCall
	ld l,a ; second byte of instruction is label address
	ld h, $CD ; first byte of instruction
	ld d, HIGHER_BYTE_USED_FOR_LABELS; third byte of instruction

	ld e, 3
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,3
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete

SelectedToAddUnconditionalCallAddress:
	resetArrow
	call GetSelectionForSingleByte ;returns byte in a
	push af
	call GetSelectionForHigherSingleByte

	ld l,a
	pop af
	ld d, a
	ld h, $CD

	ld e, 3
	pop bc
	push bc
	call AddInstructionAtAddress
	pop bc
	ld e,3
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	resetArrow
	jp redrawComplete

SelectedToAddRet:
	pop bc
	ld e,1
	call IncreaseLabelAddress
	ld h,$C9
	ld e,1
	call AddInstructionAtAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelecteNOP:
	resetArrow
	
	ld e,1
	call IncreaseLabelAddress
	ld h,0
	ld e,1
	call AddInstructionAtAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete
SelectedLDA:
	resetArrow
	push bc

	ld de, SelectionAddLoadInstr
	ld hl, $9800
	call ShowSelectionScreen

	cp a, 0
	jp z,SelectInstructionType
	cp a, 1
	jp z, SelectedLdR8R8
	cp a, 2
	jp z, SelectedLdR8D8
	cp a, 3
	jp z,SelectedLdAMemoryd16
	cp a,4
	jp z, SelectedLMemoryd16FromA
	cp a, 5
	jp z, SelectedLdR16fromD16
	cp a, 6
	jp z, SelectedLdR16FromA
	cp a, 7
	jp z, SelectedLdR16ToA
	pop bc

	ld e, 3
	call IncreaseLabelAddress
	ld hl, $FA00
	ld de, $FF03
	call AddInstructionAtAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete
SelectedLdR8D8:
	call FunctionToWaitForNextVBlank
	resetArrow
	call GetSelectionForR8

	cp a, 0 ; go back
	jp z,GoBackToLoad
	dec a ; substract back option from regsiter so b:0
	push af ; save register selected

	call GetSelectionForSingleByte ;returns byte in a

	ld l, a ; second byte of instruciton in l
	pop af ; Register select in a
	moveBit0To2ThreeBitsLeft
	or a, $06 ; or instruction
	ld h, a ; first byte of instruction in h
	ld e,2 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 2
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedLdR8R8:
	call FunctionToWaitForNextVBlank
	resetArrow

	;; Get first Register
	ld hl, $9800
	ld de, HeadlineLdR8R8RegisterDest
	ld bc, HeadlineLdR8R8RegisterDestEnd-HeadlineLdR8R8RegisterDest
	call printStr
	ld de, SelectionR8
	ld hl, $9800
	call ShowSelectionScreen

	cp a, 0 ; go back
	jp z,GoBackToLoad
	dec a ; substract back option from regsiter so b:0
	moveBit0To2ThreeBitsLeft
	push af ; save register selected
	
	;; Get second Register
	ld hl, $9800
	ld de, HeadlineLdR8R8RegisterSrc
	ld bc, HeadlineLdR8R8RegisterSrcEnd-HeadlineLdR8R8RegisterSrc
	call printStr
	ld de, SelectionR8
	ld hl, $9800
	call ShowSelectionScreen

	cp a, 0 ; go back
	jp z,GoBackToLoad
	dec a ; substract back option from regsiter so b:0
	
	pop de
	or a, d ; combine both registers
	or a, $40 ; Ld r8,r8 instruction
	ld h, a ; first byte of instruction in h
	ld e,1 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 1
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedLdAMemoryd16:
	call FunctionToWaitForNextVBlank
	resetArrow

	call GetSelectionForSingleByte ;returns byte in a
	push af
	call GetSelectionForHigherSingleByte
	

	ld h, $FA ; first byte of instruction in h
	ld l, a ; Second byte of instruction
	pop af
	ld d, a ; third byte of instruction 
	ld e,3 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 3
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedLMemoryd16FromA:
	call FunctionToWaitForNextVBlank
	resetArrow

	call GetSelectionForSingleByte ;returns byte in a
	push af
	call GetSelectionForHigherSingleByte
	

	ld h, $EA ; first byte of instruction in h
	ld l, a ; Second byte of instruction
	pop af
	ld d, a ; third byte of instruction 
	ld e,3 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 3
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedLdR16fromD16:
	call FunctionToWaitForNextVBlank
	resetArrow

	;; Get first Register
	ld de, SelectionR16
	ld hl, $9800
	call ShowSelectionScreen

	cp a, 0 ; go back
	jp z,GoBackToLoad
	dec a ; substract back option from regsiter so b:0
	moveBit0To1FourBitsLeft
	push af ; save register selected
	
	call GetSelectionForSingleByte ;returns byte in a
	push af
	call GetSelectionForHigherSingleByte
	
	pop de ; third byte of instruction in d
	ld l ,a ; second byte of instruction
	pop af
	or a, $01 ; Ld r16,d16 instruction
	ld h, a ; first byte of instruction in h
	ld e,3 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 3
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedLdR16FromA:
	call FunctionToWaitForNextVBlank
	resetArrow

	ld de, SelectionR16IndirectLoad
	ld hl, $9800
	call ShowSelectionScreen

	cp a, 0 ; go back
	jp z,GoBackToLoad
	dec a ; substract back option from regsiter so b:0
	moveBit0To1FourBitsLeft
	
	or a, $02 ; Ld r16,d16 instruction
	ld h, a ; first byte of instruction in h
	ld e,1 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 1
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedLdR16ToA:
	call FunctionToWaitForNextVBlank
	resetArrow

	ld de, SelectionR16IndirectLoad
	ld hl, $9800
	call ShowSelectionScreen

	cp a, 0 ; go back
	jp z,GoBackToLoad
	dec a ; substract back option from regsiter so b:0
	moveBit0To1FourBitsLeft
	
	or a, $0A ; Ld r16,d16 instruction
	ld h, a ; first byte of instruction in h
	ld e,1 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 1
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedAluR8:
	push bc
	call FunctionToWaitForNextVBlank
	resetArrow

	; Get arithmetic operation
	ld de, SelectionAlu
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	moveBit0To2ThreeBitsLeft
	push af

	call GetSelectionForR8

	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a

	pop de
	or a,d
	or a, $80
	ld h, a ; single byte of instruction in h
	ld e,1 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 1
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete

SelectedAluD8:
	push bc
	call FunctionToWaitForNextVBlank
	resetArrow

	; Get arithmetic operation
	ld de, SelectionAlu
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	moveBit0To2ThreeBitsLeft
	push af

	call GetSelectionForSingleByte


	ld l, a ; second byte of instruction is D8
	pop af
	or a, $C6 ; instruction
	ld h, a ; first byte of instruction in h
	ld e,2 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 2
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete


SelectedBitOperation:
	push bc
	call FunctionToWaitForNextVBlank
	resetArrow

	; Get arithmetic operation
	ld de, SelectionBitOperation
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	moveBit0To1SixBitsLeft
	push af

	cp a, 0
	jp z, SelectedShiftOperation
	ld de, SelectionD3
	ld hl, $9800
	call ShowSelectionScreen
	jp ContinueBitOperationWithRegister

SelectedShiftOperation:
	ld de, SelectionShiftOperation
	ld hl, $9800
	call ShowSelectionScreen

ContinueBitOperationWithRegister:
	moveBit0To2ThreeBitsLeft
	push af
	call GetSelectionForR8

	cp a, 0 ; go back
	jp z,SelectInstructionType
	dec a
	pop de
	
	or a, d ; bit to modify
	pop de
	or a, d ; operation bit, res, set

	ld h, $CB ; CB prefixed instruction
	ld l, a ; real operation in second byte
	ld e,2 ; instruction 2 byte long
	pop bc ; address where to add
	push bc
	call AddInstructionAtAddress
	ld e, 2
	pop bc
	call IncreaseLabelAddress
	call FunctionToWaitForNextVBlank
	jp redrawComplete


GoBackToLoad:
	pop bc
	jp SelectedLDA
SelectedLabelToAdd:
	resetArrow
	push bc

	getLabelAddress
	cp a, $32
	jp nc, InvalidLabelNumer
	sla a; multiply with 2
	res 0,a
	ld hl, CdsLabels
	ld d,0
	ld e, a
	add hl, de ; calculate slot for label


	pop bc

	ld a, c
	ld [hli],a ; store address in slot 
	ld a, b
	ld [hl],a

	call FunctionToWaitForNextVBlank
	jp redrawComplete
InvalidLabelNumer:
	pop bc
	call FunctionToWaitForNextVBlank
	jp redrawComplete

NoRightKey:
	call GetButtons
	and a, BUTTON_START
	jp nz, NoAKey
	ld de, SelectionRunCode
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 1 ; Option 1 is YES
	jp z, StartCode
	resetArrow
	jp ScrollToBeginning
StartCode:
	call FunctionToWaitForNextVBlank
	call ClearOam
	call ReplaceLabelsWithRealAddresses
	; enable interrupt to return here
	ei
	ld hl,$FFFF
	ld a, $01
	ld [hl],a
	call Cds ; TEMP disabled to test replace
ContinueAfterCodeRun:
	; disable interrupts again
	di
	ld hl,$FFFF
	ld a, $00
	ld [hl],a
	call FunctionToWaitForNextVBlank
	call ClearOam
	call FunctionToWaitForNextVBlank
	resetArrow
	call ReplaceRealAddressesWithLabels
	ld sp,$FFFE ; reset stack 
	push hl
	push hl ; push two dummy values as ScrollToBeginning assumes two values on the stack
	jp ScrollToBeginning
NoAKey:

	call GetButtons
	and a, BUTTON_SELECT
	jp nz, NoSelectKey
	ld de, SelectionOptions
	ld hl, $9800
	call ShowSelectionScreen
	cp a, 1 
	jp z, SaveCode
	cp a, 2 
	jp z, LoadSaveCode
	cp a, 3 
	jp z, LoadSample1
	cp a, 4 
	jp z, LoadSound1
	cp a, 5 
	jp z, LoadEmpty
	resetArrow
	jp ScrollToBeginning
SaveCode:
	;Enable external ram
	ld a, $A
	ld [$0000],a
	ld de,BatterySaveStart
	ld hl, BATTERY_RAM
	ld bc, BatterySaveEnd-BatterySaveStart
	call MemCopy
	; Disable external Ram
	ld a, 0
	ld [$0000],a
	call FunctionToWaitForNextVBlank
	resetArrow
	jp ScrollToBeginning
LoadSaveCode:
	;Enable external ram
	ld a, $A
	ld [$0000],a
	ld de, BATTERY_RAM
	ld hl, BatterySaveStart
	ld bc, BatterySaveEnd-BatterySaveStart
	call MemCopy
	; Disable external Ram
	ld a, 0
	ld [$0000],a
	call FunctionToWaitForNextVBlank
	resetArrow
	jp ScrollToBeginning
LoadSample1:
	ld de, SampleCode1
	ld hl, Cds
	ld bc, SampleCode1End - SampleCode1
	call MemCopy
	ld de, SampleCode1Labels
	ld hl, CdsLabels
	ld bc, SampleCode1LabelsEnd - SampleCode1Labels
	call MemCopy
	call FunctionToWaitForNextVBlank
	resetArrow
	jp ScrollToBeginning

LoadSound1:
	ld de, SampleSound1
	ld hl, Cds
	ld bc, SampleSound1End - SampleSound1
	call MemCopy
	call FunctionToWaitForNextVBlank
	resetArrow
	jp ScrollToBeginning


LoadEmpty:
	ld hl, Cds
	ld bc, CdsEnd - Cds
LoopLoadEmpty:
	ld a,0
	ld [hli],a
	dec bc
	ld a, b
	or a, c
	jp nz,LoopLoadEmpty

	ld hl, CdsLabels
	ld bc, CdsLabelsEnd - CdsLabels
LoopLoadEmptyLabels:
	ld a,$FF
	ld [hli],a
	dec bc
	ld a, b
	or a, c
	jp nz,LoopLoadEmptyLabels
	call FunctionToWaitForNextVBlank
	resetArrow
	jp ScrollToBeginning

NoSelectKey:
NotAnyKey:
	pop bc
	pop de
MovementUpDone:


WaitB102:
	ld a, [rLY]
	cp 144
	jp nc, WaitB102

	ld a, [FoundLabel]
	cp a, $FF
	jp z,StartFindLabel
	ld a, [bitmap]
	bit 5, a ; bitmap check label was really printed
	jp z, SkipLabelSearch; if label was not printed inbetween
	res 5, a
	ld [bitmap], a
	ld a, $FF
	ld [FoundLabel],a
	jp SkipLabelSearch

StartFindLabel:
	ld a, [bitmap]
	bit 4, a
	jp nz , SkipLabelSearch
	ld a, [bitmap]
	set 4, a
	ld [bitmap], a
	push de
	push bc

	call FindAddressInLabels
	ld [FoundLabel],a
	pop bc
	pop de
SkipLabelSearch:

WaitVBlank102:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank102

	ld a, d
	cp a, $9b
	jp c, checkIfPrint
	; first time we get in vram above $9b00 set bit to 0
	ld a, [bitmap]
	res 0, a
	ld [bitmap], a

checkIfPrint:
	ld a, [bitmap]
	ld h, a
	res 2, a; ignore holding down key bit
	res 3, a; ignore holding key up bit
	res 4, a; ignore label print
	cp a,0
	jp z, SkipPrint ; If no bit set skip print
	ld a, h
	res 1,a ;
	ld [bitmap], a
	jp printIntrucstionLoop


DEF SAMPLE_CODE1_INVERSE_SPEED = 10

EndMain:
	nop
DEF SP1_LABEL_NO_MOVE = 0
DEF SP1_LABEL_MOVE_TO_THE_RIGHT = 1
DEF SP1_LABEL_NOT_REACHED_RIGTH = 2
DEF SP1_LOOP = 3
SampleCode1Labels:
dw Cds+(  noMove  -SampleCode1) ; LABEL 0 = SP1_LABEL_NO_MOVE
dw Cds+(  moveToTheRight  -SampleCode1) ; LABEL 1 = SP1_LABEL_MOVE_TO_THE_RIGHT
dw Cds+(  NotRechedRight  -SampleCode1) ; LABEL 2 = SP1_LABEL_NOT_REACHED_RIGTH
dw Cds+(  SampleCode1Loop  -SampleCode1); LABEL 3 = SP1_LOOP
SampleCode1LabelsEnd:
	nop
SampleCode1:
	call ClearBackground
; Create tile
	ld hl, $8010

	ld a,$28
	ld [hli],a
	ld a,$10
	ld [hli],a

	ld a,$54
	ld [hli],a
	ld a,$38
	ld [hli],a

	ld a,$44
	ld [hli],a
	ld a,$10
	ld [hli],a

	ld a,$00
	ld [hli],a
	ld a,$10
	ld [hli],a

	ld a,0
	ld [hli],a
	ld a,$38
	ld [hli],a

	ld a,0
	ld [hli],a
	ld a,$10
	ld [hli],a

	ld a,0
	ld [hli],a
	ld a,$28
	ld [hli],a

	ld a,$44
	ld [hli],a
	ld a,$6c
	ld [hli],a

	ld b,0 ; counter if it reaches SAMPLE_CODE1_INVERSE_SPEED i gets reset
	ld d, 50 ; current position
	ld e, 1 ; direction 1: right ; 2: else move to the left
SampleCode1Loop:
	call FunctionToWaitForNextVBlank
	ld a,b
	sub a, SAMPLE_CODE1_INVERSE_SPEED ; 
	jp nz, SP1_LABEL_NO_MOVE +HIGHER_BYTE_USED_FOR_LABELS*256 
	ld b, 0 ; Set move counter
	ld a, e
	cp a, 1
	jp z, SP1_LABEL_MOVE_TO_THE_RIGHT + HIGHER_BYTE_USED_FOR_LABELS*256 
	dec d
	dec d
moveToTheRight:
	inc d
	ld a, d
	cp a, 150
	jp c, SP1_LABEL_NOT_REACHED_RIGTH + HIGHER_BYTE_USED_FOR_LABELS*256 
	dec d
	ld e,2 ; swith to move right next
	jp SP1_LABEL_NO_MOVE + HIGHER_BYTE_USED_FOR_LABELS*256 
NotRechedRight:
	cp a, 10
	jp nc, SP1_LABEL_NO_MOVE + HIGHER_BYTE_USED_FOR_LABELS*256 
	ld e,1
noMove:	
	inc b
	ld hl, _OAMRAM
    ld a, 100 ; y cooridate
    ld [hl], a
	inc hl
    ld a, d ; x cooridnate
    ld [hl], a
	inc hl
    ld a, 1 ; tile index 1
    ld [hl], a
	inc hl
	ld a, 0 ; pallete 0
    ld [hl], a
	jp SP1_LOOP + HIGHER_BYTE_USED_FOR_LABELS*256 
SampleCode1Exit:
	ret
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
SampleCode1End:
	nop


SampleSound1:
    
TurnSoundOn:
	ld a, $8f
    ld [rAUDENA], a

    ld a, $ff
    ld [rAUDTERM], a

    ld a, $77
    ld [rAUDVOL], a

PlaySound:
    ld a, $15
    ld [rNR10], a
    ld a, $80
	ld [rNR11], a 
    ld a, $eb
	ld [rNR12], a
    ld a, $ab
	ld [rNR13], a
    ld a, $cd
	ld [rNR14], a

    ret
	nop
	nop
	nop
	nop
	nop
	nop
SampleSound1End:

;@param de source
;@param hl destination
;@param bc length
MemCopy:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, MemCopy

; @Param bc Address in Cds 
; @return a number of Label or $FF if no label found
FindAddressInLabels:
    ld hl, CdsLabels
    ld e, (CdsLabelsEnd - CdsLabels) / 2 + 1
	ld a, 0
LoopOverCdsLabels:
	dec e
	jp z, NoCdLabelFound
    ld a,[hli]
	ld d, a
	ld a, [hli]
	cp a, b
	jp nz, LoopOverCdsLabels
	ld a, d
	cp a, c
	jp nz, LoopOverCdsLabels
LabelFound:
	ld a, (CdsLabelsEnd - CdsLabels) / 2
	sub a,e
	ret
NoCdLabelFound:
	ld a, $FF
	ret


; @Param bc Address in Cds after which increase should happen
; @Param e how much to increase
IncreaseLabelAddress:
    ld hl, CdsLabels
    ld d, (CdsLabelsEnd - CdsLabels) / 2 + 1
LoopOverCdsLabelsIncrease:
	dec d
	jp z, Done
	push de
    ld a,[hli]
	ld d, a
	cp a, c
	jp c, FirstByteSmaller ; least significant byte
	ld a, [hli]
CheckSecondByte:
	cp a, b
	jp c, ByteSmaller
	push hl
	ld h, a
	ld l, d
	ld d, 0
	add hl, de ; Calculate increased address
	ld d, h
	ld e, l
	pop hl
	dec hl
	ld a, d
	ld [hl],a
	dec hl
	ld a, e
	ld [hl],a
	inc hl
	inc hl
ByteSmaller:
	pop de
	jp LoopOverCdsLabelsIncrease
FirstByteSmaller:
	ld a, [hli] ; load most significant byte
	dec a ;; as least significant byte was smaller higher bytes needs to be > not >=
	jp CheckSecondByte
Done:
	ret

; @Param bc Address in Cds after which decrease should happen
; @Param e how much to decrease
; Does not modidy bc
DecreaseLabelAddress:
	inc bc ; if instruction at label is removed do not increase label address
    ld hl, CdsLabels
    ld d, (CdsLabelsEnd - CdsLabels) / 2 + 1
LoopOverCdsLabelsDecrease:
	dec d
	jp z, DoneDecreas
	push de
    ld a,[hli]
	ld d, a
	cp a, c
	jp c, FirstByteSmallerDecrease
	ld a, [hli]
CheckSecondByteDecrease:
	cp a, b
	jp c, ByteSmallerDecrease
	push hl
	ld h,a
	ld a, d
	sub a, e
	ld e, a
	ld d, h
	pop hl
	jp nc, noCarryDecrease
	dec d
noCarryDecrease:
	dec hl
	ld a, d
	ld [hl],a
	dec hl
	ld a, e
	ld [hl],a
	inc hl
	inc hl
ByteSmallerDecrease:
	pop de
	jp LoopOverCdsLabelsDecrease
FirstByteSmallerDecrease:
	ld a, [hli]
	dec a
	jp CheckSecondByteDecrease
DoneDecreas:
	dec bc
	ret

;  @param bc: addres to delete
; return e length of deleted address
DeleteInstructionAtAddress:
	push bc
	ld h, b
	ld l, c
	ld a,[hl]
	ld c,a
	getLengthOfInstruction
	ld a,c
	pop bc
	ld h, b
	ld l, c
	ld d, 0
	ld e, a
	add hl,de
	; now copy backwards from hl to bc
LoopCopyBack:
	ld a, h
	cp a, HIGHER_BYTE_ADDRESS_CODE_END
	jp nc, EndOfCopyBack
	ld a, [hli]
	ld [bc],a
	inc bc
	jp LoopCopyBack
EndOfCopyBack:
	ret

; @param bc: addres where to add instruction
; @param h: first byte
; @param l: second byte
; @param d: third byte
; @param e: number of bytes (1,2 or 3)
AddInstructionAtAddress:
	push hl
	push de
	push bc
	ld hl, CdsEnd
	dec hl
	dec hl
	dec hl
	ld b,h
	ld c, l
	ld d,0
	add hl,de ; hl points lenght bytes after bc
	ld d, b
	ld e, c
	; now copy backwards from de to hl
	pop bc
	push bc
AddLoopCopyBack:
	ld a, h
	cp a, b
	jp nz, ByteNotSame
	ld a,l
	cp a, c
	jp z, AddEndOfCopyBack
ByteNotSame:
	ld a, [de]
	ld [hl],a
	dec de
	dec hl
	jp AddLoopCopyBack
AddEndOfCopyBack:
	pop bc
	pop de
	pop hl
	ld a, h ; copy first byte
	ld [bc],a 
	inc bc
	dec e
	ld a, e
	cp a, 0
	jp z, AddDone
	ld a, l ; copy second byte
	ld [bc],a 
	inc bc
	dec e
	ld a, e
	cp a, 0
	jp z, AddDone
	ld a, d ; copy second byte
	ld [bc], a
	inc bc
AddDone:
	ret

; @param nothing
ReplaceLabelsWithRealAddresses:
	ld hl, Cds ; next address to read
LoopReplaceLabels:
	ld a, [hl]
	ld d, a ; store byte value of instr in d
	cp a, $CD ; uncondional call
	jp z, FoundJumpOrCall
	cp a, $C3 ; unconditional jump
	jp z, FoundJumpOrCall
	res 5,a
	res 4,a
	res 3,a
	cp a, $C2 ; conditional jump
	jp z, FoundJumpOrCall

ContinueToNextInstruction:
	ld a,h
	cp a, HIGHER_BYTE_ADDRESS_CODE_END
	jp nc, DoneReplaceLabels
	ld c,d ; Instruction value is now in c
	push hl
	getLengthOfInstruction ;; return lenghts of current instruction in bc
	pop hl
	add hl, bc
	jp LoopReplaceLabels

FoundJumpOrCall:
	inc hl
	inc hl
	ld a, [hl] ; check high byte
	inc hl
	cp a, HIGHER_BYTE_USED_FOR_LABELS
	jp nz, LoopReplaceLabels ; call to some other address without a label
	dec hl
	dec hl
	ld a, [hl] ; get number of label
	sla a; multiply 2
	res 0,a
	ld d,0
	ld e, a ; no need to handle carry as number of labels <256/2

	; get label address
	push hl
	ld hl, CdsLabels
	add hl, de
	ld a, [hli]
	ld d,a
	ld a, [hli]
	ld c,a
	pop hl

	; write address to instruction
	ld a,d
	ld [hli],a
	ld a,c
	ld [hli],a

	jp LoopReplaceLabels

DoneReplaceLabels:
ret


; @param nothing
ReplaceRealAddressesWithLabels:
	ld hl, Cds ; next address to read
LoopReplaceLabels2:
	ld a, [hl]
	ld d, a ; store byte value of instr in d
	cp a, $CD ; uncondional call
	jp z, FoundJumpOrCall2
	cp a, $C3 ; unconditional jump
	jp z, FoundJumpOrCall2
	res 5,a
	res 4,a
	res 3,a
	cp a, $C2 ; conditional jump
	jp z, FoundJumpOrCall2

ContinueToNextInstruction2:
	ld a,h
	cp a, HIGHER_BYTE_ADDRESS_CODE_END
	jp nc, DoneReplaceLabels2
	ld c,d ; Instruction value is now in c
	push hl
	getLengthOfInstruction ;; return lenghts of current instruction in bc
	pop hl
	add hl, bc
	jp LoopReplaceLabels2

FoundJumpOrCall2:
	inc hl
	ld a, [hli] ; get address
	ld c,a
	ld a, [hli] ; get address
	ld b, a
	
	push hl
	call FindAddressInLabels ; bc is the address to search for
	pop hl
	cp a, $FF ;; nothing found
	jp z, LoopReplaceLabels2

	dec hl
	dec hl
	; write address to instruction
	ld [hli],a
	ld a, HIGHER_BYTE_USED_FOR_LABELS
	ld [hli],a

	jp LoopReplaceLabels2

DoneReplaceLabels2:
ret


; return address in bc
GetAddressOfArrow:
	ld hl, _OAMRAM
    ld a, [hl]
	ld b, a
	ld a, [rSCY]
	add a, b ; calculate position of arrow in tilemap
	; now devide by 8
CalculateAdressPosition:
	sub a, $10 ; account for initial offest of arrow tile
	sra a ; shift a 3 bits right so register is in a
	sra a
	sra a
	res 5,a
	res 6,a
	res 7,a
	; loop to get correct address
	ld hl,$9800
	ld b, 0
	ld c, $20 ; Vram line size
LoopCalculateStartTile:
	cp a, 0
	jp z, FoundStartTileAddress
	dec a
	add hl,bc
	jp LoopCalculateStartTile
FoundStartTileAddress:
	ld a, [hli]
	ld d,a
	ld a, [hli]
	ld b, a
	ld a,d
	call DecodeByteFromVrame
	ld e,a
	ld a, [hli]
	ld d,a
	ld a, [hli]
	ld b, a
	ld a,d
	call DecodeByteFromVrame
	ld b, e
	ld c, a
	ret

; return address in bc
GetAddressInTopScreen:
	ld a, [rSCY]
	add a, 16
	; now devide by 8
	jp CalculateAdressPosition

; input in register a upper nibble b lower nibble
; output in register a
; does not modify e and hl
DecodeByteFromVrame:
	cp a, END_OF_NUMBER
	jp nc, BiggerThan9_1
	sub a, "0"
	jp NextNibble
BiggerThan9_1:
	sub a, "A"
	add a, 10
NextNibble	:
	ld c, a
	ld a, b
	cp a, END_OF_NUMBER
	jp nc, BiggerThan9_2
	sub a, "0"
	jp NotBiggerThan9_2
BiggerThan9_2:
	sub a, "A"
	add a, 10
NotBiggerThan9_2:
	ld b,a
	ld a,c
; @param upper nibble in a, lower nibble in b, return in a
CombineNibbles:
	sla a; move 4 bits to the left
	sla a;
	sla a;
	sla a;
	res 0,a ; set other bits 0
	res 1,a
	res 2,a
	res 3,a
	or a,b
	ret

; Return in register a 0: Back b:1 ....
GetSelectionForR8:
	ld hl, $9800
	ld de, HeadlineLdR8D8Register
	ld bc, HeadlineLdR8D8RegisterEnd-HeadlineLdR8D8Register
	call printStr

	ld de, SelectionR8
	ld hl, $9800
	call ShowSelectionScreen
	ret

; return byte in a
GetSelectionForSingleByte:
	ld hl, $9800
	ld de, HeadlineSelectFirstNibbled8
	ld bc, HeadlineSelectFirstNibbled8End-HeadlineSelectFirstNibbled8
	call printStr

	ld de, SelectionHexNibble
	ld hl, $9800
	call ShowSelectionScreen

	push af

	ld hl, $9800
	ld de, HeadlineSelectSecondNibbled8
	ld bc, HeadlineSelectSecondNibbled8End-HeadlineSelectSecondNibbled8
	call printStr

	ld de, SelectionHexNibble
	ld hl, $9800
	call ShowSelectionScreen
	pop de
	ld b, a
	ld a, d
	call CombineNibbles ; d8 in a
	ret

GetSelectionForHigherSingleByte:
	ld hl, $9800
	ld de, HeadlineSelectThirdNibbled8
	ld bc, HeadlineSelectThirdNibbled8End-HeadlineSelectThirdNibbled8
	call printStr

	ld de, SelectionHexNibble
	ld hl, $9800
	call ShowSelectionScreen

	push af

	ld hl, $9800
	ld de, HeadlineSelectForthNibbled8
	ld bc, HeadlineSelectForthNibbled8End-HeadlineSelectForthNibbled8
	call printStr

	ld de, SelectionHexNibble
	ld hl, $9800
	call ShowSelectionScreen
	pop de
	ld b, a
	ld a, d
	call CombineNibbles ; d8 in a
	ret

HeadlineSelectFirstNibbled8:
db "First nibble $0-$F"
HeadlineSelectFirstNibbled8End:

HeadlineSelectSecondNibbled8:
db "Second nibble $0-$F"
HeadlineSelectSecondNibbled8End:

HeadlineSelectThirdNibbled8:
db "Third nibble $0-$F"
HeadlineSelectThirdNibbled8End:

HeadlineSelectForthNibbled8:
db "Forth nibble $0-$F"
HeadlineSelectForthNibbled8End:

HeadlineSelectFirstNibble:
db "First nibble $0-$3"
HeadlineSelectFirstNibbleEnd:

HeadlineSelectSecondNibble:
db "Second $00-$31      "
HeadlineSelectSecondNibbleEnd:

HeadlineLdR8D8Register:
db "SET REGISTER TO D8   "
HeadlineLdR8D8RegisterEnd:

HeadlineLdR8R8RegisterDest:
db "SET DESTINATION REGISTER "
HeadlineLdR8R8RegisterDestEnd:

HeadlineLdR8R8RegisterSrc:
db "SET SOURCE REGISTER   "
HeadlineLdR8R8RegisterSrcEnd:

; new line: ;
; End: #
; new line without clear screen: {
SelectionRunCode:
db "RUN CODE?; NO           ; YES         #"

SelectionDeleteLine:
db "DELETE THIS INSTRUCTION?; NO           ; YES         #"

SelectionAddInstr:
db "INSERT ABOVE; BACK; nop ; ld : LOAD INSTR; add/sub/cp A, R8; add/sub/cp A, D8; jump/call; push/pop; inc/dec R8/R16; bit D3, R8; LABEL $00-$31  #"

SelectionOptions:
db "OPTIONS; BACK; SAVE CODE; LOAD SAVED CODE; LOAD SAMPLE GRAPHIC; LOAD SAMPLE SOUND; LOAD EMPTY (nop) #"

SelectionAddLoadInstr:
db "LOAD INSTR; BACK; ld R8/[HL], R8/[HL]; ld R8, D8; ld A, [D16]; ld [D16], A; ld R16, D16; ld [R16/HLI/HLD], a; ld a, [R16/HLI/HLD]#"

SelectionR16IndirectLoad:
db "INDIRECT MEMORY REG; BACK; BC; DE; HLI INCREMENTS HL; HLD DECREMENTS HL#"

SelectionHexNibble:
db "~ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; A; B; C; D; E; F#"

SelectionD3:
db "BIT; 0; 1; 2; 3; 4; 5; 6; 7#"


SelectionR8:
db "~ BACK; B; C; D; E; H; L; [HL]; A#"

SelectionAlu:
db "ARITHMETIC OPERATION; BACK; add a,; adc a,; sub a,; sbc a,; and a,; xor a,; or a,; cp a,#"

SelectionR16:
db "REGISTER PAIR; BACK; BC; DE; HL; SP#"

SelectionAddJumpInstruction:
db "JUMP INSTRUCTION ; BACK; jp cc, LABEL; jp LABEL; call LABEL; call d16; ret#"

SelectionCondition:
db "CONDITION FOR JUMP; BACK; nz: NOT ZERO; z: ZERO; nc: NO CARRY; c: CARRY#"

SelectionPushPop:
db "PUSH/POP; BACK; push bc; push de; push hl; push af; pop bc; pop de; pop hl; pop af#"

SelectionIncDec:
db "INC/DEC r8/r16; BACK; inc R8; inc R16; dec R8; dec R16#"

SelectionBitOperation:
db "BIT OPERATION; BACK; SHIFT/ROTATE R8; BIT D3, R8   TEST; RES D3, R8   RESET; SET D3, R8#"


SelectionShiftOperation:
db "SHIFT/ROTATE; rlc : ROTATE CARRY; rrc : RIGHT; rl : ROTATE LEFT; rr : ROTATE RIGHT; sla : SHIFT ARITHMETIC; sra : RIGHT; sll : SWAP; srl#"




; @param de: source string, first line question after that the options. every line ending with \n
; @param hl: destination tilenmap
; return a which option was selected starting at 0

; internal use b for number of options
ShowSelectionScreen:

	call FunctionToWaitForNextVBlank

	ld a, 0 
	ld [rSCY], a ; Reset view

	push hl
	; Set arrow to first option
	ld hl, _OAMRAM
	ld a,24
	ld [hli], a 
	ld a, 8
	ld [hli], a 

	pop hl
	push hl
	ld b, 0
SelectionScreenLoop:
	ld a, [de]
	cp a, "~"
	jp z, newLineSkipClear
	cp a, ";"
	jp nz, NoNewLine
	push bc
	ld b, h
	ld c, l 
	call printSpaceUntilEndOfLine
continueNewLine:
	call FunctionToWaitForNextVBlank
	pop bc
	pop hl
	ld a, b
	ld b, 0
	ld c, $20
	add hl, bc ; increase destination vram to next line
	inc a ; increase line count by one
	ld b, a
	push hl
	inc de
	jp SelectionScreenLoop
newLineSkipClear:
	push bc
	jp continueNewLine
NoNewLine:	
	cp a, "#"
	jp z, ExitSelectionScreenPrint
	ld [hli], a
	inc de
	jp SelectionScreenLoop
ExitSelectionScreenPrint:
	call FunctionToWaitForNextVBlank
	call printSpaceUntilEndOfLineHL
	pop hl
	push bc
	push hl
	; Print two empty lines
	ld b, 0
	ld c, $20
	add hl, bc
	ld b, h
	ld c, l 
	call printSpaceUntilEndOfLine
	pop hl
	ld b, 0
	ld c, $40
	add hl, bc
	ld b, h
	ld c, l 
	call printSpaceUntilEndOfLine
	pop bc

	dec b ; ignore first line
	ld d,$FE ; use d bit map for pressed previous
	ld c, 0 ; use c current arrow position and init to 0
MovementLoop:
WaitBShowSelectionScreen2:
	ld a, [rLY]
	cp 144
	jp nc, WaitBShowSelectionScreen2
WaitVBlankWaitBShowSelectionScreen2:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlankWaitBShowSelectionScreen2

	call GetDPad
	ld e,a
	and a, DPAD_DOWN
	jp z, DownKeyPressedSelection
	ld a, e
	and a, DPAD_UP
	jp z, UpKeyPressedSelection
	ld a, e
	call GetButtons
	and a, BUTTON_A
	jp z, RightKeyPressed

	ld d,$FE
	; check up key
	jp MovementLoop

UpKeyPressedSelection:
	xor a,d
	and a, DPAD_UP
	jp z, KeyPressNotChanged
	ld a, c
	cp a, 0
	jp z, KeyPressNotChanged
	ld d, 0
	ld hl, _OAMRAM
    ld a, [hl] ; get y position of arrow
	sub a, 8
	dec c
	ld [hl], a
	jp KeyPressNotChanged

DownKeyPressedSelection:
	xor a,d
	and a, DPAD_DOWN
	jp z, KeyPressNotChanged
	ld a, b
	dec a
	cp a, c ; check limit reached
	jp c, KeyPressNotChanged
	ld d, 0
	ld hl, _OAMRAM
    ld a, [hl] ; get y position of arrow
	add a, 8
	inc c
	ld [hl], a 


KeyPressNotChanged:
	jp MovementLoop
RightKeyPressed:
	ld a, c
	ret

ClearOam:
    ld a, 0
    ld b, 160
    ld hl, _OAMRAM
ClearOamLoop:
    ld [hli], a
    dec b
    jp nz, ClearOamLoop
	ret

printSpaceUntilEndOfLine:
	ld a, " "
	ld [bc],a
	inc bc
	ld a,c
	res 7, a
	res 6, a
	res 5, a
	cp a,$14
	jp c, printSpaceUntilEndOfLine
	ret

printSpaceUntilEndOfLineHL:
	ld a, " "
	ld [hli],a
	ld a,l
	res 7, a
	res 6, a
	res 5, a
	cp a,$13
	jp c, printSpaceUntilEndOfLineHL
	ret


; Parameter
; bc source address
; de destination address (tilemap)
; return a length read
PrintInstruction:
	; Store bc in SourceAdress
	ld a, b
	ld [SourceAdress], a
	ld a, c
	ld [SourceAdress+1], a
	ld h, b
	ld l, c
	ld b, 0
	ld c, [hl]; load first byte
	ld a, c
	rl c; multiply 2
	res 0,c
	jp nc,noOverflowContinue
	ld b,1
noOverflowContinue:
	ld hl,PrintInstructionTable
	add hl, bc
	ld b, [hl]
	inc hl
	ld c, [hl]
	ld h, c
	ld l, b
	ld c, a
	jp hl ; jump to PrintInstructionTable using first byte

; Paramter: 
; c first byte
; de  destination address 
NopText:
db "nop             "
PrintNop:
	ld h, d
	ld l, e
	ld de, NopText
	ld bc, 16
	call printStr
	ld a,1
	ret

RetText:
db "ret"
PrintRet:
	ld h, d
	ld l, e
	ld de, RetText
	ld bc, 3
	call printStr
	ld b,h
	ld c, l
	call printSpaceUntilEndOfLine
	ld a,1
	ret

PrintUnknown:
	ld a, "-"
	ld [de],a
	inc de
	ld a, " "
	REPT 15
  		ld [de],a
		inc de
	ENDR
	ld a,1
	ret

LdToHlIncrementText:
db "ld [HLI], A   "
PrintLdToHlIncrement:
	ld h, d
	ld l, e
	ld de, LdToHlIncrementText
	ld bc, 13
	call printStr
	ld a,1
	ret

LdToHlDecrementText:
db "ld [HLD], A   "
PrintLdToHlDecrement:
	ld h, d
	ld l, e
	ld de, LdToHlDecrementText
	ld bc, 13
	call printStr
	ld a,1
	ret

LdToBLText:
db "ld [BC], A    "
PrintLdToBL:
	ld h, d
	ld l, e
	ld de, LdToBLText
	ld bc, 13
	call printStr
	ld a,1
	ret

LdToDeText:
db "ld [DE], A    "
PrintLdToDe:
	ld h, d
	ld l, e
	ld de, LdToDeText
	ld bc, 13
	call printStr
	ld a,1
	ret


LdFromHlIncrementText:
db "ld A, [HLI]    "
PrintLdFromHlIncrement:
	ld h, d
	ld l, e
	ld de, LdFromHlIncrementText
	ld bc, 15
	call printStr
	ld a,1
	ret

LdFromHlDecrementText:
db "ld A, [HLD]   "
PrintLdFromHlDecrement:
	ld h, d
	ld l, e
	ld de, LdFromHlDecrementText
	ld bc, 13
	call printStr
	ld a,1
	ret

LdFromBcText:
db "ld A, [BC]    "
PrintLdFromBc:
	ld h, d
	ld l, e
	ld de, LdFromBcText
	ld bc, 13
	call printStr
	ld a,1
	ret

LdFromDeText:
db "ld A, [DE]    "
PrintLdFromDe:
	ld h, d
	ld l, e
	ld de, LdFromDeText
	ld bc, 13
	call printStr
	ld a,1
	ret


LdText:
db "ld "
PrintLoadAd8:
	ld h, d
	ld l, e
	ld de, LdText
	push bc
	ld bc, 3
	call printStr
	pop bc
	ld a, c
	sra a ; shift a 3 bits right so register is in a
	sra a
	sra a
	res 3,a ; set remaining bits to 0
	res 4,a
	res 5,a
	call PrintRegister
	printComma
	printSpace
	printDollar
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; next byte id immediate
	ld a, [hl]
	call PrintNumberAsHex
	call printSpaceUntilEndOfLine
	ld a, 2
	ret

PrintLoadRegisterToRegister:
	ld h, d
	ld l, e
	ld de, LdText
	push bc
	ld bc, 3
	call printStr
	pop bc
	ld a, c
	sra a ; shift a 3 bits right so register is in a
	sra a
	sra a
	res 3,a ; set remaining bits to 0
	res 4,a
	res 5,a
	res 6,a
	res 7,a
	ld e,c
	push de
	call PrintRegister
	printComma
	printSpace
	pop de
	ld a, e
	res 3,a ; set remaining bits to 0
	res 4,a
	res 5,a
	res 6,a
	res 7,a
	ld h,b
	ld l, c
	call PrintRegister
	call printSpaceUntilEndOfLine
	ld a, 1
	ret

PrintLoadd16:
	ld h, d
	ld l, e
	ld de, LdText
	push bc
	ld bc, 3
	call printStr
	pop bc
	ld a, c
	sra a ; shift a 4 bits right so register is in a
	sra a
	sra a
	sra a
	res 4,a ; set remaining bits to 0
	res 5,a
	res 6,a
	res 7,a
	call PrintRegisterPair
	printComma
	printSpace
	printDollar
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; first the third byte of instruction
	inc hl
	ld a, [hl]
	call PrintNumberAsHex
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; then second byte of instructions
	ld a, [hl]
	call PrintNumberAsHex
	ld a, 3
	ret

PrintLoadAtoImmediateMemory:
	ld h, d
	ld l, e
	ld de, LdText
	ld bc, 3
	call printStr
	ld b, h
	ld c, l
	printOpenBracket
	printDollar
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; first the third byte of instruction
	inc hl
	ld a, [hl]
	call PrintNumberAsHex
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; then second byte of instructions
	ld a, [hl]
	call PrintNumberAsHex
	printClosingBracket
	printComma
	printSpace
	printA
	ld a, 3
	ret

PrintLoadImmediateMemoryToA:
	ld h, d
	ld l, e
	ld de, LdText
	ld bc, 3
	call printStr
	ld b, h
	ld c, l
	printA
	printComma
	printSpace
	printOpenBracket
	printDollar
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; first the third byte of instruction
	inc hl
	ld a, [hl]
	call PrintNumberAsHex
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; then second byte of instructions
	ld a, [hl]
	call PrintNumberAsHex
	printClosingBracket
	ld a, 3
	ret


AddText:
db "add A, "
AdcText:
db "adc A, "
SubText:
db "sub A, "
SbcText:
db "sbc A, "
AndText:
db "and A, "
XorText:
db "xor A, "
OrText:
db "or A,  "
CpText:
db "cp A,  "
PrintAddD8:
	ld h, d
	ld l, e
	ld de, AddText
	jp PrintAluD8

PrintAdcD8:
	ld h, d
	ld l, e
	ld de, AdcText
	jp PrintAluD8

PrintSubD8:
	ld h, d
	ld l, e
	ld de, SubText
	jp PrintAluD8
PrintSbcD8:
	ld h, d
	ld l, e
	ld de, SbcText
	jp PrintAluD8
PrintAndD8:
	ld h, d
	ld l, e
	ld de, AndText
	jp PrintAluD8
PrintXorD8:
	ld h, d
	ld l, e
	ld de, XorText
	jp PrintAluD8
PrintOrD8:
	ld h, d
	ld l, e
	ld de, OrText
	jp PrintAluD8

PrintCP:
	ld h, d
	ld l, e
	ld de, CpText

PrintAluD8:
	ld bc, 6
	call printStr
	ld b, h
	ld c, l
	printDollar
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; immediate is next byte
	ld a, [hl]
	call PrintNumberAsHex
	call printSpaceUntilEndOfLine
	ld a, 2
	ret

PrintAddR8:
	ld h, d
	ld l, e
	ld de, AddText
	jp PrintAluR8

PrintAdcR8:
	ld h, d
	ld l, e
	ld de, AdcText
	jp PrintAluR8

PrintSubR8:
	ld h, d
	ld l, e
	ld de, SubText
	jp PrintAluR8

PrintSbcR8:
	ld h, d
	ld l, e
	ld de, SbcText
	jp PrintAluR8

PrintAndR8:
	ld h, d
	ld l, e
	ld de, AndText
	jp PrintAluR8

PrintXorR8:
	ld h, d
	ld l, e
	ld de, XorText
	jp PrintAluR8

PrintOrR8:
	ld h, d
	ld l, e
	ld de, OrText
	jp PrintAluR8

PrintCPR8:
	ld h, d
	ld l, e
	ld de, CpText
PrintAluR8:
	push bc	
	ld bc, 7
	call printStr
	pop bc
	ld a,c

	res 3,a 
	res 4,a 
	res 5,a 
	res 6,a
	res 7,a

	call PrintRegister
	ld a, 1
	ret



BitOperations:
db "bit res set "


PrintCBPrefixed:
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; instruction is after CB Prefix
	ld a, [hl]

	ld b, a
	and a, $C0
	cp a, 0
	jp z, PrintShiftOperation
	push bc
	moveBit6To7FourBitsRight
	sub a,4
	ld b, 0
	ld c, a

	ld hl,BitOperations
	add hl, bc
	ld b, h
	ld c, l

	ld h, d
	ld l, e
	ld d, b
	ld e, c
	ld bc, 4
	call printStr
	pop af
	push af
	moveBit3To5ThreeBitsRight
	add a, "0"
	ld [hli],a
	ld a, ","
	ld [hli],a
	ld a, " "
	ld [hli],a
	pop af
	and a, $07
	call PrintRegister
	call printSpaceUntilEndOfLine
	ld a, 2
	ret

ShiftOperations:
db "rlc rrc rl  rr  sla sra sll srl "

PrintShiftOperation:
	ld a, b
	push af
	moveBit3To5OneBitRight
	ld b, 0
	ld c, a
	ld hl,ShiftOperations
	add hl, bc
	ld b, h
	ld c, l

	ld h, d
	ld l, e
	ld d, b
	ld e, c
	ld bc, 4
	call printStr

	pop af
	and a, $07

	call PrintRegister
	call printSpaceUntilEndOfLine
	ld a, 2
	ret


PushText:
db "push "
PrintPush:
	ld h, d
	ld l, e
	ld de, PushText	
CommonPushPop:
	push bc
	ld bc, 5
	call printStr
	pop bc
	ld a, c
	ld b, h
	ld c, l
	sra a ; shift a 4 bits right so register is in a
	sra a
	sra a
	sra a
	res 2,a ; set remaining bits to 0
	res 3,a ; set remaining bits to 0
	res 4,a ; set remaining bits to 0
	res 5,a
	res 6,a
	res 7,a
	call PrintRegisterPairPushPop
	call printSpaceUntilEndOfLine
	ld a, 1
	ret

PopText:
db "pop "
PrintPop:
	ld h, d
	ld l, e
	ld de, PopText
	jp CommonPushPop


IncText:
db "inc "
PrintInc:
	ld h, d
	ld l, e
	ld de, IncText
PrintDecIncCommon:
	push bc
	ld bc, 5
	call printStr
	pop bc
	ld a, c
	ld b, h
	ld c, l
	sra a ; shift a 3 bits right so register is in a
	sra a
	sra a
	res 3,a ; set remaining bits to 0
	res 4,a ; set remaining bits to 0
	res 5,a ; set remaining bits to 0
	res 6,a
	res 7,a
	call PrintRegister
	call printSpaceUntilEndOfLine
	ld a, 1
	ret

DecText:
db "dec "
PrintDec:
	ld h, d
	ld l, e
	ld de, DecText	
	jp PrintDecIncCommon


PrintInc16:
	ld h, d
	ld l, e
	ld de, IncText
PrintDecIncCommon16:
	push bc
	ld bc, 5
	call printStr
	pop bc
	ld a, c
	ld b, h
	ld c, l
	sra a ; shift a 3 bits right so register is in a
	sra a
	sra a
	sra a
	res 2,a ; set remaining bits to 0
	res 3,a ; set remaining bits to 0
	res 4,a ; set remaining bits to 0
	res 5,a ; set remaining bits to 0
	res 6,a
	res 7,a
	call PrintRegisterPair
	call printSpaceUntilEndOfLine
	ld a, 1
	ret

PrintDec16:
	ld h, d
	ld l, e
	ld de, DecText	
	jp PrintDecIncCommon16

LabelInAddressText:
db "LABEL "
LabelInAddressTextEnd:

JpCondText:
db "jp "
PrintConditionalJump: 
	ld h, d
	ld l, e
	ld de, JpCondText
	push bc
	ld bc, 3
	call printStr
	pop bc
	ld a, c
	sra a ; shift a 3 bits right so condition is in a
	sra a
	sra a
	res 3,a ; set remaining bits to 0
	res 4,a
	res 5,a
	res 6,a
	res 7,a
	call PrintCondition
	printComma
	printSpace
CommonJumpCall:
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; first the third byte of instruction
	inc hl
	ld a, [hl]
	cp a, HIGHER_BYTE_USED_FOR_LABELS
	jp z, CommonJumpPrintLabel
	ld d,a
	printDollar
	ld a,d
	call PrintNumberAsHex
CommonJumpSecondByte:
	ld a, [SourceAdress]
	ld h,a
	ld a, [SourceAdress+1]
	ld l, a
	inc hl ; then second byte of instructions
	ld a, [hl]
	call PrintNumberAsHex
	call printSpaceUntilEndOfLine
	jp CommonJumpCallDone
CommonJumpPrintLabel:
	ld h, b
	ld l, c
	ld de, LabelInAddressText
	ld bc, LabelInAddressTextEnd-LabelInAddressText
	call printStr
	ld b,h
	ld c, l
	jp CommonJumpSecondByte

CommonJumpCallDone:
	ld a, 3
	ret

CallText:
db "call "
PrintUncoditionalCall: 
	ld h, d
	ld l, e
	ld de, CallText
	push bc
	ld bc, 5
	call printStr
	pop bc
	ld b, h
	ld c, l
	jp CommonJumpCall

PrintUncoditionalJump: 
	ld h, d
	ld l, e
	ld de, JpCondText
	push bc
	ld bc, 3
	call printStr
	pop bc
	ld a, c
	ld b, h
	ld c, l
	jp CommonJumpCall

; Parameter:
; hl destination address
; a first 2 bit register to print
; return bc next destination
PrintCondition:
	ld b, h
	ld c, l
	ld hl, PrintConditionTable
PrintTwoCharsFromTable:
	ld d, 0
	sla a; multiply 2
	res 0,a
	ld e, a
	add hl, de
	ld a, [hli] ; read first character
	ld [bc], a ; write frist character
	inc bc
	ld a, [hli] ; read second character
	ld [bc], a ; write second character
	inc bc
	ret

PrintConditionTable:
	db "NZ"
	db "Z "
	db "NC"
	db "C "


; Paramter:
; bc: destination address
; a value to print
; does NOT use hl
PrintNumberAsHex:
	ld d, a
	sra a
	sra a
	sra a
	sra a
	and a,$0F
	cp a,10
	jp nc,biggerThan10
	add a, "0"
	jp printChar1
biggerThan10:
	sub a, 10
	add a, "A"
printChar1:
	ld [bc],a
	inc bc
	ld a,d
	and a, $0F
	cp a,10
	jp nc,biggerThan10_2
	add a, "0"
	jp printChar2
biggerThan10_2:
	sub a, 10
	add a, "A"
printChar2:
	ld [bc],a
	inc bc
	ret


; Parameter:
; hl destination address
; a first 3 bit register to print
; return bc next destination
PrintRegister:
	ld b, h
	ld c, l
	ld hl, PrintRegisterStringsTable
	ld d, 0
	ld e, a
	add hl, de
	ld a, [hl] ; read single character
	ld [bc], a ; write single character
	cp a, "["
	jp nz, PrintRegisterDone
	inc bc
	ld de, PrintRegisterHLRest
	ld h, b
	ld l, c
	ld bc,3
	call printStr
	ld b, h
	ld c, l
	ret
PrintRegisterDone:
	inc bc
	ret


PrintRegisterStringsTable:
	db "B"
	db "C"
	db "D"
	db "E"
	db "H"
	db "L"
	db "[" ; Invalid alone would be 4 bytes [hl]
	db "A"

PrintRegisterHLRest:
db "HL]"
; Parameter:
; hl destination address
; a first 2 bit register to print
; return bc next destination
PrintRegisterPair:
	ld b, h
	ld c, l
	ld hl, PrintRegisterPaieStringsTable
	jp PrintTwoCharsFromTable


PrintRegisterPaieStringsTable:
	db "BC"
	db "DE"
	db "HL"
	db "SP"

PrintRegisterPairPushPop:
	ld b, h
	ld c, l
	ld hl, PrintRegisterPaieStringsTablePushPop
	jp PrintTwoCharsFromTable


PrintRegisterPaieStringsTablePushPop:
	db "bc"
	db "de"
	db "hl"
	db "af"

PrintInstructionTable:

dw	PrintNop ; 0x00
dw PrintLoadd16 ; 0x01
dw PrintLdToBL ; 0x02
dw PrintInc16 ; 0x03
dw PrintInc ; 0x04
dw PrintDec ; 0x05
dw PrintLoadAd8 ; 0x06
dw PrintUnknown ; 0x07
dw PrintUnknown ; 0x08
dw PrintUnknown ; 0x09
dw PrintLdFromBc ; 0x0A
dw PrintDec16 ; 0x0B
dw PrintInc ; 0x0C
dw PrintDec ; 0x0D
dw PrintLoadAd8 ; 0x0E
dw PrintUnknown ; 0x0F
dw PrintUnknown ; 0x10
dw PrintLoadd16 ; 0x11
dw PrintLdToDe ; 0x12
dw PrintInc16 ; 0x13
dw PrintInc ; 0x14
dw PrintDec ; 0x15
dw PrintLoadAd8 ; 0x16
dw PrintUnknown ; 0x17
dw PrintUnknown ; 0x18
dw PrintUnknown ; 0x19
dw PrintLdFromDe ; 0x1A
dw PrintDec16 ; 0x1B
dw PrintInc ; 0x1C
dw PrintDec ; 0x1D
dw PrintLoadAd8 ; 0x1E
dw PrintUnknown ; 0x1F
dw PrintUnknown ; 0x20
dw PrintLoadd16 ; 0x21
dw PrintLdToHlIncrement ; 0x22
dw PrintInc16 ; 0x23
dw PrintInc ; 0x24
dw PrintDec ; 0x25
dw PrintLoadAd8 ; 0x26
dw PrintUnknown ; 0x27
dw PrintUnknown ; 0x28
dw PrintUnknown ; 0x29
dw PrintLdFromHlIncrement ; 0x2A
dw PrintDec16 ; 0x2B
dw PrintInc ; 0x2C
dw PrintDec ; 0x2D
dw PrintLoadAd8 ; 0x2E
dw PrintUnknown ; 0x2F
dw PrintUnknown ; 0x30
dw PrintLoadd16 ; 0x31
dw PrintLdToHlDecrement ; 0x32
dw PrintInc16 ; 0x33
dw PrintInc ; 0x34
dw PrintDec ; 0x35
dw PrintUnknown ; 0x36
dw PrintUnknown ; 0x37
dw PrintUnknown ; 0x38
dw PrintUnknown ; 0x39
dw PrintLdFromHlDecrement ; 0x3A
dw PrintDec16 ; 0x3B
dw PrintInc ; 0x3C
dw PrintDec ; 0x3D
dw PrintLoadAd8 ; 0x3E
dw PrintUnknown ; 0x3F
dw PrintLoadRegisterToRegister ; 0x40
dw PrintLoadRegisterToRegister ; 0x41
dw PrintLoadRegisterToRegister ; 0x42
dw PrintLoadRegisterToRegister ; 0x43
dw PrintLoadRegisterToRegister ; 0x44
dw PrintLoadRegisterToRegister ; 0x45
dw PrintLoadRegisterToRegister ; 0x46
dw PrintLoadRegisterToRegister ; 0x47
dw PrintLoadRegisterToRegister ; 0x48
dw PrintLoadRegisterToRegister ; 0x49
dw PrintLoadRegisterToRegister ; 0x4A
dw PrintLoadRegisterToRegister ; 0x4B
dw PrintLoadRegisterToRegister ; 0x4C
dw PrintLoadRegisterToRegister ; 0x4D
dw PrintLoadRegisterToRegister ; 0x4E
dw PrintLoadRegisterToRegister ; 0x4F
dw PrintLoadRegisterToRegister ; 0x50
dw PrintLoadRegisterToRegister ; 0x51
dw PrintLoadRegisterToRegister ; 0x52
dw PrintLoadRegisterToRegister ; 0x53
dw PrintLoadRegisterToRegister ; 0x54
dw PrintLoadRegisterToRegister ; 0x55
dw PrintLoadRegisterToRegister ; 0x56
dw PrintLoadRegisterToRegister ; 0x57
dw PrintLoadRegisterToRegister ; 0x58
dw PrintLoadRegisterToRegister ; 0x59
dw PrintLoadRegisterToRegister ; 0x5A
dw PrintLoadRegisterToRegister ; 0x5B
dw PrintLoadRegisterToRegister ; 0x5C
dw PrintLoadRegisterToRegister ; 0x5D
dw PrintLoadRegisterToRegister ; 0x5E
dw PrintLoadRegisterToRegister ; 0x5F
dw PrintLoadRegisterToRegister ; 0x60
dw PrintLoadRegisterToRegister ; 0x61
dw PrintLoadRegisterToRegister ; 0x62
dw PrintLoadRegisterToRegister ; 0x63
dw PrintLoadRegisterToRegister ; 0x64
dw PrintLoadRegisterToRegister ; 0x65
dw PrintLoadRegisterToRegister ; 0x66
dw PrintLoadRegisterToRegister ; 0x67
dw PrintLoadRegisterToRegister ; 0x68
dw PrintLoadRegisterToRegister ; 0x69
dw PrintLoadRegisterToRegister ; 0x6A
dw PrintLoadRegisterToRegister ; 0x6B
dw PrintLoadRegisterToRegister ; 0x6C
dw PrintLoadRegisterToRegister ; 0x6D
dw PrintLoadRegisterToRegister ; 0x6E
dw PrintLoadRegisterToRegister ; 0x6F
dw PrintLoadRegisterToRegister ; 0x70
dw PrintLoadRegisterToRegister ; 0x71
dw PrintLoadRegisterToRegister ; 0x72
dw PrintLoadRegisterToRegister ; 0x73
dw PrintLoadRegisterToRegister ; 0x74
dw PrintLoadRegisterToRegister ; 0x75
dw PrintLoadRegisterToRegister ; 0x76
dw PrintLoadRegisterToRegister ; 0x77
dw PrintLoadRegisterToRegister ; 0x78
dw PrintLoadRegisterToRegister ; 0x79
dw PrintLoadRegisterToRegister ; 0x7A
dw PrintLoadRegisterToRegister ; 0x7B
dw PrintLoadRegisterToRegister ; 0x7C
dw PrintLoadRegisterToRegister ; 0x7D
dw PrintLoadRegisterToRegister ; 0x7E
dw PrintLoadRegisterToRegister ; 0x7F
dw PrintAddR8 ; 0x80
dw PrintAddR8 ; 0x81
dw PrintAddR8 ; 0x82
dw PrintAddR8 ; 0x83
dw PrintAddR8 ; 0x84
dw PrintAddR8 ; 0x85
dw PrintAddR8 ; 0x86
dw PrintAddR8 ; 0x87
dw PrintAdcR8 ; 0x88
dw PrintAdcR8 ; 0x89
dw PrintAdcR8 ; 0x8A
dw PrintAdcR8 ; 0x8B
dw PrintAdcR8 ; 0x8C
dw PrintAdcR8 ; 0x8D
dw PrintAdcR8 ; 0x8E
dw PrintAdcR8 ; 0x8F
dw PrintSubR8 ; 0x90
dw PrintSubR8 ; 0x91
dw PrintSubR8 ; 0x92
dw PrintSubR8 ; 0x93
dw PrintSubR8 ; 0x94
dw PrintSubR8 ; 0x95
dw PrintSubR8 ; 0x96
dw PrintSubR8 ; 0x97
dw PrintSbcR8 ; 0x98
dw PrintSbcR8 ; 0x99
dw PrintSbcR8 ; 0x9A
dw PrintSbcR8 ; 0x9B
dw PrintSbcR8 ; 0x9C
dw PrintSbcR8 ; 0x9D
dw PrintSbcR8 ; 0x9E
dw PrintSbcR8 ; 0x9F
dw PrintAndR8 ; 0xA0
dw PrintAndR8 ; 0xA1
dw PrintAndR8 ; 0xA2
dw PrintAndR8 ; 0xA3
dw PrintAndR8 ; 0xA4
dw PrintAndR8 ; 0xA5
dw PrintAndR8 ; 0xA6
dw PrintAndR8 ; 0xA7
dw PrintXorR8 ; 0xA8
dw PrintXorR8 ; 0xA9
dw PrintXorR8 ; 0xAA
dw PrintXorR8 ; 0xAB
dw PrintXorR8 ; 0xAC
dw PrintXorR8 ; 0xAD
dw PrintXorR8 ; 0xAE
dw PrintXorR8 ; 0xAF
dw PrintOrR8 ; 0xB0
dw PrintOrR8 ; 0xB1
dw PrintOrR8 ; 0xB2
dw PrintOrR8 ; 0xB3
dw PrintOrR8 ; 0xB4
dw PrintOrR8 ; 0xB5
dw PrintOrR8 ; 0xB6
dw PrintOrR8 ; 0xB7
dw PrintCPR8 ; 0xB8
dw PrintCPR8 ; 0xB9
dw PrintCPR8 ; 0xBA
dw PrintCPR8 ; 0xBB
dw PrintCPR8 ; 0xBC
dw PrintCPR8 ; 0xBD
dw PrintCPR8 ; 0xBE
dw PrintCPR8 ; 0xBF
dw PrintUnknown ; 0xC0
dw PrintPop ; 0xC1
dw PrintConditionalJump ; 0xC2
dw PrintUncoditionalJump ; 0xC3
dw PrintUnknown ; 0xC4
dw PrintPush ; 0xC5
dw PrintAddD8 ; 0xC6
dw PrintUnknown ; 0xC7
dw PrintUnknown ; 0xC8
dw PrintRet ; 0xC9
dw PrintConditionalJump ; 0xCA
dw PrintCBPrefixed ; 0xCB
dw PrintUnknown ; 0xCC
dw PrintUncoditionalCall ; 0xCD
dw PrintAdcD8 ; 0xCE
dw PrintUnknown ; 0xCF
dw PrintUnknown ; 0xD0
dw PrintPop ; 0xD1
dw PrintConditionalJump ; 0xD2
dw PrintUnknown ; 0xD3
dw PrintUnknown ; 0xD4
dw PrintPush ; 0xD5
dw PrintSubD8 ; 0xD6
dw PrintUnknown ; 0xD7
dw PrintUnknown ; 0xD8
dw PrintUnknown ; 0xD9
dw PrintConditionalJump ; 0xDA
dw PrintUnknown ; 0xDB
dw PrintUnknown ; 0xDC
dw PrintUnknown ; 0xDD
dw PrintSbcD8 ; 0xDE
dw PrintUnknown ; 0xDF
dw PrintUnknown ; 0xE0
dw PrintPop ; 0xE1
dw PrintUnknown ; 0xE2
dw PrintUnknown ; 0xE3
dw PrintUnknown ; 0xE4
dw PrintPush ; 0xE5
dw PrintAndD8 ; 0xE6
dw PrintUnknown ; 0xE7
dw PrintUnknown ; 0xE8
dw PrintUnknown ; 0xE9
dw PrintLoadAtoImmediateMemory ; 0xEA
dw PrintUnknown ; 0xEB
dw PrintUnknown ; 0xEC
dw PrintUnknown ; 0xED
dw PrintXorD8 ; 0xEE
dw PrintUnknown ; 0xEF
dw PrintUnknown ; 0xF0
dw PrintPop ; 0xF1
dw PrintUnknown ; 0xF2
dw PrintUnknown ; 0xF3
dw PrintUnknown ; 0xF4
dw PrintPush ; 0xF5
dw PrintOrD8 ; 0xF6
dw PrintUnknown ; 0xF7
dw PrintUnknown ; 0xF8
dw PrintUnknown ; 0xF9
dw PrintLoadImmediateMemoryToA ; 0xFA
dw PrintUnknown ; 0xFB
dw PrintUnknown ; 0xFC
dw PrintUnknown ; 0xFD
dw PrintCP ; 0xFE
dw PrintUnknown ; 0xFF

PrintInstructionTableEnd:


LengthOfInstructions:
db 1 ; 0x00 
db 3 ; 0x01 
db 1 ; 0x02 
db 1 ; 0x03 
db 1 ; 0x04 
db 1 ; 0x05 
db 2 ; 0x06 
db 1 ; 0x07 
db 3 ; 0x08 
db 1 ; 0x09 
db 1 ; 0x0a 
db 1 ; 0x0b 
db 1 ; 0x0c 
db 1 ; 0x0d 
db 2 ; 0x0e 
db 1 ; 0x0f 
db 1 ; 0x10 
db 3 ; 0x11 
db 1 ; 0x12 
db 1 ; 0x13 
db 1 ; 0x14 
db 1 ; 0x15 
db 2 ; 0x16 
db 1 ; 0x17 
db 2 ; 0x18 
db 1 ; 0x19 
db 1 ; 0x1a 
db 1 ; 0x1b 
db 1 ; 0x1c 
db 1 ; 0x1d 
db 2 ; 0x1e 
db 1 ; 0x1f 
db 2 ; 0x20 
db 3 ; 0x21 
db 1 ; 0x22 
db 1 ; 0x23 
db 1 ; 0x24 
db 1 ; 0x25 
db 2 ; 0x26 
db 1 ; 0x27 
db 2 ; 0x28 
db 1 ; 0x29 
db 1 ; 0x2a 
db 1 ; 0x2b 
db 1 ; 0x2c 
db 1 ; 0x2d 
db 2 ; 0x2e 
db 1 ; 0x2f 
db 2 ; 0x30 
db 3 ; 0x31 
db 1 ; 0x32 
db 1 ; 0x33 
db 1 ; 0x34 
db 1 ; 0x35 
db 2 ; 0x36 
db 1 ; 0x37 
db 2 ; 0x38 
db 1 ; 0x39 
db 1 ; 0x3a 
db 1 ; 0x3b 
db 1 ; 0x3c 
db 1 ; 0x3d 
db 2 ; 0x3e 
db 1 ; 0x3f 
db 1 ; 0x40 
db 1 ; 0x41 
db 1 ; 0x42 
db 1 ; 0x43 
db 1 ; 0x44 
db 1 ; 0x45 
db 1 ; 0x46 
db 1 ; 0x47 
db 1 ; 0x48 
db 1 ; 0x49 
db 1 ; 0x4a 
db 1 ; 0x4b 
db 1 ; 0x4c 
db 1 ; 0x4d 
db 1 ; 0x4e 
db 1 ; 0x4f 
db 1 ; 0x50 
db 1 ; 0x51 
db 1 ; 0x52 
db 1 ; 0x53 
db 1 ; 0x54 
db 1 ; 0x55 
db 1 ; 0x56 
db 1 ; 0x57 
db 1 ; 0x58 
db 1 ; 0x59 
db 1 ; 0x5a 
db 1 ; 0x5b 
db 1 ; 0x5c 
db 1 ; 0x5d 
db 1 ; 0x5e 
db 1 ; 0x5f 
db 1 ; 0x60 
db 1 ; 0x61 
db 1 ; 0x62 
db 1 ; 0x63 
db 1 ; 0x64 
db 1 ; 0x65 
db 1 ; 0x66 
db 1 ; 0x67 
db 1 ; 0x68 
db 1 ; 0x69 
db 1 ; 0x6a 
db 1 ; 0x6b 
db 1 ; 0x6c 
db 1 ; 0x6d 
db 1 ; 0x6e 
db 1 ; 0x6f 
db 1 ; 0x70 
db 1 ; 0x71 
db 1 ; 0x72 
db 1 ; 0x73 
db 1 ; 0x74 
db 1 ; 0x75 
db 1 ; 0x76 
db 1 ; 0x77 
db 1 ; 0x78 
db 1 ; 0x79 
db 1 ; 0x7a 
db 1 ; 0x7b 
db 1 ; 0x7c 
db 1 ; 0x7d 
db 1 ; 0x7e 
db 1 ; 0x7f 
db 1 ; 0x80 
db 1 ; 0x81 
db 1 ; 0x82 
db 1 ; 0x83 
db 1 ; 0x84 
db 1 ; 0x85 
db 1 ; 0x86 
db 1 ; 0x87 
db 1 ; 0x88 
db 1 ; 0x89 
db 1 ; 0x8a 
db 1 ; 0x8b 
db 1 ; 0x8c 
db 1 ; 0x8d 
db 1 ; 0x8e 
db 1 ; 0x8f 
db 1 ; 0x90 
db 1 ; 0x91 
db 1 ; 0x92 
db 1 ; 0x93 
db 1 ; 0x94 
db 1 ; 0x95 
db 1 ; 0x96 
db 1 ; 0x97 
db 1 ; 0x98 
db 1 ; 0x99 
db 1 ; 0x9a 
db 1 ; 0x9b 
db 1 ; 0x9c 
db 1 ; 0x9d 
db 1 ; 0x9e 
db 1 ; 0x9f 
db 1 ; 0xa0 
db 1 ; 0xa1 
db 1 ; 0xa2 
db 1 ; 0xa3 
db 1 ; 0xa4 
db 1 ; 0xa5 
db 1 ; 0xa6 
db 1 ; 0xa7 
db 1 ; 0xa8 
db 1 ; 0xa9 
db 1 ; 0xaa 
db 1 ; 0xab 
db 1 ; 0xac 
db 1 ; 0xad 
db 1 ; 0xae 
db 1 ; 0xaf 
db 1 ; 0xb0 
db 1 ; 0xb1 
db 1 ; 0xb2 
db 1 ; 0xb3 
db 1 ; 0xb4 
db 1 ; 0xb5 
db 1 ; 0xb6 
db 1 ; 0xb7 
db 1 ; 0xb8 
db 1 ; 0xb9 
db 1 ; 0xba 
db 1 ; 0xbb 
db 1 ; 0xbc 
db 1 ; 0xbd 
db 1 ; 0xbe 
db 1 ; 0xbf 
db 1 ; 0xc0 
db 1 ; 0xc1 
db 3 ; 0xc2 
db 3 ; 0xc3 
db 3 ; 0xc4 
db 1 ; 0xc5 
db 2 ; 0xc6 
db 1 ; 0xc7 
db 1 ; 0xc8 
db 1 ; 0xc9 
db 3 ; 0xca 
db 2 ; 0xcb all prefixed instrucitons are size 2
db 3 ; 0xcc 
db 3 ; 0xcd 
db 2 ; 0xce 
db 1 ; 0xcf 
db 1 ; 0xd0 
db 1 ; 0xd1 
db 3 ; 0xd2 
db 1 ; 0xd3 invalid instruction
db 3 ; 0xd4 
db 1 ; 0xd5 
db 2 ; 0xd6 
db 1 ; 0xd7 
db 1 ; 0xd8 
db 1 ; 0xd9 
db 3 ; 0xda 
db 1 ; 0xdb invalid instruction
db 3 ; 0xdc 
db 1 ; 0xdd invalid instruction
db 2 ; 0xde 
db 1 ; 0xdf 
db 2 ; 0xe0 
db 1 ; 0xe1 
db 1 ; 0xe2 
db 1 ; 0xe3 invalid instruction
db 1 ; 0xe4 invalid instruction
db 1 ; 0xe5 
db 2 ; 0xe6 
db 1 ; 0xe7 
db 2 ; 0xe8 
db 1 ; 0xe9 
db 3 ; 0xea 
db 1 ; 0xeb invalid instruction
db 1 ; 0xec invalid instruction
db 1 ; 0xed invalid instruction
db 2 ; 0xee 
db 1 ; 0xef 
db 2 ; 0xf0 
db 1 ; 0xf1 
db 1 ; 0xf2 
db 1 ; 0xf3 
db 1 ; 0xf4 invalid instruction
db 1 ; 0xf5 
db 2 ; 0xf6 
db 1 ; 0xf7 
db 2 ; 0xf8 
db 1 ; 0xf9 
db 3 ; 0xfa 
db 1 ; 0xfb 
db 1 ; 0xfc invalid instruction
db 1 ; 0xfd invalid instruction
db 2 ; 0xfe 
db 1 ; 0xff 



; @param de: source string in ASCII
; @param hl: desitnation tilemap
; @param bc: Length
; return hl after destination address
printStr:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, printStr
	ret

LabelText:
db "-- LABEL "
LabelTextEnd:

SECTION "Tile data", ROM0

SelectorTile:
    dw `00000000
    dw `00003000
    dw `00000300
    dw `03333330
    dw `03333330
    dw `00000300
    dw `00003000
    dw `00000000
SelectorTileEnd:


INCLUDE "font.asm"	


; Code Section for the code the be executed
SECTION "CdsSection", WRAM0
counter:
ds 1
bitmap:
ds 1
SourceAdress:
ds 2
FoundLabel:
ds 1
BatterySaveStart:

; Every label consists of two bytes so (CdsLabelsEnd-CdsLabels) / 2 different label positions .
; The first label is refered to as 0 with HIGHER_BYTE_USED_FOR_LABELS = $E0 in the higher byte
; So $E000 will be replaced with the address at CdsLabels[0] and CdsLabels[1]
CdsLabels:
ds 100
CdsLabelsEnd:

; Cds contains the code. before we can jump directly to Cds the labels needs to be replaced with real addresses
Cds:
ds $F90
CdsEnd: ; Must be last in RAM as $D0 = HIGHER_BYTE_ADDRESS_CODE_END in higher byte is used to detect end
BatterySaveEnd: