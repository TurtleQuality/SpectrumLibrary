; library routines, should be useful for other programs in the future


; start with a function and a macro




profile = true                                          ;

                        [[                              ;
                        function dix(Addr)              ;
                        {
                        return Addr-IX_Base             ; Set IX to IX_Base, then you can use LD A,(IX + dix(label)) to point to "label"
                        }
                        ]]                              ;


                        rpt macro(count,object)         ; We're given two parameters

                        loop count                      ; Assume the count is an integer and loop that many times
                          object                        ; Just 'do' whatever we're given each time
                          lend                          ; Loop end

                        mend                            ;


                      org Bloxstop                      ;

                      malign 256                        ;  start on 256 byte border
                      btable: defb 1,2,4,8,16,32,64,128 ;
                      maligned                          ;

; =================================================================================================================================================================
; Library code !
; Bitcmd lets you set, reset or return bit D of address HL, E should be set to command type
; Blats the standard regs, EXX before usage maybe ?
; code below can be replaced with

;       ld h, high btable       and

;       and (hl)        for Bit command
;       or (hl)         for Set
;       xor (hl)        for reset

; bit would be loaded in L, byte to be tested/altered in A . Very efficient BUT only issue is that normally the target byte is (HL) in my code
; Since we often have E for direction / bit, we could LD A, (DE), X/OR (HL),  LD (HL),A         which is 7 + 7 + 7 t states, and the bit command doesn't need the last 7
; 2 main locations are line_chk and prefill blocks

bitcmd_setup          ld hl,513                         ; hl =address
                      ld de,17927                       ; e is bit and d is cmd, bit=70, res =134 and set=198
                      ld bc,0                           ;



bitcmd_start:         ld a,e                            ;
                      rlca                              ;
                      rlca                              ;
                      rlca                              ;
                      add a,d                           ;
                      ld (bitcmd+1),a                   ;

bitcmd:               bit 7,(hl)                        ;


                      ret z                             ;
                      inc bc                            ;
                      ret                               ;


; =================================================================================================================================================================
; Library code !

; MY_PRINT               Prints a string (mostly using RST16) from DE to zero terminator with following extra special characters
;       0       terminate string
;       7       repeat character, to be followed by number of repeats then the character to repeat
;       8       cursor left
;       9       cursor right
;       10      cursor up
;       11      cursor down
;       12      repeat string, followed by number of repeats, then string up to 0 terminator, don't forget you need a terminator for each 12
;       13      line return - modified to allow return to continue to lower screen area

; All normal registers well and truly blatted



ROM_CLS               EQU 0x0D6B                        ; Clears the screen and opens channel 2       CL_ALL is the correct name
ROM_OPEN_CHANNEL      EQU 0x1601                        ; Open a channel                              CHAN_OPEN


malign 256,false                                        ;
MY_PR_JTBL1           defb low MY_CRS_CALL              ; 8     08
                      defb low MY_CRS_CALL              ; 9     09
                      defb low MY_CRS_CALL              ; 10    0A
                      defb low MY_CRS_CALL              ; 11    0B
                      defb low MY_P_SREP                ; 12    0C
                      defb low MY_CRS_CALL              ; 13   0D
                      defb low MY_P_RST                 ; 14    0E
                      defb low MY_P_RST                 ; 15    0F
                      defb low MY_P_2RST                ; 16    10
                      defb low MY_P_2RST                ; 17    11
                      defb low MY_P_2RST                ; 18    12
                      defb low MY_P_2RST                ; 19    13
                      defb low MY_P_2RST                ; 20    14
                      defb low MY_P_2RST                ; 21    15
                      defb low MY_P_3RST                ; 22    16
                      defb low MY_P_2RST                ; 23    17
maligned

MY_PRINT:             LD A, (DE)                        ; Get the character
                      ; halt                            ; for testing, so the screen refreshes
                      OR A                              ; CP with 0
                      RET Z                             ; Ret if it is zero, should really be char 226

                      CP 24                             ;
                      JR NC,MY_P_RST                    ;   Chars > 23 should be printed normally

                      CP 7                              ;     If it's not between 7 and 13, just do the rst

                      JR Z,MY_P_CREP                    ;     If it's 7, jump to repeat
                      JR C,MY_P_RST                     ;     If A<7, just print

; now we can use a jump table for characters 7 through to 23

                      ADD A, (LOW MY_PR_JTBL1) - 8      ;   Start of the table, minus starting value of A
                      LD H, HIGH MY_PR_JTBL1            ;
                      LD L,A                            ;
                      SUB A, (LOW MY_PR_JTBL1) - 8      ;    Return A to character value
                      LD L,(HL)                         ;
                      LD H, high MY_P_RST               ;
                      PUSH HL                           ;
                      RET                               ;

malign 256,false                                        ;

MY_CRS_CALL           CALL CRS_HANDLER                  ;   Now we know it's a cursor move
                      INC DE                            ;
                      JP MY_PRINT                       ;    Loop to next character


                      ;


MY_P_3RST             RST 0x10                          ; print the character AT + both co-ordinates
                      INC DE                            ;
                      LD A,(DE)                         ;
MY_P_2RST             RST 0x10                          ; print single paramter control characters (INK,PAPER,OVER etc )
                      INC DE                            ;
                      LD A,(DE)                         ;
MY_P_RST              RST 0x10                          ; print the character
PR_INC_DE             INC DE                            ; Inc to the next character in the string
                      JP MY_PRINT                       ; Loop


MY_P_SREP             INC DE                            ;
maligned
                      LD A,(DE)                         ;
                      LD B,A                            ; If the Print text includes 6,x,string..., 0       then string... will be repeated x times
                      INC DE                            ;
MY_P_SREP_LOOP        PUSH DE                           ;
                      PUSH BC                           ;
                      CALL MY_PRINT                     ;
                      POP BC                            ;
                      DJNZ MY_P_SREP_LOOP2              ;
                      POP AF                            ; Continue from the current DE value not the saved one
                      INC DE                            ;
                      JR MY_PRINT                       ;
MY_P_SREP_LOOP2       POP DE                            ;
                      JR MY_P_SREP_LOOP                 ;

PRINT_REP_LOOP        RST 0x10                          ;
                      LD A,C                            ;  RESTORE A
                      DJNZ PRINT_REP_LOOP               ;  Loop B times
                      INC DE                            ;
                      JR MY_PRINT                       ;

MY_P_CREP             INC DE                            ;
                      LD A,(DE)                         ; next character is number of repeats
                      LD B,A                            ;
                      INC DE                            ;
                      LD A,(DE)                         ; character to be printed
                      LD C,A                            ;

                      CP 14                             ;          Checking here if it's a cursor control
                      JR NC, PRINT_REP_LOOP             ;
                      CP 8                              ;
                      JR C, PRINT_REP_LOOP              ;

CRS_LOOP              CALL CRS_HANDLER                  ;
                      LD A,C                            ;  RESTORE A
                      DJNZ CRS_LOOP                     ;  MOVE B TIMES
                      INC DE                            ;
                      JR MY_PRINT                       ;


CRS_HANDLER           LD HL,(DF_CC)                     ;     System variable for address of next character
                      LD IX,S_POSN                      ;    System variable for co-ords of next character

                      CP 9                              ;
                      JR C,CRS_LEFT                     ;
                      JR Z,CRS_RIGHT                    ;
                      CP 11                             ;
                      JR Z,CRS_DOWN                     ;
                      JP C, CRS_UP                      ;
                      JR CRS_RETURN                     ;


                      ;

CRS_RETURN            LD A, L                           ;    Like cursor down, but if the down is valid we must also set X to left side of screen
                      ADD A, 32                         ;
                      LD L, A                           ;
                      JR NC,CRS_RET_2                   ;
                      LD A, H                           ;
                      CP 80                             ;    If H >= 80, then adding 8 will take us outside past the pixels and into the attributes
                      RET NC                            ;
                      ADD A, 8                          ;
                      LD H, A                           ;
CRS_RET_2             DEC (IX+1)                        ;
                      LD A,L                            ;
                      AND 224                           ;
                      LD L,A                            ;
                      LD (IX),33                        ;
                      JR CRS_SAV_RET                    ;

CRS_LEFT              LD A,L                            ;
                      AND 31                            ;
                      RET Z                             ;  If L AND 31 is zero, we're already in column zero so ignore the move left
                      DEC L                             ;
                      INC (IX)                          ;
                      JR CRS_SAV_RET                    ;

CRS_RIGHT             LD A,L                            ;
                      AND 31                            ;
                      CP 31                             ;   If L AND 31 is 31, we're already in column 31 and can't move right
                      RET Z                             ;
                      INC L                             ;
                      DEC (IX)                          ;
                      JR CRS_SAV_RET                    ;


CRS_DOWN              LD A, L                           ;
                      ADD A, 32                         ;
                      LD L, A                           ;
                      JR NC,CRS_D_EXIT                  ;    No need to alter high byte
                      LD A, H                           ;
                      CP 80                             ;    If H >= 80, then adding 8 will take us outside past the pixels and into the attributes
                      RET NC                            ;
                      ADD A, 8                          ;
                      LD H, A                           ;
CRS_D_EXIT            DEC (IX+1)                        ;
                      JR CRS_SAV_RET                    ;


CRS_UP                LD A, L                           ;
                      SUB A, 32                         ;
                      LD L, A                           ;
                      JR NC,CRS_U_EXIT                  ;   No need to alter high byte
                      LD A, H                           ;
                      CP 72                             ;     If H <72 , then after subbing 8 we'll be trying to print to the ROM  .... not so good
                      RET C                             ;
                      SUB A, 8                          ;
                      LD H, A                           ;
CRS_U_EXIT            INC (IX+1)                        ;

CRS_SAV_RET           LD (DF_CC),HL                     ;    Save the updated position
                      RET                               ;






; =================================================================================================================================================================
; Library code !
; Drawball draws the following to screen DE = source, HL = screen address, C = width in bytes, B = height in bytes;
; Blats yer normal regs

; GrabBall should use an extra byte of width for dropping block, to allow for L/R
; DrawBall could have a byte to increase dest to skip that byte if not needed, or nop if needed.


profile = true                                          ;

DrawBinc              PUSH BC                           ;
                      PUSH DE                           ;
                      XOR A                             ;
                      LD B,A                            ;
                      LDIR                              ; copy from HL to DE
                      INC HL                            ; when we are not scrolling or blanking
                      POP DE                            ;
                      POP BC                            ;
                      CALL One_DE_Pixel_Down            ; go to next line of pixels
                      DJNZ DrawBinc                     ;
                      RET                               ;


DrawBall              PUSH BC                           ;
                      PUSH DE                           ;
                      XOR A                             ;
                      LD B,A                            ;
                      LDIR                              ; copy from HL to DE
                      POP DE                            ;
                      POP BC                            ;
                      CALL One_DE_Pixel_Down            ; go to next line of pixels , note A is blatted
                      DJNZ DrawBall                     ;
                      RET                               ;
                      ;

; =================================================================================================================================================================
; Library code !
; DrawXor does the same as Drawball but with XOR
; Blats yer normal regs

                      ; DE = source, HL = screen address, C = width in bytes, B = height in bytes

DrawXor               PUSH BC                           ;
                      PUSH HL                           ;
xorloop:              LD A,(DE)                         ;
                      XOR (HL)                          ;
                      LD (HL),A                         ;
                      INC HL                            ;
                      INC DE                            ;
                      DEC C                             ;
                      jr nz,xorloop                     ;
Xor_Ball_Inc          INC DE                            ; optional increment,  NOP as applicable
                      POP HL                            ;
                      POP BC                            ;

                      CALL OnePixelLineDown             ; go to next line of pixels with HL
                      DJNZ DrawXor                      ;
                      RET                               ;

; =================================================================================================================================================================
; Library code !
; GrabBall copies from HL screen address to DE in memory, where B is height and C is width (helps to have width +1 if scrolling needed)
; Blats yer normal regs

GrabBall:             PUSH BC                           ;
                      LD B,0                            ;   so ldir is only executed C times
                      PUSH HL                           ;
                      LDIR                              ;
                      POP HL                            ;
                      POP BC                            ;

                      CALL OnePixelLineDown             ; go to next line of pixels with HL
                      DJNZ GrabBall                     ;
                      RET                               ;


; =================================================================================================================================================================
; Library code !
; OnePixelLineDown moves HL to the next screen row down
; Blats A

OnePixelLineDown      INC H                             ; go to new line
                      LD A,H                            ;
                      AND 00000111B                     ;
                      RET NZ                            ; new line belongs to the same character line

                      LD A,L                            ; going to new character line
                      ADD A,32                          ;
                      LD L,A                            ;
                      RET C                             ; going to new screen segment, if e.g h was 71, now it is 72

                      LD A,H                            ;
                      SUB 8                             ;
                      LD H,A                            ; decrease d if we are in the same screen segment
                      RET                               ;

; =================================================================================================================================================================
; Library code !
; OnePixelLineUp moves HL to the next screen row up
; Blats A

OnePixelLineUp        DEC H                             ; Go up onto the next pixel line
                      LD A,H                            ; Check if we have gone onto the next character boundary
                      AND 7                             ;
                      CP 7                              ;
                      RET NZ                            ;
                      LD A,L                            ;
                      SUB 32                            ;
                      LD L,A                            ;
                      RET C                             ;
                      LD A,H                            ;
                      ADD A,8                           ;
                      LD H,A                            ;
                      RET                               ;


; same same but DE

OnePixelLineUpDE      DEC D                             ; Go up onto the next pixel line
                      LD A,D                            ; Check if we have gone onto the next character boundary
                      AND 7                             ;
                      CP 7                              ;
                      RET NZ                            ;
                      LD A,E                            ;
                      SUB 32                            ;
                      LD E,A                            ;
                      RET C                             ;
                      LD A,D                            ;
                      ADD A,8                           ;
                      LD D,A                            ;
                      RET                               ;

; =================================================================================================================================================================
; Library code !
                      ; DE version of One_Pixel_Down, also since DEC DE often needed with it, included in the sub
;Blats A



One_DEC_DE_Pixel_Down DEC E                             ;
One_DE_Pixel_Down     INC D                             ; go to new line
                      LD A,D                            ;
                      AND 00000111B                     ;
                      RET NZ                            ; new line belongs to the same character line

                      LD A,E                            ; going to new character line
                      ADD A,32                          ;
                      LD E,A                            ;
                      RET C                             ; going to new screen segment, if e.g d was 71, now it is 72

                      LD A,D                            ;
                      SUB 8                             ;
                      LD D,A                            ; decrease d if we are in the same screen segment
                      RET                               ;

Six_DE_Pixel_Down     LD A,D                            ;
                      AND 00000111B                     ;
                      CP 2                              ;
                      LD A,6                            ;
                      JR NC, SIX_P_C                    ; will need a new line
                      ADD A,D                           ;
                      LD D,A                            ;
                      RET                               ;

SIX_P_C               ADD A,D                           ;
                      LD D,A                            ;

                      LD A,E                            ; going to new character line
                      ADD A,32                          ;
                      LD E,A                            ;
                      RET C                             ; going to new screen segment, if e.g d was 71, now it is 72

                      LD A,D                            ;
                      SUB 8                             ;
                      LD D,A                            ; decrease d if we are in the same screen segment
                      RET                               ;

;  Moves DE screen address down B pixels assuming B is between 0 and 8

DE_B_Pixels_Down      LD A,D
                      AND 00000111B
                      ADD B
                      BIT 3,A
                      JR NZ, DE_B_DOWN_C
                      LD A,B
                      ADD D
                      LD D,A
                      RET

DE_B_DOWN_C           LD A,B
                      ADD A,D                           ;
                      LD D,A                            ;

                      LD A,E                            ; going to new character line
                      ADD A,32                          ;
                      LD E,A                            ;
                      RET C                             ; going to new screen segment, if e.g d was 71, now it is 72

                      LD A,D                            ;
                      SUB 8                             ;
                      LD D,A                            ; decrease d if we are in the same screen segment
                      RET                               ;



; =================================================================================================================================================================
; Library code !
; fill_blk clears from HL,fill BC-1 bytes with zero
; Sets BC to zero, HL and DE to end of block

fill_blk:             ld d,h                            ;
                      ld e,l                            ;
                      inc de                            ;
                      ld (hl),0                         ;
                      ldir                              ;
                      ret                               ;

; =================================================================================================================================================================

; Library code !
; Calculates screen address
; B = Y pixel position
; C = X pixel position
; Returns address in HL
; Blats A

Get_Pixel_Addr:       LD A,B                            ; Calculate Y2,Y1,Y0
                      AND %00000111                     ; Mask out unwanted bits
                      OR %01000000                      ; Set base address of screen
                      LD H,A                            ; Store in H
                      LD A,B                            ; Calculate Y7,Y6
                      RRA                               ; Shift to position             ;
                      RRA                               ;
                      RRA                               ;
                      AND %00011000                     ; Mask out unwanted bits
                      OR H                              ; OR with Y2,Y1,Y0
                      LD H,A                            ; Store in H
                      LD A,B                            ; Calculate Y5,Y4,Y3
                      RLA                               ; Shift to position             ;
                      RLA                               ;
                      AND %11100000                     ; Mask out unwanted bits
                      LD L,A                            ; Store in L
                      LD A,C                            ; Calculate X4,X3,X2,X1,X0
                      RRA                               ; Shift into position           ;
                      RRA                               ;
                      RRA                               ;
                      AND %00011111                     ; Mask out unwanted bits
                      OR L                              ; OR with Y5,Y4,Y3
                      LD L,A                            ; Store in L

                      RET                               ;

; =================================================================================================================================================================

; Library code !
; Updates HL to next row down
; Returns address in HL
; Blats A

Char_Address_Down:    LD A, L                           ;
                      ADD A, 32                         ;
                      LD L, A                           ;
                      RET NC                            ;
                      LD A, H                           ;
                      ADD A, 8                          ;
                      LD H, A                           ;
                      RET                               ;

; =================================================================================================================================================================
; Library code !
; get_print_addr Calculates print screen address
; B = Y pixel position
; C = X CHARACTER position
; Returns address in HL




get_print_addr:       ld a,b                            ; vertical position.
                      and 24                            ; which segment, 0, 1 or 2?
                      add a,64                          ; 64*256 = 16384, Spectrum's screen memory.
                      ld h,a                            ; this is our high byte.
                      ld a,b                            ; what was that vertical position again?
                      and 7                             ; which row within segment?
                      rrca                              ; multiply row by 32.           ;
                      rrca                              ;
                      rrca                              ;
                      ld l,a                            ; low byte.
                      ld a,c                            ; add on y coordinate.
                      add a,l                           ; mix with low byte.
                      ld l,a                            ; address of screen position in de.
                      ret                               ;


; =================================================================================================================================================================

; get_byte_addr
; d = Y pixel position
; e = X CHARACTER position
; Returns address in HL

get_byte_addr:        ld a,d                            ; fetch vertical coordinate.


; Find line within cell.

                      and 7                             ; line 0-7 within character square.
                      add a,64                          ; 64 * 256 = 16384 = start of screen display.
                      ld h,a                            ; line * 256.

; Find which third of the screen we're in.

                      ld a,d                            ; restore the vertical.
                      and 192                           ; segment 0, 1 or 2 multiplied by 64.
                      rrca                              ; divide this by 8.
                      rrca                              ;
                      rrca                              ; segment 0-2 multiplied by 8.
                      add a,h                           ; add to d give segment start address.
                      ld h,a                            ;

; Find character cell within segment.

                      ld a,d                            ; 8 character squares per segment.
                      rlca                              ; divide x by 8 and multiply by 32,
                      rlca                              ; net calculation: multiply by 4.
                      and 224                           ; mask off bits we don't want.
                      add a,e                           ; add the horizontal (no need to div 8, insert AND 32 if taking e from screen address).
                      ld l,a                            ;
                      ret                               ;

; =================================================================================================================================================================
; Library code !
;print_block_outline - assuming this is printed to a character boundary
; on entry DE = address to start printing from, H = height pixels and L = length (width really in bytes)
;  HL kept intact, B is zeroed,


; top row
print_block_outline   ld b,l                            ;
                      ld a,255                          ;
                      push de                           ;

Outline_Toprow_Loop:  ld (de),a                         ;
                      inc e                             ;
                      djnz Outline_Toprow_Loop          ;

                      pop de                            ;
                      ex de,hl                          ;  HL has screen address and DE block dimensions

                      ld b,d                            ; b is now height
                      dec b                             ; deduct top and bottom rows
                      dec b                             ;
                      ld d,0                            ; de is width in bytes
                      dec e                             ;
                      call OnePixelLineDown             ;

middle_outline_loop:  set 7,(hl)                        ;
                      add hl,de                         ;
                      set 0,(hl)                        ;
                      sbc hl,de                         ;


                      call OnePixelLineDown             ;

                      djnz middle_outline_loop          ;



                      ex de,hl                          ;
                      ld a,255                          ;
                      ld b,l                            ;
                      inc b                             ;
outline_bottomrow_loop ld (de),a                        ;
                      inc e                             ;
                      djnz outline_bottomrow_loop       ;

                      ret                               ;

; =================================================================================================================================================================
; Library code !
; Title:        ZX Spectrum Keyboard Routines
; Author:       Dean Belfield
; Created:      29/07/2011
; Last Updated: 29/07/2011
;
; Requires:
;
; Modinfo:
;

; Read the in-game controls
; HL: The control map
; Returns:
;  A: Input flags - 000UDLRF (Up, Down, Left, Right, Fire)
; Zero flag set if no key pressed


;
Read_Controls:        LD HL,Input_Custom                ;
                      LD D, 5                           ; Number of controls to check
                      LD E, 0                           ; The output flags
                      LD C,0xFE                         ; Low is always 0xFE for reading keyboard

Read_Controls1:       LD B,(HL)                         ; Get the keyboard port address
                      INC HL                            ;
                      IN A,(C)                          ; Read the rows in
                      AND (HL)                          ; And with the mask
                      JR NZ, Read_Controls2             ; Skip if not pressed (bit is 0)
                      SCF                               ; Clear C flag

Read_Controls2:       RL E                              ; Rotate the carry flag into E
                      INC HL                            ;
                      DEC D                             ;
                      JR NZ, Read_Controls1             ; Loop
                      LD A,E                            ; Fetch the key flags
                      AND A                             ; Check for 0
                      RET                               ;


; As Read_Keyboard, but with debounce
;
Read_Keyboard_Debounce:CALL Read_Keyboard               ; A debounced versiion - Read the keyboard
                      AND A                             ; Quick way to do CP 0
                      JR NZ, Read_Keyboard_Debounce     ; Loop until key released

KB_DB:                CALL Read_Keyboard                ; And second loop reading the keyboard
                      AND A                             ; CP 0
                      JR Z,KB_DB                        ; Loop until key is pressed
                      RET                               ;

; Read the keyboard and return an ASCII character code
; Returns:
;  A: The character code, or 0 if no key pressed
; BC: The keyboard port (0x7FFE to 0xFEFE)
;
Read_Keyboard:        LD HL,Keyboard_Map                ; Point HL at the keyboard list
                      LD D,8                            ; This is the number of ports (rows) to check
                      LD C,0xFE                         ; Low is always 0xFE for reading keyboard ports

Read_Keyboard_0:      LD B,(HL)                         ; Get the keyboard port address
                      INC HL                            ; Increment to keyboard list of table
                      IN A,(C)                          ; Read the row of keys in
                      AND 0x1F                          ; We are only interested in the first five bits
                      LD E,5                            ; This is the number of keys in the row

Read_Keyboard_1:      RL A                              ; Shift A right; bit 0 sets carry bit
                      JR NC,Read_Keyboard_2             ; If the bit is 0, we've found our key
                      INC HL                            ; Go to next table address
                      DEC E                             ; Decrement key loop counter
                      JR NZ,Read_Keyboard_1             ; Loop around until this row finished
                      DEC D                             ; Decrement row loop counter
                      JR NZ,Read_Keyboard_0             ; Loop around until we are done
                      AND A                             ; Clear A (no key found)
                      RET                               ;

Read_Keyboard_2:      LD A,(HL)                         ; We've found a key at this point; fetch the character code!
                      RET                               ;

;========================================================================00
; BCD stuff copied from https://www.chibiakumas.com/z80/advanced.php
; not sure what the first one is doing...

BCD_Get_End:           push bc                          ;

                      ld b,0                            ;
                      ld c,b                            ;
                      dec C                             ;
                      add hl,bc                         ;
                      ex de,hl                          ;
                      add hl,bc                         ;
                      ex de,hl                          ;
                      pop bc                            ;
                      ret                               ;


BCD_Print:            call BCD_Get_End                   ;

BCD_Show_Direct:      ld a,(de)                         ;
                      and %11110000                     ;
                      rrca                              ;
                      rrca                              ;
                      rrca                              ;
                      rrca                              ;
                      add '0'                        ;
                      rst 16                   ;
                      ld a,(de)                        ;
                      dec de                            ;
                      and %00001111                     ;
                      add '0'                           ;
                      rst 16                   ;
                      djnz BCD_Show_Direct              ;

BCD_Add:              or a                              ;
BCD_Add_Again:        ld a,(de)                         ;
                      adc (hl)                          ;
                      daa                               ;
                      ld (de),a                         ;
                      inc de                            ;
                      inc hl                            ;
                      djnz BCD_Add_Again:               ;
                      ret                               ;

BCD_Sub:              or a                              ;
BCD_Sub_Again:        ld a,(de)                         ;
                      sbc (hl)                          ;
                      daa                               ;
                      ld (de),a                         ;
                      inc de                            ;
                      inc hl                            ;
                      djnz BCD_Sub_Again:               ;
                      ret                               ;


BCD_CP                call BCD_Get_End                  ;

BCD_CP_Direct         ld a,(de)                         ;
                      CP (HL)                           ;
                      ret c                             ;
                      ret nz                            ;
                      dec de                            ;
                      dec hl                            ;
                      djnz BCD_CP_Direct                ;

                      or a                              ; Clear Carry

                      ret                               ;




; =================================================================================================================================================================
; Library code !
; returns 1 byte random number in A, disturbs HL and DE

rnd                   ld hl,(rnd_hl)                    ; yw -> zt
                      ld de,(rnd_de)                    ; xz -> yw
                      ld (rnd_de),hl                    ; x = y, z = w
                      ld a,l                            ; w = w ^ ( w << 3 )
                      add a,a                           ;
                      add a,a                           ;
                      add a,a                           ;
                      xor l                             ;
                      ld l,a                            ;
                      ld a,d                            ; t = x ^ (x << 1)
                      add a,a                           ;
                      xor d                             ;
                      ld h,a                            ;
                      rra                               ; t = t ^ (t >> 1) ^ w
                      xor h                             ;
                      xor l                             ;
                      ld h,e                            ; y = z
                      ld l,a                            ; w = t
                      ld (rnd_hl),hl                    ;
                      ret                               ;
rnd_hl                defw zeusrand                     ;
rnd_de                defw zeusrand                     ;

mir_rnd               call rnd                          ;
                      ; ld a,l
                      inc a                             ;
                      and 00001111B                     ;
                      ld h,a                            ;
                      sla a                             ;
                      sla a                             ;
                      sla a                             ;
                      sla a                             ;
                      or h                              ;
                      ret                               ;

; =================================================================================================================================================================
;library code
;pixel_to_attr
; converts pixel address in HL to attr address

pixel_to_attr         ld a,h                            ;
                      or #87                            ;
                      rra                               ;
                      rra                               ;
                      srl a                             ;
                      ld h,a                            ;
                      ret                               ;
; =================================================================================================================================================================

;attr_to_pixel
; converts attr addess in HL to pixel address

attr_to_pixel         ld a,h                            ;
                      add a                             ;
                      add a                             ;
                      add a                             ;
                      and h                             ;
                      ld h,a                            ;
                      ret                               ;
; =================================================================================================================================================================

;Pixel Scroll a window right

;b = width  bytes
;c = height pixels
;hl = leftmost address


scroll_r:             ld d,b                            ; taking a copy

scroll_r_loop1:       ld e,l                            ;
                      and a                             ;
scroll_r_loop2:       rr (hl)                           ;
                      inc l                             ;
                      djnz scroll_r_loop2               ;

                      dec c                             ;
                      ret z                             ;
                      ld l,e                            ;
                      call OnePixelLineDown             ;
                      ld b,d                            ;
                      jr scroll_r_loop1                 ;

; =================================================================================================================================================================

;Pixel scroll a window left

;b = width
;c = height
;hl = rightmost address


scroll_l              ld d,b                            ; taking a copy


scroll_l_loop1        ld e,l                            ;   take copy of start column
                      and a                             ;
scroll_l_loop2:       rl (hl)                           ;
                      dec hl                            ;
                      djnz scroll_l_loop2               ;

                      dec c                             ;    row count
                      ret z                             ;    quit when done
                      ld l,e                            ;    restore hl then
                      call OnePixelLineDown             ;    next row
                      ld b,d                            ;    restore length in bytes
                      jr scroll_l_loop1                 ;


; =================================================================================================================================================================

;Half byte Scroll a window right

;b = width  bytes
;c = height pixels
;hl = leftmost address


scroll_hr:            ld d,b                            ; taking a copy


scroll_hr_loop1:      ld e,l                            ;
                      xor a                             ;
scroll_hr_loop2:      rrd                               ;
                      inc l                             ;
                      djnz scroll_hr_loop2              ;

                      dec c                             ;
                      ret z                             ;
                      ld l,e                            ;
                      call OnePixelLineDown             ;
                      ld b,d                            ;
                      jr scroll_hr_loop1                ;

; =================================================================================================================================================================

;Half byte scroll a screen window left

;b = width
;c = height
;hl = rightmost address


scroll_hl             ld d,b                            ; taking a copy


scroll_hl_loop1       ld e,l                            ;   take copy of start column
                      xor a                             ; clear a so we're zeroing the right side of the range
scroll_hl_loop2:      rld                               ;
                      dec hl                            ;
                      djnz scroll_hl_loop2              ;

                      dec c                             ;    row count
                      ret z                             ;    quit when done
                      ld l,e                            ;    restore hl then
                      call OnePixelLineDown             ;    next row
                      ld b,d                            ;    restore length in bytes
                      jr scroll_hl_loop1                ;

; =================================================================================================================================================================

;Half byte scroll a mem block (under 256 bytes) right

;b=size   hl = start point
                      half_shift_blk: xor a             ;
half_blk_loop:        rrd                               ;
                      inc hl                            ;
                      djnz half_blk_loop                ;
                      ret                               ;


; =================================================================================================================================================================
; scroll a window down

; b = width bytes
; c = height pixels
; h = y pos - pixels
; l = x pos - bytes
                      ; Grab / Draw ball parms DE = storage, HL = screen address, C = width in bytes, B = height in bytes
test_scr              halt                              ;
                      ld hl,0x4027                      ;
                      ld de,41000                       ;
                      ld b,2                            ;
                      ld c,32                           ;
                      call GrabBall                     ;
                      halt                              ;
                      ld de,41000                       ;
                      ld b,2                            ;
                      ld c,32                           ;
                      ld hl,0x4067                      ;
                      call DrawBall                     ;
                      halt                              ;

scroll_down           ld e,l                            ;
                      ld a,c                            ;
                      add h                             ;
                      ld d,a                            ;
                      call get_byte_addr                ;      HL becomes bottom left screen address
                      ld d,h                            ;
                      ld e,l                            ;

sdloop1:              call OnePixelLineUp               ;      HL is next row up from DE
                      push hl                           ;
                      ld a,b                            ;
sdloop2:              ldi                               ;      copy from HL to DE
                      djnz sdloop2                      ;
                      ld b,a                            ;
                      dec c                             ;
                      pop de                            ;      DE moves one row up now
                      ret z                             ;
                      ld h,d                            ;
                      ld l,e                            ;
                      call sdloop1                      ;



;IM2_Table:              EQU 0xFE00                              ; 256 byte page (+ 1 byte) for IM2
;IM2_JP:                 EQU 0xFDFD                              ; 3 bytes for JP routine under IM2 table

Initialise_Interrupt: DI                                ;
                      LD DE, IM2_Table                  ; The IM2 vector table (on page boundary)
                      LD HL, IM2_JP                     ; Pointer for 3-byte interrupt handler
                      LD A, D                           ; Interrupt table page high address
                      LD I, A                           ; Set the interrupt register to that page


                      IM 2                              ; Set the interrupt mode
                      EI                                ; Enable interrupts
                      RET                               ;

library_end:

