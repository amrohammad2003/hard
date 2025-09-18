LIST    P=16F877A          ; Specify the microcontroller model (PIC16F877A)
    INCLUDE "P16F877A.INC"     ; Include the header file containing register definitions

    ; Configure the microcontroller settings:
    ; _CP_OFF   -> Code protection disabled
    ; _WDT_OFF  -> Watchdog Timer disabled
    ; _PWRTE_ON -> Power-up Timer enabled
    ; _XT_OSC   -> External crystal oscillator mode
    __CONFIG _CP_OFF & _WDT_OFF & _PWRTE_ON & _XT_OSC  

    ORG     0x0000             ; Set the origin at address 0 (Reset Vector)
    GOTO    MAIN               ; Jump to the main program execution

    ORG     0x0004             ; Interrupt vector location (not used in this program)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variable definitions
TimeoutCounter   EQU 0x40  ; Define a memory location for the timeout counter
Index_REG    EQU 0x2A    ; General-purpose register for player input pointer
STATUS_REG EQU 0x20            ; Define a register for the Status variable
TEMP_REG   EQU 0x21            ; Temporary register for button states
INDEX_REG  EQU 0x22            ; Index register for tracking button-LED pairs
COUNT1     EQU 0x23            ; Outer loop counter for delay
COUNT2     EQU 0x24            ; Middle loop counter for delay
COUNT3     EQU 0x25            ; Inner loop counter for delay
Counter    EQU 0x26            ; Counter for looping
LEDSequence EQU 0x27           ; LED sequence storage (4 bytes)
InputIndex  EQU 0x32           ; Index for player input
PlayerInput EQU 0x2D           ; Player input storage (4 bytes for Level 1)
BUTTON_MASK EQU 0x0F           ; Mask for buttons (RC0-RC3)
Random     EQU 0x38            ; Random number storage
Delay1     EQU 0x36            ; Delay variable 1
Delay2     EQU 0x37            ; Delay variable 2
AttemptsCounter   EQU 0x39            ; Counter for looping
ClicksCounter   EQU 0x3A            ; Counter for number of clicks 
Counter3   EQU 0X3B		    ; Counter to addresses 	
TEMP_REG2   EQU 0x3C            ; Temporary register for button states
BlinkCounter  EQU 0x3D
IsSuccess    EQU 0X3E	        ; Boolean to determine wethere the user input is correct or not	
LevelsCounter EQU 0X3F	        ; Counter for the level number 
LEVEL    EQU 0x31    ; General-purpose register for player input pointer


SEED    EQU  0x70  ; Seed storage for randomness
TEMP    EQU  0x71  ; Temporary register
INDEX   EQU  0x72  ; Index for lookup table
LoopCounter   EQU  0x73  ; Index for lookup table
FSR_Backup   EQU 0x74   ; Temporary register to track LEDSequence pointer
TempPointer   EQU 0x75   ; Temporary register to track LEDSequence pointer

; Delay subroutine constants
DELAY_1S_COUNT1 EQU 0x07       ; Outer loop counter for 1-second delay
DELAY_1S_COUNT2 EQU 0xFF       ; Middle loop counter for 1-second delay
DELAY_1S_COUNT3 EQU 0xFF       ; Inner loop counter for 1-second delay



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main program
; ... [Previous code up to MAIN remains the same] ...

MAIN:
    CLRF    PORTB              ; Turn off all LEDs on PORTB (RB0-RB3)
    CLRF    TimeoutCounter     ; Initialize timeout counter to 0
    BANKSEL OPTION_REG
    MOVLW   b'00000111'        ; Prescaler 1:256 (slows Timer0 for seed variation)
    MOVWF   OPTION_REG
    BANKSEL PORTA
    ; ======== Initialize Ports ========
    BSF     STATUS, RP0        ; Select Bank 1
    CLRF    TRISB              ; PORTB as output
    MOVLW   0x0F               ; PORTC0-3 as input
    MOVWF   TRISC
    BCF     STATUS, RP0        ; Back to Bank 0
    ; ======== Configure PORTA for LCD ========
    BANKSEL TRISA
    CLRF    TRISA              ; PORTA as output
    BANKSEL PORTA
    ; ======== Initialize Variables ========
    CLRF    STATUS_REG
    CLRF    PORTB
    CALL    xms
    CALL    xms
    CALL    inid               ; Initialize LCD
    ; Display initial messages and wait for button press
    CALL    BlinkMessage       ; "Press any button to start"
    CALL    LevelMessage       ; Display initial level (Level 1)

WaitForStartButton:
    ; Wait for any button press (RC0-RC3)
    MOVF    PORTC, W
    ANDLW   BUTTON_MASK
    XORLW   BUTTON_MASK
    BTFSC   STATUS, Z          ; Skip if any button pressed
    GOTO    WaitForStartButton

    ; Generate and display the LED sequence
    CALL    GenerateLEDSequence
    CALL    Delay1Second

    CALL    DisplayLEDSequence
    CALL    Delay1Second
    ; Initialize counters and variables
    MOVLW   0x03
    MOVWF   AttemptsCounter
    MOVLW   0x03
    MOVWF   ClicksCounter
    CLRF    Counter3
    CLRF    IsSuccess
    CLRF    LevelsCounter      ; Initialize level counter to 0 (Level 1)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main programe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MAIN_LOOP:
    ; Check each button and toggle the corresponding LED
    MOVF    PORTC, W           ; Read PORTC (buttons)
    ANDLW   BUTTON_MASK        ; Mask out unused bits (RC0-RC3)
    XORLW   BUTTON_MASK        ; Invert the button states (active low)
    MOVWF   TEMP_REG           ; Store the button states in TEMP_REG

    ; Loop through each button-LED pair
    MOVLW   0x00               ; Start with RB0/RC0
    MOVWF   INDEX_REG          ; Use INDEX_REG to track the current pair

CHECK_BUTTONS:
    RRF     TEMP_REG, F        ; Rotate button states to the right
    BTFSS   STATUS, C          ; Skip if the current button is pressed
    GOTO    NEXT_BUTTON        ; If not pressed, check the next button

    ; Turn on the corresponding LED
    MOVF    INDEX_REG, W       ; Get the current index
    CALL    TURN_ON_LED        ; Call the turn-on function
    
    CALL    DELAY_1S
        ; Turn off the LED
    MOVF    INDEX_REG, W       ; Get the current index
    CALL    TURN_OFF_LED       ; Call the turn-off function   

    ; Check if all pairs have been pressed
    MOVF    ClicksCounter, W   ; Move the value of ClicksCounter into W
    BTFSS   STATUS, Z          ; Skip the next instruction if ClicksCounter is 0
    GOTO    NEXT_BUTTON      ; If ClicksCounter is not zero, continue 

    ; If ClicksCounter is zero, call checkPlayerInput
    CALL    checkPlayerInput   ; Compare player input with the LED sequence
 
    BTFSC   IsSuccess, 0    ; Check if bit 0 of IsSuccess is clear (0)  --> skip 
    GOTO    NextLevel     ; If not (attempt is success), jump to next level
  
    ; Decrement AttemptsCounter and check if it reaches zero
    DECFSZ  AttemptsCounter, F ; Decrement AttemptsCounter, skip next instruction if zero
    GOTO    RESET_COUNTERS     ; If AttemptsCounter is not zero, reset counters for the next attempt

    ; If AttemptsCounter is zero, end the game or reset
    GOTO    GAME_OVER          ; Handle game over logic

NEXT_BUTTON:
    INCF    INDEX_REG, F       ; Move to next button pair
    BTFSS   INDEX_REG, 2       ; Check if all pairs processed
    GOTO    CHECK_BUTTONS      ; Continue if not
    CALL    CHECK_TIMEOUT      ; Check for timeout after all buttons
    GOTO    MAIN_LOOP          ; Repeat main loop
   
RESET_COUNTERS:          ;Reset counters after each attempt
    MOVF    LevelsCounter, W   ; Move the value of levels counter into W
    ADDLW   0x03        ; Add the level number to the initial value
    MOVWF   ClicksCounter

    MOVLW   0x00               ; Reset Counter3 (address offset for PlayerInput)
    MOVWF   Counter3
    GOTO    MAIN_LOOP    ; Repeat the main loop for the next attempt

GAME_OVER:
    CALL    DisplayGameOver    ; Show "Game Over!"
   CLRF    LevelsCounter      ; Reset to Level 1
    CLRF    IsSuccess
    CLRF    AttemptsCounter
    GOTO    MAIN               ; Jump back to restart game
NextLevel:
    INCF    LevelsCounter, F   ; Increment level counter

    ; Check if all levels completed (e.g., 5 levels)
    MOVLW   D'20'               ; Maximum level (adjust as needed)
    SUBWF   LevelsCounter, W
    BTFSS   STATUS, C          ; Skip if LevelsCounter < 5
    GOTO    ContinueNextLevel

    ; All levels completed: Reset and restart
    CALL    DisplayMessages
    CLRF    LevelsCounter      ; Reset to Level 1
    CLRF    IsSuccess
    CLRF    AttemptsCounter
    GOTO    MAIN               ; Jump back to restart game

ContinueNextLevel:
    ; Proceed to next level setup
    CALL    Delay1Second
    CALL    Delay1Second
    CALL    LevelMessage
    CALL    Delay1Second
    CALL    Delay1Second
    CALL    GenerateLEDSequence
    CALL    DisplayLEDSequence
    CALL    Delay1Second
    DECF    IsSuccess, F       ; Reset IsSuccess
    MOVLW   0x03
    MOVWF   AttemptsCounter
    GOTO    RESET_COUNTERS


      
; Subroutine to turn on the LED corresponding to the index in W
TURN_ON_LED:
    ADDWF   PCL, F             ; Jump to the appropriate LED turn-on based on W
    GOTO    TURN_ON_RB0        ; Turn on RB0
    GOTO    TURN_ON_RB1        ; Turn on RB1
    GOTO    TURN_ON_RB2        ; Turn on RB2
    GOTO    TURN_ON_RB3        ; Turn on RB3

TURN_ON_RB0:
    BSF     PORTB, 0           ; Turn on RB0
    MOVLW   0x01               ; LED1 (RB0)
    CALL    STORE_PLAYER_INPUT ; Store in PlayerInput + Counter3
    DECF    ClicksCounter, F   ; Decrement ClicksCounter
        CLRF    TimeoutCounter     ; Reset timeout counter

    RETURN

TURN_ON_RB1:
    BSF     PORTB, 1           ; Turn on RB1
    MOVLW   0x02               ; LED2 (RB1)
    CALL    STORE_PLAYER_INPUT ; Store in PlayerInput + Counter3
    DECF    ClicksCounter, F        ;decrement number of clicks    CLRF    TimeoutCounter     ; Reset timeout counter

    RETURN

TURN_ON_RB2:
    BSF     PORTB, 2           ; Turn on RB2
    MOVLW   0x04               ; LED3 (RB2)
    CALL    STORE_PLAYER_INPUT ; Store in PlayerInput + Counter3
    DECF    ClicksCounter, F        ;decrement number of clicks
        CLRF    TimeoutCounter     ; Reset timeout counter

    RETURN

TURN_ON_RB3:
    BSF     PORTB, 3           ; Turn on RB3
    MOVLW   0x08               ; LED4 (RB3)
    CALL    STORE_PLAYER_INPUT ; Store in PlayerInput + Counter3
    DECF    ClicksCounter, F        ;decrement number of clicks
        CLRF    TimeoutCounter     ; Reset timeout counter

    RETURN
    
CHECK_TIMEOUT:
    BANKSEL INTCON             ; Access INTCON register
    BTFSS   INTCON, TMR0IF     ; Check if Timer0 overflowed
    RETURN                     ; Return if no overflow
    BCF     INTCON, TMR0IF     ; Clear Timer0 overflow flag
    INCF    TimeoutCounter, F  ; Increment timeout counter
    MOVLW   31                 ; 31 overflows ˜ 2 seconds
    SUBWF   TimeoutCounter, W
    BTFSS   STATUS, C          ; Check if TimeoutCounter >= 31
    RETURN                     ; Return if not yet 2 seconds
    CALL    checkPlayerInput   ; Timeout occurred, check input
    CALL    ClearPlayerInput   ; Clear the PlayerInput array
  ;;  DECF    AttemptsCounter, F ; Decrement AttemptsCounter, skip next instruction if zero
    CLRF    TimeoutCounter     ; Reset timeout counter
     ; Decrement AttemptsCounter and check if it reaches zero
    DECFSZ  AttemptsCounter, F ; Decrement AttemptsCounter, skip next instruction if zero
    GOTO    RESET_COUNTERS     ; If AttemptsCounter is not zero, reset counters for the next attempt
GOTO    GAME_OVER          ; Handle game over logic
    RETURN
        ; Repeat main loop
; ClearPlayerInput subroutine
ClearPlayerInput:
    MOVLW   PlayerInput      ; Load the starting address of PlayerInput
    MOVWF   FSR              ; Store it in the File Select Register (FSR)
    MOVLW   0x04             ; Load the number of bytes to clear (4 bytes for Level 1)
    MOVWF   Counter          ; Store in Counter

ClearLoop:
    CLRF    INDF             ; Clear the byte pointed to by FSR
    INCF    FSR, F           ; Move to the next byte in PlayerInput
    DECFSZ  Counter, F       ; Decrement Counter, skip if zero
    GOTO    ClearLoop        ; Repeat until all bytes are cleared

    RETURN                   ; Return from the subroutine
STORE_PLAYER_INPUT:
    MOVWF   TEMP_REG2           ; Temporarily store the LED value in TEMP_REG
    MOVF    Counter3, W   ; Move the value of Counter3 into W
    ADDLW   PlayerInput        ; Add the base address of PlayerInput to W
    MOVWF   FSR                ; Store the result in FSR (pointer to PlayerInput + ClicksCounter)
    MOVF    TEMP_REG2, W        ; Move the LED value into W
    MOVWF   INDF               ; Store the value in W at the address pointed to by FSR
    INCF    Counter3, F   ; Increment Counter3
    RETURN    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate a fixed LED sequence
; ======== Generate a Fixed LED Sequence ========

GenerateLEDSequence:

    ; Read Timer0 multiple times for better entropy
    MOVF    TMR0, W          ; Read Timer0
    ADDLW   0x55             ; Add a non-zero value to avoid zero
    MOVWF   SEED             ; Store as seed
    BTFSC   SEED, 7          ; Ensure high bit is not zero
    GOTO    Seed_OK
    COMF    SEED, F          ; Invert bits if high bit is zero
Seed_OK:
    ; Re-seed the RNG with Timer0 value
    MOVF    TMR0, W          ; Read Timer0 value
    BTFSC   STATUS, Z        ; Avoid zero seed
    ADDLW   0x05             ; Add a small offset if Timer0 is zero
    MOVWF   SEED             ; Update the seed

    ; Initialize loop counter (LevelsCounter + 2)
    MOVF    LevelsCounter, W ; Load LevelsCounter into W register
    ADDLW   0x03             ; Add 2 to LevelsCounter
    MOVWF   LoopCounter      ; Store LevelsCounter + 2 in LoopCounter

    ; Initialize pointer to LEDSequence
    MOVLW   LEDSequence      ; Load the starting address of LEDSequence
    MOVWF   FSR              ; Store it in the File Select Register (FSR)

GenerateLoop:
    CALL    RANDOM           ; Generate a random number
    MOVWF   INDF             ; Store the random number in the address pointed by FSR
    INCF    FSR, F           ; Increment FSR to point to the next position in LEDSequence

    DECFSZ  LoopCounter, F   ; Decrement LoopCounter and skip next instruction if zero
    GOTO    GenerateLoop     ; Repeat the loop if LoopCounter is not zero

    RETURN                   ; Return from the subroutine

; ======== RANDOM Number Generator ========
RANDOM:
    RLF     SEED, F          ; Shift left SEED into carry
    BTFSS   STATUS, C        ; Skip if carry is 1 (MSB was 1)
    GOTO    NO_XOR
    MOVLW   0x8D             ; Correct polynomial: 0x8D (x^8 + x^6 + x^5 + x^4 + 1)
    XORWF   SEED, F          ; Apply feedback to SEED
NO_XOR:
    MOVF    SEED, W          ; Load SEED into W
    ANDLW   0x03             ; Mask to 0-3 (lower 2 bits)
    CALL    LOOKUP_TABLE     ; Get LED pattern from lookup
    RETURN

; ======== LOOKUP TABLE ========
LOOKUP_TABLE:
    ADDWF   PCL, F
    RETLW   0x01   ; Index 0 ? 0x01
    RETLW   0x02   ; Index 1 ? 0x02
    RETLW   0x04   ; Index 2 ? 0x04
    RETLW   0x08   ; Index 3 ? 0x08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Display the LED sequence;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DisplayLEDSequence:
    ; Initialize loop counter (LevelsCounter + 2)
    MOVF    LevelsCounter, W   ; Load LevelsCounter into W register
    ADDLW   0x03               ; Add 2 to LevelsCounter
    MOVWF   Counter            ; Store LevelsCounter + 2 in Counter

    ; Initialize pointer to LEDSequence
    MOVLW   LEDSequence        ; Point to the start of the sequence
    MOVWF   FSR                ; Store in FSR (File Select Register)

DisplayLoop:
    MOVF    INDF, W            ; Load the current LED pattern from the address pointed by FSR
    MOVWF   PORTB              ; Turn on the LED by writing to PORTB
    CALL    Delay1Second       ; Keep it on for 1 second
    CALL    Delay1Second       ; Keep it on for another 1 second (total 2 seconds)
    CLRF    PORTB              ; Turn off the LED
    CALL    Delay1Second       ; Keep it off for 1 second

    INCF    FSR, F             ; Move to the next LED in the sequence
    DECFSZ  Counter, F         ; Decrement Counter, skip if zero
    GOTO    DisplayLoop        ; Repeat until all LEDs are displayed

    RETURN
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Delay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
; Delay subroutine (1 second)
Delay1Second:
    MOVLW   0xFF
    MOVWF   Delay1
    MOVWF   Delay2
DelayLoop:
    DECFSZ  Delay1, F
    GOTO    DelayLoop
    DECFSZ  Delay2, F
    GOTO    DelayLoop
    RETURN

; Subroutine for 1-second delay
DELAY_1S:
    MOVLW   DELAY_1S_COUNT1    ; Load outer loop counter
    MOVWF   COUNT1             ; Store in COUNT1

DELAY_OUTER:
    MOVLW   DELAY_1S_COUNT2    ; Load middle loop counter
    MOVWF   COUNT2             ; Store in COUNT2

DELAY_MIDDLE:
    MOVLW   DELAY_1S_COUNT3    ; Load inner loop counter
    MOVWF   COUNT3             ; Store in COUNT3

DELAY_INNER:
    DECFSZ  COUNT3, F          ; Decrement COUNT3 and skip if zero
    GOTO    DELAY_INNER        ; Repeat inner loop

    DECFSZ  COUNT2, F          ; Decrement COUNT2 and skip if zero
    GOTO    DELAY_MIDDLE       ; Repeat middle loop

    DECFSZ  COUNT1, F          ; Decrement COUNT1 and skip if zero
    GOTO    DELAY_OUTER        ; Repeat outer loop

    RETURN                     ; Return from delay subroutine
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Subroutine to turn off the LED corresponding to the index in W
TURN_OFF_LED:
    ADDWF   PCL, F             ; Jump to the appropriate LED turn-off based on W
    GOTO    TURN_OFF_RB0       ; Turn off RB0
    GOTO    TURN_OFF_RB1       ; Turn off RB1
    GOTO    TURN_OFF_RB2       ; Turn off RB2
    GOTO    TURN_OFF_RB3       ; Turn off RB3

TURN_OFF_RB0:
    BCF     PORTB, 0           ; Turn off RB0
    RETURN

TURN_OFF_RB1:
    BCF     PORTB, 1           ; Turn off RB1
    RETURN

TURN_OFF_RB2:
    BCF     PORTB, 2           ; Turn off RB2
    RETURN

TURN_OFF_RB3:
    BCF     PORTB, 3           ; Turn off RB3
    RETURN

; Check player input against the LED sequence
checkPlayerInput:
    ; Initialize LoopCounter = LevelsCounter + 2
    MOVF    LevelsCounter, W
    ADDLW   0x03
    MOVWF   LoopCounter

    ; Initialize pointers
    MOVLW   LEDSequence
    MOVWF   FSR_Backup       ; FSR_Backup points to LEDSequence
    MOVLW   PlayerInput
    MOVWF   TempPointer      ; TempPointer points to PlayerInput

CompareLoop:
    ; Load LEDSequence byte into TEMP
    MOVF    FSR_Backup, W    ; Load LEDSequence pointer into W
    MOVWF   FSR              ; Set FSR to point to LEDSequence
    MOVF    INDF, W          ; W = current byte of LEDSequence
    MOVWF   TEMP             ; Save in TEMP

    ; Load PlayerInput byte into W
    MOVF    TempPointer, W   ; Load PlayerInput pointer into W
    MOVWF   FSR              ; Set FSR to point to PlayerInput
    MOVF    INDF, W          ; W = current byte of PlayerInput

    ; Compare TEMP (LEDSequence) with W (PlayerInput)
    SUBWF   TEMP, W          ; TEMP - W ? Check Z flag
    BTFSS   STATUS, Z        ; Skip next line if bytes are equal
    GOTO    DisplayFailure   ; Bytes differ ? jump to failure

    ; Increment pointers for next byte
    INCF    FSR_Backup, F    ; Move to next byte in LEDSequence
    INCF    TempPointer, F   ; Move to next byte in PlayerInput

    ; Decrement LoopCounter and check if done
    DECFSZ  LoopCounter, F
    GOTO    CompareLoop      ; Repeat until LoopCounter = 0

    ; All bytes matched ? Success
    MOVLW   0x01
    MOVWF   IsSuccess
    GOTO    DisplaySuccess

 
    GOTO MAIN_LOOP

InputMismatch:
    CALL    DisplayFailure
    RETURN

; Display success message
DisplaySuccess:
    CALL ClearLCD
    
    MOVLW 0x80          ; Move cursor to the second line
    CALL SendCommand
    MOVLW 'C'
    CALL SendData
    MOVLW 'o'
    CALL SendData
    MOVLW 'n'
    CALL SendData
    MOVLW 'g'
    CALL SendData
    MOVLW 'r'
    CALL SendData
    MOVLW 'a'
    CALL SendData
    MOVLW 't'
    CALL SendData
    MOVLW 'u'
    CALL SendData
    MOVLW 'l'
    CALL SendData
    MOVLW 'a'
    CALL SendData
    MOVLW 't'
    CALL SendData
    MOVLW 'i'
    CALL SendData
    MOVLW 'o'
    CALL SendData
    MOVLW 'n'
    CALL SendData
    MOVLW 's'
    CALL SendData
    MOVLW '!'
    CALL SendData
    MOVLW '!'
    CALL SendData
    RETURN

; Display failure attempt message (Try again)
DisplayFailure:
    CALL ClearLCD

    MOVLW 0x80          ; Move cursor to the second line
    CALL SendCommand
    MOVLW 'T'
    CALL SendData
    MOVLW 'r'
    CALL SendData
    MOVLW 'y'
    CALL SendData
    MOVLW ' '
    CALL SendData
    MOVLW 'a'
    CALL SendData
    MOVLW 'g'
    CALL SendData
    MOVLW 'a'
    CALL SendData
    MOVLW 'i'
    CALL SendData
    MOVLW 'n'
    CALL SendData
    RETURN

; Display Game Over meesage
DisplayGameOver:
    CALL ClearLCD

    MOVLW 0x80          ; Move cursor to the second line
    CALL SendCommand
    MOVLW 'G'
    CALL SendData
    MOVLW 'a'
    CALL SendData
    MOVLW 'm'
    CALL SendData
    MOVLW 'e'
    CALL SendData
    MOVLW ' '
    CALL SendData
    MOVLW 'O'
    CALL SendData
    MOVLW 'v'
    CALL SendData
    MOVLW 'e'
    CALL SendData
    MOVLW 'r'
    CALL SendData
    MOVLW '!'
    CALL SendData
    
    RETURN
    
BlinkMessage:
    MOVLW   0x03               ; Set the number of blinks (3)
    MOVWF   BlinkCounter       ; Store in BlinkCounter

BlinkLoop:
    ; Display the message
    CALL    DisplayMessages    ; Display "Press any button to start"
    CALL    Delay1Second       ; Wait for 1 second

    ; Clear the LCD
    CALL    ClearLCD           ; Clear the LCD
    CALL    Delay1Second       ; Wait for 1 second

    ; Decrement BlinkCounter and check if it reaches zero
    DECFSZ  BlinkCounter, F    ; Decrement BlinkCounter, skip next instruction if zero
    GOTO    BlinkLoop          ; If BlinkCounter is not zero, repeat the blink loop

    RETURN                     ; Return after blinking 3 times
    
DisplayMessages:
    ; First line: "Press any button"
    MOVLW 0x80          ; Move cursor to the first line
    CALL SendCommand
    MOVLW 'P'
    CALL SendData
    MOVLW 'r'
    CALL SendData
    MOVLW 'e'
    CALL SendData
    MOVLW 's'
    CALL SendData
    MOVLW 's'
    CALL SendData
    MOVLW ' '
    CALL SendData
    MOVLW 'a'
    CALL SendData
    MOVLW 'n'
    CALL SendData
    MOVLW 'y'
    CALL SendData
    MOVLW ' '
    CALL SendData
    MOVLW 'b'
    CALL SendData
    MOVLW 'u'
    CALL SendData
    MOVLW 't'
    CALL SendData
    MOVLW 't'
    CALL SendData
    MOVLW 'o'
    CALL SendData
    MOVLW 'n'
    CALL SendData

    ; Second line: "to start"
    MOVLW 0xC0          ; Move cursor to the second line
    CALL SendCommand
    MOVLW 't'
    CALL SendData
    MOVLW 'o'
    CALL SendData
    MOVLW ' '
    CALL SendData
    MOVLW 's'
    CALL SendData
    MOVLW 't'
    CALL SendData
    MOVLW 'a'
    CALL SendData
    MOVLW 'r'
    CALL SendData
    MOVLW 't'
    CALL SendData
    RETURN

LevelMessage:
    CALL ClearLCD

    MOVLW 0x80          ; Move cursor to the second line
    CALL SendCommand
    MOVLW 'L'
    CALL SendData
    MOVLW 'E'
    CALL SendData
    MOVLW 'V'
    CALL SendData
    MOVLW 'E'
    CALL SendData
    MOVLW 'L'
    CALL SendData
    MOVLW ' '
    CALL SendData
    MOVF LevelsCounter, W  ; Move the value of LevelsCounter into W
    CALL ConvertToASCII    ; Convert it to ASCII before displaying
    CALL SendData
    MOVLW '!'
    CALL SendData
    RETURN

ConvertToASCII:
    ADDLW 0x31    ; Convert binary 0-9 to ASCII ('0' to '9')   ;Used 31 instead to 30 cuz the initial value of levelscounter is 0 so we need to change it to '1' not 0 
    RETURN
    
; Clear the LCD
ClearLCD:
    MOVLW 0x01          ; Clear display command
    CALL SendCommand
    RETURN

; Delay subroutine (1 second)


; Send command to LCD
SendCommand:
    BCF Select, RS      ; Clear RS (command mode)
    CALL send
    RETURN

; Send data to LCD
SendData:
    BSF Select, RS      ; Set RS (data mode)
    CALL send
    RETURN
Delay:
    MOVLW   0xFF            ; Adjust delay value as needed
    MOVWF   Delay1
DelayLoop2:
    DECFSZ  Delay1, F
    GOTO    DelayLoop2
    RETURN
;INCLUDE "LCDIS_PORTD.INC" ; IF U WANT TO USE LCD ON PORT D
INCLUDE "LCDIS_PORTA.INC" ; IF U WANT TO USE LCD ON PORT A
    END 