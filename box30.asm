
;-------------------------------------------------------------------------;
;                                Box 3.0                                  ;
;          August '06  AlferSoft (fvicente@alfersoft.com.ar)              ;
;                                                                         ;
;                                                                         ;
;  RB0-RB6 to 7-segment displays (2)                                      ;
;  RA0-RA1 to bases of transistors connect to common cathode of displays  ;
;  RA2 to amateur/professional switch                                     ;
;  RA3 to "playing" led                                                   ;
;  RB4 to "recess" led                                                   ;
;  RB7 to alarm output                                                    ;
;-------------------------------------------------------------------------;


        LIST p=16F84A
        #INCLUDE "p16f84a.inc"

;-------------------------------------------------------------------------;
;    Here we define our own personal registers and give them names        ;
;-------------------------------------------------------------------------;

ROUND       EQU H'0C'          ; this register holds the value of rounds
ROUND10     EQU H'0D'          ; holds value of 10's of rounds
SEC         EQU H'0E'          ; holds value of seconds counter
CURSTATUS   EQU H'0F'          ; everything 0's means recess, everything 1's playing
DIGCTR      EQU H'10'          ; 8 bit counter, only lowest bit actually used
DIGIT       EQU H'11'          ; hold digit number to access table
INTCNT      EQU H'12'          ; counts # interrupts to determine when 1 sec up
FUDGE       EQU H'13'          ; allows slight adjustment every 7 interrupts
RUNFLG      EQU H'14'          ; bit 0 only, tells if countdown in progress 
W_TEMP      EQU H'15'          ; temporarily holds value of W
STATUS_TEMP EQU H'16'          ; temporarily holds value of STATUS
SECNT       EQU H'17'          ; used in counting 50, 20 msec delays for 1 sec
CNTMSEC     EQU H'18'          ; used in timing of milliseconds
ALARM       EQU H'19'          ; bit 0 only, used as flag for when to alarm
ALARMCNT    EQU H'1A'          ; used in counting seconds that alarm was ringing
TMPVAR      EQU H'1B'          ; temporary holder
TMPCNT      EQU H'1C'          ; temporary counter
CURDIGIT	EQU	H'1D'
TIMER0		EQU H'1E'          ; timer digit #0
TIMER1		EQU H'1F'          ; timer digit #1
TIMER2		EQU H'20'          ; timer digit #2
TIMER3		EQU H'21'          ; timer digit #3
TMPVARR     EQU H'22'          ; temporary holder
TMPCNTR     EQU H'23'          ; temporary counter

;-------------------------------------------------------------------------;
;    Here we give names to some numbers to make their use more clear      ;
;-------------------------------------------------------------------------;

   #DEFINE   AMPRO       D'4'  ; amateur/professional bit
   #DEFINE   PLAYINGLED  D'3'  ; playing led bit
   #DEFINE   RECESSLED   D'4'  ; recess led bit
   #DEFINE   ALARMOUT    D'3'  ; alarm output bit
   #DEFINE   ROUNDDIGIT  H'0C' ; first digit address (following digits must be consecutive)

;-------------------------------------------------------------------------;
;         We set the start of code to orginate a location zero            ;
;-------------------------------------------------------------------------;

      ORG 0

            GOTO MAIN                  ; jump to the main routine
            NOP                        
            NOP                        
            NOP                        
            GOTO INTERRUPT             ; interrupt routine

;-------------------------------------------------------------------------;
;      These tables are used to get a 7-segment number bit pattern        ;
;-------------------------------------------------------------------------;

TABLE       ADDWF PCL,f
            RETLW b'11111101'          ; 0 digit
            RETLW b'01100001'          ; 1 digit
            RETLW b'11001111'          ; 2 digit
            RETLW b'11101011'          ; 3 digit
            RETLW b'01110011'          ; 4 digit
            RETLW b'10111011'          ; 5 digit
            RETLW b'10111111'          ; 6 digit
            RETLW b'11100001'          ; 7 digit
            RETLW b'11111111'          ; 8 digit
            RETLW b'11111011'          ; 9 digit
            RETLW b'00000011'          ; - symbol
            RETLW b'11000111'          ; º symbol
            RETLW b'00000000'          ; Display off
			;       abcfdeg.
TABLER      ADDWF PCL,f
            RETLW b'11101111'          ; 0 digit
            RETLW b'10001001'          ; 1 digit
            RETLW b'11010111'          ; 2 digit
            RETLW b'11011101'          ; 3 digit
            RETLW b'10111001'          ; 4 digit
            RETLW b'01111101'          ; 5 digit
            RETLW b'01111111'          ; 6 digit
            RETLW b'11001001'          ; 7 digit
            RETLW b'11111111'          ; 8 digit
            RETLW b'11111101'          ; 9 digit
            RETLW b'00010001'          ; - symbol
            RETLW b'11010011'          ; º symbol
            RETLW b'00000000'          ; Display off
			;       bafgcde.

;-------------------------------------------------------------------------;
;    This table is used to get a bit pattern that will turn on a digit    ;
;-------------------------------------------------------------------------;

BITPAT      ADDWF PCL,f                ; get bit pattern for transistors
            RETLW H'02'                ; a high, (1), turns the transistor on  
            RETLW H'01'                

;-------------------------------------------------------------------------;
;          Initialization routine sets up ports and timer                 ;
;-------------------------------------------------------------------------;

INIT        MOVLW H'00'                ; all outputs
            TRIS PORTB                 
            MOVLW H'10'                ; Port RA2 input, others outputs
            TRIS PORTA                 
            MOVLW H'03'                ; prescaler on TMR0 and 1:16
            OPTION                     
            MOVLW H'A0'                ; GIE & T0IE set T0IF cleared
            MOVWF INTCON               
            MOVLW H'F4'                ; initialize INTCNT = 244
            MOVWF INTCNT               
            MOVLW H'06'                ; initialize FUDGE = 6
            MOVWF FUDGE                
            MOVLW H'00'                ; initialize CURSTATUS = 0 (recess)
            MOVWF CURSTATUS
            CLRF ROUND                 ; reset round counter
            CLRF ROUND10               ; reset round counter tents
			MOVLW b'10000000'          ; led 1
			MOVWF CURDIGIT
            CALL NEXTSTATUS            ; change status, ring bell and set new counter
            RETURN                     

;-------------------------------------------------------------------------;
;  Stop count down, ring bell toggle to new status (recess/playing), turn ;
;  playing led on/off, and set new timeout for recess or playing (am/pro) ;
;-------------------------------------------------------------------------;

NEXTSTATUS  BCF RUNFLG,0               ; stop the countdown
            COMF CURSTATUS,W           ; toggle status in W
            MOVWF CURSTATUS            ; save new staus
			CLRF TIMER0
			CLRF TIMER2
			CLRF TIMER3
            BTFSC CURSTATUS,0          ; skip if we are in recess
            GOTO PLAYING               ; go to playing
            BCF PORTB,PLAYINGLED       ; turn playing led off
			BSF PORTB,RECESSLED        ; turn recess led on
			MOVLW D'1'
			MOVWF TIMER1
            MOVLW D'60'                ; recess is always 60 seconds
            MOVWF SEC                  ; 1 minute
            RETURN
PLAYING     CALL INCRROUND             ; increment round counter
			CALL DRAWROUND
            BSF PORTB,PLAYINGLED       ; turn playing led on
			BCF PORTB,RECESSLED        ; turn recess led off
            BTFSC PORTA,AMPRO          ; skip if switch not pressed (amateur)
            GOTO PRO                   ; go to professional
			MOVLW D'2'
			MOVWF TIMER1
            MOVLW D'120'               ; amateur plays 120 seconds
            MOVWF SEC                  ; 2 minutes
            RETURN
PRO         MOVLW D'3'
			MOVWF TIMER1
			MOVLW D'180'               ; professionals plays 180 seconds
            MOVWF SEC                  ; 3 minutes
            RETURN

;-------------------------------------------------------------------------;
;                        Ring bell for 2 seconds                          ;
;-------------------------------------------------------------------------;

RINGBELL    BSF PORTA,ALARMOUT
            MOVLW D'100'               ; 100 delays of 20 msec
            MOVWF ALARMCNT             ; into counting register
WAIT        CALL DLY20                 
            DECFSZ ALARMCNT,f          ; finished 1 sec delay?
            GOTO WAIT                  ; continue wait
            BCF PORTA,ALARMOUT
            RETURN

;-------------------------------------------------------------------------;
;   This is the interrupt routine that is jumped to when TMR0 overflows   ;
;-------------------------------------------------------------------------;

INTERRUPT	MOVWF W_TEMP               ; save W
            SWAPF STATUS,W             ; save status
            MOVWF STATUS_TEMP          ; without changing flags
			CALL DRAWDIGIT
			BCF STATUS,C               ; clear carry
			RRF CURDIGIT,W             ; shift current digit
			MOVWF CURDIGIT
			BTFSS CURDIGIT,4           ; test bit 4
			GOTO SKIPRESET
			MOVLW b'10000000'          ; reset to first digit
			MOVWF CURDIGIT
SKIPRESET   DECFSZ INTCNT,f            ; finished 1 sec ?
            GOTO RESTORE               ; not yet, return and enable inter.
            CALL EVERYSEC              ; go to every second routine
            MOVLW H'F4'                ; reset INTCNT to normal value
            MOVWF INTCNT               
            DECFSZ FUDGE,f             ; time for fudge?
            GOTO RESTORE               ; not yet, continue on
            MOVLW H'06'                ; reset FUDGE to 6
            MOVWF FUDGE                
            INCF INTCNT,f              ; INTCNT to 245
RESTORE     SWAPF STATUS_TEMP,W        ; get original status back
            MOVWF STATUS               ; into status register
            SWAPF STATUS_TEMP,f        ; old no flags trick again
            SWAPF STATUS_TEMP,W        ; to restore W
            BCF INTCON,T0IF            ; clear the TMR0 interrupt flag
            RETFIE                     ; finished

UPDATEDIS	DECF TIMER3,W              ; decrement seconds
			MOVWF TIMER3
            XORLW H'FF'                ; xor seconds and 255
            BTFSS STATUS,Z             ; test zero
            RETURN
			MOVLW D'9'                 ; reset seconds to 9
            MOVWF TIMER3
			DECF TIMER2,W              ; decrement tents of seconds
			MOVWF TIMER2
            XORLW H'FF'                ; xor tents of seconds and 255
            BTFSS STATUS,Z             ; test zero
            RETURN
			MOVLW D'5'                 ; reset tents of seconds to 5
            MOVWF TIMER2
			DECF TIMER1,W              ; decrement minutes
			MOVWF TIMER1
            XORLW H'FF'                ; xor minutes and 255
            BTFSS STATUS,Z             ; test zero
            RETURN
			MOVLW D'9'                 ; reset minutes to 9
            MOVWF TIMER1
			DECF TIMER0,W              ; decrement tents of minutes
			MOVWF TIMER0
            XORLW H'FF'                ; xor tents of minutes and 255
            BTFSS STATUS,Z             ; test zero
            RETURN
			MOVLW D'9'                 ; reset tents of minutes to 9
            MOVWF TIMER0
			RETURN

;-------------------------------------------------------------------------;
;       This routine is called by the interrupt routine every second      ;
;-------------------------------------------------------------------------;

EVERYSEC    BTFSS RUNFLG,0             ; return if runflg not set
            RETURN
			CALL UPDATEDIS             ; update display digits
            DECF SEC,f                 ; decrement seconds
            INCFSZ SEC,W               ; test for underflow
            RETURN                     ; seconds left to go
            CALL NEXTSTATUS            ; change status, ring bell and set new counter
            RETURN                     

;-------------------------------------------------------------------------;
;                     Increment round counter digits                      ;
;-------------------------------------------------------------------------;

INCRROUND   INCF ROUND,f               ; increment round counter
            MOVF ROUND,W               ; prepare test
            XORLW D'10'                ; test round digit <> 10
            BTFSS STATUS,Z
            RETURN                     ; digit < 10
            CLRF ROUND
            INCF ROUND10,f             ; increment tents round counter
            MOVF ROUND10,W             ; prepare test
            XORLW D'10'                ; test round tents digit <> 10
            BTFSS STATUS,Z
            RETURN                     ; digit < 10
            CLRF ROUND10
            RETURN                     

SHOWDIGIT	MOVWF TMPVAR               ; into temporary variable
			MOVLW D'8'                 ; 8
			MOVWF TMPCNT               ; into counting register
LOOPBIT		RRF TMPVAR,W
            BTFSS STATUS,C
			GOTO CONTOFF
			GOTO CONTON
CONTON      BSF PORTA,1                ; set data
			GOTO CONTINUE
CONTOFF		BCF PORTA,1                ; clear data
CONTINUE	MOVWF TMPVAR               ; into temporary variable
			BSF PORTA,2                ; set clock
			NOP
			BCF PORTA,2                ; clear clock
			NOP
            DECFSZ TMPCNT,f            ; finished count down
            GOTO LOOPBIT               ; continue loop
			BSF PORTA,0                ; set stroble
			NOP
			BCF PORTA,0                ; clear stroble
			NOP
			RETURN

GETNEXTDIG	MOVF TIMER0,W
			BTFSC CURDIGIT,4
			RETURN
			MOVF TIMER1,W
			BTFSC CURDIGIT,5
			RETURN
			MOVF TIMER2,W
			BTFSC CURDIGIT,6
			RETURN
			MOVF TIMER3,W
			RETURN

DRAWDIGIT	CALL GETNEXTDIG
			CALL TABLE
			CALL SHOWDIGIT
			MOVF PORTB,W               ; put current port value into W
            ANDLW b'00011111'          ; mask 4 lowest bits
			MOVWF PORTB                ; disable 4 digits
            IORWF CURDIGIT,W           ; or current digit
			MOVWF PORTB                ; enable digit
			RETURN

SHOWDIGITR	MOVWF TMPVARR              ; into temporary variable
			MOVLW D'8'                 ; 8
			MOVWF TMPCNTR              ; into counting register
LOOPBITR	RRF TMPVARR,W
            BTFSS STATUS,C
			GOTO CONTOFFR
			GOTO CONTONR
CONTONR     BSF PORTB,1                ; set data
			GOTO CONTINUER
CONTOFFR	BCF PORTB,1                ; clear data
CONTINUER	MOVWF TMPVARR              ; into temporary variable
			BSF PORTB,2                ; set clock
			NOP
			BCF PORTB,2                ; clear clock
			NOP
            DECFSZ TMPCNTR,f           ; finished count down
            GOTO LOOPBITR              ; continue loop
			RETURN

DRAWROUND	MOVF ROUND10,W
			CALL TABLER
			CALL SHOWDIGITR
			MOVF ROUND,W
			CALL TABLER
			CALL SHOWDIGITR
			BSF PORTB,0                ; set stroble
			NOP
			BCF PORTB,0                ; clear stroble
			NOP
			RETURN

;-------------------------------------------------------------------------;
;            This is the main routine, the program starts here            ;
;-------------------------------------------------------------------------;

MAIN        CALL INIT                  ; set up ports etc.
MAINLOOP    BTFSC RUNFLG,0             ; skip if stopped
            GOTO MAINLOOP              ; continue waiting for an event
            CALL RINGBELL              ; ring
            CALL STARTCNT              ; start running counter!
            GOTO MAINLOOP              ; continue waiting for an event

;-------------------------------------------------------------------------;
;               Re-initialize counters and set the run flag               ;
;-------------------------------------------------------------------------;

STARTCNT    MOVLW D'244'               ; reset INTCNT
            MOVWF INTCNT
            CLRF TMR0                  ; and clear timer 0
            BSF RUNFLG,0               ; start the countdown
            RETURN

;-------------------------------------------------------------------------;
;  The following are various delay routines based on instruction length.  ;  
;  The instruction length is assumed to be 1 microsecond (4Mhz crystal).  ;
;-------------------------------------------------------------------------;

;-------------------------------------------------------------------------;
;  N millisecond delay routine.                                           ;  
;-------------------------------------------------------------------------;

DLY20       MOVLW 20                   ; delay for 20 milliseconds
NMSEC       MOVWF CNTMSEC              ; delay for N (in W) milliseconds
MSECLOOP    MOVLW D'248'               ; load takes 1 microsec
            CALL MICRO4                ; by itself CALL takes ...
                                       ; 2 + 247 X 4 + 3 + 2 = 995
            NOP                        ; 1 more microsec 
            DECFSZ CNTMSEC,f           ; 1 when skip not taken, else 2
            GOTO MSECLOOP              ; 2 here: total 1000 per msecloop
            RETURN                     ; final time through takes 999 to here
                                       ; overhead in and out ignored

;-------------------------------------------------------------------------;
;  1 millisecond delay routine.                                           ;  
;-------------------------------------------------------------------------;
ONEMSEC     MOVLW D'249'               ; 1 microsec for load W
                                       ; loops below take 248 X 4 + 3 = 995
MICRO4      ADDLW H'FF'                ; subtract 1 from 'W'
            BTFSS STATUS,Z             ; skip when you reach zero
            GOTO MICRO4                ; loops takes 4 microsec, 3 for last
            RETURN                     ; takes 2 microsec
                                       ; call + load  W + loops + return =
                                       ; 2 + 1 + 995 + 2 = 1000 microsec

            END
