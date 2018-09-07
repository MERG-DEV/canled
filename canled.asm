    TITLE   "Source for CAN multiplexed LED driver using CBUS"
; filename CANLED2d.asm  12/11/09
; a LED driver for 64 LEDs intended for control panels. Consumer node for SLiM model only


; CAN rate at 125000 for now

;modified version of CANLED for using onboard FLASH for event storage.
;working except for unlearn.
;unlearn of events in flash working 15/01/08
;unlearn of EVs in EEPROM working
;clear all on reset if unlearn in. Working
;added polarity change during learn. Working
;added toggling of pairs with a single command.
;event applies to the taught LED and the next one is opposite.
;next one can still be controlled independently so care is needed.
;minor corrections  26/01/08
;Port corrections  31/01/08
;Fix to CAN filters to reject extended frames  (rev c)
;Fix to matrix clear. rev d  12/11/09




; 
; Assembly options
  LIST  P=18F2480,r=hex,N=75,C=120,T=ON

  include   "p18f2480.inc"

; set config registers

; note. there seem to be differences in the naming of the CONFIG parameters between
; versions of the p18F2480.inf files

  CONFIG  FCMEN = OFF, OSC = HSPLL, IESO = OFF
  CONFIG  PWRT = ON,BOREN = BOHW, BORV=0
  CONFIG  WDT=OFF
  CONFIG  MCLRE = ON
  CONFIG  LPT1OSC = OFF, PBADEN = OFF
  CONFIG  DEBUG = OFF
  CONFIG  XINST = OFF,LVP = OFF,STVREN = ON,CP0 = OFF
  CONFIG  CP1 = OFF, CPB = OFF, CPD = OFF,WRT0 = OFF,WRT1 = OFF, WRTB = OFF
  CONFIG  WRTC = OFF,WRTD = OFF, EBTR0 = OFF, EBTR1 = OFF, EBTRB = OFF

;original CONFIG settings left here for reference
  
; __CONFIG  _CONFIG1H,  B'00100110' ;oscillator HS with PLL
; __CONFIG  _CONFIG2L,  B'00001110' ;brown out voltage and PWT  
; __CONFIG  _CONFIG2H,  B'00000000' ;watchdog time and enable (disabled for now)
; __CONFIG  _CONFIG3H,  B'10000000' ;MCLR enable  
; __CONFIG  _CONFIG4L,  B'10000001' ;B'10000001'  for   no debug
; __CONFIG  _CONFIG5L,  B'00001111' ;code protection (off)  
; __CONFIG  _CONFIG5H,  B'11000000' ;code protection (off)  
; __CONFIG  _CONFIG6L,  B'00001111' ;write protection (off) 
; __CONFIG  _CONFIG6H,  B'11100000' ;write protection (off) 
; __CONFIG  _CONFIG7L,  B'00001111' ;table read protection (off)  
; __CONFIG  _CONFIG7H,  B'01000000' ;boot block protection (off)



; processor uses 4 MHz resonator but clock is 16 MHz.

;**************************************************************************
;definitions

LEARN     equ 0 ;setup jumper in port C
POL     equ 4 ;setup jumper in port C
UNLEARN   equ 1 ;setup jumper in port C
TOG     equ 3 ;setup jumper in port C
CMD_ON    equ 0x90  ;on event
CMD_OFF   equ 0x91  ;off event
SCMD_ON   equ 0x98  ;short on event
SCMD_OFF  equ 0x99  ;short off event
EN_NUM  equ .254  ;number of allowed events


;****************************************************************
; define RAM storage
  
  CBLOCK  0   ;file registers - access bank
          ;interrupt stack for low priority
          ;hpint uses fast stack
  W_tempL
  St_tempL
  Bsr_tempL
  PCH_tempH   ;save PCH in hpint
  PCH_tempL   ;save PCH in lpint
  Fsr_temp0L
  Fsr_temp0H 
  Fsr_temp1L
  Fsr_temp1H 
  Fsr_temp2L
  Fsr_temp2H 
  TempCANCON
  TempCANSTAT
  Datmode     ;flag for data waiting 
  Count     ;counter for loading
  Count1
  Count2
  Count3
  Dcount
  Number      ;used to set matrix
  Numtemp
  Roll
  Row       ;LED scan row
  Colout      ;LED scan column data
  Rowout      ;final row value
  Temp      ;temps
  Temp1
  Flcount     ;flash memory counter
  Flcount1    ;flash memory counter low
  Flcounth    ;flash memory counter high
  Blk_st      ;first block for unlearn
  Blk_lst     ;last block for unlearn
  Blk_num     ;number of blocks to shift up
  
  
  ENDC
  
  
  

  
  CBLOCK  h'60' ;rest of bank 0
  
  Rx0con      ;start of receive packet 0
  Rx0sidh
  Rx0sidl
  Rx0eidh
  Rx0eidl
  Rx0dlc
  Rx0d0
  Rx0d1
  Rx0d2
  Rx0d3
  Rx0d4
  Rx0d5
  Rx0d6
  Rx0d7
  
  
  Fhold     ;holding block for flash write
  Fhold1
  Fhold2
  Fhold3
  Fhold4
  Fhold5
  Fhold6
  Fhold7
  
  
  
  
  
    
  
  Cmdtmp    ;command temp for number of bytes in frame jump table
  Match   ;match flag
  ENcount   ;which EN matched
  ENcount1  ;temp for count offset
  ENend   ;last  EN number
  ENtemp
  ENtempl
  ENtemph
  EVtemp    ;holds current EV
  EVtemp1
  
  Mask
  
  
  
  Eadr    ;temp eeprom address
  
  
  
  Matrix    ;8 bytes for scan array
  Matrix1
  Matrix2
  Matrix3
  Matrix4
  Matrix5
  Matrix6
  Matrix7
  
  

    
  ;*************************************************************
  Intemp    ;used in input test
  Intemp1
  
  ;
  ;****************************************************************
  
  
    
  ENDC
  
  CBLOCK  0x100   ;bank 1
  Flash_buf
  Fb1
  Fb2
  Fb3
  Fb4
  Fb5
  Fb6
  Fb7
  Fb8
  Fb9
  Fb10
  Fb11
  Fb12
  Fb13
  Fb14
  Fb15
  Fb16
  Fb17
  Fb18
  Fb19
  Fb20
  Fb21
  Fb22
  Fb23
  Fb24
  Fb25
  Fb26
  Fb27
  Fb28
  Fb29
  Fb30
  Fb31
  Fb32
  Fb33
  Fb34
  Fb35
  Fb36
  Fb37
  Fb38
  Fb39
  Fb40
  Fb41
  Fb42
  Fb43
  Fb44
  Fb45
  Fb46
  Fb47
  Fb48
  Fb49
  Fb50
  Fb51
  Fb52
  Fb53
  Fb54
  Fb55
  Fb56
  Fb57
  Fb58
  Fb59
  Fb60
  Fb61
  Fb62
  Fb63
  Fb64
  Fb65
  Fb66
  Fb67
  Fb68
  
  ENDC
  
  
  

;****************************************************************
;
;   start of program code

    ORG   0000h
    nop           ;for debug
    goto  setup

    ORG   0008h
    goto  hpint     ;high priority interrupt

    ORG   0018h 
    goto  lpint     ;low priority interrupt


;*******************************************************************

    ORG   0020h     ;start of program
; 
;
;   high priority interrupt. Used for CAN receive and transmit error.

hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
    movff PCLATH,PCH_tempH    ;save PCLATH
    clrf  PCLATH
  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H

  
    movf  TempCANSTAT,W     ;Jump table
    andlw B'00001110'
    addwf PCL,F     ;jump
    bra   back
    bra   back      
    bra   back
    bra   back
    bra   back
    bra   rxb1int     ;only receive interrupts used
    bra   rxb0int
    bra   back
    
rxb1int bcf   PIR3,RXB1IF   ;uses RB0 to RB1 rollover so may never use this
                ;may need bank switch?
  
    lfsr  FSR0,Rx0con   ;
    bsf   Datmode,0
    goto  access
    
rxb0int bcf   PIR3,RXB0IF
    
    lfsr  FSR0,Rx0con
    bsf   Datmode,0
    goto  access
    


access  movf  CANCON,W
    andlw B'11110001'
    movwf CANCON
    movf  TempCANSTAT,W
    andlw B'00001110'
    iorwf CANCON
    lfsr  FSR1,RXB0CON  ;this is switched bank
load  movff POSTINC1,POSTINC0
    movlw 0x6E      ;end of access buffer lo byte
    cpfseq  FSR1L
    bra   load    
    
back  bcf   RXB0CON,RXFUL ;ready for next
  
back1 movlw B'00000000'
    andwf PIR3      ;clear any other flags
    movf  CANCON,W
    andlw B'11110001'
    iorwf TempCANCON,W
    
    movwf CANCON
    movff PCH_tempH,PCLATH
    movff Fsr_temp0L,FSR0L    ;recover FSR0
    movff Fsr_temp0H,FSR0H

    movff Fsr_temp1L,FSR1L    ;recover FSR1
    movff Fsr_temp1H,FSR1H

    
    retfie  1       ;use shadow registers



;**************************************************************
;
;

; 
;   low priority interrupt. Used by  timer 1 overflow. Every 10 millisecs.
;   used for LED scan routine
; 

lpint ;retfie

    movwf W_tempL       ;save variables
    movff STATUS,St_tempL
;   movff BSR,Bsr_tempL

    movff FSR2L,Fsr_temp2L
    movff FSR2H,Fsr_temp2H
    clrf  PIR1        ;clear all timer flags
    movlw 0x6F        ;Timer 1 lo byte. (adjust if needed)
    movwf TMR1L       ;reset timer 1
    
    movlw 2
    movwf Count3
    incf  Row,W
    andlw B'00000011'     ;count 0 to 3
    movwf Row
    lfsr  2,Matrix
    rlncf Row,W
    addwf FSR2L
    
    incf  FSR2L       ;add 1
nxt_byt movff INDF2,Colout    ;ready to send hi byte of column
    movlw 8
    movwf Count2
    
col1  rlcf  Colout,F
    bc    one_out
    bcf   PORTB,5       ;serial data
    bra   clock
one_out bsf   PORTB,5 
clock bsf   PORTB,4       ;clock it
    bcf   PORTB,4
    decfsz  Count2
    bra   col1
    decf  Count3,F      ;second byte?
    bz    row
    decf  FSR2L,F       ;LSbyte
    bra   nxt_byt
row   swapf Row,W
    movwf Rowout
    rlncf Rowout,F
    rlncf Rowout,W
    bsf   PORTC,2       ;row disable
    nop
    bsf   PORTB,1       ;latch serial
    bcf   PORTB,1       
    bcf   PORTC,6
    bcf   PORTC,7
    iorwf PORTC       ;put in new row
    bcf   PORTC,2       ;turn back on
    
    
lpend movff Fsr_temp2L,FSR2L
    movff Fsr_temp2H,FSR2H
;   movff Bsr_tempL,BSR
    movf  W_tempL,W
    movff St_tempL,STATUS 
    retfie  
            

            

;*********************************************************************


    


main  btfss Datmode,0   ;any new CAN frame received?
    bra   main
    bcf   Datmode,0
  
    
  
    
  
  
                ;main packet handling is here
    
packet  movlw CMD_ON  ;only ON and OFF events supported
    subwf Rx0d0,W
    bz    go_on
    movlw CMD_OFF
    subwf Rx0d0,W
    bz    go_on
    movlw SCMD_ON ;short commands?
    subwf Rx0d0,W
    bz    short
    movlw SCMD_OFF
    subwf Rx0d0,W
    bz    short
    bra   main
    
short clrf  Rx0d1 ;if short, ignore NN
    clrf  Rx0d2   
          

    
go_on btfss PORTC,LEARN
    bra   learn1      ;is in learn mode
    call  enmatch
    sublw 0
    bz    do_it
    bra   main      ;not here
    
do_it 
    call  ev_set      ;do it (not yet)
    bra   main
    
    
learn1  call  enmatch     ;is it there already?
    sublw   0
    bz    isthere
    btfss PORTC,UNLEARN   ;if unset and not here
    bra   l_out     ;do nothing else 
    call  learnin     ;put EN into stack and RAM
    sublw 0
    bz    isthere
    bra   l_out
isthere btfss PORTC,UNLEARN   ;is it here and unlearn,goto unlearn
    bra   unlearn     ;else modify EVs
    movf  PORTA,W     ;get switch
    andlw B'00111111'   ;mask
    movwf EVtemp
    bcf   EVtemp,6    ;allows for a clear of pol.
    btfss PORTC,POL     ;pol bit
    bsf   EVtemp,6
    bcf   EVtemp,7
    btfss PORTC,TOG   ;mode bit
    bsf   EVtemp,7
  
    
    movf  ENcount,W       ;recover EN counter
    
    addlw LOW EVstart       ;point to EV
    movwf EEADR
  
    movf  EVtemp,W    
    movwf EEDATA
    call  eewrite       ;put back EV value  
    
  
    call  ev_set      ;try it
      

l_out ;bcf    Datmode,0
    clrf  PCLATH
    goto  main
                ;unlearn an EN. 
unlearn movlw LOW ENindex+1   ;get number of events in stack
    movwf EEADR
    bsf   EECON1,RD
    
    movff EEDATA,ENend
    movff EEDATA,Blk_lst    ;work out which block is last
    rrncf Blk_lst,F     ;divide by 16
    rrncf Blk_lst,F
    rrncf Blk_lst,F
    rrncf Blk_lst,W
    andlw B'00001111'
    movwf Blk_lst
    movff ENcount,ENtempl ;point to EN in stack
    movff ENcount,Blk_st    ;work out the first block 
    rrncf Blk_st,F      ;divide by 16
    rrncf Blk_st,F
    rrncf Blk_st,F
    rrncf Blk_st,W
    andlw B'00001111'     ;16 blocks max.
    movwf Blk_st
    subwf Blk_lst,W
    movwf Blk_num       ;number of blocks to shift
                  ;now get table pointer
    clrf  ENtemph
    rlncf ENtempl,F     ;double it for pointing to Table 
    bcf   STATUS,C
    rlcf  ENtempl,F     ;double it
    rlcf  ENtemph,F     ;if carry from ENtempl
unlrn1  movlw B'11000000'
    andwf ENtempl,W
    movwf TBLPTRL       ;start of 64 byte block
    movf  ENtemph,W
    addlw 0x39
    movwf TBLPTRH
    
    movlw .68         ;read block + 4 more
    movwf Flcount
    lfsr  FSR2,Flash_buf    ;point to holding buffer
    clrf  TBLPTRU
    
    
read_block  
    tblrd*+       ;read into TABLAT and increment
    movf  TABLAT,W
    movwf POSTINC2
    decfsz  Flcount
    bra   read_block
    
    lfsr  FSR1,Flash_buf    ;use both FSR1 and FSR2 for shuffle
    lfsr  FSR2,Flash_buf+4
    movf  ENtempl,W
    andlw B'00111111'     ;64 byte range
    addwf FSR1L,F       ;FSR1 points to EN to remove
    addwf FSR2L,F       ;FSR2 points to EN to shift up
        
shuffle movff POSTINC2,POSTINC1
    movlw LOW Flash_buf+0x41    ;one past end of block
    subwf FSR1L,W
    bnz   shuffle
    
erase movf  ENtempl,W     ;point to start of block to erase
    movwf TBLPTRL
    movf  ENtemph,W
    addlw 0x39
    movwf TBLPTRH
    bsf   EECON1,EEPGD    ;set up for erase
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    bsf   EECON1,FREE
    clrf  INTCON
    bsf   PORTC,2       ;LEDs off
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR     ;erase
    nop
    movlw B'11000000'
    movwf INTCON        ;reenable interrupts
    bcf   PORTC,2       ;LEDs on
    
    
    
    movlw B'11000000'
    andwf TBLPTRL,F     ;put to 64 byte boundary
    tblrd*-           ;back 1
    lfsr  FSR2,Flash_buf    ;ready for rewrite
    movlw .8
    movwf Flcount
write movlw .8
    movwf Flcount1
    
write1  movf  POSTINC2,W
    movwf TABLAT
    tblwt+*
    decfsz  Flcount1
    bra   write1
    bsf   EECON1,EEPGD    ;set up for write back
    bcf   EECON1,CFGS
    bcf   EECON1,FREE
    bsf   EECON1,WREN
  
    clrf  INTCON
    
    bsf   PORTC,2       ;LEDs off
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR     ;write back
    nop
    nop
    nop
    decfsz  Flcount
    bra   write       ;next block of 8
    movf  Blk_num,F
    bz    lastblk
    decf  Blk_num,F
    tblrd*+
    movf  TBLPTRL,W
    movwf ENtempl
    movf  TBLPTRH,W
    movwf ENtemph
    movlw 0x39
    subwf ENtemph,F
      
    bra   unlrn1        ;shift up next block
    
lastblk movlw LOW EVstart
    addwf ENend,F       ;last in EEPROM
    incf  ENend,F
    movlw LOW EVstart
    addwf ENcount       ;point to one to remove
    incf  ENcount,W
    movwf EEADR
evshift call  eeread
    decf  EEADR
    call  eewrite
    incf  EEADR
    incf  EEADR
    movf  ENend,W
    subwf EEADR,W
    bnz   evshift

    
    
    movlw LOW ENindex+1
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf Temp
    decf  Temp,W
    call  eewrite     ;put back number in stack less 1
    bcf   EECON1,WREN
    movlw B'11000000'
    movwf INTCON        ;reenable interrupts
    bcf   PORTC,2       ;LEDs on
    bra   l_out
    

    
;***************************************************************************
;   main setup routine
;*************************************************************************
setup clrf  INTCON      ;no interrupts yet
    clrf  ADCON0      ;ADC is off
    movlw B'00001111'   ;set Port A to all digital for now
    movwf ADCON1
    movlw B'00111111'   ;Port A  is LED select (1 of 64)
    movwf TRISA     ;
    movlw B'00001001'   ;RB0 is spare, RB1 is /LE on serial MUX ,  RB2 = CANTX, RB3 = CANRX, 
                ;RB4 is MUX clock, RB5 is MUX data - RB6,7 for debug and LEDs
    movwf TRISB
    clrf  PORTB
    bsf   PORTB,2     ;CAN recessive
    movlw B'00011011'   ;Port C. RC0, RC1 are learn and unlearn, RC2 is LED enable, 
                ;RC3 is toggle input, RC4 is polarity input.
                ;RC5 spare, RC6, RC7 are row select.
                
    movwf TRISC
    clrf  PORTC     ;all outputs off
    bsf   PORTC,2     ;LEDs off
    
  

    
    bsf   RCON,IPEN   ;enable interrupt priority levels
    clrf  BSR       ;set to bank 0
  ; clrf  EECON1      ;no accesses to program memory  
    clrf  Datmode

    clrf  ECANCON     ;CAN mode 0 for now
     
    bsf   CANCON,7    ;CAN to config mode
    movlw B'00000011'   ;set CAN bit rate at 125000 for now
    movwf BRGCON1
    movlw B'10011110'   ;set phase 1 etc
    movwf BRGCON2
    movlw B'00000011'   ;set phase 2 etc
    movwf BRGCON3
    movlw B'00100000'
    movwf CIOCON      ;CAN to high when off
    movlw B'00100100'
    movwf RXB0CON     ;enable double buffer of RX0
    movlb .15
    movlw B'00100000'   ;reject extended frames
    movwf RXB1CON
    clrf  RXF0SIDL
    clrf  RXF1SIDL
    movlb 0
    
mskload lfsr  FSR0,RXM0SIDH   ;Clear masks, point to start
mskloop clrf  POSTINC0    
    movlw LOW RXM1EIDL+1    ;end of masks
    cpfseq  FSR0L
    bra   mskloop
    
    
  
    clrf  CANCON      ;out of CAN setup mode
    movlw B'10000001'   ;Timer 1 control.16 bit write
    movwf T1CON     ;Timer 1 is for output duration
    movlw 0xE0
    movwf TMR1H     ;set timer hi byte
    
    movlw B'00000011'
    movwf IPR3      ;high priority CAN RX  interrupts(for now)
    clrf  IPR1      ;all peripheral interrupts are low priority
    clrf  IPR2
    clrf  PIE2
    
    
    
    
    
    
    
    lfsr  FSR2,Matrix   ;clear LED matrix
matclr  clrf  POSTINC2
    movlw Matrix + 8
    subwf FSR2L,W
    bnz   matclr
    
    
    
    
    
    clrf  INTCON2     ;enable port B pullups 
    clrf  INTCON3     ;just in case
    
    bsf   PIE1,TMR1IE   ;enable Timer 1 interrupts
    movlw B'00000011'   ;Rx0 and RX1 interrupt 
    movwf PIE3  
    
    btfss PORTC,0     ;check for 'clear all' on reset
    bra   noclear
    btfsc PORTC,1     ;unlearn on
    bra   noclear
    call  clrflsh     ;clear all program flash events
    call  clrepr      ;clear all EEPROM
    
  
noclear clrf  PIR1
    clrf  PIR2
    clrf  PIR3      ;clear all flags
    bcf   RXB0CON,RXFUL
    bsf   PORTB,7     ;on LED
    movlw B'11000000'
    movwf INTCON      ;enable interrupts    

main4 goto  main  
    
;****************************************************************************
;   start of subroutines    


;********************************************************************
;   Set an event in scan array.  arrives with EV in EVtemp 

ev_set  call  ev_set1   ;single LED set
    btfss EVtemp,7  ;toggle mode?
    return
    movf  EVtemp,W
    addlw 1     ;next LED
    andlw B'00111111' ;prevent overflow past 64
    movwf EVtemp1   ;hold new number
    btfss EVtemp,6  ;pol set?
    bsf   EVtemp1,6
    movff EVtemp1,EVtemp
    call  ev_set1
    return
    


ev_set1 movlw B'00000111'
    andwf EVtemp,W
    movwf Numtemp
    movlw 1
    movwf Roll    ;rolling bit
set1  movf  Numtemp,F ;is it zero
    bz    gotnum
    rlncf Roll,F    ;roll bit
    decf  Numtemp,F
    bra   set1
gotnum  movlw B'00111000'
    andwf EVtemp,W
    movwf Numtemp
    rrncf Numtemp,F
    rrncf Numtemp,F
    rrncf Numtemp,W ;position of byte in matrix (0 to 7)
    lfsr  2,Matrix
    addwf FSR2L   ;offset
    btfsc Rx0d0,0   ;is it ON?
    bra   off
    btfsc EVtemp,6  ;check POL
    bra   off1
on    movf  INDF2,W
    iorwf Roll,W
    movwf INDF2   
    return      
off   btfsc EVtemp,6  ;check POL
    bra   on
off1  movf  INDF2,W
    comf  Roll,F
    andwf Roll,W
    movwf INDF2
    return
      
          
    

    







;   
eeread  bcf   EECON1,EEPGD  ;read a EEPROM byte, EEADR must be set before this sub.
    bcf   EECON1,CFGS
    bsf   EECON1,RD
    movf  EEDATA,W
    return

;**************************************************************************
eewrite movwf EEDATA      ;write to EEPROM, EEADR must be set before this sub.
    bcf   EECON1,EEPGD
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    
    clrf  INTCON  ;disable interrupts
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR
eetest  btfsc EECON1,WR
    bra   eetest
    bcf   PIR2,EEIF
    bcf   EECON1,WREN
    movlw B'11000000'
    movwf INTCON    ;reenable interrupts
    
    return  
    



;**************************************************************************
;
;   EN match. Compares EN (in Rx0d1, Rx0d2, Rx0d3 and Rx0d4) with stored ENs
;   If match, returns with W = 0
;   The matching number is in ENcount. The corresponding EV is in EVtemp 
;
enmatch clrf  TBLPTRL
    movlw 0x39
    movwf TBLPTRH   ;start of ENs in Flash
    movlw LOW ENindex+1 ;
    movwf EEADR
    bcf   EECON1,EEPGD
    bcf   EECON1,CFGS
    bsf   EECON1,RD
    movff EEDATA,Count
    movf  Count,F
  
    bz    en_out    ;if no events set, do nothing
    clrf  ENcount
  
    
ennext  clrf  Match
    tblrd*+
    movf  TABLAT,W
    cpfseq  Rx0d1
    incf  Match
    tblrd*+
    movf  TABLAT,W
    cpfseq  Rx0d2
    incf  Match
    tblrd*+
    movf  TABLAT,W
    cpfseq  Rx0d3
    incf  Match
    tblrd*+
    movf  TABLAT,W
    cpfseq  Rx0d4
    incf  Match
    tstfsz  Match
    bra   en_match
    movf  ENcount,W   ;get EVs
    addlw LOW EVstart   
    movwf EEADR
    
    bsf   EECON1,RD
    movff EEDATA,EVtemp ;EV
    
    
    retlw 0     ;is a match
en_match  
    movf  Count,F
    bz    en_out
    decf  Count,F
    incf  ENcount,F
    bra   ennext
en_out  retlw 1   
      
;**************************************************************************



    


;*********************************************************

;   learn input of EN

learnin btfss PORTC,UNLEARN     ;don't do if unlearn
    return
    movlw LOW ENindex+1
    movwf EEADR
    bsf   EECON1,RD
    movff EEDATA,ENcount    ;hold pointer
    movlw EN_NUM
    cpfslt  ENcount
    retlw 1           ;too many

  
nopage  clrf  ENtemph       ;work out FLASH address for new EN
    movff ENcount,ENtempl
    rlncf ENtempl,F     ;double it
    bcf   STATUS,C
    rlcf  ENtempl,F     ;double it
    rlcf  ENtemph,F     ;if carry from ENtempl
    call  fladd       ;add to FLASH

    movlw LOW ENindex+1
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    addlw 1         ;increment for next
    call  eewrite       ;put back
    retlw 0
    

    
;*******************************************************************


      
;************************************************************************************
;
;   FLASH add 
;   Add a 4 byte EN to end of FLASH. Assumes blank
;   ENtemph and ENtempl hold the offset for the start of the 4 bytes

fladd movf  ENtempl,W
    movwf TBLPTRL
    movf  ENtemph,W
    addlw 0x39      ;FLASH buffer starts at 0x3900
    movwf TBLPTRH
    btfsc ENtempl,2   ;which 4 of 8
    bra   fladd2
fladd1  movf  Rx0d1,W
    movwf TABLAT  ;1st four
    TBLWT*+
    movf  Rx0d2,W
    movwf TABLAT    
    TBLWT*+
    movf  Rx0d3,W
    movwf TABLAT    
    TBLWT*+
    movf  Rx0d4,W
    movwf TABLAT    
    TBLWT*+
    movlw 0xFF
    movwf TABLAT
    TBLWT*+
    movlw 0xFF
    movwf TABLAT
    TBLWT*+
    movlw 0xFF
    movwf TABLAT
    TBLWT*+
    movlw 0xFF
    movwf TABLAT
    TBLWT*
    
    bra   fladd3
fladd2  movlw 4
    subwf TBLPTRL,F     ;back 4
    tblrd*            ;read first 4 bytes of the 8
    tblwt*+ 
    tblrd*            ;put back in
    tblwt*+
    tblrd*
    tblwt*+         
    tblrd*
    tblwt*+
    movf  Rx0d1,W
    movwf TABLAT    ;2nd four is EN
    tblwt*+
    movf  Rx0d2,W
    movwf TABLAT    
    tblwt*+
    movf  Rx0d3,W
    movwf TABLAT    
    tblwt*+
    movf  Rx0d4,W
    movwf TABLAT
    tblwt*
    
fladd3  bsf   EECON1, EEPGD
    bcf   EECON1,CFGS
    bcf   EECON1,FREE     ;no erase
    bsf   EECON1, WREN
    bsf   PORTC,2       ;LEDs off
    clrf  INTCON
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR
    nop
    movlw B'11000000'
    movwf INTCON
    bcf   PORTC,2       ;LEDs on
    return
    
;*******************************************************************
;
;   Flash clear

clrflsh movlw .16     ;16 blocks of 64
    movwf Count
    movlw 0
    movlw LOW TBLPTRL
    movlw 0x39    ;start at 3900
    movwf TBLPTRH
    clrf  TBLPTRU
clr1  bsf   EECON1,EEPGD    ;set up for erase
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    bsf   EECON1,FREE
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR     ;erase
    nop
    nop
    nop
    decfsz  Count
    bra   nxtclr
    return
    
nxtclr  movlw .64
    addwf TBLPTRL,F
    movlw 0
    addwfc  TBLPTRH
    bra   clr1
    
;*********************************************************************

;   clear all EEPROM

clrepr  movlw .255      ;clear all 256 bytes
    movwf Count
    clrf  EEDATA
    movlw LOW ENindex
    movwf EEADR 
    bcf   EECON1,EEPGD
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
clrnxt  movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR
clrtst  btfsc EECON1,WR
    bra   clrtst
    movf  Count,F
    bz    clrdone
    decf  Count,F
    incf  EEADR
    bra   clrnxt
clrdone bcf   PIR2,EEIF
    bcf   EECON1,WREN
    return
    
;***********************************************************************      
;   a delay routine
      
dely  movlw .10
    movwf Count1
dely2 clrf  Count
dely1 decfsz  Count,F
    goto  dely1
    decfsz  Count1
    bra   dely2
    return    
    
;*************************************************************

    ORG 0x3900
    
ENstart       ;start of events in FLASH program memory


;************************************************************************   
  ORG 0xF00000      ;EEPROM data. Defaults
  

    
ENindex de  0,0x00  ;points to next available EN number (only hi byte used)

EVstart de  0,0   ;allows 254 possible EVs, one per event
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    de  0,0
    

    
    end
