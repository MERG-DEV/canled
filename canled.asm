  TITLE "Source for CANLED64"
; file name CANLED64_v2hBeta1.asm 12/07/14 (RH)

; version a - the first development version
; version b - add bootloader code
; version c - incorporate code from LED2j.asm
; version d - Slim mode appears to be working
; version e - Flim seems reasonable, learn and read events OK
; version f - delete event works OK, removes event leds from matrix
;     - clearing all events (0x55) also clears the matrix
; version g - make learn in Flim mode display updated leds
; version h - fix bug in findEN
; no version i
; version j - remove support for CMD_REQ and SCMD_REQ
; version k - 03/03/11 Add WRACK to FLiM learn, unlearn and erase all events
;       -   Boot command now only works with NN of zero
;     - Read parameters by index now works in SLiM mode with NN of zero
; no version l
; version m - 15/03/11 clear NN_temph and NN_templ in slimset
; version n - 19/03/11 stop LEDs flickering after read Events
; version p - 27/04/11 add flags to event variables
;       07/05/11 remove all test code

;Rev 102a   First version wrt CBUS Developers Guide
;     Add code to support 0x11 (RQMN)
;     Add code to return 8th parameter by index - Flags

;Rev 102b Ignore extended frames in packet receive routine
;Rev 102c Remove 102b fix

;Rev v2a    First release build, version 2
;Rev v2b    Correct error in do_rdev1 and send error code
;       if EV index is invalid
;Rev v2c    Change reply to QNN to OPC_PNN
;Rev v2d    Add check for zero index on read params and correct error code
;Rev v2e    Add more checks for valid EN and EV indices in evsend, correct error code
;Rev v2f    Change parameters to new format
;Rev v2g    Added self enum as separate sub routine. Added OpCodes 0x5D and 0x75
;       Changes to button pressing
;Rev v2h    Add new parameter structure and code to read PIC type
;       Go straight to main2 after teaching an event

;node number release frame <0x51><NN hi><NN lo>
;keep alive frame  <0x52><NN hi><NN lo>
;set learn mode <0x53><NN hi><NN lo>
;out of learn mode <0x54><NN hi><NN lo>
;clear all events <0x55><NN hi><NN lo>  Valid only if in learn mode
;read no. of events left <0x56><NN hi><NN lo>
;set event in learn mode  <0xD2><EN1><EN2><EN3><EN4><EVI><EV>  uses EV indexing
;The EV sent will overwrite the existing one
;read event variable in learn mode <0xB2><EN1><EN2><EN3><EN4><EVI>
;unset event in learn mode <0x95><EN1><EN2><EN3><EN4>
;reply to 0xB2. <0xD3><EN1><EN2><EN3><EN4><EVI><EV>
;Also sent if attempt to read / write too many EVs. Returns with EVI = 0 in that case
;read an event variable by index <0x9C><NN hi><NN lo><EN#><EV#>
;reply to 0x9C <B5><NN hi><NN lo><EN#><EV#><EVval>
; 0x72 and 0xF2 response to read stored ENs by index. Response is F2
; 0x57 to read all events. Response is F2
; 0x73  to read individual parameters. Response is 9B
; 0x58 to read number of stored events. Response is 73

;read node parameters <0x10> Only works in setup mode. Sends string of 7 bytes as 
;<0xEF><para1><para2><para3><para4><para5><para6><para7>

; This is the bootloader section

;*  Filename Boot2.asm  30/10/09

;*************************************************************** * * * * * * * * * * * * * * ;*
;*  CBUS bootloader

;*  Based on the Microchip botloader 'canio.asm' tho which full acknowledgement is made.
;*  Relevant information is contained in the Microchip Application note AN247

;*
;* Basic Operation:
;* The following is a CAN bootloader designed for PIC18F microcontrollers
;* with built-in CAN such as the PIC18F458. The bootloader is designed to
;* be simple, small, flexible, and portable.
;*
;
;
;*
;* Commands:
;* Put commands received from source (Master --> Slave)
;* The count (DLC) can vary.
;* XXXXXXXXXXX 0 0 8 XXXXXXXX XXXXXX00 ADDRL ADDRH ADDRU RESVD CTLBT SPCMD CPDTL CPDTH
;* XXXXXXXXXXX 0 0 8 XXXXXXXX XXXXXX01 DATA0 DATA1 DATA2 DATA3 DATA4 DATA5 DATA6 DATA7
;*


;*
;* ADDRL - Bits 0 to 7 of the memory pointer.
;* ADDRH - Bits 8 - 15 of the memory pointer.
;* ADDRU - Bits 16 - 23 of the memory pointer.
;* RESVD - Reserved for future use.
;* CTLBT - Control bits.
;* SPCMD - Special command.
;* CPDTL - Bits 0 - 7 of 2s complement checksum
;* CPDTH - Bits 8 - 15 of 2s complement checksum
;* DATAX - General data.
;*
;* Control bits:
;* MODE_WRT_UNLCK-Set this to allow write and erase operations to memory.
;* MODE_ERASE_ONLY-Set this to only erase Program Memory on a put command. Must be on 64-byte
;*  boundary.
;* MODE_AUTO_ERASE-Set this to automatically erase Program Memory while writing data.
;* MODE_AUTO_INC-Set this to automatically increment the pointer after writing.
;* MODE_ACK-Set this to generate an acknowledge after a 'put' (PG Mode only)
;*
;* Special Commands:
;* CMD_NOP      0x00  Do nothing
;* CMD_RESET    0x01  Issue a soft reset after setting last EEPROM data to 0x00
;* CMD_RST_CHKSM  0x02  Reset the checksum counter and verify
;* CMD_CHK_RUN    0x03  Add checksum to special data, if verify and zero checksum
;* CMD_BOOT_TEST  0x04  Just sends a message frame back to verify boot mode.

;*  Modified version of the Microchip code by M Bolton  30/10/09
;
; The user program must have the folowing vectors

; User code reset vector  0x0800
; User code HPINT vector  0x0808
; user code LPINT vector  0x0818

; Checksum is 16 bit addition of all programmable bytes.
; User sends 2s complement of addition at end of program in command 0x03 (16 bits only)

;**********************************************************************************



  LIST P=18F2480, r=hex, N=75, C=120,T=ON
  
  include   "p18f2480.inc"
  include   "cbuslib/cbusdefs.inc"
  
; definitions used by bootloader

#define MODE_SELF_VERIFY  ;Enable self verification of written data (undefine if not wanted)

#define HIGH_INT_VECT 0x0808  ;HP interrupt vector redirect. Change if target is different
#define LOW_INT_VECT  0x0818  ;LP interrupt vector redirect. Change if target is different.
#define RESET_VECT  0x0800  ;start of target
#define CAN_CD_BIT  RXB0EIDL,0  ;Received control / data select bit
#define CAN_PG_BIT  RXB0EIDL,1  ;Received PUT / GET bit
#define CANTX_CD_BIT  TXB0EIDL,0  ;Transmit control/data select bit
#define CAN_TXB0SIDH  B'10000000' ;Transmitted ID for target node
#define CAN_TXB0SIDL  B'00001000'
#define CAN_TXB0EIDH  B'00000000' ;
#define CAN_TXB0EIDL  B'00000100'
#define CAN_RXF0SIDH  B'00000000' ;Receive filter for target node
#define CAN_RXF0SIDL  B'00001000'
#define CAN_RXF0EIDH  B'00000000'
#define CAN_RXF0EIDL  B'00000111'
#define CAN_RXM0SIDH  B'11111111' ;Receive masks for target node
#define CAN_RXM0SIDL  B'11101011'
#define CAN_RXM0EIDH  B'11111111'
#define CAN_RXM0EIDL  B'11111000'
#define CAN_BRGCON1   B'00000011' ;CAN bit rate controls. As for other CBUS modules
#define CAN_BRGCON2   B'10011110'
#define CAN_BRGCON3   B'00000011'
#define CAN_CIOCON    B'00100000' ;CAN I/O control  

; ************************************************************ ** * * * * * * * * * * * * * * *


#ifndef EEADRH    
#define EEADRH  EEADR+ 1  
#endif      
#define TRUE  1 
#define FALSE 0 
#define WREG1 PRODH ; Alternate working register
#define WREG2 PRODL 
#define MODE_WRT_UNLCK  _bootCtlBits, 0 ; Unlock write and erase
#define MODE_ERASE_ONLY _bootCtlBits, 1 ; Erase without write
#define MODE_AUTO_ERASE _bootCtlBits, 2 ; Enable auto erase before write
#define MODE_AUTO_INC _bootCtlBits, 3 ; Enable auto inc the address
#define MODE_ACK    _bootCtlBits, 4 ; Acknowledge mode
#define ERR_VERIFY    _bootErrStat, 0 ; Failed to verify if set
#define CMD_NOP     0x00  
#define CMD_RESET   0x01  
#define CMD_RST_CHKSM 0x02  
#define CMD_CHK_RUN   0x03
#define CMD_BOOT_TEST   0x04  


;definitions

S_PORT    equ PORTB ;setup switch  Change as needed
S_BIT   equ 0
LEARN     equ 0 ;setup jumper in port C
POL     equ 4 ;setup jumper in port C
UNLEARN   equ 1 ;setup jumper in port C
TOG     equ 3 ;setup jumper in port C

LED_PORT equ  PORTB  ;change as needed
LED2  equ   7 ;PB7 is the green LED on the PCB
LED1  equ   6 ;PB6 is the yellow LED on the PCB

CMD_ON    equ 0x90  ;on event
CMD_OFF   equ 0x91  ;off event
CMD_REQ   equ 0x92
SCMD_ON   equ 0x98  ;short on event
SCMD_OFF  equ 0x99  ;short off event
SCMD_REQ  equ 0x9A
OPC_PNN   equ 0xB6  ; reply to QNN

EN_NUM  equ .255  ;number of allowed events

EV_NUM  equ .17   ;number of allowed EVs per event
NV_NUM  equ 0   ;number of allowed NVs for node (provisional)
LED64_ID equ 7    ; id for CANLED64

CONSUMER  equ 1
PRODUCER  equ 2
COMBI   equ 3

RQNN  equ 0xbc  ; response to OPC_QNN - provisional

Modstat equ 1   ;address in EEPROM

MAN_NO      equ MANU_MERG    ;manufacturer number
MAJOR_VER   equ 2
MINOR_VER   equ "H"
MODULE_ID   equ MTYP_CANLED64 ; id to identify this type of module
EVT_NUM     equ EN_NUM           ; Number of events
EVperEVT    equ EV_NUM           ; Event variables per event
NV_NUM      equ 0          ; Number of node variables
NODEFLGS    equ PF_CONSUMER + PF_BOOT
CPU_TYPE    equ P18F2480
BETA    equ 1




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
  
;**************************************************************************
; RAM addresses used by boot. can also be used by application.

  CBLOCK 0
  _bootCtlMem
  _bootAddrL    ; Address info
  _bootAddrH    
  _bootAddrU    
  _unused0    ;(Reserved)
  _bootCtlBits  ; Boot Mode Control bits
  _bootSpcCmd   ; Special boot commands
  _bootChkL   ; Chksum low byte fromPC
  _bootChkH   ; Chksum hi byte from PC    
  _bootCount    
  _bootChksmL   ; 16 bit checksum
  _bootChksmH   
  _bootErrStat  ;Error Status flags
  ENDC
  
  ; end of bootloader RAM


  
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
  CanID_tmp   ;temp for CAN Node ID
  IDtemph     ;used in ID shuffle
  IDtempl
  NN_temph    ;node number in RAM
  NN_templ
  ENtemp1     ;number of events
  TestTemp
  
  IDcount     ;used in self allocation of CAN ID.
  
  Mode      ;for FLiM / SLiM etc
  Datmode     ;flag for data waiting 
  Count     ;counter for loading
  Count1
  Count2
  Count3
  CountFb0  ; used when reading/writing fb data from/to flash
  CountFb1  ;  ditto
  Lcount2   ;used in Lpint
  Lcount3
  Keepcnt   ;keep alive counter
  Latcount  ;latency counter

  Number      ;used to set matrix
  Numtemp
  Roll
  Row       ;LED scan row
  Colout      ;LED scan column data
  Rowout      ;final row value
  Temp      ;temps
  Temp1
  Dlc       ;data length
  
  evaddrh     ; event data ptr
  evaddrl
  prevadrh    ; previous event data ptr
  prevadrl
  nextadrh    ; next event data ptr
  nextadrl
  htaddrh     ; current hash table ptr
  htaddrl
  htidx     ; index of current hash table entry in EEPROM
  hnum      ; actual hash number
  freadrh     ; current free chain address
  freadrl
  
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

  Tx1con
  Tx1sidh
  Tx1sidl
  Tx1eidh
  Tx1eidl
  Tx1dlc
  Tx1d0
  Tx1d1
  Tx1d2
  Tx1d3
  Tx1d4
  Tx1d5
  Tx1d6
  Tx1d7
  
  Match   ;match flag
  ENcount   ;which EN matched
  ENcount1  ;temp for count offset

  EVtemp    ;holds current EV
  EVtemp1
  
  EVidx   ; EV index from learn cmd
  EVdata    ; EV data from learn cmd
  ENidx   ; event index from commands which access events by index
  EVflags
  
  
  Matrix    ;8 bytes for scan array
  Matrix1
  Matrix2
  Matrix3
  Matrix4
  Matrix5
  Matrix6
  Matrix7
  
  FlMatOn0  ; LEDs which flash On/Off
  FlMatOn1
  FlMatOn2
  FlMatOn3
  FlMatOn4
  FlMatOn5
  FlMatOn6
  FlMatOn7
  
  FlMatOff0 ; LEDs which flash Off/On
  FlMatOff1
  FlMatOff2
  FlMatOff3
  FlMatOff4
  FlMatOff5
  FlMatOff6
  FlMatOff7
  
  FlCtrl    ; LED flash contol
  FlCount   ; flash rate counter
  TkCount   ; counts number of timer ticks
  
  Rolle   ;rolling bit for enum
  
  Fsr_tmp1Le  ;temp store for FSR1
  Fsr_tmp1He 
  Enum0   ;bits for new enum scheme.
  Enum1
  Enum2
  Enum3
  Enum4
  Enum5
  Enum6
  Enum7
  Enum8
  Enum9
  Enum10
  Enum11
  Enum12
  Enum13
  
  ;add variables to suit
;data area to store data for event learning and event matching

  ev0   ;event number from learn command and from received event
  ev1
  ev2
  ev3
  led0  ; led and polarity data for current event
  led1
  led2
  led3
  led4
  led5
  led6
  led7
  pol0
  pol1
  pol2
  pol3
  pol4
  pol5
  pol6
  pol7
  flags
  
  setevt  ; temp variable. holds next event number
  
  endc


; data area for storing event data while flash is being updated 
; Structure of each 32 bytes is as follows
; Bytes 0-3 - the event number
; Bytes 4-5 - the next event pointer
; Bytes 6-7 - the previous event pointer
; Bytes 8-15 - the led control bits
; Bytes 16-23 - the polarity control bits
; Bytes 24-31 - spare (unused)

  CBLOCK 0x100  ; bank 1
  ; first event data block
  evt00
  evt01
  evt02
  evt03
  next0h
  next0l
  prev0h
  prev0l
  led00
  led01
  led02
  led03
  led04
  led05
  led06
  led07
  pol00
  pol01
  pol02
  pol03
  pol04
  pol05
  pol06
  pol07
  flags0
  fb25
  fb26
  fb27
  fb28
  fb29
  fb30
  fb31
  ; second event data block
  evt10
  evt11
  evt12
  evt13
  next1h
  next1l
  prev1h
  prev1l
  led10
  led11
  led12
  led13
  led14
  led15
  led16
  led17
  pol10
  pol11
  pol12
  pol13
  pol14
  pol15
  pol16
  pol17
  flags1
  fb57
  fb58
  fb59
  fb60
  fb61
  fb62
  fb63
  
  endc

;****************************************************************
; This is the bootloader
; ***************************************************************************** 
;_STARTUPCODE 0x00
  ORG 0x0000
; *****************************************************************************
  bra _CANInit
  bra _StartWrite
; ***************************************************************************** 
;_INTV_H CODE 0x08
  ORG 0x0008
; *****************************************************************************

  goto  HIGH_INT_VECT

; ***************************************************************************** 
;_INTV_L CODE 0x18
  ORG 0x0018
; *****************************************************************************

  goto  LOW_INT_VECT 

; ************************************************************** 
; Code start
; **************************************************************
  ORG 0x0020
;_CAN_IO_MODULE CODE
; ************************************************************ ** * * * * * * * * * * * * * * * 
; Function: VOID _StartWrite(WREG _eecon_data)
;PreCondition: Nothing
;Input: _eecon_data
;Output: Nothing. Self write timing started.
;Side Effects: EECON1 is corrupted; WREG is corrupted.
;Stack Requirements: 1 level.
;Overview: Unlock and start the write or erase sequence to protected
; memory. Function will wait until write is finished.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_StartWrite
  movwf   EECON1
  btfss   MODE_WRT_UNLCK  ; Stop if write locked
  return
  movlw   0x55  ; Unlock
  movwf    EECON2 
  movlw  0xAA 
  movwf    EECON2
  bsf  EECON1, WR ; Start the write
  nop
  btfsc   EECON1, WR  ; Wait (depends on mem type)
  bra $ - 2
  return
; ************************************************************ ** * * * * * * * * * * * * * * *

; Function: _bootChksm _UpdateChksum(WREG _bootChksmL)
;
; PreCondition: Nothing
; Input: _bootChksmL
; Output: _bootChksm. This is a static 16 bit value stored in the Access Bank.
; Side Effects: STATUS register is corrupted.
; Stack Requirements: 1 level.
; Overview: This function adds a byte to the current 16 bit checksum
; count. WREG should contain the byte before being called.
;
; The _bootChksm value is considered a part of the special
; register set for bootloading. Thus it is not visible. ;
;*************************************************************** * * * * * * * * * * * *
_UpdateChksum:
  addwf _bootChksmL,  F ; Keep a checksum
  btfsc STATUS, C
  incf  _bootChksmH,  F
  return
;************************************************************ ** * * * * * * * * * * * * * * *
;
; Function: VOID _CANInit(CAN,  BOOT)
;
; PreCondition: Enter only after a reset has occurred.
; Input: CAN control information, bootloader control information ; Output: None.
; Side Effects: N/A. Only run immediately after reset.
; Stack Requirements: N/A
; Overview: This routine is technically not a function since it will not
; return when called. It has been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied. ;
; This routine tests the boot flags to determine if boot mode is
; desired or normal operation is desired. If boot mode then the
; routine initializes the CAN module defined by user input. It
; also resets some registers associated to bootloading.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_CANInit:
  clrf  EECON1
  setf  EEADR ; Point to last location of EEDATA
  setf  EEADRH
  bsf EECON1, RD  ; Read the control code
  incfsz EEDATA, W

  goto  RESET_VECT


  clrf  _bootSpcCmd   ; Reset the special command register
  movlw   0x1C    ; Reset the boot control bits
  movwf   _bootCtlBits 
  movlb d'15'   ; Set Bank 15
  bcf   TRISB, CANTX  ; Set the TX pin to output 
  movlw   CAN_RXF0SIDH  ; Set filter 0
  movwf   RXF0SIDH
  movlw   CAN_RXF0SIDL 
  movwf   RXF0SIDL
  comf  WREG    ; Prevent filter 1 from causing a receive event





  movwf RXF1SIDL  ;   
  movlw CAN_RXF0EIDH  
  movwf RXF0EIDH  
  movlw CAN_RXF0EIDL  
  movwf RXF0EIDL  
  movlw CAN_RXM0SIDH  ; Set mask
  movwf RXM0SIDH  
  movlw CAN_RXM0SIDL  
  movwf RXM0SIDL  
  movlw CAN_RXM0EIDH  
  movwf RXM0EIDH  
  movlw CAN_RXM0EIDL  
  movwf RXM0EIDL  
  movlw CAN_BRGCON1 ; Set bit rate
  movwf BRGCON1 
  movlw CAN_BRGCON2 
  movwf BRGCON2 
  movlw CAN_BRGCON3 
  movwf BRGCON3 
  movlw CAN_CIOCON  ; Set IO
  movwf CIOCON  
  
  clrf  CANCON  ; Enter Normal mode
  movlw B'00001110'
  movwf ADCON1
  bcf TRISB,7
  bcf TRISB,6
  bsf PORTB,7   ;green LED on
  bsf PORTB,6   ;yellow LED on


; ************************************************************ ** * * * * * * * * * * * * * * * 
; This routine is essentially a polling loop that waits for a
; receive event from RXB0 of the CAN module. When data is
; received, FSR0 is set to point to the TX or RX buffer depending
; upon whether the request was a 'put' or a 'get'.
; ************************************************************ ** * * * * * * * * * * * * * * * 
_CANMain
  
  bcf RXB0CON, RXFUL  ; Clear the receive flag
_wait clrwdt      ; Clear WDT while waiting   
  btfss   RXB0CON, RXFUL  ; Wait for a message  
  bra _wait



_CANMainJp1
  lfsr  0, RXB0D0
  movf  RXB0DLC, W 
  andlw   0x0F
  movwf   _bootCount 
  movwf   WREG1
  bz  _CANMain 
_CANMainJp2       ;?
  


; ************************************************************** * * * * * * * * * * * * * * * 
; Function: VOID _ReadWriteMemory()
;
; PreCondition:Enter only after _CANMain().
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: This routine is technically not a function since it will not
; return when called. It has been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;This is the memory I/O engine. A total of eight data bytes are received and decoded. In addition two control bits are received, put/get and control/data.
;A pointer to the buffer is passed via FSR0 for reading or writing. 
;The control register set contains a pointer, some control bits and special command registers.
;Control
;<PG><CD><ADDRL><ADDRH><ADDRU><_RES_><CTLBT>< SPCMD><CPDTL><CPDTH>
;Data
;<PG>< CD>< DATA0>< DATA1>< DATA2>< DATA3>< DATA4>< DATA5>< DATA6>< DATA7>
;PG bit:  Put = 0, Get = 1
;CD bit:  Control = 0, Data = 1

; ************************************************************ ** * * * * * * * * * * * * * * *
_ReadWriteMemory:
  btfsc CAN_CD_BIT  ; Write/read data or control registers
  bra _DataReg
; ************************************************************ ** * * * * * * * * * * * * * * * ; This routine reads or writes the bootloader control registers,
; then executes any immediate command received.
_ControlReg
  lfsr  1, _bootAddrL   ;_bootCtlMem
_ControlRegLp1

  movff   POSTINC0, POSTINC1 
  decfsz  WREG1, F
  bra _ControlRegLp1

; ********************************************************* 
; This is a no operation command.
  movf  _bootSpcCmd, W    ; NOP Command
  bz  _CANMain
; bz  _SpecialCmdJp2    ; or send an acknowledge

; ********************************************************* 
; This is the reset command.
  xorlw   CMD_RESET   ; RESET Command 
  btfss   STATUS, Z
  bra   _SpecialCmdJp4
  setf  EEADR   ; Point to last location of EEDATA
  setf  EEADRH
  clrf  EEDATA    ; and clear the data (FF for now)
  movlw   b'00000100' ; Setup for EEData
  rcall   _StartWrite
  bcf   PORTB,6   ;yellow LED off
  reset
; *********************************************************
; This is the Selfcheck reset command. This routine 
; resets the internal check registers, i.e. checksum and 
; self verify.
_SpecialCmdJp4
  movf  _bootSpcCmd, W 
  xorlw   CMD_RST_CHKSM
  bnz   _SpecialCmdJp1
  clrf  _bootChksmH
  clrf  _bootChksmL
  bcf   ERR_VERIFY    
  clrf  _bootErrStat
  bra   _CANMain
; RESET_CHKSM Command
; Reset chksum
; Clear the error verify flag

;This is the Test and Run command. The checksum is
; verified, and the self-write verification bit is checked. 
; If both pass, then the boot flag is cleared.
_SpecialCmdJp1
  movf  _bootSpcCmd, W    ; RUN_CHKSM Command
  xorlw   CMD_CHK_RUN 
  bnz _SpecialCmdJp3
  movf  _bootChkL, W  ; Add the control byte
  addwf  _bootChksmL, F
  bnz _SpecialCmdJp2
  movf  _bootChkH, W 
  addwfc  _bootChksmH, F
  bnz _SpecialCmdJp2
  btfsc   ERR_VERIFY    ; Look for verify errors
  bra _SpecialCmdJp2

  bra   _CANSendOK  ;send OK message


_SpecialCmdJp2

  bra _CANSendNOK ; or send an error acknowledge


_SpecialCmdJp3
  movf  _bootSpcCmd, W    ; RUN_CHKSM Command
  xorlw   CMD_BOOT_TEST 
  bnz _CANMain
  bra _CANSendBoot

; ************************************************************** * * * * * * * * * * * * * * * 
; This is a jump routine to branch to the appropriate memory access function.
; The high byte of the 24-bit pointer is used to determine which memory to access. 
; All program memories (including Config and User IDs) are directly mapped. 
; EEDATA is remapped.
_DataReg
; *********************************************************
_SetPointers
  movf  _bootAddrU, W ; Copy upper pointer
  movwf   TBLPTRU
  andlw   0xF0  ; Filter
  movwf   WREG2
  movf  _bootAddrH, W ; Copy the high pointer
  movwf   TBLPTRH
  movwf   EEADRH
  movf  _bootAddrL, W ; Copy the low pointer
  movwf   TBLPTRL
  movwf  EEADR
  btfss   MODE_AUTO_INC ; Adjust the pointer if auto inc is enabled
  bra _SetPointersJp1
  movf  _bootCount, W ; add the count to the pointer
  addwf  _bootAddrL, F 
  clrf  WREG
  addwfc   _bootAddrH, F 
  addwfc   _bootAddrU, F 

_SetPointersJp1     ;?

_Decode
  movlw   0x30
  cpfslt  WREG2
  bra _DecodeJp1



  bra _PMEraseWrite

_DecodeJp1
  movf  WREG2,W
  xorlw   0x30
  bnz _DecodeJp2



  bra _CFGWrite 
_DecodeJp2
  movf  WREG2,W 
  xorlw 0xF0
  bnz _CANMain
  bra _EEWrite

f 

; Program memory < 0x300000
; Config memory = 0x300000
; EEPROM data = 0xF00000
  

; ************************************************************** * 
; Function: VOID _PMRead()
; VOID _PMEraseWrite ()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of
; the source data.
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;These are the program memory read/write functions. Erase is available through control flags. An automatic erase option is also available.
; A write lock indicator is in place to ensure intentional write operations.
;Note: write operations must be on 8-byte boundaries and must be 8 bytes long. Also erase operations can only occur on 64-byte boundaries.
; ************************************************************ ** * * * * * * * * * * * * * * *



_PMEraseWrite:
  btfss   MODE_AUTO_ERASE
  bra _PMWrite
_PMErase:
  movf  TBLPTRL, W
  andlw b'00111111'
  bnz _PMWrite
_PMEraseJp1
  movlw b'10010100' 
  rcall   _StartWrite 
_PMWrite:
  btfsc   MODE_ERASE_ONLY


  bra _CANMain 

  movf  TBLPTRL, W
  andlw b'00000111'
  bnz _CANMain 
  movlw   0x08
  movwf WREG1

_PMWriteLp1         ; Load the holding registers
  movf  POSTINC0, W 
  movwf   TABLAT
  rcall  _UpdateChksum  ; Adjust the checksum
  tblwt*+
  decfsz   WREG1, F
  bra _PMWriteLp1

#ifdef MODE_SELF_VERIFY 
  movlw  0x08
  movwf   WREG1 
_PMWriteLp2
  tblrd*-     ; Point back into the block
  movf  POSTDEC0, W 
  decfsz   WREG1, F
  bra _PMWriteLp2
  movlw  b'10000100'  ; Setup writes
  rcall _StartWrite   ; Write the data
  movlw   0x08
  movwf   WREG1
_PMReadBackLp1
  tblrd*+     ; Test the data
  movf  TABLAT, W 
  xorwf   POSTINC0, W
  btfss STATUS, Z
  bsf ERR_VERIFY 
  decfsz  WREG1, F
  bra _PMReadBackLp1  ; Not finished then repeat
#else
  tblrd*-     ; Point back into the block
         ; Setup writes
  movlw   b'10000100'   ; Write the data
  rcall   _StartWrite   ; Return the pointer position
  tblrd*+
#endif

  bra _CANMain


; ************************************************************** * * * * * * * * * * * * * * *
 ; Function: VOID _CFGWrite()
; VOID _CFGRead()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of the source data. 
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;
; These are the Config memory read/write functions. Read is
; actually the same for standard program memory, so any read
; request is passed directly to _PMRead.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_CFGWrite

#ifdef MODE_SELF_VERIFY   ; Write to config area
  movf  INDF0, W    ; Load data
#else
  movf  POSTINC0, W
#endif
  movwf   TABLAT
  rcall   _UpdateChksum ; Adjust the checksum
  tblwt*      ; Write the data
  movlw b'11000100' 
  rcall   _StartWrite
  tblrd*+     ; Move the pointers and verify
#ifdef MODE_SELF_VERIFY 
  movf  TABLAT, W 
  xorwf   POSTINC0, W

#endif
  decfsz  WREG1, F
  bra _CFGWrite ; Not finished then repeat

  bra _CANMain 



; ************************************************************** * * * * * * * * * * * * * * * 
; Function: VOID _EERead()
; VOID _EEWrite()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of
 ;  the source data.
; Input:  None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;
; This is the EEDATA memory read/write functions.
;
; ************************************************************ ** * * * * * * * * * * * * * * *


_EEWrite:

#ifdef MODE_SELF_VERIFY
  movf  INDF0, W
#else
  movf  POSTINC0, W 
#endif

  movwf   EEDATA
  rcall   _UpdateChksum 
  movlw b'00000100' 
  rcall  _StartWrite

#ifdef MODE_SELF_VERIFY 
  clrf  EECON1
  bsf EECON1, RD
  movf  EEDATA, W 
  xorwf   POSTINC0, W
  btfss STATUS, Z
  bsf ERR_VERIFY
#endif

  infsnz   EEADR, F 
  incf  EEADRH, F 
  decfsz  WREG1, F
  bra _EEWrite


  bra _CANMain 
  

; Read the data

; Adjust EEDATA pointer
; Not finished then repeat
; Load data
; Adjust the checksum 
; Setup for EEData
; and write
; Read back the data ; verify the data ; and adjust pointer
; Adjust EEDATA pointer
; Not finished then repeat

; ************************************************************** * * * * * * * * * * * * * * *
; Function: VOID _CANSendAck()
; VOID _CANSendResponce ()
;
; PreCondition:TXB0 must be preloaded with the data.
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied. ;
; These routines are used for 'talking back' to the source. The
; _CANSendAck routine sends an empty message to indicate
; acknowledgement of a memory write operation. The
; _CANSendResponce is used to send data back to the source. ;
; ************************************************************ ** * * * * * * * * * * * * * * *



_CANSendMessage
  btfsc   TXB0CON,TXREQ 
  bra $ - 2
  movlw   CAN_TXB0SIDH 
  movwf   TXB0SIDH
  movlw   CAN_TXB0SIDL 
  movwf   TXB0SIDL
  movlw   CAN_TXB0EIDH 
  movwf   TXB0EIDH  

  movlw CAN_TXB0EIDL
  movwf TXB0EIDL
  bsf CANTX_CD_BIT
  btfss CAN_CD_BIT 
  bcf CANTX_CD_BIT
  bsf TXB0CON, TXREQ
      bra  _CANMain ; Setup the command bit

_CANSendOK        ;send OK message 
  movlw 1     ;a 1 is OK
  movwf TXB0D0
  movwf TXB0DLC
  bra   _CANSendMessage
  
_CANSendNOK       ;send not OK message
  clrf  TXB0D0    ;a 0 is not OK
  movlw 1
  movwf TXB0DLC
  bra   _CANSendMessage

_CANSendBoot
  movlw 2     ;2 is confirm boot mode
  movwf TXB0D0
  movlw 1
  movwf TXB0DLC
  bra   _CANSendMessage
    
; Start the transmission

;   End of bootloader 
  
  org RESET_VECT
loadadr
  nop
  goto  setup

  

  org 0x0808
  goto hpint
  
  org 0810h
myName  db  "LED64  "
  
  org 0x0818
  goto lpint
  
    ORG   0820h

nodeprm     db  MAN_NO, MINOR_VER, MODULE_ID, EVT_NUM, EVperEVT, NV_NUM 
      db  MAJOR_VER,NODEFLGS,CPU_TYPE,PB_CAN    ; Main parameters
            dw  RESET_VECT     ; Load address for module code above bootloader
            dw  0           ; Top 2 bytes of 32 bit address not used
            db  0,0,0,0,CPUM_MICROCHIP,BETA
sparprm     fill 0,prmcnt-$ ; Unused parameter space set to zero

PRMCOUNT    equ sparprm-nodeprm ; Number of parameter bytes implemented

             ORG 0838h

prmcnt      dw  PRMCOUNT    ; Number of parameters implemented
nodenam     dw  myName      ; Pointer to module type name
            dw  0 ; Top 2 bytes of 32 bit address not used


PRCKSUM     equ MAN_NO+MINOR_VER+MODULE_ID+EVT_NUM+EVperEVT+NV_NUM+MAJOR_VER+NODEFLGS+CPU_TYPE+PB_CAN+HIGH myName+LOW myName+HIGH loadadr+LOW loadadr+PRMCOUNT+CPUM_MICROCHIP+BETA

cksum       dw  PRCKSUM     ; Checksum of parameters
  
  
;*******************************************************************

    ORG   0840h     ;start of program
; 
;
;   high priority interrupt. Used for CAN receive and transmit error.

hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
;   movff PCLATH,PCH_tempH    ;save PCLATH
;   clrf  PCLATH
  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H

    
    
    movlw 8     ;for relocated code
    movwf PCLATH
    movf  TempCANSTAT,W     ;Jump table
    andlw B'00001110'
    addwf PCL,F     ;jump
    bra   back
    bra   errint      ;error interrupt
    bra   back
    bra   back
    bra   back
    bra   rxb1int     ;only receive interrupts used
    bra   rxb0int
    bra   back
    
rxb1int bcf   PIR3,RXB1IF   ;uses RB0 to RB1 rollover so may never use this
    
    lfsr  FSR0,Rx0con   ;
    
    goto  access
    
rxb0int bcf   PIR3,RXB0IF
    btfsc Datmode,1     ;setup mode?
    bra   setmode 
    lfsr  FSR0,Rx0con
    
    goto  access
    
    ;error routine here. Only acts on lost arbitration  
errint  movlb .15         ;change bank      
    btfss TXB1CON,TXLARB
    bra   errbak        ;not lost arb.
  
    movf  Latcount,F      ;is it already at zero?
    bz    errbak
    decfsz  Latcount,F
    bra   errbak
    bcf   TXB1CON,TXREQ
    movlw B'00111111'
    andwf TXB1SIDH,F      ;change priority
txagain bsf   TXB1CON,TXREQ   ;try again
          
errbak  movlb 0
    bcf   RXB0CON,RXFUL   ;ready for next
    bcf   RXB1CON,RXFUL 
    bra   back1

access  movf  CANCON,W        ;switch buffers
    andlw B'11110001'
    movwf CANCON
    movf  TempCANSTAT,W
    andlw B'00001110'
    iorwf CANCON
    lfsr  FSR1,RXB0CON  ;this is switched bank
load  movf  POSTINC1,W
    movwf POSTINC0
    movlw 0x6E      ;end of access buffer lo byte
    cpfseq  FSR1L
    bra   load
    bcf   RXB0CON,RXFUL
    
    btfsc Rx0dlc,RXRTR    ;is it RTR?
    bra   isRTR
;   btfsc Datmode,1     ;setup mode?
;   bra   setmode 
    movf  Rx0dlc,F
    bz    back      ;ignore zero length frames
;   btfss Rx0sidl,3   ;ignore extended frames
    bsf   Datmode,0   ;valid message frame  
    
back  bcf   RXB0CON,RXFUL ;ready for next
  
  
back1 clrf  PIR3      ;clear all flags
    movf  CANCON,W
    andlw B'11110001'
    iorwf TempCANCON,W
    
    movwf CANCON
;   movff PCH_tempH,PCLATH
    movff Fsr_temp0L,FSR0L    ;recover FSR0
    movff Fsr_temp0H,FSR0H

    movff Fsr_temp1L,FSR1L    ;recover FSR1
    movff Fsr_temp1H,FSR1H

    
    retfie  1       ;use shadow registers
    
isRTR btfsc Datmode,1   ;setup mode?
    bra   back      ;back
    btfss Mode,1      ;FLiM?
    bra   back
    movlb .15
isRTR1  btfsc TXB2CON,TXREQ
    bra   isRTR1
    bsf   TXB2CON,TXREQ ;send ID frame - preloaded in TXB2

    movlb 0
    bra   back
    
    
setmode tstfsz  RXB0DLC
    bra   back        ;only zero length frames for setup
    
    swapf RXB0SIDH,W      ;get ID into one byte
    rrcf  WREG
    andlw B'01111000'     ;mask
    movwf Temp
    swapf RXB0SIDL,W
    rrncf WREG
    andlw B'00000111'
    iorwf Temp,W
    movwf IDcount       ;has current incoming CAN_ID

    lfsr  FSR1,Enum0      ;set enum to table
enum_st clrf  Rolle       ;start of enum sequence
    bsf   Rolle,0
    movlw 8
enum_1  cpfsgt  IDcount
    bra   enum_2
    subwf IDcount,F     ;subtract 8
    incf  FSR1L       ;next table byte
    bra   enum_1
enum_2  dcfsnz  IDcount,F
    bra   enum_3
    rlncf Rolle,F
    bra   enum_2
enum_3  movf  Rolle,W
    iorwf INDF1,F

    bra   back    


;**************************************************************
;
;

; 
;   low priority interrupt. Used by  timer 1 overflow. Every 2 millisecs.
;   used for LED scan routine
; 

lpint ;retfie

    movwf W_tempL       ;save variables
    movff STATUS,St_tempL
;   movff BSR,Bsr_tempL

    movff FSR2L,Fsr_temp2L
    movff FSR2H,Fsr_temp2H
    movff FSR1L,Fsr_temp1L
    movff FSR1H,Fsr_temp1H
    movff FSR0L,Fsr_temp0L
    movff FSR0H,Fsr_temp0H

    clrf  PIR1        ;clear all timer flags
    movlw 0x6F        ;Timer 1 lo byte. (adjust if needed)
    movwf TMR1L       ;reset timer 1
    
    btfss Mode,1        ; FLiM mode?
    bra   doMatrix
    
    decfsz  TkCount       ; tick counter, wraps giving 1/2 sec count
    bra   doMatrix      ; j if not expired
    decfsz  FlCount       ; flash counter
    bra   doMatrix
    movff FlCtrl,FlCount    ; recharge counter
    bcf   FlCount,7     ; clear state flag
    btfss FlCtrl, 7     ; check for On or Off control
    bra   flashOff
    ; clear Off LEDs from matrix and add On Leds
    
    lfsr  FSR2, Matrix
    lfsr  FSR1, FlMatOff0
    movlw 8
    movwf Lcount3
clrOff
    comf  POSTINC1, w
    andwf POSTINC2
    decfsz  Lcount3
    bra   clrOff
    
    lfsr  FSR2, Matrix
    lfsr  FSR1, FlMatOn0
    movlw 8
    movwf Lcount3
setOn
    movf  POSTINC1,w
    iorwf POSTINC2
    decfsz  Lcount3
    bra   setOn 
    btg   FlCtrl, 7     ; toggle state contol bit
    bra   doMatrix
    
flashOff
    lfsr  FSR2, Matrix
    lfsr  FSR1, FlMatOn0
    movlw 8
    movwf Lcount3
clrOn
    comf  POSTINC1, w
    andwf POSTINC2
    decfsz  Lcount3
    bra   clrOn
    
    lfsr  FSR2, Matrix
    lfsr  FSR1, FlMatOff0
    movlw 8
    movwf Lcount3
setOff
    movf  POSTINC1,w
    iorwf POSTINC2
    decfsz  Lcount3
    bra   setOff  
    btg   FlCtrl, 7     ; toggle state contol bit
    
doMatrix    
    movlw 2
    movwf Lcount3
    incf  Row,W
    andlw B'00000011'     ;count 0 to 3
    movwf Row
    lfsr  2,Matrix
    rlncf Row,W
    addwf FSR2L
    
    incf  FSR2L       ;add 1
nxt_byt movff INDF2,Colout    ;ready to send hi byte of column
    movlw 8
    movwf Lcount2
    
col1  rlcf  Colout,F
    bc    one_out
    bcf   PORTB,5       ;serial data
    bra   clock
one_out bsf   PORTB,5 
clock bsf   PORTB,4       ;clock it
    bcf   PORTB,4
    decfsz  Lcount2
    bra   col1
    decf  Lcount3,F     ;second byte?
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
    
    
lpend
    movff Fsr_temp2L,FSR2L
    movff Fsr_temp2H,FSR2H
    movff Fsr_temp1L,FSR1L
    movff Fsr_temp1H,FSR1H
    movff Fsr_temp0L,FSR0L
    movff Fsr_temp0H,FSR0H
    
;   movff Bsr_tempL,BSR
    movf  W_tempL,W
    movff St_tempL,STATUS 
    retfie  
              

;*********************************************************************

; main waiting loop

main  btfsc Mode,1      ;is it SLiM?
    bra   mainf     ;no

mains             ;is SLiM

    btfss PIR2,TMR3IF   ;flash timer overflow?
    bra   nofl_s      ;no SLiM flash
    btg   PORTB,7     ;toggle green LED
    bcf   PIR2,TMR3IF
nofl_s  bra   noflash       ;main1
    
; here if FLiM mde

mainf btfss INTCON,TMR0IF   ;is it flash?
    bra   noflash
    btfss Datmode,2
    bra   nofl1
    
    btg   PORTB,6     ;flash yellow LED
    
nofl1 bcf   INTCON,TMR0IF
    btfss Datmode,3   ;running mode
    bra   noflash
    decfsz  Keepcnt     ;send keep alive?
    bra   noflash
    movlw .10
    movwf Keepcnt
    movlw 0x52
;   call  nnrel     ;send keep alive frame (works OK, turn off for now)

noflash btfsc S_PORT,S_BIT  ;setup button?
    bra   main3
    movlw .100
    movwf Count
    clrf  Count1
    clrf  Count2
wait  decfsz  Count2
    goto  wait
    btfss Datmode,2
    bra   wait2
    btfss INTCON,TMR0IF   ;is it flash?
    bra   wait2
    btg   PORTB,6     ;flash LED
    bcf   INTCON,TMR0IF
wait2 decfsz  Count1
    goto  wait
    btfsc S_PORT,S_BIT
    bra   main4     ;not held long enough
    decfsz  Count
    goto  wait
    btfss Mode,1      ;is it in FLiM?
    bra   go_FLiM
    clrf  Datmode     ;back to virgin

    bcf   PORTB,6     ;yellow off
    
    bsf   PORTB,7     ;Green LED on
    clrf  INTCON      ;interrupts off
    movlw 1
    movwf IDcount     ;back to start
    movlw Modstat
    movwf EEADR
    movlw   0
    call  eewrite     ;status to reset
    movlw 0x51      ;send node release frame
    call  nnrel
    clrf  NN_temph
    clrf  NN_templ
wait1 btfss S_PORT,S_BIT
    bra   wait1     ;wait till release
    call  ldely
    btfss S_PORT,S_BIT
    bra   wait1
  
    
    movlw LOW NodeID      ;put NN back to 0000
    movwf EEADR
    movlw 0
    call  eewrite
    incf  EEADR
    movlw 0
    call  eewrite 
    btfss Mode,1
    bra   main5       ;FLiM setup
    movlw Modstat
    movwf EEADR
    movlw 0
    call  eewrite       ;mode back to SLiM
    clrf  Datmode
    bcf   Mode,1
    bcf   PORTB,6
    bsf   PORTB,7       ;green LED on
  
    movlw B'11000000'
    movwf INTCON
    goto  main        ;

main5 movlw Modstat
    movwf EEADR
    movlw 1
    call  eewrite       ;mode to FLiM in EEPROM
    bsf   Mode,1        ;to FLiM
    call  self_en       ;self enumerate routine
    bcf   Datmode,1
    call  nnack       ;send request for NN
    bsf   Datmode,2

    bra   main1


main4 
    btfss Datmode,2
    bra   mset2
    bcf   Datmode,2
    bsf   PORTB,6     ;LED on
    movlw 0x52
    call  nnrel
    movlw Modstat
    movwf EEADR
    movlw B'00001000'
    movwf Datmode     ;normal
    call  eewrite
    bra   main3
    
mset2 bsf   Datmode,2
    call  self_en
    bcf   Datmode,1
    call  nnack
    bra   main1

main3 btfss Datmode,1   ;setup mode ?
    bra   main1
;   call  self_en

    bcf   Datmode,1   ;out of setup
    bsf   Datmode,2   ;wait for NN

  
;   
;   bsf   PORTB,7     ;on light
    bra   main1     ;continue normally

go_FLiM bsf   Datmode,1   ;FLiM setup mode
    bcf   PORTB,7     ;green off
    bra   wait1
    
    

; common to FLiM and SLiM   
  
  
main1 
    btfss Datmode,0   ;any new CAN frame received?
    bra   main
    
    bra   packet      ;yes

;********************************************************************

;   These are here as branch was too long

unset           ; 0x95 unlearn event in learn mode
    btfss Datmode,4
    bra   main2
    call  copyev
    bra   do_unlearn
    
readEV            ; 0xB2 read event variable in learn mode
    btfss Datmode,4
    bra   main2     ; prevent error message
    call  copyev      ; copy ev data to safe buffer
    bra   do_rdev     ; now do it

evns            ; 0x58 read number of stored events
    call  thisNN        
    sublw 0
    bnz   notNNx
    call  evns2
    bra   main2

reval           ; 0x9C read event variable
    call  thisNN    
    sublw 0
    bnz   notNNx
    movff Rx0d3,ENidx ; event index
    movff Rx0d4,EVidx ; event variable index
    call  evsend
    bra   main2
    
notNNx  goto  notNN

go_on_x goto  go_on

params              ; 0x10 read parameters in setup mode
    btfss Datmode,2   
    bra   main2
    call  parasend
    bra   main2

name
    btfss Datmode,2   ;only in setup mode
    bra   main2
    call  namesend
    bra   main2
    
doQnn
    movf  NN_temph,w    ;respond if NN is not zero
    addwf NN_templ,w
    btfss STATUS,Z
    call  whoami
    bra   main2
        
short
    clrf  Rx0d1
    clrf  Rx0d2
    bra   go_on 
    
readENi             ; 0x72  read event by index
    call  thisNN      
    sublw 0
    bnz   notNNx
    movff Rx0d3, ENidx  ; copy requested index
    call  enrdi
    bra   main2

enum  call  thisNN
    sublw 0
    bnz   notNN1
    call  self_en
    movlw 0x52
    call  nnrel     ;send confirm frame
    movlw B'00001000'   ;back to normal running
    movwf Datmode
    goto  main2
notNN1  goto  notNN
    
copyev    ; copy event data to safe buffer
    movff Rx0d1, ev0
    movff Rx0d2, ev1
    movff Rx0d3, ev2
    movff Rx0d4, ev3
    movff Rx0d5, EVidx    ; only used by learn and some read cmds
    movff Rx0d6, EVdata   ; only used by learn cmd
    return
    
    ;main packet handling is here

packet  movlw CMD_ON  ;only ON and OFF events supported
    subwf Rx0d0,W 
    bz    go_on_x
    movlw CMD_OFF
    subwf Rx0d0,W
    bz    go_on_x
    movlw SCMD_ON
    subwf Rx0d0,W
    bz    short
    movlw SCMD_OFF
    subwf Rx0d0,W
    bz    short
    movlw 0x5C      ;reboot
    subwf Rx0d0,W
    bz    reboot
    movlw 0x73
    subwf Rx0d0,W
    bz    para1a      ; read individual parameters
    btfss Mode,1      ;FLiM?
    bra   main2
    movlw 0x42      ;set NN on 0x42
    subwf Rx0d0,W
    bz    setNN
    movlw 0x0d      ; QNN
    subwf Rx0d0,w
    bz    doQnn
    movlw 0x10      
    subwf Rx0d0,W
    bz    params      ;read node parameters
    movlw 0x11
    subwf Rx0d0,w
    bz    name      ;read module name   
    movlw 0x53      ; set to learn mode on 0x53
    subwf Rx0d0,W
    bz    setlrn1   
    movlw 0x54      ; clear learn mode on 0x54
    subwf Rx0d0,W
    bz    notlrn1
    movlw 0x55      ; clear all events on 0x55
    subwf Rx0d0,W
    bz    clrens1
    movlw 0x56      ; read number of events left
    subwf Rx0d0,W
    bz    rden
    movlw 0xD2      ; is it set event?
    subwf Rx0d0,W
    bz    chklrn      ; do it
    movlw 0x95      ; is it unset event
    subwf Rx0d0,W     
    bz    unset1
    movlw 0xB2      ; read event variables
    subwf Rx0d0,W
    bz    readEV1
    
    movlw 0x57      ; is it read events
    subwf Rx0d0,W
    bz    readEN1
    movlw 0x72
    subwf Rx0d0,W
    bz    readENi     ; read event by index
    movlw 0x58
    subwf Rx0d0,W
    bz    evns1
    movlw 0x9C      ; read event variables by EN#
    subwf Rx0d0,W
    bz    reval1
    movlw 0x5D
    subwf Rx0d0,W
    bz    enum1
    movlw 0x75      ;force new CAN_ID
    subwf Rx0d0,W
    bz    newID1

    bra main2

enum1 goto  enum      ;jumps
newID1  goto  newID
clrens1 goto  clrens
notlrn1 goto  notlrn
chklrn1 goto  chklrn
rdENi_1 goto  readENi
setlrn1 goto  setlrn
unset1  goto  unset
readEN1 goto  readEN
readEV1 goto  readEV
evns1 goto  evns
reval1  goto  reval

reboot              ; 0x5C  reboot
    btfss Mode,1      ; FLiM mode?...
    bra   reboots     ; ... then check NN
    call  thisNN    
    sublw 0
    bnz   notNN
    
reboot1 
    movlw 0xFF
    movwf EEADR
    movlw 0xFF
    call  eewrite     ; set last EEPROM byte to 0xFF
    reset         ; software reset to bootloader
    
reboots
    movf  Rx0d1,w
    addwf Rx0d2,w
    bz    reboot1     ; NN is zero
    bra   notNN

para1a  btfss Mode,1
    bra   para1s      ; Slim mode
    call  thisNN      ;read parameter by index
    sublw 0
    bnz   notNN
    call  para1rd
    bra   main2
    
para1s  movf  Rx0d1,w
    addwf Rx0d2,w
    bnz   notNN
    call  para1rd
    bra   main2
    
chklrn              ; 0xD2 teach an event in learn mode
    btfss Datmode,4   ; learn mode?
    bra   main2
    call  copyev  
    bra   do_learn

main2
    bcf   Datmode,0 
    goto  main      ; loop
    
setNN             ; 0x42 set NN in setup mode
    btfss Datmode,2   ; in NN set mode?
    bra   main2     ; no
    call  putNN     ; put in NN
    bcf   Datmode,2
    bsf   Datmode,3
    movlw .10
    movwf Keepcnt     ; for keep alive
    movlw 0x52
    call  nnrel     ; confirm NN set
    bsf   LED_PORT,LED1 ; LED ON
    bcf   LED_PORT,LED2
    bra   main2

newID call  thisNN
    sublw 0
    bnz   notNN
    movff Rx0d3,IDcount

    call  here2       ;put in as if it was enumerated
    movlw 0x52
    call  nnrel       ;acknowledge new CAN_ID
    goto  main2
    
sendNN
    btfss Datmode,2   ; in NN set mode?
    bra   main2     ; no
    movlw 0x50      ; send back NN
    movwf Tx1d0
    movlw 3
    movwf Dlc
    call  sendTX
    bra   main2

rden              ; 0x56 read number of events available
    call  thisNN
    sublw 0
    bnz   notNN
    movlw LOW ENCount+1 ;read number of events available
    movwf EEADR
    call  eeread
    movwf Tx1d3
    movlw 0x70
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    bra   main2

setlrn              ; 0x53 set learn mode
    call  thisNN
    sublw 0
    bnz   notNN
    bsf   Datmode,4
    bsf   LED_PORT,LED1 ;LED on
    bra   main2

notlrn              ; 0x54 clear learn mode
    call  thisNN
    sublw 0
    bnz   notNN
    bcf   Datmode,4   ; clear learn flag
    bra   main2
    
clrens
    call  thisNN
    sublw 0
    bnz   notNN
    btfss Datmode,4
    bra   clrerr
    call  initevdata
    call  clrmtrx
    movlw 0x59
    call  nnrel
notNN
    bra   main2

readEN
    call  thisNN
    sublw 0
    bnz   notNN
    call  enread
    bra   main2
    


clrerr  movlw 2       ;not in learn mode
    goto  errmsg

go_on
    call  copyev      ; save event info
    btfss Mode,1      ;FLiM?
    bra   go_on_s     ; j if SLiM
    
go_on1  
    call  enmatch
    sublw 0
    bz    do_it
    bra   main2     ;not here

go_on_s
    call  getop     ; get the switches, updates EVdata and EVidx
    btfsc PORTC,LEARN
    bra   go_on_s1    ; j if learn not set
    btfsc PORTC,UNLEARN
    bra   learns
    call  unlearn
    bra   do_it
;   bra   main2
    
learns              ; learn event in SLiM mode
    call  learn
    sublw 0
    bnz   lrnerr
    btfss EVtemp,6    ; test POL flag
    bra   endlrns     ; j if POL not set
    movlw 8
    addwf EVidx     ; index of POL byte
    call  learn     ; update POL info
endlrns
    bra   do_it     ; try it
    
lrnerr  movlw 4
    goto  errmsg1
    
go_on_s1            ; learn not set, test unlearn
    btfsc PORTC,UNLEARN
    bra   go_on1      ; no switches set, so action event
    call  unlrnled    ; remove led from event
    bra   do_it
;   bra   main2
    
do_it 
    call  rdfbev25  ; read relevant event data
    lfsr  FSR0, flags0
    movff INDF0, EVflags
    btfss EVflags,2 ; test flash control bit
    bra   setupfl
    lfsr  FSR0, led00
    lfsr  FSR1, pol00
    lfsr  FSR2,Matrix
    movlw 8
    movwf Count
    btfsc Rx0d0,0   ; is it ON cmnd
    bra   offcmnd   ; j if OFF
    btfss EVflags, 1
    bra   main2   ; j if On event not actioned
    
nxtled
    movff POSTINC1, Temp  ; POL bits
    comf  Temp,w      ; invert
    andwf INDF2     ; turn off POL bits in matrix
    andwf POSTINC0,w    ; remove POL bits from data
    iorwf POSTINC2    ; and 'or' into matrix
    decfsz  Count
    bra   nxtled

    bra   main2
    
offcmnd btfss EVflags, 0
    bra   main2     ; j if Off event not actioned
offcmnd1
    movff POSTINC0, Temp  ; leds which may be ON
    comf  Temp,w      ; invert
    andwf INDF2     ; turn them off in matrix
    movf  POSTINC1,w    ; get POL bits
    iorwf POSTINC2    ; or into matrix
    decfsz  Count
    bra   offcmnd1
    bra   main2
    
setupfl 
    lfsr  FSR0, led00   ; event LEDs
    lfsr  FSR1, FlMatOn0  ; On flash matrix
    movlw 8
    movwf Count
    btfsc Rx0d0,0   ; is it ON cmnd
    bra   unsetfl 
      
    lfsr  FSR2, pol00
nxtonfl
    comf  POSTINC2,w  ; comp of pol bits
    andwf POSTINC0,w  ; remove pol bits from LEDs
    iorwf POSTINC1  ; or On LEDS into On matrix
    decfsz  Count
    bra   nxtonfl
    
    lfsr  FSR0, pol00
    lfsr  FSR1, FlMatOff0
    movlw 8
    movwf Count
nxtoffl
    movf  POSTINC0, w ; get pol bits
    iorwf POSTINC1  ; or into Off matrix
    decfsz  Count
    bra   nxtoffl
    bra   main2
    
unsetfl
    lfsr  FSR2, FlMatOff0 ;Off flash matrix
nxtunset
    comf  POSTINC0,w    ; comp of all LEDs
    andwf POSTINC1    ; remove LEDs from On matrix
    andwf POSTINC2    ; remove LEDs from Off matrix
    decfsz  Count
    bra   nxtunset
    
    ; now remove all LEDs from main matrix
    lfsr  FSR0, led00
    lfsr  FSR1, Matrix
    movlw 8
    movwf Count
unsetmx
    comf  POSTINC0, w ; comp of event LEDs
    andwf POSTINC1  ; remove from matrix
    decfsz  Count
    bra   unsetmx
    
    bra   main2
    
    
do_learn
    movf  EVidx,w     ; indexes start at 1
    bnz   do_lrn1
    bra   main2
do_lrn1
    decf  EVidx     ; index now zero based
    call  learn
    movwf Temp      ; save result
    movlw 0x59      ; send WRACK
    call  nnrel
    movf  Temp, w     ; recover result
    sublw 0       ; check if no space left
    bz    lrnend
    movlw 4
    goto  errmsg2     
lrnend  
;   goto  do_it     ; to prevent LEDs changing...
    goto  main2     ; ... j to main2
    
do_unlearn
    call  unlearn
    movlw 0x59      ; send WRACK
    call  nnrel
    bra   do_it
    
do_rdev
    tstfsz  EVidx
    bra   do_rdev1
rdeverr

    movlw 6
    call  errsub
    goto  main2
    
do_rdev1
    decf  EVidx     ; index starts at zero
    movlw EV_NUM
    cpfslt  EVidx
    bra   rdeverr
    call  readev
    goto  main2

l_out bcf   Datmode,4

l_out1  bcf   Datmode,6
l_out2  bcf   Datmode,0
    
    clrf  PCLATH
    goto  main2
    
;***************************************************************************
;   main setup routine
;*************************************************************************
setup lfsr  FSR0, 0
nextram clrf  POSTINC0
    btfss FSR0L, 7
    bra   nextram
    
    clrf  INTCON      ;no interrupts yet
    clrf  ADCON0      ;ADC is off
    movlw B'00001111'   ;set Port A to all digital for now
    movwf ADCON1
    movlw B'00111111'   ;Port A  is LED select (1 of 64)
    movwf TRISA     ;
    movlw B'00001001'   ;RB0 is PB, RB1 is /LE on serial MUX ,  RB2 = CANTX, RB3 = CANRX, 
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
    clrf  Datmode
    clrf  Latcount
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
    clrf  CCP1CON
    movlw B'10000001'   ;Timer 1 control.16 bit write
    movwf T1CON     ;Timer 1 is for output duration
    movlw 0xE0
    movwf TMR1H     ;set timer hi byte
    movlw B'10000100'
    movwf T0CON     ;set Timer 0 for LED flash
    clrf  Tx1con
    movlw B'00100011'
    movwf IPR3      ;high priority CAN RX  interrupts(for now)
    clrf  IPR1      ;all peripheral interrupts are low priority
    clrf  IPR2
    clrf  PIE2
    call  clrmtrx     ; clear the matrix
    
    movlw B'00000001'
    movwf IDcount     ;set at lowest value for starters
    clrf  INTCON2     ;enable port B pullups 
    clrf  INTCON3     ;just in case
    bsf   PIE1,TMR1IE   ;enable Timer 1 interrupts
    movlw B'00100011'   ;Rx0 and RX1 interrupt and Tx error
    movwf PIE3
    clrf  PIR1
    clrf  PIR2
    clrf  PIR3      ;clear all flags
    bcf   RXB0CON,RXFUL

      ;   test for setup mode
    clrf  Mode
    movlw Modstat     ;get setup status
    movwf EEADR
    call  eeread
    movwf Datmode
    sublw 0       ;not set yet
    bnz   setid
    movlw LOW FreeCh
    movwf EEADR
    call  eeread
    addlw 0
    bnz   slimset     ; j if free chain exists
    call  initevdata    ; clear event data flash and eeprom
    bra   slimset     ;wait for setup PB

setid bsf   Mode,1      ;flag FLiM
    call  newid_f     ;put ID into Tx1buf, TXB2 and ID number store
    
    movlw 1       ; set up flash control
    movwf FlCtrl
    movwf FlCount

    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   LED_PORT,LED2
    bsf   LED_PORT,LED1     ;Yellow LED on.
    bcf   Datmode,0
    goto  main

slimset bcf   Mode,1
    clrf  NN_temph
    clrf  NN_templ
    ;test for clear all events
    btfss PORTC,LEARN   ;ignore the clear if learn is set
    goto  seten
    btfss PORTC,UNLEARN
    call  initevdata      ;clear all events if unlearn is set during power up
seten
    bsf   PORTB,7     ;on LED
    bcf   PORTB,6
    movlw B'11000000'
    movwf INTCON      ;enable interrupts    

    goto  main

    


;**************************************************************************
;
; learn an event

learn   ; data is in ev(n), EVdata and EVidx
  call  enmatch
  sublw 0
  bnz   newev
  call  rdfbev      ; and fetch the data
  
  lfsr  FSR0, flags0
  movlw 0x20
  andwf evaddrl, w
  btfss STATUS, Z
  lfsr  FSR0, flags1
  movff INDF0, EVflags
  movlw 0xff      ; clear flags for now
  movwf INDF0
; movlw 0x07
; andwf EVflags,w
; sublw 0x07
; bz    lrn2
  btfsc EVflags, 2
  bra   lrn2      ; if not flash event
  
; bsf   EVflags, 2    ; clear flash flag bit
; movff EVflags, INDF0
  lfsr  FSR0,led00    ; point at led data in lower block
  movlw 0x20      ; check if ev is in lower or upper block
  andwf evaddrl,w
  btfss STATUS, Z   ; j if lower block
  lfsr  FSR0, led10   ; point at led data in upper block  
  lfsr  FSR1, FlMatOn0  
  lfsr  FSR2, FlMatOff0
  movlw 8
  movwf Count
lrn2b
  comf  POSTINC0, w   ; comp LEDs in event
  andwf POSTINC1    ; clear LEDs from On matrix
  andwf POSTINC2    ; clear LEDs from Off matrix
  decfsz  Count
  bra   lrn2b
  
lrn2
  lfsr  FSR0,led00    ; point at led data in lower block
  lfsr  FSR2,Matrix
  movlw 0x20      ; check if ev is in lower or upper block
  andwf evaddrl,w
  btfss STATUS, Z   ; j if lower block
  lfsr  FSR0, led10   ; point at led data in upper block
  btfsc EVidx,4     ; check if writing flags
  bra   lrn2a     ; j if yes
  btfsc EVidx,3     ; check if updating POL bit
  bra   lrn2a     ; j if yes
  movf  EVidx,w
  movff PLUSW0, EVtemp1 ; new data
  comf  EVtemp1
  movf  PLUSW2,w    ; get current matrix data
  andwf EVtemp1     ; clear leds in new data
  movf  EVidx,w
  movff EVtemp1,PLUSW2  ; update matrix - do_it will sort out correct leds
lrn2a
  btfss Mode,1      ; is it FLiM?
  bra   lrn2s     ; j if SLiM
  
  movf  EVidx,w
  movff EVdata, PLUSW0  ; in FLiM mode, just overwrite the existing EV
  bra   endlrn2 
  
  
      ; in SLiM mode, 'or' in the additional LED bit
      ; and clear the equivalent POL bit...
      ; ...if not updating POL data
lrn2s               
  movff EVdata, EVtemp1 
  movf  EVidx,w     ; index of led dat
  movf  PLUSW0,w    ; get led data
  iorwf EVtemp1     ; or in new led bit
  movf  EVidx,w
  movff EVtemp1, PLUSW0 ; and write back
  btfsc EVidx,3     ; check if EVidx is pointing at POL byte...
  bra   endlrn2     ;... j if it is
  movff EVdata, EVtemp1 ; recopy data...
  comf  EVtemp1     ;... and deal with POL bit
  addlw 8       ; POL byte
  movf  PLUSW0, w   ; read POL byte
  andwf EVtemp1     ; remove POL bit
  movf  EVidx,w     ; index of...
  addlw 8       ; ... POL byte
  movff EVtemp1, PLUSW0 ; write POL byte back
  
endlrn2
  call  wrfbev      ; update flash ram
  retlw 0
  
newev   
  ; check remaining space
  movlw LOW ENCount+1
  movwf EEADR
  call  eeread
  sublw 0
  bnz   lrn1
  retlw 1   ; no space left
  
lrn1  
  movlw LOW FreeCh  ; get free chain pointer
  movwf EEADR
  call  eeread
  movwf evaddrh
  movlw LOW FreeCh + 1
  movwf EEADR
  call  eeread
  movwf evaddrl
  
  ; now check and update hash table pointer
  tstfsz  htaddrh
  bra   htok
  bra   newev2    ;j if no hash table for this event

htok  ; hash table pointer is valid so read data
  movlw 0x20
  andwf htaddrl,w
  movwf CountFb1  ; first or second block flag
  call  rdfbht    ; read from hash table address
  tstfsz  CountFb1  ; j if first block
  bra   newev0
  movff evaddrh, prev0h
  movff evaddrl, prev0l
  bra   newev1
newev0
  movff evaddrh, prev1h
  movff evaddrl, prev1l
newev1
  call  wrfbht    ; write back using hash table address
  
newev2    ; read free chain data block
  movlw 0x20
  andwf evaddrl,w ; first or second block?
  movwf CountFb1
  call  rdfbev    ; read free chain entry 
  
  ; now update FreeCh with next event pointer from event data
  movlw LOW FreeCh
  movwf EEADR
  tstfsz  CountFb1  ; j if second block
  bra   newev3
  movff next0h, Temp
  bra   newev4
newev3
  movff next1h,Temp
newev4
  movf  Temp,w
  call  eewrite
  
  movlw LOW FreeCh+1
  movwf EEADR
  tstfsz  CountFb1  ; j if second block
  bra   newev5
  movff next0l, Temp
  bra   newev6
newev5
  movff next1l,Temp
newev6
  movf  Temp,w
  call  eewrite 
  
  ; write new event address to hash table
  movff htidx, EEADR
  movf  evaddrh,w
  call  eewrite
  incf  EEADR
  movf  evaddrl,w
  call  eewrite
  
  clrf  Temp
  tstfsz  CountFb1  
  bra   newev7    ; j if second block
  movff htaddrh, next0h   ; copy previous head of chain address
  movff htaddrl, next0l
  movff Temp,prev0h     ; clear previous ptr
  movff Temp,prev0l
  movff ev0,evt00
  movff ev1,evt01
  movff ev2,evt02
  movff ev3,evt03
  lfsr  FSR0,led00
  bra   newev8
newev7
  movff htaddrh, next1h   ; copy previous head of chain address
  movff htaddrl, next1l
  movff Temp,prev1h     ; clear previous ptr
  movff Temp,prev1l
  movff ev0,evt10     ; copy event data
  movff ev1,evt11
  movff ev2,evt12
  movff ev3,evt13
  lfsr  FSR0,led10
newev8
  movlw .16
  movwf CountFb0
  
newev9
  clrf  POSTINC0      ; clear event data
  decfsz  CountFb0
  bra   newev9
  
  movf  hnum,w        ; hash number of event
  addlw LOW hashnum     ; update count of events in this hash
  movwf EEADR
  call  eeread
  incf  WREG
  call  eewrite
  
  movlw LOW ENCount
  movwf EEADR
  call  eeread
  addlw 1
  call  eewrite
  incf  EEADR
  call  eeread
  decf  WREG
  call  eewrite
  bra   lrn2

;**************************************************************************
;
; unlearn an event

unlearn     ; on entry the target event number must be in ev0-3
  call  enmatch
  sublw 0
  bz    unl1      ; j if event found
  return
  
unl1
  movlw LOW FreeCh    ; get free chain address
  movwf EEADR
  call  eeread
  movwf freadrh
  movlw LOW FreeCh+1
  movwf EEADR
  call  eeread
  movwf freadrl
  
  call  rdfbev        ; read entry
  lfsr  FSR2, Matrix
  movlw 0x20
  andwf evaddrl,w
  bnz   unl2        ; j if in upper block
  lfsr  FSR0,led00
  call  remove
  movff next0h, nextadrh  ; save chain pointers
  movff next0l, nextadrl
  movff prev0h, prevadrh
  movff prev0l, prevadrl
  movff freadrh, next0h   ; set next ptr to current free chain
  movff freadrl, next0l
  bra   unl3
unl2
  lfsr  FSR0,led10
  call  remove
  movff next1h, nextadrh  ; save chain pointers
  movff next1l, nextadrl
  movff prev1h, prevadrh
  movff prev1l, prevadrl
  movff freadrh, next1h   ; set next ptr to current free chain
  movff freadrl, next1l
unl3
  movlw LOW FreeCh    ; update free chain address to current entry
  movwf EEADR
  movf  evaddrh,w
  call  eewrite
  movlw LOW FreeCh+1
  movwf EEADR
  movf  evaddrl,w
  call  eewrite
  
  call  wrfbev      ; write freed event data back
  
  tstfsz  prevadrh    ; check if previous link id valid
  bra   unl4      ; j if it is
  bra   unl6a

unl4            ; read and update previous event entry
  movff prevadrh, evaddrh
  movff prevadrl, evaddrl
  call  rdfbev
  movlw 0x20
  andwf evaddrl,w
  bnz   unl5      ; j if upper block
  movff nextadrh, next0h
  movff nextadrl, next0l
  bra   unl6
unl5
  movff nextadrh, next1h
  movff nextadrl, next1l
unl6
  call  wrfbev      ; write back with updated next pointer
  bra   unl7
  
unl6a           ;must write next ptr to hash table
  movff htidx, EEADR
  movf  nextadrh,w
  call  eewrite
  incf  EEADR
  movf  nextadrl,w
  call  eewrite

unl7
  tstfsz  nextadrh    ; check if next link is valid
  bra   unl8      ; j if it is
  bra   unl11     ; no more to do
  
unl8
  movff nextadrh, evaddrh
  movff nextadrl, evaddrl
  call  rdfbev
  movlw 0x20
  andwf evaddrl, w
  bnz   unl9
  movff prevadrh, prev0h
  movff prevadrl, prev0l
  bra   unl10
unl9
  movff prevadrh, prev1h
  movff prevadrl, prev1l
unl10
  call  wrfbev

unl11
  movf  hnum, w       ; hash number of event
  addlw LOW hashnum     ; update number of events for this hash
  movwf EEADR
  call  eeread
  decf  WREG
  call  eewrite

  movlw LOW ENCount     ; update free space and no of events
  movwf EEADR
  call  eeread
  decf  WREG
  call  eewrite
  incf  EEADR
  call  eeread
  addlw 1
  call  eewrite 
  return
  
remove      ; remove all leds for event from Matrix
  movlw 8
  movwf Count3
nxtrem
  movff POSTINC0,Temp   ; get EV data
  comf  Temp,w
  andwf POSTINC2      ; remove from matrix
  decf  Count3
  bnz   nxtrem
  return
  
;************************************************************************** 
;
; remove an LED from event - Slim mode only

unlrnled
  call  enmatch
  sublw 0
  bz    unled1      ; j if event found
  return

unled1
  call  rdfbev      ; get the data
  lfsr  FSR0,led00    ; point at led data in lower block
  lfsr  FSR2,Matrix   ; point at matrix
  movlw 0x20      ; check if ev is in lower or upper block
  andwf evaddrl,w
  btfss STATUS, Z   ; j if lower block
  lfsr  FSR0, led10   ; point at led data in upper block
  movf  EVidx,w
  movff EVdata, EVtemp1 ; led to remove
  comf  EVtemp1
  movf  PLUSW2,w    ; current matrix data
  andwf EVtemp1     ; remove led from matrix data...
  movf  EVidx,w
  movff EVtemp1, PLUSW2 ; ...and write back to matrix
  movff EVdata,EVtemp1  ; get data again
  comf  EVtemp1
  movf  PLUSW0,w    ; get led data
  andwf EVtemp1     ; remove led bit
  movf  EVidx,w
  movff EVtemp1, PLUSW0 ; and write back
  movff EVdata, EVtemp1 ; recopy data...
  comf  EVtemp1     ;... and deal with POL bit
  addlw 8       ; POL byte
  movf  PLUSW0, w   ; read POL byte
  andwf EVtemp1     ; remove POL bit
  movf  EVidx,w     ; index of...
  addlw 8       ; ... POL byte
  movff EVtemp1, PLUSW0 ; write POL byte back
  call  wrfbev      ; update flash ram
  return

;**************************************************************************
;
; read event variable
  
readev
  call  enmatch
  sublw 0
  bz    readev1     ; j if event found
  clrf  EVdata
  clrf  EVidx
  bra   endrdev
  
readev1
  call  rdfbev25    ; get 24 bytes of event data
  lfsr  FSR0, led00   ; point at EVs
  movf  EVidx,w
  movf  PLUSW0,w    ; get the byte
  movwf EVdata
  incf  EVidx     ; put back to 1 based
  
endrdev
  movlw 0xD3
  movwf Tx1d0
  movff evt00,Tx1d1
  movff evt01,Tx1d2
  movff evt02,Tx1d3
  movff evt03,Tx1d4
  movff EVidx,Tx1d5
  movff EVdata,Tx1d6
  movlw 7
  movwf Dlc
  call  sendTXa
  return

;**************************************************************************
;
; clear all events and associted data structures

initevdata    ; clear all event info        
  movlw LOW ENCount   ; clear number of events
  movwf EEADR
  movlw 0
  call  eewrite     ; no events set up
  movlw 0xff
  incf  EEADR
  call  eewrite     ; set up no of free events

  movlw .128
  movwf Count 
  movlw LOW hashtab
  movwf EEADR
  
nextht            ; clear hashtable
  movlw 0
  call  eewrite
  incf  EEADR
  decfsz  Count
  bra   nextht
  
  movlw .64
  movwf Count
  movlw LOW hashnum
  movwf EEADR
  
nexthn            ; clear hash table count
  movlw 0
  call  eewrite
  incf  EEADR
  decfsz  Count
  bra   nexthn
  
  call  clrev   ; erase all event data
  movlw LOW FreeCh  ; set up free chain pointer in ROM
  movwf EEADR
  movlw HIGH evdata
  movwf evaddrh
  call  eewrite
  movlw LOW FreeCh + 1
  movwf EEADR
  movlw 0
  movwf evaddrl
  call  eewrite
  call  clrfb
  clrf  prevadrh
  clrf  prevadrl
  
  movlw .128
  movwf Count   ; loop 128 times
nxtev
  movlw .32
  addwf evaddrl,w
  movwf Temp
  movff Temp, next0l
  movlw 0
  addwfc  evaddrh,w
  movwf Temp
  movff Temp,next0h
  movff prevadrh, prev0h
  movff prevadrl, prev0l
  movff evaddrh, prevadrh
  movff evaddrl, prevadrl
  movlw 1
  subwf Count, W
  bnz   nxtaddr
  
  clrf  Temp
  movff Temp,next1h
  movff Temp,next1l
  movff prevadrh,prev1h
  movff prevadrl, prev1l
  bra   writefbs
nxtaddr 
  movlw .64
  addwf evaddrl,w
  movwf Temp
  movff Temp, next1l
  movlw 0
  addwfc  evaddrh,w
  movwf Temp
  movff Temp, next1h
  movff prevadrh,prev1h
  movff prevadrl,prev1l
  movff next0h, prevadrh
  movff next0l, prevadrl
writefbs
  call  wrfbev
  movlw .64
  addwf evaddrl
  movlw 0
  addwfc  evaddrh
  decfsz  Count
  bra   nxtev
  return
  
  
;**************************************************************************
;
; routines for reading the event data

rdfbht    ; on entry htaddrh and htaddrl must point to valid entry
  movlw 0xc0
  andwf htaddrl,w   ; align to 64 byte boundary
  movwf TBLPTRL
  movf  htaddrh,w
  movwf TBLPTRH
  movlw .64
  movwf CountFb0
  bra   rdfb
  
rdfbev    ; On entry evaddrh and evaddrl must point to the correct entry
  movlw 0xc0
  andwf evaddrl,w   ; align to 64 byte boundary
  movwf TBLPTRL
  movf  evaddrh,w
  movwf TBLPTRH
  clrf  TBLPTRU
  movlw .64
  movwf CountFb0
rdfb
  clrf  TBLPTRU
  lfsr  FSR0,evt00
nxtfbrd
  tblrd*+
  movf  TABLAT, w
  movwf POSTINC0
  decfsz  CountFb0
  bra   nxtfbrd
  return
  
rdfbev25    ; read first 25 bytes of event data, on entry evaddrh and evaddr must be valid
  clrf  TBLPTRU
  movff evaddrh, TBLPTRH
  movff evaddrl, TBLPTRL
  movlw 25
  movwf CountFb0
  bra   rdfb
  
rdfbev8   ; read first 8 bytes of event data, on entry evaddrh and evaddr must be valid
  clrf  TBLPTRU
  movff evaddrh, TBLPTRH
  movff evaddrl, TBLPTRL
  movlw 8
  movwf CountFb0
  bra   rdfb

;**************************************************************************
;
; routine for finding an event - returns 0 on success, 1 on failure

enmatch   ;on exit if success w = 0 and evaddrh/evaddrl point at led data
  movlw 0x3f
  andwf ev1,w   ;ls 6 bits as hash
  movwf Temp
  movwf hnum
  rlncf Temp    ; times 2 as hash tab is 2 bytes per entry  
  movlw LOW hashtab
  addwf Temp, w
  movwf htidx   ; save EEPROM offset of hash tab entry
  movwf EEADR
  call  eeread
  movwf evaddrh
  movwf htaddrh   ; save hash table point ms
  incf  EEADR
  call  eeread
  movwf evaddrl
  movwf htaddrl   ; save hash table pointer ls
nextev
  tstfsz  evaddrh   ;is it zero?, high address cannot be zero if valid
  bra   addrok
  retlw 1     ; not found
  
addrok
  movf  evaddrl,w
  movwf TBLPTRL
  movf  evaddrh,w
  movwf TBLPTRH
  clrf  TBLPTRU
  
  clrf  Match
  tblrd*+
  movf  TABLAT,W
  cpfseq  ev0
  incf  Match
  tblrd*+
  movf  TABLAT,W
  cpfseq  ev1
  incf  Match
  tblrd*+
  movf  TABLAT,W
  cpfseq  ev2
  incf  Match
  tblrd*+
  movf  TABLAT,W
  cpfseq  ev3
  incf  Match
  tstfsz  Match
  bra   no_match
  retlw 0
  
no_match    ;get link address to next event
  tblrd*+
  movf  TABLAT,w
  movwf evaddrh
  tblrd*+
  movf  TABLAT,w
  movwf evaddrl
  bra   nextev
  
;**************************************************************************
;
; EEPROM routines

eeread    ; On entry, EEADR must contain the EPROM address, W contains the data on exit
  bcf   EECON1,EEPGD
  bcf   EECON1,CFGS
  bsf   EECON1,RD
  movf  EEDATA, W
  return
  
eewrite   ; On entry, EEADR must contain the address and W the data
  movwf EEDATA    
  bcf   EECON1,EEPGD
  bcf   EECON1,CFGS
  bsf   EECON1,WREN
    
  clrf  INTCON  ;disable interrupts
  movlw 0x55
  movwf EECON2
  movlw 0xAA
  movwf EECON2
  bsf   EECON1,WR
eetest
  btfsc EECON1,WR
  bra   eetest
  bcf   PIR2,EEIF
  bcf   EECON1,WREN
  movlw B'11000000'
  movwf INTCON    ;reenable interrupts
    
  return

;**************************************************************************
;
; routines for clearing flash ram
  
clrev   ; erase all of the event data area in flash
  movlw .128
  movwf Count   ; do 128 * 64 blockd
  movlw 0
  movwf TBLPTRL
  movlw high evdata
  movwf TBLPTRH
  clrf  TBLPTRU
nxtclr
  call  clrflsh
  decfsz  Count
  bra   nxtblk
  return
nxtblk
  movlw .64
  addwf TBLPTRL,F
  movlw 0
  addwfc  TBLPTRH
  bra   nxtclr
  
clrflsh   ; clears 64 bytes of flash ram, TBLPTR must point to target ram
  bsf   EECON1,EEPGD    ;set up for erase
  bcf   EECON1,CFGS
  bsf   EECON1,WREN
  bsf   EECON1,FREE
  clrf  INTCON  ;disable interrupts
  movlw 0x55
  movwf EECON2
  movlw 0xAA
  movwf EECON2
  bsf   EECON1,WR     ;erase
  nop
  nop
  nop
  movlw B'11000000'
  movwf INTCON    ;reenable interrupts

  return
  
clrfb   ; clears the 64 bytes data ram for ev data
  lfsr  FSR0, evt00
  movlw .64
  movwf CountFb0
nxtone
  clrf  POSTINC0
  decfsz  CountFb0
  bra   nxtone
  return

;**************************************************************************
;
; routines fot writing flash
;
; erases flash ram and the writes data back
; writes the 64 bytes of data ram back to flash ram
; HTADDRH and EHTADDRL must contain the flash address on entry

wrfbht
  movlw 0xc0
  andwf htaddrl,w   ; align to 64 byte boundary
  movwf TBLPTRL
  movf  htaddrh, W
  movwf TBLPTRH
  bra   wrfb
  
  ; erases flash ram and the writes data back
  ; writes the 64 bytes of data ram back to flash ram
  ; EVADDRH and EVADDRL must contain the flash address on entry
wrfbev
  movlw 0xc0
  andwf evaddrl,w   ; align to 64 byte boundary
  movwf TBLPTRL
  movf  evaddrh, W
  movwf TBLPTRH
  
wrfb
  clrf  TBLPTRU
  call  clrflsh
  lfsr  FSR0, evt00
  movlw 2
  movwf CountFb1
nxt32
  movlw .25     ; only need to write 25 bytes
  movwf CountFb0
nxtfb
  movf  POSTINC0, W
  movwf TABLAT
  tblwt*+
  decfsz  CountFb0
  bra   nxtfb
  call  wrflsh
  decfsz  CountFb1
  bra   dofb32
  return
dofb32
  movlw 7     ;must add up to 32 with no of bytes written
  addwf TBLPTRL
  lfsr  FSR0, evt10
  bra   nxt32
  
wrflsh    ; write upto 32 bytes of flash  
  bsf   EECON1, EEPGD
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
  
;****************************************************************************
;   start of subroutines    

;*****************************************************************************
;
;   shuffle for standard ID. Puts 7 bit ID into IDtemph and IDtempl for CAN frame
shuffle movff CanID_tmp,IDtempl   ;get 7 bit ID
    swapf IDtempl,F
    rlncf IDtempl,W
    andlw B'11100000'
    movwf IDtempl         ;has sidl
    movff CanID_tmp,IDtemph
    rrncf IDtemph,F
    rrncf IDtemph,F
    rrncf IDtemph,W
    andlw B'00001111'
    movwf IDtemph         ;has sidh
    return

;*********************************************************************************

;   reverse shuffle for incoming ID. sidh and sidl into one byte.

shuffin movff Rx0sidl,IDtempl
    swapf IDtempl,F
    rrncf IDtempl,W
    andlw B'00000111'
    movwf IDtempl
    movff Rx0sidh,IDtemph
    rlncf IDtemph,F
    rlncf IDtemph,F
    rlncf IDtemph,W
    andlw B'01111000'
    iorwf IDtempl,W     ;returns with ID in W
    return

;*********************************************************************************
;
clrmtrx
    lfsr  FSR2, Matrix    ;clear LED matrix
    lfsr  FSR1, FlMatOn0
    lfsr  FSR0, FlMatOff0
matclr  clrf  POSTINC2
    clrf  POSTINC1
    clrf  POSTINC0
    movlw Matrix + 8
    subwf FSR2L,W
    bnz   matclr
    return
      
;***************************************************************


;*************************************************************************

;   Send contents of Tx1 buffer via CAN TXB1

sendTX1 lfsr  FSR0,Tx1con
    lfsr  FSR1,TXB1CON
    
    movlb .15       ;check for buffer access
ldTX2 btfsc TXB1CON,TXREQ ; Tx buffer available...?
    bra   ldTX2     ;... not yet
    movlb 0
    
ldTX1 movf  POSTINC0,W
    movwf POSTINC1  ;load TXB1
    movlw Tx1d7+1
    cpfseq  FSR0L
    bra   ldTX1

    
    movlb .15       ;bank 15
tx1test btfsc TXB1CON,TXREQ ;test if clear to send
    bra   tx1test
    bsf   TXB1CON,TXREQ ;OK so send
    
tx1done movlb 0       ;bank 0
    return          ;successful send


;*********************************************************************
;   send a CAN frame
;   entry at sendTX puts the current NN in the frame - for producer events
;   entry at sendTXa neeeds Tx1d1 and Tx1d2 setting first
;   Latcount is the number of CAN send retries before priority is increased
;   the CAN-ID is pre-loaded in the Tx1 buffer 
;   Dlc must be loaded by calling source to the data length value
    
sendTX
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2

sendTXa
    movf  Dlc,W       ;get data length
    movwf Tx1dlc
    movlw B'00001111'   ;clear old priority
    andwf Tx1sidh,F
    movlw B'10110000'
    iorwf Tx1sidh     ;low priority
    movlw .10
    movwf Latcount
    call  sendTX1     ;send frame
    return      

;**************************************************************************

;   check if command is for this node

thisNN  movf  NN_temph,W
    subwf Rx0d1,W
    bnz   not_NN
    movf  NN_templ,W
    subwf Rx0d2,W
    bnz   not_NN
    retlw   0     ;returns 0 if match
not_NN  retlw 1
          
;**************************************************************************

nnack movlw 0x50      ;request frame for new NN or ack if not virgin
nnrel movwf Tx1d0
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    movlw 3
    movwf Dlc
    call  sendTX
    return

;*********************************************************************
;   put in NN from command

putNN movff Rx0d1,NN_temph
    movff Rx0d2,NN_templ
    movlw LOW NodeID
    movwf EEADR
    movf  Rx0d1,W
    call  eewrite
    incf  EEADR
    movf  Rx0d2,W
    call  eewrite
    movlw Modstat
    movwf EEADR
    movlw B'00001000'   ;Module status has NN set
    call  eewrite
    return

;***************************************************************************  

newid_f movlw LOW CANid     ;put in stored ID. FLiM mode
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf CanID_tmp     
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlw LOW NodeID
    movwf EEADR
    call  eeread
    movwf NN_temph      ;get stored NN
    incf  EEADR
    call  eeread
    movwf NN_templ  
    
    movlb .15       ;put ID into TXB2 for enumeration response to RTR
new_1f  btfsc TXB2CON,TXREQ
    bra   new_1f
    clrf  TXB2SIDH
    movf  IDtemph,W
    movwf TXB2SIDH
    movf  IDtempl,W
    movwf TXB2SIDL
    movlw 0xB0
    iorwf TXB2SIDH    ;set priority
    clrf  TXB2DLC     ;no data, no RTR
    movlb 0
    
    return
    
;**************************************************************************
; SLim mode only, returns the LED bit in EVdata and the index in EVidx
; EVtemp contains the switch data, bit 6 is the POL bit, bit 7 is unused
;
getop     ; read switches
    movf  PORTA,W   ;get switch
    andlw B'00111111'   ;mask
    movwf EVtemp
    movlw B'00000111'
    andwf EVtemp,W
    movwf EVidx     ; temp location
    movlw 1
    movwf EVdata      ;rolling bit
movbit  movf  EVidx,F     ;is it zero
    bz    gotbit
    rlncf EVdata,F    ;roll bit
    decf  EVidx,F
    bra   movbit
gotbit  movlw B'00111000'
    andwf EVtemp,W
    movwf EVidx
    rrncf EVidx,F
    rrncf EVidx,F
    rrncf EVidx,F     ; index of EVdata in event data
    btfss PORTC,POL   ; pol bit
    bsf   EVtemp,6
    return
    
;*********************************************************************
  
;   a delay routine
      
dely  movlw .10
    movwf Count1
dely2 clrf  Count
dely1 decfsz  Count,F
    goto  dely1
    decfsz  Count1
    bra   dely2
    return  

;****************************************************************

;   longer delay

ldely movlw .100
    movwf Count2
ldely1  call  dely
    decfsz  Count2
    bra   ldely1
    
    return
;*************************************************************************

;   read back all events in sequence

enread  clrf  Temp
    movlw LOW ENCount
    movwf EEADR
    call  eeread
    sublw 0
    bz    noens   ;no events set
    
    clrf  Tx1d7   ; first event
    movlw .64     ; hash table size
    movwf ENcount 
    movlw LOW hashtab
    movwf EEADR
nxtht
    call  eeread
    movwf evaddrh
    incf  EEADR
    call  eeread
    movwf evaddrl
nxten
    tstfsz  evaddrh   ; check for valid entry
    bra   evaddrok
nxthtab
    decf  ENcount
    bz    lasten
    incf  EEADR
    bra   nxtht
    
evaddrok          ; ht address is valid
    call  rdfbev8   ; read 8 bytes from event info
    incf  Tx1d7
    movff evt00, Tx1d3
    movff evt01, Tx1d4
    movff evt02, Tx1d5
    movff evt03, Tx1d6
    
    movlw 0xF2
    movwf Tx1d0
    movlw 8
    movwf Dlc
    call  sendTX
    call  dely
    call  dely
    
    movff next0h, evaddrh
    movff next0l, evaddrl
    bra   nxten

noens clrf  Rx0d3
    bra   noens1
  
  
lasten  return  

;*************************************************************************
;
; findEN - finds the event by its index

findEN          ; ENidx must be valid and in range
    clrf  hnum
    clrf  ENcount
    clrf  ENcount1
findloop
    movf  hnum,w
    addlw LOW hashnum
    movwf EEADR
    call  eeread
    addlw 0
    bz    nxtfnd
    addwf ENcount1
    movf  ENcount1,w
    cpfslt  ENidx   ;skip if ENidx < ENcount1
    bra   nxtfnd
    bra   htfound
nxtfnd
    movff ENcount1, ENcount
    incf  hnum
    bra   findloop
htfound
    rlncf hnum,w
    addlw LOW hashtab
    movwf EEADR
    call  eeread
    movwf evaddrh
    incf  EEADR
    call  eeread
    movwf evaddrl
nxtEN
    movf  ENidx,w
    cpfslt  ENcount
    return
    
nxtEN1
    incf  ENcount
    call  rdfbev8
    movff next0h, evaddrh
    movff next0l, evaddrl
    bra   nxtEN
    
;*************************************************************************

; send individual event by index

enrdi movlw LOW ENCount ; no of events set
    movwf EEADR
    call  eeread
    movwf ENtemp1
    bz    noens1    ;no events set
    
    movf  ENidx,w   ; index starts at 1
    bz    noens1
    
    decf  ENidx   ; make zero based for lookup
    movlw LOW ENCount
    movwf EEADR
    call  eeread    ; read no of events
    cpfslt  ENidx   ; required index is in range
    bra   noens1
    
    call  findEN
    call  rdfbev8   ; get event data
    
    movff evt00, Tx1d3
    movff evt01, Tx1d4
    movff evt02, Tx1d5
    movff evt03, Tx1d6
    incf  ENidx
    movff ENidx, Tx1d7
    movlw 0xF2
    movwf Tx1d0
    movlw 8
    movwf Dlc
    call  sendTX
    return
    
noens1  movlw 7       ;no events set
    call  errsub
    return

;************************************************************************
    
;   send number of events

evns2 movlw LOW ENCount
    movwf EEADR
    call  eeread
    movwf Tx1d3
    movlw 0x74
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    return

;***********************************************************

;   send EVs by reference to EN index

evsend
    movlw LOW ENCount ; no of events set
    movwf EEADR
    call  eeread
    sublw 0
    bz    noens1    ;no events set
    
    movf  ENidx,w   ; index starts at 1
    bz    noens1
    
    decf  ENidx   ; make zero based for lookup
    movlw LOW ENCount
    movwf EEADR
    call  eeread    ; read no of events
    cpfslt  ENidx   ; required index is in range
    bra   noens1

    movf  EVidx,w   ; index starts at 1
    bz    notEV
    decf  EVidx
    movlw EV_NUM
    cpfslt  EVidx   ; skip if in range
    bra   notEV
    
    call  findEN
    
    call  rdfbev25  ; read event data
    lfsr  FSR0,led00
    movf  EVidx,w
    movff PLUSW0, Tx1d5
    incf  EVidx   ; make 1 based again...
    incf  ENidx   ; ... ditto
    movlw 0xB5
    movwf Tx1d0
    movff ENidx,Tx1d3
    movff EVidx,Tx1d4
    movlw 6
    movwf Dlc
    call  sendTX
    return

notEV movlw 6   ;invalid EV index
    call  errsub
    return
    

;**************************************************************************
;   send node parameter bytes (7 maximum)

parasend  
    movlw 0xEF
    movwf Tx1d0
    movlw LOW nodeprm
    movwf TBLPTRL
    movlw 8
    movwf TBLPTRH   ;relocated code
    lfsr  FSR0,Tx1d1
    movlw 7
    movwf Count
    bsf   EECON1,EEPGD
    
para1 tblrd*+
    movff TABLAT,POSTINC0
    decfsz  Count
    bra   para1
    bcf   EECON1,EEPGD  
    movlw 8
    movwf Dlc
    call  sendTXa
    return

;**************************************************************************
;   send module name - 7 bytes

namesend  
    movlw 0xE2
    movwf Tx1d0
    movlw LOW myName
    movwf TBLPTRL
    movlw HIGH myName
    movwf TBLPTRH   ;relocated code
    lfsr  FSR0,Tx1d1
    movlw 7
    movwf Count
    bsf   EECON1,EEPGD
    
name1 tblrd*+
    movff TABLAT,POSTINC0
    decfsz  Count
    bra   name1
    bcf   EECON1,EEPGD  
    movlw 8
    movwf Dlc
    call  sendTXa
    return
    


;**********************************************************

;   send individual parameter

;   Index 0 sends no of parameters

para1rd movf  Rx0d3,w
    sublw 0
    bz    numParams
    movlw PRMCOUNT
    movff Rx0d3, Temp
    decf  Temp
    cpfslt  Temp
    bra   pidxerr
    movlw 0x9B
    movwf Tx1d0
    movlw 7   ;FLAGS index in nodeprm
    cpfseq  Temp
    bra   notFlags      
    call  getflags
    movwf Tx1d4
    bra   addflags
notFlags  
    movlw .14
    cpfseq  Temp
    bra   nxtparam
    call  getId1
    movwf Tx1d4
    bra   addflags
nxtparam
    movlw .15
    cpfseq  Temp
    bra   paramrd
    call  getId2
    movwf Tx1d4
    bra   addflags
paramrd
    movlw LOW nodeprm
    movwf TBLPTRL
    movlw HIGH nodeprm
    movwf TBLPTRH   ;relocated code
    clrf  TBLPTRU
    decf  Rx0d3,W
    addwf TBLPTRL
    bsf   EECON1,EEPGD
    tblrd*
    movff TABLAT,Tx1d4
addflags            
    movff Rx0d3,Tx1d3
    movlw 5
    movwf Dlc
    call  sendTX
    return  
    
numParams
    movlw 0x9B
    movwf Tx1d0
    movlw PRMCOUNT
    movwf Tx1d4
    movff Rx0d3,Tx1d3
    movlw 5
    movwf Dlc
    call  sendTX
    return
    
pidxerr
    movlw .10
    call  errsub
    return
    
getflags    ; create flags byte
    movlw PF_CONSUMER
    btfsc Mode,1
    iorlw 4   ; set bit 2
    movwf Temp
    bsf   Temp,3    ;set bit 3, we are bootable
    movf  Temp,w
    return
    
;**********************************************************

; returns Node Number, Manufacturer Id, Module Id and Flags

whoami
    call  ldely   ;wait for other nodes
    movlw OPC_PNN
    movwf Tx1d0
    movlw MAN_NO    ;Manufacturer Id
    movwf Tx1d3
    movlw MODULE_ID   ; Module Id
    movwf Tx1d4
    call  getflags
    movwf Tx1d5
    movlw 6
    movwf Dlc
    call  sendTX
    return

;***********************************************************
;
;getDevId returnd DEVID2 and DEVID1 in PRODH and PRODL

getId1
  call  getProdId
  movf  PRODL,w
  return
  
getId2
  call  getProdId
  movf  PRODH,w
  return

getProdId

  movlw 0x3F
  movwf TBLPTRU
  movlw 0xFF
  movwf TBLPTRH
  movlw 0xFE
  movwf TBLPTRL
  bsf   EECON1, EEPGD
  tblrd*+
  movff TABLAT, PRODL
  tblrd*
  movff TABLAT, PRODH
  return    

;***********************************************************

; error message send

errmsg  call  errsub
    goto  main2 
errmsg1 call  errsub
    goto  l_out2
errmsg2 call  errsub
    goto  l_out1

errsub
    movwf Tx1d3   ;main eror message send. Error no. in WREG
    movlw 0x6F
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    return

;**********************************************************************

;
;   self enumeration as separate subroutine

self_en movff FSR1L,Fsr_tmp1Le  ;save FSR1 just in case
    movff FSR1H,Fsr_tmp1He 
    movlw B'11000000'
    movwf INTCON      ;start interrupts if not already started
    bsf   Datmode,1   ;set to 'setup' mode
    clrf  Tx1con      ;CAN ID enumeration. Send RTR frame, start timer
    movlw .14
    movwf Count
    lfsr  FSR0, Enum0
clr_en
    clrf  POSTINC0
    decfsz  Count
    bra   clr_en
    
    movlw B'10111111'   ;fixed node, default ID  
    movwf Tx1sidh
    movlw B'11100000'
    movwf Tx1sidl
    movlw B'01000000'   ;RTR frame
    movwf Dlc
    
    movlw 0x3C      ;set T3 to 100 mSec (may need more?)
    movwf TMR3H
    movlw 0xAF
    movwf TMR3L
    movlw B'10110001'
    movwf T3CON     ;enable timer 3

    movlw .10
    movwf Latcount
    
    call  sendTXa     ;send RTR frame
    clrf  Tx1dlc      ;prevent more RTR frames

self_en1    btfss PIR2,TMR3IF   ;setup timer out?
    bra   self_en1      ;fast loop till timer out 
    bcf   T3CON,TMR3ON  ;timer off
    bcf   PIR2,TMR3IF   ;clear flag


    clrf  IDcount
    incf  IDcount,F     ;ID starts at 1
    clrf  Roll
    bsf   Roll,0
    lfsr  FSR1,Enum0      ;set FSR to start
here1 incf  INDF1,W       ;find a space
    bnz   here
    movlw 8
    addwf IDcount,F
    incf  FSR1L
    bra   here1
here  movf  Roll,W
    andwf INDF1,W
    bz    here2
    rlcf  Roll,F
    incf  IDcount,F
    bra   here
here2 movlw .100        ;limit to ID
    cpfslt  IDcount
    bra   segful        ;segment full
    
here3 movlw LOW CANid   ;put new ID in EEPROM
    movwf EEADR
    movf  IDcount,W
    call  eewrite
    call  newid_f     ;put new ID in various buffers
;   movlw Modstat
;   movwf EEADR
;   movlw 1
;   call  eewrite     ;set to normal status
;   bcf   Datmode,1   ;out of setup
      
    movff Fsr_tmp1Le,FSR1L  ;
    movff Fsr_tmp1He,FSR1H 
    return

segful  movlw 7   ;segment full, no CAN_ID allocated
    call  errsub
    setf  IDcount
    bcf   IDcount,7
    bra   here3
      
ENstart   ;dummy label
    
; event data
  org 0x2000
evdata

;************************************************************************   
  ORG 0xF00000      ;EEPROM data. Defaults
  

CANid de  B'01111111',0 ;CAN id default and module status
NodeID  de  0,0     ;Node ID    

ENCount de  0,0xff  ; no of events in event data/free space
FreeCh  de  0,0   ; address of first free entry

; event hash table, indexed by ls 6 bits of ls byte of event node num.
; table contains address of event data in High/Low pairs if present else zero

  org 0xf00010
hashtab de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    
; number of events in each hash table entry
hashnum de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    
    de 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    
    ORG 0xF000FE
NVstart de  0,0     ;for bootloader and NV

  end


