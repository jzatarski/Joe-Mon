; Joe Zatarski's 68332 monitor
; Written for use with Beckman DU600 photospectrometer

; This is written for use on a 68332 processor, but should be portable to
; other 68K archs without too much trouble.

; Copyright 2014-2017 Joe Zatarski
; email joe.zatarski@gmail.com

; This software is licensed under the GPL. See license.txt for more details

; PORTING:
; setup exception vector table to suit your particular system
; there is a ton of 68332 specific init code just at start. Be sure to change
; this to suit your system, and do any special init you need
; change rambase and ramsize for your system
; the boot command sets the vector base register (VBR). Not all 68K
; processors have this register.
; to run software in user mode, the monitor creates a fake exception
; frame on the stack and runs RTE. the format of this exception frame can
; differ slightly between 68K variants.
; likewise, the exception stack frames for various faults and internally
; generated exceptions may differ. There is a section where these stack frames
; are parsed, and this will need to be updated for stack frames which differ
; from those used on the CPU32

; if the symbol ram_version is defined, this is used as an indication that
; the monitor should be assembled so that it can be downloaded into
; RAM and tested without having to reburn EPROMs or flash memory ICs


	ifnd ram_version
rambase	equ	$400000	; this is where main RAM starts
ramend	equ	$500000	; one past end of RAM
rombase	equ	$000000	; used for org

	else
rambase	equ	monitor_end
ramend	equ	$500000
rombase	equ	$400000

	endif

ramsize	equ	ramend-rambase

; memory is organized as follows:
; top of RAM
; supervisor stack
; monitor variables - 8K below top of RAM
; user stack
;
super_stack	equ	ramend	; stack is at end of RAM
mon_ram		equ	super_stack-$2000	; start of variables used by the monitor
user_stack	equ	mon_ram	; stack is predecrement, so this should be fine

; monitor only uses these equates for testing in the init for now
; later they may be used for a RAM disk with a simple file system
sram1	equ	$5C0000	; start of onboard battery backed SRAM
sram1size	equ	$40000	; 256K in size

sram2	equ	$600000	; start of battery backed SRAM card
sram2size	equ	$100000	; 1MB in size

; a few character constants:
newline	equ	$D	; most terminals send this when you hit enter
cr	equ	$D
lf	equ	$A
esc	equ	$1B
bs	equ	$8	; backspace character
del	equ	$7F	; some terminals use delete in place of backspace
xon	equ	$11
xoff	equ	$13

; CCR bit set (OR) masks
carry_flag	equ	%1

	include "CPU.EQU"
	include "MISC.EQU"
	include "PARPORT.EQU"
	include "SERIAL.EQU"
	include	"RTC.EQU"
	include	"KB.EQU"
	include	"ATA.EQU"
	
	org	rombase	; vector table
	
	dc.l	super_stack	; reset: initial stack pointer
	dc.l	reset	; reset: initial program counter
	
	dc.l	berr	; bus error
	dc.l	aerr	; address error
	
	dc.l	illinst	; 4 - illegal instruction
	dc.l	divzero	; zero division
	dc.l	chk	; CHK/CHK2
	dc.l	trap	; TRAPcc, TRAPV
	
	dc.l	priv	; 8 - privilege violation
	dc.l	trace	; trace
	dc.l	aline	; line 1010 emulator
	dc.l	fline	; line 1111 emulator (for FPU emulation)
	
	dc.l	hdwbkpt	; 12 - hardware breakpoint
	dc.l	res	;dc.l	coprocv	; coprocessor protocol violation - not on CPU32
	dc.l	formerr
	dc.l	unint
	
	dc.l	res	; 16
	dc.l	res	; vectors 16-23 are reserved
	dc.l	res
	dc.l	res
	
	dc.l	res	; 20
	dc.l	res
	dc.l	res
	dc.l	res
	
	dc.l	spurint	; 24
	dc.l	psu_irq	; IRQ from the PSU, AVEC
	dc.l	pp_irq	; IRQ from parallel port, AVEC
	dc.l	rtc_irq	; IRQ from RTC, AVEC
	
	dc.l	avec4	; 28 - autovector for IRQ4, not used
	dc.l	avec5	; autovector for IRQ5, not used
	dc.l	kbd_irq	; IRQ from keyboard controller, AVEC
	dc.l	nmi	; setup to AVEC, available as a pin on J3
	
	dc.l	trap0	; 32
	dc.l	trap1
	dc.l	trap2
	dc.l	trap3
	
	dc.l	trap4	; 36
	dc.l	trap5
	dc.l	trap6
	dc.l	trap7
	
	dc.l	trap8	; 40
	dc.l	trap9
	dc.l	trapa
	dc.l	trapb
	
	dc.l	trapc	; 44
	dc.l	trapd
	dc.l	trape
	dc.l	trapf
	
	dc.l	res	;dc.l	coproc0	; 48
	dc.l	res	;dc.l	coproc1
	dc.l	res	;dc.l	coproc2
	dc.l	res	;dc.l	coproc3
	
	dc.l	res	;dc.l	coproc4	; 52
	dc.l	res	;dc.l	coproc5
	dc.l	res	;dc.l	coproc6
	dc.l	res	;dc.l	coproc7
	
	dc.l	res	;dc.l	coproc8	; 56
	dc.l	res	;dc.l	coproc9
	dc.l	res	;dc.l	coproca
	dc.l	res
	
	dc.l	res	; 60
	dc.l	res
	dc.l	res
	dc.l	res
	
; this is where user vectors start
; various CPU modules will use vectors here, as well as the UARTs
	dc.l	pitirq	; 64 - periodic interupt timer IRQ
	
	org	rombase+$140
	dc.l	qspi	; 80 - QSPI IRQ
	
	org	rombase+$180
	dc.l	tpu0	; 96 - TPU channel 0 IRQ
	dc.l	tpu1
	dc.l	tpu2
	dc.l	tpu3
	
	dc.l	tpu4	; 100 - TPU channel 4 IRQ
	dc.l	tpu5
	dc.l	tpu6
	dc.l	tpu7
	
	dc.l	tpu8	; 104 - TPU channel 8 IRQ
	dc.l	tpu9
	dc.l	tpua
	dc.l	tpub
	
	dc.l	tpuc	; 108 - TPU channel 12 IRQ
	dc.l	tpud
	dc.l	tpue
	dc.l	tpuf
	
	org	rombase+$200
	dc.l	duart0_irq	; 128 - DUART1 IRQ

	org	rombase+$400	; just past vector table
	
reset	
sysinit	
; This section is meant to be for system initialization that isn't strictly
; something a monitor would have to do. For example, initializing the DRAM
; controller on the Beckman DU600 motherboard would go in this section, while
; initializing the UART (or other console device) would normally be part of the
; monitor itself. The intention is that the monitor will have a series of JSRs
; to various system specific subroutines, and that series of JSRs won't have to
; be changed. If there is something you need to add that is not part of that
; series of JSRs, then you are probably either doing initialization of hardware
; which should be done in this section, or you are adding an entirely new
; feature.

; anyway, it's really up to you, but the above is what I intended

; TODO: there are a couple simple RAM tests here as well, those should be moved
; to the main monitor code.

	move.w	#$60C8,(SIMCR)	; set SIMCR such that:
				; CLKOUT pin is driven from an internal	clock
				; source
				; when FREEZE is asserted, software watchdog,
				; periodic interupt timer counters, and bus
				; monitor are disabled
				; IMB is not available to an external master
				; show internal cycles disabled, external
				; arbitration enabled
				; SIM regs restricted to supervisor access only
				; internal modules mapped to $FF F000-$FF FFFF
				; SIM IARB set to 8
	move.w	#$7F00,(SYNCR)	; set SYNCR such that:
				; clock multiplier is 512 overall, system clock
				; is about 16.8mhz
				; ECLK is 1/8 system clock
				; loss of crystal puts MCU in limp mode
				; in LPSTOP mode, SIM clock is driven by
				; crystal and VCO is turned off
				; in LPSTOP mode, CLKOUT is held negated
	move.b	#4,(SYPCR)	; set SYPCR such that:
				; software watchdog disabled
				; halt monitor disabled
				; internal bus monitor enabled, 64 clock timeout
	move.w	#$FFE0,(TRAMBAR) ; set TRAMBAR such that:
				; base address of TPU RAM is $FF E000
	move.w	#$140,(PITR) 	; set PITR such that:
				; time period is approximately 1 second
	move.w	#$040,(PICR)	; set PICR such that:
				; IRQ disabled
				; vector number #$40
	move.w	#$35FF,(CSPAR0) ; set CSPAR0 such that:
				; CS5, CS2, CS1 and CSBOOT are 16 bit ports
				; CS4 is FC1
				; CS3 is FC0
	move.w	#$2B9,(CSPAR1)	; set CSPAR1 such that:
				; CS8 is a 16 bit port
				; CS10, CS9, and CS7 are 8 bit ports
				; CS6 is ADDR19

; setting all this CS stuff *should* be OK for the RAM version as long as the location of the RAM we're loaded in
; isn't actually changed here (it can be loaded again, with the same value, and it should be fine)

	move.w	#7,(CSBARBT)	; CSBARBT - CSBOOT base addr at $00 0000 with a size of 1M
	move.w	#$1007,(CSBAR0) ; CSBAR0 - CS0 base addr at $10 0000 with a size of 1M
	move.w	#$6007,(CSBAR2) ; CSBAR2 - CS2 base addr at $60 0000 with a size of 1M
	move.w	#$5C05,(CSBAR8) ; CSBAR8 - CS8 base addr at $5C 0000 with a size of 256K
	move.w	#$4007,(CSBAR1) ; CSBAR1 - CS1 base addr at $40 0000 with a size of 1M
	move.w	#$D002,(CSBAR5) ; CSBAR5 - CS5 base addr at $D0 0000 with a size of 16K
	move.w	#$B007,(CSBAR7) ; CSBAR7 - CS7 base addr at $B0 0000 with a size of 1M
	move.w	#$E000,(CSBAR9) ; CSBAR9 - CS9 base addr at $E0 0000 with a size of 2K
	move.w	#$F003,(CSBAR10) ; CSBAR10 - CS10 base addr at $F0 0000 with a size of 64K
	move.w	#$7870,(CSORBT) ; set CSORBT for:
				; asynch mode
				; assert on upper and lower bytes
				; R/W access
				; assert on address strobe
				; 1 wait state
				; supervisor/user space
	move.w	#$7B70,(CSOR0)	; set CSOR0 for same as CSORBT but:
				; 13 wait states
	move.w	#$7B70,(CSOR2)	; set CSOR2 same as CSORBT but:
				; 13 wait states
	move.w	#$7870,(CSOR8)	; set CSOR8 same as CSORBT
	move.w	#$7BF0,(CSOR1)	; set CSOR1 same as CSORBT but:
				; external DSACK
	move.w	#$7B70,(CSOR5)	; set CSOR5 same as CSORBT but:
				; 13 wait states
	move.w	#$7BF0,(CSOR7)	; set CSOR7 same as CSORBT but:
				; external DSACK
	move.w	#$7B70,(CSOR9)	; set CSOR9 same as CSORBT but:
				; 13 wait states
	move.w	#$7B70,(CSOR10) ; set CSOR10 same as CSORBT but:
				; 13 wait states
	move.w	#$8A,(QSMCR)	; set QSMCR for:
				; QSM clock running
				; ignore FREEZE signal
				; QSM regs supervisor only
				; IARB number for QSM is 10
	move.w	#0,(QTEST)	; QTEST
	move.w	#$0050,(QILR)	; set QILR/QIVR for:
				; QSPI IRQ disabled
				; SCI IRQ disabled
				; QSM interupt vector #$50
	move.w	#$300,(SPCR0)	; set SPCR0 so that:
				; QSPI is a slave device
				; outputs normal (not wired OR)
				; 16 bits per transfer
				; inactive state of SCK is one
				; data is changed on leading edge of SCK and captured on following edge
				; baud generator disabled
	move.b	#$70,(PORTQS)	; PORTQS - port QS set to output #$70
	move.w	#$A70,(PQSPAR)	; set PQSPAR/DDRQS such that:
				; PQS 0, 4-6 are GPIO pins
				; PQS 1 is MOSI
				; PQS 3 is PCS0/SS
				; PQS 4-6 are outputs
				; all others inputs or alternate function
	move.w	#0,(SPCR3)	; set SPCR3/SPSR such that:
				; QSPI loop mode off
				; HALTA and MODF interupts disabled
				; halt not enabled
	move.w	#$6084,(TPUMCR)	; set TPUMCR such that:
				; TPU operating normally (not stopped)
				; timer count register 1 clock is 1/256 system clock
				; timer count register 2 clock is external TCR2 clock source divided by 1
				; TPU and TPURAM not in emulation mode
				; all TPU registers supervisor only
				; TPU IARB is set to 4
	ori.w	#$DDDD,(CFSR3)	; set CFSR3 such that TPU channels 0-3	are set for stepper motor control
	ori.w	#$DDDD,(CFSR2)	; set CFSR2 such that TPU channels 4-7	are set for stepper motor control
	ori.w	#$98,(CFSR1)	; set CFSR1 such that
				; TPU channel 9 is set for PWM
				; TPU channel 8 is set for discrete input/output
	move.w	#$82,($FFFF00)	; set channel 0 such that:
				; pin is low
				; Capture TCR1, Match TCR1
	move.w	#$6666,($FFFF02)	; sequence for CH0 to control a stepper motor
	move.w	#0,($FFFF04)	; stores current position of stepper motor
	move.w	#0,($FFFF06)	; stores desired position of stepper motor
	move.w	#1,($FFFF08)	; MOD_CNT and NEXT_STEP_RATE initialized as per documentation
	move.w	#$101,($FFFF0A)	; only 1 step rate and last sec. channel is 1
	move.w	#$82,($FFFF10)	; set channel 1 so that pin is low, capture TCR1, match TCR1
	move.w	#$3333,($FFFF12)	; set pattern to control stepper motor
	move.w	#0,($FFFF14)	; constant step rate
	move.w	#0,($FFFF16)	; fastest possible step rate
	move.w	#$82,($FFFF20)	; channel 2: pin is low, capture TCR1, match TCR1
	move.w	#$6666,($FFFF22)	; pattern to control stepper motor
	move.w	#0,($FFFF24)	; initialize current position of stepper motor
	move.w	#0,($FFFF26)	; init desired position of stepper motor
	move.w	#1,($FFFF28)	; MOD_CNT and NEXT_STEP_RATE initialized as per docs
	move.w	#$103,($FFFF2A)	; 1 step rate and last secondary channel is #3
	move.w	#$82,($FFFF30)	; set channel 3 so that pin is low, capture TCR1, match TCR1
	move.w	#$3333,($FFFF32)	; set pattern to control stepper motor
	move.w	#0,($FFFF34)	; constant step rate
	move.w	#$DA7,($FFFF36)	; sets time between steps in TCR1 clocks
	move.w	#$82,($FFFF40)	; set channel 4 so that pin is low, capture TCR1, match TCR1
	move.w	#$3333,($FFFF42)	; set pattern to control stepper motor
	move.w	#0,($FFFF44)	; init current position of the stepper motor
	move.w	#0,($FFFF46)	; init desired position of stepper motor
	move.w	#1,($FFFF48)	; MOD_CNT and NEXT_STEP_RATE init as per docs
	move.w	#$105,($FFFF4A)	; 1 step rate and last sec. channel is 5
	move.w	#$82,($FFFF50)	; set channel 5 so that pin is low, capture TCR1, match TCR1
	move.w	#$6666,($FFFF52)	; set pattern to control stepper motor
	move.w	#0,($FFFF54)	; constant step rate
	move.w	#$28F7,($FFFF56)	; sets time between steps to #$28F7 TCR1 clocks
	move.w	#$82,($FFFF60)	; set channel 6 so that pin is low, capture TCR1, match TCR1
	move.w	#$6666,($FFFF62)	; set pattern to control stepper motor
	move.w	#0,($FFFF64)	; init current position of stepper motor
	move.w	#0,($FFFF66)	; init desired position of stepper motor
	move.w	#1,($FFFF68)	; MOD_CNT and NEXT_STEP_RATE initialized as per docs
	move.w	#$107,($FFFF6A)	; 1 step rate and last secondary channel is 7
	move.w	#$82,($FFFF70)	; set channel 7 so that pin is low, capture TCR1, match TCR1
	move.w	#$3333,($FFFF72)	; set pattern to control stepper motor
	move.w	#0,($FFFF74)	; constant step rate
	move.w	#0,($FFFF76)	; no delay between steps
	move.w	#$92,($FFFF90)	; channel 9 is output, capture TCR1, compare TCR1, don't change PAC, force pin high
	move.w	#0,($FFFF94)	; set high time in TCR1 clocks
	move.w	#149,($FFFF96)	; set period time in TCR1 clocks
				; should be 440hz with about 50% duty cycle
	move.w	#$AAAA,(HSRR1)	; init channel 0-7
	move.w	#9,(HSRR0)	; init channel 9 and set channel 8 high
	move.w	#$AAAA,(CPR1)	; priority set to middle for channel 0-7
	move.w	#$A,(CPR0)	; priority set to middle for channels 8 and 9
.tpuloop1:
	cmpi.w	#0,(HSRR0)	; wait for TPU to finish what it's doing
	bne.s	.tpuloop1
.tpuloop2:
	cmpi.w	#0,(HSRR1)	; still waiting for TPU
	bne.s	.tpuloop2
	move.w	#0,(CISR)	; clear interupts
	ori.w	#$800,(CFSR1)	; set CFSR1 such that TPU channel 10 is	discrete I/O
	ori.w	#$10,(CPR0)	; channel 10 set to low priority
	ori.w	#$20,(HSRR0)	; set channel 10 output low
	ori.w	#8,(CFSR0)	; set CFSR0 such that TPU channel 12 is	discrete I/O
	ori.w	#$100,(CPR0)	; set channel 12 to low priority
	ori.w	#$100,(HSRR0)	; set channel 12 output low
	move.w	#$F3,($FFFFE0)	; channel 14 is output, capture TCR2, compare TCR2, don't change PAC, force pin low
	move.w	#$3D9,($FFFFE4)	; set time high in TCR2 clocks for channel 14
	move.w	#$7B2,($FFFFE6)	; set period time in TCR2 clocks for channel 14
	ori.w	#$900,(CFSR0)	; set channel 14 to PWM
	ori.w	#$1000,(CPR0)	; low priority for channel 14
	ori.w	#$2000,(HSRR0)	; init channel 14
	move.w	#$FFFF,(DRAMC_CNTL) ; this register controls a few interupts from the PSU, as well as a couple pins on the DRAM controller
	ori.w	#$8000,(CFSR0)	; set channel 15 to discrete I/O
				; this pin controls the	/OE for	the DRAMC_CNTL latch
	ori.w	#$4000,(CPR0)	; low priority for channel 15
	ori.w	#$8000,(HSRR0)	; force channel 15 output low
.tpuloop3:
	cmpi.w	#0,(HSRR0)	; wait for TPU to finish what it's doing
	bne.s	.tpuloop3
.tpuloop4:
	cmpi.w	#0,(HSRR1)	; wait for TPU
	bne.s	.tpuloop4
	move.w	#0,(CISR)	; clear TPU interupts
	move.w	#$160,(TICR)	; set TICR such that:
				; TPU uses IRQ1
				; vectors for TPU channels are #$6x
	move.w	#$55,(CIER)	; set CIER such that interupts are enabled for channels 0, 2, 4, and 6
	move.w	#0,(CISR)	; clear interupts
	move.b	#0,(MTR_CNTRL)	; U18 daughtercard

	ifnd	ram_version	; can't reconfigure the DRAMC if we're in DRAM

	move.w	#$EFFF,(DRAMC_CNTL) ; put /ML low (active) on the DRAM controller
	move.w	#$CFFF,(DRAMC_CNTL) ; put /DISRFSH low (active) on the DRAM controller. This starts an external reset
	move.l	#$200,d0	; load delay counter
.dramloop1:
	subi.l	#1,d0		; delay for the proper time of the reset
	bne.s	.dramloop1
	move.w	#$DFFF,(DRAMC_CNTL) ; deassert /ML BEFORE /DISRFSH
	move.w	#$FFFF,(DRAMC_CNTL) ; now deassert /DISRFSH
	movea.l	#%10001111001010011001100,a0 ; prepare a0 with programming info for the DRAM controller
				; programming value for DRAM controller:
				; even address means /CAS outputs will be deasserted with /RAS outputs when /AREQ is deasserted
				; even address means /WE output functions normal, as write enable
				; B1 is forced high in hardware, so /ADS starts memory access immediately
				; B0 is forced high in hardware, so row col and bank latches are fall through
				; C9 is forced high in hardware, so /CAS is delayed during WRITE accesses
				; row address hold time 15ns min.
				; col. address setup time 0ns min.
				; all RAS and CAS are selected during access, ECAS must be asserted for appropriate CAS to be asserted
				; B1 and B0 not used
				; no error scrubbing
				; 15 microsecond refresh period
				; DELCLK divisor is 5
				; RAS outputs all assert and negate at the same time during refresh
				; no address pipelining
				; DTACK output for /WAIT
				; DTACK will be delayed by one additional positive edge of CLK (WAITIN is held active by ha
	move.w	#$EFFF,(DRAMC_CNTL) ; assert /ML
	move.w	(a0),d0		; write programming info into the DRAM controller
	move.w	#$FFFF,(DRAMC_CNTL) ; deassert /ML
	move.l	#$1FFFF,d0
.dramloop2:
	subi.l	#1,d0		; delay
	bne.s	.dramloop2

	endif

	MOVE.W	#0,LEDS

	ifnd ram_version	; skip ram test

dramtest:
	movea.l	#rambase,a0	; point	a0 at the beginning of DRAM
	move.l	#(ramsize/4),d0	; get size of RAM in long words
.testloop:
	move.l	(a0),d1		; get a	copy of	the first long
	not.l	(a0)		; NOT the RAM location
	not.l	d1		; NOT the copy of the RAM location
	cmp.l	(a0),d1
	beq.l	.ramgood1	; if the two match, then I guess the RAM passes
	move.w	#$A000,(LEDS)	; if the ram fails, set some lights to show it
	move.l	a0,ram_test_err
	jmp	coldstart	; TODO: add some better handling of bad RAM

.ramgood1:
	not.l	(a0)		; sets RAM back	to what	it was
	not.l	d1		; sets d1 back to what ram was to begin	with
	cmp.l	(a0)+,d1
	beq.l	.ramgood2	; if the two match, I guess the	ram passes
	move.w	#$A000,(LEDS)	; if the RAM fails, set some lights to show it
	move.l	a0,ram_test_err
	jmp	coldstart

.ramgood2:
	subi.l	#1,d0		; decrement counter
	bne.l	.testloop	; if the counter isn't 0, then keep looping to test the rest of the RAM
	
sram1test:
	movea.l	#sram1,a0	; set a0 to point at the onboard 256K battery backed SRAM
	move.l	#(sram1size/4),d0 ; size of sram1 in long words
.testloop:
	move.l	(a0),d1		; get byte
	not.l	(a0)		; NOT the RAM
	not.l	d1		; and the copy of the byte
	cmp.l	(a0),d1
	beq.l	.ramgood1	; if they match	after the NOT, then the	ram location passes
	move.w	#$5000,(LEDS)	; otherwise, show failure
	move.l	a0,ram_test_err
	jmp	coldstart

.ramgood1:				; CODE XREF: sub_C58C-2400j
	not.l	(a0)		; NOT ram location again
	not.l	d1		; and the copy
	cmp.l	(a0)+,d1
	beq.l	.ramgood2	; if they match, the RAM passes
	move.w	#$4000,(LEDS)	; otherwise, show failure
	move.l	a0,ram_test_err
	jmp	coldstart
; ---------------------------------------------------------------------------
.ramgood2:				; CODE XREF: sub_C58C-23E8j
	subi.l	#1,d0		; decrement counter
	bne.s	.testloop	; if it's not 0, keep testing more RAM

	endif

;	move.w	#$F000,(PPORT)	; show we got past the first part of boot
				; ALSO CLEARS PARALLEL PORT IRQ


coldstart:
	; monitor actually starts here
	; interupts are assumed to be off at this point
	; there will be a series of JSRs here to various hardware init
	; not much yet, just the console for now

	jsr	init_console
	
	jsr	init_rtc
	
	jsr	init_pport
	
	move.w	#%0010000100000000,SR	; set IRQ mask to IRQ 1 and below
	
warmstart:
	; this is after one-time hardware init

	jsr	clr_screen

	jsr	init_mon_ram
	
	jsr	ver	; sign on message

prompt_loop:	; loop which accepts and interprets monitor commands
	movea.l	#.prompt,a0	; print the prompt
	jsr	puts

	movea.l	#input_buff,a0	; get a command string
	move.l	#input_buf_len,d1
	jsr	gets
	
	movea.l	#cmd_table,a2	; point to start of command table

.cmd_table_loop
	movea.l	(a2),a1	; point to command string from the table entry

	tst.l	a1	; check the address
	beq	.no_match	; null pointer means end of the list, we don't have a match

	movea.l	#input_buff,a0	; point to the user command input

.next_char
	move.b	(a1)+,d0	; get a character from the table
	move.b	(a0)+,d1	; get a character from the user input

	tst.b	d0	; if we're at a null in the string from the table
	beq	.string_end	; then we're almost done with the comparison

	cmp.b	lowercasebound,d1	; check if user input is lowercase and convert to uppercase
	bcs	.not_lowercase
	
	bclr	#5,d1	; clearing bit 5 converts from lowercase to uppercase

.not_lowercase

	cmp.b	d0,d1	; check if the characters match now
	bne	.next_entry
	jmp	.next_char	; if yes, continue checking

.string_end
	; the user input must either end (with null) or have a space (separating arguments)
	tst.b	d1
	beq	.match	; if it's null, then it matches
	cmp.b	#(' '),d1
	beq	.match	; if it's space, then it's also a match
	;jmp	.next_entry	; otherwise, we move on to the next entry	

.next_entry
	addq.l	#8,a2	; increment to next entry in table
	jmp	.cmd_table_loop

.no_match
	move.l	#.cmd_err,a0	; load address of command error string
	jsr	puts
	jmp	prompt_loop

.match
; pass two arguments to the command, a pointer to an array of pointers to arguments (*argv[]) in a6, and an argument count (argc) in d7
; command routine should *not* destroy d7, as it is used to restore the stack
	movea.l	#input_buff,a3	; use a3 as pointer to input buffer
.find_end
	tst.b	(a3)+
	bne	.find_end	; loop until a3 points just past the end of the command string
	
	moveq.l	#1,d7	; we will add one more argument than we actually count in .new_arg
	
.loop
	cmpi.b	#' ',-(a3)	; check if current character in command string is space
	beq	.new_arg	; if it is, then we are at the start of a new argument
	cmpa.l	#input_buff,a3
	beq	.end_cmd	; if we are back at the beginning, then we are done looking for args
	
	jmp	.loop	; otherwise, move on to next character
	
.new_arg
	clr.b	(a3)	; change space to null to end individual argument
	pea	(1,a3)	; push address of argument
	addq.l	#1,d7	; one more arg
	jmp	.loop	; go look for more args
	
.end_cmd
	pea	input_buff	; input buffer beginning is the first arg
	movea.l	a7,a6	; stack pointer is now at address of argv[]
	
	move.l	(4,a2),a1	; get 4 past a2, which is the function pointer for the command
	jsr	(a1)		; jump to the command's function
	
	lea	(0,a7,d7.l*4),a7	; restore stack

	jmp	prompt_loop

.prompt
	dc.b	">",0

.cmd_err
	dc.b	"Error: command not recognized.",cr,lf,0
	
	align 1

init_console:
; CALL BEFORE INTERRUPTS ENABLED
	move.b	#%00011010,(DUART0+DUART_CRA)	; resets pointer to MR1A and disables Tx and Rx
	;move.b	#0,(DUART0+DUART_CRA)	; removes command from register, dunno if necessary
	
	move.b	#%10000000,(DUART0+DUART_CRA)	; set Rx BRG X=1
	move.b	#%10100000,(DUART0+DUART_CRA)	; set Tx BRG X=1

	move.b	#%10010011,(DUART0+DUART_MRA)	; receiver controls RTS (acts as CTS out), Rx IRQ on RxRDY, char error mode, no parity, 8 bits/char
	move.b	#%00000111,(DUART0+DUART_MRA)	; normal channel mode, no Tx RTS control, no CTS, 1 stop bit

	move.b	#%00000000,(DUART0+DUART_ACR)	; baud rate set 1, no delta IP interupts

	move.b	#$88,(DUART0+DUART_CSRA)	; 115200 baud
	
	move.b	#$80,(DUART0+DUART_IVR)	; set interrupt vector

	move.b	#%00000101,(DUART0+DUART_CRA)	; turn on Tx and Rx for channel A
	
	
	move.b	#%00001010,(DUART0+DUART_CRB)	; turns off channel B Tx and Rx
	
	movea.l	#ser_rx_buff,a0	; initialize the ring buffers
	
	move.l	a0,ser_rx_buff_head
	move.l	a0,ser_rx_buff_tail
	
	movea.l	#ser_tx_buff,a0
	
	move.l	a0,ser_tx_buff_head
	move.l	a0,ser_tx_buff_tail
	
	clr.b	ser_tx_flow
	
	move.b	#%00000110,(DUART0+DUART_IMR)	; RXRDYA interrupt enabled
	
	rts
	
clr_screen:
	; Clears the screen, customize for your terminal, or don't use it
	; ^[[2J is the ECMA-48 control sequence to clear the whole screen
	; ^[H is the ECMA-48 control sequence to home the cursor
	; trashes a0
	; calls puts, which trashes other stuff
	movea.l	#.escapesequence,a0
	jmp	puts	; puts will return for us.
	
.escapesequence:
	dc.b	$1B,"[H",$1B,"[2J",0	; NULL terminated string which contains escape sequence

	align 1
putnl:
	movea.l	#newline_str,a0
	
puts:
	; Put string
	; string address passed in a0
	; trashes d0
	; calls putc
	
	move.b	(a0)+,d0	; get first character
	beq	.done	; if NULL, then we're done
	jsr	putc	; otherwise call putc
	jmp	puts	; and loop until we're done
.done:
	rts

putsp:
	; outputs a space
	move.b	#' ',d0
putc:
	; Put character
	; character passed in d0
	
	;tst.b	ser_tx_flow
	;bne	putc

	;btst.b	#2,(DUART0+DUART_SRA)	; check TXRDY bit
	;beq 	putc	; if TXRDY isn't set, then wait
	;move.b	d0,(DUART0+DUART_THRA)	; send byte
	;rts
	
	move.l	a0,-(a7)
	
	movea.l	ser_tx_buff_tail,a0

	move.b	d0,(a0)+
	
	cmpa.l	#ser_tx_buff_end,a0
	bne	.tx_buff_full
	
	movea.l	#ser_tx_buff,a0
	
.tx_buff_full
	cmpa.l	ser_tx_buff_head,a0
	beq	.tx_buff_full	; if buffer is full, just wait a bit to update the tail variable
	
	move.l	a0,ser_tx_buff_tail
	
	move.b	#%00000111,(DUART0+DUART_IMR)
	
	move.l	(a7)+,a0
	rts
	
gets:
	; Get string
	; gets characters until newline found or buffer filled
	; backspace (^H) deletes previous character as does delete
	; ignores characters with ascii value of less than #$20 except newline and BS
	; echos characters as they are typed
	; a0 passes pointer to buffer
	; d0 gets trashed
	; d1 passes buffer length (long)
	; d2 holds current character count
	; 0 must not be passed as the buffer length, or it will be interpreted as 2^32
	; calls getc which trashes other stuff (d0)

	clr.l	d2	; clear character counter
	subq.l	#1,d1	; subtract 1 to reserve a spot for the null

.next_char
	jsr	getc
	cmp.b	#newline,d0	; check newline char
	beq	.done
	cmp.b	#bs,d0	; check backspace
	beq	.backspace
	cmp.b	#del,d0	; check delete
	beq	.backspace

	addq.l	#1,d2	; add 1 to character counter
	
	move.b	d0,(a0)+	; add character to buffer
	jsr	putc	; echo character

	cmp.l	d2,d1	; check if we've filled the buffer
	bls	.done

	jmp	.next_char

.backspace
	tst.l	d2	; check if we have any characters to delete
	beq	.next_char

	jsr	putc	; echo the delete or backspace char

	subq.l	#1,d2	; decrement character counter
	suba	#1,a0	; move pointer back one character

	jmp	.next_char

.done
	clr.b	(a0)+	; add null terminator
	move.b	#cr,d0	; CR/LF to advance the line
	jsr	putc
	move.b	#lf,d0
	jmp	putc	; putc will return for us
	
getc:
	; Get Char
	; waits for a character
	; returns character in d0
	
; old polled I/O code
	
;	btst	#0,(DUART0+DUART_SRA)
;	beq	getc
;	move.b	(DUART0+DUART_RHRA),d0

; now we poll a buffer instead

	movem.l	a0/d1,-(a7)

	movea.l	ser_rx_buff_head,a0
	
.wait
	cmpa.l	ser_rx_buff_tail,a0
	beq	.wait	; empty buffer
	
	move.b	(a0)+,d1
	cmpa.l	#ser_rx_buff_end,a0
	bne	.no_wrap
	
	movea.l	#ser_rx_buff,a0
	
.no_wrap
	move.l	a0,ser_rx_buff_head
	
	move.w	a0,d0
	sub.b	ser_rx_buff_tail+3,d0
	cmp.b	#40,d0
	bne	.no_xon	
	
	move.b	#xon,ser_rx_flow
	move.b	#%00000111,(DUART0+DUART_IMR)
	
.no_xon
	move.b	d1,d0
	movem.l	(a7)+,a0/d1
	rts
	
flush_serial:
	; block until DUART is done sending characters
	
	; TODO: also need to wait for TX fifo to be empty
	
	btst	#DUART_SR_TXEMT_BIT,(DUART0+DUART_SRA)
	beq	flush_serial
	rts
	
init_mon_ram:
	lea.l	(mon_ram),a0
	move.w	#((input_buff-mon_ram)/2-1),d0
.loop:
	clr.w	(a0)+
	dbra	d0,.loop
	
	move.l	#user_stack,user_sp
	move.l	#rambase+$400,user_pc
	move.l	#rambase,mon_pointer
	
	move.w	sr,(user_sr)

	rts
	
init_rtc:
	bset	#0,(RTC+RTC_CE)	; mask interrupts
	rts
	
init_pport
	tst.w	PPORT	; should clear printer port IRQ
	rts
	


ver:
	movea.l	#.ver_string,a0	; setup to print version string
	jmp	puts	; puts will return for us

.ver_string
	dc.b	"Joe-Mon",cr,lf
	dc.b	"A Machine Language Monitor written for the Beckman DU600 spectrophotometer and other 68K based machines.",cr,lf

	ifd	ram_version
	dc.b	"RAM version",cr,lf
	endif

	dc.b	"Written by Joseph Zatarski",cr,lf
	dc.b	"Copyright 2014-2017",cr,lf
	dc.b	"Version 2",cr,lf
	dc.b	cr,lf
	dc.b	"This free software is licensed under the GPL. If you paid for this software or source, you were probably ripped off.",cr,lf,0
	
	align 1

load_srec:
; this is a routine to accept an s-record over the serial port
; it interprets s1,s2,s3,s7,s8, and s9 records
; it also accepts, but does not use, any other s-record types
	
	movea.l	#.ready,a0	; setup to print ready string
	jsr	puts

.next_record
	clr.b	d3	; clear the checksum
	clr.l	d5	; clear address

.wait_for_record
	jsr	getc
	cmp.b	#'S',d0	; wait for an 'S' to indicate start of a record
	beq	.record_start
	cmp.b	#esc,d0	; escape is for abort
	beq	.abort
	jmp	.wait_for_record

.record_start
	jsr	getc

	cmp.b	#esc,d0
	beq	.abort

	move.b	d0,d4	; store record type number in d4 for now

	jsr	get_hex_byte
	bcs	.bad_char

	move.b	d0,d2	; put the count in d2
	add.b	d2,d3	; update checksum

	cmp.b	#'1',d4	; S1 data record
	beq	.s1_rec

	cmp.b	#'2',d4	; S2 data record
	beq	.s2_rec

	cmp.b	#'3',d4	; S3 data record
	beq	.s3_rec

	cmp.b	#'7',d4	; S7 termination record
	beq	.s7_rec

	cmp.b	#'8',d4	; S8 termination record
	beq	.s8_rec

	cmp.b	#'9',d4	; S9 termination record
	beq	.s9_rec

	jmp	.wait_for_cr	; other records not interpreted

.s3_rec
	jsr	get_hex_byte
	bcs	.bad_char

	move.b	d0,d5	; add to address
	add.b	d0,d3	; add to checksum

	asl.l	#8,d5	; shift address to left by one byte

	subq.b	#1,d2	; decrement count

.s2_rec
	jsr	get_hex_byte
	bcs	.bad_char

	move.b	d0,d5	; add to address
	add.b	d0,d3	; add to checksum

	asl.l	#8,d5	; shift address left by one byte

	subq.b	#1,d2	; decrement count

.s1_rec
	jsr	get_hex_byte
	bcs	.bad_char

	move.b	d0,d5	; add to address
	add.b	d0,d3	; add to checksum

	asl.l	#8,d5	; shift address left one byte

	jsr	get_hex_byte
	bcs	.bad_char

	move.b	d0,d5	; add to address
	add.b	d0,d3	; add to checksum

	movea.l	d5,a0	; transfer address to a0

	subq.b	#2,d2	; decrement count

.next_data
	jsr	get_hex_byte

	add.b	d0,d3	; add to checksum

	subq.b	#1,d2	; decrement counter

	beq	.checksum	; if counter is 0, then we have just got the checksum

	move.b	d0,(a0)+	; otherwise, put the data in memory
	jmp	.next_data

.checksum
	addq.b	#1,d3	; the sum of error-free data and the checksum should be #$FF, so add 1 to get 0
	bne	.record_error

	move.b	#'O',d0	; circle for error free record
	jsr	putc

.wait_for_cr
	jsr	getc	; wait for carriage return before we look for the next record
	cmp.b	#cr,d0
	bne	.wait_for_cr
	jmp	.next_record

.record_error
	movea.l	#.bad_checksum_msg,a0
	jsr	puts	; print bad checksum error
	jmp	.wait_for_esc

.s7_rec
.s8_rec
.s9_rec
	jsr	getc	; wait for carriage return for now
	cmp.b	#cr,d0	; this will do something later
	bne	.s9_rec

	move.b	#cr,d0
	jsr	putc
	move.b	#lf,d0
	jmp	putc

.bad_char
	movea.l	#.bad_char_msg,a0
	jsr	puts	; print invalid character message

	jmp	.wait_for_esc

.wait_for_esc
	movea.l	#.wait_for_esc_msg,a0
	jsr	puts	; instruct user to press escape

.wait_for_esc2
	jsr	getc
	cmp.b	#esc,d0
	bne	.wait_for_esc2

.abort
	rts

.ready	dc.b	"Ready to accept S-record:",cr,lf,0
.bad_char_msg	dc.b	cr,lf,"Encountered invalid character.",cr,lf,0
.bad_checksum_msg	dc.b	cr,lf,"Bad checksum.",cr,lf,0
.wait_for_esc_msg	dc.b	"Please press escape to continue.",cr,lf,0

	align 1

get_hex_byte:
; gets a pair of ascii characters and converts them into a byte
; returns byte in d0
; uses d1
; calls getc (d0)
; returns with carry set if either character was not valid

	jsr	getc
	move.b	d0,d1
	jsr	getc

	cmp2.b	digitbound,d0
	bcc	.d0_digit	; if it's within the digit bounds, it's a numeral

	cmp2.b	hexletterbound,d0
	bcs	.badchar	; if it's out of bounds, it's not a letter or a numeral

	sub.b	#('A'-10),d0	; convert A-F to 10-15
	jmp	.d1_conv

.d0_digit
	sub.b	#'0',d0	; convert chars 0-9 to numbers 0-9

.d1_conv
	cmp2.b	digitbound,d1
	bcc	.d1_digit	; in bounds? then numeral

	cmp2.b	hexletterbound,d1
	bcs	.badchar	; out of bounds? bad hex digit

	sub.b	#('A'-10),d1	; convert A-F to 10-15
	jmp	.combine

.d1_digit
	sub.b	#'0',d1	; convert chars 0-9 to numbers 0-9

.combine
	asl.b	#4,d1	; shift the MSN over by one nibble (4 bits)
	add.b	d1,d0	; add MSN and LSN to combine into a byte in d0

	andi	#%11111110,ccr	; make sure carry is clear

.badchar	; the branches to here are bcs, so we exit with carry set if we go here
	rts

jump:
	; takes one argument:
	; address to jump to
	; array of argument pointers passed by address in a6
	; arg count passed in d7
	
	cmpi.l	#2,d7	; check if we have right number of args
	bne	.wrong_args
	
	movea.l	(4,a6),a0	; get address of first argument (skipping command string)
	jsr	string_to_long
	bcs	.bad_args	; if string_to_long had an issue parsing, report it
	movea.l	d0,a0	; put address from argument in a0 for the boot address

	jmp	(a0)

.wrong_args
	movea.l	#.usage_string,a0
	jmp	puts	; puts will return for us
	
.bad_args
	movea.l	#.arg_parse_error_string,a0
	jmp	puts	; puts will return for us
	
.usage_string
	dc.b	"Incorrect number of arguments.",cr,lf
	dc.b	"Usage: jump <address>",cr,lf
	dc.b	"address is a hexadecimal address corresponding to the address to boot.",cr,lf,0
	
.arg_parse_error_string
	dc.b	"Error parsing address argument.",cr,lf,0
	
	align 1
	
boot:	
	; takes one argument:
	; address to boot from
	; array of argument pointers passed by address in a6
	; arg count passed in d7
	
	cmpi.l	#2,d7	; check if we have right number of args (command + 1 argument)
	bne	.wrong_args
	
	movea.l	(4,a6),a0	; get address of first argument (skipping command string)
	jsr	string_to_long
	bcs	.bad_args	; if string_to_long had an issue parsing, report it
	movea.l	d0,a0	; put address from argument in a0 for the boot address

	andi	#%0011111111111111,sr	; clear trace bits
	ori	#%0000011100000000,sr	; disable interrupts
	movec	a0,vbr	; set vector base register
	movea.l	(a0),a7	; set supervisor stack register
	movea.l	(4,a0),a0	; jmp to reset vector
	
	jsr	flush_serial	; flush DUART before we jump
	
	jmp	(a0)

.wrong_args
	movea.l	#.usage_string,a0
	jmp	puts	; puts will return for us
	
.bad_args
	movea.l	#.arg_parse_error_string,a0
	jmp	puts	; puts will return for us
	
.usage_string
	dc.b	"Incorrect number of arguments.",cr,lf
	dc.b	"Usage: boot <address>",cr,lf
	dc.b	"address is a hexadecimal address corresponding to the address to boot.",cr,lf,0
	
.arg_parse_error_string
	dc.b	"Error parsing address argument.",cr,lf,0
	
	align 1

string_to_long:
	; gets a 32 bit number from a string
	; pass string address in a0
	; returns number from string in d0
	; destroys d1
	; returns with carry set if there's a problem

	clr.l	d0	; clear number
	
.next_digit
	move.b	(a0)+,d1	; get character
	beq	.done	; null means we're done, also carry won't be set here
	cmp2.b	digitbound,d1	; check if ASCII character is a numeral
	bcc	.isdigit
	cmp2.b	lowercasebound,d1	; check if ASCII character is lowercase
	bcs	.not_lowercase
	
	bclr	#5,d1	; change to uppercase

.not_lowercase
	cmp2.b	hexletterbound,d1	; check if ASCII character is a hex letter
	bcc	.isletter
	rts	; if it's not a digit or a letter, then we have encountered a bad hex digit, and we return with carry set

.isdigit
	sub.b	#'0',d1	; convert ascii 0-9 to numerical 0-9
	jmp	.addnibble
	
.isletter
	sub.b	#('A'-10),d1	; convert ascii A-F to be numerical 10-15
	
.addnibble
	cmp.l	#$0FFFFFFF,d0	; check if current number is too big to add another nibble
	bhi	.badnumber
	
	asl.l	#4,d0	; make space for new nibble
	add.b	d1,d0	; put new nibble on right side
	
	jmp	.next_digit
	
.badnumber
	ori	#carry_flag,CCR	; set carry and fall through to done

.done
	rts
	
regs:
	; displays contents of user registers
	movea.l	#.reg_string_1,a0
	jsr puts
	
	movea.l	#user_data,a1
	moveq	#1,d4
.next_line
	moveq	#7,d3

.next_data
	move.l	(a1)+,d1
	jsr	put_long
	jsr	putsp
	dbmi	d3,.next_data
	
	dbmi	d4,.addr_line
	jmp	.proc_stat
	
.addr_line
	movea.l	#.reg_string_2,a0
	jsr	puts
	jmp	.next_line

.proc_stat
	movea.l	#.reg_string_3,a0
	jsr	puts
	move.l	user_pc,d1
	jsr	put_long
	jsr	putsp
	move.w	user_sr,d1
	jsr	put_word
	jsr	putsp
	move.w	user_sr,d1
	moveq	#4,d2
.ccr_loop
	btst	d2,d1
	beq	.cleared
	move.b	#'+',d0
	jmp	.put
.cleared
	move.b	#'-',d0
.put
	jsr	putc
	dbmi	d2,.ccr_loop

	jsr	putnl
	
	rts

.reg_string_1
	dc.b	"D0       D1       D2       D3       D4       D5       D6       D7",cr,lf,0

.reg_string_2
	dc.b	cr,lf,"A0       A1       A2       A3       A4       A5       A6       A7",cr,lf,0
	
.reg_string_3
	dc.b	cr,lf,"PC       SR   XNZVC",cr,lf,0

	align 1
	
put_long:
	; prints the long contents of d1 in hexadecimal
	; trashes d0
	; calls put_nibble
	
	swap	d1	; put most significant word where least significant word was
	jsr	put_word	; put word only trashes LSW of d1
	swap	d1	; put LSW back and fall through to put_word

	
put_word:
	; prints the word contents of d1 in hexadecimal
	; trashes d0
	; calls put_nibble
	
	rol.w	#8,d1	; put most significant byte where least significant byte was
	jsr	put_byte	; put byte only trashes low byte of d1
	rol.w	#8,d1	; put LSB back where it should be and fall through to put_byte
	
put_byte:
	; prints the byte contents of d1 in hexadecimal
	; trashes d0
	; calls put_nibble
	move.b	d1,d0
	rol.b	#4,d0	; put most significant nibble where least significant nibble was
	jsr	put_nibble
	move.b	d1,d0	; this will print the next nibble, by falling through to put_nibble
	
put_nibble:
	; prints low nibble of d0 (destroys d0)
	andi.b	#$F,d0	; get nibble
	addi.b	#'0',d0	; convert to ASCII digit
	cmpi.b	#'9',d0
	bls	.digit	; if it's not higher than '9' then it's a digit
	addi.b	#('A'-'9'-1),d0	; converts to ASCII hex letter
.digit
	jmp	putc	; putc will return for us
	
disassemble:
	; attempt to disassemble instruction at PC
	; later will support disassembling multiple instructions, and at arbitrary addresses
	; since disassembly is going to get a bit complex, we may use stack-based local variables here
	; we're going to try just using registers though:
	; a6 - points to current instruction
	; d6 - holds instruction word
	; a0,a1,d0,d1 - scratch registers since they are used to pass parameters to so many things
	movea.l	user_pc,a6
	move.l	a6,d1
	jsr	put_long	; print address
	
	movea.l #.addr_suffix,a0
	jsr	puts
	
	move.w	(a6)+,d6
	move.w	d6,d1
	jsr	put_word	; print instruction word
	
	move.b	#' ',d0
	jsr	putc
	
	andi.w	#$F000,d1	; mask off all but upper 4 bits, which are always part of instruction encoding
	
	lsr.w	#8,d1	; have to do 2 shifts because we can only shift up to 8
	lsr.w	#2,d1	; turn it into an address offset
	
	movea.l	#.dis_addr,a0
	movea.l	(a0,d1.w),a0	; get address of disassembly table

.loop	;loop through disassembly table
	move.w	(a0)+,d0	; get AND mask
	beq	.match	; if 0, hit function pointer entry
	
	and.w	d6,d0	; apply AND mask
	cmp2.w	(a0),d0	; check the range
	bcs	.out	; carry set means out of bounds
	addq.l	#4,a0	; point to inside address
	movea.l	(a0),a0	; get next entry
	jmp	.loop
	
.out
	addq.l	#8,a0	; point to next entry
	jmp	.loop
	
.match	; found a match
	movea.l	(a0),a0	; get the address in a0
	jsr	(a0)
	
.done
	move.l	a6,user_pc
	rts
	
.addr_suffix
	dc.b	": ",0
	
	align 1
	
.dis_addr	; list of tables to disassemble each group of instructions
	dc.l	dis_table0	;0
	dc.l	dis_table1	; move.b
	dc.l	dis_table2	; move.l
	dc.l	dis_table2	; reuse table two for word size
	
	dc.l	dis_table4	;4
	dc.l	dis_tableF
	dc.l	dis_tableF
	dc.l	dis_tableF
	
	dc.l	dis_tableF	;8
	dc.l	dis_tableF
	dc.l	dis_tableF
	dc.l	dis_tableF
	
	dc.l	dis_tableF	;C
	dc.l	dis_tableF
	dc.l	dis_tableF
	dc.l	dis_tableF
	
; instruction signature decision tree
; tree of checks to determine instruction
; (well, not strictly a tree since nothing prevents a node having multiple parents, not to mention this scenario is also a useful one)
; first word is an AND mask for the instruction word
; second and third words are _inclusive_ bounds for CMP2. lower bound is followed by the upper bound
; next comes the 'inside' address (points to next entry if CMP2 indicated inside)
; afterwards, and last, comes the 'outside' address (points to next entry if CMP2 indicated outside)

; Additionally, if the AND mask is 0, then the entry is a function pointer to a subroutine designed to handle the instruction
; this entry need only be three words long.
	
dis_table0	; group 0 instruction decoding
	; almost all of these fit into the single EA format, none allow An direct mode, except movep
	dc.w	%0000000000111000	; filter out An modes
	dc.w	%0000000000001000	; movep looks like it has An EA
	dc.w	%0000000000001000
	dc.l	.movep_node
	
	dc.w	%0000000000111111	; filter out some other (invalid) modes while we're at it
	dc.w	%0000000000111101
	dc.w	%0000000000111111
	dc.l	dis_unk_node
	
	dc.w	%0000000111000000	; check for btst Dn
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.l	.btst
	
	dc.w	%0000111111000000	; check for btst #
	dc.w	%0000100000000000
	dc.w	%0000100000000000
	dc.l	.btst_static
	
	dc.w	%0000111111000000	; check for cmpi (checks size field as well)
	dc.w	%0000110000000000
	dc.w	%0000110010000000
	dc.l	.cmpi
	
	dc.w	%0000100111000000	; check for cmp2/chk2
	dc.w	%0000000011000000
	dc.w	%0000000011000000
	dc.l	.cmp2chk2
	
	dc.w	%0000000000111111	; filter out (d16,PC) and (bd,PC,Xn)
	dc.w	%0000000000111010
	dc.w	%0000000000111011
	dc.l	dis_unk_node
	
	dc.w	%0000000011111111	; filter out long-word access to SR register
	dc.w	%0000000010111100
	dc.w	%0000000010111100
	dc.l	dis_unk_node
	
	dc.w	%0000111111000000	; ori
	dc.w	%0000000000000000
	dc.w	%0000000010000000
	dc.l	.ori
	
	dc.w	%0000111111000000	; andi
	dc.w	%0000001000000000
	dc.w	%0000001010000000
	dc.l	.andi
	
	dc.w	%0000111111000000	; eori
	dc.w	%0000101000000000
	dc.w	%0000101010000000
	dc.l	.eori
	
	dc.w	%0000000000111111	; filter out immediate mode
	dc.w	%0000000000111100
	dc.w	%0000000000111100
	dc.l	dis_unk_node
	
	dc.w	%0000000111000000	; BCHG/BCLR/BSET Dn
	dc.w	%0000000101000000
	dc.w	%0000000111000000
	dc.l	.bxxx
	
	dc.w	%0000111111000000	; BCHG/BCLR/BSET #
	dc.w	%0000100001000000
	dc.w	%0000100011000000
	dc.l	.bxxx
	
	dc.w	%0000110111000000	; ADDI/SUBI
	dc.w	%0000010000000000
	dc.w	%0000010010000000
	dc.l	.addsubi
	
	dc.w	%0000000000111000	; filter out Dn mode
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000111111000000	; moves
	dc.w	%0000111000000000
	dc.w	%0000111010000000
	dc.l	.moves
	
	dc.w	0
	dc.l	dis_unk
	
.movep_node
	dc.w	%0000000100111000	; movep check
	dc.w	%0000000100001001
	dc.w	%0000000100000111
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_movep	; movep confirmed

.cmp2chk2
	dc.w	%0000011000000000	; make sure SIZ is valid
	dc.w	%0000011000000000
	dc.w	%0000011000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000011000	; check for Dn and -(An) which are not allowed
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000111000	; check for (An)+ mode
	dc.w	%0000000000011000
	dc.w	%0000000000011000
	dc.l	dis_unk_node

	dc.w	0
	dc.l	dis_cmp2chk2	; CMP2/CHK2
	
.ori
	dc.w	0
	dc.l	dis_ori
	
.andi
	dc.w	0
	dc.l	dis_andi	;dis_andi
	
.eori
	dc.w	0
	dc.l	dis_eori	;eori
	
.btst_static
	dc.w	%0000000000111111	; filter out immediate mode for ea
	dc.w	%0000000000111100
	dc.w	%0000000000111100
	dc.l	dis_unk_node

.btst
	dc.w	0
	dc.l	dis_btst	; BTST
	
.cmpi
	dc.w	%0000000000111111
	dc.w	%0000000000111100
	dc.w	%0000000000111100
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_cmpi
	
.bxxx	; BCHG/BCLR/BSET filtering
	dc.w	%0000000011000000
	dc.w	%0000000001000000
	dc.w	%0000000001000000
	dc.l	.bchg
	
	dc.w	%0000000011000000
	dc.w	%0000000010000000
	dc.w	%0000000010000000
	dc.l	.bclr
	
	; fall through to bset
	
.bset
	dc.w	0
	dc.l	dis_bset	; bset
	
.bchg
	dc.w	0
	dc.l	dis_bchg	; bchg

.bclr
	dc.w	0
	dc.l	dis_bclr	; bclr
	
.addsubi
	dc.w	%0000001000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	.subi
	
	dc.w	0
	dc.l	dis_addi	; addi
	
.subi
	dc.w	0
	dc.l	dis_subi	; subi
	
.moves
	dc.w	0
	dc.l	dis_moves	; moves
	
dis_table1	; table of instruction signatures for group 1 (MOVE.B)
	dc.w	%0000000111000000	; this and the next filter out An register byte accesses (not allowed)
	dc.w	%0000000001000000
	dc.w	%0000000001000000
	dc.l	dis_unk_node

	dc.w	%0000000000111000
	dc.w	%0000000000001000
	dc.w	%0000000000001000
	dc.l	dis_unk_node	; after here, we've covered all specific cases of move.b
				; so fall into the table for group 2
	
dis_table2	; table of instruction signatures for group 2 (reused for group 3, both MOVE)
	dc.w	%0000000111000000	; check for mode 7 destination
	dc.w	%0000000000000000
	dc.w	%0000000110000000
	dc.l	.check_src_ea	; go here if not mode 7
	
	dc.w	%0000111000000000
	dc.w	%0000010000000000
	dc.w	%0000111000000000
	dc.l	dis_unk_node	; go here if not allowed mode
	
.check_src_ea
	dc.w	%0000000000111111	; filter out some invalid modes
	dc.w	%0000000000111101
	dc.w	%0000000000111111
	dc.l	dis_unk_node
	
.dis_move_node
	dc.w	0
	dc.l	dis_move	; just pass on to move
	
dis_table4	; table of instructions signatures for group 4
	dc.w	%1111111111111111
	dc.w	%0100101011111010	; BGND
	dc.w	%0100101011111010
	dc.l	.bgnd_node
	
	dc.w	%1111111111111111	; ILLEGAL
	dc.w	%0100101011111100
	dc.w	%0100101011111100
	dc.l	.illegal_node
	
	dc.w	%1111111111111111	; RESET
	dc.w	%0100111001110000
	dc.w	%0100111001110000
	dc.l	.reset_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001110001
	dc.w	%0100111001110001
	dc.l	.nop_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001110010
	dc.w	%0100111001110010
	dc.l	.stop_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001110011
	dc.w	%0100111001110011
	dc.l	.rte_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001110100
	dc.w	%0100111001110100
	dc.l	.rtd_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001110101
	dc.w	%0100111001110101
	dc.l	.rts_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001110110
	dc.w	%0100111001110110
	dc.l	.trapv_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001110111
	dc.w	%0100111001110111
	dc.l	.rtr_node
	
	dc.w	%1111111111111000
	dc.w	%0100100000001000
	dc.w	%0100100000001000
	dc.l	.link_long_node
	
	dc.w	%1111111111111000
	dc.w	%0100111001010000
	dc.w	%0100111001010000
	dc.l	.link_word_node
	
	dc.w	%1111111111111000
	dc.w	%0100111001011000
	dc.w	%0100111001011000
	dc.l	.unlk_node
	
	dc.w	%1111111111111000
	dc.w	%0100100001000000
	dc.w	%0100100001000000
	dc.l	.swap_node
	
	dc.w	%1111111111111000
	dc.w	%0100111001101000
	dc.w	%0100111001101000
	dc.l	.move_from_usp_node
	
	dc.w	%1111111111111000
	dc.w	%0100111001100000
	dc.w	%0100111001100000
	dc.l	.move_to_usp_node
	
	dc.w	%1111111111111000
	dc.w	%0100100001001000
	dc.w	%0100100001001000
	dc.l	.bkpt_node
	
	dc.w	%1111111111110000
	dc.w	%0100111001000000
	dc.w	%0100111001000000
	dc.l	.trap_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001111010
	dc.w	%0100111001111010
	dc.l	.movec_to_general_node
	
	dc.w	%1111111111111111
	dc.w	%0100111001111011
	dc.w	%0100111001111011
	dc.l	.movec_to_control_node
	
	dc.w	%1111111111111000
	dc.w	%0100100010000000
	dc.w	%0100100010000000
	dc.l	.ext_word_node
	
	dc.w	%1111111111111000
	dc.w	%0100100011000000
	dc.w	%0100100011000000
	dc.l	.ext_long_node
	
	dc.w	%1111111111111000
	dc.w	%0100100111000000
	dc.w	%0100100111000000
	dc.l	.extb_long_node
	
	dc.w	%0000000000111111	; filter bad EA modes
	dc.w	%0000000000111101
	dc.w	%0000000000111111
	dc.l	dis_unk_node
	
	dc.w	%0000111111000000
	dc.w	%0000101000000000
	dc.w	%0000101010000000
	dc.l	.tst_node
	
	dc.w	%0000000000111000
	dc.w	%0000000000001000
	dc.w	%0000000000001000
	dc.l	dis_unk_node
	
	dc.w	%1111111111000000
	dc.w	%0100010011000000
	dc.w	%0100010011000000
	dc.l	.move_to_ccr_node
	
	dc.w	%1111111111000000
	dc.w	%0100011011000000
	dc.w	%0100011011000000
	dc.l	.move_to_sr_node
	
	dc.w	%0000111111000000	; covers both mulu and muls
	dc.w	%0000110000000000
	dc.w	%0000110000000000
	dc.l	.mul_long_node
	
	dc.w	%0000000000111111	; filter out immediate mode
	dc.w	%0000000000111100
	dc.w	%0000000000111100
	dc.l	dis_unk_node
	
	dc.w	%0000000101000000	; CHK
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.l	.chk_node
	
	dc.w	%0000111110000000	; MOVEM <ea>,regs
	dc.w	%0000110010000000
	dc.w	%0000110010000000
	dc.l	.movem_to_reg_node
	
	dc.w	%0000000111000000	; LEA
	dc.w	%0000000111000000
	dc.w	%0000000111000000
	dc.l	.lea_node
	
	dc.w	%0000111111000000	; JSR
	dc.w	%0000111010000000
	dc.w	%0000111010000000
	dc.l	.jsr_node
	
	dc.w	%0000111111000000	; JMP
	dc.w	%0000111011000000
	dc.w	%0000111011000000
	dc.l	.jmp_node
	
	dc.w	%0000111111000000	; PEA
	dc.w	%0000100001000000
	dc.w	%0000100001000000
	dc.l	.pea_node
	
	dc.w	%0000000000111111	; filter out PC relative modes
	dc.w	%0000000000111010
	dc.w	%0000000000111011
	dc.l	dis_unk
	
	dc.w	%0000111111000000	; NEGX
	dc.w	%0000000000000000
	dc.w	%0000000010000000
	dc.l	.negx_node
	
	dc.w	%0000111111000000	; NEG
	dc.w	%0000010000000000
	dc.w	%0000010010000000
	dc.l	.neg_node
	
	dc.w	%0000111111000000	; NBCD
	dc.w	%0000100000000000
	dc.w	%0000100000000000
	dc.l	.nbcd_node
	
	dc.w	%0000111111000000	; NOT
	dc.w	%0000011000000000
	dc.w	%0000011010000000
	dc.l	.not_node
	
	dc.w	%0000111111000000	; CLR
	dc.w	%0000001000000000
	dc.w	%0000001010000000
	dc.l	.clr_node
	
	dc.w	%0000111111000000	; TAS
	dc.w	%0000101011000000
	dc.w	%0000101011000000
	dc.l	.tas_node
	
	dc.w	%0000111111000000	; move SR,xxx
	dc.w	%0000000011000000
	dc.w	%0000000011000000
	dc.l	.move_from_sr_node
	
	dc.w	%0000111111000000	; move CCR,xxx
	dc.w	%0000001011000000
	dc.w	%0000001011000000
	dc.l	.move_from_ccr_node
	
	dc.w	%0000000000111000	; filter Dn mode
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000111000	; filter (An)+ mode
	dc.w	%0000000000011000
	dc.w	%0000000000011000
	dc.l	dis_unk_node
	
	dc.w	%0000111110000000	; movem regs,<ea>
	dc.w	%0000100010000000
	dc.w	%0000100010000000
	dc.l	.movem_from_reg_node
		
	dc.w	0
	dc.l	dis_unk
	
.move_from_ccr_node
	dc.w	0
	dc.l	dis_move_from_ccr
	
.move_from_sr_node
	dc.w	0
	dc.l	dis_move_from_sr
	
.tas_node
	dc.w	0
	dc.l	dis_tas

.clr_node
	dc.w	0
	dc.l	dis_clr

.not_node
	dc.w	0
	dc.l	dis_not
	
.nbcd_node
	dc.w	0
	dc.l	dis_nbcd
	
.neg_node
	dc.w	0
	dc.l	dis_neg
	
.negx_node
	dc.w	0
	dc.l	dis_negx
	
.pea_node
	dc.w	%0000000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000111000
	dc.w	%0000000000011000
	dc.w	%0000000000100000
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_pea
	
.jmp_node
	dc.w	%0000000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000111000
	dc.w	%0000000000011000
	dc.w	%0000000000100000
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_jmp
	
.jsr_node
	dc.w	%0000000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000111000
	dc.w	%0000000000011000
	dc.w	%0000000000100000
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_jsr
	
.lea_node
	dc.w	%0000000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000111000
	dc.w	%0000000000011000
	dc.w	%0000000000100000
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_lea
	
.movem_from_reg_node
	dc.w	0
	dc.l	dis_movem_from_reg
	
.movem_to_reg_node
	dc.w	%0000000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.l	dis_unk_node
	
	dc.w	%0000000000111000
	dc.w	%0000000000100000
	dc.w	%0000000000100000
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_movem_to_reg
	
.chk_node
	dc.w	0
	dc.l	dis_chk
	
.mul_long_node
	dc.w	0
	dc.l	dis_mul_long
	
.move_to_sr_node
	dc.w	0
	dc.l	dis_move_to_sr
	
.move_to_ccr_node
	dc.w	0
	dc.l	dis_move_to_ccr
	
.tst_node
	dc.w	%0000000011111000	; byte test on An not allowed
	dc.w	%0000000000001000
	dc.w	%0000000000001000
	dc.l	dis_unk_node
	
	dc.w	0
	dc.l	dis_tst
	
.ext_long_node
	dc.w	0
	dc.l	dis_ext_l
	
.ext_word_node
	dc.w	0
	dc.l	dis_ext_w
	
.extb_long_node
	dc.w	0
	dc.l	dis_extb_l
	
.movec_to_general_node
	dc.w	0
	dc.l	dis_movec_to_Xn
	
.movec_to_control_node
	dc.w	0
	dc.l	dis_movec_from_Xn
	
.trap_node
	dc.w	0
	dc.l	dis_trap
	
.bkpt_node
	dc.w	0
	dc.l	dis_bkpt
	
.move_to_usp_node
	dc.w	0
	dc.l	dis_move_to_usp
	
.move_from_usp_node
	dc.w	0	; MOVE USP,An
	dc.l	dis_move_from_usp
	
.swap_node
	dc.w	0	; SWAP
	dc.l	dis_swap
	
.unlk_node
	dc.w	0	; UNLK
	dc.l	dis_unlk
	
.link_word_node
	dc.w	0	; LINK.W
	dc.l	dis_link_word
	
.link_long_node
	dc.w	0	; LINK.L
	dc.l	dis_link_long
	
.rtr_node
	dc.w	0	; rtr
	dc.l	dis_rtr
	
.trapv_node
	dc.w	0	; trapv
	dc.l	dis_trapv

.rts_node
	dc.w	0	; rts
	dc.l	dis_rts
	
.rtd_node
	dc.w	0	; rtd
	dc.l	dis_rtd
	
.rte_node
	dc.w	0	; rte
	dc.l	dis_rte
	
.stop_node
	dc.w	0	; stop
	dc.l	dis_stop
	
.nop_node
	dc.w	0	; nop
	dc.l	dis_nop

.reset_node
	dc.w	0	; reset
	dc.l	dis_reset
	
.illegal_node
	dc.w	0	; illegal
	dc.l	dis_illegal
	
.bgnd_node
	dc.w	0
	dc.l	dis_bgnd	; bgnd
	
dis_tableF
	
dis_unk_node	; used a lot, so made it a global label rather than local
	dc.w	0
	dc.l	dis_unk
	
dis_tas
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea_no_siz
	
.msg
	dc.b	"TAS ",0
	
	align 1
	
dis_clr
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea
	
.msg
	dc.b	"CLR.",0
	
	align 1
	
dis_not
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea
	
.msg
	dc.b	"NOT.",0
	
	align 1
	
dis_nbcd
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea_no_siz
	
.msg
	dc.b	"NBCD ",0
	
	align 1
	
dis_neg
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea
	
.msg
	dc.b	"NEG.",0
	
	align 1
	
dis_negx
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea
	
.msg
	dc.b	"NEGX.",0
	
	align 1
	
dis_pea
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea_no_siz
	
.msg
	dc.b	"PEA ",0
	
	align 1
	
dis_jmp
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea_no_siz
	
.msg
	dc.b	"JMP ",0
	
	align 1
	
dis_jsr
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea_no_siz
	
.msg
	dc.b	"JSR ",0
	
	align 1
	
dis_lea
	movea.l	#.msg,a0
	jsr	puts
	
	movea.l	a6,a4
	jsr	dis_ea_std
	
	movea.l	#.separator_msg,a0
	jsr	puts
	
	move.w	d6,d0
	rol.w	#7,d0
	jsr	put_nibble
	
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"LEA ",0
	
.separator_msg
	dc.b	",A",0
	
	align 1
	
dis_movem_from_reg
	movea.l	#.msg,a0
	jsr	puts
	
	btst	#6,d6
	beq	.word
	
	move.b	#'L',d0
	move.b	#2,d4
	jmp	.regs
	
.word
	move.b	#'W',d0
	move.b	#1,d4
	
.regs
	jsr	putc
	
	jsr	putsp
	
	move.w	(a6)+,d3
	
	move.w	d6,d5
	andi.w	#%111000,d5
	cmpi.b	#%100000,d5
	beq	.reverse_regs
	
	jsr	dis_reg_mask
	jmp	.ea
	
.reverse_regs
	jsr	dis_reg_mask_reverse
	
.ea
	move.b	#',',d0
	jsr	putc
	
	movea.l	a6,a4
	jsr	dis_ea_std
	
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	move.w	(-2,a4),d1
	jsr	put_word
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"MOVEM.",0
	
	align 1
	
dis_movem_to_reg
	movea.l	#.msg,a0
	jsr	puts
	
	btst	#6,d6
	beq	.word
	
	move.b	#'L',d0
	move.b	#2,d4
	jmp	.ea
	
.word
	move.b	#'W',d0
	move.b	#1,d4
	
.ea
	jsr	putc
	
	jsr	putsp
	
	tst.w	(a6)+
	
	movea.l	a6,a4
	jsr	dis_ea_std
	
	move.b	#',',d0
	jsr	putc
	
	move.w	(-2,a4),d3	; get reg mask
	
	jsr	dis_reg_mask
	
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	move.w	(-2,a4),d1
	jsr	put_word
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"MOVEM.",0
	
	align 1
	
dis_reg_mask
; disassemble a register mask in D3 (for MOVEM) of the normal kind
; (not predecrement)
; destroys d3-d0
; d3 is the reg mask
; d2 is register counter
; d1 is a flag that is set when we print a register
; (used to prevent leading /) 
	clr.b	d1
	clr.b	d2

.data_loop	
	btst	#0,d3
	beq	.skip_data
	
	tst.b	d1
	beq	.skip_data_slash
	
	move.b	#'/',d0
	jsr	putc
	
.skip_data_slash
	move.b	#'D',d0
	jsr	putc
	
	move.b	d2,d0
	jsr	put_nibble
	
	move.b	#1,d1
	
.skip_data
	lsr	#1,d3
	addq	#1,d2
	cmpi	#8,d2
	bne	.data_loop
	
	clr.b	d2

.addr_loop	
	btst	#0,d3
	beq	.skip_addr
	
	tst.b	d1
	beq	.skip_addr_slash
	
	move.b	#'/',d0
	jsr	putc
	
.skip_addr_slash
	move.b	#'A',d0
	jsr	putc
	
	move.b	d2,d0
	jsr	put_nibble
	
	move.b	#1,d1
	
.skip_addr
	lsr	#1,d3
	addq	#1,d2
	cmpi	#8,d2
	bne	.addr_loop
	
	rts
	
dis_reg_mask_reverse
; disassemble a reverse register mask in D3 (for MOVEM)
; (for predecrement)
; destroys d3-d0
; d3 is the reg mask
; d2 is register counter
; d1 is a flag that is set when we print a register
; (used to prevent leading /) 
	clr.b	d1
	clr.b	d2

.data_loop	
	btst	#15,d3
	beq	.skip_data
	
	tst.b	d1
	beq	.skip_data_slash
	
	move.b	#'/',d0
	jsr	putc
	
.skip_data_slash
	move.b	#'D',d0
	jsr	putc
	
	move.b	d2,d0
	jsr	put_nibble
	
	move.b	#1,d1
	
.skip_data
	lsl	#1,d3
	addq	#1,d2
	cmpi	#8,d2
	bne	.data_loop
	
	clr.b	d2

.addr_loop	
	btst	#15,d3
	beq	.skip_addr
	
	tst.b	d1
	beq	.skip_addr_slash
	
	move.b	#'/',d0
	jsr	putc
	
.skip_addr_slash
	move.b	#'A',d0
	jsr	putc
	
	move.b	d2,d0
	jsr	put_nibble
	
	move.b	#1,d1
	
.skip_addr
	lsl	#1,d3
	addq	#1,d2
	cmpi	#8,d2
	bne	.addr_loop
	
	rts
	
dis_chk
	movea.l	#.msg,a0
	jsr	puts
	
	btst	#7,d6
	beq	.long
	
	move.b	#'W',d0
	move.w	#1,d4
	jmp	.ea
	
.long
	move.w	#2,d4
	move.b	#'L',d0
	
.ea
	jsr	putc
	
	jsr	putsp
	
	movea.l	a6,a4
	jsr	dis_ea_std
	
	movea.l	#.separator,a0
	jsr	puts
	
	move.w	d6,d0
	rol.w	#7,d0
	andi.b	#%111,d0
	jsr	put_nibble
	
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"CHK.",0
	
.separator
	dc.b	",D",0
	
	align 1
	
dis_mul_long
	movea.l	#.msg,a0
	jsr	puts
	
	btst	#3,(a6)
	bne	.signed
	
	move.b	#'U',d0
	jmp	.siz
	
.signed
	move.b	#'S',d0
	
.siz
	jsr	putc
	
	movea.l	#.siz_msg,a0
	jsr	puts
	
	move.w	#2,d4
	tst.w	(a6)+
	movea.l	a6,a4
	jsr	dis_ea_std
	
	movea.l	#.separator_msg,a0
	jsr	puts
	
	btst	#2,(-2,a4)
	beq	.single
	
	move.w	(-2,a4),d0
	andi.w	#7,d0
	jsr	put_nibble
	
	movea.l	#.double_msg,a0
	jsr	puts
	
.single	
	move.b	(-2,a4),d0
	lsr.b	#4,d0
	jsr	put_nibble
	
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	move.w	(-2,a4),d1
	jsr	put_word
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"MUL",0
	
.siz_msg
	dc.b	".L ",0
	
.separator_msg
	dc.b	",D",0
	
.double_msg
	dc.b	":D",0
	
	align 1
	
dis_move_from_sr
	movea.l	#.msg,a0
	jsr	puts
	
	move.w	#1,d4
	movea.l	a6,a4
	jsr	dis_ea_std
	
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"MOVE SR,",0
	
	align 1
	
dis_move_from_ccr
	movea.l	#.msg,a0
	jsr	puts
	
	move.w	#1,d4
	movea.l	a6,a4
	jsr	dis_ea_std
	
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"MOVE CCR,",0
	
	align 1

dis_move_to_sr
	movea.l	#.msg,a0
	jsr	puts
	
	move.w	#1,d4
	movea.l	a6,a4
	jsr	dis_ea_std
	
	movea.l	#.sr_msg,a0
	jsr	puts
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"MOVE ",0
	
.sr_msg
	dc.b	",SR",cr,lf,0
	
	align 1
	
dis_move_to_ccr
	movea.l	#.msg,a0
	jsr	puts
	
	move.w	#1,d4
	movea.l	a6,a4
	jsr	dis_ea_std
	
	movea.l	#.ccr_msg,a0
	jsr	puts
	
	movea.l	a6,a5
	jmp	dis_print_op
	
.msg
	dc.b	"MOVE ",0
	
.ccr_msg
	dc.b	",CCR",cr,lf,0
	
	align 1
	
dis_tst
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_ea

.msg
	dc.b	"TST.",0
	
	align 1
	
dis_single_ea
; dissassembly for instructions with single EA in 0-5, with standard SIZ in 6-7
; and no other operands
	
	move.w	d6,d4
	andi.w	#%0000000011000000,d4
	lsr.w	#6,d4
	jsr	dis_print_siz
	
	jsr	putsp
	
dis_single_ea_no_siz
; same as above, but for unsized instructions
	
	movea.l	a6,a4
	jsr	dis_ea_std
	
	jsr	putnl
	
	movea.l	a6,a5
	jmp	dis_print_op
	
dis_ext_w
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_Xn
	
.msg
	dc.b	"EXT.W D",0
	
	align 1
	
dis_ext_l
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_Xn
	
.msg
	dc.b	"EXT.L D",0
	
dis_extb_l
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_Xn
	
.msg
	dc.b	"EXTB.L D",0
	
	align 1
	
dis_movec_to_Xn
	jsr	dis_movec_msg
	
	jsr	dis_movec_Rc
	
	move.b	#',',d0
	jsr	putc
	
	jsr	dis_movec_Rn
	
	jmp	dis_movec_word2
	
dis_movec_from_Xn
	jsr	dis_movec_msg
	
	jsr	dis_movec_Rn
	
	move.b	#',',d0
	jsr	putc
	
	jsr	dis_movec_Rc
	
	;jmp	dis_movec_word2	; fall through
	
dis_movec_word2
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts

	move.w	(a6)+,d1
	jsr	put_word
	
	jmp	putnl
	
dis_movec_Rc
	move.w	(a6),d0
	andi.w	#%0000111111111111,d0
	
	beq	.sfc
	
	cmpi.w	#$001,d0
	beq	.dfc
	
	cmpi.w	#$002,d0
	beq	.cacr
	
	cmpi.w	#$800,d0
	beq	.usp
	
	cmpi.w	#$801,d0
	beq	.vbr
	
	cmpi.w	#$802,d0
	beq	.caar
	
	cmpi.w	#$803,d0
	beq	.msp
	
	cmpi.w	#$804,d0
	beq	.isp
	
	movea.l	#.unk_msg,a0
	jmp	.print
	
.sfc
	movea.l	#.sfc_msg,a0
	jmp	.print
	
.dfc
	movea.l	#.dfc_msg,a0
	jmp	.print
	
.cacr
	movea.l	#.cacr_msg,a0
	jmp	.print
	
.usp
	movea.l	#.usp_msg,a0
	jmp	.print
	
.vbr
	movea.l	#.vbr_msg,a0
	jmp	.print
	
.caar
	movea.l	#.caar_msg,a0
	jmp	.print
	
.msp
	movea.l	#.msp_msg,a0
	jmp	.print
	
.isp
	movea.l	#.isp_msg,a0
	; fall through
	
.print
	jmp	puts
	
.unk_msg
	dc.b	"<unk>",0
	
.sfc_msg
	dc.b	"SFC",0
	
.dfc_msg
	dc.b	"DFC",0
	
.cacr_msg
	dc.b	"CACR",0
	
.usp_msg
	dc.b	"USP",0
	
.vbr_msg
	dc.b	"VBR",0
	
.caar_msg
	dc.b	"CAAR",0
	
.msp_msg
	dc.b	"MSP",0
	
.isp_msg
	dc.b	"ISP",0
	
	align 1
	
dis_movec_Rn
	btst	#8,(a6)
	beq	.Dn
	
	move.b	#'A',d0
	jmp	.AD_print
	
.Dn
	move.b	#'D',d0
	
.AD_print
	jsr	putc
	
	move.b	(a6),d0
	lsr	#4,d0
	andi.b	#%00000111,d0
	jmp	put_nibble
	
dis_movec_msg
	movea.l	#.movec_msg,a0
	jmp	puts
	
.movec_msg
	dc.b	"MOVEC ",0
	
	align 1
	
dis_trap
	movea.l	#.msg,a0
	jsr	puts
	
	move.b	d6,d0
	jsr	put_nibble
	
	jmp	putnl
	
.msg
	dc.b	"TRAP #",0
	
	align 1
	
dis_bkpt
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_Xn
	
.msg
	dc.b	"BKPT #",0
	
	align 1
	
dis_move_to_usp
	movea.l	#.msg,a0
	jsr	puts
	
	move.b	d6,d0
	andi.b	#7,d0
	jsr	put_nibble
	
	movea.l	#.suffix,a0
	jmp	puts
	
.msg
	dc.b	"MOVE A",0
	
.suffix
	dc.b	",USP",cr,lf,0
	
	align 1
	
dis_move_from_usp
	movea.l	#.msg,a0
	jsr	puts
	
	jmp	dis_single_Xn
	
.msg
	dc.b	"MOVE USP,A",0
	
	align 1
	
dis_swap
	movea.l	#.swap_msg,a0
	jsr	puts
	
	jmp	dis_single_Xn
	
.swap_msg
	dc.b	"SWAP D",0
	
	align 1
	
dis_unlk
	movea.l	#.unlk_msg,a0
	jsr	puts
	
	jmp	dis_single_Xn
	
.unlk_msg
	dc.b	"UNLK A",0
	
	align 1
	
dis_single_Xn
	move.b	d6,d0
	andi.b	#7,d0
	jsr	put_nibble
	
	jmp	putnl
	
dis_link_word
	movea.l	#.link_word_msg,a0
	jsr	puts
	
	move.b	d6,d0
	andi.b	#7,d0
	jsr	put_nibble
	
	movea.l	#.middle,a0
	jsr	puts
	
	move.w	(a6),d1
	jsr	put_d16
	
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	move.w	(a6)+,d1
	
	jsr	put_word
	
	jmp	putnl
	
.link_word_msg
	dc.b	"LINK.W A",0
	
.middle
	dc.b	",#",0
	
	align 1
	
dis_link_long
	movea.l	#.link_long_msg,a0
	jsr	puts
	
	move.b	d6,d0
	andi.b	#7,d0
	jsr	put_nibble
	
	movea.l	#.separator,a0
	jsr	puts
	
	move.l	(a6),d1
	jsr	put_d32
	
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	move.l	(a6)+,d1
	jsr	put_long
	
	jmp	putnl
	
.link_long_msg
	dc.b "LINK.L A",0	
	
.separator
	dc.b ",#",0
	
	align 1
	
dis_rtr
	movea.l	#.rtr_msg,a0
	jmp	puts
	
.rtr_msg
	dc.b	"RTR",cr,lf,0
	
	align 1
	
dis_trapv
	movea.l	#.trapv_msg,a0
	jmp	puts
	
.trapv_msg
	dc.b	"TRAPV",cr,lf,0
	
	align 1
	
dis_rts
	movea.l	#.rts_msg,a0
	jmp	puts
	
.rts_msg
	dc.b	"RTS",cr,lf,0
	
	align 1
	
dis_rtd
	movea.l	#.rtd_msg,a0
	jsr	puts
	
	move.w	(a6),d1
	jsr	put_d16
	
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	move.w	(a6)+,d1
	jsr	put_word
	
	jmp	putnl

.rtd_msg
	dc.b	"RTD #",0
	
	align 1
	
dis_rte
	movea.l	#.rte_msg,a0
	jmp	puts
	
.rte_msg
	dc.b	"RTE",cr,lf,0
	
	align 1
	
dis_stop
	movea.l	#.stop_msg,a0
	jsr	puts
	
	move.w	(a6),d1
	jsr	put_word
	
	jsr	putnl
	
	move.l	#dis_align_msg,a0
	jsr	puts
	
	move.w	(a6)+,d1
	jsr	put_word
	
	jmp	putnl
	
.stop_msg
	dc.b	"STOP #",0
	
	align 1
	
dis_nop
	movea.l	#.nop_msg,a0
	jmp	puts
	
.nop_msg
	dc.b	"NOP",cr,lf,0
	
	align 1
	
dis_reset
	movea.l	#.reset_msg,a0
	jmp	puts
	
.reset_msg
	dc.b	"RESET",cr,lf,0
	
	align 1
	
dis_illegal
	movea.l	#.illegal_msg,a0
	jmp	puts

.illegal_msg	
	dc.b	"ILLEGAL",cr,lf,0
	
	align 1

dis_bgnd
	movea.l	#.bgnd_msg,a0
	jmp	puts
	
.bgnd_msg
	dc.b	"BGND",cr,lf,0
	
	align 1
	
dis_moves
	movea.l	#.moves_msg,a0
	jsr	puts
	
	move.w	d6,d4
	andi.w	#%0000000011000000,d4
	lsr.w	#6,d4
	jsr	dis_print_siz
	
	jsr	putsp
	
	movea.l	a6,a5
	move.w	(a6)+,d1
	btst	#11,d1
	beq	.ea_to_r
	
	; register to ea
	
	btst	#15,d1
	beq	.Dn
	
	move.b	#'A',d0
	jmp	.ea
	
.Dn
	move.b	#'D',d0

.ea
	jsr	putc
	
	move.w	d1,d0
	andi.w	#%0111000000000000,d0
	rol.w	#4,d0
	jsr	put_nibble
	
	move.b	#',',d0
	jsr	putc
	
	movea.l	a6,a4
	jsr	dis_ea_std
	
	jmp	.done
	
.ea_to_r
	movea.l	a6,a4
	jsr	dis_ea_std
	
	move.b	#',',d0
	jsr	putc
	
	move.w	(a5),d1
	btst	#15,d1
	bne	.An
	
	move.b	#'D',d0
	jmp	.reg
	
.An
	move.b	#'A',d0
	
.reg
	jsr	putc
	
	move.w	d1,d0
	andi.w	#%0111000000000000,d0
	rol.w	#4,d0
	jsr	put_nibble

.done
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	move.w	(a5),d1
	jsr	put_word
	
	jsr	putnl
	
	movea.l	a6,a5
	jsr	dis_print_op
	
	rts
	
.moves_msg
	dc.b	"MOVES.",0

	align 1

dis_addi:
	movea.l	#.addi_msg,a0
	jsr	puts
	
	jmp	dis_bit_logic_imm
	
.addi_msg
	dc.b	"ADDI.",0
	
dis_subi:
	movea.l	#.subi_msg,a0
	jsr	puts
	
	jmp	dis_bit_logic_imm
	
.subi_msg
	dc.b	"SUBI.",0
	
dis_cmp2chk2:
	movea.l	a6,a5

	move.w	(a6)+,d1
	
	btst	#11,d1
	beq	.cmp2
	
	movea.l	#.chk2_msg,a0
	jmp	.siz
	
.cmp2
	movea.l	#.cmp2_msg,a0

.siz
	jsr	puts
	
	move.w	d6,d4
	andi.w	#%0000011000000000,d4
	rol.w	#7,d4
	jsr	dis_print_siz
	
	jsr	putsp
	
	movea.l	a6,a4
	
	jsr	dis_ea_std
	
	move.b	#',',d0
	jsr	putc
	
	move.w	(a5),d1
	btst	#15,d1
	beq	.Dn
	
	move.b	#'A',d0
	jmp	.num
	
.Dn
	move.b	#'D',d0

.num
	jsr	putc
	
	move.w	d1,d0
	andi.w	#%0111000000000000,d0
	rol	#4,d0
	jsr	put_nibble
	
	jsr	putnl
	
	movea.l	#dis_align_msg,a0
	jsr	puts
	
	jsr	put_word	; print extension word contents (still in d1)
	
	jsr	putnl
	
	movea.l	a6,a5
	jsr	dis_print_op
	
	rts
	
.chk2_msg
	dc.b	"CHK2.",0

.cmp2_msg
	dc.b	"CMP2.",0
	
	align 1

dis_cmpi:
	movea.l	#.cmpi_msg,a0
	jsr	puts
	jmp	dis_bit_logic_imm	; works for cmpi too, since immediate mode isn't used for EA

.cmpi_msg
	dc.b	"CMPI.",0
	
dis_btst
	movea.l	#.btst_msg,a0
	jsr	puts
	jmp	dis_bxxx
	
.btst_msg
	dc.b	"BTST ",0
	
	align 1

dis_bchg
	movea.l	#.bchg_msg,a0
	jsr	puts
	jmp	dis_bxxx
	
.bchg_msg
	dc.b	"BCHG ",0
	
	align 1
	
dis_bclr
	movea.l	#.bclr_msg,a0
	jsr	puts
	jmp	dis_bxxx
	
.bclr_msg
	dc.b	"BCLR ",0
	
	align 1
	
dis_bset
	movea.l	#.bset_msg,a0
	jsr	puts
	jmp	dis_bxxx
	
.bset_msg
	dc.b	"BSET ",0
	
	align 1	
dis_bxxx:	; BTST/BCHG/BCLR/BSET
	btst	#8,d6
	beq	.static
	
	move.b	#'D',d0
	jsr	putc
	
	move.w	d6,d0
	andi.w	#%0000111000000000,d0
	rol.w	#7,d0	; same as shift right 9 times
	jsr	put_nibble
	
	movea.l	a6,a4
	jmp	.ea
	
.static
	move.b	#'#',d0
	jsr	putc
	
	movea.l	a6,a4
	clr.w	d5
	
	move.w	(a6)+,d1
	
	move.w	d6,d0
	andi.w	#%0000000000111000,d0
	cmpi.w	#%0000000000000000,d0
	beq	.dn_ea
	
	andi.w	#%111,d1
	jmp	.bit_num
	
.dn_ea
	andi.w	#%11111,d1
	
.bit_num
	jsr	put_byte

.ea
	move.b	#',',d0
	jsr	putc
	
	swap	d5	; save these things
	movea.l	a6,a5
	
	clr.w	d4	; if it is immediate (only time size matters) it is a byte
	
	jsr	dis_ea_std
	
	jsr	putnl
	
	swap	d5
	jsr	dis_print_op
	
	swap	d5
	movea.l	a5,a4
	movea.l	a6,a5
	
	jmp	dis_print_op
	
dis_movep:
	movea.l	#.movep_msg,a0
	jsr	puts
	
	btst	#6,d6
	beq	.word
	
	move.b	#'L',d0
	jmp	.size
	
.word
	move.b	#'W',d0
	
.size
	jsr	putc
	
	jsr	putsp
	
	btst	#7,d6
	beq	.mtor
	
	; register to memory
	move.b	#'D',d0
	jsr	putc
	
	move.w	d6,d0
	andi.w	#%0000111000000000,d0
	rol.w	#7,d0	; same as shift right 9 (in this case)
	jsr	put_nibble
	
	movea.l	#.rtom_text_0,a0
	jsr	puts
	
	move.w	(a6),d1
	jsr	put_d16
	
	movea.l	#.rtom_text_1,a0
	jsr	puts
	
	move.w	d6,d0
	andi.w	#%111,d0
	jsr	put_nibble
	
	move.b	#')',d0
	jsr	putc
	
	jmp	.put_operands

.mtor
	move.b	#'(',d0
	jsr	putc
	
	move.w	(a6),d1
	jsr	put_d16
	
	movea.l	#.rtom_text_1,a0
	jsr	puts
	
	move.w	d6,d0
	andi.w	#%111,d0
	jsr	put_nibble
	
	movea.l	#.mtor_text,a0
	jsr	puts
	
	move.w	d6,d0
	andi.w	#%0000111000000000,d0
	rol.w	#7,d0	; same as shift right 9 (in this case)
	jsr	put_nibble
	
.put_operands
	jsr	putnl
	
	movea.l	#dis_align_msg,a0	; align the numbers
	jsr	puts
	
	move.w	(a6)+,d1
	jsr	put_word
	
	jmp	putnl

.movep_msg:
	dc.b	"MOVEP.",0
	
.rtom_text_0
	dc.b	",(",0
	
.rtom_text_1
	dc.b	",A",0
	
.mtor_text
	dc.b	"),D",0
	
	align 1
	
dis_ori:
	; ORI
	movea.l	#.ori_msg,a0
	jsr	puts
	jmp	dis_bit_logic_imm

.ori_msg
	dc.b	"ORI.",0
	
	align 1
	
dis_andi:
	; ANDI
	
	movea.l	#.andi_msg,a0
	jsr	puts
	jmp	dis_bit_logic_imm

.andi_msg
	dc.b	"ANDI.",0

	align 1
	
	
dis_eori:
	; EORI
	
	movea.l	#.eori_msg,a0
	jsr	puts
	jmp	dis_bit_logic_imm

.eori_msg
	dc.b	"EORI.",0

	align 1
	
dis_bit_logic_imm	; immediate bitwise logic functions
	movea.l	a6,a4	; save for later
	
	btst	#7,d6
	bne	.long
	
	clr.w	d5
	
	btst	#6,d6
	bne	.word
	
	movea.l	#.byte_msg,a0
	jsr	puts
	
	move.w	(a6)+,d1
	jsr	put_byte
	
	jmp	.ea
	
.word
	movea.l	#.word_msg,a0
	jsr	puts
	
	move.w	(a6)+,d1
	jsr	put_word
	
	jmp	.ea
	
.long
	move.w	#1,d5
	
	movea.l	#.long_msg,a0
	jsr	puts
	
	move.l	(a6)+,d1
	jsr	put_long
	
.ea
	move.b	#',',d0
	jsr	putc
	swap	d5
	
	movea.l	a6,a5	; save for later
	
	move.w	d6,d0
	andi.w	#%111111,d0
	cmpi.w	#%111100,d0
	beq	.sr
	
	jsr	dis_ea_std	; don't need size because no immediate mode
	jmp	.done
	
.sr
	btst	#6,d6	; check for CCR vs SR
	beq	.ccr
	
	movea.l	#.sr_text,a0
	jsr	puts
	jmp	.done
	
.ccr
	movea.l	#.ccr_text,a0
	jsr	puts
	
.done
	jsr	putnl
	
	swap	d5
	jsr	dis_print_op
	
	swap	d5
	movea.l	a5,a4
	movea.l	a6,a5
	jmp	dis_print_op
	
.byte_msg
	dc.b	"B #$",0
	
.word_msg
	dc.b	"W #$",0
	
.long_msg
	dc.b	"L #$",0
	
.sr_text
	dc.b	"SR",0

.ccr_text
	dc.b	"CCR",0
	
	align 1

dis_move:
	movea.l	#.move_msg,a0
	jsr	puts
	
	move.w	d6,d0	; get instruction
	andi.w	#%0000000111000000,d0	; mask to check if MOVEA or MOVE
	cmpi.w	#%0000000001000000,d0	; MOVEA
	bne	.not_a
	
	move.b	#'A',d0
	jsr	putc
	
.not_a
	move.b	#'.',d0
	jsr	putc
	
	btst	#13,d6	; check if move.b
	bne	.not_byte

	move.b	#'B',d0
	jsr	putc
	
	clr.w	d4
	jmp	.siz_done
	
.not_byte
	btst	#12,d6	; check if move.l
	bne	.not_long
	
	move.b	#'L',d0
	jsr	putc
	
	move.w	#%10,d4
	jmp	.siz_done
	
.not_long
	move.b	#'W',d0	; must be move.w
	jsr	putc
	
	move.w	#%01,d4

.siz_done

	move.b	#' ',d0
	jsr	putc
	
	movea.l	a6,a4	; a5 holds address of first set of operands
	
	jsr	dis_ea_std
	
	swap	d5	; keep old operand bits in upper word
	
	move.b	#',',d0
	jsr	putc
	
	movea.l	a6,a5	; a4 holds address of second set of operands
	
	move.w	d6,d3	; get instruction
	andi.w	#%0000000111000000,d3	; mask for dest EA mode
	lsr.w	#6,d3	; shift into lower bits
	
	move.w	d6,d2
	andi.w	#%0000111000000000,d2	; mask for dest EA reg
	rol.w	#7,d2	; same as shift right 9 times in this case
	
	jsr	dis_ea	; don't need to worry about destination size, since size only matters for immediate mode
	
	jsr	putnl
	
	swap	d5	; get first set of operand bits
	
	jsr	dis_print_op
	
	swap	d5	; get bits for second set of operands
	movea.l	a5,a4
	movea.l	a6,a5
	
	jmp	dis_print_op

.move_msg
	dc.b	"MOVE",0
	
	align 1
	
dis_ill:
	movea.l	#.msg,a0
	jmp	puts
	
.msg
	dc.b	"ILLEGAL",cr,lf,0
	
	align 1

dis_unk:
	; unknown instruction
	movea.l	#.msg,a0
	jmp	puts
	
.msg
	dc.b	"UNK",cr,lf,0
	
	align 1
	
dis_print_siz:
	; print size character based on d4.w
	; 00 - byte
	; 01 - word
	; 10 - long
	; 11 - undefined
	
	btst	#0,d4
	beq	.not_word
	
	move.b	#'W',d0
	jmp	putc
	
.not_word
	btst	#1,d4
	beq	.not_long
	
	move.b	#'L',d0
	jmp	putc
	
.not_long
	move.b	#'B',d0
	jmp	putc
	
dis_print_op:
	; print operands based on bitfield returned by dis_ea in d5.w, starting address in a4, and ending
	; address in a5
	cmpa.l	a4,a5	; if no operands
	beq	.done	; skip printing
	
	movea.l	#dis_align_msg,a0	; align the numbers
	jsr	puts
	
.next_operand
	btst	#0,d5	; check if operand is word or long
	beq	.word_operand
	
	move.l	(a4)+,d1
	jsr	put_long
	jmp	.check_last
	
.word_operand
	move.w	(a4)+,d1
	jsr	put_word

.check_last
	jsr	putsp
	lsr.w	#1,d5
	cmpa.l	a4,a5	; check if more operands
	bne	.next_operand
	
	jsr	putnl
	
.done
	rts
	
dis_ea_std:
	; assumes ea mode is in bits 3-5 of d6, and ea reg is in bits 0-2 of d6
	; size must be passed in d4
	move.w	d6,d3
	andi.w	#%111000,d3
	lsr.w	#3,d3
	
	move.w	d6,d2
	andi.w	#%111,d2
	
dis_ea:
	; disassemble effective address
	;
	; a6 still holds address to disassemble, but points to start of operands and extension words
	; d6 will explicitely not be touched (holds copy of instruction word)
	; d5.w will return bits corresponding to operand size, 1 bit means long, 0 bit means word
	; d4.w passes size
	;	encoding is:
	;	00 - byte
	;	01 - word
	;	10 - long
	; d3.w passes EA mode
	; d2.w passes EA reg
	; a1,a0,d1,d0 - scratch/parameter passing
	
	movea.l	#.addr,a0
	movea.l	(a0,d3.w*4),a0
	jmp	(a0)
	
.addr
	dc.l	dis_ea_000
	dc.l	dis_ea_001
	dc.l	dis_ea_010
	dc.l	dis_ea_011
	dc.l	dis_ea_100
	dc.l	dis_ea_101
	dc.l	dis_ea_110
	dc.l	dis_ea_111
	
dis_ea_000
	; data register direct mode
	move.b	#'D',d0
	jsr	putc
	
	move.b	d2,d0
	jmp	put_nibble
	
dis_ea_001
	; address register direct mode
	move.b	#'A',d0
	jsr	putc
	
	move.b	d2,d0
	jmp	put_nibble
	
dis_ea_010
	; address register indirect
	movea.l	#.prefix,a0
	jsr	puts
	
	move.b	d2,d0
	jsr	put_nibble
	
	move.b	#')',d0
	jmp	putc
	
.prefix
	dc.b	"(A",0
	
	align 1
	
dis_ea_011
	; address register indirect with postincrement
	movea.l	#.prefix,a0
	jsr	puts
	
	move.b	d2,d0
	jsr	put_nibble

	movea.l	#.suffix,a0
	jmp	puts
	
.prefix
	dc.b	"(A",0

.suffix
	dc.b	")+",0
	
	align 1
	
dis_ea_100
	; address register indirect with predecrement
	movea.l	#.prefix,a0
	jsr	puts
	
	move.b	d2,d0
	jsr	put_nibble
	
	move.b	#')',d0
	jmp	putc
	
.prefix
	dc.b	"-(A",0
	
	align 1
	
dis_ea_101
	; address register indirect with displacement (d16)
	clr.w	d5	; single word operand
	move.b	#'(',d0
	jsr	putc
	
	move.w	(a6)+,d1
	jsr	put_d16
	
	cmpi.w	#%111,d3
	beq	.pc_base
	
	movea.l	#.addr_msg,a0
	jsr	puts
	
	move.b	d2,d0
	jsr	put_nibble
	jmp	.suffix
	
.pc_base
	movea.l	#.pc_msg,a0
	jsr	puts
	
.suffix
	
	move.b	#')',d0
	jmp	putc
	
.addr_msg
	dc.b	".W,A",0
	
.pc_msg
	dc.b	".W,PC",0
	
	align 1
	
dis_ea_110
	; (bd,An,Xn.SIZ*SCALE) modes (and similar variants)
	; also handles PC variants (mode 111)
	move.b	#'(',d0
	jsr	putc
	
	btst.b	#0,(a6)	; check full or brief extension word
	bne	dis_ea_110f	; call for full format extension word
	
	move.w	(a6)+,d4	; get brief extension word
	
	move.b	d4,d1	; get displacement byte
	bpl	.pos_disp	; jump if positive
	
	move.b	#'-',d0	; displacement is negative
	jsr	putc
	
	neg.b	d1	; negate to get magnitude
	
.pos_disp
	move.b	#'$',d0	; hex value, so print $
	jsr	putc
	
	jsr	put_word	; print displacement
	
	move.b	#',',d0
	jsr	putc
	
	cmpi.w	#%111,d3
	bne	.addr_base
	
	movea.l	#.pc_string,a0
	jsr	puts
	
	jmp	.index
	
.addr_base
	move.b	#'A',d0	; next is base register (An)
	jsr	putc
	
	move.b	d2,d0
	jsr	put_nibble	; An
	
	move.b	#',',d0
	jsr	putc
	
.index
	btst	#15,d4	; check if index in Dn or An
	beq	.data_index
	
	move.b	#'A',d0
	jsr	putc
	jmp	.index_num
	
.data_index
	move.b	#'D',d0
	jsr	putc
	
.index_num
	move.w	d4,d0
	andi.w	#%0111000000000000,d0	; get index register number
	lsr.w	#8,d0
	lsr.w	#4,d0
	jsr	put_nibble
	
	move.b	#'.',d0
	jsr	putc
	
	btst	#11,d4	; check index size (l or w)
	bne	.long_index
	
	move.b	#'W',d0
	jsr	putc
	jmp	.index_scale
	
.long_index
	move.b	#'L',d0
	jsr	putc
	
.index_scale
	move.b	#'*',d0
	jsr	putc
	
	move.w	d4,d1	; convert scale to digit
	andi.w	#%0000011000000000,d1
	lsr.w	#8,d1
	lsr.w	#1,d1
	
	move.b	#1,d0
	lsl	d1,d0
	jsr	put_nibble
	
	move.b	#')',d0
	jsr	putc
	
	clr.w	d5
	rts
	
.pc_string
	dc.b	"PC,",0
	
	align 1
	
dis_ea_110f
	; this one gets a bit complicated, so to refresh and add some:
	;
	; a6 still holds address to disassemble, but points to start of operands and extension words
	; d6 will explicitely not be touched (holds copy of instruction word)
	; d5.w will return bits corresponding to operand size, 1 bit means long, 0 bit means word
	; d4.w will hold the extension word.
	; d3.w passes EA mode. We're going to reuse this as a bit index into d5.w.
	;	to retain the EA mode for later use, we'll swap it into the upper word.
	; d2.w passes EA reg. Once we're done with the register, it will act as a flag. 0 means previous
	; fields have been empty, non-zero means at least one previous field was used (not suppressed). This
	; will be set to 1 when a trailing ']' is printed as well. This used used to determine whether
	; preceding commas should be printed.
	; a1,a0,d1,d0 - scratch/parameter passing
	move.w	(a6)+,d4	; get extension word
	clr.w	d5	; operand data was a word
	swap	d3	; lets us save the EA mode
	move.b	#1,d3	; next bit to modify in d5 will be bit 1

	; begin checking extension format validity. This is all explained one way or another in
	; the 68000PRM, chapter 2.
	move.w	d4,d1
	andi.w	#%0000000000110000,d1	; check some reserved values
	beq	dis_ea_inv	; if BD SIZE field is 00, that's reserved
	
	btst	#3,d4	; this should be 0 according to 68K PRM
	bne	dis_ea_inv
	
	move.w	d4,d1
	andi.w	#%0000000001000111,d1
	cmpi.w	#%0000000001000011,d1	; certain indirection modes not allowed with index reg suppressed
	bhi	dis_ea_inv
	
	cmpi.w	#%0000000000000100,d1	; explicitely reserved mode
	beq	dis_ea_inv
	
	; So at this point, we've determined that the extension word seems to be valid
	
	andi.w	#%0000000000000111,d1
	beq	.no_indirect0	; if IS is 000, no extra indirection

	move.b	#'[',d0
	jsr	putc
	
.no_indirect0
	; checking bd size
	; we know BD SIZE field isn't 00, since it's reserved
	; 01 means null displacement
	btst	#5,d4
	beq	.null_bd
	
	btst	#4,d4
	beq	.word_bd
	
	; long bd
	move.l	(a6)+,d1
	jsr	put_d32
	bset	d3,d5
	addq	#1,d3
	
	move.b	#'.',d0
	jsr	putc
	
	move.b	#'L',d0
	jsr	putc
	
	jmp	.null_bd
	
.word_bd
	move.w	(a6)+,d1
	jsr	put_d16
	addq	#1,d3
	
	move.b	#'.',d0
	jsr	putc
	
	move.b	#'W',d0
	jsr	putc

.null_bd
	btst	#7,d4
	bne	.base_suppressed
	
	btst	#5,d4
	beq	.no_base_comma	; skip preceding comma
	
	move.b	#',',d0
	jsr	putc
	
.no_base_comma
	swap	d3
	cmpi.w	#%111,d3	; check PC base mode
	bne	.addr_base
	
	; PC base reg
	swap	d3
	movea.l	#.PC_text,a0
	jsr	puts
	
	jmp	.base_not_suppressed
	
.addr_base
	swap	d3
	move.b	#'A',d0
	jsr	putc
	
	move.w	d2,d0
	jsr	put_nibble
	
	jmp	.base_not_suppressed
	
.base_suppressed
	;check if bd suppressed too, then d2 is 0, otherwise d2 is nonzero (1)
	btst	#5,d4
	beq	.empty	; if previous portion of EA is empty (bd and base reg suppressed)
	
.base_not_suppressed	; used for when the base isn't suppressed to set d2
	move.w	#1,d2
	jmp	.postindex_check
	
.empty
	clr.w	d2
	
.postindex_check
	btst	#2,d4
	beq	.index	; not postindex mode
	
	tst.w	d2
	bne	.nonzero_postindex
	
	move.b	#'0',d0
	jsr	putc
	
.nonzero_postindex
	move.b	#']',d0
	jsr	putc
	
	move.w	#1,d2
	
.index
	btst	#6,d4
	bne	.index_suppressed
	
	tst.w	d2
	beq	.no_index_comma	; no previous stuff, so no comma
	
	move.b	#',',d0
	jsr	putc
	
.no_index_comma
	move.w	#1,d2
	
	btst	#15,d4
	bne	.addr_index
	
	; data index
	move.b	#'D',d0
	jmp	.index_num
	
.addr_index
	move.b	#'A',d0
	
.index_num
	jsr	putc
	
	move.w	d4,d0
	andi.w	#%0111000000000000,d0
	lsr.w	#8,d0
	lsr.w	#4,d0
	jsr	put_nibble
	
	move.b	#'.',d0
	jsr	putc
	
	btst	#11,d4
	bne	.long_index
	
	;word index
	move.b	#'W',d0
	jmp	.index_scale
	
.long_index
	move.b	#'L',d0
	
.index_scale
	jsr	putc	; finish up size
	
	move.b	#'*',d0
	jsr	putc
	
	move.w	d4,d1
	andi.w	#%0000011000000000,d1
	move.b	#1,d0
	lsr	#8,d1
	lsr	#1,d1
	lsl	d1,d0
	jsr	put_nibble
	
.index_suppressed
	move.w	d4,d0
	andi.w	#%0000000000000111,d0
	beq	.no_preindex	; 0 is no indirection
	cmpi.w	#%011,d0
	bhi	.no_preindex
	
	tst.w	d2
	bne	.not_empty	; check if we need to put an implied 0
	
	move.b	#'0',d0	; implied 0
	jsr	putc
	move.w	#1,d2
	
.not_empty
	move.b	#']',d0
	jsr	putc
	
.no_preindex

	btst	#1,d4	; check for od
	beq	.no_od
	
	move.b	#',',d0	; *always* a comma before od, due to implied [0]
	jsr	putc
	
	btst	#0,d4	; check od size
	beq	.word_od
	
	; long od
	move.l	(a6)+,d1
	jsr	put_d32
	bset	d3,d5
	
	move.b	#'.',d0
	jsr	putc
	move.b	#'L',d0
	jsr	putc
	
	jmp	.no_od
	
.word_od
	move.w	(a6)+,d1
	jsr	put_d16
	
	move.b	#'.',d0
	jsr	putc
	move.b	#'W',d0
	jsr	putc
	
.no_od
	move.b	#')',d0
	jsr	putc

	rts
	
.PC_text
	dc.b	"PC",0

	align 1
	
dis_ea_111
	tst.w	d2
	beq	dis_ea_abs_short
	
	cmpi.w	#%001,d2
	beq	dis_ea_abs_long
	
	cmpi.w	#%010,d2
	beq	dis_ea_101
	
	cmpi.w	#%011,d2
	beq	dis_ea_110
	
	jmp	dis_ea_imm
	
dis_ea_abs_short
	clr.w	d5
	
	movea.l	#.prefix,a0
	jsr	puts
	
	move.w	(a6)+,d1
	jsr	put_word
	
	movea.l	#.suffix,a0
	jmp	puts
	
.prefix
	dc.b	"($",0
	
.suffix
	dc.b	").W",0
	
	align 1
	
dis_ea_abs_long
	move.w	#1,d5
	
	movea.l	#.prefix,a0
	jsr	puts
	
	move.l	(a6)+,d1
	jsr	put_long
	
	movea.l	#.suffix,a0
	jmp	puts
	
.prefix
	dc.b	"($",0
	
.suffix
	dc.b	").L",0
	
	align 1
	
dis_ea_imm
	movea.l	#.prefix,a0
	jsr	puts
	
	tst.w	d4
	bne	.not_byte
	
	move.w	(a6)+,d1
	clr.w	d5
	
	jmp	put_byte

.not_byte
	cmpi.w	#1,d4
	bne	.not_word
	
	move.w	(a6)+,d1
	clr.w	d5
	
	jmp	put_word

.not_word
	move.l	(a6)+,d1
	move.w	#1,d5
	
	jmp	put_long
	
.not
	
.prefix
	dc.b	"#$",0
	
	align 1
	
dis_ea_inv
	move.l	#.msg,a0
	jmp	puts
	
.msg
	dc.b	"<inv>",0
	
	align 1

	
dis_align_msg
	dc.b	"          ",0	; correct number of spaces to align operand printing
	
	align 1
	
put_d32
	; displacement in d1
	tst.l	d1
	bpl	.pos_disp
	
	move.b	#'-',d0
	jsr	putc
	neg.l	d1
	
.pos_disp
	move.b	#'$',d0
	jsr	putc
	
	jmp	put_long
	
put_d16
	; displacement in d1
	tst.w	d1
	bpl	.pos_disp
	
	move.b	#'-',d0
	jsr	putc
	neg.w	d1
	
.pos_disp
	move.b	#'$',d0
	jsr	putc
	
	jmp	put_word

	
hang:
	; endless loop in event of fatal error
	jmp	hang
	
; at least report a few 'fatal' exceptions
berr:
aerr:
illinst:
divzero:
chk:
trap:
priv:
aline:
fline:
hdwbkpt:
;coprocv:
formerr:
res:
;coproc0:
;coproc1:
;coproc2:
;coproc3:
;coproc4:
;coproc5:
;coproc6:
;coproc7:
;coproc8:
;coproc9:
;coproca:

exception_parser:
	movem.l	d0-d7/a0-a6,user_data	; store registers before we destroy them
	move	usp,a0	; we make an assumption that the faulted software was running in user mode
	move.l	a0,user_sp
	
	move.w	(a7),user_sr	; store status register
	move.l	(2,a7),user_pc	; and store PC
	
	movea.l	#exception_msg,a0	; get start of exception message pointer table
	
	move.w	(6,a7),d0	; get vector offset from stack frame
	andi.w	#$0FFF,d0	; clear format code, we just want the offset
	
	cmpi.w	#($F*4),d0	; check if vector is 15 or less
	bhi	.unk	; if it's more than vector 15, then it's not something we know
	
	movea.l (0,a0,d0.w),a0	; otherwise, get address of message for this vector
	jmp	.known
	
.unk
	movea.l #res_msg,a0	; default message for unknown vector

.known	
	jsr	puts	; print description of exception

	movea.l	#.vec_num,a0
	jsr	puts
	move.w	(6,a7),d1
	andi.w	#$0FFF,d1
	lsr.w	#2,d1	; divide by four to get vector number
	jsr	put_byte
	
	movea.l	#.stack_format,a0
	jsr	puts
	
	move.b	(6,a7),d0	; get byte with format code
	rol.b	#4,d0	; swap nibbles
	jsr	put_nibble	; and write format code to screen
	jsr	putnl	; and put a newline
	
	move.b	(6,a7),d0	; get byte with format code again
	andi.b	#$F0,d0	; get just format code in upper nibble
	
	;cmpi.b	#0,d0	; check if it's format 0
	beq	exception_format_0	; if it is, handle it here
	
	cmpi.b	#$20,d0	; check format 2
	beq	exception_format_2
	
	cmpi.b	#$C0,d0	; check format $C
	beq	exception_format_c
	
	movea.l	#.unk_format,a0
	jsr	puts
	
	movea.l	#super_stack,a7	; reset supervisor stack to be at the top

	jmp	prompt_loop	; return back to a prompt
	
.vec_num
	dc.b	cr,lf,"Vector number: ",0
	
.stack_format
	dc.b	cr,lf,"Exception stack frame format: ",0
	
.unk_format
	dc.b	"Unknown stack frame format.",0
	
	align 1
	
exception_format_0
	movea.l	#.pc_message,a0
	jsr	puts
	
	move.l	(2,a7),d1
	jsr	put_long
	
	movea.l	#.sr_message,a0
	jsr	puts
	
	move.w	(a7),d1
	jsr	put_word
	jsr	putnl
	
	movea.l	#super_stack,a7	; reset supervisor stack to be at the top
	
	jsr	regs

	jmp	prompt_loop	; return back to a prompt
	
.pc_message
	dc.b	"Address of faulted instruction: ",0
	
.sr_message
	dc.b	cr,lf,"Status register: ",0
	
	align 1

exception_format_2
	movea.l	#.retpc_message,a0
	jsr	puts
	
	move.l	(2,a7),d1
	jsr	put_long
	
	movea.l	#.faultpc_message,a0
	jsr	puts
	
	move.l	(8,a7),d1
	jsr	put_long
	
	movea.l	#.sr_message,a0
	jsr	puts
	
	move.w	(a7),d1
	jsr	put_word
	jsr	putnl
	
	movea.l	#super_stack,a7	; reset supervisor stack to be at the top
	
	jsr	regs

	jmp	prompt_loop	; return back to a prompt
	
.retpc_message
	dc.b	"Address of next instruction: ",0
	
.faultpc_message
	dc.b	cr,lf,"Address of faulted instruction: ",0
	
.sr_message
	dc.b	cr,lf,"Status register: ",0
	
	align 1

exception_format_c
	move.w	($16,a7),d7	; get the SSW word
	
	movea.l	#.ssw_head,a0
	jsr	puts
	
	moveq	#15,d2
	
.ssw_loop
	btst	d2,d7	; check the bit
	beq	.clear
	
	move.b	#'+',d0
	jmp	.put
	
.clear
	move.b	#'-',d0
	
.put
	jsr	putc
	
	dbra	d2,.ssw_loop
	
	jsr	putnl
	
	btst	#15,d7	; check TP bit
	beq	.not_tp
	
	movea.l	#.tp_msg,a0
	jmp	.type_put
	
.not_tp
	btst	#14,d7	; check MV bit
	beq	.not_mv
	
	movea.l	#.mv_msg,a0
	jmp	.type_put
	
.not_mv
	btst	#7,d7	; check IN bit
	beq	.not_pf
	
	movea.l	#.pf_msg,a0
	jmp	.type_put
	
.not_pf
	movea.l	#.of_msg,a0
	
.type_put
	jsr	puts
	
	movea.l	#.fault_addr_msg,a0
	jsr	puts
	
	move.l	($8,a7),d1
	jsr	put_long
	jsr	putnl
	
	btst	#12,d7	; check TR bit
	beq	.not_tr
	
	movea.l	#.tr_msg,a0
	jsr	puts
	
.not_tr
	btst	#11,d7	; B1 bit
	bne	.with_br
	
	btst	#10,d7	; B0 bit
	bne	.with_br
	jmp	.not_br
	
.with_br
	movea.l	#.br_msg,a0
	jmp	puts
	
.not_br
	btst	#9,d7	; RR bit
	beq	.not_rr
	
	movea.l	#.rr_msg,a0
	jsr	puts
	
.not_rr
	jsr	putnl

	btst	#8,d7	; RM bit
	beq	.not_rm
	
	movea.l	#.rm_msg,a0
	jsr	puts
	
.not_rm	
	movea.l	#.opr_msg,a0
	jsr	puts
	
	btst	#5,d7	; LG bit
	beq	.not_long
	
	movea.l	#.long_msg,a0
	jmp	.opr_put
	
.not_long
	btst	#4,d7	; this bit is part of SIZ, indicates word
	beq	.not_word
	
	movea.l	#.word_msg,a0
	jmp	.opr_put
	
.not_word
	movea.l	#.byte_msg,a0
	
.opr_put
	jsr	puts

	
	btst	#6,d7	; RW bit
	beq	.not_r
	
	movea.l	#.r_msg,a0
	jmp	.rw_put
	
.not_r
	movea.l	#.w_msg,a0
	
.rw_put
	jsr	puts
		
	movea.l	#.siz_msg,a0
	jsr	puts
	
	move.b	d7,d0
	andi.b	#%11000,d0	; get size
	bne	.not_4
	
	move.b	#4,d0
	jmp	.siz_put
	
.not_4
	lsr.b	#3,d0
	
.siz_put
	jsr	put_nibble
	jsr	putnl
	
	btst	#15,d7	; check TP bit
	beq	.not_tp_2
	
	movea.l	#.next_pc_msg,a0
	jsr	puts
	
	move.l	($2,a7),d1
	jsr	put_long
	jsr	putnl
	
	movea.l	#.format_msg,a0
	jsr	puts
	
	move.b	($E,a7),d0
	asr.b	#4,d0
	jsr	put_nibble
	jsr	putnl
	
	movea.l	#.pre_sr_msg,a0
	jsr	puts
	
	move.w	($C,a7),d1
	jsr	put_word
	jsr	putnl
	
.not_tp_2	
	movea.l	#super_stack,a7	; reset supervisor stack to be at the top
	
	jsr	regs

	jmp	prompt_loop	; return back to a prompt
	
.ssw_head
	dc.b	"SSW:         F",cr,lf
	dc.b	"           S U",cr,lf
	dc.b	"TM TBBRRIRLI N",cr,lf
	dc.b	"PV0R10RMNWGZ C",cr,lf,0

.tp_msg
	dc.b	"Exception occurred during exception processing.",cr,lf,0
.mv_msg
	dc.b	"Exception occurred during MOVEM operand transfer.",cr,lf,0
.pf_msg
	dc.b	"Exception occurred during instruction prefetch.",cr,lf,0
.of_msg
	dc.b	"Exception occurred during normal (non-MOVEM) operand transfer.",cr,lf,0
.tr_msg
	dc.b	"Trace pending. ",0
.br_msg
	dc.b	"Breakpoint pending. ",0
.rr_msg
	dc.b	"Faulted bus cycle was a released write.",0
.rm_msg
	dc.b	"Faulted bus cycle was a RMW cycle.",0
.opr_msg
	dc.b	"Faulted bus cycle size was a ",0
.long_msg
	dc.b	"long",0
.word_msg
	dc.b	"word",0
.byte_msg
	dc.b	"byte",0
.r_msg
	dc.b	" read. ",0
.w_msg
	dc.b	" write. ",0
.siz_msg
	dc.b	"Number of bytes left in cycle: ",0
.fault_addr_msg
	dc.b	"Faulted address: ",0
.next_pc_msg
	dc.b	"Next instruction PC: ",0
.pre_sr_msg
	dc.b	"Pre-exception status register: ",0
.format_msg
	dc.b	"Faulted exception frame format: ",0
	
	align 1
	
; table of pointers to exception notification messages
; used only for exceptions which break into the debugger
exception_msg

	dc.l	res_msg		; SSP initial address: shouldn't get an exception at this vector
	dc.l	res_msg		; reset: shouldn't be possible to get a stack frame for reset
	dc.l	berr_msg	; bus error
	dc.l	aerr_msg	; address error
	
	dc.l	illinst_msg	; illegal instruction
	dc.l	divzero_msg	; division by zero
	dc.l	chk_msg		; CHK, CHK2
	dc.l	trap_msg	; TRAPV, TRAPcc
	
	dc.l	priv_msg	; priviledge violation
	dc.l	res_msg		; trace is not handled here (yet, maybe it will be)
	dc.l	aline_msg	; A-line instruction
	dc.l	fline_msg	; F-line instruction

	dc.l	hdwbkpt_msg	; hardware breakpoint
	dc.l	res_msg		; coproc protocol violation (not used on CPU32)
	dc.l	formerr_msg	; format error
	dc.l	res_msg		; uninitialized interrupt, not handled here

; everything beyond here (last vector is 15) is not handled here

	
res_msg		dc.b	"An unknown exception has occurred.",0
	
berr_msg	dc.b	"A bus error exception has occurred.",0

aerr_msg	dc.b	"An address error exception has occurred.",0

illinst_msg	dc.b	"An illegal instruction exception has occurred.",0

divzero_msg	dc.b	"A division by zero exception has occurred.",0

chk_msg		dc.b	"A CHK or CHK2 exception has occurred.",0

trap_msg	dc.b	"A TRAPcc or TRAPV exception has occurred.",0

priv_msg	dc.b	"A priviledge violation exception has occurred.",0

aline_msg	dc.b	"An unimplemented A-line instruction exception has occurred.",0

fline_msg	dc.b	"An unimplemented F-line instruction exception has occurred.",0

hdwbkpt_msg	dc.b	"A hardware breakpoint exception has occurred.",0

formerr_msg	dc.b	"A format error exception has occurred.",0
	align	1

kbd_irq:
	; only possible KB controller interrupt is output buffer full:
	tst.b	(KB+KB_DATA)	; clear output buffer
	rte
	
pp_irq
	tst.w	PPORT	; clears printer port IRQ
	rte
	
duart0_irq
	movem.l	d0/a0,-(a7)
	
	btst	#1,(DUART0+DUART_MISR)	; check if RX interrupt
	beq	.no_rx
	
	move.b	(DUART0+DUART_RHRA),d0
	
	cmp.b	#xon,d0
	beq	.rx_xon
	
	cmp.b	#xoff,d0
	beq	.rx_xoff
	
	movea.l	ser_rx_buff_tail,a0
	
	move.b	d0,(a0)+
	
	cmpa.l	#ser_rx_buff_end,a0
	bne	.no_rx_wrap
	
	movea.l	#ser_rx_buff,a0
	
.no_rx_wrap
	cmpa.l	ser_rx_buff_head,a0
	beq	.full_rx_buff
	
	move.l	a0,ser_rx_buff_tail	; save new tail
	
.full_rx_buff
	move.b	ser_rx_buff_tail+3,d0
	sub.b	ser_rx_buff_head+3,d0
	cmpi.b	#224,d0
	bls	.no_rx
	
	move.b	#xoff,ser_rx_flow
	move.b	#%00000111,(DUART0+DUART_IMR)
	jmp	.no_rx
	
.rx_xon
	clr.b	ser_tx_flow
	move.b	#%00000111,(DUART0+DUART_IMR)
	jmp	.no_rx
	
.rx_xoff
	move.b	#-1,ser_tx_flow
	
.no_rx
	btst	#0,(DUART0+DUART_MISR)	; check if TX interrupt
	beq	.no_tx
	
	move.b	ser_rx_flow,d0
	beq	.no_rx_flow
	
	move.b	d0,(DUART0+DUART_THRA)
	clr.b	ser_rx_flow
	jmp	.no_tx
	
.no_rx_flow	
	tst.b	ser_tx_flow
	bne	.empty_tx_buff
	
	movea.l	ser_tx_buff_head,a0
	cmpa.l	ser_tx_buff_tail,a0
	beq	.empty_tx_buff
	
	move.b	(a0)+,(DUART0+DUART_THRA)
	
	cmpa.l	#ser_tx_buff_end,a0
	bne	.no_wrap
	
	movea.l	#ser_tx_buff,a0
	
.no_wrap
	move.l	a0,ser_tx_buff_head
	jmp	.no_tx
	
.empty_tx_buff
	move.b	#%00000110,(DUART0+DUART_IMR)	; turn off TX IRQ if buffer is empty
	
.no_tx
	btst	#2,(DUART0+DUART_MISR)	; check for break condition
	beq	.no_break
	
	move.b	#%01010000,(DUART0+DUART_CRA)	; clear the break interrupt
	
	andi	#%0011111111111111,sr	; clear trace bits
	ori	#%0000011100000000,sr	; disable interrupts
	movec	vbr,a0	; set vector base register
	movea.l	(a0),a7	; set supervisor stack register
	movea.l	(4,a0),a0	; jmp to reset vector
	
	; this might cause issues, so maybe don't. not a big deal.
	;jsr	flush_serial	; flush DUART before we jump
	
	jmp	(a0)
	
.no_break
	movem.l	(a7)+,d0/a0
	
	rte

; dummy exception routine to at least make everything compile
spurint:
unint:
trace:
psu_irq:
;pp_irq:
rtc_irq:
avec4:
avec5:
;kbd_irq:
nmi:
trap0:
trap1:
trap2:
trap3:
trap4:
trap5:
trap6:
trap7:
trap8:
trap9:
trapa:
trapb:
trapc:
trapd:
trape:
trapf:
pitirq:
qspi:
tpu0:
tpu1:
tpu2:
tpu3:
tpu4:
tpu5:
tpu6:
tpu7:
tpu8:
tpu9:
tpua:
tpub:
tpuc:
tpud:
tpue:
tpuf:
	rte
	
newline_str
	dc.b	cr,lf,0
	
digitbound
	dc.b	'0','9'	; lower and upper bound for digit

hexletterbound
	dc.b	'A','F'	; lower and upper bound for hex letters

lowercasebound
	dc.b	'a','z' ; lower and upper bound for lowercase ascii characters
	
	align 1

;test:


cmd_table:
; this table holds one entry per command
; each entry consists first of a pointer to a string of the command
; next there is a pointer to the subroutine for that command
; the table ends with an entry that has a null string pointer


	dc.l	load_srec_cmd_string_abbr
	dc.l	load_srec

	dc.l	ver_cmd_string_abbr
	dc.l	ver

	dc.l	jump_cmd_string_abbr
	dc.l	jump
	
	dc.l	boot_cmd_string_abbr
	dc.l	boot
	
	dc.l	regs_cmd_string_abbr
	dc.l	regs
	
	dc.l	disassemble_cmd_string_abbr
	dc.l	disassemble


	dc.l	load_srec_cmd_string
	dc.l	load_srec

	dc.l	ver_cmd_string
	dc.l	ver

	dc.l	jump_cmd_string
	dc.l	jump
	
	dc.l	boot_cmd_string
	dc.l	boot
	
	dc.l	regs_cmd_string
	dc.l	regs
	
	dc.l	disassemble_cmd_string
	dc.l	disassemble
	
;	dc.l	test_cmd_string
;	dc.l	test

	dc.l	0	; null entry ends table

; here's the list of all the strings for command lookup

load_srec_cmd_string:
	dc.b	"LOAD",0

load_srec_cmd_string_abbr:
	dc.b	"L",0

ver_cmd_string_abbr
	dc.b	"V",0
	
ver_cmd_string
	dc.b	"VER",0

jump_cmd_string_abbr
	dc.b	"J",0
	
jump_cmd_string
	dc.b	"JUMP",0

boot_cmd_string_abbr
	dc.b	"B",0
		
boot_cmd_string
	dc.b	"BOOT",0
	
regs_cmd_string_abbr
	dc.b	"R",0

regs_cmd_string
	dc.b	"REGS",0
	
disassemble_cmd_string_abbr
	dc.b	"D",0
disassemble_cmd_string
	dc.b	"DISASSEMBLE",0
	
;test_cmd_string
;	dc.b	"TEST",0
	
	align	1
monitor_end:	; this points just past the end of the monitor's code	

input_buf_len	equ	32
	
	offset mon_ram
	; all of the monitor variables go here
mon_pointer	ds.l	1	; holds address where monitor is currently looking
user_data	ds.l	8	; d0-d7
user_addr	ds.l	7	; a0-a6
user_sp		ds.l	1	; a7
user_sr		ds.w	1	; user status register
user_pc		ds.l	1	; user PC
input_buff	ds.b	input_buf_len	; used as an input buffer

ser_rx_buff	ds.b	256	; serial input FIFO (ring buffer)
ser_rx_buff_end
ser_rx_buff_head	ds.l	1	; characters taken from here (pointer)
ser_rx_buff_tail	ds.l	1	; characters added here (pointer)

ser_tx_buff	ds.b	256	; serial output FIFO
ser_tx_buff_end
ser_tx_buff_head	ds.l	1	; characters taken from here (pointer)
ser_tx_buff_tail	ds.l	1	; characters added here (pointer)

ser_tx_flow	ds.b	1	; non-zero to disable tx
ser_rx_flow	ds.b	1	; used to pass a single character for high-priority tx (bypasses TX fifo)
				; this character will be transmitted regardless of tx flow control
				; tx interrupt must be enabled. This is used for sending xon/xoff
				; will be sent if non-zero, cleared to 0 after tx
				
ram_test_err	ds.l	1	; holds address of failed RAM, NULL (0) means RAM passed

mon_ram_end:

; here we've got some statistics to be reported during assembly:

	printt	"monitor variables start at:"
	printv	mon_ram
	printt	"monitor variables end at:"
	printv	mon_ram_end
	printt	"size remaining for supervisor stack is:"
	printv	super_stack-mon_ram_end

