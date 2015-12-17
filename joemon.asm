; Joe Zatarski's 68332 monitor
; Written for use with Beckman DU600 photospectrometer

; This is written for use on a 68332 processor, but should be portable to
; other 68K archs without too much trouble.

; Copyright 2014,2015 Joe Zatarski
; email joe.zatarski@gmail.com

; TODO: put GPL notice here

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

	include "CPU.EQU"
	include "MISC.EQU"
	include "PARPORT.EQU"
	include "SERIAL.EQU"

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
	dc.l	coprocv	; coprocessor protocol violation
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
	
	dc.l	coproc0	; 48
	dc.l	coproc1
	dc.l	coproc2
	dc.l	coproc3
	
	dc.l	coproc4	; 52
	dc.l	coproc5
	dc.l	coproc6
	dc.l	coproc7
	
	dc.l	coproc8	; 56
	dc.l	coproc9
	dc.l	coproca
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
	move.b	#0,(SYPCR)	; set SYPCR such that:
				; software watchdog disabled
				; halt monitor disabled
				; internal bus monitor disabled
	move.w	#$FFE0,(TRAMBAR) ; set TRAMBAR such that:
				; base address of TPU RAM is $FF E000
	move.w	#$140,(PITR) 	; set PITR such that:
				; time period is approximately 1 second
	move.w	#$540,(PICR)	; set PICR such that:
				; use IRQ5
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
	move.w	#$3050,(QILR)	; set QILR/QIVR for:
				; QSPI IRQ 6
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
	jmp	hang		; TODO: add some better handling of bad RAM

.ramgood1:
	not.l	(a0)		; sets RAM back	to what	it was
	not.l	d1		; sets d1 back to what ram was to begin	with
	cmp.l	(a0)+,d1
	beq.l	.ramgood2	; if the two match, I guess the	ram passes
	move.w	#$A000,(LEDS)	; if the RAM fails, set some lights to show it
	jmp	hang

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
	jmp	hang

.ramgood1:				; CODE XREF: sub_C58C-2400j
	not.l	(a0)		; NOT ram location again
	not.l	d1		; and the copy
	cmp.l	(a0)+,d1
	beq.l	.ramgood2	; if they match, the RAM passes
	move.w	#$4000,(LEDS)	; otherwise, show failure
	jmp	hang
; ---------------------------------------------------------------------------
.ramgood2:				; CODE XREF: sub_C58C-23E8j
	subi.l	#1,d0		; decrement counter
	bne.s	.testloop	; if it's not 0, keep testing more RAM

	endif

	move.w	#$F000,(PPORT)	; show we got past the first part of boot
				; ALSO CLEARS PARALLEL PORT IRQ


coldstart:
	; monitor actually starts here
	; interupts are assumed to be off at this point
	; there will be a series of JSRs here to various hardware init
	; not much yet, just the console for now

	jsr	init_console
	
warmstart:
	; this is after one-time hardware init

	jsr	clr_screen

	jsr	init_mon_ram

prompt_loop:	; loop which accepts and interprets monitor commands
	movea.l	#.prompt,a0	; print the prompt
	jsr	puts

	movea.l	#input_buff,a0	; get a command string
	move.l	#input_buf_len,d1
	jsr	gets
	
	movea.l	#cmd_table,a0	; point to start of command table

.cmd_table_loop
	movea.l	(a0),a1	; point to command string from the table entry

	tst.l	a1	; check the address
	beq	.no_match	; null pointer means end of the list, we don't have a match

	movea.l	#input_buff,a2	; point to the users command input

.next_char
	move.b	(a1)+,d0	; get a character from the table
	move.b	(a2)+,d1	; get a character from the user input

	tst.b	d0	; if we're at a null in the string from the table
	beq	.string_end	; then we're almost done with the comparison

	cmp.b	#('a'-1),d1 ; check if user input is lowercase and convert to uppercase
	bls	.not_lowercase
	cmp.b	#('z'),d1
	bhi	.not_lowercase

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
	jmp	.next_entry	; otherwise, we move on to the next entry	

.next_entry
	addq.l	#8,a0		; increment to next entry in table
	jmp	.cmd_table_loop

.no_match
	move.l	#.cmd_err,a0	; load address of command error string
	jsr	puts
	jmp	prompt_loop

.match
	move.l	(4,a0),a0	; get 4 past a0, which is the function pointer
	jsr	(a0)		; jump to the command's function
	jmp	prompt_loop

.prompt
	dc.b	">",0

.cmd_err
	dc.b	"Error: command not recognized.",cr,lf,0
	
	align 1

init_console:
	move.b	#%00011010,(DUART1+DUART_CRA)	; resets pointer to MR1A and disables Tx and Rx
	move.b	#0,(DUART1+DUART_CRA)	; removes command from register

	move.b	#%11010011,(DUART1+DUART_MRA)	; receiver controls RTS, Rx IRQ on FFULL, char error mode, no parity, 8 bits/char
	move.b	#%00000111,(DUART1+DUART_MRA)	; normal channel mode, no Tx RTS control, no CTS, 1 stop bit

	move.b	#%00000000,(DUART1+DUART_ACR)	; baud rate set 1, no delta IP interupts

	move.b	#$BB,(DUART1+DUART_CSRA)	; 9600 baud

	move.b	#0,(DUART1+DUART_IMR)	; disable interupts

	move.b	#%00000101,(DUART1+DUART_CRA)	; turn on Tx and Rx for channel A

	rts
	
clr_screen:
	; Clears the screen, customize for your terminal, or don't use it
	; ^[[2J is the ECMA-48 control sequence to clear the whole screen
	; trashes a0
	; calls puts, which trashes other stuff
	movea.l	#.escapesequence,a0
	jmp	puts	; puts will return for us.
	
.escapesequence:
	dc.b	$1B,"[H",$1B,"[2J",0	; NULL terminated string which contains escape sequence

	align 1
	
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

putc:
	; Put character
	; character passed in d0

	btst.b	#2,(DUART1+DUART_SRA)	; check TXRDY bit
	beq 	putc	; if TXRDY isn't set, then wait
	move.b	d0,(DUART1+DUART_THRA)	; send byte
	rts
	
gets:
	; Get string
	; gets characters until newline found or buffer filled
	; backspace (^H) deletes previous character as does delete
	; ignores characters with ascii value of less than #$20 except newline and BS
	; echos characters as they are typed
	; a0 holds pointer to buffer
	; d0 gets trashed
	; d1 holds buffer length (long)
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
	
	btst	#0,(DUART1+DUART_SRA)
	beq	getc
	move.b	(DUART1+DUART_RHRA),d0
	rts
	
init_mon_ram:
	lea.l	(mon_ram),a0
	move.w	#((mon_ram_end-mon_ram-1)/4),d0
.loop:
	clr.l	(a0)+
	dbra	d0,.loop
	
	move.l	#user_stack,user_sp
	move.l	#rambase+$400,user_pc
	move.l	#rambase,mon_pointer
	
	move.w	sr,(user_sr)

	rts

ver:
	movea.l	#.ver_string,a0	; setup to print version string
	jmp	puts	; puts will return for us

.ver_string	dc.b	"Joe-Mon",cr,lf
	dc.b	"A Machine Language Monitor written for the Beckman DU600 spectrophotometer and other 68K based machines.",cr,lf
	dc.b	"Written by Joseph Zatarski",cr,lf
	dc.b	"Copyright 2014,2015",cr,lf
	dc.b	"Version 1",cr,lf,0
	
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

	cmp2.b	.digitbound,d0
	bcc	.d0_digit	; if it's within the digit bounds, it's a numeral

	cmp2.b	.letterbound,d0
	bcs	.badchar	; if it's out of bounds, it's not a letter or a numeral

	sub.b	#('A'-10),d0	; convert A-F to 10-15
	jmp	.d1_conv

.d0_digit
	sub.b	#'0',d0	; convert chars 0-9 to numbers 0-9

.d1_conv
	cmp2.b	.digitbound,d1
	bcc	.d1_digit	; in bounds? then numeral

	cmp2.b	.letterbound,d1
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

.digitbound
	dc.b	'0','9'	; lower and upper bound for digit

.letterbound
	dc.b	'A','F'	; lower and upper bound for letters

	align 1

jump:
	jmp	rambase

boot:	
	andi	#%0011111111111111,sr	; clear trace bits
	ori	#%0010011100000000,sr	; set supervisor bit and change interrupt mask to %111
	movea.l	#rambase,a0	; set vector base register
	movec	a0,vbr
	movea.l	(rambase),a7	; set supervisor stack register
	movea.l	(rambase+4),a0	; jmp to reset vector
	jmp	(a0)

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
trace:
aline:
fline:
hdwbkpt:
coprocv:
formerr:
unint:
res:
spurint:
coproc0:
coproc1:
coproc2:
coproc3:
coproc4:
coproc5:
coproc6:
coproc7:
coproc8:
coproc9:
coproca:

	movea.l	#.fatal_exception,a0
	jsr	puts

	jmp	hang

.fatal_exception	dc.b	"A fatal unhandled exception has occurred.",cr,lf,0
	align	1

; dummy exception routine to at least make everything compile
psu_irq:
pp_irq:
rtc_irq:
avec4:
avec5:
kbd_irq:
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

cmd_table:
; this table holds one entry per command
; each entry consists first of a pointer to a string of the command
; next there is a pointer to the subroutine for that command
; the table ends with an entry that has a null string pointer

	dc.l	load_srec_cmd_string
	dc.l	load_srec

	dc.l	ver_cmd_string
	dc.l	ver

	dc.l	jump_cmd_string
	dc.l	jump
	
	dc.l	boot_cmd_string
	dc.l	boot

	dc.l	0	; null entry ends table

; here's the list of all the strings for command lookup

load_srec_cmd_string:
	dc.b	"LOAD",0

ver_cmd_string
	dc.b	"VER",0

jump_cmd_string
	dc.b	"JUMP",0
	
boot_cmd_string
	dc.b	"BOOT",0
	
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
user_pc		ds.w	1	; user PC
input_buff	ds.b	input_buf_len	; used as an input buffer
mon_ram_end:
