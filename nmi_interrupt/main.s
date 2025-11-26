; NMI Test Program for NES
; Tests the PPU's NMI interrupt functionality


.segment "HEADER"
    .byte "NES", $1A      ; iNES header identifier
    .byte 2               ; 2x 16KB PRG ROM
    .byte 1               ; 1x 8KB CHR ROM
    .byte $01, $00        ; Mapper 0, vertical mirroring

.segment "ZEROPAGE"
; Zero page variables
nmi_count:    .res 1      ; Counts NMIs received
nmi_ready:    .res 1      ; Flag to indicate NMI has occurred
temp:         .res 1      ; Temporary variable
frame_count:  .res 1      ; Counts frames for timing

.segment "OAM"
oam_data:     .res 256    ; OAM buffer for sprites

.segment "CODE"
; Reset vector
reset:
    SEI                   ; Disable interrupts
    CLD                   ; Clear decimal mode
    LDX #$40
    STX $4017             ; Disable APU frame IRQ
    LDX #$FF
    TXS                   ; Initialize stack pointer
    INX                   ; X = 0
    STX $2000             ; Disable PPU rendering
    STX $2001             ; Disable PPU rendering
    STX $4010             ; Disable DMC IRQs

    ; Wait for first vblank
vblankwait1:
    BIT $2002
    BPL vblankwait1

    ; Clear RAM
    TXA                   ; A = 0
clear_ram:
    STA $0000, X
    STA $0100, X
    STA $0200, X
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X
    INX
    BNE clear_ram

    ; Wait for second vblank
vblankwait2:
    BIT $2002
    BPL vblankwait2

    ; Initialize variables
    LDA #0
    STA nmi_count
    STA nmi_ready
    STA frame_count

    ; Set up palette
    LDA $2002             ; Reset PPU address latch
    LDA #$3F
    STA $2006             ; PPU address high byte
    LDA #$00
    STA $2006             ; PPU address low byte
    
    ; Load palette data
    LDX #0
load_palette:
    LDA palette_data, X
    STA $2007
    INX
    CPX #32
    BNE load_palette

    ; Clear nametable
    LDA $2002             ; Reset PPU address latch
    LDA #$20
    STA $2006             ; Nametable 0 address high
    LDA #$00
    STA $2006             ; Nametable 0 address low
    
    LDX #0
    LDY #0
    LDA #0
clear_nametable:
    STA $2007
    INX
    BNE clear_nametable
    INY
    CPY #4
    BNE clear_nametable

    ; Write test message to nametable
    LDA $2002             ; Reset PPU address latch
    LDA #$21
    STA $2006             ; Nametable address high
    LDA #$C9
    STA $2006             ; Nametable address low
    
    LDX #0
write_message:
    LDA message, X
    BEQ message_done
    STA $2007
    INX
    JMP write_message
message_done:

    ; Enable NMI and set background pattern table
    LDA #%10000000        ; Enable NMI
    STA $2000
    
    ; Enable background rendering
    LDA #%00001110        ; Enable background, disable sprites
    STA $2001

    ; Main program loop
main_loop:
    ; Wait for NMI
    LDA nmi_ready
    BEQ main_loop
    
    ; Clear NMI ready flag
    LDA #0
    STA nmi_ready
    
    ; Update frame counter
    INC frame_count
    
    ; Every 60 frames (approx 1 second), update NMI counter display
    LDA frame_count
    CMP #60
    BCC skip_update
    
    ; Reset frame counter
    LDA #0
    STA frame_count
    
    ; Update NMI counter display
    JSR update_counter_display
    
skip_update:
    JMP main_loop

; NMI interrupt handler
nmi:
    ; Save registers
    PHA
    TXA
    PHA
    TYA
    PHA
    
    ; Increment NMI counter
    INC nmi_count
    
    ; Set NMI ready flag
    LDA #1
    STA nmi_ready
    
    ; Reset scroll
    LDA #0
    STA $2005
    STA $2005
    
    ; Restore registers
    PLA
    TAY
    PLA
    TAX
    PLA
    
    RTI

; Update counter display subroutine
update_counter_display:
    LDA $2002             ; Reset PPU address latch
    
    ; Position for counter display
    LDA #$22
    STA $2006
    LDA #$09
    STA $2006
    
    ; Display NMI count in hexadecimal
    LDA nmi_count
    LSR A
    LSR A
    LSR A
    LSR A
    TAX
    LDA hex_digits, X
    STA $2007
    
    LDA nmi_count
    AND #$0F
    TAX
    LDA hex_digits, X
    STA $2007
    
    RTS

; IRQ handler (unused)
irq:
    RTI

; Data section
palette_data:
    .byte $0F, $00, $10, $20   ; Background palette 0
    .byte $0F, $06, $16, $26   ; Background palette 1
    .byte $0F, $08, $18, $28   ; Background palette 2
    .byte $0F, $0A, $1A, $2A   ; Background palette 3
    .byte $0F, $00, $10, $20   ; Sprite palette 0 (unused)
    .byte $0F, $00, $10, $20   ; Sprite palette 1 (unused)
    .byte $0F, $00, $10, $20   ; Sprite palette 2 (unused)
    .byte $0F, $00, $10, $20   ; Sprite palette 3 (unused)

message:
    .byte "NMI TEST: COUNT=", 0

hex_digits:
    .byte "0123456789ABCDEF"

; Interrupt vectors
.segment "VECTORS"
    .word nmi    ; NMI vector
    .word reset  ; Reset vector
    .word irq    ; IRQ vector

.segment "CHARS"
    .res 8192    ; 8KB for CHR ROM (pattern tables)