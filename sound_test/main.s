; APU Sound Test Program for NES
; Tests the APU's square wave channels with 400Hz tone

.segment "HEADER"
    .byte "NES", $1A      ; iNES header identifier
    .byte 2               ; 2x 16KB PRG ROM
    .byte 1               ; 1x 8KB CHR ROM
    .byte $01, $00        ; Mapper 0, vertical mirroring

.segment "ZEROPAGE"
; Zero page variables
nmi_ready:    .res 1      ; Flag to indicate NMI has occurred
temp:         .res 1      ; Temporary variable
frame_count:  .res 1      ; Counts frames for timing
current_channel: .res 1   ; Current active channel (1 or 2)
switch_timer: .res 1      ; Timer for switching channels

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
    STA nmi_ready
    STA frame_count
    LDA #1
    STA current_channel  ; Start with channel 1
    LDA #120             ; Switch every 2 seconds (120 frames)
    STA switch_timer

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

    ; Clear nametable with spaces (tile 32)
    LDA $2002             ; Reset PPU address latch
    LDA #$20
    STA $2006             ; Nametable 0 address high
    LDA #$00
    STA $2006             ; Nametable 0 address low

    LDX #0
    LDY #0
    LDA #32               ; Use space character (tile 32)
clear_nametable:
    STA $2007
    INX
    BNE clear_nametable
    INY
    CPY #4
    BCC clear_nametable

    ; Clear attribute table with 0 (all palette 0 = red text)
    LDA $2002             ; Reset PPU address latch
    LDA #$23
    STA $2006             ; Attribute table address high
    LDA #$C0
    STA $2006             ; Attribute table address low ($23C0)

    LDX #64               ; 64 bytes in attribute table
    LDA #0
clear_attributes:
    STA $2007
    DEX
    BNE clear_attributes

    ; Write main test message to nametable (row 14, col 9)
    LDA $2002             ; Reset PPU address latch
    LDA #$21
    STA $2006             ; Nametable address high
    LDA #$C9
    STA $2006             ; Nametable address low ($21C9 = row 14, col 9)

    LDX #0
write_message:
    LDA message, X
    BEQ message_done
    STA $2007
    INX
    JMP write_message
message_done:

    ; Write channel message to nametable (row 17, col 9)
    LDA $2002             ; Reset PPU address latch
    LDA #$22
    STA $2006             ; Nametable address high
    LDA #$29
    STA $2006             ; Nametable address low ($2229 = row 17, col 9)

    LDX #0
write_channel_msg:
    LDA channel_message, X
    BEQ channel_msg_done
    STA $2007
    INX
    JMP write_channel_msg
channel_msg_done:

    ; Write initial channel number (1)
    LDA #$31              ; ASCII '1'
    STA $2007

    ; Set attribute table for message area (row 14, cols 9-24) to use palette 2 (yellow)
    ; Message is at row 14, which maps to attribute row 3 (14/4=3)
    ; Attribute table starts at $23C0, row 3 starts at offset 3*8 = 24 = $18
    LDA $2002             ; Reset PPU address latch
    LDA #$23
    STA $2006
    LDA #$DA              ; $23C0 + $1A = $23DA (attribute byte for cols 8-11)
    STA $2006

    ; Write palette 2 (%10) to bottom half (bits 4-7) for columns 8-27
    ; Byte controls 4x4 tiles: bits 0-1=top-left, 2-3=top-right, 4-5=bottom-left, 6-7=bottom-right
    ; Row 14 is in bottom half, so we set bits 4-5 and 6-7 to %10 (palette 2)
    LDA #$A8              ; %10101000 = palette 2 for both bottom quadrants
    STA $2007             ; $23DA (cols 8-11)
    STA $2007             ; $23DB (cols 12-15)
    STA $2007             ; $23DC (cols 16-19)
    STA $2007             ; $23DD (cols 20-23)
    STA $2007             ; $23DE (cols 24-27)

    ; Set attribute table for channel message area (row 17, cols 9-20) to use palette 2
    ; Row 17 / 4 = 4.25, so it's in attribute row 4
    ; Attribute table offset: 4 * 8 = 32 = $20
    LDA $2002             ; Reset PPU address latch
    LDA #$23
    STA $2006
    LDA #$E2              ; $23C0 + $22 = $23E2 (attribute byte for cols 8-11)
    STA $2006

    ; Row 17 is in top half of the 4x4 block, so we set bits 2-3 and 0-1 to %10 (palette 2)
    LDA #$2A              ; %00101010 = palette 2 for both top quadrants
    STA $2007             ; $23E2 (cols 8-11)
    STA $2007             ; $23E3 (cols 12-15)
    STA $2007             ; $23E4 (cols 16-19)
    STA $2007             ; $23E5 (cols 20-23)

    ; Initialize APU
    JSR init_apu

    ; Start with Square Wave 1
    JSR enable_square1

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

    ; Decrement switch timer
    DEC switch_timer
    BNE skip_switch

    ; Reset switch timer
    LDA #120
    STA switch_timer

    ; Toggle channel
    LDA current_channel
    CMP #1
    BEQ switch_to_2

    ; Switch to channel 1
    LDA #1
    STA current_channel
    JSR enable_square1
    JMP update_display

switch_to_2:
    ; Switch to channel 2
    LDA #2
    STA current_channel
    JSR enable_square2

update_display:
    JSR update_channel_display

skip_switch:
    JMP main_loop

; NMI interrupt handler
nmi:
    ; Save registers
    PHA
    TXA
    PHA
    TYA
    PHA

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

; Initialize APU
init_apu:
    ; Disable APU channels
    LDA #$00
    STA $4015

    ; Initialize frame counter (4-step mode, no IRQ, reset)
    LDA #$C0
    STA $4017

    ; Configure Square Wave 1 (400Hz, 50% duty cycle, volume 15)
    LDA #%10111111        ; 50% duty, constant volume, volume = 15
    STA $4000
    LDA #$00              ; No sweep
    STA $4001
    LDA #$17              ; Timer low byte (279 & $FF)
    STA $4002
    LDA #$01              ; Timer high 3 bits ((279 >> 8) & $07)
    STA $4003

    ; Configure Square Wave 2 (400Hz, 50% duty cycle, volume 15)
    LDA #%10111111        ; 50% duty, constant volume, volume = 15
    STA $4004
    LDA #$00              ; No sweep
    STA $4005
    LDA #$17              ; Timer low byte (279 & $FF)
    STA $4006
    LDA #$01              ; Timer high 3 bits ((279 >> 8) & $07)
    STA $4007

    RTS

; Enable Square Wave 1, disable Square Wave 2
enable_square1:
    LDA #%00000001        ; Enable square 1 only
    STA $4015
    RTS

; Enable Square Wave 2, disable Square Wave 1
enable_square2:
    LDA #%00000010        ; Enable square 2 only
    STA $4015
    RTS

; Update channel display subroutine
update_channel_display:
    LDA $2002             ; Reset PPU address latch

    ; Position for channel display (row 17, col 10)
    LDA #$22
    STA $2006
    LDA #$2A
    STA $2006

    ; Display current channel number
    LDA current_channel
    CLC
    ADC #$30              ; Convert to ASCII digit
    STA $2007

    RTS

; IRQ handler (unused)
irq:
    RTI

; Data section
palette_data:
    .byte $21, $30, $27, $16   ; Background palette 0 - blue bg, white text
    .byte $0F, $06, $16, $26   ; Background palette 1
    .byte $0F, $08, $18, $28   ; Background palette 2
    .byte $0F, $0A, $1A, $2A   ; Background palette 3
    .byte $0F, $00, $10, $20   ; Sprite palette 0 (unused)
    .byte $0F, $00, $10, $20   ; Sprite palette 1 (unused)
    .byte $0F, $00, $10, $20   ; Sprite palette 2 (unused)
    .byte $0F, $00, $10, $20   ; Sprite palette 3 (unused)

message:
    .byte "APU TEST: 400HZ", 0

channel_message:
    .byte "CHANNEL: ", 0

; Interrupt vectors
.segment "VECTORS"
    .word nmi    ; NMI vector
    .word reset  ; Reset vector
    .word irq    ; IRQ vector

.segment "CHARS"
    ; Tiles 0-31: Empty/unused (16 bytes per tile)
    .res 32*16, $00

    ; Tile 32 ($20): ASCII ' ' (space) - blank tile
    .byte $00, $00, $00, $00, $00, $00, $00, $00  ; Bitplane 0
    .byte $00, $00, $00, $00, $00, $00, $00, $00  ; Bitplane 1

    ; Tiles 33-47: Empty
    .res 15*16, $00

    ; Tile 48 ($30): ASCII '0'
    .byte $3C, $66, $6E, $76, $66, $66, $3C, $00  ; Bitplane 0
    .byte $3C, $66, $6E, $76, $66, $66, $3C, $00  ; Bitplane 1
    ; Tile 49 ($31): ASCII '1'
    .byte $18, $38, $18, $18, $18, $18, $7E, $00
    .byte $18, $38, $18, $18, $18, $18, $7E, $00
    ; Tile 50 ($32): ASCII '2'
    .byte $3C, $66, $06, $0C, $18, $30, $7E, $00
    .byte $3C, $66, $06, $0C, $18, $30, $7E, $00
    ; Tile 51 ($33): ASCII '3'
    .byte $3C, $66, $06, $1C, $06, $66, $3C, $00
    .byte $3C, $66, $06, $1C, $06, $66, $3C, $00
    ; Tile 52 ($34): ASCII '4'
    .byte $0C, $1C, $3C, $6C, $7E, $0C, $0C, $00
    .byte $0C, $1C, $3C, $6C, $7E, $0C, $0C, $00
    ; Tile 53 ($35): ASCII '5'
    .byte $7E, $60, $7C, $06, $06, $66, $3C, $00
    .byte $7E, $60, $7C, $06, $06, $66, $3C, $00
    ; Tile 54 ($36): ASCII '6'
    .byte $3C, $66, $60, $7C, $66, $66, $3C, $00
    .byte $3C, $66, $60, $7C, $66, $66, $3C, $00
    ; Tile 55 ($37): ASCII '7'
    .byte $7E, $06, $0C, $18, $30, $30, $30, $00
    .byte $7E, $06, $0C, $18, $30, $30, $30, $00
    ; Tile 56 ($38): ASCII '8'
    .byte $3C, $66, $66, $3C, $66, $66, $3C, $00
    .byte $3C, $66, $66, $3C, $66, $66, $3C, $00
    ; Tile 57 ($39): ASCII '9'
    .byte $3C, $66, $66, $3E, $06, $66, $3C, $00
    .byte $3C, $66, $66, $3E, $06, $66, $3C, $00

    ; Tile 58 ($3A): ASCII ':'
    .byte $00, $18, $18, $00, $00, $18, $18, $00
    .byte $00, $18, $18, $00, $00, $18, $18, $00
    ; Tiles 59-60: Empty
    .res 2*16, $00

    ; Tile 61 ($3D): ASCII '='
    .byte $00, $00, $7E, $00, $7E, $00, $00, $00
    .byte $00, $00, $7E, $00, $7E, $00, $00, $00
    ; Tiles 62-64: Empty
    .res 3*16, $00

    ; Tile 65 ($41): ASCII 'A'
    .byte $3C, $66, $66, $7E, $66, $66, $66, $00
    .byte $3C, $66, $66, $7E, $66, $66, $66, $00
    ; Tile 66 ($42): ASCII 'B'
    .byte $7C, $66, $66, $7C, $66, $66, $7C, $00
    .byte $7C, $66, $66, $7C, $66, $66, $7C, $00
    ; Tile 67 ($43): ASCII 'C'
    .byte $3C, $66, $60, $60, $60, $66, $3C, $00
    .byte $3C, $66, $60, $60, $60, $66, $3C, $00
    ; Tile 68 ($44): ASCII 'D'
    .byte $78, $6C, $66, $66, $66, $6C, $78, $00
    .byte $78, $6C, $66, $66, $66, $6C, $78, $00
    ; Tile 69 ($45): ASCII 'E'
    .byte $7E, $60, $60, $78, $60, $60, $7E, $00
    .byte $7E, $60, $60, $78, $60, $60, $7E, $00
    ; Tile 70 ($46): ASCII 'F'
    .byte $7E, $60, $60, $78, $60, $60, $60, $00
    .byte $7E, $60, $60, $78, $60, $60, $60, $00
    ; Tile 71 ($47): ASCII 'G'
    .byte $3C, $66, $60, $6E, $66, $66, $3C, $00
    .byte $3C, $66, $60, $6E, $66, $66, $3C, $00
    ; Tile 72 ($48): ASCII 'H'
    .byte $66, $66, $66, $7E, $66, $66, $66, $00
    .byte $66, $66, $66, $7E, $66, $66, $66, $00
    ; Tile 73 ($49): ASCII 'I'
    .byte $7E, $18, $18, $18, $18, $18, $7E, $00
    .byte $7E, $18, $18, $18, $18, $18, $7E, $00
    ; Tile 74 ($4A): ASCII 'J'
    .byte $06, $06, $06, $06, $06, $66, $3C, $00
    .byte $06, $06, $06, $06, $06, $66, $3C, $00
    ; Tile 75 ($4B): ASCII 'K'
    .byte $66, $6C, $78, $70, $78, $6C, $66, $00
    .byte $66, $6C, $78, $70, $78, $6C, $66, $00
    ; Tile 76 ($4C): ASCII 'L'
    .byte $60, $60, $60, $60, $60, $60, $7E, $00
    .byte $60, $60, $60, $60, $60, $60, $7E, $00
    ; Tile 77 ($4D): ASCII 'M'
    .byte $63, $77, $7F, $6B, $63, $63, $63, $00
    .byte $63, $77, $7F, $6B, $63, $63, $63, $00
    ; Tile 78 ($4E): ASCII 'N'
    .byte $66, $76, $7E, $7E, $6E, $66, $66, $00
    .byte $66, $76, $7E, $7E, $6E, $66, $66, $00
    ; Tile 79 ($4F): ASCII 'O'
    .byte $3C, $66, $66, $66, $66, $66, $3C, $00
    .byte $3C, $66, $66, $66, $66, $66, $3C, $00
    ; Tile 80 ($50): ASCII 'P'
    .byte $7C, $66, $66, $7C, $60, $60, $60, $00
    .byte $7C, $66, $66, $7C, $60, $60, $60, $00
    ; Tile 81 ($51): ASCII 'Q'
    .byte $3C, $66, $66, $66, $6A, $6C, $36, $00
    .byte $3C, $66, $66, $66, $6A, $6C, $36, $00
    ; Tile 82 ($52): ASCII 'R'
    .byte $7C, $66, $66, $7C, $78, $6C, $66, $00
    .byte $7C, $66, $66, $7C, $78, $6C, $66, $00
    ; Tile 83 ($53): ASCII 'S'
    .byte $3C, $66, $60, $3C, $06, $66, $3C, $00
    .byte $3C, $66, $60, $3C, $06, $66, $3C, $00
    ; Tile 84 ($54): ASCII 'T'
    .byte $7E, $18, $18, $18, $18, $18, $18, $00
    .byte $7E, $18, $18, $18, $18, $18, $18, $00
    ; Tile 85 ($55): ASCII 'U'
    .byte $66, $66, $66, $66, $66, $66, $3C, $00
    .byte $66, $66, $66, $66, $66, $66, $3C, $00
    ; Tile 86 ($56): ASCII 'V'
    .byte $66, $66, $66, $66, $66, $3C, $18, $00
    .byte $66, $66, $66, $66, $66, $3C, $18, $00
    ; Tile 87 ($57): ASCII 'W'
    .byte $63, $63, $63, $6B, $7F, $77, $63, $00
    .byte $63, $63, $63, $6B, $7F, $77, $63, $00
    ; Tile 88 ($58): ASCII 'X'
    .byte $66, $66, $3C, $18, $3C, $66, $66, $00
    .byte $66, $66, $3C, $18, $3C, $66, $66, $00
    ; Tile 89 ($59): ASCII 'Y'
    .byte $66, $66, $66, $3C, $18, $18, $18, $00
    .byte $66, $66, $66, $3C, $18, $18, $18, $00
    ; Tile 90 ($5A): ASCII 'Z'
    .byte $7E, $06, $0C, $18, $30, $60, $7E, $00
    .byte $7E, $06, $0C, $18, $30, $60, $7E, $00

    ; Tiles 91-255: Fill remaining CHR ROM
    .res 8192-91*16, $00