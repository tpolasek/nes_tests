; NMI Test Program for NES
; Tests the PPU's NMI interrupt functionality

.include "constants.inc"

.segment "HEADER"
    .byte "NES", $1A      ; iNES header identifier
    .byte 2               ; 2x 16KB PRG ROM
    .byte 1               ; 1x 8KB CHR ROM
    .byte $01, $00        ; Mapper 0, vertical mirroring

.segment "ZEROPAGE"
; Zero page variables
nmi_count:    .res 2      ; Counts NMIs received (16-bit: low, high)
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
    STX APU_FRAME_COUNTER             ; Disable APU frame IRQ
    LDX #$FF
    TXS                   ; Initialize stack pointer
    INX                   ; X = 0
    STX PPU_CTRL             ; Disable PPU rendering
    STX PPU_MASK             ; Disable PPU rendering
    STX DMC_IRQ             ; Disable DMC IRQs

    ; Wait for first vblank
vblankwait1:
    BIT PPU_STATUS
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
    BIT PPU_STATUS
    BPL vblankwait2

    ; Initialize variables
    LDA #0
    STA nmi_count       ; Clear low byte
    STA nmi_count+1     ; Clear high byte
    STA nmi_ready
    STA frame_count

    ; Set up palette
    LDA PPU_STATUS             ; Reset PPU address latch
    LDA #$3F
    STA PPU_ADDR             ; PPU address high byte
    LDA #$00
    STA PPU_ADDR             ; PPU address low byte
    
    ; Load palette data
    LDX #0
load_palette:
    LDA palette_data, X
    STA PPU_DATA
    INX
    CPX #32
    BNE load_palette

    ; Clear nametable with spaces (tile 32)
    LDA PPU_STATUS             ; Reset PPU address latch
    LDA #$20
    STA PPU_ADDR             ; Nametable 0 address high
    LDA #$00
    STA PPU_ADDR             ; Nametable 0 address low

    LDX #0
    LDY #0
    LDA #32               ; Use space character (tile 32)
clear_nametable:
    STA PPU_DATA
    INX
    BNE clear_nametable
    INY
    CPY #4
    BCC clear_nametable

    ; Clear attribute table with 0 (all palette 0 = red text)
    LDA PPU_STATUS             ; Reset PPU address latch
    LDA #$23
    STA PPU_ADDR             ; Attribute table address high
    LDA #$C0
    STA PPU_ADDR             ; Attribute table address low ($23C0)

    LDX #64               ; 64 bytes in attribute table
    LDA #0
clear_attributes:
    STA PPU_DATA
    DEX
    BNE clear_attributes

    ; Write test message to nametable
    LDA PPU_STATUS             ; Reset PPU address latch
    LDA #$21
    STA PPU_ADDR             ; Nametable address high
    LDA #$C9
    STA PPU_ADDR             ; Nametable address low
    
    LDX #0
write_message:
    LDA message, X
    BEQ message_done
    STA PPU_DATA
    INX
    JMP write_message
message_done:

    ; Set attribute table for message area (row 14, cols 9-24) to use palette 2 (yellow)
    ; Message is at row 14, which maps to attribute row 3 (14/4=3)
    ; Attribute table starts at $23C0, row 3 starts at offset 3*8 = 24 = $18
    LDA PPU_STATUS             ; Reset PPU address latch
    LDA #$23
    STA PPU_ADDR
    LDA #$DA              ; $23C0 + $1A = $23DA (attribute byte for cols 8-11)
    STA PPU_ADDR

    ; Write palette 2 (%10) to bottom half (bits 4-7) for columns 8-27
    ; Byte controls 4x4 tiles: bits 0-1=top-left, 2-3=top-right, 4-5=bottom-left, 6-7=bottom-right
    ; Row 14 is in bottom half, so we set bits 4-5 and 6-7 to %10 (palette 2)
    LDA #$A8              ; %10101000 = palette 2 for both bottom quadrants
    STA PPU_DATA             ; $23DA (cols 8-11)
    STA PPU_DATA             ; $23DB (cols 12-15)
    STA PPU_DATA             ; $23DC (cols 16-19)
    STA PPU_DATA             ; $23DD (cols 20-23)
    STA PPU_DATA             ; $23DE (cols 24-27)

    ; Enable NMI and set background pattern table
    LDA #%10000000        ; Enable NMI
    STA PPU_CTRL
    
    ; Enable background rendering
    LDA #%00001110        ; Enable background, disable sprites
    STA PPU_MASK

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

    ; Increment 16-bit NMI counter
    INC nmi_count       ; Increment low byte
    BNE nmi_no_carry    ; If not zero, skip high byte
    INC nmi_count+1     ; Increment high byte
nmi_no_carry:

    ; Set NMI ready flag
    LDA #1
    STA nmi_ready
    
    ; Reset scroll
    LDA #0
    STA PPU_SCROLL
    STA PPU_SCROLL
    
    ; Restore registers
    PLA
    TAY
    PLA
    TAX
    PLA
    
    RTI

; Update counter display subroutine
update_counter_display:
    LDA PPU_STATUS             ; Reset PPU address latch

    ; Position for counter display
    LDA #$22
    STA PPU_ADDR
    LDA #$09
    STA PPU_ADDR

    ; Display high byte (2 digits)
    LDA nmi_count+1       ; Get high byte
    LSR A
    LSR A
    LSR A
    LSR A
    TAX
    LDA hex_digits, X
    STA PPU_DATA

    LDA nmi_count+1
    AND #$0F
    TAX
    LDA hex_digits, X
    STA PPU_DATA

    ; Display low byte (2 digits)
    LDA nmi_count         ; Get low byte
    LSR A
    LSR A
    LSR A
    LSR A
    TAX
    LDA hex_digits, X
    STA PPU_DATA

    LDA nmi_count
    AND #$0F
    TAX
    LDA hex_digits, X
    STA PPU_DATA

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
    .byte "NMI TEST: COUNT=", 0

hex_digits:
    .byte "0123456789ABCDEF"

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