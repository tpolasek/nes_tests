; NES hardware constants
; Generated from SYMBOLS section of nes.cfg

; PPU registers
PPU_CTRL   = $2000
PPU_MASK   = $2001
PPU_STATUS = $2002
OAM_ADDR   = $2003
OAM_DATA   = $2004
PPU_SCROLL = $2005
PPU_ADDR   = $2006
PPU_DATA   = $2007

; APU registers
SQ1_VOL    = $4000
SQ1_SWEEP  = $4001
SQ1_LO     = $4002
SQ1_HI     = $4003
SQ2_VOL    = $4004
SQ2_SWEEP  = $4005
SQ2_LO     = $4006
SQ2_HI     = $4007
TRI_LINEAR = $4008
TRI_UNUSED = $4009
TRI_LO     = $400A
TRI_HI     = $400B
NOISE_VOL  = $400C
NOISE_UNUSED = $400D
NOISE_LO   = $400E
NOISE_HI   = $400F
DMC_FREQ   = $4010   ; also DMC_IRQ
DMC_RAW    = $4011
DMC_START  = $4012
DMC_LEN    = $4013

; APU status, DMA, and joypad
OAM_DMA    = $4014
APU_STATUS = $4015
JOY1       = $4016
JOY2       = $4017   ; same as APU_FRAME_COUNTER
APU_FRAME_COUNTER = $4017
DMC_IRQ    = $4010   ; alias for DMC_FREQ