
# Output files
TARGET = build/build.nes
DBGFILE = build/build.dbg

# Source files
SOURCES = main.s

# Config file
CONFIG = nes.cfg

# Compiler
CC65 = cl65

# Flags
CFLAGS = --debug-info -C $(CONFIG) -Wl --dbgfile,"$(DBGFILE)" --target nes

# Default target
all: $(TARGET)

# Build the NES ROM
$(TARGET): $(SOURCES) $(CONFIG)
	rm -rf build
	@mkdir -p build
	
	$(CC65) $(CFLAGS) $(SOURCES) -o $(TARGET)

# Clean build artifacts
clean:
	rm -f $(TARGET) $(DBGFILE) *.o

# Run (if you have an emulator configured)
run: $(TARGET)
	@echo "Run with your NES emulator: $(TARGET)"

.PHONY: all clean run
