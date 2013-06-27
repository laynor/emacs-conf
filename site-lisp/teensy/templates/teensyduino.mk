# Teensyduino command-line project-independent Makefile
#
# Create a small Makefile that includes this one after optionally setting a few
# variables.
# TEENSY defaults to 2 and may be set to 1, 1p, 2, or 2p depending on the version of your Teensy board.
# TEENSY_CORE defaults to hid and may be set to hid, midi, serial, or disk.
# F_CPU defaults to 16MHz.
# TARGET defaults to the basename of the first .pde file in the current directory.
# ARDUINO_LIBS may contain a set of names of subdirectories of ARDUINO/libraries.
# OBJDIR defaults to .obj and may be set to a directory name to store compiled files.
# If make complains about 'Circular Makefile.out <- Makefile dependency dropped', add 'Makefile:;' to your Makefile.
#
# Once you've made your Makefile, you can run 'make upload' to build and upload
# your sketch to your Teensy.
#
# Besides make upload you can also
#   make            - no upload
#   make clean      - remove all our dependencies
#   make depends    - update dependencies
#
# All sources should be in the current directory and can include:
#  - at most one .pde file which will be treated as C++ after the standard
#    Arduino header and footer have been affixed.
#  - any number of .c, .cpp, .s and .h files

TEENSY ?= 2
MCU ?= $(MCU_TEENSY_$(TEENSY))
F_CPU ?= 16000000
OBJDIR ?= .out
TEENSY_CORE ?= hid
TARGET ?= $(firstword $(basename $(wildcard *.pde)))
ARDUINO ?= $(ARDUINO_$(shell uname))
#ARDUINO_LIBS ?= SPI SD

ARDUINO_Darwin = /Applications/Arduino.app/Contents/Resources/Java
ARDUINO_CORE_PATH = $(ARDUINO)/hardware/teensy/cores/teensy
TOOLS_PATH_Linux = $(ARDUINO)/hardware/tools
TOOLS_PATH_Darwin = $(TOOLS_PATH_Linux)/avr/bin
TOOLS_PATH = $(TOOLS_PATH_$(shell uname))

ifeq (,$(wildcard $(TOOLS_PATH)/avr-gcc))
	AVR_TOOLS_PATH := $(subst /avr-gcc,,$(shell which avr-gcc))
else
	AVR_TOOLS_PATH := $(TOOLS_PATH)
endif

TEENSY_CORE_hid_CPPFLAGS += -DUSB_HID -DLAYOUT_US_ENGLISH
TEENSY_CORE_serial_CPPFLAGS += -DUSB_SERIAL
TEENSY_CORE_disk_CPPFLAGS += -DUSB_DISK
TEENSY_CORE_midi_CPPFLAGS += -DUSB_MIDI
CPPFLAGS += $(TEENSY_CORE_$(TEENSY_CORE)_CPPFLAGS)

MCU_TEENSY_1 = at90usb162
MCU_TEENSY_1p = at90usb646
MCU_TEENSY_2 = atmega32u4
MCU_TEENSY_2p = at90usb1286

# Local sources
LOCAL_C_SRCS = $(wildcard *.c)
LOCAL_CPP_SRCS = $(wildcard *.cpp)
LOCAL_CC_SRCS = $(wildcard *.cc)
LOCAL_PDE_SRCS = $(wildcard *.pde)
LOCAL_AS_SRCS = $(wildcard *.S)
LOCAL_OBJ_FILES = $(LOCAL_C_SRCS:.c=.o) $(LOCAL_CPP_SRCS:.cpp=.o)
LOCAL_OBJ_FILES += $(LOCAL_CC_SRCS:.cc=.o) $(LOCAL_PDE_SRCS:.pde=.o)
LOCAL_OBJ_FILES += $(LOCAL_AS_SRCS:.S=.o)
LOCAL_OBJS = $(patsubst %,$(OBJDIR)/%,$(LOCAL_OBJ_FILES))

# Dependency files
DEPS = $(LOCAL_OBJS:.o=.d)

# core sources
ifeq ($(strip $(NO_CORE)),)
CORE_C_SRCS = $(wildcard $(ARDUINO_CORE_PATH)/*.c)
CORE_CPP_SRCS = $(wildcard $(ARDUINO_CORE_PATH)/*.cpp)
CORE_OBJ_FILES = $(CORE_C_SRCS:.c=.o) $(CORE_CPP_SRCS:.cpp=.o)
CORE_OBJS = $(patsubst $(ARDUINO_CORE_PATH)/%, $(OBJDIR)/%,$(CORE_OBJ_FILES))
endif

OBJS = $(LOCAL_OBJS) $(CORE_OBJS)

# The name of the main targets
TARGET_HEX = $(OBJDIR)/$(TARGET).hex
TARGET_ELF = $(OBJDIR)/$(TARGET).elf
TARGETS = $(OBJDIR)/$(TARGET).*

# A list of dependencies
DEP_FILE = $(OBJDIR)/depends.mk

# Names of executables
CC = $(AVR_TOOLS_PATH)/avr-gcc
CXX = $(AVR_TOOLS_PATH)/avr-g++
OBJCOPY = $(AVR_TOOLS_PATH)/avr-objcopy
OBJDUMP = $(AVR_TOOLS_PATH)/avr-objdump
AR = $(AVR_TOOLS_PATH)/avr-ar
SIZE = $(AVR_TOOLS_PATH)/avr-size
NM = $(AVR_TOOLS_PATH)/avr-nm
POST_COMPILE = $(TOOLS_PATH)/teensy_post_compile
REBOOT = $(TOOLS_PATH)/teensy_reboot

# General arguments
SYS_LIBS = $(patsubst %,$(ARDUINO_LIB_PATH)/%,$(ARDUINO_LIBS))
SYS_INCLUDES = $(patsubst %,-I%,$(SYS_LIBS))
SYS_OBJS = $(wildcard $(patsubst %,%/*.o,$(SYS_LIBS)))

CPPFLAGS += -mmcu=$(MCU) -DF_CPU=$(F_CPU) -g -Os -w -Wall -ffunction-sections -fdata-sections
CPPFLAGS += -I. -I$(ARDUINO_CORE_PATH) $(SYS_INCLUDES)
CFLAGS += -std=gnu99
CXXFLAGS += -fno-exceptions
ASFLAGS += -mmcu=$(MCU) -I. -x assembler-with-cpp
LDFLAGS += -mmcu=$(MCU) -lm -Wl,--gc-sections -Os

PDEHEADER = \\\#include \"WProgram.h\"

all: $(TARGET_HEX)

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(TARGET_ELF): $(OBJS)
	$(CC) -o $@ $(OBJS) $(SYS_OBJS) $(LDFLAGS)

$(DEP_FILE): $(OBJDIR) $(DEPS)
	cat $(DEPS) > $(DEP_FILE)

upload: $(TARGET_HEX)
	-$(POST_COMPILE) -board=teensy$(TEENSY) -tools=$(TOOLS_PATH)/ -path=$(CURDIR)/$(OBJDIR) -file=$(TARGET)
	$(REBOOT) || $(REBOOT)

clean:
	rm -rf $(OBJDIR) $(TARGET).hex

depends: $(OBJDIR) $(DEPS)
	@cat $(DEPS) > $(DEP_FILE)

.PHONY: all clean depends upload

# Implicit rules for building everything (needed to get everything in
# the right directory)
#
# Rather than mess around with VPATH there are quasi-duplicate rules
# here for building e.g. a system C++ file and a local C++
# file. Besides making things simpler now, this would also make it
# easy to change the build options in future

# normal local sources
# .o rules are for objects, .d for dependency tracking
# there seems to be an awful lot of duplication here!!!
$(OBJDIR)/%.o: %.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $< -o $@
$(OBJDIR)/%.o: %.cc
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@
$(OBJDIR)/%.o: %.cpp
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@
$(OBJDIR)/%.o: %.S
	$(CC) -c $(CPPFLAGS) $(ASFLAGS) $< -o $@
$(OBJDIR)/%.o: %.s
	$(CC) -c $(CPPFLAGS) $(ASFLAGS) $< -o $@
$(OBJDIR)/%.d: %.c
	$(CC) -MM $(CPPFLAGS) $(CFLAGS) $< -MF $@ -MT $(@:.d=.o)
$(OBJDIR)/%.d: %.cc
	$(CXX) -MM $(CPPFLAGS) $(CXXFLAGS) $< -MF $@ -MT $(@:.d=.o)
$(OBJDIR)/%.d: %.cpp
	$(CXX) -MM $(CPPFLAGS) $(CXXFLAGS) $< -MF $@ -MT $(@:.d=.o)
$(OBJDIR)/%.d: %.S
	$(CC) -MM $(CPPFLAGS) $(ASFLAGS) $< -MF $@ -MT $(@:.d=.o)
$(OBJDIR)/%.d: %.s
	$(CC) -MM $(CPPFLAGS) $(ASFLAGS) $< -MF $@ -MT $(@:.d=.o)
$(OBJDIR)/%.cpp: %.pde
	echo $(PDEHEADER) > $@
	cat $< >> $@
$(OBJDIR)/%.o: $(OBJDIR)/%.cpp
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@
$(OBJDIR)/%.d: $(OBJDIR)/%.cpp
	$(CXX) -MM $(CPPFLAGS) $(CXXFLAGS) $< -MF $@ -MT $(@:.d=.o)
$(OBJDIR)/%.o: $(ARDUINO_CORE_PATH)/%.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $< -o $@
$(OBJDIR)/%.o: $(ARDUINO_CORE_PATH)/%.cpp
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@
$(OBJDIR)/%.hex: $(OBJDIR)/%.elf
	$(OBJCOPY) -O ihex -R .eeprom $< $@
$(OBJDIR)/%.eep: $(OBJDIR)/%.elf
	-$(OBJCOPY) -j .eeprom --set-section-flags=.eeprom="alloc,load" \
		--change-section-lma .eeprom=0 -O ihex $< $@
$(OBJDIR)/%.lss: $(OBJDIR)/%.elf
	$(OBJDUMP) -h -S $< > $@
$(OBJDIR)/%.sym: $(OBJDIR)/%.elf
	$(NM) -n $< > $@
$(lastword $(MAKEFILE_LIST)): ;
$(TARGET).pde.out: ;
ifneq ($(wildcard $(DEP_FILE)),)
include $(DEP_FILE)
endif

cflags: 
	@echo $(CFLAGS)

cppflags:
	@echo $(CPPFLAGS) $(CXXFLAGS)
# Copyright (C) 2010 Martin Oldfield <m@mjo.tc>, based on work that is
# Copyright Nicholas Zambetti, David A. Mellis & Hernando Barragan
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the
# License, or (at your option) any later version.
