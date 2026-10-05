FC = gfortran
FFLAGS = -std=legacy -ffixed-line-length-none
MACHINE_SOURCE ?= ../lib/linux/machine.f
BUILD_DIR ?= build
MORTRAN_EXE ?= $(BUILD_DIR)/mortran3_inc.exe

.PHONY: all
all: $(MORTRAN_EXE)

$(MORTRAN_EXE): mortran3.f $(MACHINE_SOURCE)
	mkdir -p "$(dir $@)"
	$(FC) $(FFLAGS) -o "$@" mortran3.f "$(MACHINE_SOURCE)"