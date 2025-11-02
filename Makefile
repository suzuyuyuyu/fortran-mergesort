include ./common/mod_out_flag.mk
# compiler and flags
FC=ifx
FFLAGS=-O3
# FFLAGS+=-Wall -Wextra -g -fbacktrace -fbounds-check -ffpe-trap=invalid,zero,overflow
AR=llvm-ar rcs

LIB_OUT_DIR=lib
INC_OUT_DIR=include
FFLAGS+=$(call mod_out_flag,$(FC),$(INC_OUT_DIR))

SRC_DIR=src
INC_DIR=$(SRC_DIR)/include
OBJ_DIR=$(SRC_DIR)/obj

.SUFFIXES: .o .F90 .f90 .mod
TARGET=libsort.a
TARGET_FULLPATH=$(LIB_OUT_DIR)/$(TARGET)

FILE_BASE=\
	merge_sort

SRC=$(SRC_DIR)/$(FILE_BASE).F90
OBJ=$(patsubst $(SRC_DIR)/%.F90,$(OBJ_DIR)/%.o,$(SRC))

# default main target
all: _MKDIR $(TARGET_FULLPATH)

# archive
$(TARGET_FULLPATH): $(OBJ)
	$(AR) $(TARGET_FULLPATH) $(OBJ)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90
	$(FC) $(FFLAGS) -c $< -o $@

_MKDIR:
	mkdir -p $(LIB_OUT_DIR) $(INC_OUT_DIR) $(OBJ_DIR)

.PHONY: clean
clean: clean-dir clean-file

clean-file:
	rm -rf $(TARGET_FULLPATH) $(OBJ) $(INC_OUT_DIR)/*.mod

clean-dir:
	rmdir $(LIB_OUT_DIR) $(INC_OUT_DIR) $(OBJ_DIR) 2>/dev/null || true
