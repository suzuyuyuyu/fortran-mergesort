# compiler and flags
FC=ifx
FFLAGS=-O2
FFLAGS+= -Wall -Wextra -g -fbacktrace -fbounds-check -ffpe-trap=invalid,zero,overflow

SRC_DIR=.
INC_DIR=../include
OBJ_DIR=obj
INC_OUT_DIR=include
LIB_DIR=../lib


.SUFFIXES: .o .F90 .mod
TARGET=merge_sort_example
MAIN:=main
LIB=sort

# LIBRARY=$(addsuffix .a,$(addprefix lib,$(LIB)))
SRC = $(SRC_DIR)/$(MAIN).F90
OBJ = $(patsubst $(SRC_DIR)/%.F90,$(OBJ_DIR)/%.o,$(SRC))


# default main target
all: _MKDIR $(TARGET)

# run
$(TARGET): $(OBJ)
	$(FC) -I$(INC_DIR) -L$(LIB_DIR) $(FFLAGS) $(OBJ) -o $(TARGET) -l$(LIB)

$(OBJ): $(SRC)
	$(FC) -I$(INC_DIR) $(FFLAGS) -c $< -o $@

_MKDIR:
	@mkdir -p $(OBJ_DIR)

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJ_DIR)/*.o *.mod
