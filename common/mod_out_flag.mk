# Function to generate module directory flags based on compiler
# Usage: $(call mod_out_flag,COMPILER,MODULE_DIRECTORIES)
define mod_out_flag
$(if $(filter $(1),mpiifort ifort mpiifx ifx),$(addprefix -module ,$(2)),$(if $(filter $(1),mpifrtpx frtpx),$(addprefix -M,$(2)),$(addprefix -J,$(2))))
endef
