ifeq ($(COMPILER),gfortran)
  FC= gfortran
  FCFLAGS = -fopenmp
  CPUFLAGS = $(FCFLAGS) -O3
  OPTFLAGS = -fopt-info-missed=missed.all
else
  FC= ifort
  FCFLAGS= -qopenmp
  CPUFLAGS = $(FCFLAGS) -xhost
  OPTFLAGS = -qopt-report=5 -qopt-report-file=$@.optrpt
endif

SM-OBJECTS = strip-mining.o
NOBEN-OBJECTS = no_benefits.o

TARGET= no_benefits strip-mining

.SUFFIXES: .o .f90

all: $(TARGET)

%-mining: $(SM-OBJECTS)
	  $(info )
	  $(info Linking the CPU executable:)
	  $(FC) $(CPUFLAGS) -o $@ $(SM-OBJECTS)

%_benefits: $(NOBEN-OBJECTS)
	    $(info )
	    $(info Linking the CPU executable:)
	    $(FC) $(CPUFLAGS) -o $@ $(NOBEN-OBJECTS)

.f90.o:
	$(info )
	$(info Compiling a CPU object file:)
	$(FC) -c $(CPUFLAGS) $(OPTFLAGS) -o "$@" "$<"

clean: 
	rm -f $(SM-OBJECTS) $(NOBEN-OBJECTS) $(MICOBJECTS) $(TARGET) *.optrpt *.mod
