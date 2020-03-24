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

CPUOBJECTS = SoA.o

TARGET=app-CPU

.SUFFIXES: .o .f90

all: $(TARGET)

%-CPU: $(CPUOBJECTS)
	$(info )
	$(info Linking the CPU executable:)
	$(FC) $(CPUFLAGS) -o $@ $(CPUOBJECTS)

.f90.o:
	$(info )
	$(info Compiling a CPU object file:)
	$(FC) -c $(CPUFLAGS) $(OPTFLAGS) -o "$@" "$<"

clean:
	rm -f $(CPUOBJECTS) $(MICOBJECTS) $(TARGET) *.optrpt *.mod
