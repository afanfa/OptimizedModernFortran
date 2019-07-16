CXX = ifort
CXXFLAGS= -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
CPUFLAGS = $(CXXFLAGS) -xhost
OPTFLAGS = -qopt-report=5 -qopt-report-file=$@.optrpt

CPUOBJECTS = matmul_test.o

TARGET=app-CPU
FC=ifort

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

