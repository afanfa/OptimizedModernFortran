CXX = ifort
CXXFLAGS= -qopenmp
CPUFLAGS = $(CXXFLAGS) -xhost
OPTFLAGS = -qopt-report=5 -qopt-report-file=$@.optrpt

CPUOBJECTS = vectorization.o

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

run-cpu: app-CPU
	./app-CPU

clean: 
	rm -f $(CPUOBJECTS) $(TARGET) *.optrpt *.mod

