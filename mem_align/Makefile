CXX = ifort
CXXFLAGS= -qopenmp
CPUFLAGS = $(CXXFLAGS) -xhost
OPTFLAGS = -qopt-report=5 -qopt-report-file=$@.optrpt

CPUOBJECTS = align.o

TARGET=app-CPU
FC=ifort

.SUFFIXES: .o .f90

all: $(TARGET) instructions

%-CPU: $(CPUOBJECTS)
	$(info )
	$(info Linking the CPU executable:)
	$(FC) $(CPUFLAGS) -o $@ $(CPUOBJECTS)

.f90.o:
	$(info )
	$(info Compiling a CPU object file:)
	$(FC) -c $(CPUFLAGS) $(OPTFLAGS) -o "$@" "$<"

instructions: 
	$(info )
	$(info TO EXECUTE THE APPLICATION: )
	$(info "make run-cpu" to run the application on the host CPU)
	$(info )

run-cpu: app-CPU
	./app-CPU

clean: 
	rm -f $(CPUOBJECTS) $(MICOBJECTS) $(TARGET) *.optrpt *.mod
