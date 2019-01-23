CXX = ifort
CXXFLAGS=
CPUFLAGS = $(CXXFLAGS) -xhost
OPTFLAGS = -qopt-report -qopt-report-file=$@.optrpt

CPUOBJECTS = noise.o

TARGET=app-CPU
CXX=ifort

.SUFFIXES: .o .f90

all: $(TARGET)

%-CPU: $(CPUOBJECTS)
	$(info )
	$(info Linking the CPU executable:)
	$(CXX) $(CPUFLAGS) -o $@ $(CPUOBJECTS)

.f90.o:
	$(info )
	$(info Compiling a CPU object file:)
	$(CXX) -c $(CPUFLAGS) $(OPTFLAGS) -o "$@" "$<"

clean: 
	rm -f $(CPUOBJECTS) $(TARGET) *.optrpt *.mod
