CXX = ifort
CXXFLAGS= -qopenmp
CPUFLAGS = $(CXXFLAGS) -xhost
MICFLAGS = $(CXXFLAGS) -mmic
OPTFLAGS = -qopt-report=5 -qopt-report-file=$@.optrpt

CPUOBJECTS = vectorization.o
MICOBJECTS = vectorization.oMIC worker.oMIC

TARGET=app-CPU
FC=ifort

.SUFFIXES: .o .f90

all: $(TARGET) instructions

%-CPU: $(CPUOBJECTS)
	$(info )
	$(info Linking the CPU executable:)
	$(FC) $(CPUFLAGS) -o $@ $(CPUOBJECTS)

%-MIC: $(MICOBJECTS)
	$(info )
	$(info Linking the MIC executable:)
	$(FC) $(MICFLAGS) -o $@ $(MICOBJECTS)

.f90.o:
	$(info )
	$(info Compiling a CPU object file:)
	$(FC) -c $(CPUFLAGS) $(OPTFLAGS) -o "$@" "$<"

.cc.oMIC:
	$(info )
	$(info Compiling a MIC object file:)
	$(FC) -c $(MICFLAGS) $(OPTFLAGS) -o "$@" "$<"

instructions: 
	$(info )
	$(info TO EXECUTE THE APPLICATION: )
	$(info "make run-cpu" to run the application on the host CPU)
	$(info "make run-mic" to run the application on the coprocessor)
	$(info )

run-cpu: app-CPU
	./app-CPU

run-mic: app-MIC
	scp app-MIC mic0:~/
	ssh mic0 LD_LIBRARY_PATH=$(MIC_LD_LIBRARY_PATH) ./app-MIC

clean: 
	rm -f $(CPUOBJECTS) $(MICOBJECTS) $(TARGET) *.optrpt

