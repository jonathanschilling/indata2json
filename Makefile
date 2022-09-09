
F90:=gfortran -cpp
F77:=gfortran --std=legacy -cpp

# Specify the type of machine this is compiled for.
# Look into LIBSTELL/Sources/Miscel/getcarg.f90 for available options.
HOST:=LINUX

.PHONY: all clean

all: indata2json
clean:
	rm -f *.o *.mod
	rm -f indata2json

json.o: json-fortran/json.f90
	$(F90) -c json-fortran/json.f90 -o json.o

getcarg.o: LIBSTELL/Sources/Miscel/getcarg.f90
	$(F90) -D$(HOST) -c LIBSTELL/Sources/Miscel/getcarg.f90 -o getcarg.o

tolower.o: LIBSTELL/Sources/Miscel/tolower.f90
	$(F90) -D$(HOST) -c LIBSTELL/Sources/Miscel/tolower.f90 -o tolower.o

safe_open_mod.o: LIBSTELL/Sources/Modules/safe_open_mod.f90
	$(F90) -c LIBSTELL/Sources/Modules/safe_open_mod.f90 -o safe_open_mod.o

stel_kinds.o: LIBSTELL/Sources/Modules/stel_kinds.f
	$(F77) -c LIBSTELL/Sources/Modules/stel_kinds.f -o stel_kinds.o

stel_constants.o: LIBSTELL/Sources/Modules/stel_constants.f stel_kinds.o
	$(F77) -c LIBSTELL/Sources/Modules/stel_constants.f -o stel_constants.o

vparams.o: LIBSTELL/Sources/Modules/vparams.f stel_kinds.o stel_constants.o
	$(F77) -c LIBSTELL/Sources/Modules/vparams.f -o vparams.o

vsvd0.o: LIBSTELL/Sources/Modules/vsvd0.f stel_kinds.o
	$(F77) -c LIBSTELL/Sources/Modules/vsvd0.f -o vsvd0.o

vmec_input.o: src/vmec_input.f vparams.o vsvd0.o
	$(F77) -c src/vmec_input.f -o vmec_input.o

nonzerolen.o: src/nonzerolen.f90 stel_kinds.o stel_constants.o
	$(F77) -c src/nonzerolen.f90 -o nonzerolen.o

indata2json.o: src/indata2json.f90 json.o getcarg.o safe_open_mod.o nonzerolen.o tolower.o vmec_input.o
	$(F90) -c src/indata2json.f90 -o indata2json.o

indata2json: indata2json.o
	$(F90) json.o getcarg.o safe_open_mod.o stel_kinds.o stel_constants.o vparams.o vsvd0.o nonzerolen.o tolower.o vmec_input.o indata2json.o -o indata2json
