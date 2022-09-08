
F90:=gfortran

.PHONY: all clean objdir

all: indata2json
clean:
	rm -rf obj
	rm -f json.mod
	rm -f indata2json

objdir:
	mkdir -p obj/

json.o: objdir json-fortran/json.f90
	$(F90) -c json-fortran/json.f90 -o obj/json.o

indata2json.o: obj/json.o
	$(F90) -c src/indata2json.f90 -o obj/indata2json.o

indata2json: json.o indata2json.o 
	$(F90) obj/json.o obj/indata2json.o -o indata2json

