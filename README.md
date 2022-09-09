# indata2json
Converter for VMEC inputs (INDATA namelist) to JSON

This utility is based on the input file reading routines in VMEC, in particular: 
* [TimeStep/vmec.f](https://github.com/ORNL-Fusion/PARVMEC/blob/master/Sources/TimeStep/vmec.f)
* [TimeStep/runvmec.f](https://github.com/ORNL-Fusion/PARVMEC/blob/master/Sources/TimeStep/runvmec.f)
* [Input_Output/readin.f](https://github.com/ORNL-Fusion/PARVMEC/blob/master/Sources/Input_Output/readin.f)
* [Input_Output/read_indata.f](https://github.com/ORNL-Fusion/PARVMEC/blob/master/Sources/Input_Output/read_indata.f)

Default values for the input parameters are set in `read_indata_namelist()` in `LIBSTELL/.../Modules/vmec_input.f`.
Furthermore, after reading the namelist, a few fixups are done there as well:
* If all entries in `niter_array` stayed at their default values of -1,
  they are all set to the value of `niter` (which defaults to 100).
* `raxis` --> `raxis_cc` and `zaxis` --> `zaxis_cs` for all non-zero entries in `raxis`,`zaxis`

## Clone with submodules
This repository uses two Git submodules (`LIBSTELL` and `json-fortran`)
which need to be properly cloned for the build process to work.

Here is how you **get this repo including the submodules**:

```bash
git clone --recursive https://github.com/jonathanschilling/indata2json.git
```

and here is how you get the submodules if you accidentially cloned the repo without the submodules
using the command in the first line here:

```bash
git clone https://github.com/jonathanschilling/indata2json.git
cd indata2json
git submodule update --init --recursive
```
