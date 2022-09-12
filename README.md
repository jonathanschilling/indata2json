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

## Input File Contents

The following lists the input parameters for VMEC
which are extracted from the `INDATA` namelist and exported to the `json` file.

### numerical resolution, symmetry assumption

  `lasym` flag to indicate non-stellarator-symmetry
  
  `nfp`   number of toroidal field periods (=1 for Tokamak)
  
  `mpol` number of poloidal Fourier harmonics; m = 0, 1, ..., (mpol-1)
  
  `ntor` number of toroidal Fourier harmonics; n = -ntor, -ntor+1, ..., -1, 0, 1, ..., ntor-1, ntor
  
  `ntheta` number of poloidal grid points; is rounded to next smaller even number, if odd
  
  `nzeta` number of toroidal grid points; must match nzeta of mgrid file if using free-boundary
  
### multi-grid steps

  `ns_array` [numGrids] number of flux surfaces per multigrid step
  
  `ftol_array` [numGrids] requested force tolerance for convergence per multigrid step
  
  `niter_array` [numGrids] maximum number of iterations per multigrid step

### solution method tweaking parameters

  `delt` initial value for artificial time step in iterative solver
  
  `tcon0` constraint force scaling factor for ns --> 0
  
  `aphi` radial flux zoning profile coefficients

### total enclosed toroidal magnetic flux

  `phiedge` total enclosed toroidal magnetic flux in Vs == Wb
  
### printout interval

  `nstep` printout interval for convergence information
  
### mass / pressure profile

  `pmass_type`   Specifies parameterization type of pmass function  
                 'power_series' - p(s)=Sum[ am(j) s ** j] - Default  
                 'gauss_trunc'  - p(s)=am(0) (exp(-(s/am(1)) ** 2) -
                                               exp(-(1/am(1)) ** 2)) 
                 others - see function pmass

  `am` mass or pressure (gamma=0) expansion coefficients (series in s)
       in MKS units [NWT/M**2]
       Interpretation changes with pmass_type

  `am_aux_s`    Auxiliary array for mass profile. Used for splines, s values

  `am_aux_f`    Auxiliary array for mass profile. Used for splines, function values
  
  `pres_scale`   factor used to scale pressure profile (default value = 1)
                 useful so user can fix profile and change beta without having to change
                 all AM coefficients separately
                 
  `gamma` value of compressibility index (gamma=0 => pressure prescribed)

  `spres_ped` location of pressure pedestal in s

### select constraint on iota or enclosed toroidal current profiles

  `ncurr` 0: constrained-iota; 1: constrained-current
  
### (initial guess for) iota profile

  `piota_type`   Specifies parameterization type of piota function  
                 'power_series' - p(s)=Sum[ am(j) s ** j] - Default  
                  others - see function piota

  `ai`   expansion coefficients for iota (power series in s) used when ncurr=0  
          Interpretation changes with piota_type

  `ai_aux_s`   Auxiliary array for iota profile. Used for splines, s values

  `ai_aux_f`   Auxiliary array for iota profile. Used for splines, function values
  
### enclosed toroidal current profile

  `pcurr_type`   Specifies parameterization type of pcurr function  
                 'power_series' - I'(s)=Sum[ ac(j) s ** j] - Default  
                 'gauss_trunc'  - I'(s)=ac(0) (exp(-(s/ac(1)) ** 2) -
                                               exp(-(1/ac(1)) ** 2))  
                  others - see function pcurr

  `ac`   expansion coefficients for the normalized (pcurr(s=1) = 1)  
         radial derivative of the flux-averaged toroidal current density
         (power series in s) used when ncurr=1  
         Interpretation changes with pcurr_type

  `ac_aux_s`   Auxiliary array for current profile. Used for splines, s values

  `ac_aux_f`   Auxiliary array for current profile. Used for splines, function values

  `curtor`   value of toroidal current in A.
              Used if ncurr = 1 to specify current profile.
  
  `bloat` bloating factor (for constrained toroidal current)
  
### initial guess for magnetic axis

  `raxis_cc` magnetic axis coefficients for R ~ cos(n*v); stellarator-symmetric
  
  `zaxis_cs` magnetic axis coefficients for Z ~ sin(n*v); stellarator-symmetric
  
  `raxis_cs` magnetic axis coefficients for R ~ sin(n*v); non-stellarator-symmetric
  
  `zaxis_cc` magnetic axis coefficients for Z ~ cos(n*v); non-stellarator-symmetric
              
### (initial guess for) boundary shape
  
  `rbc`   boundary coefficients of COS(m*theta-n*zeta) for R [m]; stellarator-symmetric

  `zbs`   boundary coefficients of SIN(m*theta-n*zeta) for Z [m]; stellarator-symmetric

  `rbs`   boundary coefficients of SIN(m*theta-n*zeta) for R [m]; non-stellarator-symmetric

  `zbc`   boundary coefficients of COS(m*theta-n*zeta) for Z [m]; non-stellarator-symmetric

### free-boundary parameters

  `lfreeb`   =T, run in free boundary mode if mgrid_file exists

  `mgrid_file`   full path for vacuum Green's function data

  `extcur`   array of currents in each external current group. Used to
             multiply Green''s function for fields and loops read in from
             MGRID file. Should use real current units (A).
              
  `nvacskip`   number of iteration steps between accurate calculation of vacuum
               response; use fast interpolation scheme in between

### flags for internal hacks

  `lforbal`   =T, use non-variational forces to ensure <EQUIF> = 0;
              =F, use variational form of forces, <EQUIF> ~ 0
