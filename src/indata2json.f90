program indata2json
  use json
  use safe_open_mod
  use stel_constants, only: zero, one
  use vmec_input
  implicit none

  INTEGER :: numargs, index_dat, index_end, iunit, istat
  CHARACTER(LEN=120), DIMENSION(1) :: command_arg
  CHARACTER(LEN=120) :: input_file0
  CHARACTER(LEN=120) :: input_file
  character(len=1000) :: line

  ! TimeStep/vmec.f
  CALL getcarg(1, command_arg(1), numargs)

  IF (numargs .lt. 1) THEN
    STOP 'Invalid command line'
  end if

  input_file0 = command_arg(1)

  ! TimeStep/runvmec.f
  index_dat = INDEX(input_file0, 'input.')
  index_end = LEN_TRIM(input_file0)
  IF (index_dat .gt. 0) THEN
     ! found 'input.' in command line argument
     ! --> likely, this is a whole filename
     input_extension  = input_file0(index_dat+6:index_end)
     input_file = TRIM(input_file0)
  ELSE
     ! 'input.' not found in command line argument
     ! --> likely this is only the extension
     !     (== part of input filename after 'input.')
     input_extension = input_file0(1:index_end)
     input_file = 'input.'//TRIM(input_extension)
  END IF

  ! Input_Output/read_indata.f
  iunit = 10 ! unit 10 should be available; why not?
  CALL safe_open (iunit, istat, input_file, 'old', 'formatted')
  IF (istat .ne. 0) THEN
     WRITE (6, '(3a,i4)') ' In VMEC, error opening input file: ', &
       TRIM(input_file), '. Iostat = ', istat
     stop
  ENDIF

  istat = -1
  REWIND (iunit)
  CALL read_indata_namelist (iunit, istat)
  IF (istat .ne. 0) THEN
     WRITE (6, '(a,i4)') &
       ' In VMEC, indata NAMELIST error: iostat = ', istat

     ! In case of an error, "backspace" the file (== move one line up)
     ! to get the line on which the parsing error occured:
     backspace(iunit)
     read(iunit,fmt='(A)') line
     write(*,'(A)') 'Invalid line in INDATA namelist: '//trim(line)

     stop
  ENDIF

  ! additional INDATA fixups from read_indata.f
  IF (lfreeb .and. mgrid_file.eq.'NONE') lfreeb = .false.
  IF (bloat .eq. zero) bloat = one

  IF ((bloat.ne.one) .and. (ncurr.ne.1)) THEN
    stop "VMEC INDATA ERROR: NCURR.ne.1 but BLOAT.ne.1."
  ENDIF

  mpol = ABS(mpol)
  ntor = ABS(ntor)

  close(iunit)

  print *, "Successfully parsed VMEC INDATA from '", &
    trim(input_file), "'"

  !---------------------------------------------------!
  ! Here, reading of the VMEC INDATA namelist is done !
  ! and we can start outputting the contents          !
  ! to a fresh and shiny JSON file.                   !
  !---------------------------------------------------!

  call open_dbg_out(trim(input_extension)//".json")

  ! numerical resolution, symmetry assumption
  call add_logical("lasym", lasym)
  call add_int("nfp", nfp)
  call add_int("mpol", mpol)
  call add_int("ntor", ntor)
  call add_int("ntheta", ntheta)
  call add_int("nzeta", nzeta)

  ! multi-grid steps
  ! ns_array
  ! ftol_array
  ! niter_array

  ! solution method tweaking parameters
  call add_real("delt", delt)
  call add_real("tcon0", tcon0)
  ! aphi

  ! total enclosed toroidal magnetic flux
  call add_real("phiedge", phiedge)

  ! printout interval
  call add_int("nstep", nstep)

  ! mass / pressure profile
  call add_element("pmass_type", '"'//trim(pmass_type)//'"')
  ! am
  ! am_aux_s
  ! am_aux_f
  call add_real("pres_scale", pres_scale)
  call add_real("gamma", gamma)
  call add_real("spres_ped", spres_ped)

  ! select constraint on iota or enclosed toroidal current profiles
  call add_int("ncurr", ncurr)

  ! (initial guess for) iota profile
  call add_element("piota_type", '"'//trim(piota_type)//'"')
  ! ai
  ! ai_aux_s
  ! ai_aux_f

  ! enclosed toroidal current profile
  call add_element("pcurr_type", '"'//trim(pcurr_type)//'"')
  ! ac
  ! ac_aux_s
  ! ac_aux_f
  call add_real("curtor", curtor)
  call add_real("bloat", bloat)

  ! initial guess for magnetic axis
  ! raxis_c
  ! zaxis_s
  ! if (lasym) then
  !   raxis_s
  !   zaxis_c
  ! end if

  ! (initial guess for) boundary shape
  ! rbc
  ! zbs
  ! if (lasym) then
  !   rbs
  !   zbc
  ! end if

  ! free-boundary parameters
  call add_logical("lfreeb", lfreeb)
  call add_element("mgrid_file", '"'//trim(mgrid_file)//'"')
  ! extcur
  call add_int("nvacskip", nvacskip)
  ! vac_1_2 is only available in NEMEC

  ! flags for internal hacks
  call add_logical("lforbal", lforbal)

  call close_dbg_out()

  print *, "INDATA contents written to '", &
    trim(input_extension)//".json", "'"

end ! program indata2json
