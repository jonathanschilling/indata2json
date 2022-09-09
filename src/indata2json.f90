program indata2json
  use json
  use safe_open_mod
  use stel_constants, only: zero, one
  use vmec_input
  use vparams, only: nsd, cbig
  use nzl
  implicit none

  INTEGER :: numargs, index_dat, index_end, iunit, istat
  CHARACTER(LEN=120), DIMENSION(1) :: command_arg
  CHARACTER(LEN=120) :: input_file0
  CHARACTER(LEN=120) :: input_file
  character(len=1000) :: line

  integer :: igrid, multi_ns_grid, nsmin, i, n, m, nextcur
  integer :: numCoeff, iRBC, iZBS, iRBS, iZBC

  ! lowercase copies to make case-insensitive string comparisons
  character(len=20) :: pmass_type_lc
  character(len=20) :: piota_type_lc
  character(len=20) :: pcurr_type_lc

  character(len=1024), dimension(:), allocatable :: rbc_entries
  character(len=1024), dimension(:), allocatable :: zbs_entries
  character(len=1024), dimension(:), allocatable :: rbs_entries
  character(len=1024), dimension(:), allocatable :: zbc_entries

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

  ! HACK: set extcur to cbig to find out later which entries were set
  ! In VMEC, the mgrid file is read and this defines
  ! the number of external coil currents to be expected in extcur.
  extcur = cbig

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

  close(iunit)

  ! additional INDATA fixups from read_indata.f
  IF (lfreeb .and. mgrid_file.eq.'NONE') lfreeb = .false.
  IF (bloat .eq. zero) bloat = one

  IF ((bloat.ne.one) .and. (ncurr.ne.1)) THEN
    stop "VMEC INDATA ERROR: NCURR.ne.1 but BLOAT.ne.1."
  ENDIF

  mpol = ABS(mpol)
  ntor = ABS(ntor)

  !
  !     PARSE NS_ARRAY
  !
  nsin = MAX (3, nsin)
  multi_ns_grid = 1
  IF (ns_array(1) .eq. 0) THEN                    !Old input style
      ns_array(1) = MIN(nsin,nsd)
      multi_ns_grid = 2
      ns_array(multi_ns_grid) = ns_default        !Run on 31-point mesh
  ELSE
      nsmin = 1
      DO WHILE (ns_array(multi_ns_grid) .ge. nsmin .and. &
                multi_ns_grid .lt. 100)      ! .ge. previously .gt.
         nsmin = MAX(nsmin, ns_array(multi_ns_grid))
         IF (nsmin .le. nsd) THEN
            multi_ns_grid = multi_ns_grid + 1
         ELSE                                      !Optimizer, Boozer code overflows otherwise
            ns_array(multi_ns_grid) = nsd
            nsmin = nsd
            PRINT *,' NS_ARRAY ELEMENTS CANNOT EXCEED ',nsd
            PRINT *,' CHANGING NS_ARRAY(',multi_ns_grid,') to ', nsd
         END IF
      END DO
      multi_ns_grid = multi_ns_grid - 1
  ENDIF
  IF (ftol_array(1) .eq. zero) THEN
     ftol_array(1) = 1.e-8_dp
     IF (multi_ns_grid .eq. 1) ftol_array(1) = ftol
     DO igrid = 2, multi_ns_grid
        ftol_array(igrid) = 1.e-8_dp * (1.e8_dp * ftol)** &
                            ( REAL(igrid-1,dp)/(multi_ns_grid-1) )
     END DO
  ENDIF

  IF (nvacskip .LE. 0) nvacskip = nfp

  ! HACK
  ! Figure out how many entries in extcur were specified
  ! in the INDATA namelist and are now not cbig anymore.
  nextcur = 0
  do i = 1, nigroup
    if (extcur(i) .ne. cbig) then
!      print *, "extcur(",i,")=",extcur(i)
      nextcur = nextcur + 1
    end if
  end do

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
  call add_int_1d("ns_array", multi_ns_grid, ns_array)
  call add_real_1d("ftol_array", multi_ns_grid, ftol_array)
  call add_int_1d("niter_array", multi_ns_grid, niter_array)

  ! solution method tweaking parameters
  call add_real("delt", delt)
  call add_real("tcon0", tcon0)
  n = NonZeroLen(aphi,SIZE(aphi))
  call add_real_1d("aphi", n, aphi)

  ! total enclosed toroidal magnetic flux
  call add_real("phiedge", phiedge)

  ! printout interval
  call add_int("nstep", nstep)

  ! mass / pressure profile
  pmass_type_lc = pmass_type
  CALL tolower(pmass_type_lc)
  call add_element("pmass_type", '"'//trim(pmass_type_lc)//'"')
  SELECT CASE(TRIM(pmass_type_lc))
    CASE ('akima_spline','cubic_spline')
      n = NonZeroLen(am_aux_s,SIZE(am_aux_s))
      call add_real_1d("am_aux_s", n, am_aux_s(1:n))
      n = NonZeroLen(am_aux_f,SIZE(am_aux_f))
      call add_real_1d("am_aux_f", n, am_aux_f(1:n))
    CASE DEFAULT
      n = NonZeroLen(am,SIZE(am))
      call add_real_1d("am", n, am(0:n-1))
  END SELECT

  call add_real("pres_scale", pres_scale)
  call add_real("gamma", gamma)
  call add_real("spres_ped", spres_ped)

  ! select constraint on iota or enclosed toroidal current profiles
  call add_int("ncurr", ncurr)

  if (ncurr .eq. 0) then
    ! (initial guess for) iota profile
    piota_type_lc = piota_type
    CALL tolower(piota_type_lc)
    call add_element("piota_type", '"'//trim(piota_type_lc)//'"')
    SELECT CASE(TRIM(piota_type_lc))
      CASE ('akima_spline','cubic_spline')
        n = NonZeroLen(ai_aux_s,SIZE(ai_aux_s))
        call add_real_1d("ai_aux_s", n, ai_aux_s(1:n))
        n = NonZeroLen(ai_aux_f,SIZE(ai_aux_f))
        call add_real_1d("ai_aux_f", n, ai_aux_f(1:n))
      CASE DEFAULT
        n = NonZeroLen(ai,SIZE(ai))
        call add_real_1d("ai", n, ai(0:n-1))
    END SELECT

  else ! ncurr .eq. 0

    ! enclosed toroidal current profile
    pcurr_type_lc = pcurr_type
    CALL tolower(pcurr_type_lc)
    call add_element("pcurr_type", '"'//trim(pcurr_type_lc)//'"')
    SELECT CASE(TRIM(pcurr_type_lc))
      CASE ('akima_spline_ip','akima_spline_i', &
            'cubic_spline_ip','cubic_spline_i')
        n = NonZeroLen(ac_aux_s,SIZE(ac_aux_s))
        call add_real_1d("ac_aux_s", n, ac_aux_s(1:n))
        n = NonZeroLen(ac_aux_f,SIZE(ac_aux_f))
        call add_real_1d("ac_aux_f", n, ac_aux_f(1:n))
      CASE DEFAULT
        n = NonZeroLen(ac,SIZE(ac))
        call add_real_1d("ac", n, ac(0:n-1))
    END SELECT

    call add_real("curtor", curtor)
    call add_real("bloat", bloat)

  end if ! ncurr .eq. 0

  ! initial guess for magnetic axis
  call add_real_1d("raxis_c", ntor+1, raxis_cc(0:ntor))
  call add_real_1d("zaxis_s", ntor,   zaxis_cs(1:ntor))
  if (lasym) then
    call add_real_1d("raxis_s", ntor,   raxis_cs(1:ntor))
    call add_real_1d("zaxis_c", ntor+1, zaxis_cc(0:ntor))
  end if ! lasym

  ! (initial guess for) boundary shape
  numCoeff = mpol * (2 * ntor + 1)
  allocate(rbc_entries(numCoeff), zbs_entries(numCoeff))
  if (lasym) then
    allocate(rbs_entries(numCoeff), zbc_entries(numCoeff))
  end if ! lasym

  ! find non-zero entries and
  iRBC = 0
  iZBS = 0
  iRBS = 0
  iZBC = 0
  do m = 0, mpol-1
    do n = -ntor, ntor
      if (abs(rbc(n,m)) .ne. zero) then
        iRBC = iRBC + 1
        write(rbc_entries(iRBC), 1000) n, m, rbc(n,m)
      end if
      if (abs(zbs(n,m)) .ne. zero) then
        iZBS = iZBS + 1
        write(zbs_entries(iZBS), 1000) n, m, zbs(n,m)
      end if
      if (lasym) then
        if (abs(rbs(n,m)) .ne. zero) then
          iRBS = iRBS + 1
          write(rbs_entries(iRBS), 1000) n, m, rbs(n,m)
        end if
        if (abs(zbc(n,m)) .ne. zero) then
          iZBC = iZBC + 1
          write(zbc_entries(iZBC), 1000) n, m, zbc(n,m)
        end if
      end if ! lasym
    end do ! n = -ntor, ntor
  end do ! m = 0, mpol-1
1000 format('{"n":',i4,',"m":',i4',"value":',f20.12,'}')

  ! NOTE
  ! The following code interacts with the intended-private members
  ! dbg_unit and has_previous from json-fortran.

  ! write all non-zero RBC entries
  if (iRBC .gt. 0) then
    if (has_previous) then
      write(dbg_unit, '(A)', advance='no') ','
    end if
    write(dbg_unit, '(A)', advance='no') '"rbc":['
    do i = 1, iRBC
      write(dbg_unit, '(A)', advance='no') trim(rbc_entries(i))
      if (i .lt. iRBC) then
        write(dbg_unit, '(A)', advance='no') ','
      end if
    end do
    write(dbg_unit, '(A)', advance='no') ']'
    has_previous = .true.
  end if

  ! write all non-zero ZBS entries
  if (iZBS .gt. 0) then
    if (has_previous) then
      write(dbg_unit, '(A)', advance='no') ','
    end if
    write(dbg_unit, '(A)', advance='no') '"zbs":['
    do i = 1, iZBS
      write(dbg_unit, '(A)', advance='no') trim(zbs_entries(i))
      if (i .lt. iZBS) then
        write(dbg_unit, '(A)', advance='no') ','
      end if
    end do
    write(dbg_unit, '(A)', advance='no') ']'
    has_previous = .true.
  end if

  if (lasym) then
    ! write all non-zero RBS entries
    if (iRBS .gt. 0) then
      if (has_previous) then
        write(dbg_unit, '(A)', advance='no') ','
      end if
      write(dbg_unit, '(A)', advance='no') '"rbs":['
      do i = 1, iRBS
        write(dbg_unit, '(A)', advance='no') trim(rbs_entries(i))
        if (i .lt. iRBS) then
          write(dbg_unit, '(A)', advance='no') ','
        end if
      end do
      write(dbg_unit, '(A)', advance='no') ']'
      has_previous = .true.
    end if

    ! write all non-zero ZBC entries
    if (iZBC .gt. 0) then
      if (has_previous) then
        write(dbg_unit, '(A)', advance='no') ','
      end if
      write(dbg_unit, '(A)', advance='no') '"zbc":['
      do i = 1, iZBC
        write(dbg_unit, '(A)', advance='no') trim(zbc_entries(i))
        if (i .lt. iZBC) then
          write(dbg_unit, '(A)', advance='no') ','
        end if
      end do
      write(dbg_unit, '(A)', advance='no') ']'
      has_previous = .true.
    end if

  end if ! lasym

  deallocate(rbc_entries, zbs_entries)
  if (lasym) then
    deallocate(rbs_entries, zbc_entries)
  end if ! lasym

  ! free-boundary parameters
  call add_logical("lfreeb", lfreeb)
  call add_element("mgrid_file", '"'//trim(mgrid_file)//'"')
  call add_real_1d("extcur", nextcur, extcur(1:nextcur))
  call add_int("nvacskip", nvacskip)
  ! vac_1_2 is only available in NEMEC

  ! flags for internal hacks
  call add_logical("lforbal", lforbal)

  call close_dbg_out()

  print *, "INDATA contents written to '", &
    trim(input_extension)//".json", "'"

end ! program indata2json
