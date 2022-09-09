program indata2json
  use json
  use safe_open_mod
  use vmec_input
  implicit none

  INTEGER :: numargs, index_dat, index_end, iunit, istat
  CHARACTER(LEN=120), DIMENSION(1) :: command_arg
  CHARACTER(LEN=120) :: input_file0
  CHARACTER(LEN=120) :: input_file

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
     stop
  ENDIF

!----------------------------------------------------------------------!

  call open_dbg_out(trim(input_extension)//".json")

  call close_dbg_out()

end ! program indata2json
