program indata2json
  use json
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 300)


  call open_dbg_out("test.json")

  print *, "add  int arrays..."

  call add_int("a", 42)
  call add_int_1d("b", 3,       (/ 1, 2, 3 /) )
  call add_int_2d("c", 3, 2, (/ (/ 1, 2, 3 /), &
                                (/ 4, 5, 6 /) /) )

  call add_int_3d("d", 3, 2, 2, (/ &
                                (/ (/  1,  2,  3 /), &
                                   (/  4,  5,  6 /) /), &
                                (/ (/  7,  8,  9 /), &
                                   (/ 10, 11, 12 /) /) &
                                /) )

  call add_int_4d("e", 3, 2, 2, 1, (/ (/ &
                                (/ (/  1,  2,  3 /), &
                                   (/  4,  5,  6 /) /), &
                                (/ (/  7,  8,  9 /), &
                                   (/ 10, 11, 12 /) /) &
                                /) /) )

  call add_int_5d("f", 3, 2, 2, 1, 1, (/ (/ (/ &
                                (/ (/  1,  2,  3 /), &
                                   (/  4,  5,  6 /) /), &
                                (/ (/  7,  8,  9 /), &
                                   (/ 10, 11, 12 /) /) &
                                /) /) /) )

  print *, "add real arrays..."

  call add_real("A", 42.0_dp)
  call add_real_1d("B", 3,       (/ 1.0_dp, 2.0_dp, 3.0_dp /) )
  call add_real_2d("C", 3, 2, (/ (/ 1.0_dp, 2.0_dp, 3.0_dp /), &
                                 (/ 4.0_dp, 5.0_dp, 6.0_dp /) /) )

  call add_real_3d("D", 3, 2, 2, (/ &
                                 (/ (/  1.0_dp,  2.0_dp,  3.0_dp /), &
                                    (/  4.0_dp,  5.0_dp,  6.0_dp /) /), &
                                 (/ (/  7.0_dp,  8.0_dp,  9.0_dp /), &
                                    (/ 10.0_dp, 11.0_dp, 12.0_dp /) /) &
                                 /) )

  call add_real_4d("E", 3, 2, 2, 1, (/ (/ &
                                    (/ (/  1.0_dp,  2.0_dp,  3.0_dp /), &
                                       (/  4.0_dp,  5.0_dp,  6.0_dp /) /), &
                                    (/ (/  7.0_dp,  8.0_dp,  9.0_dp /), &
                                       (/ 10.0_dp, 11.0_dp, 12.0_dp /) /) &
                                    /) /) )

  call add_real_5d("F", 3, 2, 2, 1, 1, (/ (/ (/ &
                                       (/ (/  1.0_dp,  2.0_dp,  3.0_dp /), &
                                          (/  4.0_dp,  5.0_dp,  6.0_dp /) /), &
                                       (/ (/  7.0_dp,  8.0_dp,  9.0_dp /), &
                                          (/ 10.0_dp, 11.0_dp, 12.0_dp /) /) &
                                       /) /) /) )

  print *, "done"

  call close_dbg_out()

end ! program indata2json
