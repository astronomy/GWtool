!> \file m1_m2-mc_eta.f90  Compute Mc and eta from M1,M2

!***********************************************************************************************************************************
program m1m2_mceta
  implicit none
  integer :: iargc
  real*8 :: eta,mc,m1,m2,m
  character :: bla*(99)
  
  if(iargc().eq.2) then
     call getarg(1,bla)
     read(bla,*)m1
     call getarg(2,bla)
     read(bla,*)m2
  else
     write(6,'(/,A,/)')'  Syntax: m1_m2-mc_eta <m1> <m2>'
     stop
  end if
  
  m = m1+m2
  eta = m1*m2/(m*m)
  mc = m*eta**0.6d0
  
  write(6,*)''
  write(6,'(A,2F15.8)')'  M1, M2:   ',m1,m2
  write(6,'(A,2F15.8)')'  Mc, eta:  ',mc,eta
  write(6,'(A,2F15.8)')'  M1/M2, M: ',m1/m2,m
  write(6,*)''
  
end program m1m2_mceta
!***********************************************************************************************************************************

