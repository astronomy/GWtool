!> \file isco.f90  Calculates isco properties for a black hole with mass m and spin a
!!
!! \see Bardeen, Press, Teukolsky, 1972ApJ...178..347B
!!
!! AF, 1/5/2008


!***********************************************************************************************************************************
program isco
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants, c3rd,pi, pc_g,pc_c, msun
  
  implicit none
  real(double) :: a, risco,m,z1,z2,fiscogw,fiscoorb
  integer :: narg,retro
  character :: str*(99)
  
  call set_SUFR_constants()
  
  a = 0.d0
  narg = command_argument_count()
  if(narg.ge.1) then
     call get_command_argument(1,str)
     read(str,*)m
     if(narg.ge.2) then
        call get_command_argument(2,str)
        read(str,*)a
     end if
  else
     write(*,'(/,A)')'  This program calculates ISCO properties for a black hole with mass m and spin a.'
     write(*,'(A)')'    syntax: isco <m> [<a>]'
     write(*,'(A,/)')'  Input parameters are the BH mass (Mo) and the dimensionless spin -1<a<1; a<0: retrograde.'
     stop
  end if
  
  retro = 0
  if(a.lt.0.d0) retro = 1
  a = abs(a)
  
  z1 = 1 + (1 - a*a)**c3rd * ( (1+a)**c3rd + (1-a)**c3rd )
  z2 = sqrt(3*a*a + z1*z1)
  if(retro.eq.0) then
     risco = m * (3+z2 - sqrt((3-z1)*(3+z1+2*z2)) )
  else
     risco = m * (3+z2 + sqrt((3-z1)*(3+z1+2*z2)) )
  end if
  fiscoorb = pc_c**3/(2*pi*(risco/m)**1.5d0*pc_g*m*msun)
  fiscogw =  2*fiscoorb
  
  !write(*,'(10ES13.3)')m,a,z1,z2,risco
  write(*,*)
  write(*,'(A,F12.3)')'  Mass:     ',m
  write(*,'(A,F12.3)')'  Spin:     ',a
  write(*,*)
  write(*,'(A,F12.3)')'  Risco/M:  ',risco/m
  write(*,'(A,F12.4)')'  fisco,orb:',fiscoorb
  write(*,'(A,F12.4)')'  fisco,gw: ',fiscogw
  write(*,*)
  
  
end program isco
!***********************************************************************************************************************************

