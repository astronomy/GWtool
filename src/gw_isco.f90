!> \file gw_isco.f90  Calculates isco properties for a black hole with mass m and spin a
!!
!! \see Bardeen, Press, Teukolsky, 1972ApJ...178..347B
!!
!! AF, 2008-05-01


!   
!   GWtool:       Simple tools for working with gravitational waves
!                 http://gwtool.sourceforge.net/
!   
!   
!   Copyright 2007-2013 AstroFloyd - astrofloyd.org
!   
!   
!   This file is part of GWtool.
!   
!   GWtool is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!   
!   GWtool is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!   
!   You should have received a copy of the GNU General Public License
!   along with GWtool.  If not, see <http://www.gnu.org/licenses/>.



!***********************************************************************************************************************************
program gw_isco
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_constants, only: set_SUFR_constants, c3rd,pi, pc_g,pc_c, msun
  
  implicit none
  real(double) :: a, risco,m,z1,z2,f_isco_gw,f_isco_orb
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
     call syntax_quit('<Mbh> [<a_spin>]', 0, &
          'This program calculates ISCO properties for a black hole with mass Mbh (Mo) and dimensionless spin -1 < a_spin < 1')
  end if
  
  retro = 0
  if(a.lt.0.d0) retro = 1
  a = abs(a)
  
  z1 = 1 + (1 - a*a)**c3rd * ( (1+a)**c3rd + (1-a)**c3rd )
  z2 = sqrt(3*a*a + z1*z1)
  if(retro.eq.0) then
     risco = m * (3+z2 - sqrt((3-z1)*(3+z1+2*z2)) )  ! Expressed in M
  else
     risco = m * (3+z2 + sqrt((3-z1)*(3+z1+2*z2)) )  ! Expressed in M
  end if
  f_isco_orb = pc_c**3/(2*pi*(risco/m)**1.5d0*pc_g*m*msun)
  f_isco_gw =  2*f_isco_orb
  
  
  write(*,*)
  write(*,'(2x,A,F12.3)') 'Mass:      ', m
  write(*,'(2x,A,F12.3)') 'Spin:      ', a
  write(*,*)
  write(*,'(2x,A,F12.3)') 'Risco/M:   ', risco/m
  write(*,'(2x,A,F12.4)') 'f_isco,orb:', f_isco_orb
  write(*,'(2x,A,F12.4)') 'f_isco,gw: ', f_isco_gw
  write(*,*)
  
end program gw_isco
!***********************************************************************************************************************************

