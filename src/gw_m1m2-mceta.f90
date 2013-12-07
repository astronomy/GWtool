!> \file gw_m1m2-mceta.f90  Compute Mc and eta from M1 and M2


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
program m1m2_mceta
  use SUFR_kinds, only: double
  implicit none
  real(double) :: eta,mc,m1,m2,m
  character :: bla*(99)
  
  if(command_argument_count().eq.2) then
     call get_command_argument(1,bla)
     read(bla,*)m1
     call get_command_argument(2,bla)
     read(bla,*)m2
  else
     write(6,'(/,A,/)')'  Syntax: m1_m2-mc_eta <m1> <m2>'
     stop
  end if
  
  m = m1+m2
  eta = m1*m2/(m*m)
  mc = m*eta**0.6d0
  
  write(6,*)''
  write(6,'(A,2F15.8)') '  M1, M2:          ',m1,m2
  write(6,'(A,2F15.8)') '  Mc, eta:         ',mc,eta
  write(6,'(A,3F15.8)') '  M1/M2, M2/M1, M: ',m1/m2,m2/m1,m
  write(6,*)''
  
end program m1m2_mceta
!***********************************************************************************************************************************

