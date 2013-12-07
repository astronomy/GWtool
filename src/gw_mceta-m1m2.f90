!> \file  gw_mceta-m1m2.f90  Convert the Chirp mass and symmetric mass ratio to M1 and M2


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
program gw_mceta_m1m2
  use SUFR_kinds, only: double
  use SUFR_system, only: quit_program_error, syntax_quit
  
  implicit none
  real(double) :: eta,eta1,mc,m1,m2,mtot
  character :: tmpstr*(99)
  
  if(command_argument_count().eq.2) then
     call get_command_argument(1,tmpstr)
     read(tmpstr,*) mc
     call get_command_argument(2,tmpstr)
     read(tmpstr,*) eta
  else
     call syntax_quit('<Mc> <eta>', 0, 'This program converts masses expressed in Mchirp and eta to M1 and M2')
  end if
  
  if(eta.lt.0.d0.or.eta.gt.0.5d0) call quit_program_error('eta should be 0.0 <= eta <= 0.5', 0)
  
  if(eta.le.0.25d0) then
     write(6,'(/,A,2F15.8)')'  Mc, eta:  ',mc,eta
  else
     eta1 = 0.5d0 - eta
     write(*,'(/,A,2F15.8,A,F8.5,A1)') '  Warning: eta > 0.25; Mapping it to 0.5-eta:'
     write(*,'(A,2F15.8,A,F8.5,A1)')   '  Mc, eta:  ',mc,eta,'  (->',eta1,')'
     eta = eta1
  end if
  
  mtot = mc/eta**(3.d0/5.d0)
  m1 = mtot/2.d0 * (1.d0 + sqrt(1.d0 - 4*eta))
  m2 = mtot/2.d0 * (1.d0 - sqrt(1.d0 - 4*eta))
  
  write(*,*)
  write(*,'(2x,A,2F15.8)') 'M1, M2:   ',m1,m2
  write(*,'(2x,A,2F15.8)') 'M1/M2, M: ',m1/m2,m1+m2
  write(*,*)
  
end program gw_mceta_m1m2
!***********************************************************************************************************************************

