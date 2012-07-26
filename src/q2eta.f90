!> \file q2eta.f90  Compute the symmetric mass ratio eta from the asymmetric mass ratio q

!   
!   GWtool:       Simple tools for working with gravitational waves
!                 http://gwtool.sourceforge.net/
!   
!   
!   Copyright 2007-2011 AstroFloyd - astrofloyd.org
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
!> \brief Compute the symmetric mass ratio eta from the asymmetric mass ratio q

program q2eta
  use SUFR_kinds, only: double
  implicit none
  real(double) :: q, eta
  character :: tmpstr*(99)
  
  if(command_argument_count().eq.1) then
     call get_command_argument(1,tmpstr)
     read(tmpstr,*) q
  else
     write(6,'(/,A,/)')'  Syntax: q2eta <q>'
     stop
  end if
  
  eta = q/(1.d0+q)**2
  
  write(6,'(/,A,F15.8,/)') '  Eta:  ', eta
  
end program q2eta
!***********************************************************************************************************************************

