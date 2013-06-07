!> \file eta2q.f90  Compute the symmetric mass ratio eta from the asymmetric mass ratio q

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
!> \brief  Compute the symmetric mass ratio eta from the asymmetric mass ratio q

program eta2q
  use SUFR_kinds, only: double
  implicit none
  real(double) :: eta, q,x
  character :: tmpstr*(99)
  
  if(command_argument_count().eq.1) then
     call get_command_argument(1,tmpstr)
     read(tmpstr,*) eta
  else
     write(6,'(/,A,/)')'  Syntax: eta2q <eta>'
     stop
  end if
  
  x = sqrt( max(min( 1.d0 - 4*eta,1.d0 ),0.d0) )
  q = (1.d0 - x) / (1.d0 + x)
  
  write(6,'(/,A,F15.8,/)') '  q:  ', q
  
end program eta2q
!***********************************************************************************************************************************

