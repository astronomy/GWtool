!> \file gw_coalescence_pmax.f90  Calculates the maximum initial period that can coalesce within a given time

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
!> \brief  Calculates the maximum initial period that can coalesce within a given time

program gw_coalescence_pmax    
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_constants, only: set_SUFR_constants, c3rd,pi2, pc_g,pc_c, msun, solday,julyear
  
  implicit none
  real(double) :: m1,m2,per,time,cst
  character :: tmpstr*(99)
  
  if(command_argument_count().eq.3) then
     call get_command_argument(1, tmpstr)
     read(tmpstr,*) M1
     call get_command_argument(2, tmpstr)
     read(tmpstr,*) M2
     call get_command_argument(3, tmpstr)
     read(tmpstr,*) time
  else
     call syntax_quit('<M1> <M2> <time>  (Mo and Gyr)', 0, &
          'This program calculates the maximum initial period that can coalesce due to GWs within a given time')
  end if
  
  call set_SUFR_constants()
  
  cst = 5.d0/256.d0 * pi2**(-8*c3rd) * pc_c**5/pc_g**(5*c3rd) * (m1+m2)**(c3rd) / (m1*m2*msun**(5*c3rd))
  
  per = (time*1.d9*julyear/cst)**0.375d0/solday
  
  write(6,'(/,A,1p,G13.5,A5,/)') '  Maximum initial period for convergence:',per,'days'
  
end program gw_coalescence_pmax
!***********************************************************************************************************************************
