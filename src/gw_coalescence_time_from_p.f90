!> \file gw_coalescence_time_from_p.f90  Calculates the time needed to reach a certain Porb (default 0) using gravitational-wave evolution

!   
!   GWtool:       Simple tools for working with gravitational waves
!                 http://gwtool.sourceforge.net/
!   
!   
!   Copyright 2007-2024 AstroFloyd - astrofloyd.org
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
!> \brief  Calculates the time needed to reach a certain Porb (default 0) using gravitational-wave evolution.

program gw_coalescence_time_from_p
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants, pi2,msun,julyear,solday,c3rd, pc_g,pc_c
  use SUFR_system, only: syntax_quit
  
  implicit none
  integer :: narg
  real(double) :: trl,M1,M2,Pin,Pf,cst
  character :: tmpstr*(99)
  
  ! Check command-line arguments:
  narg = command_argument_count()
  if(narg.eq.3.or.narg.eq.4) then
     call get_command_argument(1,tmpstr)
     read(tmpstr,*) M1
     call get_command_argument(2,tmpstr)
     read(tmpstr,*) M2
     call get_command_argument(3,tmpstr)
     read(tmpstr,*) Pin
     
     Pf = 0.d0
     if(narg.eq.4) then
        call get_command_argument(4,tmpstr)
        read(tmpstr,*) Pf
     end if
  else
     call syntax_quit('<M1> <M2> <Pi> [<Pf>]   (Mo and days, default Pf=0)',0, &
          'This program calculates the time needed to reach a ceratain Porb with gravitational waves')
  end if
  
  
  call set_SUFR_constants()
  
  cst = 5.d0/256.d0 * pi2**(-8.d0*c3rd) * pc_c**5 / pc_g**(5*c3rd) * (M1+M2)**c3rd/(M1*M2 * msun**(5*c3rd))
  
  Pin = Pin * solday
  Pf  = Pf  * solday
  trl = ( Pin**(8*c3rd) - Pf**(8*c3rd) ) * cst/julyear
  
  write(*,'(A,ES15.7,A,/)') '  Time needed: ',trl,' yr'
  
end program gw_coalescence_time_from_p
!***********************************************************************************************************************************

