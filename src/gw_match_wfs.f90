!> \file gw_match_wfs.f90  Match the two waveforms in specified files

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
!> \brief  Match the two waveforms in specified files
!!
!! \note  The files are assumed to contain time-domain waveforms, in two columns: time and strain

program gw_match_wfs
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_numerics, only: deq,dne
  
  implicit none
  integer :: ln,lnr
  real(double) :: dt,dh,rdh, hmax,hsum,hsum1,hsum2
  character :: infile1*(99),infile2*(99)
  
  call set_SUFR_constants()
  
  if(command_argument_count().eq.2) then
     call get_command_argument(1, infile1)
     call get_command_argument(2, infile2)
  else
     call syntax_quit('<WFfile1> <WFfile2>', 0)
  end if
  
  call match_wfs_file(trim(infile1),trim(infile2), ln, dt,dh,hmax,hsum1,hsum2,rdh,lnr)
  
  hsum = hsum1 + hsum2
  
  write(*,*)
  !write(*,'(A,I9)') '  Nl: ',ln
  !write(*,'(5(A,ES12.5))') '  Tot dh: ',dh, ',  avg.dh: ',dh/dble(ln), ',  avg.rel.dh: ',rdh/dble(lnr)
  !write(*,'(5(A,ES12.5))') '  Max h:  ',hmax, ',  avg.dh/hmax: ', dh/dble(ln) / hmax
  write(*,'(5(A,ES12.5))') '  Avg h:  ',hsum/dble(2*ln), ',  avg.dh/avg.h: ', dh/hsum
  !write(*,'(5(A,ES12.5))') '  Avg h1: ',hsum1/dble(ln), ',  avg.dh/avg.h1: ', dh/(2*hsum1)
  !write(*,'(2(A,ES12.5),A,F10.5)') '  Tot dt: ',dt, ',  avg.dt: ',dt/dble(ln), ',  avg.dt*1e6: ',dt/dble(ln)*1.d6
  write(*,'(2(A,ES12.5),A,F10.5)') '  Avg.dt: ',dt/dble(ln)
  if(deq(dt,0.d0) .and. deq(dh,0.d0)) write(*,'(/,A)') '  The two waveforms are identical'
  if(dne(dt,0.d0)) write(*,'(/,A)') '  There is a time offset'
  write(*,*)
  
end program gw_match_wfs
!***********************************************************************************************************************************



