!> \file wf_routines.f90  Routines for waveform matching

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
!> \brief  Match the time-domain waveforms in two specified files
!!
!! \param ipfile1  File containing WF 1
!! \param ipfile2  File containing WF 2
!!
!! \retval ln     Number of lines read
!! \retval dt     Sum(t2-t1)
!! \retval dh     Sum(h2-h1)
!! \retval hmax   Maximum absolute strain amplitude found in h1, h2
!! \retval hsum1  Sum of absolute values of strain amplitude h1
!! \retval hsum2  Sum of absolute values of strain amplitude h1
!! \retval rdh    Relative dh
!! \retval lnr    Number of lines used to compute rdh

subroutine match_wfs_file(ipfile1,ipfile2, ln, dt,dh, hmax,hsum1,hsum2, rdh,lnr)
  use SUFR_kinds, only: double
  use SUFR_system, only: find_free_io_unit, quit_program_error
  
  implicit none
  character, intent(in) :: ipfile1*(*), ipfile2*(*)
  integer, intent(out) :: ln, lnr
  real(double), intent(out) :: dt,dh, hmax,hsum1,hsum2, rdh
  
  integer :: ip1,ip2, status
  real(double) :: t1,t2, h1,h2
  
  call find_free_io_unit(ip1)
  open(unit=ip1,form='formatted',status='old',action='read',position='rewind',file=trim(ipfile1),iostat=status)
  if(status.ne.0) call quit_program_error('Error opening '//trim(ipfile1)//', aborting...', 0)
  
  call find_free_io_unit(ip2)
  open(unit=ip2,form='formatted',status='old',action='read',position='rewind',file=trim(ipfile2),iostat=status)
  if(status.ne.0) call quit_program_error('Error opening '//trim(ipfile2)//', aborting...', 0)
  
  
  ln = 0
  lnr = 0
  dh = 0.d0
  rdh = 0.d0
  dt = 0.d0
  hmax = -huge(hmax)
  hsum1 = 0.d0
  hsum2 = 0.d0
  do
     ln = ln + 1
     
     read(ip1,*, iostat=status) t1, h1
     if(status.lt.0) exit
     if(status.gt.0) then
        write(0,'(A,I4,A,/)') '  Error reading '//trim(ipfile1)//', line',ln,' aborting...'
        stop
     end if
     
     read(ip2,*, iostat=status) t2, h2
     if(status.lt.0) exit
     if(status.gt.0) then
        write(0,'(A,I4,A,/)') '  Error reading '//trim(ipfile2)//', line',ln,' aborting...'
        stop
     end if
     
     dt = dt + abs(t2-t1)
     dh = dh + abs(h2-h1)
     hmax = maxval( (/hmax, abs(h1), abs(h2)/) )
     hsum1 = hsum1 + abs(h1)
     hsum2 = hsum2 + abs(h2)
     if(min(abs(h1),abs(h2)).gt.1.d-23) then
        rdh = rdh + abs((h2-h1)/h1)
        lnr = lnr + 1
     end if
     
     !if(min(abs(h1),abs(h2)).gt.1.d-23 .and. mod(ln,100).eq.0) &
     !if(mod(ln,1000).eq.0) &
     !     write(*,'(I9,2F12.6,3ES15.5,F10.4)') ln,t1-1.d9,t2-1.d9, h1,h2,abs(h2-h1),abs((h2-h1)/h1)
     
  end do
  
  ln = ln - 1
  close(ip1)
  close(ip2)
  
end subroutine match_wfs_file
!***********************************************************************************************************************************
