!> \file gw_binary_contact.f90  Calculates time of coalescence for a binary with given masses and period
!!
!! Adapted from tcoal.f90


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
program binary_contact_gw
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants, c3rd,pi, solday,julyear, pc_g,pc_c, msun
  
  implicit none  
  real(double) :: m1,m2,per, mc,mu,eta,t,f0,m
  real(double) :: m0, kepler_p2a, aorb
  integer :: narg
  character :: str*(99)
  
  call set_SUFR_constants()
  m0   = msun*pc_g/pc_c**3  ! Solar mass in seconds (4.926d-6)
  
  narg = command_argument_count()
  if(narg.eq.3) then
     call get_command_argument(1,str)
     read(str,*) m1
     call get_command_argument(2,str)
     read(str,*) m2
     call get_command_argument(3,str)
     read(str,*) per
  else
     write(6,'(/,A)')'  This program calculates the coalescence time for a binary with masses M1 and M2 and period P.'
     write(6,'(A,/)')'  syntax: tcoal <M1 (Mo)> <M2 (Mo)> <Porb (d)>'
     stop
  end if
  
  m = m1+m2
  mu = m1*m2/m
  eta = mu/m
  mc = m*eta**0.6d0
  f0 = 2.d0/(per*solday)
  
  aorb = kepler_p2a(m,per)
  
  t = 5*(8.d0*pi*f0)**(-8.d0*c3rd) * (mc*m0)**(-5.d0*c3rd) / (julyear*1.d9)  ! Newtonian
  
  
  
  write(6,*)''
  write(6,'(A,4F15.4)')  '  M1,M2,P,a:  ',m1,m2,per,aorb
  !write(6,'(A,3F15.4)')  '  Mu,eta,Mc:  ',mu,eta,mc
  write(6,'(A,F15.4,A)') '  Tcoal:      ',t,' Gyr'
  write(6,*)''
  
end program binary_contact_gw
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Compute the orbital period from the total mass and orbital separation of a binary using Kepler's equation
!!
!! \param mt   Total binary mass, in Mo
!! \param per  Orbital period in days
!!
!! \retval kepler_p2a  Orbital separation in Ro

function kepler_p2a(mt, per)
  use SUFR_kinds, only: double
  use SUFR_constants, only: rpi, solday, pc_g, rsun,msun
  
  implicit none
  real(double), intent(in) :: mt, per
  real(double) :: kepler_p2a
  
  kepler_p2a = (per*solday / sqrt(4*rpi**2/(pc_g*mt*msun)))**0.6667d0/rsun
  
end function kepler_p2a
!***********************************************************************************************************************************


