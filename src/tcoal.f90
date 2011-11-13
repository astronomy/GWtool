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



!> \file tcoal.f90 Calculates time of coalescence from certain frequency f0.
!!
!! Adapted from p2a.f



!***********************************************************************************************************************************
program tcoal
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants, c3rd,pi, pc_g,pc_c, msun
  
  implicit none  
  real(double) :: m1,m2,mc,mu,eta,t,f0,m,s,theta,x,beta,pn
  real(double) :: m0
  integer :: narg
  character :: str*(6)
  
  call set_SUFR_constants()
  m0   = msun*pc_g/pc_c**3  ! Solar mass in seconds (4.926d-6)
  
  narg = command_argument_count()
  if(narg.eq.5) then
     call get_command_argument(1,str)
     read(str,*)f0
     call get_command_argument(2,str)
     read(str,*)m1
     call get_command_argument(3,str)
     read(str,*)m2
     call get_command_argument(4,str)
     read(str,*)s
     call get_command_argument(5,str)
     read(str,*)theta
  else
     write(6,'(/,A)')'This program calculates the coalescence time for a binary with masses M1 and M2 from a lower GW'// &
          ' frequency of f0.'
     write(6,'(A,/)')'  syntax: tcoal <f0> <M1> <M2> <S1> <theta_SL>'
     stop
     !write(6,'(A30,$)')'f0, M1, M2, S1, theta_SL:  '
     !read*,f0,m1,m2,s,theta
  end if
  
  theta = theta/180.d0*pi
  
  m = m1+m2
  mu = m1*m2/m
  eta = mu/m
  mc = m*eta**0.6
  
  !Newtonian:
  t = 5*(8.d0*pi*f0)**(-8.d0*c3rd) * (mc*m0)**(-5.d0*c3rd)
  
  !PN:
  beta = 1.d0/12.d0*(113*(m1/m)**2 + 75*eta)*s*cos(theta)
  x = pi*m*m0*f0
  pn = 1.d0 + 0.75d0*(743.d0/336.d0 + 11.d0/4.d0*eta)*x**(2*c3rd) - 8.d0/5.d0*(4*pi-beta)*x
  
  
  write(6,*)''
  write(6,'(A15,3F15.4)')'M1,M2,f0:    ',m1,m2,f0
  write(6,'(A15,3F15.4)')'Mu,eta,Mc:   ',mu,eta,mc
  write(6,'(A15)')       'Tcoal:       '
  write(6,'(A15,F15.4)') '  Newtonian: ',t
  write(6,'(A15,F15.4)') '  PN:        ',t*pn
  write(6,*)''
  
end program tcoal
!***********************************************************************************************************************************

