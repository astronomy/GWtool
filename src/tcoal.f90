! tcoal.f: Calculates time of coalescence from certain frequency f0.  Adapted from p2a.f

program tcoal
  implicit none
  
  real*8 :: m1,m2,mc,mu,eta,t,f0,m,s,theta,x,beta,pn
  real*8 :: pi,g,c,m0,day,c3rd
  integer :: narg,iargc
  character :: str*6
  
  pi = 4*datan(1.d0)
   g = 6.67259d-8
   c = 299792458.d2
  m0 = 1.9891d33*g/c**3 !Solar mass in seconds (4.926d-6)
  day = 8.64d4
  c3rd = 1.d0/3.d0
  
  narg = iargc()
  if(narg.eq.5) then
     call getarg(1,str)
     read(str,*)f0
     call getarg(2,str)
     read(str,*)m1
     call getarg(3,str)
     read(str,*)m2
     call getarg(4,str)
     read(str,*)s
     call getarg(5,str)
     read(str,*)theta
  else
     write(6,*)''
     write(6,'(A)')'This program calculates the coalescence time for a binary with masses M1 and M2 from a lower GW frequency of f0.'
     write(6,'(A)')'  syntax: tcoal <f0> <M1> <M2> <S1> <theta_SL>'
     write(6,*)''
     write(6,'(A30,$)')'f0, M1, M2, S1, theta_SL:  '
     read*,f0,m1,m2,s,theta
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
  
end program
