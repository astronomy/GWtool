!Convert the Chirp mass and symmetric mass ratio to M1 and M2
program mc_eta_m1_m2
  implicit none
  real*8 :: eta,eta1,mc,m1,m2,mtot
  character :: bla*99
  mc = 2.99
  eta = 0.11
  if(iargc().eq.2) then
     call getarg(1,bla)
     read(bla,*)mc
     call getarg(2,bla)
     read(bla,*)eta
  else
     write(6,'(A)')'  Syntax: mc_eta-m1_m2 <Mc> <eta>'
     write(6,'(A,$)')'  Give Mc, eta: '
     read*,mc,eta
  end if
  if(eta.lt.0..or.eta.gt.0.5) then
     write(0,'(/,A,/)')'  Error: eta should be 0.0<eta<0.5'
     stop
  end if
  if(eta.le.0.25) then
     write(6,'(/,A,2F15.8)')'  Mc, eta:  ',mc,eta
     mtot = mc/eta**(3/5.)
     m1 = mtot/2. * (1+sqrt(1-4*eta))
     m2 = mtot/2. * (1-sqrt(1-4*eta))
  else
     eta1 = 0.5 - eta
     write(6,'(/,A,2F15.8,A,F8.5,A1)')'  Warning: eta > 0.25; Mapping it to 0.5-eta:'
     write(6,'(A,2F15.8,A,F8.5,A1)')'  Mc, eta:  ',mc,eta,'  (->',eta1,')'
     mtot = mc/eta1**(3/5.)
     m1 = mtot/2. * (1+sqrt(1-4*eta1))
     m2 = mtot/2. * (1-sqrt(1-4*eta1))
  end if
  write(6,'(A,2F15.8)')'  M1, M2:   ',m1,m2
  write(6,'(A,2F15.8)')'  M1/M2, M: ',m1/m2,m1+m2
  write(*,*)
end program mc_eta_m1_m2
