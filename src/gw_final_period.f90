!> \file gw_final_period.f90  Calculates the orbital period of a detached stellar binary after a given time of GW evolution


!***********************************************************************************************************************************
!> \brief  Calculates the orbital period of a detached stellar binary after a given time of GW evolution

program gw_final_period
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_constants, only: set_SUFR_constants, c3rd,pi2, pc_g,pc_c, msun, solday,julyear
  
  implicit none
  real(double) :: m1,m2,time,cst,Pin,Pf83,Pf
  character :: tmpstr*(99)
  
  if(command_argument_count().eq.4) then
     call get_command_argument(1, tmpstr)
     read(tmpstr,*) M1
     call get_command_argument(2, tmpstr)
     read(tmpstr,*) M2
     call get_command_argument(3, tmpstr)
     read(tmpstr,*) Pin
     call get_command_argument(4, tmpstr)
     read(tmpstr,*) time
  else
     call syntax_quit('<M1> <M2> <Pi> <t>  (Mo, days and yr)', 0, &
          'This program calculates the orbital period of a detached stellar binary after a given time of GW evolution')
  end if
  
  call set_SUFR_constants()
  
  cst = 5.d0/256.d0 * pi2**(-8.d0*c3rd) * pc_c**5 / pc_g**(5.d0*c3rd) * (M1+M2)**c3rd / (M1*M2*msun**(5.d0*c3rd))
  Pf83 = (Pin*solday)**(8.d0*c3rd) - time*julyear/cst  ! Pf^8/3
  
  
  write(*,*)
  if(Pf83.lt.0.d0) then
     write(*,'(2x,A,ES11.3,A)') 'System has already merged after ',time,' yr.'
     Pf83 = 0.d0
  end if
  
  Pf = Pf83 ** 0.375d0 / solday
  
  write(*,'(2x,A,ES20.9,A5)') 'Pf:    ',Pf,' days'
  write(*,'(2x,A,ES20.9,A5)') 'Pf/Pi: ',Pf/Pin,''
  write(*,*)
  
end program gw_final_period
!***********************************************************************************************************************************

