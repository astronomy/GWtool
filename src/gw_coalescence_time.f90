!> \file gw_coalescence_time.f90  Calculates the time needed to reach a certain Porb (default 0) using gravitational-wave evolution

!***********************************************************************************************************************************
program gwtime
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
  
  write(*,'(A,ES15.7,A,/)')'  Time needed: ',trl,' yr'
  
end program gwtime
!***********************************************************************************************************************************

