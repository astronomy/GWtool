!> \file gw_ligo_mcmc_injection.f90  Create an inspiral injection xml to use for lalinference_mcmc

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
!> \brief  Share stream data content

module data
  use SUFR_kinds, only: double,long
  implicit none
  private :: double, long
  
  character :: process_id*(99), waveform*(99), source*(99), numrel_data*(99), taper*(99), simulation_id*(99)
  integer(long) :: geocent_end_time,geocent_end_time_ns, h_end_time,h_end_time_ns, l_end_time,l_end_time_ns, g_end_time
  integer(long) :: g_end_time_ns, t_end_time,t_end_time_ns, v_end_time,v_end_time_ns
  integer(long) :: numrel_mode_min,numrel_mode_max, amp_order, bandpass
  real(double) :: mass1,mass2, mchirp,eta,  distance, longitude,latitude,  inclination, coa_phase, polarization
  real(double) :: end_time_gmst,  psi0,psi3,  alpha, alpha1,alpha2,alpha3,alpha4,alpha5,alpha6, beta
  real(double) :: spin1x,spin1y,spin1z, spin2x,spin2y,spin2z, spin1,theta1,phi1, spin2,theta2,phi2
  real(double) :: theta0,phi0,  f_lower,f_final,  eff_dist_h,eff_dist_l,eff_dist_g,eff_dist_t,eff_dist_v
  
end module data
!***********************************************************************************************************************************



!***********************************************************************************************************************************
module routines
  implicit none
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Write a (number of) injection xml floating-point data values to file
  !!
  !! \param op     Output unit
  !! \param array  Array of floating-point values to write
  
  subroutine write_floats(op,array)
    use SUFR_kinds, only: double
    
    implicit none
    integer, intent(in) :: op
    real(double), intent(in) :: array(:)
    integer :: i
    
    do i=1,size(array)
       if(array(i).lt.0.d0) then
          write(op,'(ES13.6,A)', advance='no') array(i),','  ! Extra space for minus sign
       else
          write(op,'(ES12.6,A)', advance='no') array(i),','
       end if
    end do
    
  end subroutine write_floats
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Write a (number of) injection xml integer data values to file
  !!
  !! \param op     Output unit
  !! \param array  Array of integer values to write
  
  subroutine write_ints(op, array)
    use SUFR_kinds, only: long
    implicit none
    integer, intent(in) :: op
    integer(long), intent(in) :: array(:)
    integer :: i
    
    do i=1,size(array)
       write(op,'(I0,A)', advance='no') array(i),','
    end do
    
  end subroutine write_ints
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Write an injection xml character data value to file
  !!
  !! \param op      Output unit
  !! \param string  Character string to write
  !! \param comma   Write comma after entry?
  
  subroutine write_string(op, string, comma)
    implicit none
    integer, intent(in) :: op
    character, intent(in) :: string*(*)
    logical, optional, intent(in) :: comma
    logical :: lcomma
    
    lcomma = .true.
    if(present(comma)) lcomma = comma
    
    write(op,'(A)', advance='no') '"'//trim(string)//'"'
    if(lcomma) write(op,'(A)', advance='no') ','
    
  end subroutine write_string
  !*********************************************************************************************************************************
  
end module routines
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Create an inspiral injection xml to use for lalinference_mcmc

program gw_ligo_mcmc_injection
  use SUFR_system, only: find_free_io_unit, syntax_quit, file_open_error_quit
  use SUFR_constants, only: set_SUFR_constants
  
  implicit none
  integer :: op, status, Narg
  character :: ipfile*(99),opfile*(99)
  
  call set_SUFR_constants()
  
  Narg = command_argument_count()
  if(Narg.eq.1.or.Narg.eq.2) then
     call get_command_argument(1, ipfile)
     opfile = 'injection.xml'
     if(Narg.eq.2) call get_command_argument(2, opfile)
  else
     call syntax_quit('<inputfile.dat> [<outputfile.xml>]', 0)
  end if
  
  
  call find_free_io_unit(op)
  open(unit=op,form='formatted',status='replace',action='write',file=trim(opfile),iostat=status)
  if(status.ne.0) call file_open_error_quit(trim(opfile), 0, 0)  ! 1st 0: output
  
  call write_xml_header(op)
  
  call set_default_injection_values()
  
  call read_injection_values(trim(ipfile))
  
  call write_xml_content_stream(op)
  
  call write_xml_footer(op)
  close(op)

end program gw_ligo_mcmc_injection
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Write the injection content stream to the xml file
!!
!! \param op  Output unit

subroutine write_xml_content_stream(op)
  use data, only: process_id, waveform, source, numrel_data, taper, simulation_id
  use data, only: geocent_end_time,geocent_end_time_ns, h_end_time,h_end_time_ns, l_end_time,l_end_time_ns, g_end_time
  use data, only: g_end_time_ns, t_end_time,t_end_time_ns, v_end_time,v_end_time_ns
  use data, only: numrel_mode_min,numrel_mode_max, amp_order, bandpass
  use data, only: mass1,mass2, mchirp,eta,  distance, longitude,latitude,  inclination, coa_phase, polarization
  use data, only: end_time_gmst,  psi0,psi3,  alpha, alpha1,alpha2,alpha3,alpha4,alpha5,alpha6, beta
  use data, only: spin1x,spin1y,spin1z, spin2x,spin2y,spin2z, spin1,theta1,phi1, spin2,theta2,phi2
  use data, only: theta0,phi0,  f_lower,f_final,  eff_dist_h,eff_dist_l,eff_dist_g,eff_dist_t,eff_dist_v
  use routines, only: write_ints, write_floats, write_string
  
  implicit none
  integer, intent(in) :: op
  
  write(op,'(9x)', advance='no')
  call write_string(op, trim(process_id))
  call write_string(op, trim(waveform))
  
  call write_ints(op, (/geocent_end_time, geocent_end_time_ns, h_end_time, h_end_time_ns, l_end_time, l_end_time_ns,  &
       g_end_time, g_end_time_ns,  t_end_time, t_end_time_ns, v_end_time, v_end_time_ns/) )
  
  write(op,'(ES22.16,A )', advance='no') end_time_gmst,','
       
  call write_string(op, trim(source))

  
  ! Compute chirp mass and eta from M1 and M2:
  call m1m2_mceta(mass1,mass2, mchirp,eta)
  
  call write_floats(op, (/mass1,mass2, mchirp,eta,  distance, longitude,latitude, inclination, coa_phase, polarization/) )
  call write_floats(op, (/psi0,psi3,  alpha, alpha1,alpha2,alpha3,alpha4,alpha5,alpha6, beta/) )
  
  ! Compute cartesian spin vectors from spherical:
  spin1x = spin1 * sin(theta1) * cos(phi1)
  spin1y = spin1 * sin(theta1) * sin(phi1)
  spin1z = spin1 * cos(theta1)
  
  spin2x = spin2 * sin(theta2) * cos(phi2)
  spin2y = spin2 * sin(theta2) * sin(phi2)
  spin2z = spin2 * cos(theta2)
  
  
  call write_floats(op, (/spin1x, spin1y, spin1z,  spin2x, spin2y, spin2z/) )
  call write_floats(op, (/theta0,phi0,  f_lower,f_final,  eff_dist_h, eff_dist_l, eff_dist_g, eff_dist_t, eff_dist_v/) )
  
  call write_ints(op, (/numrel_mode_min, numrel_mode_max/) )
  call write_string(op, trim(numrel_data))
  call write_ints(op, (/amp_order/) )
  call write_string(op, trim(taper))
  call write_ints(op, (/bandpass/) )
  call write_string(op, trim(simulation_id), comma=.false.)
  
end subroutine write_xml_content_stream
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Set the default values for the inspiral injection xml stream data

subroutine set_default_injection_values()
  use data, only: process_id, waveform, source, numrel_data, taper, simulation_id
  use data, only: geocent_end_time,geocent_end_time_ns, h_end_time,h_end_time_ns, l_end_time,l_end_time_ns, g_end_time
  use data, only: g_end_time_ns, t_end_time,t_end_time_ns, v_end_time,v_end_time_ns
  use data, only: numrel_mode_min,numrel_mode_max, amp_order, bandpass
  use data, only: mass1,mass2,  distance, longitude,latitude,  inclination, coa_phase, polarization
  use data, only: end_time_gmst,  psi0,psi3,  alpha, alpha1,alpha2,alpha3,alpha4,alpha5,alpha6, beta
  use data, only: spin1,theta1,phi1, spin2,theta2,phi2
  use data, only: theta0,phi0,  f_lower,f_final,  eff_dist_h,eff_dist_l,eff_dist_g,eff_dist_t,eff_dist_v

  implicit none
  
  waveform = 'SpinTaylorT4threePointFivePN'
  
  geocent_end_time = 1000000010
  geocent_end_time_ns = 0
  
  mass1 = 10.
  mass2 = 10.
  
  distance     = 20.
  longitude    = 1.5001
  latitude     = 0.7606
  inclination  = 0.1745329
  coa_phase    = 4.286765
  polarization = 0.3490658
  
  spin1  = 0.999
  theta1 = 1.1331489
  phi1   = 1.0176645
  spin2  = 0.0000001
  theta2 = 1.9058877
  phi2   = 4.5542608
  
  f_lower = 40.
  f_final = 0.  ! ?
  
  taper = 'TAPER_NONE'  ! ?
  
  
  ! Dummy values, not used by lalinference_mcmc:
  process_id = 'process:process_id:0'
  
  h_end_time    = 0
  h_end_time_ns = 0
  l_end_time    = 0
  l_end_time_ns = 0
  g_end_time    = 0
  g_end_time_ns = 0
  t_end_time    = 0
  t_end_time_ns = 0
  v_end_time    = 0
  v_end_time_ns = 0
  
  end_time_gmst = 0.
  
  source = ''
  
  
  psi0   = 0.
  psi3   = 0.
  alpha  = 0.
  alpha1 = 0.
  alpha2 = 0.
  alpha3 = 0.
  alpha4 = 0.
  alpha5 = 0.
  alpha6 = 0.
  beta   = 0.
  
  theta0 = 0.
  phi0   = 0.
  
  eff_dist_h = 0.
  eff_dist_l = 0.
  eff_dist_g = 0.
  eff_dist_t = 0.
  eff_dist_v = 0.
  
  numrel_mode_min = 0 
  numrel_mode_max = 0 
  numrel_data = ''
  
  amp_order = -1
  
  bandpass = 0 
  simulation_id = 'sim_inspiral:simulation_id:0'
  
end subroutine set_default_injection_values
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Set the default values for the inspiral injection xml stream data
!!
!! \param ipfile  Name of the input file

subroutine read_injection_values(ipfile)
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit
  use data, only: process_id, waveform, source, numrel_data, taper, simulation_id
  use data, only: geocent_end_time,geocent_end_time_ns, h_end_time,h_end_time_ns, l_end_time,l_end_time_ns, g_end_time
  use data, only: g_end_time_ns, t_end_time,t_end_time_ns, v_end_time,v_end_time_ns
  use data, only: numrel_mode_min,numrel_mode_max, amp_order, bandpass
  use data, only: mass1,mass2,  distance, longitude,latitude,  inclination, coa_phase, polarization
  use data, only: end_time_gmst,  psi0,psi3,  alpha, alpha1,alpha2,alpha3,alpha4,alpha5,alpha6, beta
  use data, only: spin1,theta1,phi1, spin2,theta2,phi2
  use data, only: theta0,phi0,  f_lower,f_final,  eff_dist_h,eff_dist_l,eff_dist_g,eff_dist_t,eff_dist_v
  
  implicit none
  character, intent(in) :: ipfile*(*)
  integer :: ip, status
  
  namelist /injection_data/ process_id, waveform, source, numrel_data, taper, simulation_id, &
       geocent_end_time,geocent_end_time_ns, h_end_time,h_end_time_ns, l_end_time,l_end_time_ns, g_end_time, &
       g_end_time_ns, t_end_time,t_end_time_ns, v_end_time,v_end_time_ns, &
       numrel_mode_min,numrel_mode_max, amp_order, bandpass, &
       mass1,mass2, distance, longitude,latitude,  inclination, coa_phase, polarization, &
       end_time_gmst,  psi0,psi3,  alpha, alpha1,alpha2,alpha3,alpha4,alpha5,alpha6, beta, &
       spin1,theta1,phi1, spin2,theta2,phi2, &
       theta0,phi0,  f_lower,f_final,  eff_dist_h,eff_dist_l,eff_dist_g,eff_dist_t,eff_dist_v
  
  
  call find_free_io_unit(ip)
  open(unit=ip,form='formatted',status='old',action='read',file=trim(ipfile),iostat=status)
  if(status.ne.0) call file_open_error_quit(trim(ipfile), 1, 0)  ! 1: input file

  read(ip, nml=injection_data, iostat=status)
  if(status.ne.0) call file_read_error_quit(trim(ipfile), 0, 0)  ! 1st 0: no particular line
  
  close(ip)
  
end subroutine read_injection_values
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Compute chirp mass and symmetric mass ration from individual masses
!!
!! \param m1  Individual mass 1
!! \param m2  Individual mass 2
!!
!! \retval mc   Chirp mass
!! \retval eta  Symmetric mass ratio

subroutine m1m2_mceta(m1,m2, mc,eta)
  use SUFR_kinds, only: double
  
  implicit none
  real(double), intent(in) :: m1, m2
  real(double), intent(out) :: mc, eta
  real(double) :: m
  
  m = m1+m2
  eta = m1*m2/(m*m)
  mc = m*eta**0.6d0
  
end subroutine m1m2_mceta
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Write the XML header to the injection file
!!
!! \param op  Output unit

subroutine write_xml_header(op)
  implicit none
  integer, intent(in) :: op
  
  write(op,'(A)') "<?xml version='1.0' encoding='utf-8' ?>"
  write(op,'(A)') '<!DOCTYPE LIGO_LW SYSTEM "http://ldas-sw.ligo.caltech.edu/doc/ligolwAPI/html/ligolw_dtd.txt"><LIGO_LW>'
  write(op,'(A)') '   <Table Name="sim_inspiralgroup:sim_inspiral:table">'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:process_id" Type="ilwd:char"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:waveform" Type="lstring"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:geocent_end_time" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:geocent_end_time_ns" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:h_end_time" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:h_end_time_ns" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:l_end_time" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:l_end_time_ns" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:g_end_time" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:g_end_time_ns" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:t_end_time" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:t_end_time_ns" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:v_end_time" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:v_end_time_ns" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:end_time_gmst" Type="real_8"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:source" Type="lstring"/>'
  write(op,'(6x,A)') ''
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:mass1" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:mass2" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:mchirp" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:eta" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:distance" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:longitude" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:latitude" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:inclination" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:coa_phase" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:polarization" Type="real_4"/>'
  write(op,'(6x,A)') ''
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:psi0" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:psi3" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:alpha" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:alpha1" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:alpha2" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:alpha3" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:alpha4" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:alpha5" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:alpha6" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:beta" Type="real_4"/>'
  write(op,'(6x,A)') ''
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:spin1x" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:spin1y" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:spin1z" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:spin2x" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:spin2y" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:spin2z" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:theta0" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:phi0" Type="real_4"/>'
  write(op,'(6x,A)') ''
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:f_lower" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:f_final" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:eff_dist_h" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:eff_dist_l" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:eff_dist_g" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:eff_dist_t" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:eff_dist_v" Type="real_4"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:numrel_mode_min" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:numrel_mode_max" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:numrel_data" Type="lstring"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:amp_order" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:taper" Type="lstring"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:bandpass" Type="int_4s"/>'
  write(op,'(6x,A)') '<Column Name="sim_inspiralgroup:sim_inspiral:simulation_id" Type="ilwd:char"/>'
  write(op,'(6x,A)') ''
  write(op,'(6x,A)') '<Stream Name="sim_inspiralgroup:sim_inspiral:table" Type="Local" Delimiter=",">'
  
end subroutine write_xml_header
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Write the XML footer to the injection file
!!
!! \param op  Output unit

subroutine write_xml_footer(op)
  implicit none
  integer, intent(in) :: op
  
  write(op,*)
  write(op,'(6x,A)') '</Stream>'
  write(op,'(6x,A)') ''
  write(op,'(A)') '   </Table>'
  write(op,'(A)') '</LIGO_LW>'
  
end subroutine write_xml_footer
!***********************************************************************************************************************************

