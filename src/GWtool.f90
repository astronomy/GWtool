!> \file GWtool.f90  Used for documentation purposes
!!  
!!  \mainpage GWtool documentation
!!  GWtool is a set of simple command-line tools to deal with gravitational waves.  GWtool is available for free
!!  under the conditions of the GPL v.3.
!!  
!!  \par
!!  Copyright &copy; 2007-2013  AstroFloyd - <a href="http://astrofloyd.org">astrofloyd.org</a>
!!
!!  \see
!!  These pages contain the documentation for GWtool, created by Doxygen.  
!!  For the GWtool home page, see: http://gwtool.sf.net
!!
!!  \section Programs
!!
!! - \b gw_coalescence_pmax:     Calculates the maximum initial period that can coalesce within a certain time
!! - \b gw_coalescence_time:     Calculates the time needed to reach a certain Porb (default 0) using gravitational-wave evolution
!! - \b gw_coalescence_time_from_f0:  Calculates time of coalescence for a LIGO binary from certain frequency f0
!! - \b gw_eta2q:                Compute the symmetric mass ratio &eta; from the asymmetric mass ratio q
!! - \b gw_final_period:         Calculates the orbital period of a detached stellar binary after a given time of GW evolution
!! - \b gw_isco:                 Calculates isco properties for a black hole with given mass and spin
!! - \b gw_ligo_mcmc_injection:  Create an inspiral injection xml to use for lalinference_mcmc
!! - \b gw_ligo_xml2screen:      Print the contents of a LIGO/Virgo injection.xml file to screen
!! - \b gw_m1m2-mceta:           Compute chirp mass and symmetric mass ratio from the individual masses
!! - \b gw_match_wfs:            Match two time-domain waveforms in specified files
!! - \b gw_mceta-m1m2:           Convert the chirp mass and symmetric mass ratio to individual masses
!! - \b gw_q2eta:                Compute the symmetric mass ratio &eta; from the asymmetric mass ratio q


!  Copyright (c) 2007-2013  AstroFloyd - astrofloyd.org
!   
!  This file is part of the GWtool package, 
!  see: http://gwtool.sf.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
!  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.


