!> \file gw_ligo_xml2screen.f90  Print the contents of a LIGO/Virgo injection.xml file to screen

!***********************************************************************************************************************************
!> \brief  Print the contents of a LIGO/Virgo injection.xml file to screen

program gw_ligo_xml2screen
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: find_free_io_unit, quit_program_error, syntax_quit
  use SUFR_text, only: replace_substring, remove_substring
  
  implicit none
  integer, parameter :: maxcol=199
  integer :: verbose, status, ip, ln, i1,i2, coli,ncol,colnr, strlen, ntabs
  !real :: spin1x,spin1y,spin1z, spin2x,spin2y,spin2z, a1,phi1,theta1, a2
  character :: infile*(99), line*(999), colnames(maxcol)*(99), values(maxcol)*(99)
  logical :: instream
  
  verbose = 0
  
  call set_SUFR_constants()
  
  if(command_argument_count().eq.1) then
     call get_command_argument(1, infile)
  else
     call syntax_quit('<xml file>', 0)
  end if
  
  
  instream = .false.
  colnames = ' '
  values = ' '
  
  call find_free_io_unit(ip)
  open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(infile),iostat=status)
  if(status.ne.0) call quit_program_error('Error opening '//trim(infile)//', aborting...', 0)
  
  
  ln = 0
  colnr = 0
  ntabs = 0
  do
     ln = ln + 1
     
     read(ip,'(A)', iostat=status) line
     if(status.lt.0) exit
     if(status.gt.0) then
        write(0,'(A,I4,A,/)') '  Error reading '//trim(infile)//', line',ln,' aborting...'
        stop
     end if
     
     line =  adjustl(line)  ! left-adjust line
     
     if(verbose.gt.0) then
        print*
        print*,ln,trim(line)
     end if
     
     if(index(line,'</Stream>').ne.0) inStream = .false.
     
     if(index(line,'<Table ').ne.0) then  ! Print table name
        ntabs = ntabs + 1
        call remove_substring(line, '<Table Name="')
        call remove_substring(line, ':table')
        call remove_substring(line, '">')
        call replace_substring(line, ':', ' - ')
        
        write(*,*)
        write(*,*)
        write(*,'(A)') '##########################################################################################################'
        write(*,'(A)') '  Table: '//trim(line)
        write(*,'(A)') '##########################################################################################################'
        colnr = 0
     end if
     
     if(index(line,'<Column').ne.0) then  ! Get column title
        call remove_substring(line, '<Column Name="')
        call remove_substring(line, 'processgroup:process:')
        call remove_substring(line, 'process_paramsgroup:process_params:')
        call remove_substring(line, 'sim_inspiralgroup:sim_inspiral:')
        i1 = index(line, '<Column Name="', back=.false.)
        i2 = index(line, ' Type="', back=.true.) - 2
        
        if(verbose.gt.0) print*,i1,i2,line(1:i2)
        colnr = colnr + 1
        colnames(colnr) = line(1:i2)
     end if
     
     if(inStream) then
        strlen = len_trim(line)
        call replace_substring(line, ' ', '_')
        line(strlen+1:) = ' '
        call replace_substring(line, ',', ' ')
        
        if(verbose.gt.0) print*,'Stream: ',trim(line)
        read(line,*, iostat=status) values
        !if(status.lt.0) exit
        if(status.gt.0) then
           write(0,'(A,I4,A,/)') '  Error reading stream value column',coli,' aborting...'
           stop
        end if
        
        ! Print results:
        write(*,*)
        ncol = 0
        do coli=1,maxcol
           if(len_trim(values(coli)).ne.0 .or. len_trim(colnames(coli)).ne.0) then
              ncol = ncol+1
              if(mod(coli,10).eq.1) write(*,*)
              write(*,'(I5,2A50)') coli,trim(colnames(coli)),trim(values(coli))
              
              !if(trim(colnames(coli)).eq.'spin1x') read(values(coli),*) spin1x
              !if(trim(colnames(coli)).eq.'spin1y') read(values(coli),*) spin1y
              !if(trim(colnames(coli)).eq.'spin1z') read(values(coli),*) spin1z
              !if(trim(colnames(coli)).eq.'spin2x') read(values(coli),*) spin2x
              !if(trim(colnames(coli)).eq.'spin2y') read(values(coli),*) spin2y
              !if(trim(colnames(coli)).eq.'spin2z') read(values(coli),*) spin2z
           end if
        end do
        
        !a1 = sqrt(spin1x**2 + spin1y**2 + spin1z**2)
        !theta1 = 0.
        !phi1 = 0.
        !if(a1.gt.0.) then
        !   theta1 = acos(spin1z/a1)
        !end if
        !a2 = sqrt(spin2x**2 + spin2y**2 + spin2z**2)
        
        write(*,'(/,A,2(I0,A))') '  Found: ',colnr,' column titles, ',ncol,' column values.'
        if(colnr.ne.ncol) write(0,'(//,A,2(I0,A),//)') '  *** Warning: the number of column titles (',colnr, &
             ') is not equal to the number of column values (',ncol,').  Please verify the output.'
     end if
     
     if(index(line,'<Stream ').ne.0)  inStream = .true.
     
  end do
  
  
  
  ln = ln - 1
  close(ip)
  
  write(*,'(/,A,I0,A)') '  Found: ',ntabs,' tables.'
  
  write(*,*)
  
end program gw_ligo_xml2screen
!***********************************************************************************************************************************
