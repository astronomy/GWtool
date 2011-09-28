#Compile the programme and put the executable in ~/bin

#FORT = lf95
#CFLAGS = --wide -O -lpgplot -lX11 -lpng 
FORT = ifort
#CFLAGS = -O -132 -vec-report0 -lpgplot_ifort -L/usr/X11R6/lib -lX11 -lm -L/opt/local/lib -laquaterm -Wl,-framework -Wl,Foundation -lifcore -limf -lirc -lsvml -lifport -Xlinker -multiply_defined suppress -bind_at_load 
#CFLAGS = -O0 -132 -vec-report0 
CFLAGS = -O0 -free -vec-report0 -traceback -warn all -check all
#LFLAGS = -lpgplot_ifort -L/usr/X11R6/lib -lX11 -lm -L/opt/local/lib -laquaterm -Wl,-framework -Wl,Foundation -lifcore -limf -lirc -lsvml -lifport -Xlinker -multiply_defined suppress -bind_at_load 
LFLAGS = 

FORT = gfortran
CFLAGS = -O2


STDOBJ = #functions.o

SRCDIR = src
OBJDIR = obj
#MODDIR = mod
#LIBDIR = lib
BINDIR = ~/usr/bin


#%.o: %.f90
#	$(FORT) $(OPT) $(CFLAGS) -c $< -o $@

#.f90.o: 
#	$(FORT) $(CFLAGS) -c $?

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FORT) $(OPT) $(CFLAGS) -c $< -o $@

exit:

all: isco m1_m2-mc_eta mc_eta-m1_m2 tcoal

isco: $(STDOBJ) $(OBJDIR)/isco.o
	$(FORT) $(LFLAGS) -o $(BINDIR)/isco $(STDOBJ) $(OBJDIR)/isco.o

m1_m2-mc_eta: $(STDOBJ) $(OBJDIR)/m1_m2-mc_eta.o
	$(FORT) $(LFLAGS) -o $(BINDIR)/m1_m2-mc_eta $(STDOBJ) $(OBJDIR)/m1_m2-mc_eta.o

mc_eta-m1_m2: $(STDOBJ) $(OBJDIR)/mc_eta-m1_m2.o
	$(FORT) $(LFLAGS) -o $(BINDIR)/mc_eta-m1_m2 $(STDOBJ) $(OBJDIR)/mc_eta-m1_m2.o

tcoal: $(STDOBJ) $(OBJDIR)/tcoal.o
	$(FORT) $(LFLAGS) -o $(BINDIR)/tcoal $(STDOBJ) $(OBJDIR)/tcoal.o


clean:
	rm -f $(OBJDIR)/*.o #$(MODDIR)/*.mod
