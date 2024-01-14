FC=gfortran
FFlags=-O3 -Wall -Wextra -std=f2018
SRC=arrayTools.f90 drunkenSailor.f90 getParams.f90 mtmod.f90 TDTools.f90
OBJ=${SRC:.f90=.o}

%.o: %.f90
	$(FC) $(FFlags) -o $@ -c $<

sailor: $(OBJ)
	$(FC) $(FFlags) -o $@ $(OBJ)

clean:
	@rm -f *.o sailor
