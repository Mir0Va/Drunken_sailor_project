in terminal to compile the program input the following:

cd src/ <br />
make

if .mod files are not found you have to compile each program (except drunkenSailor.f90):

gfortran TDTools.f90
gfortran mtmod.f90
gfortran arrayTools.f90
gfortran getParams.f90
make


then to run:
./sailor (integer) (Y/N) (walk/SAW) !integer>0, also only put one of the options i.e. either Y or N and walk or SAW

Here the integer tells the program how many runs it should do, Y/N tells whether or not an output.dat file is wanted of trajectories, and walk or SAW tells walking type for the program.

make clean removes all .o files and sailor file(the executable)
