in terminal to compile the program input the following:<br />

cd src/ <br />
make<br />

if .mod files are not found you have to compile each program (except drunkenSailor.f90):<br />

gfortran TDTools.f90<br />
gfortran mtmod.f90<br />
gfortran arrayTools.f90<br />
gfortran getParams.f90<br />
make<br />


then to run:<br />
./sailor (integer) (Y/N) (walk/SAW) !integer>0, also only put one of the options i.e. either Y or N and walk or SAW<br />

Here the integer tells the program how many runs it should do, Y/N tells whether or not an output.dat file is wanted of trajectories, and walk or SAW tells walking type for the program.<br />

make clean removes all .o files and sailor file(the executable)
