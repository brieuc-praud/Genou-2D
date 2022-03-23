EXE = main
F90 = gfortran
OPT = -O0 -Wall -pedantic
OBJ = variables.o linalg.o quatre_barres_forces.o lissage.o $(EXE).o
OUT = 'comparaison_cir.dat'


$(EXE)	: $(OBJ)
	$(F90) $(OPT) -o $(EXE) $^

%.o	: %.f90
	$(F90) $(OPT) -c $<

clean	:
	rm *.o *.mod $(EXE)

exe	: $(EXE)
	./$(EXE)

plot	:
	gnuplot -e "set title 'Positions successives du centre instantané de rotation (CIR) au cours du mouvement' ; plot $(OUT)  u 1:2 w lp title 'CIR du modèle', $(OUT) u 3:4 w lp title 'CIR du relevé'; pause -1"

anim 	:
	python3 animation.py

exeplot :
	make exe && make plot

exeanim :
	make exe && make anim
