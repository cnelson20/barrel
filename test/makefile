ifeq ($(OS),Windows_NT)
	CC = fpc.exe
else 
	CC = fpc
endif

FLAGS = -O3 -Px86_64
SRC_LOC = ../barrel

all: copy barreltester.pas
	$(CC) -Fubarrel/synapse barreltester.pas $(FLAGS)

copy: $(SRC_LOC)/*
	cp $(SRC_LOC) -r ./

barrel/templater.ppu: barrel/templater.pas
	$(CC) barrel/templater.pas $(FLAGS)

barrel/barrel.ppu: barrel/barrel.pas
	$(CC) -Fubarrel/synapse barrel/barrel.pas $(FLAGS)

clean:
	-rm *.o