ifeq ($(OS),Windows_NT)
	CC = fpc.exe
else 
	CC = fpc
endif

all: barrel/barrel.ppu barrel/templater.ppu barreltester.pas
	$(CC) -Fubarrel -Fubarrel/synapse barreltester.pas -O3 -Px86_64

barrel/templater.ppu: barrel/templater.pas
	$(CC) -Fubarrel barrel/templater.pas -O3 -Px86_64

barrel/barrel.ppu: barrel/barrel.pas
	$(CC) -Fubarrel -Fubarrel/synapse barrel/barrel.pas -O3 -Px86_64

clean:
	-rm *.o