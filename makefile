ifndef CC
	ifeq ($(OS),Windows_NT)
		CC = fpc.exe
	else 
		CC = fpc
	endif
endif

all: barrel.ppu barreltester.pas
	$(CC) -Fusynapse barreltester.pas

barrel.ppu: barrel.pas
	$(CC) -Fusynapse barrel.pas

clean:
	-rm *.o