all: barrel.ppu templater.ppu barrelhelpers.ppu

CC = fpc

FLAGS = -O3 -Px86_64
BARREL_FLAGS = -Fusynapse/ $(FLAGS)

templater.ppu: templater.pas
	$(CC) $(BARREL_FLAGS) templater.pas

barrel.ppu: barrel.pas
	$(CC) $(BARREL_FLAGS) barrel.pas

barrelhelpers.ppu: barrelhelpers.pas
	$(CC) $(BARREL_FLAGS) barrelhelpers.pas
