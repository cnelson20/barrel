CC = fpc

FLAGS = -O3 -Px86_64
SRC_LOC = ../barrel

all: copy barreltester.pas
	$(CC) -Fubarrel/synapse barreltester.pas $(FLAGS)

copy: $(SRC_LOC)/*
	make -C $(SRC_LOC)
	cp $(SRC_LOC)/*.o ./
	cp $(SRC_LOC)/synapse -r ./

BARREL_FLAGS = -Fubarrel/synapse -Fubarrel $(FLAGS)

clean:
	-rm *.o
