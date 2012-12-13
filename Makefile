HC=ghc
FLAGS=

#SRC=IrcParse.hs Irc.hs Glennbot.hs
SRC=Protocol.hs Glennbot.hs
OUTPUT=gb

all: $(OUTPUT) 

go: $(OUTPUT)
	./go

clean:
	rm -f *.hi *.o
	rm -f *~

$(OUTPUT): $(SRC)
	$(HC) --make -o $(OUTPUT) $(SRC)
