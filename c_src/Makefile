LIBS=-lortp -lpthread -lrt

all: compile

compile: clean
	gcc -o ../_build/voice_client rtpsend.c $(LIBS)

clean:
	rm  ../_build/voice_client