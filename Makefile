HC      = ghc
HC_OPTS = -cpp $(EXTRA_HC_OPTS)

SRCS = Sabio.hs Laberinto.hs
OBJS = Sabio.o   Laberinto.o

.SUFFIXES : .o .hs .hi

Sabio : $(OBJS)
	rm -f $@
	$(HC) -o $@ $(HC_OPTS) $(OBJS)

# reglas estandard de sufijos
.o.hi:
        @:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot:
        @:

.lhs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

.hs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

# Dependencias
Sabio.o Sabio.hs    : Laberinto.o          # Sabio
