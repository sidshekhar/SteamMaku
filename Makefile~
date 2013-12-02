################################################################################
# Makefile for Steammaku
#
# This section contains convenience commands for building and running the game
# You may wish to modify these or add new ones

default: all
all: team/babybot.exe game/game.exe

# Note: you should not need to run "make clean" every time you change something.
# The "make" tool figures out what has changed and what needs to be rebuilt.
# However, you should run "make clean" before you submit; submitting compiled
# files is bad form.
clean:
	rm -f $(shell find -name "*.cmo")
	rm -f $(shell find -name "*.cmi")
	rm -f $(shell find -name "*.cma")
	rm -f $(shell find -name "*.exe")
	rm -f .depend


FLAGS = -I +threads -I shared -I team -I game
LIBS  = unix.cma threads.cma str.cma

top: shared/shared.cma
	rlwrap ocaml $(FLAGS) $(LIBS) $^

gui:
	java -jar gui/gui_client.jar


# .PHONY tells make that these targets aren't really files that need to be built
.PHONY: default all clean top gui

################################################################################
# This section lists the prerequisites for the files that don't have
# corresponding .ml files
#
# You should add any new files you create to these lists of prerequisites.  If
# you want to create new executables, you can add them here too as well.
# Be sure to put \ at the end of each line, and be sure to put them in the right
# order.
#


# shared.cma bundles together the shared modules for convenience
shared/shared.cma:             \
	shared/definitions.cmo \
	shared/connection.cmo  \
	shared/constants.cmo   \
	shared/util.cmo

team/babybot.exe:         \
	shared/shared.cma \
	team/team.cmo     \
	team/babybot.cmo

game/game.exe:               \
	shared/shared.cma    \
	game/netgraphics.cmo \
	game/game.cmo        \
	game/server.cmo


################################################################################
# These define the general rules for compiling ocaml programs
# 
# You shouldn't need to change them

%.cmo: %.ml
	ocamlc -c $(FLAGS) $< -o $@

%.cmi: %.mli
	ocamlc -c $(FLAGS) $< -o $@

%.exe:
	ocamlc $(FLAGS) $(LIBS) $^ -o $@

%.cma:
	ocamlc -a $(FLAGS) $^ -o $@


################################################################################
# This section causes make to find the set of dependencies for each .ml and
# .mli file
# 
# You shouldn't need to edit this portion

FILES=$(wildcard game/*.ml* shared/*.ml* team/*.ml*)

.depend: $(FILES)
	ocamldep $(FLAGS) $(FILES) > .depend
include .depend

