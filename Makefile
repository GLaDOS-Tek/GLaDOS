## EPITECH PROJECT, 2023
## Makefile
## File description:
## Glados

.PHONY: all clean fclean re test

all: install-stack
	cd Glados && stack install

clean:
	cd Glados && stack clean

fclean: clean
	cd Glados && stack purge

re: fclean all

test: all
	cd Glados && stack test

OS := $(shell uname)

install-stack:
	ifeq ($(OS),Linux)
	ifneq (,$(wildcard /usr/bin/stack))
		@echo "Stack is already installed."
	else
		curl -sSL https://get.haskellstack.org/ | sh
	endif
	else
		@echo "This Makefile is designed for Ubuntu/Linux and Stack installation."
	endif
