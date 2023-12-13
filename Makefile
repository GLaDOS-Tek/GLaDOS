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
    ifeq (,$(wildcard /usr/bin/stack))
        curl -sSL https://get.haskellstack.org/ | sh
    else
        CURRENT_VERSION := $(shell stack --numeric-version)
        REQUIRED_VERSION := 2.13.1

        ifneq ($(CURRENT_VERSION),$(REQUIRED_VERSION))
            stack upgrade
        else
            @echo "Stack is already installed."
        endif
    endif