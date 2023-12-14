## EPITECH PROJECT, 2023
## Makefile
## File description:
## Glados

.PHONY: all clean fclean re test

all:
	cd Glados && stack install

clean:
	cd Glados && stack clean

fclean: clean
	cd Glados && stack purge

re: fclean all

test: all
	cd Glados && stack test
