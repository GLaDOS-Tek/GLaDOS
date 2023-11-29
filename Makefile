##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Glados
##

all:
	cd Glados && stack install

clean:
	cd Glados && stack clean

fclean: clean
	rm -f $(NAME)
	cd Glados && stack purge

re: fclean all

.PHONY: all clean fclean re