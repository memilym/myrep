import math
def display_board(board):
    print("+-------" * 3, sep="+", end="+\n")
    print("|       " * 3, sep="|", end="|\n")
    print("|  ",board[0], "  |  ", board[1], "  |  ", board[2], end="   |\n")
    print("|       " * 3, sep="|", end="|\n")
    print("+-------" * 3, sep="+", end="+\n")
    print("|       " * 3, sep="|", end="|\n")
    print("|  ",board[3], "  |  ", board[4], "  |  ", board[5], end="   |\n")
    print("|       " * 3, sep="|", end="|\n")
    print("+-------" * 3, sep="+", end="+\n")
    print("|       " * 3, sep="|", end="|\n")
    print("|  ",board[6], "  |  ", board[7], "  |  ", board[8], end="   |\n")
    print("|       " * 3, sep="|", end="|\n")
    print("+-------" * 3, sep="+", end="+\n")

board=[1,2,3,4,"X",6,7,8,9]
display_board(board)

def enter_move(board):
    move = int(input("Enter your move: "))
    if move >= 1 and move <= 9:
        if board[move - 1] == "X":
            print("Miejsce jest już zajęte.")
        else: 
            board[move - 1] = "O"
    else:
        print("Wprowadziłeś złą liczbę.")
    display_board(board)
        
enter_move(board)

def make_list_of_free_fields(board):
    list_of_free_fields = []
    rows = ()
    columns = ()
    for i in range(0,9):
        if board[i] != "O" and board[i] != "X":
            if (i+1) % 3 == 0:
                columns = 3
            else:
                columns = (i+1) % 3
            rows = math.ceil((i+1)/3)
            list_of_free_fields.append((rows,columns)) 
        i += 1
        
    print("List of free fields: ", list_of_free_fields)

make_list_of_free_fields(board)

def victory_for(board, sign):
    if board[0] == board[1] == board[2] == sign or \
        board[3] == board[4] == board[5] == sign or \
        board[6] == board[7] == board[8] == sign or \
        board[1] == board[4] == board[7] == sign or \
        board[2] == board[5] == board[8] == sign or \
        board[0] == board[3] == board[6] == sign or \
        board[0] == board[4] == board[8] == sign or \
        board[2] == board[4] == board[6] == sign:
            if sign == "X":
                print("You lose the game.")
            elif sign == "O":
                print("You won the game!")
    else:
        return False 

victory_for(board, "O")

from random import randrange

def draw_move(board):
    computer = randrange(8)
    while True:
        if board[computer] != "O" and board[computer] != "X":
            board[computer] = "X"
            break
draw_move(board)

while victory_for(board, "O") == False:
    display_board(board)
    enter_move(board)
    make_list_of_free_fields(board)
    victory_for(board, "O")
    draw_move(board)


    
