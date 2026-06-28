import random as r

state = list[int]


class Board:
    def __init__(self):
        self.node: state = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.turn: int = 1

    def __repr__(self):
        builder = ""
        for x in range(3):
            for y in range(3):
                if self.pos_filled(x * 3 + y):
                    if self.player_at(x * 3 + y) == 1:
                        builder += "X "
                    else:
                        builder += "0 "
                else:
                    builder += "* "
            builder += '\n'
        builder += '\n'
        return builder

    def pos_filled(self, i: int):
        if self.node[i] == 1 or self.node[i] == -1:
            return True
        return False

    # only valid to use if self.pos_filled() returns True:
    def player_at(self, i: int):
        return self.node[i]

    def eval(self):
        #  check first diagonal
        if (self.pos_filled(0) and self.pos_filled(4) and self.pos_filled(8)):
            if (self.player_at(0) == self.player_at(4) and self.player_at(4) == self.player_at(8)):
                if (self.player_at(0)):
                    return 1
                else:
                    return -1

        #  check second diagonal
        if (self.pos_filled(2) and self.pos_filled(4) and self.pos_filled(6)):
            if (self.player_at(2) == self.player_at(4) and self.player_at(4) == self.player_at(6)):
                if (self.player_at(2)):
                    return 1
                else:
                    return -1

        #  check rows
        for i in range(3):
            if (self.pos_filled(i * 3) and self.pos_filled(i * 3 + 1) and self.pos_filled(i * 3 + 2)):
                if (self.player_at(i * 3) == self.player_at(i * 3 + 1) and self.player_at(i * 3 + 1) == self.player_at(i * 3 + 2)):
                    if (self.player_at(i * 3)):
                        return 1
                    else:
                        return -1

        #  check columns
        for i in range(3):
            if (self.pos_filled(i) and self.pos_filled(i + 3) and self.pos_filled(i + 6)):
                if (self.player_at(i) == self.player_at(i + 3) and self.player_at(i + 3) == self.player_at(i + 6)):
                    if (self.player_at(i)):
                        return 1
                    else:
                        return -1
        return 0

    def end(self):
        return all(self.node) or self.eval()

    def legs(self):
        return [move for move in range(9) if not self.pos_filled(move)]

    def hop(self):
        move = int(input('move\n=> '))-1
        self.node[move] = self.turn
        self.turn *= -1

    def rollout(self):
        rollouts = 1000
        score = 0
        for i in range(rollouts):
            roller = Board()
            roller.node = [p for p in self.node]
            roller.turn = self.turn
            while not roller.end():
                move = r.choice(roller.legs())
                roller.node[move] = roller.turn
                roller.turn *= -1
            score += roller.eval()
        return score / rollouts

    def Ihop(self):
        move = r.choice(self.legs())
        self.node[move] = self.turn
        self.turn *= -1

    def Rhop(self):
        moves = self.legs()
        scores = [0 for i in moves]
        for i, move in enumerate(moves):
            self.node[move] = self.turn
            self.turn *= -1
            scores[i] = self.rollout()
            self.node[move] = 0
            self.turn *= -1
        maxvalpos = scores.index(min(scores))
        self.node[moves[maxvalpos]] = self.turn
        self.turn *= -1
        print("WIN PREDICTION: ", (1-min(scores))*100, "%")


if __name__ == "__main__":

    board = Board()
    print(board)
    while not board.end():
        board.hop()
        if board.end():
            print(board)
            break
        print(board)
        board.Rhop()
        print(board)
