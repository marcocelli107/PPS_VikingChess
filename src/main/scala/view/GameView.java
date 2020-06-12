package view;

import model.Player;
import utils.Board;
import utils.Pair;

import java.util.List;


public interface GameView {


    void updateMove(Board board, int nBlackCaptured, int nWhiteCaptured);

    void setEndGame(Player winner, List<Pair> coordKing);




}