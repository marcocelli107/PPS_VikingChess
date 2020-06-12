package view;

import scala.Int;
import utils.BoardGame.Board;
import utils.Pair;

import java.util.List;


public interface GameView {


    void updateMove(Board board, int nBlackCaptured, int nWhiteCaptured);

    void setEndGame(String winner, List<Pair<Int>> coordKing);




}