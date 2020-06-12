package view;

import utils.Pair;

import java.util.ArrayList;

public interface GameView {

    void update(ArrayList<Pair> list);

    void showPossibleMoves();

}