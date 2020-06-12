package view;

import model.PieceEnum;
import model.Player;
import scala.Enumeration;
import scala.Int;
import scala.Tuple3;
import utils.Board;
import utils.Pair;
import utils.Pair.PairImpl;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;


public class Game {

    GameViewImpl gameViewImpl;

    public JPanel gamePanel,northPanel,southPanel,boardPanel,boardPlusColumns, leftPanel, rightPanel;
    private JButton menuButton;
    private HashMap<Pair<Int>, JButton> cells;
    private ViewFactory viewFactory;
    private List<Pair<Integer>> possibleMoves;
    private Optional<Pair> selectedCell = Optional.empty();
    private Board.Board board;
    private ColorProvider colorProvider;
    private int lostBlackPawns, lostWhitePawns;
    private Enumeration.Value player;


    public Game(ViewFactory scalaViewFactory, GameViewImpl GVImpl){
        this.viewFactory = scalaViewFactory;
        gameViewImpl = GVImpl;
        cells = new HashMap<>();
        possibleMoves = new ArrayList<>();
        colorProvider = new ColorProvider.ColorProviderImpl();
        lostBlackPawns = 0;
        lostWhitePawns = 0;
        player = gameViewImpl.menuUtils.getPlayer();
    }

    public JPanel initGamePanel(Board.Board board){
        this.board = board;
        gamePanel = viewFactory.createGamePanel();
        initNorthPanel();
        initSouthPanel();
        initLeftRightPanel();
        initBoard();
        setPawns(board.cells());

        gamePanel.add(northPanel);

        boardPlusColumns = viewFactory.createBoardPlusColumnsPanel();

        boardPlusColumns.add(leftPanel);

        boardPlusColumns.add(boardPanel);

        boardPlusColumns.add(rightPanel);

        gamePanel.add(boardPlusColumns);

        gamePanel.add(southPanel);

        return gamePanel;
    }

    private void initBoard(){
        boardPanel = viewFactory.createBoardPanel();
        GridBagLayout layout = new GridBagLayout();
        boardPanel.setLayout(layout);
        GridBagConstraints lim = new GridBagConstraints();
        for (int i = 1; i <= gameViewImpl.getDimension(); i++) {
            for (int j = 1; j<= gameViewImpl.getDimension(); j++) {
                Pair<Int> coordinate = new PairImpl(i,j);
                JButton cell= cellChoice(coordinate);
                cell.addActionListener(e -> actionCell(cell));
                lim.gridx=j;
                lim.gridy=i;
                layout.setConstraints(cell,lim);
                cells.put(coordinate,cell);
                boardPanel.add(cell);
            }
        }
    };

    public void actionCell(JButton cell) {
        if(cell.getComponents().length > 0 && possibleMoves.isEmpty()){
            selectedCell = Optional.of(getCoordinate(cell));
            moveRequest(getCoordinate(cell));
        } else if(!possibleMoves.isEmpty() &&  !possibleMoves.contains(getCoordinate(cell))) {
            possibleMoves.forEach((c -> {
                setColorBackground(c, colorProvider.getNormalCellColor());
            }));
            deselectCell();
        }else if(possibleMoves.contains(getCoordinate(cell)) && selectedCell.isPresent()){
            Pair<Int> coordinateStart = selectedCell.get();
            Pair<Int> coordinateArrival = getCoordinate(cell);
            moveAndPaint(coordinateStart, coordinateArrival);
        }
    }

    private void moveAndPaint(Pair<Int> coordStart, Pair<Int> coordArr) {
         gameViewImpl.setMove(coordStart, coordArr);
        /*addLostPawns(tuple);
        Board.Board board = (Board.Board) tuple._1();
        setPawns(board.cells());
        boardPanel.repaint();
        possibleMoves.forEach((c -> {
            setColorBackground(c, colorProvider.getNormalCellColor());
        }));
        deselectCell();
        boardPanel.validate();
        gamePanel.validate();
        switchPlayer();*/

    }
    private List<Pair<Integer>> findSpecialCell(List<Pair<Integer>> cellList){
        return cellList.stream().filter(cell->(isCornerCell(cell)|| isCenterCell(cell))).collect(Collectors.toList());
    }

    private void switchPlayer() {
        player = Player.Black() == player ? Player.White(): Player.Black();
    }

    private void addLostPawns(Tuple3 tuple) {
        int length = Player.Black() == player ? (int)tuple._2(): (int)tuple._3();
        JPanel panel = Player.Black() == player ? leftPanel : rightPanel;
        JLabel pawn = Player.Black() == player ? viewFactory.createLostBlackPawn() : viewFactory.createLostWhitePawn();
        panel.removeAll();

        for(int i = 0; i < length; i++) {
            panel.add(pawn);
        }

     /*   if(player == Player.Black()){
            leftPanel.removeAll();
            for(int i = 0; i < (int)tuple._2(); i++) {
                leftPanel.add(viewFactory.createLostBlackPawn());
            }
            leftPanel.repaint();
            leftPanel.validate();
        } else if(player == Player.White()) {
            rightPanel.removeAll();
            for(int i = 0; i < (int)tuple._3(); i++) {
                rightPanel.add(viewFactory.createLostWhitePawn());
            }
            rightPanel.repaint();
            rightPanel.validate();
        }*/
    }

    private Pair getCoordinate(JButton cell) {
        Pair<Int> cord= null;
        for(Map.Entry<Pair<Int>,JButton> entry : cells.entrySet()){
            if(entry.getValue().equals(cell)){
                cord= entry.getKey();
            }
        }
        return cord;
    }

    public void moveRequest(Pair<Integer> coord) {
        possibleMoves = gameViewImpl.getPossibleMoves(coord);
        possibleMoves.forEach((c -> {
            cells.get(c).setBackground(colorProvider.getPossibleMovesColor());
        }));
    }

    public void setColorBackground(Pair<Integer> c, Color color){
        if(isCenterCell(c) || isCornerCell(c)){
            cells.get(c).setBackground(colorProvider.getSpecialCellColor());
        }
        else{
            cells.get(c).setBackground(color);
        }
    }

    public void deselectCell(){
        selectedCell = Optional.empty();
        possibleMoves.clear();
    }

    private void setPawns(List<Board.BoardCell> positions) {
        for(Board.BoardCell p : positions) {
            if (cells.get(p.getCoordinate()).getComponentCount() > 0) {
                cells.get(p.getCoordinate()).removeAll();
            }
            pawnChoice(p);
        }
    }

    private void pawnChoice(Board.BoardCell c) {
        Enumeration.Value piece = c.getPiece();
        if(piece.equals(PieceEnum.WhitePawn())){
            cells.get(c.getCoordinate()).add(viewFactory.createWhitePawn());
        }
        else if(piece.equals(PieceEnum.BlackPawn())) {
            cells.get(c.getCoordinate()).add(viewFactory.createBlackPawn());
        }
        else if(piece.equals(PieceEnum.WhiteKing())) {
            cells.get(c.getCoordinate()).add(viewFactory.createKingPawn());
        }
    }

    public void updateMove(utils.Board board, int nBlackCaptured, int nWhiteCaptured){
        Tuple3 tuple = new Tuple3(board,nBlackCaptured,nWhiteCaptured);
        addLostPawns(tuple);
        Board.Board currentBoard= (Board.Board) tuple._1();
        setPawns(currentBoard.cells());
        boardPanel.repaint();
        possibleMoves.forEach((c -> {
            setColorBackground(c, colorProvider.getNormalCellColor());
        }));
        deselectCell();
        boardPanel.validate();
        gamePanel.validate();
        switchPlayer();


    }

    public void setEndGame(Player winner, List<Pair> coordKing){
        System.out.println("The winner is " + winner + " and King is " + coordKing);

    }

    private void initNorthPanel(){
        northPanel=viewFactory.createTopBottomPanel();
        GridBagLayout layout = new GridBagLayout();
        northPanel.setLayout(layout);
        GridBagConstraints lim = new GridBagConstraints();

        menuButton=viewFactory.createGameButton("");
        menuButton.addActionListener(e-> gameViewImpl.showOverlay(gamePanel, gameViewImpl.inGameMenuPanel));

        lim.gridx=0;
        lim.gridy=0;
        lim.weightx=1;
        lim.fill = GridBagConstraints.NONE;
        lim.anchor=GridBagConstraints.LINE_END;

        northPanel.add(menuButton, lim);
    }
    private void initSouthPanel(){
        southPanel=viewFactory.createTopBottomPanel();
    }

    private JButton cellChoice(Pair c){
        if(isCornerCell(c)) {
            return viewFactory.createCornerCell(gameViewImpl.dimension);
        }
        if(isCenterCell(c)){
            return viewFactory.createCenterCell(gameViewImpl.dimension);
        }
        return viewFactory.createNormalCell(gameViewImpl.dimension);

    }

    private void initLeftRightPanel() {
        leftPanel=viewFactory.createLeftRightPanel(1, gameViewImpl.dimension);
        rightPanel=viewFactory.createLeftRightPanel(1, gameViewImpl.dimension);

    }

    public void restoreGame() {
        cells.clear();
        gamePanel.removeAll();
    }

    private boolean isCornerCell(Pair<Integer> c) {
        return c.getX() == 1 && c.getY() == 1 ||
                c.getX() == 1 && c.getY() == gameViewImpl.dimension ||
                c.getX() == gameViewImpl.dimension && c.getY() == 1 ||
                c.getX() == gameViewImpl.dimension && c.getY() == gameViewImpl.dimension;
    }

    private boolean isCenterCell(Pair<Integer> c) {
        return c.getX() == gameViewImpl.dimension/2 + 1  && c.getY() == gameViewImpl.dimension/2 + 1;
    }
}
