package view;

import model.PieceEnum;
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


public class Game {

    GameViewImpl gameViewImpl;

    public JPanel gamePanel,northPanel,southPanel,boardPanel,boardPlusColumns, leftPanel, rightPanel;
    private JButton menuButton;
    private HashMap<Pair<Int>, JButton> cells;
    private ViewFactory viewFactory;
    private List<Pair<Int>> possibleMoves;
    private Optional<Pair> selectedCell = Optional.empty();
    private Board.Board board;
    private ColorProvider colorProvider;



    public Game(ViewFactory scalaViewFactory, GameViewImpl GVImpl){
        this.viewFactory = scalaViewFactory;
        gameViewImpl = GVImpl;
        cells = new HashMap<>();
        possibleMoves = new ArrayList<>();
        colorProvider = new ColorProvider.ColorProviderImpl();

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
            setColorBackground(colorProvider.getNormalCellColor());
            deselectCell();
        }else if(possibleMoves.contains(getCoordinate(cell)) && selectedCell.isPresent()){
            Pair<Int> coordinateStart = selectedCell.get();
            Pair<Int> coordinateArrival = getCoordinate(cell);
            moveAndPaint(coordinateStart, coordinateArrival);
        }
    }

    private void moveAndPaint(Pair<Int> coordStart, Pair<Int> coordArr) {
        Tuple3 tuple = gameViewImpl.setMove(coordStart, coordArr);
        Board.Board board = (Board.Board) tuple._1();
        setPawns(board.cells());
        boardPanel.repaint();
        setColorBackground(colorProvider.getNormalCellColor());
        deselectCell();
        boardPanel.validate();

        //if(gameViewImpl.menuUtils.player)
        rightPanel.add(viewFactory.createLostWhitePawn());
        rightPanel.validate();
        gamePanel.validate();

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


    public void moveRequest(Pair<Int> coord) {
        possibleMoves = gameViewImpl.getPossibleMoves(coord);
        setColorBackground(colorProvider.getPossibleMovesColor());
    }


    public void setColorBackground(Color color){
        possibleMoves.forEach((c -> {
            cells.get(c).setBackground(color);
        }));
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
