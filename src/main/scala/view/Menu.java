package view;

import model.GameVariant;
import model.ModeGame;
import model.Player;
import scala.Enumeration;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;

public class Menu {

    private ViewFactory viewFactory;
    private JPanel menuPanel, variantsPanel, diffPanel, playerChoicePanel, inGameMenuPanel;
    private JButton pvpButton,pveButton, exitButtonMenu, hnefatafl, tawlbwrdd, tablut, brandubh,
            newcomer, amateur, standard, advanced, whiteButton, blackButton, quitGame, returnToMenu, returnToGame;
    public GameViewImpl gameViewImpl;
    private int smallerSide;
    public Enumeration.Value boardVariant, player;
    private Enumeration.Value gameMode;



    public Menu(ViewFactory viewFactory, GameViewImpl GVImpl){
        this.viewFactory = viewFactory;
        gameViewImpl = GVImpl;
        smallerSide = this.viewFactory.getSmallerSide();
        gameMode = ModeGame.PVE();
        boardVariant = GameVariant.Hnefatafl();
    }

    public JPanel initMenu() {
        menuPanel = viewFactory.createMenuPanel("Choose Mode: ");
        pveButton = viewFactory.createMenuButton(" Player Vs Computer");
        pveButton.addActionListener(e -> pveVariant());
        pvpButton = viewFactory.createMenuButton("Player Vs Player");
        pvpButton.addActionListener(e -> pvpVariant());
        exitButtonMenu = viewFactory.createMenuButton("Exit");
        exitButtonMenu.addActionListener(e -> System.exit(0));

        menuPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2/100)));
        menuPanel.add(pveButton);
        menuPanel.add(pvpButton);
        menuPanel.add(exitButtonMenu);
        menuPanel.add(Box.createVerticalGlue());

        return menuPanel;
    }

    public void pveVariant() {
        gameMode = ModeGame.PVE();
        showVariant();
    }

    public void pvpVariant(){
        gameMode = ModeGame.PVP();
        showVariant();
    }

    public void showVariant() {
        gameViewImpl.showOverlay(menuPanel, variantsPanel);
    }

    public void setBoardVariant(String variant){

        if(variant == GameVariant.Hnefatafl().nameVariant()){
            boardVariant = GameVariant.Hnefatafl();
        }
        else if(variant == GameVariant.Tawlbwrdd().nameVariant()){
            boardVariant = GameVariant.Tawlbwrdd();
        }
        else if(variant == GameVariant.Tablut().nameVariant()){
            boardVariant = GameVariant.Tablut();
        }
        else if (variant == GameVariant.Brandubh().nameVariant()){
            boardVariant = GameVariant.Brandubh();
        }
    }

    public ActionListener chooseListener(String text){
        ActionListener listener;
        listener = e -> {
            if (gameMode == ModeGame.PVP()) {
                gameViewImpl.showOverlay(variantsPanel, playerChoicePanel);
            } else {
                setBoardVariant(text);
                gameViewImpl.showOverlay(variantsPanel, diffPanel);
            }
        };
        return listener;
    }


    public JPanel initVariantsMenu() {
        variantsPanel = viewFactory.createMenuPanel("Choose Board Variant: ");

        hnefatafl = viewFactory.createMenuButton("Hnefatafl(11 x 11)");
        hnefatafl.addActionListener(chooseListener("Hnefatafl"));

        tawlbwrdd = viewFactory.createMenuButton("Tawlbwrdd(11 x 11)");
        tawlbwrdd.addActionListener(chooseListener("Tawlbwrdd"));

        tablut = viewFactory.createMenuButton("Tablut(9 x 9)");
        tablut.addActionListener(chooseListener("Tablut"));

        brandubh = viewFactory.createMenuButton("Brandubh(7 x 7)");
        brandubh.addActionListener(chooseListener("Brandubh"));

        variantsPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2/100)));
        variantsPanel.add(hnefatafl);
        variantsPanel.add(tawlbwrdd);
        variantsPanel.add(tablut);
        variantsPanel.add(brandubh);
        variantsPanel.add(Box.createVerticalGlue());

        variantsPanel.setVisible(false);

        return variantsPanel;
    }

    public JPanel initDiffMenu() {
        diffPanel = viewFactory.createMenuPanel("Choose Difficulty: ");

        newcomer = viewFactory.createMenuButton("Newcomer");
        newcomer.addActionListener(e -> gameViewImpl.showOverlay(diffPanel, playerChoicePanel));
        amateur = viewFactory.createMenuButton("Amateur");
        amateur.addActionListener(e -> gameViewImpl.showOverlay(diffPanel, playerChoicePanel));

        standard = viewFactory.createMenuButton("Standard");
        standard.addActionListener(e -> gameViewImpl.showOverlay(diffPanel, playerChoicePanel));

        advanced = viewFactory.createMenuButton("Advanced");
        advanced.addActionListener(e -> gameViewImpl.showOverlay(diffPanel, playerChoicePanel));

        diffPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2/100)));
        diffPanel.add(newcomer);
        diffPanel.add(amateur);
        diffPanel.add(standard);
        diffPanel.add(advanced);
        diffPanel.add(Box.createVerticalGlue());

        diffPanel.setVisible(false);

        return diffPanel;
    }

    public JPanel initPlayerChoiceMenu() {
        playerChoicePanel = viewFactory.createMenuPanel("Choose Player: ");

        whiteButton = viewFactory.createMenuButton("White Pawns");
        whiteButton.addActionListener(e -> whitePlayer());
        blackButton = viewFactory.createMenuButton("Black Pawns");
        blackButton.addActionListener(e -> blackPlayer());

        playerChoicePanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2/100)));
        playerChoicePanel.add(whiteButton);
        playerChoicePanel.add(blackButton);
        playerChoicePanel.add(Box.createVerticalGlue());

        playerChoicePanel.setVisible(false);

        return playerChoicePanel;
    }

    private void initOrRestore(){
        gameViewImpl.initOrRestoreGUI();
    }

    public void whitePlayer() {
        player = Player.White();
        initOrRestore();
    }

    public void blackPlayer(){
        player = Player.Black();
        initOrRestore();
    }

    public Enumeration.Value getPlayer() {
        return player;
    }


    public JPanel initInGameMenu(){
        inGameMenuPanel = viewFactory.createMenuPanel("Choose Option: ");

        returnToGame = viewFactory.createMenuButton("Return to Game");
        returnToGame.addActionListener(e -> gameViewImpl.showOverlay(inGameMenuPanel, gameViewImpl.gamePanel));
        returnToMenu = viewFactory.createMenuButton("Leave Match");
        returnToMenu.addActionListener(e -> gameViewImpl.showOverlay(inGameMenuPanel, menuPanel));
        quitGame = viewFactory.createMenuButton("Quit Game");
        quitGame.addActionListener(e -> System.exit(0));


        inGameMenuPanel.add(Box.createRigidArea(new Dimension(smallerSide, smallerSide * 2/100)));
        inGameMenuPanel.add(returnToGame);
        inGameMenuPanel.add(returnToMenu);
        inGameMenuPanel.add(quitGame);
        inGameMenuPanel.add(Box.createVerticalGlue());

        inGameMenuPanel.setVisible(false);

        return inGameMenuPanel;
    }



}
