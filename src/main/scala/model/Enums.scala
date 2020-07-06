package model

/**
  * Defines Enumeration for the game mode.
  */
object GameMode extends Enumeration {
  type GameMode = Value
  val PVP: GameMode.Value = Value("PVP")
  val PVE: GameMode.Value = Value("PVE")
}

/**
  * Defines Enumeration for the player type.
  */
object Player extends Enumeration {
  type Player = Value
  val White: Value = Value("w")
  val Black: Value = Value("b")
  val None: Value = Value("n")
  val Draw: Value = Value("d")

  class ExtendedStringPlayer(player: Player) {
    def extendedString: String = player match {
      case Player.White => "White"
      case Player.Black => "Black"
      case Player.None => "None"
      case _ => "Draw"
    }
  }

  implicit def getExtendedString(player: Player): ExtendedStringPlayer = new ExtendedStringPlayer(player)
}

/**
  * Defines Enumeration for the IA difficulty.
  */
object Level extends Enumeration {
  case class Val(difficulty: String, depth: Int) extends super.Val
  val Newcomer: Val = Val("Newcomer", 1)
  val Amateur: Val = Val("Amateur", 2)
  val Standard: Val = Val("Standard", 3)
  val Advanced: Val = Val("Advanced", 4)
}

/**
  * Defines Enumeration for the game variant.
  */
object GameVariant extends Enumeration {
  type GameVariant = Value
  val Hnefatafl: Value = Value("Hnefatafl")
  val Tawlbwrdd: Value = Value("Tawlbwrdd")
  val Tablut: Value = Value("Tablut")
  val Brandubh: Value = Value("Brandubh")

  class GameVariantBoardSize(gameVariant: GameVariant) {
    def boardSize: Int = gameVariant match {
      case GameVariant.Tablut => 9
      case GameVariant.Brandubh => 7
      case _ => 11
    }
  }

  implicit def getBoardSize(gameVariant: GameVariant): GameVariantBoardSize = new GameVariantBoardSize(gameVariant)
}

/**
  * Defines Enumeration for the piece in each cell.
  */
object Piece extends Enumeration {
  type Piece = Value
  val WhitePawn: Value = Value("w")
  val BlackPawn: Value = Value("b")
  val WhiteKing: Value = Value("k")
  val Empty: Value = Value("e")
}

/**
  * Defines Enumeration for previous, next, first or last snapshot.
  */
object Snapshot extends Enumeration {
  type Snapshot = Value
  val Previous, Next, First, Last = Value
}

/**
 * Defines Enumeration for Maximize or Minimize.
 */
object MaxMin extends Enumeration {
  type MaxMin = Value
  val Max, min = Value
}

