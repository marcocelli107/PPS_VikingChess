package model.game

/**
  * Defines Enumeration for the game mode.
  */
object GameMode extends Enumeration {
  type GameMode = Value
  val PVP: GameMode.Value = Value("Player vs Player")
  val PVE: GameMode.Value = Value("Player vs Computer")
}

/**
  * Defines Enumeration for the player type.
  */
object Player extends Enumeration {
  type Player = Value
  val White: Value = Value("White")
  val Black: Value = Value("Black")
  val None: Value = Value("None")
  val Draw: Value = Value("Draw")

  case class ParserStringPlayer(player: Player) {
    def parserString: String = player match {
      case Player.White => "w"
      case Player.Black => "b"
      case Player.None => "n"
      case _ => "d"
    }
  }

  /**
    * Parses enum player in prolog player implicitly.
    *
    * @param player
    *               player enum.
    *
    * @return player string
    */
  implicit def getParserString(player: Player): ParserStringPlayer = ParserStringPlayer(player)
}

/**
  * Defines Enumeration for the IA difficulty.
  */
object Level extends Enumeration {
  type Level = Value
  val Newcomer: Value = Value("Newcomer")
  val Standard: Value = Value("Standard")
  val Advanced: Value = Value("Advanced")

  case class LevelDepth(level: Level) {
    def depth: Int = level match {
      case Level.Newcomer => 1
      case _ => 2
    }
  }

  /**
    * Extracts implicitly depth according to difficulty.
    *
    * @param level
    *               level difficulty IA.
    *
    * @return depth to int
    */
  implicit def depth(level: Level): LevelDepth = LevelDepth(level)
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

  case class GameVariantBoardSize(gameVariant: GameVariant) {
    def boardSize: Int = gameVariant match {
      case GameVariant.Tablut => 9
      case GameVariant.Brandubh => 7
      case _ => 11
    }
  }

  /**
    * Extracts implicitly size of board according to variant.
    *
    * @param gameVariant
    *               game's variant.
    *
    * @return size to int
    */
  implicit def getBoardSize(gameVariant: GameVariant): GameVariantBoardSize = GameVariantBoardSize(gameVariant)
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

