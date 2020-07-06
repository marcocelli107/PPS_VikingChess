package model

/**
  * Defines Enumeration for the game mode.
  */
object GameMode extends Enumeration {
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
  case class Val(nameVariant: String, size: Int) extends super.Val
  val Hnefatafl: Val = Val("Hnefatafl", 11)
  val Tawlbwrdd: Val = Val("Tawlbwrdd", 11)
  val Tablut: Val = Val("Tablut", 9)
  val Brandubh: Val = Val("Brandubh", 7)
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
  type SnapshotType = Value
  val Previous, Next, First, Last = Value
}

/**
 * Defines Enumeration for Maximize or Minimize.
 */
object MaxMin extends Enumeration {
  type MaxMin = Value
  val Max, min = Value
}

