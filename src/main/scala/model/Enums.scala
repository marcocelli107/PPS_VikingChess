package model

/**
  * Defines game's rules path.
  */
object TheoryGame extends Enumeration {
  val GameRules: TheoryGame.Value = Value("src/main/scala/model/gameRules.pl")
}

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
  case class Val(playerString: String) extends super.Val
  val White: Val = Val("w")
  val Black: Val = Val("b")
  val None: Val = Val("n")
  val Draw: Val = Val("d")
}

/**
  * Defines Enumeration for the IA.
  */
object Level extends Enumeration {
  val Newcomer: Level.Value = Value("Newcomer")
  val Amateur: Level.Value = Value("Amateur")
  val Standard: Level.Value = Value("Standard")
  val Advanced: Level.Value = Value("Advanced")
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
  case class Val(pieceString: String) extends super.Val
  val WhitePawn: Val = Val("wp")
  val BlackPawn: Val = Val("bp")
  val WhiteKing : Val= Val("wk")
  val Empty: Val = Val("e")
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