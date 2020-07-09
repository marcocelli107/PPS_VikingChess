package ia.evaluation_function

object ScoreProvider {

   def WhiteWin: Int = 1000

   def BlackWin: Int = 1000

   def PossibleWinInOne: Int = 900

   def KingCatchableInOne: Int = 900

   def KingEscapeNearCorner: Int = 850

   def KingEscapeToCorner: Int = 900

   def KingDistanceToCornerDividend: Int = 900

   def Draw: Int = 0

   def KingOnThrone: Int = 0

   def TowerCoefficient: Double = 3

   def WhitePawnOuterCordon: Int = 30

   def FirstLastRowOrColumnOwnedByBlack: Int = 10

   def FirstLastRowOrColumnOwnedByWhite: Int = 20

   def FirstLastRowOrColumnOwnedByKing: Int = 200

   def RowOrColumnOwnedKing: Int = 50

   def RowAndColumnOwnedKing: Int = 100

   def BlackNearKing: Int = 50

   def PawnInCordon: Int = 5

   def WhiteInsideCordon: Int = 10

   def RightCordon: Int = 100

   def WrongCordon: Int = 50

   def WrongBarricade: Int = 50

   def BlackOnDiagonalKing: Int = 25

   def BlackCaptured: Int = 20

   def WhiteCaptured: Int = 50

   def LastBlackMovedCatchableInOne: Int = 100

   def LastWhiteMovedCatchableInOne: Int = 150
}
