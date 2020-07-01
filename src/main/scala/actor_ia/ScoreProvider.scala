package actor_ia



object ScoreProvider {

   def WhiteWinScore: Int = 1000

   def BlackWinScore: Int = 1000

   def KingCatchableInOne: Int = 900

   def KingNearCornerScore: Int = 900

   def KingOnThroneScore: Int = 0

   def KingDistanceToCornerDividend: Int = 150

   def TowerCoefficient: Double = 1.25

   def ErroneousBarricade: Int = 20

   def WhitePawnOuterCordon: Int = 10

   def RowOrColumnOwnerScore: Int = 10

   def KingRowOrColumnOwnerScore: Int = 90

   def BlackNearKing: Int = 30

   def CordonPawn: Int = 5

   def BlackDiagonalToKing: Int = 25

   def BlackCapturedScore: Int = 10

   def WhiteCapturedScore: Int = 15

}
