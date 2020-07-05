package actor_ia



object ScoreProvider {

   def WhiteWinScore: Int = 1000

   def BlackWinScore: Int = 1000

   def KingCatchableInOne: Int = 900

   def KingNearCornerScore: Int = 900

   def KingToCornerScore: Int = 900

   def DrawScore: Int = 0

   def KingOnThroneScore: Int = 0

   def KingDistanceToCornerDividend: Int = 150

   def TowerCoefficient: Double = 1.25

   def RightBarricade: Int = 20

   def WhitePawnOuterCordon: Int = 10

   def BlackFirstLastRowOrColumnOwnerScore: Int = 5

   def WhiteFirstLastRowOrColumnOwnerScore: Int = 10

   def KingFirstLastRowOrColumnOwnerScore: Int = 90

   def KingRowOrColumnOwnerScore: Int = 25

   def KingRowAndColumnOwnerScore: Int = 50

   def BlackNearKing: Int = 30

   def CordonPawn: Int = 5

   def RightCordon: Int = 30

   def WrongCordon: Int = 30

   def WrongBarricade: Int = 20

   def BlackDiagonalToKing: Int = 25

   def BlackCapturedScore: Int = 20

   def WhiteCapturedScore: Int = 50

}
