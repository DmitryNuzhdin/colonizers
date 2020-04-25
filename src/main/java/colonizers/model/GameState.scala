package colonizers.model

case class GameState(
                      players: List[Player],
                      playerStates: Map[Player, PlayerState],
                      history: List[Turn],
                      currentPlayerOpt: Option[Player],
                      gameFieldState: GameFieldState
                    ) {
  def possibleTurns: Turn = ???
}

trait GameField
case class PlayerState(lastTurnCubes: Option[Cube6x6])