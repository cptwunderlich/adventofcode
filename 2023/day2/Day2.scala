import scala.io.StdIn.readLine

enum Color:
  case Red, Green, Blue

case class Game(id: Int, hands: Map[Color, List[Int]])
case class GameMax(id: Int, red: Int, green: Int, blue: Int)

@main def main =
    val (p1Games, p2Games) =
        Iterator.continually(readLine())
            .takeWhile(s => s != null && !s.isEmpty())
            .map(parseGame)
            .map(gameMax)
            .duplicate

    val part1 = p1Games.filter(possible).map(_.id).sum
    val part2 = p2Games.map(g => g.red * g.green * g.blue).sum

    println(s"Part1: $part1\nPart2: $part2")

def parseGame(s: String): Game =
    def hand(s: String): Array[(Int, Color)] =
        s.split(',').map: x =>
            val (cnt, rest) = x.strip() match
                                case s"$cnt $color" => (cnt, color)
            val color =
                rest match
                    case "red"   => Color.Red
                    case "green" => Color.Green
                    case "blue"  => Color.Blue
            (cnt.toInt, color)

    def hands(s: String): Map[Color, List[Int]] =
        s.split(';').map(hand).flatten.toList.groupMap(_._2)(_._1)

    s match
        case s"Game $id: $rest" => Game(id.toInt, hands(rest))

def gameMax(game: Game): GameMax =
    GameMax(game.id,
        game.hands(Color.Red).max,
        game.hands(Color.Green).max,
        game.hands(Color.Blue).max)

def possible(g: GameMax): Boolean =
       g.red   <= 12
    && g.green <= 13
    && g.blue  <= 14
