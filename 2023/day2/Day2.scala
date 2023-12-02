import scala.io.StdIn.readLine

enum Color:
  case Red, Green, Blue

case class Game(id: Int, hands: Map[Color, Array[Int]])
case class GameMax(id: Int, red: Int, green: Int, blue: Int)

@main def main =
    val gameMaxes = 
        Iterator.continually(readLine())
            .takeWhile(s => s != null && !s.isEmpty())
            .map(parseGame)
            .map(gameMax)
            .toArray

    val part1 = gameMaxes.filter(possible).map(_.id).sum
    val part2 = gameMaxes.map(g => g.red * g.green * g.blue).sum

    println(s"Part1: $part1\nPart2: $part2")

def parseGame(s: String): Game =    
    def number(span: String) = span.dropWhile(!_.isDigit).toInt
    
    def hand(s: String): Array[(Int, Color)] =        
        s.split(',').map: x =>
            val (cnt, rest) = x.strip().span(!_.isSpaceChar)
            val color =
                rest.drop(1) match
                    case "red"   => Color.Red
                    case "green" => Color.Green
                    case "blue"  => Color.Blue
                    case err     => throw new RuntimeException(s"Impossible color: $err")
            (cnt.toInt, color)
            
    def hands(s: String): Map[Color, Array[Int]] =
        s.drop(1).split(';').map(hand).flatten.groupMap(_._2)(_._1)
    
    s.span(_ != ':') match
        case (game, rest) => Game(number(game), hands(rest))
    
def gameMax(game: Game): GameMax =
    GameMax(game.id, 
        game.hands(Color.Red).max, 
        game.hands(Color.Green).max, 
        game.hands(Color.Blue).max)

def possible(g: GameMax): Boolean =
       g.red   <= 12
    && g.green <= 13 
    && g.blue  <= 14
