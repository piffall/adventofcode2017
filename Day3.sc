val input = 312051

// Case class to hold the coordinates in the spiral/grid
case class Coordinate(x: Int, y: Int) {

  def move(c: Coordinate): Coordinate =
    this.copy(x = this.x + c.x, y = this.y + c.y)

  def distance(): Int =
    math.abs(this.x) + math.abs(this.y)

}

// Start
val start = Coordinate(0, 0)

// List of movements
val up    = Coordinate(0, 1)
val down  = Coordinate(0, -1)
val right = Coordinate(1, 0)
val left  = Coordinate(-1, 0)

// Ordered sequence of movements
val movement = List(List(right, up), List(left, down))

def getNextPairOfMovements(pass: Int): List[Coordinate] = {
  movement((pass + 1) % 2)
}

def movementsGenerator(pass: Int): Seq[Coordinate] = {
  val movements = getNextPairOfMovements(pass)
  for {
    m <- movements
    _ <- 1 to pass
  } yield m
}

def getCoordinates(target: Int): Coordinate = {
  def getCoordinates(acc: Int, pass: Int, coord: Coordinate, movements: List[Coordinate]): Coordinate = {
    if (acc >= target) {
      coord
    } else {
      movements match {
        case Nil      => {
          val movements = movementsGenerator(pass + 1).toList
          getCoordinates(acc + 1, pass + 1, coord.move(movements.head), movements.tail)
        }
        case x :: xs  => getCoordinates(acc + 1, pass, coord.move(x), xs)
      }

    }
  }
  getCoordinates(1, 1, start, movementsGenerator(1).toList)
}

// Part 1
getCoordinates(input).distance()
