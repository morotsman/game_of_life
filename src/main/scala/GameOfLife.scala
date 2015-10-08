package game_of_life

  /*
	Any live cell with less than two live neighbours dies, as if caused by under-population.
	Any live cell with two or three live neighbours lives on to the next generation.
	Any live cell with more than three live neighbours dies, as if by overcrowding.
	Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
   */

case class Pos(x: Int, y: Int) 

class Game{
  
  def tick(currentGen: List[Pos]): List[Pos] = {
    
    def neighbours(pos: Pos): IndexedSeq[Pos] = {
      val Pos(xPos, yPos) = pos; 
      for {
        x <- (xPos-1) until (xPos+2)
        y <- (yPos-1) until (yPos+2)
        if pos != Pos(x,y)
      } yield Pos(x,y) 
    }
   
    
    def isAlive(pos: Pos): Boolean = {
      val aliveNeighbours = neighbours(pos).filter(pos => currentGen.contains(pos)).length
      if(currentGen.contains(pos) && (aliveNeighbours==2 || aliveNeighbours==3)){
        true 
      }else if(!currentGen.contains(pos) && aliveNeighbours==3){
        true  
      }else{
        false
      }
    }
    
    def suspects : List[Pos] = {
      val curGen = currentGen.toSet
      ((for {
        m <- curGen
        n <- neighbours(m)
      }yield n) ++ currentGen).toList
    }
    
    def loop(suspects: List[Pos]): List[Pos] = suspects match{
      case Nil => List()
      case x::xs if isAlive(x) => x::loop(xs)
      case x::xs => loop(xs)
    }
    
    loop(suspects)   
  }
  
  def game(initialGen: List[Pos]): Stream[List[Pos]] = {
    
    lazy val myGame: Stream[List[Pos]] = {
       def loop(h: List[Pos]): Stream[List[Pos]] = h #:: loop(tick(h))
       loop(initialGen)
    }
    myGame
  }
}

object Game{
  def apply(initialGen: List[Pos]): Stream[List[Pos]] = new Game().game(initialGen)
}



object GameOfLife {
  

  def main(args: Array[String]): Unit = {
    val blinker = List(Pos(1,0), Pos(1,1), Pos(1,2));
    val gameOfLifeBlinker = Game(blinker)
    gameOfLifeBlinker.take(4).foreach(println)
    
    val toad = List(Pos(1,0), Pos(2,0), Pos(3,0), Pos(0,1), Pos(1,1), Pos(2,1))
    val gameOfLifeToad = Game(toad)
    gameOfLifeToad.take(4).foreach(println)
  }

}