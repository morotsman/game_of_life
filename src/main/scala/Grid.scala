package game_of_life

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import scala.swing.Panel
import java.awt.{ Graphics2D, Color }
import javax.swing.Timer

object SimpleGUI extends SimpleSwingApplication {

  def top = new MainFrame { // top is a required method
    title = "Game of life"

    // declare Components here
    val stopButton = new Button {
      text = "Stop"
      borderPainted = true
      enabled = true
      tooltip = "Stop animation"
    }
    
    val startButton = new Button {
      text = "Start"
      borderPainted = true
      enabled = true
      tooltip = "Start animation"
    }   
      
    
    val clearButton = new Button {
      text = "Clear"
      borderPainted = true
      enabled = true
      tooltip = "Clear grid"
    }      

    val gridPanel = new GridPanel(1, 2) {
      contents += stopButton
      contents += startButton
      contents += clearButton
    }
    val gridDimension = GridDimension(70,70);
    
    val grid = new Grid(gridDimension) {
      preferredSize = new Dimension(100, 100)
    }


    // choose a top-level Panel and put components in it
    // Components may include other Panels
    contents = new BorderPanel {
      layout(gridPanel) = North
      layout(grid) = Center
    }
    size = new Dimension(500, 522)

    
    var positionList:List[Pos] = List();
    var gameOfLife = Game(positionList)
    
    
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e : java.awt.event.ActionEvent) = {
        positionList = gameOfLife.head; 
        grid.setCells(positionList)
        gameOfLife = gameOfLife.tail;
      }
    }
    val timer: Timer = new Timer(1000, timeOut)
    
   
    listenTo(grid.mouse.clicks)
    listenTo(startButton)
    listenTo(stopButton)
    listenTo(clearButton)
    reactions += {
      case MouseClicked(_, point, _, _, _) =>
        timer.stop()
        val cellWidth = grid.size.getWidth/gridDimension.columns
        val cellHeight = grid.size.getHeight/gridDimension.rows
        positionList = Pos(Math.floor(point.x/cellWidth).toInt,Math.floor(point.y/cellHeight).toInt)::positionList   
        grid.setCells(positionList)
      case ButtonClicked(component) if component == startButton =>
        gameOfLife = Game(positionList)
        timer.start()
      case ButtonClicked(component) if component == stopButton =>
        timer.stop()  
      case ButtonClicked(component) if component == clearButton =>
        timer.stop() 
        positionList  = List();
        grid.setCells(positionList);
    }
   
   

  }
}

case class GridDimension(rows: Int, columns: Int)


class Grid(dimension: GridDimension) extends Panel{
  
  var cells = List[Pos]()

  override def paintComponent(g: Graphics2D) {
    val cellWidth: Double = size.width.toDouble/dimension.columns
    val cellHeight: Double = size.height.toDouble/dimension.rows
    
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
   
    def getLines(size: Double): IndexedSeq[Double] = {
      for {
        row <- 1 until (dimension.rows+1)
      } yield (row*size)  
    }
    
    
    def getCellCoordinates(cells: List[Pos]): List[(Double,Double)] = {
      for{
        cell <- cells
      }yield(cell.x*cellWidth, cell.y*cellHeight)
    }
    
    

    // Draw background here
    g.drawRect(0, 0, size.width, size.height)
    getLines(cellHeight).foreach(row => g.drawLine(0,Math.round(row).toInt,size.width,Math.round(row).toInt))
    getLines(cellWidth).foreach(column => g.drawLine(Math.round(column).toInt,0,Math.round(column).toInt,size.height))
    
    getCellCoordinates(cells).foreach(p => g.fillRect(Math.round(p._1).toInt,Math.round(p._2).toInt,Math.round(cellWidth+1).toInt, Math.round(cellHeight+1).toInt))
 

  }

  def setCells(generation: List[Pos]){
    cells = generation;
    repaint();
  }

}

