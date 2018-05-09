import scala.io.Source

object sudoku {
  def print_board(board: Array[Array[Int]]) {
    (0 to 8).foreach(x => {
      (0 to 8).foreach(y => print(board(x)(y)))
      println()
    })
  }

  def empty_spot(board: Array[Array[Int]]): (Boolean, Array[Int]) = {
    (0 to 8).foreach(x => (0 to 8).foreach(y => if (board(x)(y) == 0) return (true, Array(x, y))))
    return (false,Array(0,0))
  }

  def check_row(board: Array[Array[Int]], row: Int, num: Int): Boolean = {
    (0 to 8).foreach(x => if (board(row)(x) == num) return true)
    return false
  }

  def check_col(board: Array[Array[Int]], col: Int, num: Int): Boolean = {
    (0 to 8).foreach(y => if (board(y)(col) == num) return true)
    return false
  }

  def check_box(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
    (0 to 2).foreach(x => (0 to 2).foreach(y => if (board(x + row)(y + col) == num) return true))
    return false
  }

  def is_valid_loc(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
    val row_taken: Boolean = check_row(board,row,num)
    val col_taken: Boolean = check_col(board,col,num)
    val box_taken: Boolean = check_box(board,row-row%3,col-col%3,num)

    return !row_taken && !col_taken && ! box_taken
  }

  def solve(board: Array[Array[Int]]): Boolean = {
    val (spot_open:Boolean, loc:Array[Int]) = empty_spot(board)

    if (spot_open == false) {
      return true
    }

    val row:Int = loc(0)
    val col:Int = loc(1)

    (1 to 9).foreach(num => if (is_valid_loc(board,row,col,num) == true) {
      board(row)(col) = num
      if (solve(board) == true) return true
      board(row)(col) = 0
    })
    return false
  }

  def main(args: Array[String]) {
    var board = Array.ofDim[Int](9, 9);

    scala.io.StdIn.readLine("Input board into 'board.txt' file. Press Enter when done.");
    val inFile = Source.fromFile("board.txt");
    var fileLine = 0;
    var badLineLength = false;

    //Check if sudoku lines are the right length, put them into multi-dimensional array
    for (line <- inFile.getLines()) {
      if (line.length() != 9) {
        throw new Exception("One of the lines is the wrong length. Exiting program...");
      }
      var lineArr = line.split("");
      try {
        var numArr = new Array[Int](9);
        for (j <- 0 to 8) {
          var newNum = lineArr(j).toInt;
          numArr(j) = newNum;
        }
        board(fileLine) = numArr;
        fileLine = fileLine + 1;
      } catch {
        case nfe: NumberFormatException => {
          println("You didn't enter all numbers! Exiting program...");
        }
      }
    }
    inFile.close();

    if(solve(board) == true) {
      println("The answer is:")
      print_board(board)
    } else {
      println("No solution")
    }
  }
}
