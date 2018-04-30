import scala.io.Source
import scala.util.control.Breaks._

class sudoku {
  def print_board(board): Unit = {
    for (x <- 0 to 9) {
      for (y <- 0 to 9) {
        print (board(x)(y))
      }
      println()
    }
  }

  def empty_spot(board, spot): Unit = {
    for (x <- 0 to 9) {
      for (y <- 0 to 9) {
        if (board(x)(y) == 0) {
          spot(0) = x
          spot(1) = y
          return true
        }
      }
    }
    return false
  }

  def check_row(board, row, num): Unit = {
    for (x <- 0 to 9) {
      if (board(row)(x) == num) {
        return true
      }
    }
    return false
  }

  def check_col(board, col, num): Unit = {
    for (y <- 0 to 9) {
      if (board(y)(col) == num) {
        return true
      }
    }
    return false
  }

  def check_box(board, row, col, num): Unit = {
    for (x <- 0 to 3) {
      for (y <- 0 to 3) {
        if (board(x+row)(y+col) == num) {
          return true
        }
      }
    }
    return false
  }

  def is_valid_loc(board, row, col, num): Unit = {
    if (!check_row(board, row, num) && !check_col(board, col, num)
      && !check_box(board, row, col, num)) {
      return true
    }
    else {
      return false
    }
  }

  def solve(board): Unit = {

  }
}

// Can function as main. Checker functions are implemented, just need to do the actual solving
object Sudoku extends App {
  def readInput(puzzle: Array[Array[Int]]) : Array[Array[Int]] = {
    //Reading sudoku board from file
    scala.io.StdIn.readLine("Input board into 'board.txt' file. Press Enter when done.");
    val inFile = Source.fromFile("board.txt");
    var fileLine = 0;
    var badLineLength = false;
    
    //Check if sudoku lines are the right length, put them into multi-dimensional array
    for (line <- inFile.getLines()) {
      if (line.length() != 9) {
        print("One of the lines is the wrong length. Starting over...");
        badLineLength = true;
        break;
      }
      var lineArr = line.split("");
      try {
        var numArr = new Array[Int](9);
        for (j <- 0 to 8) {
          var newNum = lineArr(j).toInt;
          numArr(j) = newNum;
        }
        puzzle(fileLine) = numArr;
        fileLine = fileLine + 1;
      } catch {
        case nfe: NumberFormatException => {
          println("You didn't enter all numbers! Now we have to start over!");

        }
      }
    }
    inFile.close();
    return puzzle;
  }


  def checkIfValid(array: Array[Array[Int]]): Boolean = {
    var invalidVal = false;
    for (i <- 0 to 8) {
      var usedNums = new Array[Int](9);
      for (j <- 0 to 8) {
        var currVal = array(i)(j);
        if (currVal != 0 && usedNums.contains(currVal)) {
          println("Invalid value in row " + (i + 1) + ", column " + (j + 1));
          invalidVal = true;
        } else {
          usedNums(j) = currVal;
        }
      }
    }
    for (j <- 0 to 8) {
      var usedNums = new Array[Int](9);
      for (i <- 0 to 8) {
        var currVal = array(i)(j);
        if (currVal != 0 && usedNums.contains(currVal)) {
          println("Invalid value in row " + (i + 1) + ", column " + (j + 1));
          invalidVal = true;
        } else {
          usedNums(j) = currVal;
        }
      }
    }
    return invalidVal;
  }

  def solvePuzzle(): Unit = {
    var line = "";
    var puzzle = Array.ofDim[Int](9, 9);
    println("You will be entering the values of the unfinished sudoku puzzle, line by line. Enter" +
      " in a zero (0) character to\n signify an empty space.");
    puzzle = readInput(puzzle);

    if (checkIfValid(puzzle)) {
      println("That was an invalid puzzle, try again!");
      solvePuzzle();
    } else {
      // TODO: solve(puzzle);
    }
  }

  solvePuzzle();
}
