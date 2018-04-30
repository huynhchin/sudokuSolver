import scala.io.Source
import scala.util.control.Breaks._

object Sudoku extends App {
  def readInput(puzzle: Array[Array[Int]]) : Array[Array[Int]] = {
    //Reading sudoku board from file
    scala.io.StdIn.readLine("Input board into 'board.txt' file. Press Enter when done.");
    val inFile = Source.fromFile("board.txt");
    var fileLine = 0;
    var badLineLength = false;
    for (line <- inFile.getLines()) {
      // if line is the wrong length, say invalid input
      // split line and convert it into numbers, storing them in an array


      // old code
      //    var line = scala.io.StdIn.readLine("Enter line number " + (i + 1) + " of the sudoku.");
      //    while (line.length() != 9) {
      //      println("That line is the wrong length!");
      //      line = scala.io.StdIn.readLine("Enter line number " + (i + 1) + " of the sudoku.");
      //    }

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
        //      if (i < 8) {
        //        readInput(i + 1, puzzle);
        //      }
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
