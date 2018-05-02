class sudoku () {

  def print_board(board: Array[Array[Int]]) {
    for (x <- 0 to 8) {
      (0 to 8).foreach(y => print(board(x)(y)))
      println()
    }
  }

  def empty_spot(board: Array[Array[Int]]): Boolean = {
    for (x <- 0 to 8) {
      for (y <- 0 to 8) {
        if (board(x)(y) == 0) {
          return true
        }
      }
    }
    return false
  }

  def empty_spot_loc(board: Array[Array[Int]]): (Int, Int) = {
    for (x <- 0 to 8) {
      for (y <- 0 to 8) {
        if (board(x)(y) == 0) {
          return (x,y)
        }
      }
    }
    return (0,0)
  }

  def check_row(board: Array[Array[Int]], row: Int, num: Int): Boolean = {
    for (x <- 0 to 8) {
      if (board(row)(x) == num) {
        return true
      }
    }
    return false
  }

  def check_col(board: Array[Array[Int]], col: Int, num: Int): Boolean = {
    for (y <- 0 to 8) {
      if (board(y)(col) == num) {
        return true
      }
    }
    return false
  }

  def check_box(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
    for (x <- 0 to 2) {
      for (y <- 0 to 2) {
        if (board(x+row)(y+col) == num) {
          return true
        }
      }
    }
    return false
  }

  def is_valid_loc(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
    val row_taken: Boolean = check_row(board,row,num)
    val col_taken: Boolean = check_col(board,col,num)
    val box_taken: Boolean = check_box(board,row-row%3,col-col%3,num)

    return !row_taken && !col_taken && ! box_taken
  }

  def solve(board: Array[Array[Int]]): Boolean = {
    val spot_open: Boolean = empty_spot(board)
    if (spot_open == false) {
      return true
    }

    val (row,col) = empty_spot_loc(board)

    for (num <- 0 to 8) {
      val valid: Boolean = is_valid_loc(board,row,col,num)
      if (valid == true) {
        board(row)(col) = num
        val solved: Boolean = solve(board)
        if (solved == true) {
          print_board(board)
          return true
        }
        board(row)(col) = 0
      }
    }
    return false
  }
}

object tester {
  def main(args: Array[String]) {
    val board = Array(Array(3,0,6,5,0,8,4,0,0),
                  Array(5,2,0,0,0,0,0,0,0),
                  Array(0,8,7,0,0,0,0,3,1),
                  Array(0,0,3,0,1,0,0,8,0),
                  Array(9,0,0,8,6,3,0,0,5),
                  Array(0,5,0,0,9,0,6,0,0),
                  Array(1,3,0,0,0,0,2,5,0),
                  Array(0,0,0,0,0,0,0,7,4),
                  Array(0,0,5,2,0,6,3,0,0))

    val board2 = Array(Array(3,1,6,5,7,8,4,9,2),
      Array(5,2,9,1,3,4,7,6,8),
      Array(4,8,7,6,2,9,5,3,1),
      Array(2,6,3,4,1,5,9,8,7),
      Array(9,0,0,8,6,3,0,0,5),
      Array(0,5,0,0,9,0,6,0,0),
      Array(1,3,0,0,0,0,2,5,0),
      Array(0,0,0,0,0,0,0,7,4),
      Array(0,0,5,2,0,6,3,0,0))

    val SodukoSolver = new sudoku()
    SodukoSolver.print_board(board2)
    val solved: Boolean = SodukoSolver.solve(board2)
    if(solved == true) {
      SodukoSolver.print_board(board2)
    } else {
      println("No solution")
    }
  }
}
