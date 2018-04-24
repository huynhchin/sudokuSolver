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
