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
    val board = Array(Array(3,0,6,5,0,8,4,0,0),
                      Array(5,2,0,0,0,0,0,0,0),
                      Array(0,8,7,0,0,0,0,3,1),
                      Array(0,0,3,0,1,0,0,8,0),
                      Array(9,0,0,8,6,3,0,0,5),
                      Array(0,5,0,0,9,0,6,0,0),
                      Array(1,3,0,0,0,0,2,5,0),
                      Array(0,0,0,0,0,0,0,7,4),
                      Array(0,0,5,2,0,6,3,0,0))


    if(solve(board) == true) {
      print_board(board)
    } else {
      println("No solution")
    }
  }
}
