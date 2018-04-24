//populate table
def fillTable (arg: Array[Array[Int]]) {
	for (i <- 0 until arr.length; j <- 0 until arr(0).length) {
		println("Enter value:")
		arr(i)(j) = scala.io.StdIn.readInt()
	}
}

//print table
def printTable (arg: Array[Array[Int]]) {
	for {
	  a <- 0 until row
	  b <- 0 until col
	} println(s"arr($a)($b) = ${arr(a)(b)}")
}

//create table
println("Enter the number of rows:")
val row = scala.io.StdIn.readInt()
println("Enter the number of columns:")
val col = scala.io.StdIn.readInt()
val arr = Array.ofDim[Int](row,col)

fillTable(arr)
printTable(arr)