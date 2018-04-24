//create table
println("Enter the number of rows:")
val row = scala.io.StdIn.readInt()
println("Enter the number of columns:")
val col = scala.io.StdIn.readInt()
val arr = Array.ofDim[Int](row,col)

//populate table
for (i <- 0 until arr.length; j <- 0 until arr(0).length) {
	println("Enter value:")
	arr(i)(j) = scala.io.StdIn.readInt()
}

//print table
for {
  a <- 0 until row
  b <- 0 until col
} println(s"($a)($b) = ${arr(a)(b)}")