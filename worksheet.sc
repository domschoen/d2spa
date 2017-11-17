// Example: List (3,4,5,7) =>
// List(Range(0, 1, 2), Range(3, 4, 5, 6), Range(7, 8, 9, 10, 11), Range(12, 13, 14, 15, 16, 17, 18))


val partitions = List(3,4,5,7)

val sumUntil = partitions.scanLeft(0)(_ + _)
// List(0, 3, 7, 12, 19)

val rangeTuples = sumUntil zip sumUntil.tail
// List((0,3), (3,7), (7,12), (12,19))

val result = rangeTuples.map(x => x._1 until x._2)
