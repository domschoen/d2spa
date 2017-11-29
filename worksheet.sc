// Example: List (3,4,5,7) =>
// List(Range(0, 1, 2), Range(3, 4, 5, 6), Range(7, 8, 9, 10, 11), Range(12, 13, 14, 15, 16, 17, 18))


val partitions = List(3,4,5,7)

val sumUntil = partitions.scanLeft(0)(_ + _)
// List(0, 3, 7, 12, 19)

val rangeTuples = sumUntil zip sumUntil.tail
// List((0,3), (3,7), (7,12), (12,19))

val result = rangeTuples.map(x => x._1 until x._2)

val fireRuleArguments = List("entity","task","propertyKey","key")
val fireRuleValues = List(Some("entity"),Some("task"),None,Some("key"))

val zipe = fireRuleArguments zip fireRuleValues
val zipee = zipe.filter(x => !x._2.isEmpty).map(x => (x._1, x._2.get))

val componentName = "ERD2WQueryToOneField"

componentName match {
  case "ERD2WQueryStringOperator" => "A"
  case "ERD2WQueryToOneField" => "B"
  case _ => "Component not found: " + componentName
}

val map = Map ("x" -> 2, "y" -> 3)
val map2 = Map ("z" -> 2, "y" -> 4)
val res = map ++ map2

case class Task(task: String, name: String)
val tasks = List(Task("edit", "Edit"), Task("list", "List")).toSeq
val editTask = tasks.filter(t => t.task.equals("edit")).head