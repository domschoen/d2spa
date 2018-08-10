package d2spa.shared

object SharedTestData {
  val projectEntity = EOEntity (
    "Project",
    List ("id"), // pkAttributeNames
    List ("customerID", "descr", "id", "projectNumber"), //attributes
    List (
      EORelationship (List ("customerID"), "customer", "Customer")
    )
  )
  val customerEntity = EOEntity (
    "Customer",
    List ("id"), // pkAttributeNames
    List ("acronym", "address", "id", "name"), //attributes
    List (
      EORelationship (List ("id"), "projects", "Project")
    )
  )
  val entities: List[EOEntity] = List(
    d2spa.shared.SharedTestData.customerEntity,
    d2spa.shared.SharedTestData.projectEntity
  )
  val eomodel = EOModel(entities)

}
