package d2spa.shared

object SharedTestData {
  val projectEntity = EOEntity (
    "Project",
    List ("id"), // pkAttributeNames
    List ("customerID", "descr", "id", "projectNumber"), //attributes
    List (
      EORelationship (List (EOJoin("customerID", "id")), "customer", None, false, "Customer")
    )
  )
  val customerEntity = EOEntity (
    "Customer",
    List ("id"), // pkAttributeNames
    List ("acronym", "address", "id", "name"), //attributes
    List (
      EORelationship (List (EOJoin("id","customerID")), "projects", None, true, "Project")
    )
  )
  val entities: List[EOEntity] = List(
    d2spa.shared.SharedTestData.customerEntity,
    d2spa.shared.SharedTestData.projectEntity
  )
  val eomodel = EOModel(entities)

}
