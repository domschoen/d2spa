package d2spa.shared

object SharedTestData {
  val productEntity = EOEntity (
    "Product",
    List ("id"), // pkAttributeNames
    List ("id", "descr", "name", "num"), //attributes
    List (
      EORelationship (
        List (EOJoin("id", "productID")),
        "customerProducts", // name
        None, // Definition
        true, // isToMany
        "CustomerProduct" // Destination Entity
      )
    )
  )
  val customerProductEntity = EOEntity (
    "CustomerProduct",
    List ("customerID", "productID"), // pkAttributeNames
    List ("customerID", "productID"), //attributes
    List (
      EORelationship (
        List (EOJoin("productID", "id")),
        "product", // name
        None, // Definition
        false, // isToMany
        "Product" // Destination Entity
      )
    )
  )


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
    d2spa.shared.SharedTestData.productEntity,
    d2spa.shared.SharedTestData.customerProductEntity,
    d2spa.shared.SharedTestData.customerEntity,
    d2spa.shared.SharedTestData.projectEntity
  )
  val eomodel = EOModel(entities)

}
