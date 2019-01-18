package d2spa.client.services

import d2spa.client.PageConfiguration.NoPageConfiguration
import d2spa.client.{PageConfigurationRuleResults, PropertyRuleResults}
import d2spa.shared.{D2WContext, RuleResult, RuleValue}

object ClientTestData {


  val ruleCache = Map(
    "Project" -> Map(
      "query" -> Map(
        NoPageConfiguration -> PageConfigurationRuleResults(
          List(
            RuleResult(
              D2WContext(Some("Project"), Some("query"), None, None), "displayNameForProperty", RuleValue(Some("Project"), List())
            ),
            RuleResult(D2WContext(Some("Project"), Some("query"), None, None), "displayPropertyKeys", RuleValue(None, List("customer", "descr", "ProjectNumber")))
          ),
          Map(
            "customer" ->
              PropertyRuleResults(List(
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("customer"), None), "attributeType", RuleValue(Some("eoV"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("customer"), None), "displayNameForProperty", RuleValue(Some("Customer"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("customer"), None), "componentName", RuleValue(Some("ERD2WQueryToOneField"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("customer"), None), "attributeType", RuleValue(Some("eoV"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("customer"), None), "displayNameForProperty", RuleValue(Some("Customer"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("customer"), None), "componentName", RuleValue(Some("ERD2WQueryToOneField"), List()))
              ),
                "stringV"
              ),
            "descr" ->
              PropertyRuleResults(List(
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("descr"), None), "attributeType", RuleValue(Some("stringV"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("descr"), None), "displayNameForProperty", RuleValue(Some("Description"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("descr"), None), "componentName", RuleValue(Some("ERD2WQueryStringOperator"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("descr"), None), "attributeType", RuleValue(Some("stringV"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("descr"), None), "displayNameForProperty", RuleValue(Some("Description"), List())),
                RuleResult(D2WContext(Some("Project"), Some("query"), Some("descr"), None), "componentName", RuleValue(Some("ERD2WQueryStringOperator"), List()))), "stringV"),
            "ProjectNumber" ->
              PropertyRuleResults(
                List(
                  RuleResult(D2WContext(Some("Project"), Some("query"), Some("projectNumber"), None), "attributeType", RuleValue(Some("intV"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("query"), Some("projectNumber"), None), "displayNameForProperty", RuleValue(Some("Project Number"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("query"), Some("projectNumber"), None), "componentName", RuleValue(Some("ERD2WQueryNumberRange"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("query"), Some("projectNumber"), None), "attributeType", RuleValue(Some("intV"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("query"), Some("projectNumber"), None), "displayNameForProperty", RuleValue(Some("Project Number"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("query"), Some("projectNumber"), None), "componentName", RuleValue(Some("ERD2WQueryNumberRange"), List()))
                ),
                "stringV"
              )
          )
        )
      ),
      "edit" -> Map(
        NoPageConfiguration ->
          PageConfigurationRuleResults(
            List(
              RuleResult(D2WContext(Some("Project"), Some("edit"), None, None), "displayNameForEntity", RuleValue(Some("Project"), List())),
              RuleResult(D2WContext(Some("Project"), Some("edit"), None, None), "displayPropertyKeys", RuleValue(None, List("descr", "projectNumber", "customer")))
            ),
            Map(
              "descr" ->
                PropertyRuleResults(List(
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("descr"), None), "attributeType", RuleValue(Some("stringV"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("descr"), None), "displayNameForProperty", RuleValue(Some("Description"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("descr"), None), "componentName", RuleValue(Some("ERD2WEditString"), List()))
                ),
                  "stringV"),
              "projectNumber" ->
                PropertyRuleResults(List(
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("projectNumber"), None), "attributeType", RuleValue(Some("intV"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("projectNumber"), None), "displayNameForProperty", RuleValue(Some("ProjectNumber"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("projectNumber"), None), "componentName", RuleValue(Some("ERD2WEditNumber"), List()))
                ),
                  "stringV"
                ),
              "customer" ->
                PropertyRuleResults(List(
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("customer"), None), "attributeType", RuleValue(Some("eoV"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("customer"), None), "displayNameForProperty", RuleValue(Some("Customer"), List())),
                  RuleResult(D2WContext(Some("Project"), Some("edit"), Some("customer"), None), "componentName", RuleValue(Some("ERD2WEditToOneRelationship"), List()))
                ),
                  "stringV"
                )
            )
          )
      )
    ),
    "Customer" ->
      Map(
        "query" ->
          Map(
            NoPageConfiguration ->
              PageConfigurationRuleResults(
                List(
                  RuleResult(D2WContext(Some("Customer"), Some("query"), None, None), "displayNameForProperty", RuleValue(Some("Customer"), List())),
                  RuleResult(D2WContext(Some("Customer"), Some("query"), None, None), "displayPropertyKeys", RuleValue(None, List("acronym", "address", "name", "projects")))
                ),
                Map(
                  "acronym" ->
                    PropertyRuleResults(
                      List(
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("acronym"), None), "attributeType", RuleValue(Some("stringV"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("acronym"), None), "displayNameForProperty", RuleValue(Some("Acronym"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("acronym"), None), "componentName", RuleValue(Some("ERD2WQueryStringOperator"), List()))
                      ),
                      "stringV"
                    ),
                  "address" ->
                    PropertyRuleResults(
                      List(
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("address"), None), "attributeType", RuleValue(Some("stringV"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("address"), None), "displayNameForProperty", RuleValue(Some("Address"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("address"), None), "componentName", RuleValue(Some("ERD2WQueryStringOperator"), List()))
                      ),
                      "stringV"
                    ),
                  "name" ->
                    PropertyRuleResults(
                      List(
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("name"), None), "attributeType", RuleValue(Some("stringV"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("name"), None), "displayNameForProperty", RuleValue(Some("Name"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("name"), None), "componentName", RuleValue(Some("ERD2WQueryStringOperator"), List()))
                      ),
                      "stringV"
                    ),
                  "projects" ->
                    PropertyRuleResults(
                      List(
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("projects"), None), "attributeType", RuleValue(Some("eosV"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("projects"), None), "displayNameForProperty", RuleValue(Some("Projects"), List())),
                        RuleResult(D2WContext(Some("Customer"), Some("query"), Some("projects"), None), "componentName", RuleValue(Some("ERD2WQueryToManyField"), List()))
                      ),
                      "stringV"
                    )
                )
              )
          )
      )
  )


}
