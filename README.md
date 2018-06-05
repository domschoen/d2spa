# d2spa
SPA using Scala.js connecting to a D2W WebObjects server.

It uses:
  - Scala.js
  - ScalaReact
  - Diode

## Current state
Description above gives the goal of the project. Current state is far from that. It's a working SPA app but with limited features compare to the goal.

## Usage
d2spa has been build to work with D2SPAServer which should be up and running

Go to the project directory and issue those commands:
```
> sbt
> run
```

open you browser with this url: http://localhost:9000/#task/query/entity/Project

## Features
  - At save, if server return an error (field exceeding DB field size for example), it stays in edit mode and display error message
  - Delete
  - None value in popup when value is null
  - Inspect a record with url like: http://localhost:9000/#task/inspect/entity/Customer/1

## Logging

### Client
If you want to have less log, you have to change the elideOptions in the build.sbt file. For example, if you want to remove everything below WARNING:
```
elideOptions := Seq("-Xelide-below", "WARNING"),
```

This will remove INFO, DEBUG, TRACE, ALL from your the log. You need to reload and recompile to have it:
  - reload
  - compile

Note: here the list of logging levels:
  - FATAL
  - ERROR
  - WARN
  - INFO
  - DEBUG
  - TRACE
  - ALL

#### Client server log

If you want to have client log added to the server log, you need to uncomment the line in the SPAMain.scala:
```
log.enableServerLogging("/logging")
```

### Server

This is the standard Play Framework logging system.

To turn on the log, no need to recompile, you just need to replace the content of file  d2spa/server/src/main/resources/logback.xml by the content of logback.xml.dev (same path)

To make it active:
  - restart the application

## Difficulties
### No location with Dynamic route
#### Difficulty
When you have a link to go to another page which is another route, you just use the router like this:
```
<.div(props.router.link(TodoLoc)("Check your todos!"))
```
but with dynamic route, you don't have "dynamic Location" kind of. I tried a dispatch of the action + a set of the router like this:
```
Callback.log(s"Menu selected: $entity") >> router.set(QueryPage(entity)) >>
  $.props >>= (_.proxy.dispatchCB(SelectMenu(entity)))
```
but if the action takes time the page is displayed with not information...
#### Solution
No real solution, just avoid using dynamic route. All possible dynamic routes are listed as location route. It means less flexibility and kind of hardcoding.

## To Do
### Important

  - Implement ERDList
  - Implement ERD2WDisplayToOne
  - Why to-many relationship Customer.projects is not part of exported data ?
  - List page > Inspect 1 element > Click Return button should go back to List page
  - Change D2WContext to FullFledgeD2WContext for FireRule
  - in Query page, clear a criteria should remove the query value
  
### Open Points
  - How to work with a eo value returned by a method. Let's say a method doing processing on other db attributes like a userPresentableDescription.
    - Should we reimplement the logic in the SPA ?
    - Should we ask the server every time we need this value ?

### Nice to have
  - Limit the number of characters according to eomodel size and display a message explaining it
  - Enable the save only if all mandatory attributes are present. Display a message explaining that

## Learning React
  - [HOC](https://reactjs.org/docs/higher-order-components.html)
