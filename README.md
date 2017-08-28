# d2spa
SPA using Scala.js connecting to a D2W WebObjects server.

It uses:
  - Scala.js
  - ScalaReact
  - Diode

## Current state
Description above gives the goal of the project. Current state is far from that. It's a working SPA app but with limited features compare to the goal.

## Usage
d2spa has been build to work with D2SPAServer but if you want to try it without you can do it by changing the property usesD2SPAServer in Application.conf

Go to the project directory and issue those commands:
```
> sbt
> run
```

open you browser with this url: http://localhost:9000/#task/query/entity/Project
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