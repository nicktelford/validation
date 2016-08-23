# Missing Features

* Properly handle iterative types (e.g. lists) in the path of an error message.

  We will need a reasonable way to represent the path to an item in a list, 
  both in the internal path (`List[String]`) and the output message.

  The output message should probably be formatted as:

    parent.list[0].child

  Though how we represent this internally I don't know. Perhaps change the
  `Path` type from `List[String]` to an enumeration:

    sealed trait Node
    case class NamedNode(name: String) extends Node
    case class IndexedNode(name: String, index: Int) extends Node

    type Path = List[Node]

  The downside of this approach is it makes the API more complex, as the user
  can no longer simply pattern match on a `List[String]` to handle the path.

* Provide more default validators

  The goal here should be to cover all Scala standard library types (especially
  collections) and all `cats.data` types for which default implementations can
  be feasibly defined.

