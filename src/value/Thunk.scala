package value
import context._
import expression._

class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv){
  //executes body in a new "empty" environment
}