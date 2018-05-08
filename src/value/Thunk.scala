package value
import context._
import expression._

class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv){
  //executes body in a new "empty" environment
   def apply(args: List[Value]) = {
    val tempEnv = new Environment(defEnv)
    body.execute(tempEnv)
   }
}