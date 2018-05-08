package value

import context._
import expression._
import scala.collection.mutable.ListBuffer

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value{
  def apply(args: List[Value], env: Environment) = {
    //  1. tempEnv extends defEnv
    //  2. bulk put params and args into tempEnv
    //  3. execute body in tempEnv
    var tempEnv = new Environment(defEnv)
    if (!Flags.useStaticScopeRule) 
      tempEnv = new Environment(env)
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
   }
}

