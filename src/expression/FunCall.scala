package expression

import context._
import value._
import scala.collection.mutable.ListBuffer

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  
  /*
  def execute1(env: Environment): Value = {
    var args: List[Value] = List()
    val passing = Flags.paramaterPassing
    if (env.contains(operator)){
      val maybeClosure = operator.execute(env)
      if (maybeClosure.isInstanceOf[Closure]) {
        val closure = maybeClosure.asInstanceOf[Closure]
        val args = operands.map(_.execute(env))
        closure(args, env)
      }
      
    } else {
      throw new TypeException("must be a closure")
    }
  }*/
  
  def execute(env: Environment): Value = {
    var env1 = env
    if (!Flags.useStaticScopeRule) 
    	  env1 = new Environment()  //defining environment
    
    /*
     * jedi 1.0
     var arguments = ListBuffer[Value]()
     for (i <- operands) {
       arguments += i.execute(env)
     }
     alu.execute(operator, arguments.toList)
     * 
     */ 
    
    /*
     //environment contains method
    val args = operands.map(_.execute(env))  //execute operands to get arguments
    if (env.contains(operator)){
      val maybeClosure = operator.execute(env)
      if (maybeClosure.isInstanceOf[Closure]) maybeClosure.asInstanceOf[Closure].apply(args)
      else throw new TypeException("must be a closure")
    } else {
      alu.execute(operator, args)
    }
    */
    
    
    
     // try catch method 
    /*
    val args = operands.map(_.execute(env))
    
    val passing = Flags.paramaterPassing
    
    passing match {
      case Flags.passByValue => 
      	  try {
      		  val maybeClosure = operator.execute(env)
      			if (!maybeClosure.isInstanceOf[Closure]) throw new TypeException("needs to be a closure")
      			else  maybeClosure.asInstanceOf[Closure].apply(args)
      		} catch {
      			case e: UndefinedException => alu.execute(operator, args)
      		}
      case Flags.passByName =>
        val thunks = List[Thunk]()
        for (i <- operands) {
          thunks :+ new Thunk(i, env)
        }
      case Flags.passByText => 
        val texts = List[Text]()
        for (i <- operands) {
          texts :+ new Text(i)
        }
    }
    * */
    
    
    var args: List[Value] = List()
    val passing = Flags.paramaterPassing
      
    passing match {
      case Flags.passByValue =>  {
      //  println("passByValue")
        args = operands.map(_.execute(env1))
      }
      case Flags.passByName => {
      //  println("passByName")
        args = operands.map((list: Expression) => new Thunk(list, env1))
      }
      case Flags.passByText => {
      //  println("passByText")
        args = operands.map((list: Expression) => new Text(list))
      }
    }
    
    /*
    try {
      println("try")
      val maybeClosure = operator.execute(env)
      if (!maybeClosure.isInstanceOf[Closure]) throw new TypeException
      else maybeClosure.asInstanceOf[Closure].apply(args)
    } catch {  //4th option. alu 
      case e: UndefinedException => alu.execute(operator, args)
		}
    */
    
    	if (env1.contains(operator)) {
    		val maybeClosure = operator.execute(env1)
    		if (maybeClosure.isInstanceOf[Closure]) {
    			val closure = maybeClosure.asInstanceOf[Closure]
    			//val args = operands.map(_.execute(env))
    			closure.apply(args, env1)
    		}
    		else {
    			throw new TypeException("only functions can be called")
    		}
    	} 	else {
    			val args = operands.map(_.execute(env1))
    			alu.execute(operator, args)
    	}
  }
}