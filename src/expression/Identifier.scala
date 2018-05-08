package expression


import context._
import value._

case class Identifier(val name: String) extends Expression {
   override def toString = name
   
   def execute(env: Environment) = {  
     /*
      * when you execute an identifier, it gives you an object from the environment (hashmap)
      * the object could be a value like an integer or whatever OR a thunk/text. if it is a
      * thunk/text then you have to thaw it
      */
     val result = env(this)
     /*
     result match {
       case v: Thunk => v.apply(List(result))
       case v: Text => {
         println("v.body: " + v.body)
         v.body.execute(env)
       }
       case _ => result   //any other type of value
     }
     */
     println("rrr: " + result.getClass()) 
     if (result.isInstanceOf[Thunk]){
       result.asInstanceOf[Thunk].apply(List(result))
     } else if (result.isInstanceOf[Text]) {
       println("result.body: " + result.asInstanceOf[Text].body)
       result.asInstanceOf[Text].body.execute(env)
     } else {
       println("else")
       result
     }
   }
}

  