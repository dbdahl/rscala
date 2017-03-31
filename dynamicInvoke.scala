val a = () => { scala.util.Random.nextInt() }

val b = Class.forName("scala.Function0")
val m = b.getMethod("apply")
val c = m.invoke(a)

