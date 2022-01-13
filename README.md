# utf8string

A project that provides `Utf8String` and `CodepointString` for ScalaJVM and
ScalaJS.

## example usage

```scala
import tf.bug.utf8string._

val u8s = Utf8String("Hello! ヤッホー！ 🎶")
println(u8s) // => Hello! ヤッホー！ 🎶
println(u8s.byteAt(15)) // => -100 (= 0x9b)
val cps = u8s.toCodepointString
println(cps) // => Hello! ヤッホー！ 🎶
println(cps.codepointAt(9)) // => 12507 (= 0x30db)
println(cps.codepointAt(cps.length - 1)) // => 127926 (= 0x1f3b6)
```

```scala
import fs2._
import tf.bug.utf8string.fs2.utf8text

val bytes = List(0xF0.toByte, 0x9F.toByte, 0x8E.toByte, 0xB6.toByte)
val toText = Stream.emits(bytes).through(utf8text.decodeUtf8)
toText.compile.toList // => List(🎶)
```
