This is an experiment: porting an [elm-parser](https://github.com/elm/parser) into Kotlin. It lets you write a code like this:

```kotlin
data class Point(val x: Int, val y: Int)

val parser: Parser<Point> = (
        succeed { x: Int, y: Int -> Point(x, y) }
                skip symbol("(")
                skip spaces
                keep2 int
                skip spaces
                skip symbol(",")
                skip spaces
                keep int
                skip spaces
                skip symbol(")")
        )

println(parser.run("(10,20)")) // Ok(value=Point(x=10, y=20))
```

Code is in disarray 'cause it's just two evenings worth of hacking some stuff together. I got some basic things implemented and it seems this might work quite OK, although the API needs some love and I'd like to have some utils for producing sane output messages.
