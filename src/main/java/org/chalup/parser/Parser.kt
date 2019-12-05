package org.chalup.parser

import org.chalup.parser.Json.JsonBool
import org.chalup.parser.Json.JsonInt
import org.chalup.parser.Json.JsonNull
import org.chalup.parser.LeBoolean.LeFalse
import org.chalup.parser.LeBoolean.LeOr
import org.chalup.parser.LeBoolean.LeTrue
import org.chalup.parser.LoopStep.Done
import org.chalup.parser.LoopStep.Loop
import org.chalup.parser.ParserStep.Bad
import org.chalup.parser.ParserStep.Good
import org.chalup.parser.Problem.Custom
import org.chalup.parser.Problem.ExpectedKeyword
import org.chalup.parser.Problem.ExpectedSymbol
import org.chalup.parser.Problem.ExpectingFloat
import org.chalup.parser.Problem.ExpectingInt
import org.chalup.parser.Problem.UnexpectedChar
import org.chalup.parser.Result.Error
import org.chalup.parser.Result.Ok
import org.chalup.parser.TrailingSeparatorTreatment.FORBIDDEN
import org.chalup.parser.TrailingSeparatorTreatment.MANDATORY
import org.chalup.parser.TrailingSeparatorTreatment.OPTIONAL

sealed class Result<out ErrorT, out OkT> {
    data class Error<T>(val error: T) : Result<T, Nothing>()
    data class Ok<T>(val value: T) : Result<Nothing, T>()
}

data class State(val src: String,
                 val offset: Int,
                 val indent: Int,
                 val row: Int,
                 val col: Int)

sealed class ParserStep<out T> {
    data class Good<T>(val progress: Boolean,
                       val value: T,
                       val state: State) : ParserStep<T>()

    data class Bad(val progress: Boolean,
                   val problems: List<DeadEnd>) : ParserStep<Nothing>()
}

sealed class Problem {
    data class ExpectedSymbol(val str: String) : Problem()
    data class ExpectedKeyword(val str: String) : Problem()
    object ExpectingInt : Problem()
    object ExpectingFloat : Problem()
    object UnexpectedChar : Problem()
    data class Custom(val message: String) : Problem()
}

data class Token(val str: String, val problem: Problem, val delimiterPredicate: (Char) -> Boolean)

data class DeadEnd(val row: Int,
                   val col: Int,
                   val problem: Problem) {
    constructor(state: State, problem: Problem) : this(state.row, state.col, problem)
}

class Parser<T>(val parse: (State) -> ParserStep<T>) {
    fun run(input: String): Result<List<DeadEnd>, T> =
        when (val step = parse(State(src = input, offset = 0, indent = 1, row = 1, col = 1))) {
            is Good -> Ok(step.value)
            is Bad -> Error(step.problems)
        }
}

fun isSubstring(str: String, offset: Int, row: Int, col: Int, bigString: String): Triple<Int, Int, Int> {
    var newOffset = offset
    var newRow = row
    var newCol = col

    var isGood = offset + str.length <= bigString.length
    var i = 0

    while (isGood && i < str.length) {
        val isNewline = bigString[newOffset] == '\n'
        isGood = str[i++] == bigString[newOffset++]
        if (isNewline) {
            newRow++; newCol = 1
        } else {
            newCol++
        }
    }

    return Triple(if (isGood) newOffset else -1, newRow, newCol)
}

val int: Parser<Int> = number(NumberContext(
    int = Ok { n: Int -> n },
    float = Error(ExpectingInt),
    invalid = ExpectingInt,
    expecting = ExpectingInt
))

val float: Parser<Float> = number(NumberContext(
    int = Ok { n: Int -> n.toFloat() },
    float = Ok { n: Float -> n },
    invalid = ExpectingFloat,
    expecting = ExpectingFloat
))

data class NumberContext<out T>(val int: Result<Problem, (Int) -> T>,
                                val float: Result<Problem, (Float) -> T>,
                                val invalid: Problem,
                                val expecting: Problem)

fun <T> number(context: NumberContext<T>): Parser<T> = Parser { s ->
    val (intOffset, n) = consumeBase(10, s.offset, s.src)
    finalizeFloat(context, intOffset, n, s)
}

fun consumeBase(base: Int, offset: Int, string: String): Pair<Int, Int> {
    var total = 0
    var newOffset = offset

    while (newOffset < string.length) {
        val digit = string[newOffset] - '0'
        if (digit < 0 || digit >= base) break
        total = base * total + digit
        newOffset++
    }

    return newOffset to total
}

fun <T> finalizeFloat(context: NumberContext<T>, intOffset: Int, n: Int, s: State): ParserStep<T> {
    val floatOffset = consumeDotAndExp(intOffset, s.src)

    return when {
        floatOffset < 0 -> Bad(progress = true,
                               problems = listOf(DeadEnd(row = s.row,
                                                         col = s.col - (floatOffset + s.offset),
                                                         problem = context.invalid)))
        s.offset == floatOffset -> Bad(progress = false,
                                       problems = fromState(s, context.expecting))
        intOffset == floatOffset -> finalizeInt(context.invalid, context.int, s.offset, intOffset, n, s)
        else -> when (context.float) {
            is Error -> Bad(progress = true,
                            problems = listOf(DeadEnd(s, context.invalid))) // shouldn't we take the problem from the handler?
            is Ok -> s.src.substring(s.offset, floatOffset).toFloatOrNull()
                ?.let {
                    Good(progress = true,
                         value = context.float.value(it),
                         state = s.bumpOffset(floatOffset))
                }
                ?: Bad(progress = true,
                       problems = fromState(s, context.invalid))
        }
    }
}

fun <T> finalizeInt(invalid: Problem,
                    handler: Result<Problem, (Int) -> T>,
                    startOffset: Int,
                    endOffset: Int,
                    n: Int,
                    s: State): ParserStep<T> =
    when (handler) {
        is Error -> Bad(progress = true, problems = fromState(s, handler.error))
        is Ok ->
            if (startOffset == endOffset) Bad(progress = s.offset < startOffset, problems = fromState(s, invalid))
            else Good(progress = true, value = handler.value(n), state = s.bumpOffset(endOffset))
    }

private fun State.bumpOffset(newOffset: Int) = copy(
    offset = newOffset,
    col = col + (newOffset - offset)
)

fun fromState(s: State, x: Problem) = listOf(DeadEnd(s, x))

fun consumeDotAndExp(offset: Int, src: String): Int =
    if (src.at(offset) == '.') consumeExp(chompBase10(offset + 1, src), src)
    else consumeExp(offset, src)

fun chompBase10(offset: Int, src: String): Int {
    var newOffset = offset

    while (newOffset < src.length) {
        val char = src[newOffset]
        if (char < '0' || char > '9') break
        newOffset++
    }

    return newOffset
}

fun consumeExp(offset: Int, src: String): Int {
    if (src.at(offset) == 'e' || src.at(offset) == 'E') {
        val eOffset = offset + 1
        val expOffset = if (src[eOffset] == '+' || src[eOffset] == '-') eOffset + 1 else eOffset
        val newOffset = chompBase10(expOffset, src)

        if (expOffset == newOffset) {
            return -newOffset
        } else {
            return newOffset
        }
    } else {
        return offset
    }
}

// Elm version uses under the hood some Javascript method which returns 'NaN' when reaching
// outside of the valid offset range. I'm duplicating this logic here to keep the reset of
// the code the same.
fun String.at(offset: Int) = if (offset >= length) null else get(offset)

fun symbol(str: String): Parser<Unit> = token(Token(str, ExpectedSymbol(str), delimiterPredicate = { false }))
fun keyword(str: String): Parser<Unit> = token(Token(str, ExpectedKeyword(str), delimiterPredicate = { it.isLetterOrDigit() || it == '_' }))
fun token(token: Token) = Parser { s ->
    val (newOffset, newRow, newCol) = isSubstring(token.str, s.offset, s.row, s.col, s.src)

    if (newOffset == -1 || isSubChar(token.delimiterPredicate, newOffset, s.src) >= 0) Bad(progress = false, problems = fromState(s, token.problem))
    else Good(progress = token.str.isNotEmpty(),
              value = Unit,
              state = s.copy(offset = newOffset,
                             row = newRow,
                             col = newCol))
}

val spaces: Parser<Unit> = chompWhile { it.isWhitespace() }

fun chompWhile(predicate: (Char) -> Boolean): Parser<Unit> = Parser { state ->
    tailrec fun helper(offset: Int, row: Int, col: Int): ParserStep<Unit> =
        when (val newOffset = isSubChar(predicate, offset, state.src)) {
            -1 -> Good(progress = state.offset < offset,
                       value = Unit,
                       state = state.copy(offset = offset,
                                          row = row,
                                          col = col))
            -2 -> helper(offset + 1, row + 1, 1)
            else -> helper(newOffset, row, col + 1)
        }

    helper(state.offset, state.row, state.col)
}

fun chompIf(predicate: (Char) -> Boolean): Parser<Unit> = Parser { s ->
    when (val newOffset = isSubChar(predicate, s.offset, s.src)) {
        -1 -> Bad(progress = false, problems = fromState(s, UnexpectedChar))
        -2 -> Good(progress = true,
                   value = Unit,
                   state = s.copy(offset = s.offset + 1,
                                  row = s.row + 1,
                                  col = 1))
        else -> Good(progress = true,
                     value = Unit,
                     state = s.copy(offset = newOffset,
                                    col = s.col + 1))
    }
}

fun isSubChar(predicate: (Char) -> Boolean, offset: Int, string: String): Int = when {
    string.length <= offset -> -1
    predicate(string[offset]) -> if (string[offset] == '\n') -2 else offset + 1
    else -> -1
}

fun <T> succeed(v: T): Parser<T> = Parser { state ->
    Good(progress = false,
         value = v,
         state = state)
}

fun problem(message: String): Parser<Nothing> = Parser { s ->
    Bad(progress = false,
        problems = fromState(s, Custom(message)))
}

fun <T> oneOf(parsers: List<Parser<out T>>): Parser<T> = Parser { s ->
    tailrec fun helper(problems: List<DeadEnd>, parsers: List<Parser<out T>>): ParserStep<T> =
        if (parsers.isEmpty()) Bad(progress = false, problems = problems)
        else when (val step = parsers.first().parse(s)) {
            is Good -> step
            is Bad ->
                if (step.progress) step
                else helper(problems + step.problems, parsers.drop(1))
        }

    helper(problems = emptyList(), parsers = parsers)
}

fun <T> oneOf(vararg parsers: Parser<out T>): Parser<T> = oneOf(parsers.asList())

fun <T> parser(block: (self: Parser<T>) -> Parser<T>): Parser<T> {
    var stubParser: Parser<T> = Parser { throw IllegalStateException("Using stubbed parser!") }
    val selfParser: Parser<T> = Parser { s -> stubParser.parse(s) }

    return block(selfParser).also { stubParser = it }
}

fun <T, R> map(func: (T) -> R, parser: Parser<T>): Parser<R> = Parser { s ->
    when (val step = parser.parse(s)) {
        is Good -> Good(progress = step.progress,
                        value = func(step.value),
                        state = step.state)
        is Bad -> step
    }
}

infix fun <T, R> Parser<T>.mapTo(func: (T) -> R): Parser<R> = map(func, this)

fun <T1, T2, R> map2(func: (T1, T2) -> R, parserA: Parser<T1>, parserB: Parser<T2>): Parser<R> = Parser { s ->
    when (val stepA = parserA.parse(s)) {
        is Bad -> stepA
        is Good -> when (val stepB = parserB.parse(stepA.state)) {
            is Bad -> Bad(progress = stepA.progress || stepB.progress,
                          problems = stepB.problems)
            is Good -> Good(progress = stepA.progress || stepB.progress,
                            value = func(stepA.value, stepB.value),
                            state = stepB.state)
        }
    }
}

fun getChompedString(parser: Parser<*>): Parser<String> = Parser { s ->
    when (val step = parser.parse(s)) {
        is Bad -> step
        is Good -> Good(progress = step.progress,
                        value = s.src.substring(s.offset, step.state.offset),
                        state = step.state)
    }
}

infix fun <T, R> Parser<T>.andThen(func: (T) -> Parser<out R>): Parser<R> = Parser { s ->
    when (val stepA = parse(s)) {
        is Bad -> stepA
        is Good -> when (val stepB = func(stepA.value).parse(stepA.state)) {
            is Bad -> Bad(progress = stepA.progress || stepB.progress,
                          problems = stepB.problems)
            is Good -> Good(progress = stepA.progress || stepB.progress,
                            value = stepB.value,
                            state = stepB.state)
        }
    }
}

infix fun <T, R> Parser<(T) -> R>.keep(keepParser: Parser<T>): Parser<R> = map2(
    func = { f, arg -> f(arg) },
    parserA = this,
    parserB = keepParser
)

infix fun <T1, T2, R> Parser<(T1, T2) -> R>.keep2(keepParser: Parser<T1>): Parser<(T2) -> R> = map2(
    func = { f, a1 -> { a2: T2 -> f(a1, a2) } },
    parserA = this,
    parserB = keepParser
)

infix fun <KeepT, SkipT> Parser<KeepT>.skip(skip: Parser<SkipT>): Parser<KeepT> = map2(
    func = { a: KeepT, _: SkipT -> a },
    parserA = this,
    parserB = skip
)

sealed class LoopStep<out S, out T> {
    data class Loop<S>(val state: S) : LoopStep<S, Nothing>()
    data class Done<T>(val result: T) : LoopStep<Nothing, T>()
}

fun <S, T> loop(loopState: S, callback: (S) -> Parser<LoopStep<S, T>>): Parser<T> = Parser { s ->
    tailrec fun helper(progress: Boolean, loopState: S, parserState: State): ParserStep<T> =
        when (val step = callback(loopState).parse(parserState)) {
            is Good -> when (val nextLoopStep = step.value) {
                is Loop -> helper(progress = progress || step.progress,
                                  loopState = nextLoopStep.state,
                                  parserState = step.state)
                is Done -> Good(progress = progress || step.progress,
                                value = nextLoopStep.result,
                                state = step.state)
            }
            is Bad -> Bad(progress = progress || step.progress,
                          problems = step.problems)
        }

    helper(false, loopState, s)
}

enum class TrailingSeparatorTreatment { FORBIDDEN, OPTIONAL, MANDATORY }

// TODO mark internal?
infix fun <SkipT, KeepT> Parser<SkipT>.skipThen(keep: Parser<KeepT>): Parser<KeepT> = map2(
    func = { _: SkipT, b: KeepT -> b },
    parserA = this,
    parserB = keep
)

fun <T> sequence(start: String,
                 separator: String,
                 end: String,
                 trailingSeparatorTreatment: TrailingSeparatorTreatment,
                 itemParser: Parser<T>,
                 spacesParser: Parser<Unit> = spaces): Parser<List<T>> {
    return symbol(start) skipThen spacesParser skipThen sequenceEnd(endParser = symbol(end),
                                                                    spacesParser = spacesParser,
                                                                    separatorParser = symbol(separator),
                                                                    trailingSeparatorTreatment = trailingSeparatorTreatment,
                                                                    itemParser = itemParser)
}

fun <T> sequenceEnd(endParser: Parser<Unit>,
                    spacesParser: Parser<Unit>,
                    itemParser: Parser<T>,
                    separatorParser: Parser<Unit>,
                    trailingSeparatorTreatment: TrailingSeparatorTreatment): Parser<List<T>> {
    val chompRest: (T) -> Parser<List<T>> = { item: T ->
        when (trailingSeparatorTreatment) {
            FORBIDDEN -> loop(listOf(item), { s -> sequenceEndForbidden(endParser, spacesParser, itemParser, separatorParser, s) })
            OPTIONAL -> loop(listOf(item), { s -> sequenceEndOptional(endParser, spacesParser, itemParser, separatorParser, s) })
            MANDATORY -> (spacesParser skipThen separatorParser skipThen spacesParser skipThen loop(listOf(item), { s -> sequenceEndMandatory(spacesParser, itemParser, separatorParser, s)})) skip endParser
        }
    }

    return oneOf(
        itemParser andThen chompRest,
        endParser mapTo { emptyList<T>() }
    )
}

fun <T> sequenceEndForbidden(endParser: Parser<Unit>,
                             spacesParser: Parser<Unit>,
                             itemParser: Parser<T>,
                             separatorParser: Parser<Unit>,
                             items: List<T>): Parser<LoopStep<List<T>, List<T>>> =
    spacesParser skipThen oneOf(separatorParser skipThen spacesParser skipThen (itemParser mapTo { item -> Loop(items + item) }),
                                endParser mapTo { Done(items) })

fun <T> sequenceEndOptional(endParser: Parser<Unit>,
                            spacesParser: Parser<Unit>,
                            itemParser: Parser<T>,
                            separatorParser: Parser<Unit>,
                            items: List<T>): Parser<LoopStep<List<T>, List<T>>> {
    val parseEnd = endParser mapTo { Done(items) }

    return spacesParser skipThen oneOf(separatorParser skipThen spacesParser skipThen oneOf(itemParser mapTo { item -> Loop(items + item) },
                                                                                            parseEnd),
                                       parseEnd)
}

fun <T> sequenceEndMandatory(spacesParser: Parser<Unit>,
                             itemParser: Parser<T>,
                             separatorParser: Parser<Unit>,
                             items: List<T>): Parser<LoopStep<List<T>, List<T>>> {
    return oneOf(
        itemParser skip spacesParser skip separatorParser skip spacesParser mapTo { item -> Loop(items + item) },
        succeed(Unit) mapTo { Done(items) }
    )
}

enum class Suite(val symbol: Char) {
    CLUBS('c'), HEARTS('h'), DIAMONDS('d'), SPADES('s')
}

enum class Rank(val symbol: Char) {
    TWO('2'),
    THREE('3'),
    FOUR('4'),
    FIVE('5'),
    SIX('6'),
    SEVEN('7'),
    EIGHT('8'),
    NINE('9'),
    TEN('T'),
    JACK('J'),
    QUEEN('Q'),
    KING('K'),
    ACE('A')
}

data class Card(val rank: Rank, val suite: Suite) {
    override fun toString(): String = "${rank.symbol}${suite.symbol}"
}

data class Point(val x: Int, val y: Int)

sealed class Json {
    data class JsonInt(val int: Int) : Json()
    data class JsonBool(val boolean: Boolean) : Json()
    object JsonNull : Json() {
        override fun toString() = "null"
    }
}

sealed class LeBoolean {
    object LeTrue : LeBoolean() {
        override fun toString() = "true"
    }

    object LeFalse : LeBoolean() {
        override fun toString() = "false"
    }

    data class LeOr(val lhs: LeBoolean, val rhs: LeBoolean) : LeBoolean() {
        override fun toString() = "($lhs || $rhs)"
    }
}

fun main() {
    val leBooleanParser: Parser<LeBoolean> = parser { leBoolean ->
        oneOf(
            succeed(LeTrue) skip keyword("true"),
            succeed(LeFalse) skip keyword("false"),
            (succeed { l: LeBoolean, r: LeBoolean -> LeOr(l, r) }
                    skip symbol("(")
                    skip spaces
                    keep2 leBoolean
                    skip spaces
                    skip symbol("||")
                    skip spaces
                    keep leBoolean
                    skip spaces
                    skip symbol(")")
                    )
        )
    }

    val jsonParser: Parser<Json> =
        oneOf(
            int mapTo { JsonInt(it) },
            keyword("true") mapTo { JsonBool(true) },
            keyword("false") mapTo { JsonBool(false) },
            keyword("null") mapTo { JsonNull }
        )

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

    val phpParser = getChompedString(
        succeed(Unit)
                skip chompIf { it == '$' }
                skip chompIf { it.isLetter() || it == '_' }
                skip chompWhile { it.isLetterOrDigit() || it == '_' }
    )

    val zipCodeParser = getChompedString(chompWhile { it.isDigit() })
        .andThen { code ->
            if (code.length == 5) succeed(code)
            else problem("a U.S. zip code has exactly 5 digits, but I found '$code'")
        }

    val letKeywordParser = keyword("let")

    val suiteParser = oneOf(Suite.values().map { suite -> symbol("${suite.symbol}") mapTo { suite } })
    val rankParser = oneOf(Rank.values().map { rank -> symbol("${rank.symbol}") mapTo { rank } })

    val cardParser = (
            succeed { rank: Rank, suite: Suite -> Card(rank, suite) }
                    keep2 rankParser
                    keep suiteParser
            )

    val handsParser = sequence(
        start = "[",
        end = "]",
        separator = ",",
        trailingSeparatorTreatment = MANDATORY,
        itemParser = cardParser
    )

    println(parser.run("(10,20)"))
    println(int.run("123"))
    println(float.run("123"))
    println(float.run("123.123"))
    println(jsonParser.run("10"))
    println(jsonParser.run("null"))
    println(leBooleanParser.run("(true || (true || false))"))
    println(letKeywordParser.run("let"))
    println(letKeywordParser.run("letters"))
    println(phpParser.run("\$txt"))
    println(phpParser.run("\$x"))
    println(zipCodeParser.run("12345"))
    println(zipCodeParser.run("test"))
    println(zipCodeParser.run("123456789"))
    println(cardParser.run("Kh"))
    println(cardParser.run("Qs"))
    println(cardParser.run("*K"))
    println(cardParser.run("KK"))
    println(handsParser.run("[Kh, Qs]"))
    println(handsParser.run("[Kh, Qs,]"))
}
