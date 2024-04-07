package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.Ex3Stacks.StackImpl.*
import u03.Optionals.Optional
import u03.Sequences.Sequence

/* Tests should be clear, but note they are expressed independently of the
   specific implementation
 */

class StackTest:

  @Test def testEmpty(): Unit =
    assertEquals(Sequence.Nil(), empty[Int].asSequence())

  @Test def testPush(): Unit =
    assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())

  @Test def testPopOnEmpty(): Unit =
    assertEquals(Optional.Empty(), empty[Int].pop)

  @Test def testPopOnNotEmpty(): Unit =
    assertEquals(Optional.Just((10, Sequence.Nil())), empty[Int].push(10).pop)
