package tasks

import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u04.monads.Monads.Monad

object Main:

  // Task 01, svolto da solo
  object Ex1ComplexNumbers:

    trait ComplexADT:
      type Complex
      def complex(re: Double, im: Double): Complex
      extension (complex: Complex)
        def re(): Double
        def im(): Double
        def sum(other: Complex): Complex
        def subtract(other: Complex): Complex
        def asString(): String

    object BasicComplexADT extends ComplexADT:

      private case class ComplexImpl(re: Double, im: Double)

      opaque type Complex = ComplexImpl

      def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)

      extension (complex: Complex)
        def re(): Double = complex.re

        def im(): Double = complex.im

        def sum(other: Complex): Complex =
          ComplexImpl(complex.re + other.re, complex.im + other.im)

        def subtract(other: Complex): Complex =
          ComplexImpl(complex.re - other.re, complex.im - other.im)

        def asString(): String = (complex.re, complex.im) match
          case (0.0, 0.0)      => "0.0"
          case (a, 0.0)        => s"$a"
          case (0.0, b)        => s"${b}i"
          case (a, b) if b < 0 => s"$a - ${b.abs}i"
          case (a, b)          => s"$a + ${b}i"

  // Task 02, svolto da solo
  object SchoolModel:

    trait SchoolModule:
      type School
      type Teacher
      type Course
      extension (school: School)
        def addTeacher(name: String): School
        def addCourse(name: String): School
        def teacherByName(name: String): Optional[Teacher]
        def courseByName(name: String): Optional[Course]
        def nameOfTeacher(teacher: Teacher): String
        def nameOfCourse(course: Course): String
        def setTeacherToCourse(teacher: Teacher, course: Course): School
        def coursesOfATeacher(teacher: Teacher): Sequence[Course]

    object BasicSchoolModule extends SchoolModule:
      private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

      private case class TeacherImpl(name: String, courses: Sequence[Course])

      private case class CourseImpl(name: String)

      opaque type School = SchoolImpl

      opaque type Teacher = TeacherImpl

      opaque type Course = CourseImpl

      def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School =
        SchoolImpl(teachers, courses)

      def teacher(name: String, courses: Sequence[Course]): Teacher =
        TeacherImpl(name, courses)

      def course(name: String): Course =
        CourseImpl(name)

      extension (school: School)
        def addTeacher(name: String): School =
          school.teacherByName(name) match
            case Optional.Just(t) => school
            case _ => SchoolImpl(Cons(TeacherImpl(name, Nil()), school.teachers), school.courses)

        def addCourse(name: String): School =
          school.courseByName(name) match
            case Optional.Just(c) => school
            case _ => SchoolImpl(school.teachers, Cons(CourseImpl(name), school.courses))

        def teacherByName(name: String): Optional[Teacher] =
          Sequence.filter(school.teachers)(_.name == name) match
            case Cons(teacher, _) => Optional.Just(teacher)
            case _                => Optional.Empty()

        def courseByName(name: String): Optional[Course] =
          Sequence.filter(school.courses)(_.name == name) match
            case Cons(course, _) => Optional.Just(course)
            case _               => Optional.Empty()

        def nameOfTeacher(teacher: Teacher): String = teacher.name

        def nameOfCourse(course: Course): String = course.name

        def setTeacherToCourse(teacher: Teacher, course: Course): School =
          school.courseByName(course.name) match
            case Optional.Just(c) =>
              SchoolImpl(
                Sequence.map(school.teachers)(t =>
                  t.name match
                    case teacher.name => TeacherImpl(t.name, Cons(course, t.courses))
                    case _            => t
                ),
                school.courses
              )
            case _ => school

        def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
          school.teacherByName(teacher.name) match
            case Optional.Just(t) => t.courses
            case _                => Nil()

  // Task 03, svolto da solo
  object Ex3Stacks:

    trait StackADT:
      type Stack[A]
      def empty[A]: Stack[A] // factory
      extension [A](stack: Stack[A])
        def push(a: A): Stack[A]
        def pop: Optional[(A, Stack[A])]
        def asSequence(): Sequence[A]

    object StackImpl extends StackADT:

      opaque type Stack[A] = Sequence[A]

      def empty[A]: Stack[A] = Nil()

      extension [A](stack: Stack[A])
        def push(a: A): Stack[A] = Cons(a, stack)

        def pop: Optional[(A, Stack[A])] = stack match
          case Cons(h, t) => Just((h, t))
          case _          => Empty()

        def asSequence(): Sequence[A] = stack

  // Task 04, svolto da solo
  object Ex4Summables:

    def sumAllInt(seq: Sequence[Int]): Int = seq match
      case Cons(h, t) => h + sumAllInt(t)
      case _          => 0

    trait Summable[A]:
      def sum(a1: A, a2: A): A
      def zero: A

    def sumAll[A: Summable](seq: Sequence[A]): A =
      val summable = summon[Summable[A]]
      seq match
        case Cons(h, t) => summable.sum(h, sumAll(t))
        case _          => summable.zero

    given Summable[Int] with
      def sum(a1: Int, a2: Int): Int = a1 + a2

      def zero: Int = 0

    given Summable[Double] with
      def sum(a1: Double, a2: Double): Double = a1 + a2

      def zero: Double = 0.0

    given Summable[String] with
      def sum(a1: String, a2: String): String = a1 concat a2

      def zero: String = ""

  // Task 05, svolto da solo
  object Ex5Traversable:

    def log[A](a: A): Unit = println("The next element is: " + a)

    trait Traversable[T[_]]:
      def foreach[A](el: T[A])(f: A => Unit): Unit

    given Traversable[Sequence] with
      def foreach[A](el: Sequence[A])(f: A => Unit): Unit = el match
        case Cons(h, t) => f(h); foreach(t)(f)
        case _          => ()

    given Traversable[Optional] with
      def foreach[A](el: Optional[A])(f: A => Unit): Unit = el match
        case Just(a) => f(a)
        case _       => ()

    def logAll[A, T[_]: Traversable](el: T[A])(logger: A => Unit): Unit =
      val traversable = summon[Traversable[T]]
      traversable.foreach(el)(logger)

  // Task 06, svolto da solo
  object Ex6TryModel:

    private enum TryImpl[A]:
      case Success(value: A)
      case Failure(exception: Throwable)

    opaque type Try[A] = TryImpl[A]

    def success[A](value: A): Try[A] = TryImpl.Success(value)

    def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)

    def exec[A](expression: => A): Try[A] =
      try success(expression)
      catch case e => failure(e)

    extension [A](m: Try[A])
      def getOrElse[B >: A](other: B): B = m match
        case TryImpl.Success(value) => value
        case TryImpl.Failure(_)     => other

    given Monad[Try] with
      override def unit[A](value: A): Try[A] = TryImpl.Success(value)

      extension [A](m: Try[A])
        override def flatMap[B](f: A => Try[B]): Try[B] = m match
          case TryImpl.Success(value)     => f(value)
          case TryImpl.Failure(exception) => failure(exception)
