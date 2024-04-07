import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

object MainTest:

  // Task 02
  class SchoolModelTest:

    import tasks.Main.SchoolModel.BasicSchoolModule.*

    val courseConst: Course = course("algorithms")
    val coursesConst: Sequence[Course] =
      Cons(course("algorithms"), Cons(course("data mining"), Nil()))
    val teacherConst: Teacher =
      teacher("matteo", coursesConst)
    val teachersConst: Sequence[Teacher] =
      Cons(teacher("alberto", Cons(course("tech web"), Nil())), Cons(teacherConst, Nil()))
    val schoolConst: School = school(teachersConst, coursesConst)

    @Test def testAddTeacher(): Unit =
      val teacherName = "luca"
      val teacher1    = teacher(teacherName, Nil())
      assertEquals(
        school(Cons(teacher1, teachersConst), coursesConst),
        school(teachersConst, coursesConst).addTeacher(teacherName)
      )
      assertEquals(schoolConst, schoolConst.addTeacher(schoolConst.nameOfTeacher(teacherConst)))

    @Test def testAddCourse(): Unit =
      val courseName = "tech web"
      val course1    = course(courseName)
      assertEquals(
        school(teachersConst, Cons(course1, coursesConst)),
        school(teachersConst, coursesConst).addCourse(courseName)
      )
      assertEquals(schoolConst, schoolConst.addCourse(schoolConst.nameOfCourse(courseConst)))

    @Test def testTeacherByName(): Unit =
      val teacherName = "luca"
      assertEquals(
        Optional.Just(teacherConst),
        schoolConst.teacherByName(schoolConst.nameOfTeacher(teacherConst))
      )
      assertEquals(Optional.Empty(), schoolConst.teacherByName(teacherName))

    @Test def testCourseByName(): Unit =
      val courseName = "tech web"
      assertEquals(
        Optional.Just(courseConst),
        schoolConst.courseByName(schoolConst.nameOfCourse(courseConst))
      )
      assertEquals(Optional.Empty(), schoolConst.courseByName(courseName))

    @Test def testNameOfTeacher(): Unit =
      val teacherName = "matteo"
      assertEquals(teacherName, schoolConst.nameOfTeacher(teacher(teacherName, Nil())))

    @Test def testNameOfCourse(): Unit =
      val courseName = "algorithms"
      assertEquals(courseName, schoolConst.nameOfCourse(course(courseName)))

    @Test def testSetTeacherToCourse(): Unit =
      val teacherName = "luca"
      val teacher1    = teacher(teacherName, Nil())
      val courseName  = "tech web"
      val course1     = course(courseName)
      val startSchool = schoolConst.addTeacher(teacherName).addCourse(courseName)
      val endSchool =
        school(
          Cons(teacher(teacherName, Cons(course(courseName), Nil())), teachersConst),
          Cons(course1, coursesConst)
        )
      assertEquals(endSchool, startSchool.setTeacherToCourse(teacher1, course1))
      assertEquals(startSchool, startSchool.setTeacherToCourse(teacher1, course("literature")))
      assertEquals(startSchool, startSchool.setTeacherToCourse(teacher("luigi", Nil()), course1))

    @Test def testCoursesOfATeacher(): Unit =
      val teacherName = "luca"
      assertEquals(coursesConst, schoolConst.coursesOfATeacher(teacherConst))
      assertEquals(Nil(), schoolConst.coursesOfATeacher(teacher(teacherName, Nil())))
      assertEquals(
        Nil(),
        schoolConst.coursesOfATeacher(teacher(teacherName, Cons(courseConst, Nil())))
      )

  // Task 04
  class SummablesTest:

    import tasks.Main.Ex4Summables.*

    @Test def testSumAllInt(): Unit =
      val si = Cons(10, Cons(20, Cons(30, Nil())))
      assertEquals(60, sumAllInt(si))
      assertEquals(60, sumAll(si))

    @Test def testSumAllDouble(): Unit =
      val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
      assertEquals(60.0, sumAll(sd), 0.0)

    @Test def testSumAllString(): Unit =
      val ss = Cons("10", Cons("20", Cons("30", Nil())))
      assertEquals("102030", sumAll(ss))

  // Task 06
  class TryModelTest:

    import tasks.Main.Ex6TryModel.*

    @Test def test1(): Unit =
      val result = for
        a <- success(10)
        b <- success(30)
      yield a + b
      assertEquals(40, result.getOrElse(-1))

    @Test def test2(): Unit =
      assertEquals(30, success(20).map(_ + 10).getOrElse(-1))

    @Test def test3(): Unit =
      val result = for
        a <- success(10)
        b <- failure(new RuntimeException("error"))
        c <- success(30)
      yield a + c
      assertEquals(-1, result.getOrElse(-1))

    @Test def test4(): Unit =
      val result = for
        a <- exec(10)
        b <- exec(throw new RuntimeException("error"))
        c <- exec(30)
      yield a + c
      assertEquals(-1, result.getOrElse(-1))
