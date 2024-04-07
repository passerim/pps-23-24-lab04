package tasks.adts

import org.junit.Assert.assertEquals
import org.junit.Test
import tasks.adts.SchoolModel.BasicSchoolModule
import tasks.adts.SchoolModel.BasicSchoolModule.*
import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

class SchoolModelTest:

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
