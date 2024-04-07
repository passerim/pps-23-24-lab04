package tasks.adts

import u03.Optionals.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

/*  Exercise 2:
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion:
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school
 */

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
