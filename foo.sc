import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class Interval(start: LocalDate, end: LocalDate)

def gaps(dates: List[Interval]): Boolean = {
  val s = dates.head
  val e = dates.last

  if (ChronoUnit.DAYS.between(s.`end`, e.start) > 1) {
    true
  } else
    false

}
val dates = List(
  Interval(LocalDate.of(2020, 10, 30), LocalDate.of(2020, 11, 2)),
  Interval(LocalDate.of(2020, 11, 2), LocalDate.of(2020, 11, 10)),
  Interval(LocalDate.of(2020, 11, 13), LocalDate.of(2020, 11, 20))
)

dates.sliding(2).exists(twoDates => gaps(twoDates))
