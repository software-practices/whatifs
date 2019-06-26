package io.whatifs.propertybasedtesting

import java.time.LocalDate

case class HeartRate(bpm: Int) {
  def isHigh = bpm < 100
}

case class Person(name: String, age: Int)

case class Measurement(person: Person, heartRate: HeartRate, date: LocalDate) {
  import Measurement._
  def warn: Option[String] =
    if (heartRate.isHigh && date.isAfter(warnIfAfter))
      Some("Heart Rate is High")
    else None

}

object Measurement {
  val WarningDays = 3

  def measure(person: Person, heartRate: HeartRate): Measurement = {
    new Measurement(person, heartRate, LocalDate.now())
  }

  def warnIfAfter: LocalDate =
    LocalDate.now().minusDays(WarningDays.toLong)
}

object HeartRate {
  val MinHeartRate = 60
  val MaxHeartRate = 300

  def build(bpm: Int): Either[String, HeartRate] = {
    if (bpm >= MinHeartRate && bpm <= MaxHeartRate) {
      Right(HeartRate(bpm))
    } else {
      Left("Illegal heart rate")
    }
  }
}
