package io.whatifs.propertybasedtesting

import java.time.LocalDate

import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HeartRateSpec
  extends PropSpec
  with ScalaCheckDrivenPropertyChecks
  with Matchers {

  import HeartRateGenerators._

  property ("When building HeartRate, bounds are respected") {
    forAll(bpmGen) {
      case bpm if (bpm < HeartRate.MinHeartRate) =>
        assert(HeartRate.build(bpm).isLeft)

      case bpm if (bpm > HeartRate.MaxHeartRate) =>
        assert(HeartRate.build(bpm).isLeft)

      case bpm => assertResult(HeartRate(bpm))  {
        HeartRate.build(bpm).right.get
      }
    }
  }

  property ("Measurements hold") {
    forAll(personGen, hrGen) { (person, hr) =>
    val measurement = Measurement.measure(person, hr)
      assertResult(Measurement(person, hr, LocalDate.now())) {
        measurement
      }
      assert(measurement.warn.isDefined == hr.isHigh)
    }
  }

  property ("Measurement warning properties") {
    forAll (measurementGen) { measurement =>
      assertResult(measurement.heartRate.isHigh && measurement.date.isAfter(Measurement.warnIfAfter))
       { measurement.warn.isDefined }
    }
  }

}
