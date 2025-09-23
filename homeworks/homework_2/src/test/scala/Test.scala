import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 8) == 14)
            assert(Exercises.sumOfDivBy3Or5(-10, -1) == -33)
            assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(5, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(2147483643, 2147483646) == 2147483643L + 2147483645 + 2147483646)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(997) == Seq(997))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(39215) == Seq(5, 11, 23, 31))
            assert(Exercises.primeFactor(math.pow(3, 10).toInt) == Seq(3))
            intercept[IllegalArgumentException](Exercises.primeFactor(1))
        }
    }
}
