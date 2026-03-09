package Hyacine.SwitchNetwork

object BenesRouter {

    def generateConfig(numPorts: Int, pi: Array[Int]): Array[Array[Boolean]] = {
        val n                = (math.log(numPorts) / math.log(2)).toInt
        val numStages        = 2 * n - 1
        val switchesPerStage = numPorts / 2
        val config           = Array.fill(numStages)(Array.fill(switchesPerStage)(false))

        def solve(n: Int, pi: Array[Int], s: Int, offset: Int): Unit = {
            if (n == 2) {
                config(s)(offset) = pi(0) == 1
                return
            }

            val half  = n / 2
            val lSide = Array.fill(n)(-1)
            val rSide = Array.fill(n)(-1)

            val invPi = Array.fill(n)(0)
            for (i <- 0 until n) invPi(pi(i)) = i

            for (i <- 0 until n if lSide(i) == -1) {
                var currIn = i
                var side   = 0
                while (lSide(currIn) == -1) {
                    lSide(currIn) = side
                    val currOut = pi(currIn)
                    rSide(currOut) = side

                    val nextOut = currOut ^ 1
                    val nextIn  = invPi(nextOut)

                    lSide(nextIn)  = 1 - side
                    rSide(nextOut) = 1 - side

                    currIn = nextIn ^ 1
                    side   = 0
                }
            }

            val mirrorStage = numStages - 1 - s
            for (i <- 0 until half) {
                config(s)(offset + i)           = lSide(2 * i) == 1
                config(mirrorStage)(offset + i) = rSide(2 * i) == 1
            }

            val topPi = new Array[Int](half)
            val botPi = new Array[Int](half)
            for (i <- 0 until half) {
                val inIdxTop  = if (lSide(2 * i) == 0) 2 * i else 2 * i + 1
                val outIdxTop = pi(inIdxTop)
                topPi(i) = outIdxTop / 2

                val inIdxBot  = if (lSide(2 * i) == 1) 2 * i else 2 * i + 1
                val outIdxBot = pi(inIdxBot)
                botPi(i) = outIdxBot / 2
            }

            solve(half, topPi, s + 1, offset)
            solve(half, botPi, s + 1, offset + half / 2)
        }

        solve(numPorts, pi, 0, 0)
        config
    }
}
