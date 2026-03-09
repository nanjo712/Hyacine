package Hyacine.SwitchNetwork

object BenesRouter {

    /** @param numPorts
      *   端口总数 N
      * @param pi
      *   置换向量, pi(i) = j
      * @return
      *   Array[Array[Boolean]] 维度 [numStages][N/2]
      */
    def generateConfig(numPorts: Int, pi: Array[Int]): Array[Array[Boolean]] = {
        val n                = (math.log(numPorts) / math.log(2)).toInt
        val numStages        = 2 * n - 1
        val switchesPerStage = numPorts / 2
        val config           = Array.fill(numStages)(Array.fill(switchesPerStage)(false))

        // 递归求解函数
        // n: 当前子网规模, pi: 当前子网置换, s: 递归深度, offset: 开关在当前级的起始位置
        def solve(n: Int, pi: Array[Int], s: Int, offset: Int): Unit = {
            if (n == 2) {
                // 递归基：2x2 交换单元，配置位即为 pi(0) == 1 (如果0去1，则为交叉)
                config(s)(offset) = pi(0) == 1
                return
            }

            val half  = n / 2
            val lSide = Array.fill(n)(-1) // 输入侧去向：0-上子网, 1-下子网
            val rSide = Array.fill(n)(-1) // 输出侧来源：0-上子网, 1-下子网

            val invPi = Array.fill(n)(0)
            for (i <- 0 until n) invPi(pi(i)) = i

            // --- Looping Algorithm 核心逻辑 ---
            for (i <- 0 until n if lSide(i) == -1) {
                var currIn = i
                var side   = 0
                while (lSide(currIn) == -1) {
                    lSide(currIn) = side
                    val currOut = pi(currIn)
                    rSide(currOut) = side

                    // 约束 1: 对应输出交换单元的另一个输入来自另一个子网
                    val nextOut = currOut ^ 1
                    val nextIn  = invPi(nextOut)

                    // 约束 2: 对应输入交换单元的另一个输入去往另一个子网
                    lSide(nextIn)  = 1 - side
                    rSide(nextOut) = 1 - side

                    currIn = nextIn ^ 1
                    side   = 0 // 闭环回到起始 side 的逻辑
                }
            }

            // 填充当前级和镜像级的配置位
            val mirrorStage = numStages - 1 - s
            for (i <- 0 until half) {
                // 输入级：如果 L 侧偶数端口去了下子网(1)，则该开关设为交叉(true)
                config(s)(offset + i)           = lSide(2 * i) == 1
                // 输出级：如果 R 侧偶数端口来自下子网(1)，则该开关设为交叉(true)
                config(mirrorStage)(offset + i) = rSide(2 * i) == 1
            }

            // 准备子网置换向量
            val topPi = new Array[Int](half)
            val botPi = new Array[Int](half)
            for (i <- 0 until half) {
                // 找出谁去了上子网
                val inIdxTop  = if (lSide(2 * i) == 0) 2 * i else 2 * i + 1
                val outIdxTop = pi(inIdxTop)
                topPi(i) = outIdxTop / 2

                // 找出谁去了下子网
                val inIdxBot  = if (lSide(2 * i) == 1) 2 * i else 2 * i + 1
                val outIdxBot = pi(inIdxBot)
                botPi(i) = outIdxBot / 2
            }

            // 递归：深度加1，开关偏移量根据树状结构分布
            solve(half, topPi, s + 1, offset)
            solve(half, botPi, s + 1, offset + half / 2)
        }

        solve(numPorts, pi, 0, 0)
        config
    }
}
