package Hyacine.SwitchNetwork

object BenesRouter {

    /** 计算 Benes 网络的配置位
      * @param n
      *   端口数 (2的幂)
      * @param pi
      *   置换向量，pi(i) = j 表示输入 i 到输出 j
      * @return
      *   Array[Array[Boolean]] 维度为 [numStages][numSwitchesPerStage]
      */
    def generateConfig(n: Int, pi: Array[Int]): Array[Array[Boolean]] = {
        val logN             = (math.log(n) / math.log(2)).toInt
        val numStages        = 2 * logN - 1
        val switchesPerStage = n / 2
        val config           = Array.fill(numStages)(Array.fill(switchesPerStage)(false))

        def solve(nPtrs: Int, currentPi: Array[Int], stageOffset: Int, switchOffset: Int): Unit = {
            if (nPtrs < 2) return

            val half = nPtrs / 2
            val L    = Array.fill(nPtrs)(-1) // 记录每个输入去往哪个子网 (0: 上, 1: 下)
            val R    = Array.fill(nPtrs)(-1) // 记录每个输出来自哪个子网 (0: 上, 1: 下)

            val invPi = Array.fill(nPtrs)(0)
            for (i <- 0 until nPtrs) invPi(currentPi(i)) = i

            // Looping Algorithm 核心循环
            for (i <- 0 until nPtrs if L(i) == -1) {
                var currIn = i
                var side   = 0 // 从上半区开始尝试

                while (L(currIn) == -1) {
                    L(currIn) = side
                    val currOut = currentPi(currIn)
                    R(currOut) = side

                    // 寻找同一个输出交换单元的另一个输入端口
                    val nextOut = currOut ^ 1
                    val nextIn  = invPi(nextOut)

                    R(nextOut) = 1 - side
                    L(nextIn)  = 1 - side

                    // 寻找下一个输入交换单元的配对端口并继续循环
                    currIn = nextIn ^ 1
                    side   = 0 // 重新调整或保持平衡
                }
            }

            // 1. 设置当前级 (Stage 0) 和 对应镜像级 (Last Stage) 的配置位
            for (i <- 0 until half) {
                // 输入级配置：如果 L(2*i) == 1，说明偶数输入去了下半区 -> 交叉(true)
                val s1 = L(2 * i) == 1
                config(stageOffset)(switchOffset + i) = s1

                // 输出级配置（如果不是最后一级）：根据输出侧来自哪个子网决定
                if (nPtrs > 2) {
                    val lastStageIdx = numStages - 1 - stageOffset
                    val s2           = R(2 * i) == 1
                    config(lastStageIdx)(switchOffset + i) = s2
                }
            }

            // 2. 递归构造子置换
            if (nPtrs > 2) {
                val topPi = Array.fill(half)(0)
                val botPi = Array.fill(half)(0)

                for (i <- 0 until half) {
                    // 输入 2*i 和 2*i+1 会被分流
                    val (inTop, inBot)   = if (L(2 * i) == 0) (2 * i, 2 * i + 1) else (2 * i + 1, 2 * i)
                    val (outTop, outBot) = if (R(2 * i) == 0) (2 * i, 2 * i + 1) else (2 * i + 1, 2 * i)

                    // 子网内的索引映射：映射到 0..half-1
                    topPi(i) = invPi(outTop) / 2 // 这里逻辑需谨慎，确保对应子网索引
                    // 注意：实际实现需维护全局到局部索引的映射，此处为简化版思路
                }

                // 修正：正确的递归参数提取
                val subTop = new Array[Int](half)
                val subBot = new Array[Int](half)
                for (i <- 0 until half) {
                    val p0 = currentPi(if (L(2 * i) == 0) 2 * i else 2 * i + 1)
                    subTop(i) = p0 / 2
                    val p1 = currentPi(if (L(2 * i) == 1) 2 * i else 2 * i + 1)
                    subBot(i) = p1 / 2
                }

                solve(half, subTop, stageOffset + 1, switchOffset)
                solve(half, subBot, stageOffset + 1, switchOffset + half / 2)
            }
        }

        solve(n, pi, 0, 0)
        config
    }
}
