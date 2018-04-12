package slabboy

import spinal.core._
import spinal.sim._
import spinal.core.sim._

object TopLevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(new SlabBoy).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 2)
      
      val memThread = fork {
        var i = 0
        while (true) {
          dut.clockDomain.waitRisingEdgeWhere(dut.io.en.toBoolean == true)
          dut.io.dataIn #= i % 256
          i += 1
        }
      }

      sleep(100)
    }
  }
}
