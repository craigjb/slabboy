package slabboy

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import java.nio.file.{Files, Paths}

object TopLevelSim {
  def loadProgram(name: String): Array[Byte] = {
    Files.readAllBytes(Paths.get(name))
  }

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new SlabBoy).doSim { dut =>
      // perform initial reset and generate a clock signal
      dut.clockDomain.forkStimulus(period = 2)
      
      // create a concurrent thread to handle the memory accesses
      // separate from the main test bench execution
      val memThread = fork {
        val memory = loadProgram("sw/test.gb")
        while (true) {
          dut.clockDomain.waitRisingEdgeWhere(dut.io.en.toBoolean == true)
          val address = dut.io.address.toInt
          dut.io.dataIn #= memory(address)
        }
      }

      sleep(100)
    }
  }
}
