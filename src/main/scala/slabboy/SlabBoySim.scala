package slabboy

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import java.nio.file.{Files, Paths}

object TopLevelSim {
  def loadProgram(path: String): Array[Byte] = {
    Files.readAllBytes(Paths.get(path))
  }

  def coreDump(path: String, mem: Array[Byte]) {
    Files.write(Paths.get(path), mem)
  }

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new SlabBoy).doSim { dut =>
      // perform initial reset and generate a clock signal
      dut.clockDomain.forkStimulus(period = 2)
      
      // create a concurrent thread to handle the memory accesses
      // separate from the main test bench execution
      val memory = loadProgram("sw/test.gb")
      fork {
        while (true) {
          dut.clockDomain.waitRisingEdgeWhere(dut.io.en.toBoolean == true)
          val address = dut.io.address.toInt
          if (dut.io.write.toBoolean) {
            memory(address) = dut.io.dataOut.toInt.toByte
            dut.clockDomain.waitRisingEdgeWhere(dut.io.en.toBoolean == false)
          } else {
            dut.io.dataIn #= memory(address).toInt & 0xFF
            dut.clockDomain.waitRisingEdgeWhere(dut.io.en.toBoolean == false)
            dut.io.dataIn.randomize
          }
        }
      }

      dut.clockDomain.waitRisingEdgeWhere(dut.io.halt.toBoolean == true)
      sleep(2)
      coreDump("coredump.bin", memory)
      simSuccess()
    }
  }
}
