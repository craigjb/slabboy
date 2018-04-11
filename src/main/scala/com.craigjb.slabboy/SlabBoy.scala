package slabboy

import spinal.core._
import spinal.lib._

class SlabBoy extends Component {
  val io = new Bundle {
    val address = out UInt(16 bits)
    val dataIn = in UInt(8 bits)
    val en = out Bool
  }

  val cpu = new CPU(
    bootVector = 0x0000,
    spInit = 0xFFFE
  )

  io.address := cpu.io.address
  cpu.io.dataIn := io.dataIn
  io.en := cpu.io.mreq
}

object CPU {
  object Reg16 {
    val WZ = 0
    val BC = 1
    val DE = 2
    val HL = 3
    val SP = 4
    val PC = 5
  }

  object Reg8 {
    val W = 0; val Z = 1
    val B = 2; val C = 3
    val D = 4; val E = 5
    val H = 6; val L = 7
    val SPH = 8; val SPL = 9
    val PCL = 10; val PCH = 11
  }
}

class CPU(bootVector: Int, spInit: Int) extends Component {
  import CPU._

  val io = new Bundle {
    val address = out UInt(16 bits)
    val dataIn = in UInt(8 bits)
    val mreq = out Bool
  }

  // instruction register
  val ir = RegInit(U(0x00, 8 bits))

  // register file
  val registers16 = Vec(Reg(UInt(16 bits)), 6)
  // WZ, BC, DE, and HL are all initialized to zero
  for (i <- (0 until 4)) {
    registers16(i).init(0)
  }
  // SP and PC have defined init values
  registers16(Reg16.SP).init(spInit)
  registers16(Reg16.PC).init(bootVector)

  // 8-bit register vector for easy access
  val registers8 = registers16.flatMap(
    reg16 => Seq(reg16(15 downto 8), reg16(7 downto 0))
  )
}

object TopLevelVerilog {
  def main(args: Array[String]) {
    SpinalConfig()
      .dumpWave(vcdPath = "wave.vcd")
      .generateVerilog(new SlabBoy)
  }
}
