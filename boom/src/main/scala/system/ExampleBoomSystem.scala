// See LICENSE.SiFive for license details.

package boom.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.DontTouch

/** Example Top with periphery devices and ports, and a Boom coreplex */
class ExampleBoomSystem(implicit p: Parameters) extends BoomCoreplex
    with HasAsyncExtInterrupts
    with HasMasterAXI4MemPort
    with HasMasterAXI4MMIOPort
    with HasSlaveAXI4Port
    with HasPeripheryBootROM
    with HasSystemErrorSlave {
  override lazy val module = new ExampleBoomSystemModule(this)
}

class ExampleBoomSystemModule[+L <: ExampleBoomSystem](_outer: L) extends BoomCoreplexModule(_outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with HasMasterAXI4MemPortModuleImp
    with HasMasterAXI4MMIOPortModuleImp
    with HasSlaveAXI4PortModuleImp
    with HasPeripheryBootROMModuleImp
    with DontTouch
