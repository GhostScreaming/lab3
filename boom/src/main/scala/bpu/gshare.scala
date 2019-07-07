//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV GShare Branch Predictor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2015 Apr 28


package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}


case class GShareParameters(
   enabled: Boolean = true,
   history_length: Int = 12,
   // The prediction table requires 1 read and 1 write port.
   // Should we use two ports or should we bank the p-table?
   dualported: Boolean = false
   )

class GShareResp(index_sz: Int) extends Bundle
{
   val index = UInt(width = index_sz) // needed to update predictor at Commit
   override def cloneType: this.type = new GShareResp(index_sz).asInstanceOf[this.type]
}

object GShareBrPredictor
{
   def GetRespInfoSize(p: Parameters, hlen: Int): Int =
   {
      val dummy = new GShareResp(hlen)
      dummy.getWidth
   }
}

class GShareBrPredictor(
   fetch_width: Int,
   history_length: Int = 12,
   dualported: Boolean = false
   )(implicit p: Parameters)
   extends BrPredictor(fetch_width, history_length)(p)
{
   val num_entries = 1 << history_length

   println ("\tBuilding (" + (num_entries * fetch_width * 2/8/1024) +
      " kB) GShare Predictor, with " + history_length + " bits of history for (" +
      fetch_width + "-wide fetch) and " + num_entries + " entries.")

   require (coreInstBytes == 4)

   //------------------------------------------------------------

   private def Hash (addr: UInt, hist: UInt) =
      (addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ hist

   //------------------------------------------------------------
   // Predictor state.

   val counters = Module(new TwobcCounterTable(fetch_width, num_entries, dualported))

   //------------------------------------------------------------
   // Get prediction.

   val stall = !io.resp.ready

   val s1_pc = io.req_pc
   val s1_ridx = Hash(s1_pc, this.ghistory)

   counters.io.s1_r_idx := s1_ridx
   counters.io.stall := stall

   val resp_info = Wire(new GShareResp(log2Up(num_entries)))
   resp_info.index      := RegNext(s1_ridx)
   io.resp.bits.takens  := counters.io.s2_r_out
   io.resp.bits.info    := resp_info.asUInt

   // Always overrule the BTB, which will almost certainly have less history.
   io.resp.valid := Bool(true) && !this.disable_bpd

   //------------------------------------------------------------
   // Update counter table.

   val commit_info = new GShareResp(log2Up(num_entries)).fromBits(this.commit.bits.info.info)

   counters.io.update.valid                 := this.commit.valid && !this.disable_bpd
   counters.io.update.bits.index            := commit_info.index
   counters.io.update.bits.executed         := this.commit.bits.ctrl.executed
   counters.io.update.bits.takens           := this.commit.bits.ctrl.taken
   counters.io.update.bits.was_mispredicted := this.commit.bits.ctrl.mispredicted.reduce(_|_)
   counters.io.update.bits.do_initialize    := Bool(false)

   //------------------------------------------------------------
}

