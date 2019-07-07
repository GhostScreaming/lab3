package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 9,
  info_size: Int = 46)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
    fetch_width: Int,
    history_length: Int)(implicit p: Parameters)
      extends BrPredictor(fetch_width, history_length)(p)
{

  // Predictor state: Table initialized to 0
  val local_h_num_entries = 128
  val local_h_num_width = history_length-2
  val local_p_num_entries = 128
  val local_p_num_width = 3
  val global_num_entries = 512
  val global_num_width = 2
  val choose_num_entries = 512
  val choose_num_width = 2

  val local_h_counters = Reg(
    init = Vec(Seq.fill(local_h_num_entries) { UInt("b0000000", width = local_h_num_width) }))
  val local_p_counters = Reg(
    init = Vec(Seq.fill(local_p_num_entries) { UInt("b000", width = local_p_num_width) }))
  val global_counters = Reg(
    init = Vec(Seq.fill(global_num_entries) { UInt("b00", width = global_num_width) }))
  val choose_counters = Reg(
    init = Vec(Seq.fill(choose_num_entries) { UInt("b00", width = choose_num_width) }))

  // pause prediction
  val stall = !io.resp.ready

  // index into the table to get the count
  val pred_pc = io.req_pc
  val global_history = this.ghistory

  val local_h_idx = pred_pc(history_length-1,2)
  val local_h_count = local_h_counters(local_h_idx)

  val local_p_idx = local_h_count ^ pred_pc(history_length-1,2)
  val local_p_count = local_p_counters(local_p_idx)

  val global_idx = global_history ^ pred_pc(history_length+1,2)
  val global_count = global_counters(global_idx)

  val choose_idx = global_history ^ pred_pc(history_length+1,2)
  val choose_count = choose_counters(choose_idx)

  //val count = RegEnable(Mux(choose_count(global_num_width-1),local_p_count(local_p_num_width-1),global_count(global_num_width-1)),!stall)
  val count = RegEnable(global_count(global_num_width-1),!stall)

  // keep sending predictions as long as not disabled
  io.resp.valid := !this.disable_bpd

  // prediction is the upper bit of two-bit counter
  io.resp.bits.takens := count

  // tell the pipeline to save the index for commit
  io.resp.bits.info := RegNext(Cat(global_count(global_num_width-1),local_p_count(local_p_num_width-1),global_idx,local_p_idx,local_h_idx))


  // on commit, get the index and whether the branch was actually taken
  val commit1_en = this.commit.valid
  val commit1_info = this.commit.bits.info.info
  val commit1_taken = this.commit.bits.ctrl.taken(0)

  val commit1_local_h_idx = commit1_info(local_h_num_width-1,0)
  val commit1_local_p_idx = commit1_info(2*local_h_num_width-1,local_h_num_width)
  val commit1_global_history = commit1_info(2*local_h_num_width+history_length-1,2*local_h_num_width)
  val commit1_pred_local = commit1_info(2*local_h_num_width+history_length)
  val commit1_pred_global = commit1_info(2*local_h_num_width+history_length+1)

  // index into table to get previous state
  val commit2_local_h_idx = RegEnable(commit1_local_h_idx, commit1_en)
  val commit2_local_h_count = RegEnable(local_h_counters(commit1_local_h_idx), commit1_en)

  val commit2_local_p_idx = RegEnable(commit1_local_p_idx, commit1_en)
  val commit2_local_p_count = RegEnable(local_p_counters(commit1_local_p_idx), commit1_en)

  val commit2_global_idx = RegEnable(commit1_global_history, commit1_en)
  val commit2_global_p_count = RegEnable(global_counters(commit1_global_history), commit1_en)

  val commit2_choose_idx = RegEnable(commit1_global_history, commit1_en)
  val commit2_choose_p_count = RegEnable(choose_counters(commit1_global_history), commit1_en)

  val commit2_taken = RegEnable(commit1_taken, commit1_en)
  val commit2_pred_local = RegEnable(commit1_pred_local, commit1_en)
  val commit2_pred_global = RegEnable(commit1_pred_global, commit1_en)
  val commit2_en = RegNext(commit1_en)

  // calculate updated counter value
  val commit2_local_h_update = Cat(commit2_taken,commit2_local_h_count(local_h_num_width-2,0))

  val commit2_local_p_update = Mux(commit2_taken,
    Mux(commit2_local_p_count === "b111".U, commit2_local_p_count, commit2_local_p_count + 1.U),
    Mux(commit2_local_p_count === "b000".U, commit2_local_p_count, commit2_local_p_count - 1.U))

  val commit2_global_update = Mux(commit2_taken,
    Mux(commit2_global_p_count === "b11".U, commit2_global_p_count, commit2_global_p_count + 1.U),
    Mux(commit2_global_p_count === "b00".U, commit2_global_p_count, commit2_global_p_count - 1.U))

  val commit2_choose_update = Mux(commit2_pred_local === commit2_pred_global, commit2_choose_p_count,
    Mux(commit2_taken === commit2_pred_local,
    Mux(commit2_choose_p_count === "b11".U, commit2_choose_p_count, commit2_choose_p_count + 1.U),
    Mux(commit2_choose_p_count === "b00".U, commit2_choose_p_count, commit2_choose_p_count - 1.U)))

  // write back to table
  when (commit2_en)
  {
    local_h_counters(commit2_local_h_idx) := commit2_local_h_update
    local_p_counters(commit2_local_p_idx) := commit2_local_p_update
    global_counters(commit2_global_idx) := commit2_global_update
    choose_counters(commit2_choose_idx) := commit2_choose_update
  }
}
