package acal_lab05.Hw3

import chisel3._
import chisel3.util._

import acal_lab05.Lab3._

class PRNG(seed:Int) extends Module{
    val io = IO(new Bundle{
        val gen = Input(Bool())
        val puzzle = Output(Vec(4,UInt(4.W)))
        val ready = Output(Bool())
    })

    // 三個 state，初始 state 為 sIdle
    val sIdle :: sGen :: sOut :: Nil = Enum(3)
    val state = RegInit(sIdle)

    // 16-bit LFSR
    val lfsr_out = RegInit(0.U(16.W))

    val lfsr16 = Module(new LFSR_Fibonacci(16)).io // 呼叫 Lab3 的 LFSR_Fibonacci
    lfsr16.seed.bits := seed.asUInt
    lfsr16.seed.valid := 1.U // 這裡的 valid 只有在第 1 個 cycle 會生效，LFSR_Fibnoacci.scala 中有修改
    lfsr_out := lfsr16.rndNum

    // 分成四個數字
    val lfsr_out_split = VecInit((0 until 4).map(i => lfsr_out(4*(i+1)-1, 4*i)))

    // 替換 >= 10 的數字：0xA -> 0, 0xB -> 1, 0xC -> 2, 0xD -> 3, 0xE -> 4, 0xF -> 5
    val lfsr_out_adjusted = VecInit(lfsr_out_split.map(num => Mux(num >= 10.U, num - 10.U, num)))
    
    // 檢查替換後的 4 個數字中是否有一樣的，如果有一樣的，則下一個 state 仍是 sGen，並且再取一次 lfsr
    val is_done = WireDefault(false.B)
    is_done := (lfsr_out_adjusted(0) =/= lfsr_out_adjusted(1) && lfsr_out_adjusted(0) =/= lfsr_out_adjusted(2) && lfsr_out_adjusted(0) =/= lfsr_out_adjusted(3) 
                && lfsr_out_adjusted(1) =/= lfsr_out_adjusted(2) && lfsr_out_adjusted(1) =/= lfsr_out_adjusted(3) && lfsr_out_adjusted(2) =/= lfsr_out_adjusted(3))

    //// Next State
    switch (state) {
        is (sIdle) {
            when(io.gen) {state := sGen}
        }
        is (sGen) {
            when(is_done) {state := sOut}
        }
        is (sOut) {state := sIdle}
    }

    // 將 reg 設為初始值
    when (state === sIdle) {
        lfsr_out := 0.U
    }

    //// Output
    io.puzzle := RegNext(lfsr_out_adjusted) // 因為在 is_done 為 1 時仍在 sGen，下一個 cycle 才會到 sOut，所以用 RegNext
    io.ready := (state === sOut)
}
