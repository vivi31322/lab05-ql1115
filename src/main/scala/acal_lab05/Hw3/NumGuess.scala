package acal_lab05.Hw3

import chisel3._
import chisel3.util._

class NumGuess(seed:Int = 1) extends Module{
    require (seed > 0 , "Seed cannot be 0")

    val io  = IO(new Bundle{
        val gen = Input(Bool())
        val guess = Input(UInt(16.W))
        val puzzle = Output(Vec(4,UInt(4.W)))
        val ready  = Output(Bool())
        val g_valid  = Output(Bool())
        val A      = Output(UInt(3.W))
        val B      = Output(UInt(3.W))

        //don't care at Hw6-3-2 but should be considered at Bonus
        val s_valid = Input(Bool())
    })

    //// 5 個 state
    // sIdle    : 等待 gen，gen 為 1 後，跳到 sPuzzle
    // sPuzzle  : 產生 puzzle，ready 拉高後，跳到 sGuess
    // sGuess   : 使用者輸入數字，並且判斷幾 A 幾 B。4A 跳到 sEqual，其餘跳到 sOut
    // sOut     : 拉高 g_valid 並且輸出幾 A 幾 B (除了 4A)，跳到 sGuess
    // sEqual   : 拉高 g_valid 並且輸出 4A，結束 （看要不要跳到 sIdle 準備下一輪）
    val sIdle :: sPuzzle :: sGuess :: sOut :: sEqual :: Nil = Enum(5)
    val state = RegInit(sIdle)

    val ctr = RegInit(0.U(2.W)) // 計算 sGuess 到第幾輪
    when(state === sGuess) {ctr := ctr + 1.U}
    .elsewhen (ctr === 3.U) {ctr := 0.U}

    //// PRNG 相關
    val prng = Module(new PRNG(seed)).io
    val prng_out = RegInit(VecInit(Seq.fill(4)(0.U(4.W))))  // TODO QA 不確定是否需要這一個 prng_out，或是直接 io.puzzle := prng.puzzle
    prng.gen := io.gen
    when (prng.ready) {prng_out := prng.puzzle} // TODO QA 應該是需要內部存一個 puzzle 產生的值，這樣才能比較
    io.puzzle := prng.puzzle
    io.ready  := prng.ready

    //// 判斷 幾 A 幾 B
    // 1. 先判斷幾 A，並在 A 的位置標為 true。
    // 2. 在 false 的那些位置看有沒有跟 puzzle 數字相同的
    // val guess_buffer = RegNext(io.guess)
    // val correct_pos = RegInit(0.U(4.W)) // 記錄 A 的位置
    val guess_vec = VecInit((0 until 4).map(i => io.guess(4*(i+1)-1, 4*i))) // TODO QA 順序是什麼
    val num_A = RegInit(0.U(3.W))
    val num_B = RegInit(0.U(3.W))

    when (state === sGuess) {
        when(prng_out(ctr) === guess_vec(ctr)) { 
            num_A := num_A + 1.U 
            // correct_pos := correct_pos | (1.U << ctr) // 將第 ctr 的 bit 設為 1，之後的 bit 不會再跟這個位置比較
        } .otherwise {
            when(guess_vec(ctr) === prng_out(0) && guess_vec(ctr) =/= guess_vec(0)) { num_B := num_B + 1.U }
            when(guess_vec(ctr) === prng_out(1) && guess_vec(ctr) =/= guess_vec(1)) { num_B := num_B + 1.U }
            when(guess_vec(ctr) === prng_out(2) && guess_vec(ctr) =/= guess_vec(2)) { num_B := num_B + 1.U }
            when(guess_vec(ctr) === prng_out(3) && guess_vec(ctr) =/= guess_vec(3)) { num_B := num_B + 1.U }
        }
    }.elsewhen (state === sOut || state === sEqual) {
        num_A := 0.U
        num_B := 0.U
        // correct_pos := 0.U
    }

    //// Next State
    switch (state) {
        is (sIdle) {  when(io.gen) { state := sPuzzle } }
        is (sPuzzle) { when(io.ready) { state := sGuess } }
        is (sGuess) {
            when(ctr === 3.U) { // 每回合比對 1 個數字
                when (num_A === 4.U) { state := sEqual }
                .otherwise { state := sOut }
            }
        }
        is (sOut) { state := sGuess }
        is (sEqual) { state := sIdle }
    }

    io.g_valid  := (state === sOut) || (state === sEqual)
    io.A        := num_A.asUInt
    io.B        := num_B.asUInt
}