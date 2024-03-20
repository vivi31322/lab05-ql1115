package acal_lab05.Hw2

import chisel3._
import chisel3.util._

// input	 0	 1	 2	 3	 4	 5	 6	 7	 8	 9	 +	 -	 *	 (	 )	 =
//  HW	  0x0	0x1	0x2	0x3	0x4	0x5	0x6	0x7	0x8	0x9	0xA	0xB	0xC	0xD	0xE	0xF

class LongCal extends Module{
    val io = IO(new Bundle{
        val key_in = Input(UInt(4.W))
        val value = Output(Valid(UInt(32.W)))
    })

    //State and Constant Declaration=====================
    val sIdle :: sSrc1 :: sOp :: sSrc2 :: sEqual :: Nil = Enum(5)
    val add = 0.U
    val sub = 1.U

    val state = RegInit(sIdle)

    //Wire Declaration===================================
    val is_plus_minus = WireDefault(false.B)
    is_plus_minus := io.key_in === 10.U || io.key_in === 11.U // 0xA: +, 0xB: -

    val is_num = WireDefault(false.B)
    is_num := io.key_in < 10.U // 0~9 用 0x1~0x9 來表示

    val is_equal = WireDefault(false.B)
    is_equal := io.key_in === 15.U // 0xF: =

    val is_left_bracket = WireDefault(false.B) // 左括號
    is_left_bracket := io.key_in === 13.U

    val is_right_bracket = WireDefault(false.B) // 右括號 => 要將括號內的變負數 // TODO 預設只有負數才會括號
    is_right_bracket := RegNext(io.key_in) === 14.U // TODO 小心？？要用 in_buffer 來判斷 ) ???

    //Reg Declaration====================================
    val in_buffer = RegNext(io.key_in)
    val src1 = RegInit(0.U(32.W))
    val op = RegInit(0.U(2.W))
    val src2 = RegInit(0.U(32.W))

    val partial_result = RegInit(0.U(32.W)) // 記錄目前的計算結果
    when(state === sOp || state === sEqual) {
      partial_result := partial_result + MuxLookup(op, partial_result, Seq(
          add -> (src1 + src2),
          sub -> (src1 - src2)
      ))
    } .elsewhen(state === sIdle) {partial_result := 0.U} // 初始化 partial_result 為 0

    val is_in_brackets = RegInit(false.B)
    when(io.key_in === 13.U) {is_in_brackets := true.B}
    .elsewhen(io.key_in === 14.U) {is_in_brackets := false.B} // TODO 待確認


    //Next State Decoder
    switch(state){
        is(sIdle){
            state := sSrc1
        }
        is(sSrc1){
            when(is_equal) {state := sEqual}
            .elsewhen(!is_in_brackets && is_plus_minus) {state := sOp}
        }
        is(sOp){
            when(is_num || is_left_bracket) {state := sSrc2}
        }
        is(sSrc2){
            when(is_equal) {state := sEqual}
            .elsewhen(!is_in_brackets && is_plus_minus) {state := sOp}
        }
        is(sEqual){
            state := sIdle // 新的一輪從 sIdle 開始
        }
    }
    //==================================================

    when(state === sSrc1){when(in_buffer < 10.U) {src1 := (src1<<3.U) + (src1<<1.U) + in_buffer}} // 乘以10
    when(state === sSrc2){when(in_buffer < 10.U) {src2 := (src2<<3.U) + (src2<<1.U) + in_buffer}} // 乘以10
    when(state === sOp){op := in_buffer - 10.U} // add:0.U, sub:1.U, 又 +:0xA, -:0xB

    // 根據是否為負數，修正 src1 或 src2 // TODO 這裡假設括號中的是負數
    when(is_right_bracket && state === sSrc1) {src1 := ~src1 + 1.U}
    when(is_right_bracket && state === sSrc2) {src2 := ~src2 + 1.U}

    when(state === sIdle){ // 初始化
        src1 := 0.U
        src2 := 0.U
        op := 0.U
        in_buffer := 0.U
    }
    when(state === sOp) { // 小心，為了記錄下一個數字，要把之前的清掉
      src1 := 0.U
      src2 := 0.U
    }

    io.value.valid := Mux(RegNext(state) === sEqual,true.B,false.B)
    io.value.bits := partial_result
}