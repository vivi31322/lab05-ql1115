package acal_lab05.Hw2

import chisel3._
import chisel3.util._

// input	 0	 1	 2	 3	 4	 5	 6	 7	 8	 9	 +	 -	 *	 (	 )	 =
//  HW	    0x0	0x1	0x2	0x3	0x4	0x5	0x6	0x7	0x8	0x9	0xA	0xB	0xC	0xD	0xE	0xF

class NegIntGen extends Module{
    val io = IO(new Bundle{
        val key_in = Input(UInt(4.W))
        val value = Output(Valid(UInt(32.W)))
    })

    // 判斷輸入是否為左右括號
    val left_bracket = WireDefault(false.B)
    left_bracket := io.key_in === 13.U
    val right_bracket = WireDefault(false.B)
    right_bracket := io.key_in === 14.U

    val equal = WireDefault(false.B)
    equal := io.key_in === 15.U

    val sIdle :: sAccept :: sEqual :: Nil = Enum(3)
    val state = RegInit(sIdle)
    //Next State Decoder
    switch(state){
        is(sIdle){
        state := sAccept
        }
        is(sAccept){
        when(equal) {state := sEqual}
        }
        is(sEqual){
            state := sAccept
        }
    }

    // 記住當前輸入的是正數還是負數
    val is_neg = RegInit(false.B)
    when(state === sEqual) {is_neg := false.B} // 預設是正數
    .elsewhen(io.key_in === 13.U) {is_neg := true.B} // 發現有 ( ，則表示是負數
    .otherwise {is_neg := is_neg} // 因為是 reg， 所以不會有 comb. loop

    val in_buffer = RegNext(io.key_in)

    val number = RegInit(0.U(32.W))
    when(state === sAccept){
        when(in_buffer < 10.U) { // 當確認輸入的是數字才計算，避免 (、 - 、 )
            // 乘以10 = 乘以8 + 乘以2
            number := (number<<3.U) + (number<<1.U) + in_buffer
        }
    }.elsewhen(state === sEqual){
        in_buffer := 0.U
        number := 0.U
    }

    io.value.valid := Mux(state === sEqual,true.B,false.B)
    io.value.bits := Mux(is_neg, ~number+1.U, number) // 如果是負數，則二補數(取反+1)；若是正數，則直接輸出
}