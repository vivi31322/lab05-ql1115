package acal_lab05.Hw1

import chisel3._
import chisel3.util._

class TrafficLight_p(Ytime:Int, Gtime:Int, Ptime:Int) extends Module{
  val io = IO(new Bundle{
    val P_button = Input(Bool())
    val H_traffic = Output(UInt(2.W))
    val V_traffic = Output(UInt(2.W))
    val P_traffic = Output(UInt(2.W))
    val timer     = Output(UInt(5.W))
  })

  // //please implement your code below...
  // io.H_traffic := 0.U
  // io.V_traffic := 0.U
  // io.P_traffic := 0.U
  // io.timer := 0.U

  //parameter declaration
  val Off = 0.U
  val Red = 1.U
  val Yellow = 2.U
  val Green = 3.U

  // sHGVR: 水平路燈 綠燈，垂直路段 紅燈
  // sHYVR: 水平路燈 黃燈，垂直路段 紅燈
  // sHRVG: 水平路燈 紅燈，垂直路段 綠燈
  // sHRVY: 水平路燈 紅燈，垂直路段 黃燈
  val sIdle :: sHGVR :: sHYVR :: sHRVG :: sHRVY :: Nil = Enum(5)

  //State register
  val state = RegInit(sIdle)

  //Counter============================
  val cntMode = WireDefault(0.U(1.W))
  val cntReg = RegInit(0.U(4.W))
  val cntDone = Wire(Bool())
  cntDone := cntReg === 0.U      // 倒數計時完成

  when(cntDone){
    when(cntMode === 0.U){       // 綠燈開始倒數計時
      cntReg := (Gtime-1).U
    }.elsewhen(cntMode === 1.U){ // 黃燈開始倒數計時
      cntReg := (Ytime-1).U
    }
  }.otherwise{
    cntReg := cntReg - 1.U
  }
  //Counter end========================


  //Next State Decoder
  switch(state){
    is(sIdle){
      state := sHGVR
    }
    is(sHGVR){
      when(cntDone) {state := sHYVR}
    }
    is(sHYVR){
      when(cntDone) {state := sHRVG}
    }
    is(sHRVG){
      when(cntDone) {state := sHRVY}
    }
    is(sHRVY){
      when(cntDone) {state := sHGVR}
    }
  }

  //Output Decoder
  //Default statement
  cntMode := 0.U
  io.H_traffic := Off
  io.V_traffic := Off

  switch(state){
    is(sHGVR){
      cntMode := 1.U    // 小心
      io.H_traffic := Green
      io.V_traffic := Red
    }
    is(sHYVR){
      cntMode := 0.U    // 小心
      io.H_traffic := Yellow
      io.V_traffic := Red
    }
    is(sHRVG){
      cntMode := 1.U
      io.H_traffic := Red
      io.V_traffic := Green
    }
    is(sHRVY){
      cntMode := 0.U
      io.H_traffic := Red
      io.V_traffic := Yellow
    }
  }

  io.timer := cntReg

  val ss = Module(new SevenSeg())
  ss.io.num := cntReg
  io.display := ss.io.display
}