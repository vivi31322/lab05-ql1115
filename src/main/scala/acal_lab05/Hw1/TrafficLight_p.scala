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

  //parameter declaration
  val Off = 0.U
  val Red = 1.U
  val Yellow = 2.U
  val Green = 3.U

  // sHGVR: 水平路燈 綠燈，垂直路段 紅燈
  // sHYVR: 水平路燈 黃燈，垂直路段 紅燈
  // sHRVG: 水平路燈 紅燈，垂直路段 綠燈
  // sHRVY: 水平路燈 紅燈，垂直路段 黃燈
  // sPG  : 人行道
  val sIdle :: sHGVR :: sHYVR :: sHRVG :: sHRVY :: sPG :: Nil = Enum(6)

  //State register
  val state = RegInit(sIdle)

  // state after sPG，預設是無 P_button 干擾下的 sHGVR
  val afterPgState = RegInit(sHGVR)

  //Counter============================
  val cntMode = WireDefault(0.U(2.W))
  val cntReg = RegInit(0.U(4.W))
  val cntDone = Wire(Bool())
  cntDone := cntReg === 0.U      // 倒數計時完成

  when(cntDone){
    when(cntMode === 0.U){       // 綠燈開始倒數計時
      cntReg := (Gtime-1).U
    }.elsewhen(cntMode === 1.U){ // 黃燈開始倒數計時
      cntReg := (Ytime-1).U
    }.elsewhen(cntMode === 2.U){ // 人行道開始倒數計時
      cntReg := (Ptime-1).U
    }
  // P_button 並且不是在 sPG 狀態下，從會從 Ptime 開始倒數
  }.elsewhen(io.P_button && state =/= sPG) {
    cntReg := (Ptime-1).U
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
      when(io.P_button) {
        state := sPG
        afterPgState := sHGVR
      }.elsewhen(cntDone) {state := sHYVR}
    }
    is(sHYVR){
      when(io.P_button) {
        state := sPG
        afterPgState := sHYVR
      }.elsewhen(cntDone) {state := sHRVG}
    }
    is(sHRVG){
      when(io.P_button) {
        state := sPG
        afterPgState := sHRVG
      }.elsewhen(cntDone) {state := sHRVY}
    }
    is(sHRVY){
      when(io.P_button) {
        state := sPG
        afterPgState := sHRVY
      }.elsewhen(cntDone) {state := sPG}
    }
    is(sPG){
      when(cntDone) {
        // afterPgState 若是沒被改變，代表 P_button 沒有出現
        // 若是被改變了，代表 P_button 發生之前的 state 是什麼，所以 sPG 之後要跳回去
        state := afterPgState
        afterPgState := sHGVR
      }
    }
  }

  //Output Decoder
  //Default statement
  cntMode := 0.U
  io.H_traffic := Off
  io.V_traffic := Off
  io.P_traffic := Off

  switch(state){
    is(sHGVR){
      cntMode := Mux(io.P_button, 2.U, 1.U) //
      io.H_traffic := Green
      io.V_traffic := Red
      io.P_traffic := Red
    }
    is(sHYVR){
      cntMode := Mux(io.P_button, 2.U, 0.U) //
      io.H_traffic := Yellow
      io.V_traffic := Red
      io.P_traffic := Red
    }
    is(sHRVG){
      cntMode := Mux(io.P_button, 2.U, 1.U) //
      io.H_traffic := Red
      io.V_traffic := Green
      io.P_traffic := Red
    }
    is(sHRVY){
      // cntMode := Mux(io.P_button, 2.U, 0.U)
      cntMode := 2.U // 本來就要跳到 sPG 去，從 Ptime 開始倒數
      io.H_traffic := Red
      io.V_traffic := Yellow
      io.P_traffic := Red
    }
    is(sPG){ 
      cntMode := 0.U
      switch (afterPgState){
        is (sHGVR, sHRVG) {cntMode := 0.U}
        is (sHRVY, sHYVR) {cntMode := 1.U}
      }
      io.H_traffic := Red
      io.V_traffic := Red
      io.P_traffic := Green
    }
  }

  io.timer := cntReg
}