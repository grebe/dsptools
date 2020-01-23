package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform, annotate}
import chisel3.util.{HasBlackBoxInline, HasBlackBoxResource}
import firrtl.{CircuitState, LowForm, Transform, Utils}
import firrtl.annotations.{Annotation, ModuleName, ModuleTarget, SingleTargetAnnotation}
import firrtl.ir.{ClockType, DefModule, GroundType, IntWidth, Port, ResetType, SIntType}
import firrtl.transforms.BlackBoxInlineAnno

case class DPIModuleAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget): DPIModuleAnnotation =
    this.copy(target = target)
}

case class DPIModuleChiselAnnotation(target: InstanceId) extends ChiselAnnotation with RunFirrtlTransform {
  override def transformClass = classOf[DPIModuleTransform]

  override def toFirrtl: DPIModuleAnnotation = DPIModuleAnnotation(target.toTarget match {
    case m: ModuleTarget => m
    case t => Utils.error(s"Target $t is not a module, can't run DPIModuleTransform")
  })
}

class DPIModuleTransform extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def portWidth(p: Port) = p.tpe match {
    case GroundType(IntWidth(w)) => w
    case _ => Utils.error(
      s"Got non-ground type in DPIModuleTransform for port ${p.name} with type ${p.tpe}")
  }
  def portSign(p: Port) = p.tpe match {
    case _:SIntType => true
    case _ => false
  }

  def portToCPP(p: Port): String = {
    val w = portWidth(p)
    val signed = portSign(p)
    val tpe = (w, signed) match {
      case (x, true) if 0 to 8 contains x => "char"
      case (x, false) if 0 to 8 contains x => "char"
      case (x, true) if 9 to 16 contains x => "short"
      case (x, false) if 9 to 16 contains x => "short"
    }
    val (prefix, ref) = p.direction match {
      case firrtl.ir.Input => ("const ", "")
      case firrtl.ir.Output => ("", "*")
    }
    prefix + tpe + " " + ref + p.name
  }
  def portToSVDPI(p: Port): String = {
    val w = portWidth(p)
    val signed = portSign(p)
    val tpe = (w, signed) match {
      case (x, _) if x == 1 => "byte"
      case _ => "int"
    }
    val prefix = p.direction match {
      case firrtl.ir.Input => "input"
      case firrtl.ir.Output => "output"
    }
    prefix + " " + tpe + " " + p.name
  }
  def portToSVModule(p: Port): String = {
    val w = portWidth(p)
    val signed = portSign(p)
    val tpe = (if (signed) {
      "signed"
    } else {
      ""
    }) + (if (w > 1) {
      s"[${w - 1}:0]"
    } else {
      ""
    })
    val prefix = p.direction match {
      case firrtl.ir.Input => "input"
      case firrtl.ir.Output => "output"
    }
    prefix + " " + tpe + " " + p.name
  }
  def portToSVName(p: Port): String = {
    val prefix = p.direction match {
      case firrtl.ir.Input => "in_"
      case firrtl.ir.Output => "out_"
    }
    prefix + p.name
  }
  def portToSVType(p: Port): String = {
    val tpe = portWidth(p) match {
      case x if 0 to 8 contains x => "byte"
      case x if 9 to 32 contains x => "int"
    }
    tpe + " " + portToSVName(p) + ";"
  }
  def portToSVAssign(p: Port): String = p.direction match {
    case firrtl.ir.Input => s"assign #0.1 ${portToSVName(p)} = ${p.name};"
    case firrtl.ir.Output => s"assign #0.1 ${p.name} = ${portToSVName(p)};"
  }
  def removeClockReset(ports: Seq[Port]): Seq[Port] = {
    ports.filter(p => (p.name, p.tpe) match {
      case (_, ClockType) => false
      case (_, ResetType) => false
      case ("clock", _) => false
      case ("reset", _) => false
      case _ => true
    })
  }
  def indent(level: Int = 2)(in: String): String = (" " * level) + in
  def makeCPP(name: String, ports: Seq[Port]): String = {
    val argumentList = removeClockReset(ports).map(portToCPP).map(indent()).mkString(",\n")
    s"""#include "svdpi.h"
       |extern "C" {
       |void ${name}_tick(
       |$argumentList
       |)
       |{
       |  *b = !a;
       |}
       |
       |void ${name}_reset(
       |$argumentList
       |)
       |{
       |  *b = 0;
       |}
       |
       |};
       |""".stripMargin
  }
  def makeSV(name: String, ports: Seq[Port]): String = {
    val dpiArgumentList = removeClockReset(ports).map(portToSVDPI).map(indent()).mkString(",\n")
    val modulePortList = ports.map(portToSVModule).map(indent()).mkString(",\n")
    val svTypeDeclares = removeClockReset(ports).map(portToSVType).mkString("\n")
    val svAssigns = removeClockReset(ports).map(portToSVAssign).mkString("\n")
    val svCallArgumentList = removeClockReset(ports).map(portToSVName).map(indent(6)).mkString(",\n")
    s"""import "DPI-C" function void ${name}_tick
       |(
       |$dpiArgumentList
       |);
       |import "DPI-C" function void ${name}_reset
       |(
       |$dpiArgumentList
       |);
       |
       |module $name(
       |$modulePortList
       |);
       |
       |$svTypeDeclares
       |
       |$svAssigns
       |
       |always @(posedge reset)
       |begin
       |  ${name}_reset(
       |$svCallArgumentList
       |  );
       |end
       |always @(posedge clock)
       |begin
       |  if (!reset)
       |  begin
       |    ${name}_tick(
       |$svCallArgumentList
       |    );
       |  end
       |end
       |endmodule
       |""".stripMargin
  }
  def makeAnnotations(target: ModuleTarget, ports: Seq[Port]): Seq[BlackBoxInlineAnno] = {
    Seq(
      BlackBoxInlineAnno(target, s"${target.name}.sv", makeSV(target.name, ports)),
      BlackBoxInlineAnno(target, s"${target.name}.cpp", makeCPP(target.name, ports)),
    )
  }
  override def execute(state: CircuitState): CircuitState = {
    val moduleMap = state.circuit.modules.map({
      case m => m.name -> m.ports
    }).toMap

    val newAnnos = state.annotations.collect({
      case m: DPIModuleAnnotation => makeAnnotations(m.target, moduleMap(m.target.name))
    }).flatten
    state.copy(annotations = state.annotations ++ newAnnos)
  }
}

class DPIModule[T <: Record](name: String, _io: T) extends BlackBox with HasBlackBoxInline {
  val io = IO(_io.cloneType)

  annotate(DPIModuleChiselAnnotation(this))
}

class MasterVIP(params: AXI4BundleParameters) extends BlackBox with HasBlackBoxInline with HasBlackBoxResource {
  override def desiredName: String =
    s"AXI4MasterVIP_addr_${params.addrBits}_data_${params.dataBits}_id_${params.idBits}_user_${params.userBits}"

  val reset = IO(Input(AsyncReset()))
  val io = IO(new AXI4Bundle(params))

  setInline(desiredName,
    s"""
       |import "DPI-C" function void axi4mastervip_reset();
       |
       |import "DPI-C" function void axi4mastervip_write_tick
       |(
       |  output bit m_axi_aw_valid,
       |  input bit m_axi_aw_ready,
       |  output int m_axi_aw_id,
       |  output int m_axi_aw_addr,
       |  output int m_axi_aw_len,
       |  output int m_axi_aw_size,
       |  output int m_axi_aw_burst,
       |  output int m_axi_aw_lock,
       |  output int m_axi_aw_cache,
       |  output int m_axi_aw_prot,
       |  output int m_axi_aw_qos,
       |  output bit m_axi_ar_valid,
       |  input bit m_axi_ar_ready,
       |  output int m_axi_ar_id,
       |  output int m_axi_ar_addr,
       |  output int m_axi_ar_len,
       |  output int m_axi_ar_size,
       |  output int m_axi_ar_burst,
       |  output int m_axi_ar_lock,
       |  output int m_axi_ar_cache,
       |  output int m_axi_ar_prot,
       |  output int m_axi_ar_qos,
       |  input bit m_axi_w_ready,
       |  output bit m_axi_w_valid,
       |  output int m_axi_w_data,
       |  output int m_axi_w_strb,
       |  output bit m_axi_w_last,
       |  ${if (params.wcorrupt) "output bit m_axi_w_corrupt," else ""}
       |  output bit m_axi_b_ready,
       |  input bit m_axi_b_valid,
       |  input int m_axi_b_id,
       |  input int m_axi_b_resp,
       |);
       |
       |import "DPI-C" function axi4mastervip_read_tick
       |(
       |  output bit m_axi_ar_valid,
       |  input bit m_axi_ar_ready,
       |  output int m_axi_ar_id,
       |  output int m_axi_ar_addr,
       |  output int m_axi_ar_len,
       |  output int m_axi_ar_size,
       |  output int m_axi_ar_burst,
       |  output int m_axi_ar_lock,
       |  output int m_axi_ar_cache,
       |  output int m_axi_ar_prot,
       |  output int m_axi_ar_qos,
       |  output int m_axi_ar_user,
       |  output bit m_axi_r_ready,
       |  input bit m_axi_r_valid,
       |  input int m_axi_r_id,
       |  input int m_axi_r_data,
       |  input int m_axi_r_resp,
       |  ${if (io.r.bits.user.isDefined) "input int m_axi_r_user," else ""}
       |  input int m_axi_r_last
       |);
       |
       |module ${desiredName}(
       |  input clock,
       |  input reset,
       |
       |  // AW Channel
       |  input aw_ready,
       |  output aw_valid,
       |  output [${params.idBits - 1}:0] aw_bits_id,
       |  output [${params.addrBits - 1}:0] aw_bits_addr,
       |  output [${params.lenBits - 1}:0] aw_bits_len,
       |  output [${params.sizeBits - 1}:0] aw_bits_size,
       |  output [${params.burstBits - 1}:0] aw_bits_burst,
       |  output [${params.lockBits - 1}:0] aw_bits_lock,
       |  output [${params.cacheBits - 1}:0] aw_bits_cache,
       |  output [${params.protBits - 1}:0] aw_bits_prot,
       |  ${if (params.qosBits > 0) s"output [${params.qosBits - 1}:0] aw_bits_qos," else ""}
       |
       |  // AR Channel
       |  input ar_ready,
       |  output ar_valid,
       |  output [${params.idBits - 1}:0] ar_bits_id,
       |  output [${params.addrBits - 1}:0] ar_bits_addr,
       |  output [${params.lenBits - 1}:0] ar_bits_len,
       |  output [${params.sizeBits - 1}:0] ar_bits_size,
       |  output [${params.burstBits - 1}:0] ar_bits_burst,
       |  output [${params.lockBits - 1}:0] ar_bits_lock,
       |  output [${params.cacheBits - 1}:0] ar_bits_cache,
       |  output [${params.protBits - 1}:0] ar_bits_prot,
       |  ${if (params.qosBits > 0) s"output [${params.qosBits - 1}:0] ar_bits_qos," else ""}
       |
       |  // W Channel
       |  input w_ready,
       |  output w_valid,
       |  output [${params.dataBits - 1}:0] w_bits_data,
       |  output [${params.dataBits / 8 - 1}:0] w_bits_strb,
       |  output w_bits_last,
       |  ${if (params.wcorrupt) "output w_bits_corrupt," else ""}
       |
       |  // R Channel
       |  output r_ready,
       |  input r_valid,
       |  input [${params.idBits - 1}:0] r_bits_id
       |  input [${params.dataBits - 1}:0] r_bits_data,
       |  input [${params.respBits - 1}:0] r_bits_resp,
       |  input r_bits_last,
       |
       |  // B Channel
       |  output b_ready,
       |  input b_valid,
       |  input [${params.idBits - 1}:0] b_bits_id,
       |  input [${params.respBits - 1}:0] b_bits_resp,
       |);
       |
       |  // inputs
       |  wire #0.1 __aw_ready = aw_ready;
       |  wire #0.1 __ar_ready = ar_ready;
       |  wire #0.1 __w_ready = w_ready;
       |  wire #0.1 __r_valid = r_valid;
       |  wire [${params.idBits - 1}:0] #0.1 __r_bits_id = r_bits_id;
       |  wire [${params.dataBits - 1}:0] #0.1 __r_bits_data = r_bits_data;
       |  wire [${params.respBits -1 }:0] #0.1 __r_bits_resp = r_bits_data;
       |  wire #0.1 __r_bits_last = r_bits_last;
       |  wire #0.1 __b_valid = b_valid;
       |  wire [${params.idBits - 1}:0] #0.1 __b_bits_id = b_bits_id;
       |  wire [${params.respBits - 1}:0] #0.1 __b_bits_resp = b_bits_resp;
       |
       |  // outputs
       |  bit __aw_valid;      assign #0.1 aw_valid = __aw_valid;
       |  int __aw_bits_id;    assign #0.1 aw_bits_id = __aw_bits_id;
       |  int __aw_bits_addr;  assign #0.1 aw_bits_addr = __aw_bits_addr;
       |  int __aw_bits_len;   assign #0.1 aw_bits_len = __aw_bits_len;
       |  int __aw_bits_size;  assign #0.1 aw_bits_size = __aw_bits_size;
       |  int __aw_bits_burst; assign #0.1 aw_bits_burst = __aw_bits_burst;
       |  int __aw_bits_lock;  assign #0.1 aw_bits_lock = __aw_bits_lock;
       |  int __aw_bits_cache; assign #0.1 aw_bits_cache = __aw_bits_cache;
       |  int __aw_bits_prot;  assign #0.1 aw_bits_prot = __aw_bits_prot;
       |  ${if (params.qosBits > 0) "int __aw_bits_qos; assign #0.1 aw_bits_qos = __aw_bits_qos;" else ""}
       |  bit __ar_valid;      assign #0.1 ar_valid = __ar_valid;
       |  int __ar_bits_id;    assign #0.1 ar_bits_id = __ar_bits_id;
       |  int __ar_bits_addr;  assign #0.1 ar_bits_addr = __ar_bits_addr;
       |  int __ar_bits_len;   assign #0.1 ar_bits_len = __ar_bits_len;
       |  int __ar_bits_size;  assign #0.1 ar_bits_size = __ar_bits_size;
       |  int __ar_bits_burst; assign #0.1 ar_bits_burst = __ar_bits_burst;
       |  int __ar_bits_lock;  assign #0.1 ar_bits_lock = __ar_bits_lock;
       |  int __ar_bits_cache; assign #0.1 ar_bits_cache = __ar_bits_cache;
       |  int __ar_bits_prot;  assign #0.1 ar_bits_prot = __ar_bits_prot;
       |  ${if (params.qosBits > 0) "int __ar_bits_qos; assign #0.1 ar_bits_qos = __ar_bits_qos;" else ""}
       |  bit __w_valid; assign #0.1 w_valid = __w_valid;
       |  int __w_bits_data; assign #0.1 w_bits_data = __w_bits_data;
       |  int __w_bits_strb; assign #0.1 w_bits_strb = __w_bits_strb;
       |  bit __w_bits_last; assign #0.1 w_bits_last = __w_bits_last;
       |  ${if (params.wcorrupt) "bit __w_bits_corrupt; assign #0.1 w_bits_corrupt = __w_bits_corrupt;" else ""}
       |  bit __r_ready; assign #0.1 r_ready = __r_ready;
       |  bit __b_ready; assign #0.1 b_ready = __b_ready;
       |
       |  always @(posedge reset)
       |  begin
       |    axi4mastervip_reset();
       |  end
       |  always @(posedge clock)
       |  begin
       |    if (!reset)
       |    begin
       |      axi4mastervip_write_tick(
       |        __aw_valid,
       |        __aw_ready,
       |        __aw_bits_id,
       |        __aw_bits_addr,
       |        __aw_bits_len,
       |        __aw_bits_size,
       |        __aw_bits_burst,
       |        __aw_bits_lock,
       |        __aw_bits_cache,
       |        __aw_bits_prot,
       |        __aw_bits_qos,
       |        __aw_bits_user,
       |      );
       |      axi4mastervip_read_tick();
       |    end
       |  end
       |endmodule
       |""".stripMargin)
}
