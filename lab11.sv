module top(input  logic        clk, reset, 
           output logic [31:0] WriteData, DataAdr, 
           output logic        MemWrite);

  logic [31:0] PC, Instr, ReadData;
  logic [1:0] alusrca, alusrcb;
  logic adrsrc, irwrite, pcwrite;
  
  // instantiate processor and memories
  RISCVmulti RISCVmulti(clk, reset, PC, Instr, MemWrite, DataAdr, 
                          WriteData, ReadData, alusrca, alusrcb, adrsrc, irwrite, pcwrite);
  imem imem(PC, Instr);
  dmem dmem(clk, MemWrite, DataAdr, WriteData, ReadData);
endmodule

module mem(input  logic clk, WE,
			  input logic [31:0] A, WD, 
			  output logic [31:0] RD);

  logic [31:0] RAM[63:0];

  assign RD = RAM[A[31:2]]; // word aligned
  
  initial
      $readmemh("memfile.dat",RAM);
		
  always_ff @(posedge clk)
    if (WE) RAM[A[31:2]] <= WD;
endmodule

module RISCVmulti(input  logic        clk, reset,
                   output logic [31:0] PC,
                   input  logic [31:0] Instr,
                   output logic        MemWrite,
                   output logic [31:0] ALUResult, WriteData,
                   input  logic [31:0] ReadData
						 output logic [1:0] alusrca, alusrcb,
						 output logic adrsrc, irwrite, pcwrite);

  logic       ALUSrc, RegWrite, Jump, Zero;
  logic [1:0] ResultSrc, ImmSrc;
  logic [2:0] ALUControl;

  controller c(clk, reset, Instr[6:0], Instr[14:12], Instr[30], Zero, ImmSrc, alusrca, alusrcb, ResultSrc, adrsrc, ALUControl, irwrite, pcwrite, RegWrite, MemWrite);

  datapath dp(clk, reset, ResultSrc, RegWrite, ImmSrc, alusrca, alusrcb, ALUControl, Zero, Instr, WriteData, adr
              ALUSrc, RegWrite,
              ImmSrc, ALUControl,
              Zero, PC, Instr,
              ALUResult, WriteData, ReadData);
				  
  mem(clk, MemWrite, ALUResult, WriteData, ReadData);
endmodule

module datapath(input  logic        clk, reset,
                input  logic [1:0]  ResultSrc, 
                input  logic        RegWrite,
                input  logic [1:0]  ImmSrc, ALUSrcB, ALUSrcA,
                input  logic [2:0]  ALUControl,
                output logic        Zero,
					 output  logic [31:0] Instr,
                output logic [31:0] WriteData,
					 output logic  [31:0] Adr,
                input  logic [31:0] ReadData,
					 input logic PCWrite, IRWrite);

  logic [31:0] PCNext, A, tempA, tempWriteData;
  logic [31:0] ImmExt, OldPC, Data, ALUResult, ALUOut, PC;
  logic [31:0] SrcA, SrcB;
  logic [31:0] Result;

  // next PC logic
  
  flope #(32) pcreg(clk, reset, PCNext, PCWrite, PC);
  flope #(32) instrreg(clk, reset, ReadData, IRWrite, Instr);
  flope #(32) oldpcreg(clk, reset, PC, IRWrite, OldPC);
  mux2 #(32)  pcmux(PC, Result, AdrSrc, Adr);
  
  // register file logic
  RegFile     RF(clk, RegWrite, Instr[19:15], Instr[24:20], 
                 Instr[11:7], Result, tempA, tempWriteData);
  Extend      Ext(Instr[31:7], ImmSrc, ImmExt);
  flopr2 #(32) pcreg(clk, reset, tempA, tempWriteData, A, WriteData);
  
  // ALU logic
  mux3 #(32)  SrcBmux(WriteData, ImmExt, 32'd4, ALUSrcB, SrcB);
  mux3 #(32)  SrcAmux(PC, OldPC, A, ALUSrcA, SrcA);
  ALU         ALU(SrcA, SrcB, ALUControl, ALUResult, Zero);
  flopr #(32) datareg(clk, reset, ReadData, Data);
  flopr #(32) alureg(clk, reset, ALUResult, ALUOut);
  mux3 #(32) alumux(ALUOut, Data, ALUResult, ResultSrc, Result);
   
endmodule

module flopr #(parameter WIDTH = 8)
              (input  logic             clk, reset,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if (reset) q <= 0;
    else       q <= d;
endmodule

module flope #(parameter WIDTH = 8)
              (input  logic             clk, reset,
               input  logic [WIDTH-1:0] next,
					input  logic write, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if (reset) q <= 0;
	 else if (write) q <= next;

endmodule


module flopr2 #(parameter WIDTH = 8)
              (input  logic             clk, reset,
               input  logic [WIDTH-1:0] RD1, RD2,
               output logic [WIDTH-1:0] A, wrD);

  always_ff @(posedge clk, posedge reset)
    if (reset) 
	 begin
		A <= 0;
		WrD <=0;
	 end
    else 	
	 begin
		A <= RD1;
		WrD <=RD2;
	 end
endmodule

module mux2 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] pc, result, 
              input  logic             adrsrc, 
              output logic [WIDTH-1:0] y);

  assign y = adrsrc ? result : pc; 
endmodule

module mux3 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] wd, IE, four, 
              input  logic [1:0]            ALUSrcB, 
              output logic [WIDTH-1:0] y);
	always_comb
    case (ALUSrcB)
      2'b00: result = wd;          // addition
      2'b01: result = IE;          // subtraction
      2'b10: result = four;        // and
      default: result = 0;
  endcase
endmodule


module RegFile(input  logic        clk, 
               input  logic        WE3, 
               input  logic [ 4:0] A1, A2, A3, 
               input  logic [31:0] WD3, 
               output logic [31:0] RD1, RD2);

  logic [31:0] rf[31:0];

  // three ported register file
  // read two ports combinationally (A1/RD1, A2/RD2)
  // write third port on rising edge of clock (A3/WD3/WE3)
  // register 0 hardwired to 0

  always_ff @(posedge clk)
    if (WE3) rf[A3] <= WD3;	

  assign RD1 = (A1 != 0) ? rf[A1] : 0;
  assign RD2 = (A2 != 0) ? rf[A2] : 0;
endmodule

module Extend(input  logic [31:7] Instr,
              input  logic [1:0]  ImmSrc,
              output logic [31:0] ImmExt);
 
  always_comb
    case(ImmSrc) 
               // I-type 
      2'b00:   ImmExt = {{20{Instr[31]}}, Instr[31:20]};  
               // S-type (Stores)
      2'b01:   ImmExt = {{20{Instr[31]}}, Instr[31:25], Instr[11:7]}; 
               // B-type (Branches)
      2'b10:   ImmExt = {{20{Instr[31]}}, Instr[7], Instr[30:25], Instr[11:8], 1'b0}; 
               // J-type (Jumps)
      2'b11:   ImmExt = {{12{Instr[31]}}, Instr[19:12], Instr[20], Instr[30:21], 1'b0}; 
      default: ImmExt = 32'bx; // undefined
    endcase             
endmodule

module alu(input  logic [31:0] a, b,
            input  logic [2:0]  alucontrol,
            output logic [31:0] result,
            output logic        zero); 

  logic [31:0] condinvb, sum;
  logic        sub;
  
  assign sub = (alucontrol[1:0] == 2'b01);
  assign condinvb = sub ? ~b : b; // for subtraction or slt
  assign sum = a + condinvb + sub;

  always_comb
    case (alucontrol)
      3'b000: result = sum;          // addition
      3'b001: result = sum;          // subtraction
      3'b010: result = a & b;        // and
      3'b011: result = a | b;        // or
      3'b101: result = sum[31];      // slt
      default: result = 0;
  endcase

  assign zero = (result == 32'b0);
endmodule

module controller(input logic clk,
 input logic reset,
 input logic [6:0] op,
 input logic [2:0] funct3,
 input logic funct7b5,
 input logic zero,
 output logic [1:0] immsrc,
 output logic [1:0] alusrca, alusrcb,
 output logic [1:0] resultsrc,
 output logic adrsrc,
 output logic [2:0] alucontrol,
 output logic irwrite, pcwrite,
 output logic regwrite, memwrite);
 
	logic pcupdate, branch;
	logic [1:0] aluop;

 
	main main1(clk, reset, op, pcupdate, branch, regwrite, memwrite, irwrite, adrsrc, resultsrc, alusrcb, alusrca, aluop);
	
	ALUdecoder ALUdecoder1(aluop[1],aluop[0],funct3[2],funct3[1],funct3[0],op[5],funct7b5,alucontrol[0],alucontrol[1],alucontrol[2]);
	
	instrDecoder instrDecoder1(op, immsrc);
	
	assign pcwrite = pcupdate|(zero&branch); 
	
	
 
endmodule

module instrDecoder(input logic [6:0] op,
							output logic [1:0] ImmSrc);
							
	always_comb		
		if(op==7'b0110011) ImmSrc=2'b00;
		else if(op==7'b0010011) ImmSrc=2'b00;
		else if(op==7'b0000011) ImmSrc=2'b00;
		else if(op==7'b0100011) ImmSrc=2'b01;
		else if(op==7'b1100011) ImmSrc=2'b10;
		else if(op==7'b1101111) ImmSrc=2'b11;
		else ImmSrc=2'b00;
		
endmodule
			

module ALUdecoder(input logic a,b,c,d,e,f,g,
						output logic y1, y2, y3);
						
	
	logic n1, n2, n3;
	
	and x1(n1,~a,b);
	and x2(n2,a,~b,~c,~d,~e,f,g);
	and x3(n3,a,~b,d,~e);
	or x4(y1,n1,n2,n3);
	
	and x5(y2,a,~b,c,d);
	and x6(y3, a,~b,~c,d,~e);
endmodule




module main(input logic clk, reset,
				input logic [6:0] op,
				output logic PCUpdate, Branch, RegWrite, MemWrite, IRWrite, AdrSrc,
				output logic [1:0] ResultSrc, ALUSrcB, ALUSrcA, ALUOp);
				
typedef enum logic [10:0] {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10} statetype;
statetype state, nextstate;

always_ff @(posedge clk, posedge reset)
	if (reset) state<=s0;
	else state<=nextstate;
	
	always_comb
		case (state)
			s0:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b0;
				IRWrite=1'b1;
				ALUSrcA=2'b00;
				ALUSrcB=2'b10;
				ALUOp=2'b00;
				ResultSrc=2'b10;
				PCUpdate=1'b1;
				nextstate=s1;
				end
			s1:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b01;
				ALUSrcB=2'b01;
				ALUOp=2'b00;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				if(op==7'b0000011)
					nextstate=s2;
				else if(op==7'b0100011)
					nextstate=s2;
				else if(op==7'b0110011)
					nextstate=s6;
				else if(op==7'b0010011)
					nextstate=s8;
				else if(op==7'b1101111)
					nextstate=s9;
				else if(op==7'b1100011)
					nextstate=s10;
				else 
					nextstate=s0;
				end
			s2:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b10;
				ALUSrcB=2'b01;
				ALUOp=2'b00;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				nextstate=s1;
				if(op==7'b0000011)
					nextstate=s3;
				else if(op==7'b0100011)
					nextstate=s5;
				else nextstate=s0;
				end
			s3:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b1;
				IRWrite=1'b0;
				ALUSrcA=2'b00;
				ALUSrcB=2'b00;
				ALUOp=2'b00;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				nextstate=s4;
				end
			s4:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b1;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b00;
				ALUSrcB=2'b00;
				ALUOp=2'b00;
				ResultSrc=2'b01;
				PCUpdate=1'b0;
				nextstate=s0;
				end
			s5:begin
				Branch=1'b0;
				MemWrite=1'b1;
				RegWrite=1'b0;
				AdrSrc = 1'b1;
				IRWrite=1'b0;
				ALUSrcA=2'b00;
				ALUSrcB=2'b00;
				ALUOp=2'b00;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				nextstate=s0;
				end
			s6:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b10;
				ALUSrcB=2'b00;
				ALUOp=2'b10;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				nextstate=s7;
				end
			s7:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b1;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b00;
				ALUSrcB=2'b00;
				ALUOp=2'b00;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				nextstate=s0;
				end
			s8:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b10;
				ALUSrcB=2'b01;
				ALUOp=2'b10;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				nextstate=s7;
				end
			s9:begin
				Branch=1'b0;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b01;
				ALUSrcB=2'b10;
				ALUOp=2'b00;
				ResultSrc=2'b00;
				PCUpdate=1'b1;
				nextstate=s7;
				end
			s10:begin
				Branch=1'b1;
				MemWrite=1'b0;
				RegWrite=1'b0;
				AdrSrc = 1'b0;
				IRWrite=1'b0;
				ALUSrcA=2'b10;
				ALUSrcB=2'b00;
				ALUOp=2'b01;
				ResultSrc=2'b00;
				PCUpdate=1'b0;
				nextstate=s0;
				end
		default:begin
			Branch=1'b0;
			MemWrite=1'b0;
			RegWrite=1'b0;
			AdrSrc = 1'b0;
			IRWrite=1'b0;
			ALUSrcA=2'b00;
			ALUSrcB=2'b00;
			ALUOp=2'b00;
			ResultSrc=2'b00;
			PCUpdate=1'b0;
			nextstate=s0;
			end
		
	endcase
endmodule