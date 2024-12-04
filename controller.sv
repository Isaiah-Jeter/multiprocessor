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