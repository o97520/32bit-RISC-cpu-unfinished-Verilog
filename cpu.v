
module instruction_memory (input [31:0]program_counter, output reg [31:0]o);


always @(*)
  if(program_counter == 1)
    begin
      o = 32'b00000001000010010101000000100000;
    end
  else if(program_counter == 2)
    begin
      o = 32'b00000001000010010101000000100001;
    end
  else if(program_counter == 3)
    begin
      o = 32'b00100001000010010001100100100110;
    end
  else if(program_counter == 4)
    begin
      o = 32'b00100101000010010001100100100110;
    end

endmodule

module dataMem();

endmodule

module fetch(input [31:0]program_counter, output wire [31:0]instruction);

  instruction_memory im(program_counter, instruction);

endmodule



module decode(output reg [5:0]funct, output reg [15:0]addres16bit, output reg [15:11]Rs, output reg [20:16]Rt, output reg [25:21]Rd, output reg [31:26]OPCode, input [31:0] output_instruction);

always @(*)
  begin
    funct =         output_instruction[5:0];
    addres16bit =   output_instruction[15:0];
    Rs =            output_instruction[15:11];
    Rt =            output_instruction[20:16];
    Rd =            output_instruction[25:21];
    OPCode =        output_instruction[31:26];
  end
endmodule

module sign_extend(input [15:0]inputaddres16bit, output reg [31:0]extended_address);

  always @(*)
    begin
      
      extended_address = {inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],
      inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit[15],
      inputaddres16bit[15],inputaddres16bit[15],inputaddres16bit};
      
    end

endmodule

module addone(input [31:0]extended_address, output reg [31:0] extended_address_increment);

  always @(*)
    begin
      
      extended_address_increment = extended_address + 1'b1;

    end

endmodule

module program_counter_addone(input [31:0]program_counter, output reg [31:0] program_counter_increment);

  always @(*)
    begin
      
      program_counter_increment = program_counter + 1'b1;

    end

endmodule

module program_counter(output reg [31:0] program_counter, input Clock);

  always @(*)
    begin
      
      program_counter = program_counter;

    end

endmodule

module control(input[5:0]opcode, output reg regDst, output reg Branch, output reg memRead, output reg memToReg, output reg [4:0]ALUOp, output reg memWrite, output reg aluSrc, output reg regWrite);
  
  always @(*)
    begin
        case (opcode)
          6'b100101: ALUOp = 4'b0001; aluSrc = 1'b1;     // Or                OR
          6'b100100: ALUOp = 4'b0000; aluSrc = 1'b1;     // And                 AND
          6'b100000: ALUOp = 4'b0010; regDst = 1'b1; regWrite = 1'b1;      // Add                
          6'b100010: ALUOp = 4'b0110; regDst = 1'b1; regWrite = 1'b1;      // Sub                
          6'b101010: ALUOp = 4'b0111; aluSrc = 1'b1;     // SLT                SLT
          6'b100111: ALUOp = 4'b1100; aluSrc = 1'b1;     // Nor                 NOR
          default: ALUOp = 4'b0000;        // Nothing
        endcase
      // opcode,Regdst,branch,memread,memtoreg,aluop,memwrite,alusrc, regwrite
    end
    
endmodule

module ALUControl(input [5:0]funct, input [4:0]ALUOp, output );

///////////////////////
///////////////////////
///////////////////////

endmodule


module registerFile(output reg read1, output reg read2. input aluSrc);

//////////////////////
//////////////////////
//////////////////////

endmodule


module registerWriteMultiplexer(output reg writeRegister, input regWrite, input register2, input register3);
  always @(*)
    if(regWrite == 1'b1)
      begin
        writeRegister = register2;
      end
    else (regWrite == 1'b0)
      begin
        writeRegister = register3;
      end
endmodule


module ALU(input ALUOp, input register, input input2, output reg ALU_result);
  always @(*)
    begin
        case (funct)
          4'b0001: ALU_result = register or input2;        // Or 
          4'b0000: ALU_result = register & input2;       // And
          4'b0010: ALU_result = register + input2;       // Add
          4'b0110: ALU_result = register - input2;       // sub
          4'b0111: ALU_result = register slt input2;       // Slt
          4'b1100: ALU_result = ~(register or input2);       // Nor
          default: a = 1'bx;
        endcase
    end
endmodule


module main();

  integer Clock;
  wire [31:0]instruction;
  wire [31:0]program_counter;
  wire [31:0]program_counter_increment;
  wire [31:0]extended_address;
  wire [5:0]funct;
  wire [15:0]addres16bit;
  wire [15:11]Rs;
  wire [20:16]Rt;
  wire [25:21]Rd;
  wire [31:26]OPCode;
  wire [4:0]ALUOp;
  wire memWrite;
  wire aluSrc;
  wire regWrite;
  wire regDst;
  wire Branch;
  wire memRead;
  wire memToReg;

  program_counter pc(program_counter, Clock);
  program_counter_addone pca(program_counter, program_counter_increment);
  fetch ftch(program_counter,instruction);
  decode dcd(funct, addres16bit, Rs, Rt, Rd, OPCode, instruction);
  sign_extend se(addres16bit, extended_address);
  control cr(OPCode, regDst, Branch, memRead, memToReg, ALUOp, memWrite, aluSrc, regWrite);
  registerWriteMultiplexer rgm(writeRegister, regWrite, input register2, input register3);

  initial begin
    Clock = 1;
    program_counter = 1;
  end
  initial begin
    #1 Clock = Clock +1;
    #1 Clock = Clock +1;
    #1 Clock = Clock +1;
  end

  initial begin
    $dumpfile("main.vcd");
    $dumpvars(0, main);
    $monitor("time=%g,Clock = %d      , instruction   =    %b, funct   =    %b, addres16bit   =    %b, Rs   =    %b, Rt   =    %b, Rd   =    %b, OPCode   =    %b" , $time, Clock, instruction, funct, addres16bit, Rs, Rt, Rd, OPCode);
  end

  initial #50 $finish; 

endmodule





/*
format indication %b %B binary
                  %c %C character (low 8 bits)
                  %d %D decimal  %0d for minimum width field
                  %e %E E format floating point %15.7E
                  %f %F F format floating point %9.7F
                  %g %G G general format floating point
                  %h %H hexadecimal
                  %l %L library binding information
                  %m %M hierarchical name, no expression
                  %o %O octal
                  %s %S string, 8 bits per character, 2´h00 does not print
                  %t %T simulation time, expression is  $time
                  %u %U unformatted two value data  0 and 1 
                  %v %V net signal strength
                %z %Z unformatted four value data  0, 1, x, z
*/
