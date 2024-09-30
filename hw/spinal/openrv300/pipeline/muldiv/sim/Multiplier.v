module Multiplier (
    input clk,
    input resetn,
    input [31:0] A,
    input [31:0] B,
    output [63:0] P
);

reg [31:0] A_1;
reg [31:0] B_1;
reg [31:0] A_2;
reg [31:0] B_2;
reg [63:0] Po;

assign P = Po;

always @ (posedge clk) begin
    A_1 <= A;
    B_1 <= B;
    A_2 <= A_1;
    B_2 <= B_1;
    Po <= A_2 * B_2;
end

endmodule
