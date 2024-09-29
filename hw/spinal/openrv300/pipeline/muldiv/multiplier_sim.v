module multiplier_sim (
    input [31:0] A,
    input [31:0] B,
    output [63:0] C
);

assign C = A * B;

endmodule
