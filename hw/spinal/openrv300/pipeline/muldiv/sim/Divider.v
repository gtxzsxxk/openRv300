module Divider (
    input aclk,
    input [31:0] s_axis_divisor_tdata,
    input s_axis_divisor_tvalid,
    input [31:0] s_axis_dividend_tdata,
    input s_axis_dividend_tvalid,

    output reg [63:0] m_axis_dout_tdata,
    output reg m_axis_dout_tvalid
);

    reg [31:0] divisor;
    reg [31:0] dividend;
    reg [3:0] cycle_count; // 用于计数周期
    reg busy; // 状态指示

    always @(posedge aclk) begin
        // 当除数和被除数同时有效时，保存它们并开始除法操作
        if (s_axis_divisor_tvalid && s_axis_dividend_tvalid && !busy) begin
            divisor <= s_axis_divisor_tdata;
            dividend <= s_axis_dividend_tdata;
            busy <= 1; // 设置为忙状态
            cycle_count <= 0; // 重置计数器
            m_axis_dout_tvalid <= 0; // 清除输出有效标志
        end

        // 如果正在进行除法操作
        if (busy) begin
            cycle_count <= cycle_count + 1;

            // 在12个周期后计算商和余数
            if (cycle_count < 12) begin
                // 继续保持忙状态
                busy <= 1;
            end else begin
                // 计算商和余数
                m_axis_dout_tdata <= {dividend / divisor, dividend % divisor};
                m_axis_dout_tvalid <= 1; // 设置输出有效标志
                busy <= 0; // 操作完成，重置忙状态
            end
        end
    end
endmodule
