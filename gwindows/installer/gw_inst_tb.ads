-- Trace-Back wrapper for post-mortem debugging the demo with GNAT

with TB_Wrap, GW_Install;

procedure GW_Inst_TB is new TB_Wrap(GW_Install);