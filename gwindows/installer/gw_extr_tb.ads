-- Trace-Back wrapper for post-mortem debugging the demo with GNAT

with TB_Wrap, GW_Extract;

procedure GW_Extr_TB is new TB_Wrap(GW_Extract);