-- Trace-Back wrapper for post-mortem debugging the demo with GNAT

with TB_Wrap, GWenerator;

procedure GWenerator_TB is new TB_Wrap(GWenerator);