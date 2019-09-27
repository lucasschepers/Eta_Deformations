(* ::Package:: *)

ClearAll["Global`*"]
Needs["BenderWu`"]

(*Potential*)
m[z_,e_]:=4 z e/(1+(z+e)^2);
V[x_, z_, e_]:=JacobiSN[x,m[z,e]]^2 (1+(z-e)^2 JacobiSN[x,m[z,e]]^2)/JacobiDN[x,m[z,e]]^2

(*Some machinery*)
MyBW[zin_,ein_,nu_,order_]:=BenderWu[Evaluate[V[x, zin, ein]]/.z->zin/.e->ein,x,nu,order,Monitor->False]
MyBWSeries[BW_]:=Collect[Sqrt[2] BWProcess[BW,OutputStyle->"Series",Coupling->2^(1/4) g],g]

(*For numeric computations*)
VSeries[x_,z_,e_,N_]:=Series[V[x, z, e],{x,0,N}]//Normal
MyBWNumeric[z_,e_,nu_,order_]:=BenderWu[VSeries[x,z,e,2 order+2],x,nu,order]

test=4
