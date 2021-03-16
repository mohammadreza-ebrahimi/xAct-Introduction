xAct`Invar`$Version={"2.0.5",{2013,7,1}}


xAct`Invar`$xTensorVersionExpected={"1.1.0",{2013,9,1}}


(* Invar, a free package for simplification of Riemann invariants *)

(* Copyright (C) 2006-2018 Jose M. Martin-Garcia, D. Yllanes and Renato Portugal *)

(* This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published
 by the Free Software Foundation; either version 2 of the License,
  or (at your option) any later version.

This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307,
  USA. 
*)


(* :Title: Invar *)

(* :Authors: Jose M. Martin-Garcia, David Yllanes and Renato Portugal *)

(* :Summary: Free package for simplification of Riemann invariants *)

(* :Brief Discussion:
   - Based on xTensor.
   - There is a companion version for Maple (using Canon) by Renato
     Portugal.
   - Simplifies Riemann invariants (scalars formed from the Riemann
     tensor of a metric-compatible connection and its Levi-Civita
     connection) by using permutation group theory.
   - Multiterm symmetries (cyclic, Bianchi, commutation and
     dimension-dependent identities) have been solved in advance
     and stored in solution-files.
   - Works with Riemann, Ricci, RicciScalar, Weyl and TFRicci.
*)
  
(* :Context: xAct`Invar` *)

(* :Package Version: 2.0.5 *)

(* :Copyright: Jose M. Martin-Garcia and David Yllanes (2006-2018) *)

(* :History: 
	- 22 Nov 2006 - Invar1
	-  8 Feb 2008 - Invar2
*)

(* :Keywords: *)

(* :Source: Invar.nb *)

(* :Warning: *)

(* :Mathematica Version: 6.0 and later *)

(* :Limitations:
   - Handles algebraic invariants with up to seven Riemann tensors
     or dual invariants with up to five Riemann tensors
   - Handles differential invariants with up to twelve metric derivatives
     or dual invariants with up to eight metric derivatives *)


With[{xAct`Invar`Private`InvarSymbols=DeleteCases[Join[Names["xAct`Invar`*"],Names["xAct`Invar`Private`*"]],"$Version"|"xAct`Invar`$Version"|"$xTensorVersionExpected"|"xAct`Invar`$xTensorVersionExpected"]},
Unprotect/@xAct`Invar`Private`InvarSymbols;
Clear/@xAct`Invar`Private`InvarSymbols;
]


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`Invar`"];


BeginPackage["xAct`Invar`",{"xAct`xTensor`","xAct`xPerm`","xAct`xCore`","xAct`ExpressionManipulation`"}]


General::xTensorVersion="Loaded xTensor version `1` but expected version `2` at least.";
If[Not@OrderedQ@Map[Last,{$xTensorVersionExpected,xAct`xTensor`$Version}],Message[General::xTensorVersion,xAct`xTensor`$Version,$xTensorVersionExpected];
Abort[]]


Print[xAct`xCore`Private`bars];
Print["Package xAct`Invar`  version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2006-2018, J. M. Martin-Garcia, D. Yllanes and R. Portugal, under the General Public License."]


Off[General::shdw]
xAct`Invar`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


If[xAct`xCore`Private`$LastPackage==="xAct`Invar`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]]


$InvarDirectory::usage="$InvarDirectory is a global variable giving the directory in which the package file and the database can be found.";


InvarCases::usage="InvarCases[s, d] returns the list of cases of order (number of metric derivatives) s and degree (number of Riemanns) d. InvarCases[s] returns the list of cases of order s. InvarCases[] returns the list of all cases handled by Invar (currently the 47 cases up to order 12 plus the algebraic case of degree 7).";
InvarDualCases::usage="InvarDualCases[s, d] returns the list of dual cases of order (number of metric derivatives) s and degree (number of Riemanns) d in dimension 4. InvarDualCases[s] returns the list of dual cases of order s. InvarDualCases[] returns the list of all dual cases handled by Invar (currently the 14 cases up to order 8 plus the algebraic case of degree 5).";


RInv::usage="RInv[g][c, r] denotes the Riemann invariant of case c and index r associated to the metric g.";
DualRInv::usage="DualRInv[g][c, r] denotes the dual Riemann invariant of case c and index r associated to the metric g.";
RPerm::usage="RPerm[g][{c, ed}, perm] denotes a Riemann invariant of case c with ed epsilon tensors (both associated to metric g), after reordering the indices from a canonical order using permutation perm.";


WInv::usage="WInv[g][c, r] denotes the Weyl invariant of case c and index r associated to the metric g.";
DualWInv::usage="DualWInv[g][c, r] denotes the dual Weyl invariant of case c and index r associated to the metric g.";
WPerm::usage="WPermRPerm[g][{Rd, ed}, perm] denotes a Weyl invariant  formed by Rd Weyl tensors and ed epsilon tensors, after reordering the indices from a canonical order using permutation perm. Contracted Weyls encode the Traceless Ricci tensor.";


RToW::usage="RToW[expr] transforms Riemann invariants into Weyl invariants in expression expr.";
WToR::usage="WToR[expr] transforms Weyl invariants into Riemann invariants in expression expr.";
RToWRules::usage="RToWRules[step, degree]";
RToWDualRules::usage="RToWDualRules[step, degree]";
WToRRules::usage="RToWRules[step, degree]";
WToRDualRules::usage="RToWDualRules[step, degree]";


RInvs::usage="RInvs[step, case] gives all independent Riemann invariants at the given step (1, 2, 3, 4, 5 or 6) and the given case. Using RInvs[g] instead of RInvs we get the particularization of the invariants to metric g.";
WInvs::usage="WInvs[step, case] gives all independent Weyl invariants at the given step (1, 2, 3, 4, 5, 6) and the given case. Using WInvs[g] instead of WInvs we get the particularization of the invariants to metric g.";
DualRInvs::usage="DualRInvs[step, case] gives all independent dual Riemann invariants at the given step (1, 2, 3, 4, 5 or 6) and the given case. Using DualRInvs[g] instead of RInvs we get the particularization of the dual invariants to metric g.";
DualWInvs::usage="DualWInvs[step, case] gives all independent dual Weyl invariants at the given step (1, 2, 3, 4, 5 or 6) and the given case. Using DualWInvs[g] instead of WInvs we get the particularization of the dual invariants to metric g.";
RInvRules::usage="RInvRules[step, case] gives the list of rules to be used to simplify Riemann non-dual invariants of the given case at the given step.";
WInvRules::usage="WInvRules[step, case] gives the list of rules to be used to simplify Weyl non-dual invariants of the given case at the given step.";
DualRInvRules::usage="DualRInvRules[step, case] gives the list of rules to be used to simplify Riemann dual invariants of the given case at the given step.";
DualWInvRules::usage="DualWInvRules[step, case] gives the list of rules to be used to simplify Weyl dual invariants of the given case at the given step.";


RemoveRInvRules::usage="RemoveRInvRules[step, case] removes the stored rules for the invariants of the given step and case.";
RemoveDualRInvRules::usage="RemoveDualRInvRules[step, case] removes the stored rules for the dual invariants of the given step and case.";


MaxIndex::usage="MaxIndex[step] gives the largest index of the non-dual invariants at the given step.";
MaxDualIndex::usage="MaxDualIndex[step] gives the largest index of the dual invariants at the given step.";


InvSimplify::usage="InvSimplify[expr, level] simplifies the invariants of the form inv[metric][case,count] in the expression expr, where inv is one of RInv, DualRInv, WInv, DualWInv. There are six possible levels of simplification: 1 means no simplification; 2 uses the cyclic identity; 3 uses the Bianchi identity; 4 commutes covariant derivatives; 5 uses dimensionally dependent (Lovelock type) identities and 6 reduces some non-dual invariants into products of dual invariants. The default level used in InvSimplify[expr] is $InvSimplifyLevel.";
InvToPerm::usage="InvToPerm[expr] converts all invariants in expr into their corresponding permutations with head RPerm or WPerm.";
PermToRiemann::usage="PermToRiemann[expr, cr] converts all permutations into their explicit tensor expressions. With cr=True contracted Riemann tensors are replaced by Ricci tensors. The default value used in PermToRiemann[expr] is $CurvatureRelations.";
InvToRiemann::usage="InvToRiemann[expr, cr] converts all invariants into their explicit tensor expressions. With cr=True contracted Riemann tensors are replaced by Ricci tensors. The default value used in InvToRiemann[expr] is $CurvatureRelations.";
RiemannToPerm::usage="RiemannToPerm[expr, g] converts all Riemann scalars of metric g into their canonical permutations. If g is a list of metrics then the command is folded over the list. RiemannToPerm[expr] is automatically converted into RiemannToPerm[expr, $Metrics].";
PermToInv::usage="PermToInv[expr] converts all permutations in expr into their correspondent invariants. It is assumed that the permutations have been already canonicalized.";
RiemannToInv::usage="RiemannToInv[expr, g] converts all Riemann scalars of metric g into their invariant form. If g is a list of metrics then the command is folded over the list. RiemannToInv[expr] is automatically converted into RiemannToInv[expr, $Metrics].";
RiemannSimplify::usage="RiemannSimplify[expr, level, cr, g] simplifies the Riemann scalars of metric g using relations up to the given level (see usage message for InvSimplify). With cr=True contracted Riemann tensors are replaced by Ricci tensors. If g is a list of metrics then the command is folded over the list. RiemannSimplify[expr] uses the defaults $InvSimplifyLevel, $CurvatureRelations and $Metrics, respectively. See usage messages for those global variables.";


dim::usage="dim is a constant symbol used in the Invar rules to denote the dimension of a manifold.";
sigma::usage="sigma is a constant symbol used in the Invar rules to denote the sign of the determinant of a metric.";


$InvSimplifyLevel::usage="$InvSimplifyLevel is a global variable specifying the default level of simplification in InvSimplify. Initially it is set to 6.";
$CurvatureRelations::usage="$CurvatureRelations is a global variable specifying the default behaviour of contracted Riemann conversion in the functions PermToRiemann, InvToRiemann and RiemannSimplify.";


RandomRiemannMonomial::usage="RandomRiemannMonomial[c, g] generates a random monomial of case c with Riemann tensors of the metric g. If not specified, g is taken to be the first metric in the list $Metrics.";


$ExpandedCommuteOrder12Q::usage="$ExpandedRulesDiagonalQ is a Boolean global variable stating whether we want to use the expanded form for the Commute step of some hard cases of order 12. Its default value is False.";


Begin["`Private`"]


Block[{$InvarNames={}},DefConstantSymbol[sigma,PrintAs->"\[Sigma]"]]


sigma/:Power[sigma,_Integer?EvenQ]=1;
sigma/:Power[sigma,_Integer?OddQ]=sigma;
sigma/:Abs[sigma]=1;


Block[{$InvarNames={}},DefConstantSymbol[dim,PrintAs->"d"]]


ReportSetOption[ContractMetric,AllowUpperDerivatives->True];


$InvSimplifyLevel=6;
$CurvatureRelations=False;


ReportSet[$PrePrint,ScreenDollarIndices];


ReportSet[$CovDFormat,"Postfix"];


ReportSetOption[DefCovD,CurvatureRelations->False];


ReportSet[$CommuteCovDsOnScalars,False];


tostring[list_List]:=StringJoin[ToString/@list];


Format[RInv[_][case_,index_]]:=Subscript["I",StringJoin[tostring[case],",",ToString[index]]];
Format[DualRInv[_][case_,index_]]:=Subscript["D",StringJoin[tostring[case],",",ToString[index]]];
Format[WInv[_][case_,index_]]:=Subscript["W",StringJoin[tostring[case],",",ToString[index]]];
Format[DualWInv[_][case_,index_]]:=Subscript["X",StringJoin[tostring[case],",",ToString[index]]];


Format[RInv[case_,index_]]:=Subscript["I",StringJoin[tostring[case],",",ToString[index]]];
Format[DualRInv[case_,index_]]:=Subscript["D",StringJoin[tostring[case],",",ToString[index]]];
Format[WInv[case_,index_]]:=Subscript["W",StringJoin[tostring[case],",",ToString[index]]];
Format[DualWInv[case_,index_]]:=Subscript["X",StringJoin[tostring[case],",",ToString[index]]];


$ReadWeyl=False;


$InvarDirectory=StringJoin[$xActDirectory,"/Invar"]


$DataBaseDir="Riemann/";


(* Invariants *)
RInv[deg_Integer,index_]:=RInv[Table[0,{deg}],index];
DualRInv[deg_Integer,index_]:=DualRInv[Table[0,{deg}],index];
(* Invariants with metric argument *)
RInv[metric_][deg_Integer,index_]:=RInv[metric][Table[0,{deg}],index];
DualRInv[metric_][deg_Integer,index_]:=DualRInv[metric][Table[0,{deg}],index];
(* Independent Riemann invariants *)
RInvs[step_Integer,deg_Integer,dim___]:=RInvs[step,Table[0,{deg}],dim];DualRInvs[step_Integer,deg_Integer,dim___]:=DualRInvs[step,Table[0,{deg}],dim];
(* Lists of rules *)
RInvRules[step_,deg_Integer,dim___]:=RInvRules[step,Table[0,{deg}],dim];
DualRInvRules[step_,deg_Integer,dim___]:=DualRInvRules[step,Table[0,{deg}],dim];


(* From Riemann to Weyl invariants *)
WInvs[step_Integer,deg_,dim___]:=RInvs[step,deg,dim]/.RInv->WInv;DualWInvs[step_Integer,deg_,dim___]:=DualRInvs[step,deg,dim]/.DualRInv->DualWInv;


SetAttributes[ProtSet,HoldFirst];
ProtSet[head_[args___],value_]:=Module[{prot=Unprotect[head]},Set[head[args],value];Protect[Evaluate[prot]];value];


intercase[case_List]:=StringJoin@Insert[ToString/@case,"_",List/@Range[2,Length[case]]];


filename[step:(5|6),case_,dim_]:=filename[ToString[step],case]<>"_"<>ToString[dim];
filename[step_,case_]:=StringJoin["RInv-",intercase[case],"-",ToString[step]];


dualfilename[5,case_,dim_]:=dualfilename["5",case]<>"_"<>ToString[dim];
dualfilename[step_,case_]:=StringJoin["DInv-",intercase[case],"-",ToString[step]];


RInv::nodatabase="Can't find the Invar database. You can download it from http://www.xact.es/Invar/.";
CheckImport[args__]:=Quiet[Check[Import[args],Message[RInv::nodatabase];Abort[]],Import::nffil];


firstof[{}]:=0;
firstof[{{a_,b_}}]:=a;
firstof[{{a_,b_},__}]:=a;


positionof[string_String,substring_String]:=firstof@StringPosition[string,substring];
movetofirst[0]:=1;
movetofirst[x_]:=x+4;


replacebrackets[string_String]:=StringReplace[string,{"["->"{","]"->"}"}]
replaceequal[string_String]:=StringReplace[string,{"="->"->"}]


readline[string_]:=StringTake[string,{movetofirst@positionof[string," := "],StringLength[string]-1}]


ReadInvarPerms[filename_,"Maple"]:=ToExpression/@replacebrackets/@readline/@CheckImport[StringJoin[$InvarDirectory,"/",filename],"Lines"];


ReadInvarRules[filename_,"Maple"]:=ToExpression/@replaceequal/@readline/@CheckImport[StringJoin[$InvarDirectory,"/",filename],"Lines"];


ReadInvarPerms[filename_,"Mathematica"]:=ToExpression/@CheckImport[StringJoin[$InvarDirectory,"/",filename],"Lines"];


ReadInvarRules[filename_,"Mathematica"]:=ToExpression/@CheckImport[StringJoin[$InvarDirectory,"/",filename],"Lines"];


RemoveRInvRules[step_Integer,degree_Integer,dim___]:=RemoveRInvRules[step,Table[0,{degree}]];
RemoveRInvRules[step_Integer,case_List,dim___]:=(
Unset[DispatchRules[RInv,step,case,dim]];
Unprotect[RInvRules];
Unset[RInvRules[step,case,dim]];
Protect[RInvRules];
);
RemoveDualRInvRules[step_Integer,degree_Integer,dim___]:=RemoveDualRInvRules[step,Table[0,{degree}]];
RemoveDualRInvRules[step_Integer,case_List,dim___]:=(
Unset[DispatchRules[DualRInv,step,case,dim]];
Unprotect[DualRInvRules];
Unset[DualRInvRules[step,case,dim]];
Protect[DualRInvRules];
);


addRdep[Rule[inv_[case_,count_],rhs_]]:=Rule[inv[metric_][case,count],rhs/.{RInv->RInv[metric],DualRInv->DualRInv[metric],WInv->WInv[metric],DualWInv->DualWInv[metric]}]


(* With a metric argument *)
RInvs[metric_][args__]:=RInvs[args]/.RInv->RInv[metric];WInvs[metric_][args__]:=WInvs[args]/.WInv->WInv[metric];
DualRInvs[metric_][args__]:=DualRInvs[args]/.DualRInv->DualRInv[metric];
DualWInvs[metric_][args__]:=DualWInvs[args]/.DualWInv->DualWInv[metric];


$DefaultDim:=DimOfVBundle@VBundleOfMetric@First@$Metrics;


InvarCases[]:=Flatten[Table[InvarCases[order],{order,2,14,2}],1];
InvarCases[order_Integer?EvenQ]:=InvarCases[order,Reverse@Range[order/2]];
InvarCases[order_Integer?EvenQ,list_List]:=Flatten[InvarCases[order,#]&/@list,1];

InvarCases[14]:=InvarCases[14,7];

InvarCases[2,1]={{0}};

InvarCases[4,1]={{2}};
InvarCases[4,2]={{0,0}};

InvarCases[6,1]={{4}};
InvarCases[6,2]={{0,2},{1,1}};
InvarCases[6,3]={{0,0,0}};

InvarCases[8,1]={{6}};
InvarCases[8,2]={{0,4},{1,3},{2,2}};
InvarCases[8,3]={{0,0,2},{0,1,1}};
InvarCases[8,4]={{0,0,0,0}};

InvarCases[10,1]={{8}};
InvarCases[10,2]={{0,6},{1,5},{2,4},{3,3}};
InvarCases[10,3]={{0,0,4},{0,1,3},{0,2,2},{1,1,2}};
InvarCases[10,4]={{0,0,0,2},{0,0,1,1}};
InvarCases[10,5]={{0,0,0,0,0}};

InvarCases[12,1]={{10}};
InvarCases[12,2]={{0,8},{1,7},{2,6},{3,5},{4,4}};
InvarCases[12,3]={{0,0,6},{0,1,5},{0,2,4},{1,1,4},{0,3,3},{1,2,3},{2,2,2}};
InvarCases[12,4]={{0,0,0,4},{0,0,1,3},{0,0,2,2},{0,1,1,2},{1,1,1,1}};
InvarCases[12,5]={{0,0,0,0,2},{0,0,0,1,1}};
InvarCases[12,6]={{0,0,0,0,0,0}};

InvarCases[14,7]={{0,0,0,0,0,0,0}};

InvarCases[x_]:=Throw[Print["Invalid order ",x]];
InvarCases[args__]:=Throw[Print["Invar cannot work with the cases ",{args}]];


InvarDualCases[]:=Flatten[Table[InvarDualCases[order],{order,2,10,2}],1];
InvarDualCases[order_Integer?EvenQ]:=InvarDualCases[order,Reverse@Range[order/2]];
InvarDualCases[order_Integer?EvenQ,list_List]:=Flatten[InvarDualCases[order,#]&/@list,1];

InvarDualCases[10]:=InvarDualCases[10,5];

InvarDualCases[2,1]={{0}};

InvarDualCases[4,1]={{2}};
InvarDualCases[4,2]={{0,0}};

InvarDualCases[6,1]={{4}};
InvarDualCases[6,2]={{0,2},{1,1}};
InvarDualCases[6,3]={{0,0,0}};

InvarDualCases[8,1]={{6}};
InvarDualCases[8,2]={{0,4},{1,3},{2,2}};
InvarDualCases[8,3]={{0,0,2},{0,1,1}};
InvarDualCases[8,4]={{0,0,0,0}};

InvarDualCases[10,5]={{0,0,0,0,0}};

InvarDualCases[x_]:=Throw[Print["Invalid order ",x]];
InvarDualCases[args__]:=Throw[Print["Invar cannot work with the dual cases ",{args}]];


MaxIndex[n_Integer?Positive]:=MaxIndex[Table[0,{n}]];

MaxIndex[{}]:=0;

MaxIndex[{0}]:=1;

MaxIndex[{0,0}]:=3;
MaxIndex[{2}]:=2;

MaxIndex[{0,0,0}]:=9;
MaxIndex[{0,2}]:=12;
MaxIndex[{1,1}]:=12;
MaxIndex[{4}]:=12;

MaxIndex[{0,0,0,0}]:=38;
MaxIndex[{0,0,2}]:=99;
MaxIndex[{0,1,1}]:=125;
MaxIndex[{0,4}]:=126;
MaxIndex[{1,3}]:=138;
MaxIndex[{2,2}]:=86;
MaxIndex[{6}]:=105;

MaxIndex[{0,0,0,0,0}]:=204;
MaxIndex[{0,0,0,2}]:=1020;
MaxIndex[{0,0,1,1}]:=1749;
MaxIndex[{0,0,4}]:=1473;
MaxIndex[{0,1,3}]:=3099;
MaxIndex[{0,2,2}]:=1622;
MaxIndex[{1,1,2}]:=1617;
MaxIndex[{0,6}]:=1665;
MaxIndex[{1,5}]:=1770;
MaxIndex[{2,4}]:=1746;
MaxIndex[{3,3}]:=962;
MaxIndex[{8}]:=1155;

MaxIndex[{0,0,0,0,0,0}]:=1613;
MaxIndex[{0,0,0,0,2}]:=12722;
MaxIndex[{0,0,0,1,1}]:=27022;
MaxIndex[{0,0,0,4}]:=19617;
MaxIndex[{0,0,1,3}]:=60984;
MaxIndex[{0,0,2,2}]:=30974;
MaxIndex[{0,1,1,2}]:=62465;
MaxIndex[{1,1,1,1}]:=5606;
MaxIndex[{0,0,6}]:=25590;
MaxIndex[{0,1,5}]:=53160;
MaxIndex[{0,2,4}]:=52764;
MaxIndex[{1,1,4}]:=27396;
MaxIndex[{0,3,3}]:=27024;
MaxIndex[{1,2,3}]:=54654;
MaxIndex[{2,2,2}]:=9104;
MaxIndex[{0,8}]:=25515;
MaxIndex[{1,7}]:=26670;
MaxIndex[{2,6}]:=26460;
MaxIndex[{3,5}]:=26670;
MaxIndex[{4,4}]:=13607;
MaxIndex[{10}]:=15120;

MaxIndex[{0,0,0,0,0,0,0}]:=16532;

MaxIndex[{0,0,0,0,0,0,0,0}]:=217395;

MaxIndex[{0,0,0,0,0,0,0,0,0}]:=3406747;

MaxIndex[case_]:=Throw@Print["Case ",case," not included."];


MaxDualIndex[n_Integer]:=MaxDualIndex[Table[0,{n}]];

MaxDualIndex[{}]=0;

MaxDualIndex[{0}]=1;

MaxDualIndex[{0,0}]=4;
MaxDualIndex[{2}]=3;

MaxDualIndex[{0,0,0}]=27;
MaxDualIndex[{0,2}]=58;
MaxDualIndex[{1,1}]=36;
MaxDualIndex[{4}]=32;

MaxDualIndex[{0,0,0,0}]=232;
MaxDualIndex[{0,0,2}]=967;
MaxDualIndex[{0,1,1}]=1047;
MaxDualIndex[{0,4}]=876;
MaxDualIndex[{1,3}]=920;
MaxDualIndex[{2,2}]=478;
MaxDualIndex[{6}]=435;

MaxDualIndex[{0,0,0,0,0}]=2582;

MaxDualIndex[{0,0,0,0,0,0}]=35090;

MaxDualIndex[{0,0,0,0,0,0,0}]=558323;

MaxDualIndex[case_]:=Throw[Print["Dual case", case," not included."]];


RInvs[1,case_List]:=ProtSet[RInvs[1,case],Map[RInv[case,#]&,Range[MaxIndex[case]]]];
DualRInvs[1,case_List]:=ProtSet[DualRInvs[1,case],Map[DualRInv[case,#]&,Range[MaxDualIndex[case]]]];
WInvs[1,case_List]:=ProtSet[WInvs[1,case],Map[WInv[case,#]&,Range[MaxIndex[case]]]];
DualWInvs[1,case_List]:=ProtSet[DualWInvs[1,case],Map[DualWInv[case,#]&,Range[MaxDualIndex[case]]]];


DispatchRules[RInv,step_,case_,dim___]:=Block[{sigma},DispatchRules[RInv,step,case,dim]=Dispatch[RInvRules[step,case,dim]]];
DispatchRules[DualRInv,step_,case_,dim___]:=DispatchRules[DualRInv,step,case,dim]=Dispatch[DualRInvRules[step,case,dim]];
DispatchRules[WInv,step_,case_,dim___]:=Block[{sigma},DispatchRules[WInv,step,case,dim]=Dispatch[WInvRules[step,case,dim]]];
DispatchRules[DualWInv,step_,case_,dim___]:=DispatchRules[DualWInv,step,case,dim]=Dispatch[DualWInvRules[step,case,dim]];


RInvRules[1,case_List]:=Module[{result},
MaxIndex[case];
Print["Reading InvRules for step 1 and case ",case];result=Inner[Rule,tmphead@@RInvs[1,case],tmphead@@ReadInvarPerms[$DataBaseDir<>"1/"<>filename[1,case],"Maple"],List];
ProtSet[RInvRules[1,case],result]
];


DualRInvRules[1,case_List]:=Module[{result},
MaxDualIndex[case];
Print["Reading DualInvRules for step 1 and case ",case];result=Inner[Rule,tmphead@@DualRInvs[1,case],tmphead@@ReadInvarPerms[$DataBaseDir<>"1/"<>dualfilename[1,case],"Maple"],List];
ProtSet[DualRInvRules[1,case],result]
];


reversecycles[inv_->{cycles__}]:=Cycles[cycles]->inv;


DispatchRInvToPermRules[case_List]:=DispatchRInvToPermRules[case]=Dispatch[RInvRules[1,case]];
DispatchPermToRInvRules[case_List]:=DispatchPermToRInvRules[case]=Dispatch[reversecycles/@RInvRules[1,case]];
DispatchDualRInvToPermRules[case_List]:=DispatchDualRInvToPermRules[case]=Dispatch[DualRInvRules[1,case]];
DispatchPermToDualRInvRules[case_List]:=DispatchPermToDualRInvRules[case]=Dispatch[reversecycles/@DualRInvRules[1,case]];


WInvRules[1,case_]:=RInvRules[1,case]/.RInv->WInv;
DualWInvRules[1,case_]:=DualRInvRules[1,case]/.DualRInv->DualWInv;


RInvs[2,case_]:=ProtSet[RInvs[2,case],Complement[RInvs[1,case],First/@RInvRules[2,case]]];
DualRInvs[2,case_]:=ProtSet[DualRInvs[2,case],Complement[DualRInvs[1,case],First/@DualRInvRules[2,case]]];


RInvRules[2,case_List]:=Module[{result},
MaxIndex[case];
Print["Reading InvRules for step 2 and case ",case];
result=ReadInvarRules[$DataBaseDir<>"2/"<>filename[2,case],"Mathematica"];
ProtSet[RInvRules[2,case],result]
];


DualRInvRules[2,case_List]:=Module[{result},
MaxDualIndex[case];
Print["Reading DualInvRules for step 2 and case ",case];result=ReadInvarRules[$DataBaseDir<>"2/"<>dualfilename[2,case],"Mathematica"];
ProtSet[DualRInvRules[2,case],result]
];


RInvs[3,case_]:=ProtSet[RInvs[3,case],Complement[RInvs[2,case],First/@RInvRules[3,case]]];
DualRInvs[3,case_]:=ProtSet[DualRInvs[3,case],Complement[DualRInvs[2,case],First/@DualRInvRules[3,case]]];


RInvRules[3,case_List]:=Module[{result},
MaxIndex[case];
Print["Reading InvRules for step 3 and case ",case];
result=ReadInvarRules[$DataBaseDir<>"3/"<>filename[3,case],"Mathematica"];
ProtSet[RInvRules[3,case],result]
];


DualRInvRules[3,case_List]:=Module[{result},
MaxDualIndex[case];
Print["Reading DualInvRules for step 3 and case ",case];result=ReadInvarRules[$DataBaseDir<>"3/"<>dualfilename[3,case],"Mathematica"];
ProtSet[DualRInvRules[3,case],result]
];


RInvs[4,case_]:=ProtSet[RInvs[4,case],Complement[RInvs[3,case],First/@RInvRules[4,case]]];
DualRInvs[4,case_]:=ProtSet[DualRInvs[4,case],Complement[DualRInvs[3,case],First/@DualRInvRules[4,case]]];


$ExpandedCommuteOrder12Q=False;


extendedQ[case_]:=$ExpandedCommuteOrder12Q/;MemberQ[InvarCases[12,{1,2,3}],case];
extendedQ[case_]:=True;


RInvRules[4,case_List]:=Module[{result,eQ=If[extendedQ[case],"","NE"]},
MaxIndex[case];
Print["Reading "<>eQ<>"InvRules for step 4 and case ",case];
result=ReadInvarRules[$DataBaseDir<>"4/"<>filename[4,case]<>eQ,"Mathematica"];
ProtSet[RInvRules[4,case],result]
];


DualRInvRules[4,case_List]:=Module[{result},
MaxDualIndex[case];
Print["Reading DualInvRules for step 4 and case ",case];result=ReadInvarRules[$DataBaseDir<>"4/"<>dualfilename[4,case],"Mathematica"];
ProtSet[DualRInvRules[4,case],result]
];


RInvs[5,case_]:=RInvs[5,case,$DefaultDim];
RInvs[5,case_,dim_]:=ProtSet[RInvs[5,case,dim],Complement[RInvs[4,case],First/@RInvRules[5,case,dim]]];
DualRInvs[5,case_]:=DualRInvs[5,case,$DefaultDim];
DualRInvs[5,case_,dim_]:=ProtSet[DualRInvs[5,case,dim],Complement[DualRInvs[4,case],First/@DualRInvRules[5,case,dim]]];


RInvRules[5,case_List]:=RInvRules[5,case,$DefaultDim];
RInvRules[5,case_List,dim_]:=Module[{result},
MaxIndex[case];
Print["Reading InvRules for step 5, case ",case," and dimension ",dim];
result=ReadInvarRules[$DataBaseDir<>"5_"<>ToString[dim]<>"/"<>filename[5,case,dim],"Mathematica"];
ProtSet[RInvRules[5,case,dim],result]
];


DualRInvRules[5,case_List]:=DualRInvRules[5,case,$DefaultDim];
DualRInvRules[5,case_List,dim_]:=Module[{result},
MaxDualIndex[case];
Print["Reading DualInvRules for step 5 and case ",case];result=ReadInvarRules[$DataBaseDir<>"5_"<>ToString[dim]<>"/"<>dualfilename[5,case,dim],"Mathematica"];
ProtSet[DualRInvRules[5,case,dim],result]
];


RInvs[6,case_]:=RInvs[6,case,$DefaultDim];
RInvs[6,case_,dim_]:=ProtSet[RInvs[6,case,dim],Complement[RInvs[5,case,dim],First/@RInvRules[6,case,dim]]];
DualRInvs[6,case_]:=DualRInvs[6,case,$DefaultDim];
DualRInvs[6,case_,dim_]:=ProtSet[DualRInvs[6,case,dim],DualRInvs[5,case,dim]];


RInvRules[6,case_List]:=RInvRules[6,case,$DefaultDim];
RInvRules[6,case_List,dim_]:=Block[{sigma},
Module[{result},
MaxIndex[case];
Print["Reading InvRules for step 6, case ",case," and dimension ",dim];
result=ReadInvarRules[$DataBaseDir<>"6_"<>ToString[dim]<>"/"<>filename[6,case,dim],"Mathematica"];
ProtSet[RInvRules[6,case,dim],result]
]];


InvSimplify[expr_]:=InvSimplify[expr,$InvSimplifyLevel];
InvSimplify[expr_,1]:=expr;
InvSimplify[expr_,step_]:=Expand[expr/.{inv:(RInv|DualRInv|WInv|DualWInv)[metric_][_,_]:>Block[{dim=DimOfVBundle@VBundleOfMetric@metric,sigma=SignDetOfMetric[metric]},InvSimplify1[inv,step,dim]]}];
SetNumberOfArguments[InvSimplify,{1,2}];


InvSimplify1[inv_,1,_]:=inv;
InvSimplify1[(DualRInv|DualWInv)[_][_,_],_,dim_Integer]:=Print["Duals are only handled in dimension 4."]/;dim=!=4;
InvSimplify1[_,5|6,dim_Integer]:=Print["Levels 5 and 6 of simplication are only possible in dimension 4."]/;dim=!=4;
InvSimplify1[inv:_[metric_][_,_],step:(2|3|4|5|6),dim_]:=Expand[InvSimplify56[InvSimplify234[inv,step],step]/.{RInv->RInv[metric],DualRInv->DualRInv[metric],WInv->WInv[metric],DualWInv->DualWInv[metric]}];
InvSimplify1[inv_,step_,dim_]:=Print["Invalid level of simplification: ",step];


InvSimplify234[RInv[metric_][case_,count_],step_]:=Module[{newexpr=RInv[case,count]},
If[FreeQ[RInvs[step,case],newexpr],
newexpr=newexpr/.DispatchRules[RInv,2,case];
newexpr=If[step>=3,newexpr/.DispatchRules[RInv,3,case],newexpr];
newexpr=If[step>=4,newexpr/.DispatchRules[RInv,4,case],newexpr]];
newexpr
];
InvSimplify234[DualRInv[metric_][case_,count_],step_]:=
Module[{newexpr=DualRInv[case,count]},
If[FreeQ[DualRInvs[step,case],newexpr],
newexpr=newexpr/.DispatchRules[DualRInv,2,case];
newexpr=If[step>=3,newexpr/.DispatchRules[DualRInv,3,case],newexpr];
newexpr=If[step>=4,newexpr/.DispatchRules[DualRInv,4,case],newexpr]];
newexpr
];
InvSimplify234[WInv[metric_][case_,count_],step_]:=
If[MemberQ[RInvs[step,case],RInv[case,count]],
WInv[case,count],
Module[{newexpr=RInv[case,count]/.DispatchRules[RInv,2,case]/.RInv->WInv},
newexpr=If[step>=3,newexpr/.DispatchRules[WInv,3,case],newexpr];
newexpr=If[step>=4,newexpr/.DispatchRules[WInv,4,case],newexpr];
newexpr]
];
InvSimplify234[DualWInv[metric_][case_,count_],step_]:=If[MemberQ[DualRInvs[step,case],DualRInv[case,count]],
DualWInv[case,count],
Module[{newexpr=DualRInv[case,count]/.DispatchRules[DualRInv,2,case]/.DualRInv->DualWInv},
newexpr=If[step>=3,newexpr/.DispatchRules[DualWInv,3,case],newexpr];
newexpr=If[step>=4,newexpr/.DispatchRules[DualWInv,4,case],newexpr];
newexpr]
];


RulesFor[inv_[case_,_],step_]:=DispatchRules[inv,step,case,dim]


InvSimplify56[expr_,step_/;step<5]:=expr;
InvSimplify56[expr_,5]:=expr/.{inv:(RInv|DualRInv|WInv|DualWInv)[case_,_]:>(inv/.RulesFor[inv,5])};
InvSimplify56[expr_,6]:=InvSimplify56[expr,5]/.{inv:(RInv|WInv)[case_,_]:>(inv/.RulesFor[inv,6])};


toCycles[cycles:{___List}]:=Apply[Cycles,cycles];


InvToPerm[expr_]:=expr/.inv:(RInv|DualRInv|WInv|DualWInv)[_][_,_]:>InvToPerm[inv];
InvToPerm[RInv[metric_][case_,count_]]:=RPerm[metric][{case,0},toCycles[RInv[case,count]/.DispatchRInvToPermRules[case]]];
InvToPerm[DualRInv[metric_][case_,count_]]:=RPerm[metric][{case,1},toCycles[DualRInv[case,count]/.DispatchDualRInvToPermRules[case]]];
InvToPerm[WInv[metric_][case_,count_]]:=WPerm[metric][{case,0},toCycles[RInv[case,count]/.DispatchRInvToPermRules[case]]];
InvToPerm[DualWInv[metric_][case_,count_]]:=WPerm[metric][{case,1},toCycles[DualRInv[case,count]/.DispatchDualRInvToPermRules[case]]];
SetNumberOfArguments[InvToPerm,1];


upindices[metric_]:=upindices[metric]=GetIndicesOfVBundle[VBundleOfMetric[metric],18];
canon[metric_]:=canon[metric]=Flatten@Transpose[{upindices[metric],Minus/@upindices[metric]}];


translate[RPerm[metric_][{case_,dege_},perm_]]:=First@TranslatePerm[perm,{Images,4Length[case]+Plus@@case+4dege}];


RPerm[metric_][info_,-perm_]:=-RPerm[metric][info,perm];
WPerm[metric_][info_,-perm_]:=-WPerm[metric][info,perm];


riemannof[metric_,0,indices_,{other___,n_}]:=With[{CD=CovDOfMetric[metric]},Apply[Composition,CD/@indices[[Range[4+n,5,-1]]]][Apply[Riemann[CD],Take[indices,4]]]riemannof[metric,0,Drop[indices,4+n],{other}]];
riemannof[metric_,1,indices_,case_]:=riemannof[metric,0,Drop[indices,-4],case]Apply[epsilon[metric],Take[indices,-4]];
riemannof[_,_,{},{}]:=1;


PermToRiemann[rperm_]:=PermToRiemann[rperm,$CurvatureRelations];
(* On Riemann permutations *)
PermToRiemann[rperm:RPerm[metric_][{case_,dege_},_],cr_]:=ToRicci[metric,cr][riemannof[metric,dege,canon[metric][[translate[rperm]]],Reverse[case]]];
(* On Weyl permutations *)
PermToRiemann[wperm:WPerm[metric_][{_,_},_],_]:=ToWeyl[PermToRiemann[wperm/.WPerm->RPerm,True],CovDOfMetric[metric]];
(* On general expressions *)
PermToRiemann[expr_,cr_]:=expr/.perm:(RPerm|WPerm)[_][__]:>PermToRiemann[perm,cr];
SetNumberOfArguments[PermToRiemann,{1,2}];


ToRicci[metric_,False][expr_]:=Scalar[expr];
ToRicci[metric_,True][expr_]:=Scalar@ReplaceDummies[ContractMetric[expr/.CurvatureRelations[CovDOfMetric[metric],Riemann],metric]/.CurvatureRelations[CovDOfMetric[metric],Ricci],IndexList@@upindices[metric]];
ToWeyl[expr_,cd_]:=expr/.{Riemann[cd]:>Weyl[cd],Ricci[cd]:>TFRicci[cd]};


InvToRiemann1[cr_][inv:(RInv|DualRInv|WInv|DualWInv)[_][_,_]]:=PermToRiemann[InvToPerm[inv],cr];


InvToRiemann1[False][(RInv|WInv)[metric_][{0},1]]:=Scalar[Riemann[CovDOfMetric[metric]]@@canon[metric][[{1,3,2,4}]]];
InvToRiemann1[True][(RInv|WInv)[metric_][{0},1]]:=RicciScalar[CovDOfMetric[metric]][];


InvToRiemann[expr_]:=InvToRiemann[expr,$CurvatureRelations];
InvToRiemann[expr_,cr_]:=expr/.inv:(RInv|DualRInv|WInv|DualWInv)[_][_,_]:>InvToRiemann1[cr][inv];
SetNumberOfArguments[InvToRiemann,{1,2}];


addone[head_[n_Integer]]:=head[n+1];
addone[casePlus[{head_,n_Integer}]]:=casePlus[{head,n+1}];


SetAttributes[casePlus,{Flat,Orderless}]
casePlus[{head_,n__},c___,{head_,m__}]:=casePlus[{head,m,n},c];
casePlus[0,c___]:=casePlus[c];
casePlus[ERROR]:=ERROR;
casePlus[ERROR,__]:=ERROR;


degrees[expr_,cd_,metric_]:=With[{
riemann=Riemann[cd],
ricci=Ricci[cd],
ricciscalar=RicciScalar[cd],
weyl=Weyl[cd],
tfricci=TFRicci[cd],
eps=epsilon[metric]},
Module[{deg},
deg[cd[_][expr1_]x_.]:=casePlus[addone[deg[expr1]],deg[x]];
deg[_riemann x_.]:=casePlus[{Riemann,0},deg[x]];
deg[_ricci x_.]:=casePlus[{Ricci,0},deg[x]];
deg[_ricciscalar x_.]:=casePlus[{RicciScalar,0},deg[x]];
deg[_weyl x_.]:=casePlus[{Weyl,0},deg[x]];
deg[_tfricci x_.]:=casePlus[{TFRicci,0},deg[x]];
deg[_eps x_.]:=casePlus[{epsilon,0},deg[x]];
deg[1|-1]:=0;
deg[x_]:=Infinity;
deg[expr]
]
]


(* First expand *)
RiemannToPerm[expr_,args___]:=RiemannToPerm0[ContractMetric@Expand[expr],args];
SetNumberOfArguments[RiemannToPerm,{1,2}];
(* Thread over sums *)
RiemannToPerm0[expr_Plus,args___]:=RiemannToPerm0[#,args]&/@expr;
RiemannToPerm0[expr_,args___]:=RiemannToPerm1[BreakScalars@PutScalar[expr],args];
(* Deal with the second argument *)
RiemannToPerm1[expr_]:=RiemannToPerm1[expr,$Metrics];
RiemannToPerm1[expr_,metrics_List]:=Fold[RiemannToPerm1,expr,metrics];
(* Possibilities for the first argument *)
RiemannToPerm1[Scalar[expr_],metric_Symbol]:=arrange[expr,degrees[expr,CovDOfMetric[metric],metric],metric];
RiemannToPerm1[expr_Times,metric_Symbol]:=RiemannToPerm1[#,metric]&/@expr;
RiemannToPerm1[tensor_Symbol[],metric_Symbol]:=RPerm[metric][{{0},0},xAct`xPerm`Cycles[{2,3}]]/;tensor===RicciScalar[CovDOfMetric[metric]];
RiemannToPerm1[Power[expr_,n_Integer],metric_Symbol]:=Power[RiemannToPerm1[expr,metric],n];
RiemannToPerm1[expr_,x_]:=expr;


(* 0. Trivial cases *)
arrange[0,_,_]:=0;
arrange[expr_,Infinity,_]:=expr;
(* 1. There is Ricci or RicciScalar: convert to Riemann *)
HoldPattern[arrange[expr_,casePlus[{Ricci|RicciScalar,m__},c___],metric_]]:=arrange[RicciToRiemann[expr,CovDOfMetric[metric]],casePlus[{Riemann,m},c],metric];
(* 2a. There are only Riemanns: compute non-dual permutation *)
HoldPattern[arrange[expr_,casePlus[{Riemann,Rcase__}],metric_]]:=With[{canonical={#[[1]],#[[2,1,1]]}&@Reap[ToCanonical[expr],"NewIndices"]},arrange1[canonical,{Sort[{Rcase}],0},metric]];
(* 2b. There are only Riemanns and an epsilon tensor: compute dual permutation. Oct 2011: epsilon must be sorted last *)
HoldPattern[arrange[expr_,casePlus[{Riemann,Rcase__},{epsilon,0}],metric_]]:=With[{epsilonname=epsilon[metric]},Module[{result},
TagSet[epsilonname,xSortPrecedence[epsilonname],Infinity];
With[{canonical={#[[1]],#[[2,1,1]]}&@Reap[ToCanonical[expr],"NewIndices"]},
result=arrange1[canonical,{Sort[{Rcase}],1},metric]
];
TagUnset[epsilonname,xSortPrecedence[epsilonname]];
result
]
];
(* 3. There are Weyl/TFRicci only: convert to Riemann. NOT UPDATED *)
arrange[expr_,{0,0,degWeyl_,degTFRic_,degeps_},metric_]:=With[{cd=CovDOfMetric[metric]},arrange[ToRiemann[FromWeyl[expr,cd],cd,metric],{degWeyl+degTFRic,0,0,0,degeps},metric]/.RPerm->WPerm];
(* 4. Final arrangements *)
arrange1[0,_,_]:=0;
arrange1[_Symbol[a_,b_,-a_,-b_],{{0},0},metric_]:=RPerm[metric][{{0},0},xAct`xPerm`Cycles[{2,3}]];
arrange1[{expr_,{sign_,perm_IndexList}},degs:{_,_},metric_]:=RPerm[metric][degs,sign TranslatePerm[PermutationFromTo[List@@perm,List@@IndexSort[perm]],xAct`xPerm`Cycles]];


FromWeyl[expr_,cd_]:=expr/.{Weyl[cd]:>Riemann[cd],TFRicci[cd]:>Ricci[cd]};


RicciToRiemann[expr_,cd_]:=With[{ricciscalar=RicciScalar[cd],ricci=Ricci[cd],riemann=Riemann[cd],vbundle=First@VBundlesOfCovD[cd]},expr/.{
ricciscalar[]:>Module[{c=DummyIn@vbundle,d=DummyIn@vbundle},$RicciSign riemann[c,d,-c,-d]],ricci[a_,b_]:>Module[{c=DummyIn@vbundle},$RicciSign riemann[a,-c,b,c]]
}
];


PermToInv[RPerm[metric_][{case_,0},perm_]]:=perm/.DispatchPermToRInvRules[case]/.RInv->RInv[metric];
PermToInv[RPerm[metric_][{case_,1},perm_]]:=perm/.DispatchPermToDualRInvRules[case]/.DualRInv->DualRInv[metric];
PermToInv[WPerm[metric_][{case_,0},perm_]]:=perm/.DispatchPermToRInvRules[case]/.RInv->WInv[metric];
PermToInv[WPerm[metric_][{case_,1},perm_]]:=perm/.DispatchPermToDualRInvRules[case]/.DualRInv->DualWInv[metric];
PermToInv[expr_]:=expr/.perm:(RPerm|WPerm)[_][__]:>PermToInv[perm];
SetNumberOfArguments[PermToInv,1];


RiemannToInv[expr_]:=RiemannToInv[expr,$Metrics];
RiemannToInv[expr_,metrics_List]:=Fold[RiemannToInv,expr,metrics];
RiemannToInv[expr_,metric_Symbol?MetricQ]:=PermToInv[RiemannToPerm[expr,metric]];
RiemannToInv[expr_,x_]:=(Message[RiemannToInv::unknown,"metric or list of metrics",x];expr);
SetNumberOfArguments[RiemannToInv,{1,2}];


RiemannSimplify[expr_]:=RiemannSimplify[expr,$InvSimplifyLevel,$CurvatureRelations,$Metrics];
RiemannSimplify[expr_,step_Integer]:=RiemannSimplify[expr,step,$CurvatureRelations,$Metrics];
RiemannSimplify[expr_,cr:(True|False)]:=RiemannSimplify[expr,$InvSimplifyLevel,cr,$Metrics];
RiemannSimplify[expr_,metric:(_Symbol?MetricQ|{__Symbol?MetricQ})]:=RiemannSimplify[expr,$InvSimplifyLevel,$CurvatureRelations,metric];
RiemannSimplify[expr_,step_Integer,cr:(True|False),metric_]:=InvToRiemann[InvSimplify[RiemannToInv[expr,metric],step],cr];
SetNumberOfArguments[RiemannSimplify,{1,3}];


RToWInv[RInv[metric_][deg_,index_],level_]:=setdim[RInv[deg,index]/.DispatchRToWRules[level,deg]/.WInv->WInv[metric],metric];
RToWInv[DualRInv[metric_][deg_,index_],level_]:=setdim[DualRInv[deg,index]/.DispatchRToWDualRules[level,deg]/.{DualWInv->DualWInv[metric],WInv->WInv[metric]},metric];
WToRInv[WInv[metric_][deg_,index_],level_]:=setdim[WInv[deg,index]/.DispatchWToRRules[level,deg]/.RInv->RInv[metric],metric];
WToRInv[DualWInv[metric_][deg_,index_],level_]:=setdim[DualWInv[deg,index]/.DispatchWToRDualRules[level,deg]/.{DualRInv->DualRInv[metric],RInv->RInv[metric]},metric];


setdim[expr_,metric_]:=With[{dimension=DimOfVBundle@VBundleOfMetric[metric]},expr/.dim->dimension]


RToW[expr_,level_:2]:=expr/.inv:(RInv|DualRInv)[_][_,_]:>RToWInv[inv,level];
WToR[expr_,level_:2]:=expr/.inv:(WInv|DualWInv)[_][_,_]:>WToRInv[inv,level];
SetNumberOfArguments[RToW,{1,2}];
SetNumberOfArguments[WToR,{1,2}];


RandomRiemannMonomial[case_]:=RandomRiemannMonomial[case,First[$Metrics]];
RandomRiemannMonomial[degree_Integer,metric_]:=RandomRiemannMonomial[Table[0,{degree}],metric];
RandomRiemannMonomial[case_List,metric_]:=PermToRiemann[RPerm[metric][{case,0},RandomPerm[4Length@case+Plus@@case,Images]]];
SetNumberOfArguments[RandomRiemannMonomial,{1,2}];


End[]


Protect["xAct`Invar`*"];


Unprotect[xAct`Invar`dim,xAct`Invar`sigma,xAct`Invar`$InvSimplifyLevel,xAct`Invar`$CurvatureRelations,xAct`Invar`$ExpandedCommuteOrder12Q,xAct`Invar`InvarDirectory];


EndPackage[]



