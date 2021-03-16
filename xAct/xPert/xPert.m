xAct`xPert`$Version={"1.0.6",{2018,2,28}};


xAct`xPert`$xTensorVersionExpected={"1.1.3",{2018,2,28}};


(* xPert, computer algebra for perturbation theory in General Relativity *)

(* Copyright (C) 2005-2018 David Brizuela, Jose M. Martin-Garcia and Guillermo A. Mena Marugan *)

(* This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of
 the License,or (at your option) any later version.

This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307,
  USA. 
*)


(* :Title: xPert *)

(* :Author: David Brizuela, Jose M. Martin-Garcia and 
	Guillermo A. Mena Marugan *)

(* :Summary: Computer algebra for perturbation theory in General
    Relativity *)

(* :Brief Discussion:
   - Single-parameter metric perturbation theory
   - Inert-head Perturbation and explicit expansion formulas
     through ExpandPerturbation
   - Handles densities, derivatives and all index positions in
     tensors
   - Formula for arbitrary gauge change 
*)
  
(* :Context: xAct`xPert` *)

(* :Package Version: 1.0.6 *)

(* :Copyright: David Brizuela, Jose M. Martin-Garcia and 
	Guillermo A. Mena Marugan (2005-2018) *)

(* :History: see xPert.History *)

(* :Keywords: *)

(* :Source: xPert.nb *)

(* :Warning: ToCanonical can only act on expressions with the
    Perturbation head with option UseMetricOnVBundle->None *)

(* :Mathematica Version: 6.0 and later *)

(* :Limitations: 
	- Only one perturbation structure can be defined.
	- Only single-parameter metric perturbation theory *)


With[{xAct`xPert`Private`xPertSymbols=DeleteCases[Join[Names["xAct`xPert`*"],Names["xAct`xPert`Private`*"]],"$Version"|"xAct`xPert`$Version"|"$xTensorVersionExpected"|"xAct`xPert`$xTensorVersionExpected"]},
Unprotect/@xAct`xPert`Private`xPertSymbols;
Clear/@xAct`xPert`Private`xPertSymbols;
]


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`xPert`"];


BeginPackage["xAct`xPert`",{"xAct`xTensor`","xAct`xPerm`","xAct`xCore`","xAct`ExpressionManipulation`"}]


If[Not@OrderedQ@Map[Last,{$xTensorVersionExpected,xAct`xTensor`$Version}],Throw@Message[General::versions,"xTensor",xAct`xTensor`$Version,$xTensorVersionExpected]]


Print[xAct`xCore`Private`bars];
Print["Package xAct`xPert`  version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2005-2018, David Brizuela, Jose M. Martin-Garcia and Guillermo A. Mena Marugan, under the General Public License."];


Off[General::shdw]
xAct`xPert`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


If[xAct`xCore`Private`$LastPackage==="xAct`xPert`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]];


ReportSet[$PrePrint,ScreenDollarIndices];


ReportSet[$CovDFormat,"Postfix"];


ReportSetOption[ContractMetric,AllowUpperDerivatives->True];


ReportSetOption[MakeRule,MetricOn->All];


ReportSetOption[MakeRule,ContractMetrics->True];


Perturbation::usage="Perturbation[expr] represents the perturbation of expr. Perturbation[expr, n] represents its n-th perturbation. Perturbation is an inert-head.";
Perturbed::usage="Perturbed[expr, n] returns the expansion of expr perturbed up to n-th order, using $PerturbationParameter.";
$PerturbationParameter::usage="$PerturbationParameter is a global variable containing the parameter used in the perturbative expansions.";
PerturbationParameter::usage="PerturbationParameter[metric] stores the perturbation parameter associated to the perturbation of the given metric.";
ExpandPerturbation::usage="ExpandPerturbation[expr] returns expr with all Perturbation expressions expanded in terms of metric perturbations. It uses fast pre-stored formulas for the general term of the perturbative expansion of the most common curvature tensors.";
DefTensorPerturbation::usage="DefTensorPerturbation[pert[LI[n], inds], tensor[inds], args] defines pert to be the perturbation of tensor, with the same index configuration. There is an internal call to DefTensor[pert[LI[n], inds], args].";
PerturbationOrder::usage="PerturbationOrder[expr] gives the perturbative order of expression, as given by the orders of its arguments. This function must be defined on those perturbations declared by the user.";
GaugeChange::usage="GaugeChange[pert, gen] returns the perturbative expression pert changed to a new gauge. The change of gauge is parametrized by the family of vector fields gen[LI[order], a]. Background objects are left untouched.";
GeneralPerturbation::usage="GeneralPerturbation[expr, n] returns ExpandPerturbation[Perturbation[expr, n]]. This is kept for backwards compatibility with older versions.";


DefMetricPerturbation::usage="DefMetricPerturbation[metric, pert, param] constructs a number of definitions for the perturbation of the given metric, which must exist already. A tensor pert[LI[order], -a, -b] is defined on the vbundle of the metric, and the parameter param will be defined and used to expand the perturbed expressions. DefMetricPerturbation[metric, pert] uses a default name for the parameter.";


xAct`xPert`$Version::usage="$Version is a global variable giving the version of the package xPert in use.";
xAct`xPert`$xTensorVersionExpected::usage="$xTensorVersionExpected is a global variable giving the oldest possible version of the package xTensor which is required by the version of the package xPert in use.";


Begin["xAct`xPert`Private`"]


group[list_List]:=Table[Join[Take[list,i-1],{list[[i]]+list[[i+1]]},Drop[list,i+1]],{i,1,Length[list]-1}]


SortedPartitions[n_Integer?Positive]:=Flatten[FixedPointList[Union@Flatten[group/@#,1]&,{Table[1,{n}]}],1]


addone[n_][{prev___Integer}]:=Table[{prev,i},{i,0,n-Total[{prev}]}];
onelevel[n_][list_List]:=Apply[Join,Map[addone[n],list]];
complete[n_][{prev___Integer}]:={prev,n-Total[{prev}]};
lastlevel[n_][list_List]:=Map[complete[n],list];


AllPartitions[0,n_Integer]:={};
AllPartitions[m_Integer,n_Integer]:=lastlevel[n][Nest[onelevel[n],{{}},m-1]];


LeibnizDistribute[operator_,n_,f_[args___],g_]:=Apply[g,Map[Multinomial@@# Inner[operator,{args},#,f]&,AllPartitions[Length[{args}],n]]];


FoldedPartitions[n_]:=Cases[Fold[step,fpart[{{},n},n],Reverse@Range[n-1]],{partition_,0}:>partition];


fpart[{{part___},0},d_]:={{{0,part},0}};
fpart[{{part___},m_},d_]:={{#,part},m-d #}&/@Range[0,m/d];
step[pairs_List,d_]:=Flatten[fpart[#,d]&/@pairs,1];


FaadiBruno[function_[args__],order_]:=Module[{vars=Table[Unique[x],{Length[{args}]}]},
Expand@Nest[Dt,function@@vars,order]/.Dt->Perturbation/.Power[x_Perturbation,n_]:>Scalar[x]^n/.g_?ScalarFunctionQ@@vars:>g[args]/.Inner[IndexRule,vars,{args},List]];


Off[RuleDelayed::rhs]


makepattern[a_Symbol]:=a_Symbol;
makepattern[-a_Symbol]:=-a_Symbol;


Options[DefTensorPerturbation]=Options[DefTensor];
DefTensorPerturbation[pert_[LI[_],inds___],tensor_[inds___],args__]:=Module[{prot,pinds=makepattern/@{inds}},
DefTensor[pert[LI["order"],inds],args];
xAct`xTensor`Private`SymbolRelations[pert,Null,{tensor}];
prot=Unprotect[tensor,pert];
tensor/:Perturbation[tensor@@pinds,Optional[order_Integer]]:=pert[LI[order],inds];
pert[LI[0],indices___]:=tensor[indices];
pert/:PerturbationOrder[pert[LI[order_],indices___]]:=order;
pert/:Perturbation[pert[LI[order_],Sequence@@pinds],n_.]:=pert[LI[order+n],inds];
Protect[Evaluate[prot]];
];
SetNumberOfArguments[DefTensorPerturbation,{2,Infinity}];
Protect[DefTensorPerturbation];


Options[DefMetricPerturbation]={PrintAs->Identity};


DefMetricPerturbation[metric_,pert_Symbol,param_Symbol:Automatic,options___?OptionQ]:=Module[{vbundle,manifold,indices,covd,christoffel,riemann,ricci,ricciscalar,einstein,weyl,tfricci,kretschmann,pa},

{pa}={PrintAs}/.CheckOptions[options]/.Options[DefMetricPerturbation];

(* Check of metric *)
If[!MetricQ[metric],Throw@Message[DefMetricPerturbation::unknown,"metric",metric]];
(* Define parameter if it does not exist yet *)
If[!ParameterQ[param],
If[param===Automatic,
DefParameter[GiveSymbol[PerturbationParameter,metric],PrintAs:>GiveOutputString[PerturbationParameter,metric]],
DefParameter[param]
]
];

vbundle=VBundleOfMetric[metric];
manifold=BaseOfVBundle[vbundle];
indices=GetIndicesOfVBundle[vbundle,4];
DefTensorPerturbation[pert[LI["order"],-indices[[1]],-indices[[2]]],metric[-indices[[1]],-indices[[2]]],DependenciesOfTensor[metric],Symmetric[{2,3}],PrintAs->pa];
(* Store perturbation structure in global variables *)
covd=CovDOfMetric[metric];
christoffel=Christoffel[covd];
riemann=Riemann[covd];
ricci=Ricci[covd];
ricciscalar=RicciScalar[covd];
einstein=Einstein[covd];
weyl=Weyl[covd];
tfricci=TFRicci[covd];
kretschmann=Kretschmann[covd];
$PerturbationParameter=param;

(* Define perturbation definitions *)
Unprotect[Perturbation,GeneralPerturbation];
MakexTensions[DefMetricPerturbation,"Beginning",metric,pert,param];
With[{a=indices[[1]],b=indices[[2]],c=indices[[3]],d=indices[[4]]},
DefGenPertInvMetric[vbundle,metric[a,b],pert];
DefGenPertDet[vbundle,metric,pert];
DefGenPertInvMetric[vbundle,metric[a,b],pert];
DefGenPertChristoffel[vbundle,covd,pert,christoffel[a,-b,-c]];
DefGenPertRiemannFromChristoffel[vbundle,covd,christoffel,riemann[-a,-b,-c,d]];
DefGenPertRiemann[vbundle,covd,pert,riemann[-a,-b,-c,d]];
DefGenPertRicci[vbundle,covd,pert,riemann,ricci[-a,-b]];
DefGenPertRicciScalar[vbundle,metric,ricci,ricciscalar];
DefGenPertEinstein[vbundle,metric,ricci,ricciscalar,einstein[-a,-b]];
DefGenPertWeyl[weyl[-a,-b,-c,-d]];
DefGenPertTFRicci[tfricci[-a,-b]];
DefGenPertKretschmann[vbundle,metric,riemann,kretschmann[]];
];
MakexTensions[DefMetricPerturbation,"End",metric,pert,param];
Protect[Perturbation,GeneralPerturbation];

];
SetNumberOfArguments[DefMetricPerturbation,{2,3}];
Protect[DefMetricPerturbation];


PerturbationParameter[metric_?MetricQ]:=GiveSymbol[PerturbationParameter,metric];
SetNumberOfArguments[PerturbationParameter,1];
Protect[PerturbationParameter];

PrintAsCharacter[PerturbationParameter]="\[CurlyEpsilon]";


ThreePert[covd_,pert_][LI[order_],a_,b_,c_]:=1/2(covd[c][pert[LI[order],a,b]]+covd[b][pert[LI[order],a,c]]-covd[a][pert[LI[order],b,c]])


(* Define linear inert head *)
DefInertHead[Perturbation,PrintAs->"\[EmptyUpTriangle]",DefInfo->False,ContractThrough->{delta},LinearQ->True];
(* Perturbation has two arguments, but the second defaults to 1 *)
Default[Perturbation,2]:=1;
Perturbation[expr_,0]:=expr;
Perturbation[expr_,1]:=Perturbation[expr];
(* Perturbation does not affect the density character *)
WeightOf[Perturbation[expr_,_.]]^:=WeightOf[expr];


Perturbation[_?ConstantQ,_.]:=0;
Perturbation[delta[a_,b_],_.]:=0;
Perturbation[PD[-a_][expr_],n_.]:=PD[-a][Perturbation[expr,n]];
Perturbation[cd_Symbol?CovDQ[-a_][expr_?ScalarQ],n_.]/;FreeQ[WeightOf@expr,WeightedWithBasis@cd]:=cd[-a][Perturbation[expr,n]];
Perturbation[ParamD[ps__][expr_],n_.]:=ParamD[ps][Perturbation[expr,n]];
Perturbation[OverDot[expr_],n_.]:=OverDot[Perturbation[expr,n]];
Perturbation[Scalar[expr_],n_.]:=Scalar[Perturbation[expr,n]];


Perturbation[product_Times,n_.]:=LeibnizDistribute[Perturbation,n,product,Plus];


Perturbation[product:_?ProductQ[__],n_.]:=LeibnizDistribute[Perturbation,n,product,Plus];


Perturbation/:Grade[Perturbation[expr_,n_.],prod_]:=Grade[expr,prod];


Perturbation[expr:_?ScalarFunctionQ[__],order_.]:=FaadiBruno[expr,order];


MakeBoxes[Perturbation?xAct`xTensor`Private`HeldInertHeadQ[expr_],StandardForm]:=xAct`xTensor`Private`interpretbox[Perturbation[expr],RowBox[{PrintAs[Perturbation],"[",MakeBoxes[expr,StandardForm],"]"}]];


MakeBoxes[Perturbation?xAct`xTensor`Private`HeldInertHeadQ[expr_,n_],StandardForm]:=xAct`xTensor`Private`interpretbox[Perturbation[expr,n],RowBox[{SuperscriptBox[PrintAs[Perturbation],ToString[n]],"[",MakeBoxes[expr,StandardForm],"]"}]];


Perturbation[Perturbation[expr_,n_.],m_.]:=Perturbation[expr,n+m];


SetNumberOfArguments[Perturbation,{1,2}];
Protect[Perturbation];


General::canonpert="ToCanonical on Perturbation may incorrectly change index characters.";
xAct`xTensor`Private`Identify[Perturbation[expr_,n_.]]:=(If[(UseMetricOnVBundle/.xAct`xTensor`Private`$TCOptions)=!=None,Message[ToCanonical::canonpert]];xAct`xTensor`Private`addhead1A[Perturbation[expr,n],Perturbation[#,n]&,expr]);


Perturbed[expr_,order_Integer]:=Sum[$PerturbationParameter^n Perturbation[expr,n]/n!,{n,0,order}];
SetNumberOfArguments[Perturbed,2];
Protect[Perturbed];


Options[ExpandPerturbation]:={SeparateMetric->True,OverDerivatives->True};
ExpandPerturbation[expr_,options___?OptionQ]:=expr/.expr1_Perturbation:>ExpandPerturbation1[expr1,options];
ExpandPerturbation[expr_,tensor_Symbol?xTensorQ,options___?OptionQ]:=expr/.expr1:Perturbation[tensor[___],n_.]:>ExpandPerturbation1[expr1,options];
ExpandPerturbation[expr_,list:{___Symbol?xTensorQ},options___?OptionQ]:=Fold[ExpandPerturbation[#1,#2,options]&,expr,list];
SetNumberOfArguments[ExpandPerturbation,{1,Infinity}];
Protect[ExpandPerturbation];


ExpandPerturbation1[Perturbation[expr_,n_.],options___]:=Module[{sepmetric,od,tmp},
{sepmetric,od}={SeparateMetric,OverDerivatives}/.CheckOptions[options]/.Options[ExpandPerturbation];

(* Separate metrics *)
tmp=Perturbation[If[sepmetric,SeparateMetric[][expr],expr],n];
(* Derivatives *)
If[od,tmp=tmp/.expr1:HoldPattern[Perturbation[_Symbol?CovDQ[_][_]|LieD[_][_]|Bracket[_,_][_],_.]]:>ExpandPerturbationDer[expr1]];
(* Reexpand *)
If[tmp=!=Perturbation[expr,n],tmp=ExpandPerturbation[tmp,options]];

(* Return result *)
tmp
];
ExpandPerturbation1[expr_,options___]:=expr;


GeneralPerturbation[expr_,order_:1]:=ExpandPerturbation[Perturbation[expr,order]];


ExpandPerturbationDer[Perturbation[cd_Symbol?CovDQ[-a_][expr_],n_.]]:=cd[-a][Perturbation[expr,n]]+With[{vbundles=VBundlesOfCovD[cd]},Plus@@Map[xAct`xTensor`Private`addChristoffel[expr,-a],xAct`xTensor`Private`selecton[Select[FindFreeIndices[expr],AIndexQ],vbundles]]/.expr1_ xAct`xTensor`Private`CHR[indices__]:>With[{chr=Christoffel[cd][indices]},Perturbation[expr1 chr,n]-chr Perturbation[expr1,n]]]+Module[{basis=WeightedWithBasis[cd],dummy=DummyIn@VBundleOfIndex[a],weight},weight=WeightOf[expr,basis];
If[weight===0,0,weight With[{chr=Christoffel[cd,xAct`xCoba`PDOfBasis[basis]][dummy,-a,-dummy]},-Perturbation[chr expr,n]+chr Perturbation[expr,n]]]];


ExpandPerturbationDer[Perturbation[LieD[v_][expr_],n_.]]:=Module[{lieD},
LeibnizDistribute[Perturbation,n,lieD[v,expr],Plus]/.lieD[V_,EXPR_]:>LieD[V][EXPR]
];


ExpandPerturbationDer[Perturbation[Bracket[v1_,v2_][a_Symbol],n_.],options___]:=Module[{bracket},
LeibnizDistribute[Perturbation,n,bracket[a][v1,v2],Plus]/.bracket[A_][V1_,V2_]:>Bracket[V1,V2][A]
];


homorder[list_List]:=If[Length[Union[list]]===1,First[list],list];
PerturbationOrder[expr_Times]:=Apply[Plus,Map[PerturbationOrder,List@@expr]];
PerturbationOrder[expr_Plus]:=homorder@Map[PerturbationOrder,List@@expr];
PerturbationOrder[Power[expr_,n_]]:=n PerturbationOrder[expr];
PerturbationOrder[Perturbation[expr_,n_.]]:=n;
PerturbationOrder[_?FirstDerQ[expr_]]:=PerturbationOrder[expr];
PerturbationOrder[expr_]:=0;
SetNumberOfArguments[PerturbationOrder,1];
Protect[PerturbationOrder];


GaugeChange[pert_Plus,gen_]:=GaugeChange[#,gen]&/@pert;
GaugeChange[pert_Times,gen_]:=GaugeChange[#,gen]&/@pert;
GaugeChange[func_Symbol?ScalarFunction[args__],gen_]:=Apply[func,GaugeChange[#,gen]&/@{args}];
GaugeChange[Scalar[pert_]^Optional[n_Integer],gen_]:=Scalar[xAct`xPert`GaugeChange[pert,gen]]^n;
GaugeChange[expr_,gen_]:=GaugeChange[expr,gen,PerturbationOrder[expr]];
GaugeChange[expr_,gen_,0]:=expr;
GaugeChange[expr_Perturbation,gen_,n_]:=Bruni[expr,gen,n];
GaugeChange[expr_,gen_,n_]:=With[{sexpr=SeparateMetric[][expr]},If[expr===sexpr,Bruni[expr,gen,n],GaugeChange[sexpr,gen]]];
Bruni[expr_,gen_,n_]:=expr+Expand@Sum[n!/(n-m)!
Apply[Plus,BruniTerm[Perturbation[expr,-m],gen,#]&/@FoldedPartitions[m]],{m,1,n}];
SetNumberOfArguments[GaugeChange,{2,3}];
Protect[GaugeChange];


BruniTerm[expr_,gen_,partition_]:=With[{dummy=DummyIn@Last@SlotsOfTensor[gen],range=Range@Length@partition},1/Inner[Power,Factorial[range],partition,Times]/Times@@Factorial[partition]Fold[Nest[LieD[gen[LI[First@#2],dummy]],#1,Last@#2]&,expr,Reverse@Transpose[{range,partition}]]];


productPert[{p_},vbundle_,pert_][a_,b_]:=pert[LI[p],a,b]
productPert[partition_List,vbundle_,pert_][a_,b_]:=Module[{m=Length[partition],i,indices},
indices=Table[DummyIn[vbundle],{m-1}];pert[LI[partition[[1]]],a,indices[[1]]]Product[pert[LI[partition[[i+1]]],ChangeIndex[indices[[i]]],indices[[i+1]]],{i,1,m-2}]pert[LI[partition[[m]]],ChangeIndex[indices[[m-1]]],b]
];


DefGenPertInvMetric[vbundle_,metric_[a_,b_],pert_]:=(
ExpandPerturbation1[Perturbation[metric[a_Symbol,b_Symbol],order_.],options___]:=Plus@@(Map[(-1)^Length[#](Multinomial@@#)productPert[#,vbundle,pert][a,b]&,SortedPartitions[order]])
);


productPertDet0[{p_},vbundle_,pert_]:=With[{ind=DummyIn[vbundle]},
pert[LI[p],-ind,ind]/p!
];

productPertDet0[partition_List,vbundle_,pert_]:=Module[{m=Length[partition],i,indices},
indices=Table[DummyIn[vbundle],{m}];
1/Times@@Map[#!&,partition]
 pert[LI[partition[[1]]],ChangeIndex[indices[[m]]],indices[[1]]]Product[pert[LI[partition[[i+1]]],ChangeIndex[indices[[i]]],indices[[i+1]]],{i,1,m-1}]
];

productPertDet1[partitionK_List,partitionL_List,vbundle_,pert_]:=With[{m=Length[partitionK]},
(-1)^m/m!/Times@@partitionK If[m>=2,Product[productPertDet0[Take[partitionL,{1+Plus@@Take[partitionK,i-1],Plus@@Take[partitionK,i]}],vbundle,pert],{i,2,m}],
1]
productPertDet0[Take[partitionL,{1,partitionK[[1]]}],vbundle,pert]
];

productPertDet2[partition_List,vbundle_,pert_]:=Plus@@Map[ productPertDet1[#,partition,vbundle,pert]&,SortedPartitions[Length[partition]]];


DefGenPertDet[vbundle_,metric_,pert_]:=With[{dim=DimOfVBundle[vbundle],metricepsilon=epsilon[metric],mdet=Determinant[metric][]},

ExpandPerturbation1[Perturbation[mdet,order_.]]:=With[{inds=Table[DummyIn[vbundle],{2dim}]},
With[{inds1=Take[inds,dim],inds2=Take[inds,-dim]},ContractMetric[mdet/dim!SignDetOfMetric[metric]metricepsilon@@inds1 metricepsilon@@inds2 Perturbation[Times@@Apply[metric,Transpose[-{inds1,inds2}],{1}],order]]
]
]/;IntegerQ[dim]&&order>dim;

ExpandPerturbation1[Perturbation[mdet,order_.]]:=
mdet order!Plus@@Map[(-1)^Length[#]productPertDet2[#,vbundle,pert]&,SortedPartitions[order]]/;!IntegerQ[dim]||order<=dim;

ExpandPerturbation1[Perturbation[metricepsilon[superinds__?UpIndexQ],order_.]]:=metricepsilon[superinds]Sqrt[mdet]ExpandPerturbation[Perturbation[1/Sqrt[mdet],order]];
ExpandPerturbation1[Perturbation[metricepsilon[subinds__?DownIndexQ],order_.]]:=metricepsilon[subinds]/Sqrt[mdet]ExpandPerturbation[Perturbation[Sqrt[mdet],order]];
];


productThreePert[{p_},vbundle_,covd_,pert_][a_,b_,c_]:=ThreePert[covd,pert][LI[p],a,b,c]
productThreePert[partition_List,vbundle_,covd_,pert_][a_,b_,c_]:=Module[{m=Length[partition],i,indices},
indices=Table[DummyIn[vbundle],{m-1}];pert[LI[partition[[1]]],a,indices[[1]]]Product[pert[LI[partition[[i+1]]],ChangeIndex[indices[[i]]],indices[[i+1]]],{i,1,m-2}]ThreePert[covd,pert][LI[partition[[m]]],ChangeIndex[indices[[m-1]]],b,c]]


DefGenPertChristoffel[vbundle_,covd_,pert_,christoffel_[a_,-b_,-c_]]:=(
ExpandPerturbation1[Perturbation[christoffel[a_Symbol,-b_Symbol,-c_Symbol],order_.]]:=Plus@@(Map[-(-1)^Length[#](Multinomial@@#)productThreePert[#,vbundle,covd,pert][a,-b,-c]&,SortedPartitions[order]])
);


productRiemann1[{p_},vbundle_,covd_,pert_][a_,b_,c_,d_]:=covd[a][ThreePert[covd,pert][LI[p],d,c,b]]
productRiemann1[partition_List,vbundle_,covd_,pert_][a_,b_,c_,d_]:=Module[{m=Length[partition],i,indices},
indices=Table[DummyIn[vbundle],{m-1}];pert[LI[partition[[1]]],d,indices[[1]]]Product[pert[LI[partition[[i+1]]],ChangeIndex[indices[[i]]],indices[[i+1]]],{i,1,m-2}]covd[a][ThreePert[covd,pert][LI[partition[[m]]],ChangeIndex[indices[[m-1]]],c,b]]]


productRiemann2[partition_List,s_,vbundle_,covd_,pert_][a_,b_,c_,d_]:=Module[{m=Length[partition],i,indices},
indices=Table[DummyIn[vbundle],{s}];ThreePert[covd,pert][LI[partition[[s]]],indices[[s]],d,a]Product[pert[LI[partition[[i]]],ChangeIndex[indices[[i+1]]],indices[[i]]],{i,s-1,2,-1}]ThreePert[covd,pert][LI[partition[[1]]],ChangeIndex[indices[[2]]],b,c]]/;s==Length[partition]
productRiemann2[partition_List,s_,vbundle_,covd_,pert_][a_,b_,c_,d_]:=Module[{m=Length[partition],i,indices},
indices=Table[DummyIn[vbundle],{m}];pert[LI[partition[[m]]],d,indices[[m]]]Product[pert[LI[partition[[i]]],ChangeIndex[indices[[i+1]]],indices[[i]]],{i,m-1,s+1,-1}]ThreePert[covd,pert][LI[partition[[s]]],indices[[s]],ChangeIndex[indices[[s+1]]],a]Product[pert[LI[partition[[i]]],ChangeIndex[indices[[i+1]]],indices[[i]]],{i,s-1,2,-1}]ThreePert[covd,pert][LI[partition[[1]]],ChangeIndex[indices[[2]]],b,c]]/;2<=s<Length[partition]


genpertriemann[vbundle_,covd_,pert_,IndexList[-a_,-b_,-c_,d_],order_]:=2$RiemannSign Antisymmetrize[Plus@@(Map[(-1)^Length[#](Multinomial@@#)(productRiemann1[#,vbundle,covd,pert][-a,-b,-c,d]+Sum[productRiemann2[#,s,vbundle,covd,pert][-a,-b,-c,d],{s,2,Length[#]}])&,SortedPartitions[order]]),{-a,-b}];


DefGenPertRiemann[vbundle_,covd_,pert_,riemann_[-a_,-b_,-c_,d_]]:=(
ExpandPerturbation1[Perturbation[riemann[-a_Symbol,-b_Symbol,-c_Symbol,d_Symbol],order_.]]:=genpertriemann[vbundle,covd,pert,IndexList[-a,-b,-c,d],order];ExpandPerturbation1[Perturbation[riemann[-a_Symbol,-b_Symbol,-c_Symbol,-d_Symbol],order_.]]:=With[{dummy=DummyAs[c],metric=xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfIndex[d],True]},ExpandPerturbation@Perturbation[riemann[-a,-b,-c,dummy]metric[-dummy,-d],order]]
);


DefGenPertRicci[vbundle_,covd_,pert_,riemann_,ricci_[-a_,-b_]]:=(ExpandPerturbation1[Perturbation[ricci[-a_Symbol,-b_Symbol],order_.]]:=Module[{dummy=DummyIn[vbundle]},$RicciSign genpertriemann[vbundle,covd,pert,IndexList[-a,-dummy,-b,dummy],order]]);


DefGenPertRicciScalar[vbundle_,metric_,ricci_,ricciscalar_]:=(ExpandPerturbation1[Perturbation[ricciscalar[],order_.]]:=Module[{dummy1=DummyIn[vbundle],dummy2=DummyIn[vbundle]},Sum[Binomial[order,k]ExpandPerturbation1@Perturbation[ricci[-dummy1,-dummy2],k]ExpandPerturbation1@Perturbation[metric[dummy1,dummy2],order-k],{k,0,order}]]);


DefGenPertEinstein[vbundle_,metric_,ricci_,ricciscalar_,einstein_[-a_,-b_]]:=(ExpandPerturbation1[Perturbation[einstein[-a_Symbol,-b_Symbol],order_.]]:=ExpandPerturbation1@Perturbation[ricci[-a,-b],order]-1/2Sum[Binomial[order,k]ExpandPerturbation1@Perturbation[metric[-a,-b],k]ExpandPerturbation1@Perturbation[ricciscalar[],order-k],{k,0,order}]);


DefGenPertWeyl[weyl_[-a_,-b_,-c_,-d_]]:=(ExpandPerturbation1[Perturbation[weyl[-a_Symbol,-b_Symbol,-c_Symbol,-d_Symbol],order_.]]:=ExpandPerturbation[Perturbation[WeylToRiemann[weyl[-a,-b,-c,-d]],order]]);


DefGenPertTFRicci[tfricci_[-a_,-b_]]:=(ExpandPerturbation1[Perturbation[tfricci[-a_Symbol,-b_Symbol],order_.]]:=ExpandPerturbation[Perturbation[TFRicciToRicci[tfricci[-a,-b]],order]]);


DefGenPertKretschmann[vbundle_,metric_,riemann_,kretschmann_[]]:=Module[{inds=GetIndicesOfVBundle[vbundle,8]},ExpandPerturbation1[Perturbation[kretschmann[],order_.]]:=ExpandPerturbation[Perturbation[metric[inds[[1]],inds[[5]]]metric[inds[[2]],inds[[6]]]metric[inds[[3]],inds[[7]]]metric[inds[[4]],inds[[8]]]riemann[-inds[[1]],-inds[[2]],-inds[[3]],-inds[[4]]]riemann[-inds[[5]],-inds[[6]],-inds[[7]],-inds[[8]]],order]]
];


On[RuleDelayed::rhs]


End[]
EndPackage[]
