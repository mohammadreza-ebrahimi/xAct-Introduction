xAct`SpinFrames`$Version={"0.5.2",{2018,02,28}}


(* SpinFrames: Handling of spin frame formalisms *)

(* Copyright (C) 2014-2018 Thomas B\[ADoubleDot]ckdahl and Steffen Aksteiner *)

(* This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License,or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA. 
*)


(* :Title: SpinFrames *)

(* :Authors: Thomas B\[ADoubleDot]ckdahl and Steffen Aksteiner *)

(* :Summary: Handling of spin frame formalisms *)

(* :Brief Discussion:
   - Handles Newman-Penrose and Geroch-Held-Penrose formalisms. *)
  
(* :Context: xAct`SpinFrames` *)

(* :Package Version: 0.5.2 *)

(* :Copyright: Thomas B\[ADoubleDot]ckdahl and Steffen Aksteiner (2014-2018) *)

(* :History: See SpinFrames.History *)

(* :Keywords: *)

(* :Source: SpinFrames.nb *)

(* :Warning: *)

(* :Mathematica Version: 8.0 and later *)

(* :Limitations: *)
	
(* :Acknowledgements:
	The package is partly based on a notebook by Alfonso Garcia-Parrado Gomez-Lobo.
	The autors would like to thank Johannes Mosig, Alfonso Garcia-Parrado Gomez-Lobo 
	and Jose M. Martin-Garcia for helpful discussions. *)


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`SpinFrames`"];


BeginPackage["xAct`SpinFrames`",{"xAct`SymManipulator`","xAct`TexAct`","xAct`xCoba`","xAct`Spinors`","xAct`xTensor`","xAct`xPerm`","xAct`xCore`"}]


Print[xAct`xCore`Private`bars]
Print["Package xAct`SpinFrames`  version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2014-2018, Thomas B\[ADoubleDot]ckdahl and Steffen Aksteiner, under the General Public License."];


Off[General::shdw]
xAct`SymManipulator`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


If[xAct`xCore`Private`$LastPackage==="xAct`SpinFrames`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]]


DefNPTetrad::usage ="DefNPTetrad[NP,{lNP[a],nNP[a],mNP[a]}]";
DefSpinDyad::usage ="DefSpinDyad[Dyad,{o[A],\[Iota][A]},NP]";
VectorsOfTetrad::usage="VectorsOfTetrad[NP] gives the list of vectors in the tetrad NP.";
SpinorsOfDyad::usage="SpinorsOfDyad[Dyad] gives the list of dyad spinors.";
TetradToDyadRules::usage="TetradToDyadRules[tetrad] contains a list of rules that transforms the tetrad vectors into their corresponding spinor expressions.";
TetradToBasisRules::usage="TetradToBasisRules[tetrad] contains a list of rules that transforms the tetrad vectors to their corresponding Basis objects.";
BasisToTetradRules::usage="BasisToTetradRules[tetrad] contains a list of rules that transforms Basis objects to their corresponding tetrad vectors.";
DyadToBasisRules::usage ="DyadToBasisRules[dyad] contains a list of rules that transforms the dyad spinors to their corresponding Basis objects.";
BasisToDyadRules::usage ="BasisToDyadRules[dyad] contains a list of rules that transforms Basis objects to their corresponding dyad spinors.";
CovDDyadRules::usage ="CovDDyadRules[dyad] contains a list of rules that transforms covariant derivatives of the dyad spinors into NP spin oefficients.";
AutocanonicalizeComponents::usage="AutocanonicalizeComponents[T[inds]] where inds are basis indices, creates automatic rules for all non canoical index configurations, so they are replaced by their canonical ordering.";
NPSpinCoefficients::usage="NPSpinCoefficients[Dyad] gives the list of NP spin coefficients for Dyad.";
GHPSpinCoefficients::usage="GHPSpinCoefficients[Dyad] gives the list of GHP spin coefficients for Dyad.";
GHPWeightOf::usage="GHPWeightOf[expr] returns the GHP boost and spin weight of any expression. If the expression is not properly weighted, then an error message is returned. For expressions with undefined weight the default value {0,0} is assumed.";
NPToGHPSpinCoeffs::usage="NPToGHPSpinCoeffs[dyad] contains a list of rules that transforms the NP spin coefficients into GHP notation.";
GHPToNPSpinCoeffs::usage="GHPToNPSpinCoeffs[dyad] contains a list of rules that transforms the GHP spin coefficients into NP notation.";
GHPWeightRules::usage="GHPWeightRules[T, Dyad] sets the GHPWeightOf all components of the tensor T in Dyad assuming that T is unweighted.";
CovDToGHPRules::usage="CovDToGHPRules[Dyad] contains a list of rules that transforms covariant derivatives to GHP derivatives assuming that all covariant derivatives acts on properly weighted expressions. Warning: Use this only on expressions with first order covariant derivatives to avoid GHP derivatives acting on covariant derivatives.";
CovDToNPRules::usage="CovDToNPRules[Dyad] converts the covariant derivatives in expr to NP operators.";
CommuteGHPOp::usage="CommuteGHPOp[\[CapitalTheta]Dyad[{1,-NP}],\[CapitalTheta]Dyad[{2,-NP}]] gives a rule that commutes the \[CapitalTheta]Dyad[{1,-NP}] and \[CapitalTheta]Dyad[{2,-NP}] operators. CommuteGHPOp[\[CapitalTheta]Dyad[{1,-NP}],\[CapitalTheta]Dyad[{2,-NP}],s] gives the rule acting only on s.";
CommuteNPOp::usage="CommuteNPOp[PDNP[{1,-NP}],PDNP[{2,-NP}]] gives a rule that commutes the PDNP[{1,-NP}] and PDNP[{2,-NP}] operators. CommuteNPOp[PDNP[{1,-NP}],PDNP[{2,-NP}], s] gives the rule acting only on s.";
SetTexComponents::usage="SetTexComponents[Dyad,T] sets the Tex output of the component of the spinor T in Dyad to the style T_{ij'} where i is the number of iota contracted on the abstract spinor and j the number of Dagger@iota. This is only done if the spinor is completely symmetric. It is also an option for DyadExpansionEq.";
FormatComponents::usage="FormatComponents[Dyad,T] sets the output of the component of the spinor T in Dyad to the style T_{ij'} where i is the number of iota contracted on the abstract spinor and j the number of Dagger@iota. This is only done if the spinor is completely symmetric. It is also an option for DyadExpansionEq.";
SortByGHPOrder::usage="SortByGHPOrder[expr] sorts the terms in expr according to the order of the GHP operators. The result is a list like structure which can be used for Tex output, but not additive manupulations.";
GHPCommutatorsForward::usage="GHPCommutatorsForward[Dyad] gives a list of commutator rules for the GHP operators.";
GHPCommutatorsBackward::usage="GHPCommutatorsBackward[Dyad] gives a list of commutator rules for the GHP operators.";
NPCommutatorsForward::usage="NPCommutatorsForward[Dyad] gives a list of commutator rules for the NP operators.";
NPCommutatorsBackward::usage="NPCommutatorsBackward[Dyad] gives a list of commutator rules for the NP operators.";
CovDsToGHP::usage="CovDsToGHP[expr, Dyad] converts the covariant derivatives in expr to GHP operators. All internal expressions are expected to already be expanded in the corresponding dyad. Currently only first order covariant derivaties are supported.";
CovDsToNP::usage="CovDsToNP[expr, Dyad] converts the covariant derivatives in expr to NP operators. All internal expressions are expected to already be expanded in the corresponding dyad. Currently only first order covariant derivaties are supported.";
GHPRicciRules::usage="GHPRicciRules[Dyad] gives a the GHP Ricci equations in the form of a list of replacement rules.";
NPRicciRules::usage="NPRicciRules[Dyad] gives a the NP Ricci equations in the form of a list of replacement rules.";
NPBianchiRules::usage="NPBianchiRules[Dyad] gives a the NP Bianchi equations in the form of a list of replacement rules.";
GHPBianchiRules::usage="GHPBianchiRules[Dyad] gives a the GHP Bianchi equations in the form of a list of replacement rules.";
DyadExpansionEq::usage="DyadExpansionEq[T, Dyad] gives an equation for the tensor T[inds] expanded in the dyad Dyad. The GHPWeightOf the componets are set and SetTexComponents is called.";
ToGHP::usage="ToGHP[expr,Dyad] tries to expand expr in the dyad and transform all fundamental spinor operators and covariant derivatives into GHP operators. GHPComponentRules is used recursively.";
ToNP::usage="ToNP[expr,Dyad] tries to expand expr in the dyad and transform all covariant derivatives into NP operators. NPComponentRules is used recursively.";
DyadComponents::usage="DyadComponents[expr,Dyad] gives a list of independent dyad components of expr.";
$GHPExtraRules::usage="$GHPExtraRules is a user defined list of extra rules used for the conversion of an expression into GHP formalism.";
$NPExtraRules::usage="$NPExtraRules is a user defined list of extra rules used for the conversion of an expression into NP formalism.";
GHPComponentRules::usage="GHPComponentRules[T,Dyad] gives a list of component values for the spinor T. If T consists of a fundamental spinor operator or CovarD object, this is expressed in terms of GHP derivative operators. The user can also set GHPComponentRules[T,Dyad] to a suitable list of component rules.";
NPComponentRules::usage="NPComponentRules[T,Dyad] gives a list of component values for the spinor T. If T consists of a CovarD object, this is expressed in terms of NP derivative operators. The user can also set NPComponentRules[T,Dyad] to a suitable list of component rules.";
NPToGHPRules::usage="NPToGHPRules[Dyad] contains a list of rules that converts the NP differential operators to GHP operators. Waring: This can only be used on first order expressions because the conversion is done from the outside in and the GHP weight of the NP operators are not correct.";
GHPToNPRules::usage="NPToGHPRules[Dyad] contains a list of rules that converts the NP differential operators to GHP operators.";
InertScalarOp::usage="InertScalarOp[x,F[x]][expr] gives an inert form of F[expr] which can be printed using TexPrint. To expand it, use InsertScalarOp \[Rule] Function";
SymbolFormattingFunction::usage="SymbolFormattingFunction is an option for DefNPTetrad and DefSpinDyad giving a function that transforms the formatting of the symbols.";
SymbolTexFunction::usage="SymbolTexFunction is an option for DefNPTetrad and DefSpinDyad giving a function that transforms the Tex formatting of the symbols.";
DyadComponentByNumber::usage="DyadComponentByNumber[T,{i,j}, Dyad] gives the component with i iotas and j iota' in the symmetric spinor T. Observe that we assume that SlotsOfTensor only have down spinor indices.";
$DyadCalcInfo::usage="$DyadCalcInfo=False turns off messages from Dyad calculations.";


Begin["`Private`"]


$ContextPath


PrintDaggerAsBar[T_?xTensorQ]:=With[{dgT=Dagger[T]},PrintAs[dgT]^=Overline@PrintAs@T]


AutocanonicalizeComponents[T_?xTensorQ[inds___]]:=Module[{comps=Flatten[ComponentArray[T[inds]]],canonicalrules,unchanged},
canonicalrules=Rule[#,ToCanonical@#]&/@comps;
unchanged=SameQ@@@canonicalrules;
Pick[canonicalrules,Not/@unchanged]/.Rule->(TagSet[T,#1,#2]&);];


IndependentComponents[T_?xTensorQ[inds___]]:=Module[{comps=Flatten[ComponentArray[T[inds]]],canonicalrules},
canonicalrules=Rule[#,ToCanonical@#]&/@comps;
First/@Pick[canonicalrules,SameQ@@@canonicalrules]]


RemoveSignFromEq[-x_==y_]:=x==-y;
RemoveSignFromEq[x_==y_]:=x==y;
RemoveSignFromEq[-x_->y_]:=x->-y;
RemoveSignFromEq[x_->y_]:=x->y;
RemoveSignFromEq[x_]:=x;


MakeSimpleRule[{LHS_,RHS_,conditions___},options___]:=With[{HeldLHS=Hold[LHS],HeldRHS=Hold[RHS]},
Module[{
(* Unblocked indices *)
indsLHS=List@@xAct`xTensor`Private`TakeEIndices[FindIndices[HeldLHS]],
freesLHS=List@@xAct`xTensor`Private`TakeEIndices[FindFreeIndices[HeldLHS]],
dirinds=List@@Select[FindIndices[HeldLHS],DIndexQ],
verb,
indsLHSinfo,
rulesLHS,
(*newHeldLHS=xAct`xTensor`Private`ReplaceIsolated[HeldLHS],
newHeldRHS=ReplaceDummies[HeldRHS],*)
rules,conds},

(* Options *)
{verb}={Verbose}/.CheckOptions[options]/.Options[MakeRule];

(**** B. Construct indsLHSinfo ****)
(* 0. Convert LHS inds to names *)
indsLHS=xAct`xTensor`Private`IndexName/@indsLHS;
freesLHS=xAct`xTensor`Private`IndexName/@freesLHS;

(* 1. Name of index in input rule *)
indsLHSinfo=Partition[indsLHS,1];

(* 2. Free character *)
indsLHSinfo=Append[#,MemberQ[freesLHS,First@#]]&/@indsLHSinfo;

(* 3. MetricOn. up/down character is corrected *)
indsLHSinfo=Append[#,MetricEndowedQ@VBundleOfIndex@First@#]&/@indsLHSinfo;

(* 4. TestIndices *)
indsLHSinfo=Append[#,True]&/@indsLHSinfo;

(* 5. Pattern character in final rule. up/down character in patindsLHS is corrected *)
indsLHSinfo=Append[#,True]&/@indsLHSinfo;

(* 6. Name in final rule. Currently we do not change names *)
indsLHSinfo=Append[#,First@#]&/@indsLHSinfo;
If[verb,Print["indsLHSinfo: ",indsLHSinfo]];
If[verb,Print["dirinds:", dirinds]];
(* 7. Final rules for LHS indices *)
rulesLHS=Join[(xAct`xTensor`Private`FormatIndexRule/@indsLHSinfo),FormatDirRule/@dirinds]/.{xAct`xTensor`Private`patternTest->PatternTest,xAct`xTensor`Private`pattern->Pattern};
If[verb,Print["rulesLHS: ",rulesLHS]];

(**** C. Construct rules ****)

(* 3. Construct rules *)
rules={Rule[HeldLHS,HeldRHS]};
If[verb,Print["rules: ",InputForm[rules]]];

(* 4. Replace indices *)
rules=rules/.Rule[lhs_Hold,rhs_Hold]:>Rule[ReplaceIndex[lhs,rulesLHS],rhs];
If[verb,Print["rules after pattern: ",rules]];

(* 5. Add Module on the RHS *)
rules=rules/.Rule->IndexRule;

(* 6. Add conditions *)
conds=Hold[conditions];
If[verb,Print["Conditions: ",conds]];
rules=(xAct`xTensor`Private`addconditions[#,conds]&/@rules)/.Hold[cond_]->cond;

(* 7. Return rules *)
ScreenDollarIndices@rules
]
]


DyadComponentByNumber[T_?xTensorQ,{i_,j_},dyad_]:=Module[{k=Length@Cases[UpIndex /@ SlotsOfTensor[T], VBundleOfBasis[dyad]],l=Length@Cases[UpIndex /@ SlotsOfTensor[T], Dagger@VBundleOfBasis[dyad]]},PlaceIndicesInTensor[T,Join[Table[{0,-dyad},k-i],Table[{1,-dyad},i],Table[{0,-Dagger@dyad},l-j],Table[{1,-Dagger@dyad},j]]]]/;CompatibleSymQ[SlotsOfTensor[T],SymmetryGroupOfTensor@T,{VBundleOfBasis[dyad],Dagger@VBundleOfBasis[dyad]}]


TetradToBasisRules[_]={};
BasisToTetradRules[_]={};


Options[DefNPTetrad]={SymbolFormattingFunction->Identity,SymbolTexFunction->Identity,BasisColor->RGBColor[0,0,1]};


DefNPTetrad[tetrad_,{lnp_[a_],nnp_[a_],mnp_[a_]},options:OptionsPattern[]]:=Module[{tangentvb=VBundleOfIndex@a,mfd=BaseOfVBundle@VBundleOfIndex@a, metric=First@MetricsOfVBundle@VBundleOfIndex@a,mnpdg,pdnp,symbform=OptionValue[SymbolFormattingFunction],texform=OptionValue[SymbolTexFunction]},
If[Not@xTensorQ[lnp],DefTensor[lnp[a],mfd,PrintAs->symbform@"l"]];
If[Not@xTensorQ[nnp],DefTensor[nnp[a],mfd,PrintAs->symbform@"n"]];
If[Not@xTensorQ[mnp],DefTensor[mnp[a],mfd,Dagger->Complex,PrintAs->symbform@"m"]];
mnpdg=Dagger@mnp;
PrintDaggerAsBar@mnp;
DefBasis[tetrad,tangentvb,{1,2,3,4},BasisColor->OptionValue[BasisColor]];
SetDaggerMatrix[tetrad,{{1,0,0,0},{0,1,0,0},{0,0,0,1},{0,0,1,0}}];
(* Formatting. Observe that we only only format the PDNP derivative, not the CDe derivative. *)
pdnp=PDOfBasis@tetrad;
VectorsOfTetrad[tetrad]^={lnp,nnp,mnp,mnpdg};
xTensorFormStop[];
FormatBasis[pdnp[{1,-tetrad}],symbform@"D"];
FormatBasis[pdnp[{2,-tetrad}],symbform@"\[CapitalDelta]"];
FormatBasis[pdnp[{3,-tetrad}],symbform@"\[Delta]"];
FormatBasis[pdnp[{4,-tetrad}],symbform@"\!\(\*OverscriptBox[\(\[Delta]\), \(_\)]\)"];
xTensorFormStart[];
FormatTexBasis[pdnp[{1,-tetrad}],StringJoin[texform@"D"," "]];
FormatTexBasis[pdnp[{2,-tetrad}],StringJoin[texform@"\\Delta"," "]];
FormatTexBasis[pdnp[{3,-tetrad}],StringJoin[texform@"\\delta"," "]];
FormatTexBasis[pdnp[{4,-tetrad}],StringJoin[texform@"\\bar\\delta"," "]];
(* Transformation between Basis and Tetrad *)
SetToRule@metric;
MetricInBasis[metric,tetrad,{{0,1,0,0},{1,0,0,0},{0,0,0,-1},{0,0,-1,0}}];
MetricInBasis[metric,-tetrad,{{0,1,0,0},{1,0,0,0},{0,0,0,-1},{0,0,-1,0}}];
RuleToSet@metric;
SetToRule/@{lnp,nnp,mnp,mnpdg};
AllComponentValues[lnp[{a, tetrad}] , Basis[{1, -tetrad}, {a, tetrad}]];
AllComponentValues[nnp[{a, tetrad}] , Basis[{2, -tetrad}, {a, tetrad}]];
AllComponentValues[mnp[{a, tetrad}] , Basis[{3, -tetrad}, {a, tetrad}]];
AllComponentValues[mnpdg[{a, tetrad}] , Basis[{4, -tetrad}, {a, tetrad}]];
AllComponentValues[lnp[{-a, -tetrad}] ,{0,1,0,0}];
AllComponentValues[nnp[{-a, -tetrad}] ,{1,0,0,0}];
AllComponentValues[mnp[{-a, -tetrad}] ,{0,0,0,-1}];
AllComponentValues[mnpdg[{-a, -tetrad}] ,{0,0,-1,0}];
RuleToSet/@{lnp,nnp,mnp,mnpdg};
TetradToBasisRules[tetrad]^=Flatten[MakeRule[Evaluate[#],UseSymmetries->False,MetricOn->None,ContractMetrics->False]&/@{{lnp[a],Basis[{1,-tetrad},a]},{nnp[a],Basis[{2,-tetrad},a]},{mnp[a],Basis[{3,-tetrad},a]},{mnpdg[a],Basis[{4,-tetrad},a]},{lnp[-a],Basis[-a,{2,tetrad}]},{nnp[-a],Basis[-a,{1,tetrad}]},{mnp[-a],-Basis[-a,{4,tetrad}]},{mnpdg[-a],-Basis[-a,{3,tetrad}]}}];
BasisToTetradRules[tetrad]^=Flatten[MakeRule[Evaluate[#],UseSymmetries->False,MetricOn->None,ContractMetrics->False]&/@{{Basis[{1,-tetrad},a],lnp[a]},{Basis[{2,-tetrad},a],nnp[a]},{Basis[{3,-tetrad},a],mnp[a]},{Basis[{4,-tetrad},a],mnpdg[a]},{Basis[-a,{1,tetrad}],nnp[-a]},{Basis[-a,{2,tetrad}],lnp[-a]},{Basis[-a,{3,tetrad}],-mnpdg[-a]},{Basis[-a,{4,tetrad}],-mnp[-a]}}];
];


DyadToBasisRules[_]={};
BasisToDyadRules[_]={};
TetradToDyadRules[_]={};
CovDDyadRules[_]={};
NPToGHPSpinCoeffs[_]={};
GHPToNPSpinCoeffs[_]={};
CovDNPSpinCoeffRules[_]={};
TetradSigmaRules[_]={};


NPSpinCoefficients[dyad_]:=SymbolJoin[#,dyad][]&/@{"\[Alpha]","\[Beta]","\[Gamma]","\[Epsilon]","\[Kappa]","\[Lambda]","\[Mu]","\[Nu]","\[Pi]","\[Rho]","\[Sigma]","\[Tau]"};


GHPSpinCoefficients[dyad_]:=SymbolJoin[#,dyad][]&/@{"\[Kappa]","\[Rho]","\[Sigma]","\[Tau]","\[Kappa]p","\[Rho]p","\[Sigma]p","\[Tau]p"};


Options[DefSpinDyad]={SymbolFormattingFunction->Identity,SymbolTexFunction->Identity,BasisColor->RGBColor[1,0,0]};


DefSpinDyad[dyad_,{omicron_[Ai_], iota_[Ai_]},tetrad_,options:OptionsPattern[]]:=Module[{spin=VBundleOfIndex@Ai, mfd=BaseOfVBundle@VBundleOfIndex@Ai, A,B, C, D,Adg, Bdg,dyaddg, eps=First@MetricsOfVBundle@VBundleOfIndex@Ai,epsdg,omicrondg, iotadg, sigma=SolderingFormOfVBundle@VBundleOfIndex@Ai,a,b,c,lnp,nnp,mnp,mnpdg,spincoeffnames,tmprules,cde=CovDOfMetric@First@MetricsOfVBundle@VBundleOfIndex@Ai,pddyad,pdnp,achristoffelcdepddyad,christoffelcdepdnp,tetradtodyadeqs,ExtraGHPspincoeffnames,tmpeqs,symbform=OptionValue[SymbolFormattingFunction],texform=OptionValue[SymbolTexFunction]},
epsdg=Dagger@eps;
{A,B, C, D}=Take[First@IndicesOfVBundle@spin,4];
{a,b,c}=Take[First@IndicesOfVBundle@VBundleOfBasis@tetrad,3];
Adg=DaggerIndex@A;
Bdg=DaggerIndex@B;
(* Spin Dyad *)
If[Not@xTensorQ[omicron],DefSpinor[omicron[A],mfd]];
If[Not@xTensorQ[iota],DefSpinor[iota[A],mfd]];
PrintAs[omicron]^=symbform["o"];
PrintAs[iota]^=symbform["\[Iota]"];
PrintDaggerAsBar/@{omicron,iota};
omicrondg=Dagger@omicron;
iotadg=Dagger@iota;
DefBasis[dyad,spin,{0,1},BasisColor->OptionValue[BasisColor],Dagger->Complex];
SpinorsOfDyad[dyad]^={omicron,iota};
dyaddg=Dagger@dyad;
Evaluate[dyad]/:CIndexForm[i_Integer,dyad]:=symbform@ToString[i];
Evaluate[dyaddg]/:CIndexForm[i_Integer,dyaddg]:=StringJoin[symbform@ToString[i],"'"];
SetToRule/@{eps,epsdg};
MetricInBasis[eps,dyad,{{0,1},{-1,0}}];
MetricInBasis[eps,-dyad,{{0,1},{-1,0}}];
MetricInBasis[epsdg,dyaddg,{{0,1},{-1,0}}];
MetricInBasis[epsdg,-dyaddg,{{0,1},{-1,0}}];
RuleToSet/@{eps,epsdg};
SetToRule/@{omicron, iota, omicrondg,iotadg};
AllComponentValues[omicrondg[{Adg,dyaddg}],{1,0}];
AllComponentValues[iotadg[{Adg,dyaddg}],{0,1}];
AllComponentValues[omicron[{A,dyad}],{1,0}];
AllComponentValues[iota[{A,dyad}],{0,1}];
AllComponentValues[omicrondg[{-Adg,-dyaddg}],{0,1}];
AllComponentValues[iotadg[{-Adg,-dyaddg}],{-1,0}];
AllComponentValues[omicron[{-A,-dyad}],{0,1}];
AllComponentValues[iota[{-A,-dyad}],{-1,0}];
RuleToSet/@{omicron, iota, omicrondg,iotadg};
AutomaticRules[omicron,MakeRule[{omicron[-A]*iota[A] , 1},MetricOn->All,ContractMetrics->True]];
AutomaticRules[Evaluate[omicrondg],MakeRule[{Evaluate[omicrondg[-Adg]*iotadg[Adg]] , 1},MetricOn->All,ContractMetrics->True]];
If[Evaluate[omicron[-A]*omicron[A]]=!=0,AutomaticRules[omicron,MakeRule[{omicron[-A]*omicron[A] , 0},MetricOn->None,ContractMetrics->False,UseSymmetries->False]]];
If[Evaluate[iota[-A]*iota[A]]=!=0,AutomaticRules[iota,MakeRule[{iota[-A]*iota[A] , 0},MetricOn->None,ContractMetrics->False,UseSymmetries->False]]];
If[Evaluate[omicrondg[-Adg]*omicrondg[Adg]]=!=0,AutomaticRules[Evaluate[omicrondg],MakeRule[{Evaluate[omicrondg[-Adg]*omicrondg[Adg]] , 0},MetricOn->None,ContractMetrics->False,UseSymmetries->False]]];
If[Evaluate[iotadg[-Adg]*iotadg[Adg]]=!=0,AutomaticRules[Evaluate[iotadg],MakeRule[{Evaluate[iotadg[-Adg]*iotadg[Adg]] , 0},MetricOn->None,ContractMetrics->False,UseSymmetries->False]]];
DyadToBasisRules[dyad]^=Flatten[MakeRule[#,UseSymmetries->False,MetricOn->None]&/@{{omicron[A],Basis[{0,-dyad},A]},{iota[A],Basis[{1,-dyad},A]},{omicrondg[Adg],Basis[{0,-dyaddg},Adg]},{iotadg[Adg],Basis[{1,-dyaddg},Adg]},{omicron[-A],Basis[-A,{1,dyad}]},{iota[-A],-Basis[-A,{0,dyad}]},{omicrondg[-Adg],Basis[-Adg,{1,dyaddg}]},{iotadg[-Adg],-Basis[-Adg,{0,dyaddg}]}}];
BasisToDyadRules[dyad]^=Flatten[MakeRule[#,UseSymmetries->False,MetricOn->None]&/@{{Basis[{0,-dyad},A],omicron[A]},{Basis[{1,-dyad},A],iota[A]},{Basis[{0,-dyaddg},Adg],omicrondg[Adg]},{Basis[{1,-dyaddg},Adg],iotadg[Adg]},{Basis[-A,{1,dyad}],omicron[-A]},{Basis[-A,{0,dyad}],-iota[-A]},{Basis[-Adg,{1,dyaddg}],omicrondg[-Adg]},{Basis[-Adg,{0,dyaddg}],-iotadg[-Adg]}}];
(* Relation between tetrad and dyad *)
SetToRule@sigma;
{lnp,nnp,mnp,mnpdg}=VectorsOfTetrad[tetrad];
{sigma[{1,tetrad},{0,-dyad},{0,-dyaddg}]==1,sigma[{2,tetrad},{0,-dyad},{0,-dyaddg}]==0,sigma[{3,tetrad},{0,-dyad},{0,-dyaddg}]==0,sigma[{4,tetrad},{0,-dyad},{0,-dyaddg}]==0,sigma[{1,tetrad},{1,-dyad},{1,-dyaddg}]==0,sigma[{2,tetrad},{1,-dyad},{1,-dyaddg}]==1,sigma[{3,tetrad},{1,-dyad},{1,-dyaddg}]==0,sigma[{4,tetrad},{1,-dyad},{1,-dyaddg}]==0,sigma[{1,tetrad},{0,-dyad},{1,-dyaddg}]==0,sigma[{2,tetrad},{0,-dyad},{1,-dyaddg}]==0,sigma[{3,tetrad},{0,-dyad},{1,-dyaddg}]==1,sigma[{4,tetrad},{0,-dyad},{1,-dyaddg}]==0,sigma[{1,tetrad},{1,-dyad},{0,-dyaddg}]==0,sigma[{2,tetrad},{1,-dyad},{0,-dyaddg}]==0,sigma[{3,tetrad},{1,-dyad},{0,-dyaddg}]==0,sigma[{4,tetrad},{1,-dyad},{0,-dyaddg}]==1,sigma[{1,-tetrad},{0,dyad},{0,dyaddg}]==1,sigma[{1,-tetrad},{0,dyad},{1,dyaddg}]==0,sigma[{1,-tetrad},{1,dyad},{0,dyaddg}]==0,sigma[{1,-tetrad},{1,dyad},{1,dyaddg}]==0,sigma[{2,-tetrad},{0,dyad},{0,dyaddg}]==0,sigma[{2,-tetrad},{0,dyad},{1,dyaddg}]==0,sigma[{2,-tetrad},{1,dyad},{0,dyaddg}]==0,sigma[{2,-tetrad},{1,dyad},{1,dyaddg}]==1,sigma[{3,-tetrad},{0,dyad},{0,dyaddg}]==0,sigma[{3,-tetrad},{0,dyad},{1,dyaddg}]==1,sigma[{3,-tetrad},{1,dyad},{0,dyaddg}]==0,sigma[{3,-tetrad},{1,dyad},{1,dyaddg}]==0,sigma[{4,-tetrad},{0,dyad},{0,dyaddg}]==0,sigma[{4,-tetrad},{0,dyad},{1,dyaddg}]==0,sigma[{4,-tetrad},{1,dyad},{0,dyaddg}]==1,sigma[{4,-tetrad},{1,dyad},{1,dyaddg}]==0}/.Equal->ComponentValue;
RuleToSet@sigma;
tetradtodyadeqs={lnp[a]==omicron[A]*omicrondg[Adg]*sigma[a,-A,-Adg],nnp[a]==iota[A]*iotadg[Adg]*sigma[a,-A,-Adg],mnp[a]==iotadg[Adg]*omicron[A]*sigma[a,-A,-Adg],mnpdg[a]==iota[A]*omicrondg[Adg]*sigma[a,-A,-Adg]};
TetradToDyadRules[tetrad]^=Flatten[MakeRule[#,MetricOn->All,ContractMetrics->True]&/@List@@@tetradtodyadeqs];
(* TetradSigmaRules *)
tmpeqs={sigma[{1,tetrad},-A,-Adg]==iota[-A] Dagger[iota][-Adg],sigma[{2,tetrad},-A,-Adg]==omicron[-A] Dagger[omicron][-Adg],sigma[{3,tetrad},-A,-Adg]==-Dagger[omicron][-Adg] iota[-A],sigma[{4,tetrad},-A,-Adg]==-omicron[-A] Dagger[iota][-Adg]};
TetradSigmaRules[tetrad]^=Flatten[MakeRule[#,MetricOn->All,ContractMetrics->True]&/@List@@@tmpeqs];
(* Spin coefficients and Christoffel symbols dyad *)
pddyad=PDOfBasis@dyad;
spincoeffnames=Head/@NPSpinCoefficients[dyad];
ExtraGHPspincoeffnames=Head/@Take[GHPSpinCoefficients[dyad],-4];
(DefSpinor[SymbolJoin[#,dyad][],mfd,PrintAs->symbform@#];Tex[SymbolJoin[#,dyad]]^=texform@Tex[#];
Tex[Dagger@SymbolJoin[#,dyad]]^=TexBar@texform@Tex[#];)&/@{"\[Alpha]","\[Beta]","\[Gamma]","\[Epsilon]","\[Kappa]","\[Lambda]","\[Mu]","\[Nu]","\[Pi]","\[Rho]","\[Sigma]","\[Tau]"};
DefSpinor[SymbolJoin["\[Tau]p",dyad][],mfd,PrintAs->symbform@"\[Tau]"<>"'"];
DefSpinor[SymbolJoin["\[Sigma]p",dyad][],mfd,PrintAs->symbform@"\[Sigma]"<>"'"];
DefSpinor[SymbolJoin["\[Rho]p",dyad][],mfd,PrintAs->symbform@"\[Rho]"<>"'"];
DefSpinor[SymbolJoin["\[Kappa]p",dyad][],mfd,PrintAs->symbform@"\[Kappa]"<>"'"];
Tex[Evaluate@SymbolJoin["\[Tau]p",dyad]]^=(texform@Tex@"\[Tau]")<>"'";
Tex[Evaluate@SymbolJoin["\[Sigma]p",dyad]]^=(texform@Tex@"\[Sigma]")<>"'";
Tex[Evaluate@SymbolJoin["\[Rho]p",dyad]]^=(texform@Tex@"\[Rho]")<>"'";
Tex[Evaluate@SymbolJoin["\[Kappa]p",dyad]]^=(texform@Tex@"\[Kappa]")<>"'";
PrintAs[Evaluate@Dagger@SymbolJoin["\[Tau]p",dyad]]^=Overline[symbform@"\[Tau]"]<>"'";
PrintAs[Evaluate@Dagger@SymbolJoin["\[Sigma]p",dyad]]^=Overline[symbform@"\[Sigma]"]<>"'";
PrintAs[Evaluate@Dagger@SymbolJoin["\[Rho]p",dyad]]^=Overline[symbform@"\[Rho]"]<>"'";
PrintAs[Evaluate@Dagger@SymbolJoin["\[Kappa]p",dyad]]^=Overline[symbform@"\[Kappa]"]<>"'";
Tex[Evaluate@Dagger@SymbolJoin["\[Tau]p",dyad]]^=TexBar[texform@Tex@"\[Tau]"]<>"'";
Tex[Evaluate@Dagger@SymbolJoin["\[Sigma]p",dyad]]^=TexBar[texform@Tex@"\[Sigma]"]<>"'";
Tex[Evaluate@Dagger@SymbolJoin["\[Rho]p",dyad]]^=TexBar[texform@Tex@"\[Rho]"]<>"'";
Tex[Evaluate@Dagger@SymbolJoin["\[Kappa]p",dyad]]^=TexBar[texform@Tex@"\[Kappa]"]<>"'";
PrintDaggerAsBar/@spincoeffnames;
FormatComponents[dyad,SS_?xTensorQ]^:=FormatComponents[dyad,SS,symbform];
SetTexComponents[dyad,SS_?xTensorQ]^:=SetTexComponents[dyad,SS,texform];
(* GHP weights of the spin coefficients *)
(GHPWeightOf[Evaluate[#[[1]]]]^=#[[2]])&/@Thread[{NPSpinCoefficients[dyad][[#]]&/@Range[5,12],{{3,1},{-3,1},{-1,-1},{-3,-1},{-1,1},{1,1},{3,-1},{1,-1}}}];
(GHPWeightOf[Evaluate[#[[1]]]]^=#[[2]])&/@Thread[{Take[GHPSpinCoefficients[dyad],-4],{{-3,-1},{-1,-1},{-3,1},{-1,1}}}];
UpSet[GHPWeightOf@#,Reverse@GHPWeightOf@Dagger@#]&/@(Dagger/@Join[NPSpinCoefficients[dyad][[#]]&/@Range[5,12],Take[GHPSpinCoefficients[dyad],-4]]);
(* *)
NPToGHPSpinCoeffs[dyad]^={NPSpinCoefficients[dyad][[6]]->-GHPSpinCoefficients[dyad][[7]],NPSpinCoefficients[dyad][[7]]->-GHPSpinCoefficients[dyad][[6]],NPSpinCoefficients[dyad][[8]]->-GHPSpinCoefficients[dyad][[5]],NPSpinCoefficients[dyad][[9]]->-GHPSpinCoefficients[dyad][[8]],Dagger@NPSpinCoefficients[dyad][[6]]->-Dagger@GHPSpinCoefficients[dyad][[7]],Dagger@NPSpinCoefficients[dyad][[7]]->-Dagger@GHPSpinCoefficients[dyad][[6]],Dagger@NPSpinCoefficients[dyad][[8]]->-Dagger@GHPSpinCoefficients[dyad][[5]],Dagger@NPSpinCoefficients[dyad][[9]]->-Dagger@GHPSpinCoefficients[dyad][[8]]};
GHPToNPSpinCoeffs[dyad]^={GHPSpinCoefficients[dyad][[7]]->-NPSpinCoefficients[dyad][[6]],GHPSpinCoefficients[dyad][[6]]->-NPSpinCoefficients[dyad][[7]],GHPSpinCoefficients[dyad][[5]]->-NPSpinCoefficients[dyad][[8]],GHPSpinCoefficients[dyad][[8]]->-NPSpinCoefficients[dyad][[9]],Dagger@GHPSpinCoefficients[dyad][[7]]->-Dagger@NPSpinCoefficients[dyad][[6]],Dagger@GHPSpinCoefficients[dyad][[6]]->-Dagger@NPSpinCoefficients[dyad][[7]],Dagger@GHPSpinCoefficients[dyad][[5]]->-Dagger@NPSpinCoefficients[dyad][[8]],Dagger@GHPSpinCoefficients[dyad][[8]]->-Dagger@NPSpinCoefficients[dyad][[9]]};
(* *)
achristoffelcdepddyad=Head@AChristoffel[cde,pddyad][{0,dyaddg},{1,-tetrad},{0,-dyaddg}];
tmprules=Evaluate[{AChristoffel[cde,pddyad][{0,dyad},{1,-tetrad},{0,-dyad}]->spincoeffnames[[4]][],
AChristoffel[cde,pddyad][{0,dyad},{1,-tetrad},{1,-dyad}]->spincoeffnames[[9]][],
AChristoffel[cde,pddyad][{0,dyad},{2,-tetrad},{0,-dyad}]->spincoeffnames[[3]][],AChristoffel[cde,pddyad][{0,dyad},{2,-tetrad},{1,-dyad}]->spincoeffnames[[8]][],AChristoffel[cde,pddyad][{0,dyad},{3,-tetrad},{0,-dyad}]->spincoeffnames[[2]][],AChristoffel[cde,pddyad][{0,dyad},{3,-tetrad},{1,-dyad}]->spincoeffnames[[7]][],AChristoffel[cde,pddyad][{0,dyad},{4,-tetrad},{0,-dyad}]->spincoeffnames[[1]][],AChristoffel[cde,pddyad][{0,dyad},{4,-tetrad},{1,-dyad}]->spincoeffnames[[6]][],AChristoffel[cde,pddyad][{1,dyad},{1,-tetrad},{0,-dyad}]->-spincoeffnames[[5]][],
AChristoffel[cde,pddyad][{1,dyad},{1,-tetrad},{1,-dyad}]->-spincoeffnames[[4]][],
AChristoffel[cde,pddyad][{1,dyad},{2,-tetrad},{0,-dyad}]->-spincoeffnames[[12]][],AChristoffel[cde,pddyad][{1,dyad},{2,-tetrad},{1,-dyad}]->-spincoeffnames[[3]][],AChristoffel[cde,pddyad][{1,dyad},{3,-tetrad},{0,-dyad}]->-spincoeffnames[[11]][],AChristoffel[cde,pddyad][{1,dyad},{3,-tetrad},{1,-dyad}]->-spincoeffnames[[2]][],AChristoffel[cde,pddyad][{1,dyad},{4,-tetrad},{0,-dyad}]->-spincoeffnames[[10]][],AChristoffel[cde,pddyad][{1,dyad},{4,-tetrad},{1,-dyad}]->-spincoeffnames[[1]][]}];
Map[Dagger,tmprules,{2}]/.Rule->ComponentValue;
tmprules/.Rule->ComponentValue;
RuleToSet@Evaluate@achristoffelcdepddyad;
RuleToSet@Evaluate@Dagger@achristoffelcdepddyad;
tmprules={cde[-A,-Adg][omicron[-B]]==-(iotadg[-Adg]*omicron[-A]*omicron[-B]*spincoeffnames[[1]][])-iota[-A]*omicron[-B]*omicrondg[-Adg]*spincoeffnames[[2]][]+omicron[-A]*omicron[-B]*omicrondg[-Adg]*spincoeffnames[[3]][]+iota[-A]*iotadg[-Adg]*omicron[-B]*spincoeffnames[[4]][]-iota[-A]*iota[-B]*iotadg[-Adg]*spincoeffnames[[5]][]+iota[-B]*iotadg[-Adg]*omicron[-A]*spincoeffnames[[10]][]+iota[-A]*iota[-B]*omicrondg[-Adg]*spincoeffnames[[11]][]-iota[-B]*omicron[-A]*omicrondg[-Adg]*spincoeffnames[[12]][],cde[-A,-Adg][iota[-B]]==iota[-B]*iotadg[-Adg]*omicron[-A]*spincoeffnames[[1]][]+iota[-A]*iota[-B]*omicrondg[-Adg]*spincoeffnames[[2]][]-iota[-B]*omicron[-A]*omicrondg[-Adg]*spincoeffnames[[3]][]-iota[-A]*iota[-B]*iotadg[-Adg]*spincoeffnames[[4]][]-iotadg[-Adg]*omicron[-A]*omicron[-B]*spincoeffnames[[6]][]-iota[-A]*omicron[-B]*omicrondg[-Adg]*spincoeffnames[[7]][]+omicron[-A]*omicron[-B]*omicrondg[-Adg]*spincoeffnames[[8]][]+iota[-A]*iotadg[-Adg]*omicron[-B]*spincoeffnames[[9]][]};
CovDDyadRules[dyad]^=Flatten[MakeRule[Evaluate[List@@#],MetricOn->All,ContractMetrics->True]&/@Join[tmprules,Map[Dagger,tmprules,{2}]]];
(* Christoffel symbols pdnp *)
pdnp=PDOfBasis@tetrad;
christoffelcdepdnp=Head@Christoffel[cde,pdnp][{1,tetrad},{1,-tetrad},{1,-tetrad}];
Flatten@ComponentArray@Map[TraceBasisDummy[ToBasis[dyaddg]@ToBasis[dyad]@ToBasis[tetrad]@ToBasis[tetrad]@cde[-b][#]/.cde->pdnp]&,tetradtodyadeqs,{2}]/.Equal->ComponentValue;
RuleToSet@christoffelcdepdnp;
(* Torsion *)
AllComponentValues@ToBasis[tetrad]@Torsion[pdnp][a,-b,-c];
IndependentComponents[Torsion[pdnp][{a,tetrad},{-b,-tetrad},{-c,-tetrad}]]/.Torsion[pdnp][{aa_Integer,tetrad},{ab_Integer,-tetrad},{ac_Integer,-tetrad}]:>ComponentValue[Torsion[pdnp][{aa,tetrad},{ab,-tetrad},{ac,-tetrad}],$TorsionSign*(-christoffelcdepdnp[{aa,tetrad},{ab,-tetrad},{ac,-tetrad}]+christoffelcdepdnp[{aa,tetrad},{ac,-tetrad},{ab,-tetrad}])];
RuleToSet[Torsion[pdnp]];
(* Curvature *)
DefSpinor[SymbolJoin["Psi",cde,ToString[#],dyad][],mfd,PrintAs->StringJoin["\!","\(\[CapitalPsi]\_\(",symbform@ToString[#],"\)\)"],PrintDaggerAs->StringJoin["\!","\(\(\[CapitalPsi]\&_\)\_\(",symbform@ToString[#],"\)\)"]]&/@Range[0,4];
(Tex[Evaluate[SymbolJoin["Psi",cde,ToString[#],dyad]]]^="\\Psi_{"<>texform@ToString[#]<>"}")&/@Range[0,4];
(Tex[Evaluate[Dagger@SymbolJoin["Psi",cde,ToString[#],dyad]]]^="\\bar\\Psi_{"<>texform@ToString[#]<>"}")&/@Range[0,4];
DefSpinor[SymbolJoin["Phi",cde,#,dyad][],mfd,PrintAs->StringJoin["\!","\(\[CapitalPhi]\_\(",symbform@StringTake[#,1],symbform@StringDrop[#,1],"\)\)"],PrintDaggerAs->StringJoin["\!","\(\(\[CapitalPhi]\&_\)\_\(",symbform@StringTake[#,1],symbform@StringDrop[#,1],"'\)\)"]]&/@{"00","01","02","10","11","12","20","21","22"};
(Tex[Evaluate[SymbolJoin["Phi",cde,ToString[#],dyad]]]^="\\Phi_{"<>texform@StringTake[#,1]<>texform@StringDrop[#,1]<>"}")&/@{"00","01","02","10","11","12","20","21","22"};
(Tex[Evaluate[Dagger@SymbolJoin["Phi",cde,ToString[#],dyad]]]^="\\bar\\Phi_{"<>texform@StringTake[#,1]<>texform@StringDrop[#,1]<>"}")&/@{"00","01","02","10","11","12","20","21","22"};
{GHPWeightOf@Evaluate@SymbolJoin["Psi",cde,"0",dyad][]^={4,0},GHPWeightOf@Evaluate@SymbolJoin["Psi",cde,"1",dyad][]^={2,0},GHPWeightOf@Evaluate@SymbolJoin["Psi",cde,"2",dyad][]^={0,0},GHPWeightOf@Evaluate@SymbolJoin["Psi",cde,"3",dyad][]^={-2,0},GHPWeightOf@Evaluate@SymbolJoin["Psi",cde,"4",dyad][]^={-4,0},GHPWeightOf@Evaluate@Dagger@SymbolJoin["Psi",cde,"0",dyad][]^={0,4},GHPWeightOf@Evaluate@Dagger@SymbolJoin["Psi",cde,"1",dyad][]^={0,2},GHPWeightOf@Evaluate@Dagger@SymbolJoin["Psi",cde,"2",dyad][]^={0,0},GHPWeightOf@Evaluate@Dagger@SymbolJoin["Psi",cde,"3",dyad][]^={0,-2},GHPWeightOf@Evaluate@Dagger@SymbolJoin["Psi",cde,"4",dyad][]^={0,-4},GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"00",dyad][]^={2,2},
GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"10",dyad][]^={0,2},
GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"01",dyad][]^={2,0},
GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"11",dyad][]^={0,0},
GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"20",dyad][]^={-2,2},GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"21",dyad][]^={-2,0},GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"02",dyad][]^={2,-2},GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"12",dyad][]^={0,-2},GHPWeightOf@Evaluate@SymbolJoin["Phi",cde,"22",dyad][]^={-2,-2}
};
tmprules={Psi[cde][{0,-dyad},{0,-dyad},{0,-dyad},{0,-dyad}]->SymbolJoin["Psi",cde,"0",dyad][],Psi[cde][{0,-dyad},{0,-dyad},{0,-dyad},{1,-dyad}]->SymbolJoin["Psi",cde,"1",dyad][],Psi[cde][{0,-dyad},{0,-dyad},{1,-dyad},{1,-dyad}]->SymbolJoin["Psi",cde,"2",dyad][],Psi[cde][{0,-dyad},{1,-dyad},{1,-dyad},{1,-dyad}]->SymbolJoin["Psi",cde,"3",dyad][],Psi[cde][{1,-dyad},{1,-dyad},{1,-dyad},{1,-dyad}]->SymbolJoin["Psi",cde,"4",dyad][]};
Map[Dagger,tmprules,{2}]/.Rule->(TagSet[Evaluate[Dagger@Psi[cde]],#1,#2]&);
tmprules/.Rule->(TagSet[Evaluate[Psi[cde]],#1,#2]&);
tmprules={Phi[cde][{0,-dyad},{0,-dyad},{0,-dyaddg},{0,-dyaddg}]->SymbolJoin["Phi",cde,"00",dyad][],Phi[cde][{0,-dyad},{0,-dyad},{0,-dyaddg},{1,-dyaddg}]->SymbolJoin["Phi",cde,"01",dyad][],Phi[cde][{0,-dyad},{0,-dyad},{1,-dyaddg},{1,-dyaddg}]->SymbolJoin["Phi",cde,"02",dyad][],Phi[cde][{0,-dyad},{1,-dyad},{0,-dyaddg},{0,-dyaddg}]->SymbolJoin["Phi",cde,"10",dyad][],Phi[cde][{1,-dyad},{1,-dyad},{0,-dyaddg},{0,-dyaddg}]->SymbolJoin["Phi",cde,"20",dyad][],Phi[cde][{0,-dyad},{1,-dyad},{0,-dyaddg},{1,-dyaddg}]->SymbolJoin["Phi",cde,"11",dyad][],Phi[cde][{1,-dyad},{1,-dyad},{0,-dyaddg},{1,-dyaddg}]->SymbolJoin["Phi",cde,"21",dyad][],Phi[cde][{0,-dyad},{1,-dyad},{1,-dyaddg},{1,-dyaddg}]->SymbolJoin["Phi",cde,"12",dyad][],Phi[cde][{1,-dyad},{1,-dyad},{1,-dyaddg},{1,-dyaddg}]->SymbolJoin["Phi",cde,"22",dyad][]};
tmprules/.Rule->(TagSet[Evaluate[Phi[cde]],#1,#2]&);
tmprules={SymbolJoin["Phi",cde,"00",dyaddg][]->SymbolJoin["Phi",cde,"00",dyad][],SymbolJoin["Phi",cde,"01",dyaddg][]->SymbolJoin["Phi",cde,"10",dyad][],SymbolJoin["Phi",cde,"02",dyaddg][]->SymbolJoin["Phi",cde,"20",dyad][],SymbolJoin["Phi",cde,"10",dyaddg][]->SymbolJoin["Phi",cde,"01",dyad][],SymbolJoin["Phi",cde,"11",dyaddg][]->SymbolJoin["Phi",cde,"11",dyad][],SymbolJoin["Phi",cde,"12",dyaddg][]->SymbolJoin["Phi",cde,"21",dyad][],SymbolJoin["Phi",cde,"20",dyaddg][]->SymbolJoin["Phi",cde,"02",dyad][],SymbolJoin["Phi",cde,"21",dyaddg][]->SymbolJoin["Phi",cde,"12",dyad][],SymbolJoin["Phi",cde,"22",dyaddg][]->SymbolJoin["Phi",cde,"22",dyad][]};
tmprules/.Rule->Set;
(* NP operators *)
DefNPOperatorTools[dyad,tetrad,cde,{a,A}];
(* GHP operators *)
DefGHPOperator[dyad,tetrad,cde,{a,A},symbform,texform];
(* Derivatives of the NP but not GHP spin coefficients (i.e. a subset of the NP Ricci) *)
tmprules={cde[A,Adg][NPSpinCoefficients[dyad][[1]]]==iota[A]*iotadg[Adg]*pdnp[{1,-tetrad}][NPSpinCoefficients[dyad][[1]]]+omicron[A]*omicrondg[Adg]*pdnp[{2,-tetrad}][NPSpinCoefficients[dyad][[1]]]-omicrondg[Adg]*iota[A]*pdnp[{3,-tetrad}][NPSpinCoefficients[dyad][[1]]]-omicron[A]*iotadg[Adg]*pdnp[{4,-tetrad}][NPSpinCoefficients[dyad][[1]]],cde[A,Adg][NPSpinCoefficients[dyad][[2]]]==-$LambdaSign*$RiemannSign*Lambda[cde][]*omicron[A]*iotadg[Adg]-omicron[A]*$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"11",dyad][]*iotadg[Adg]+omicron[A]*$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"2",dyad][]*iotadg[Adg]+omicron[A]*NPSpinCoefficients[dyad][[1]]*Dagger@NPSpinCoefficients[dyad][[1]]*iotadg[Adg]-2*omicron[A]*NPSpinCoefficients[dyad][[1]]*NPSpinCoefficients[dyad][[2]]*iotadg[Adg]+omicron[A]*NPSpinCoefficients[dyad][[2]]*Dagger@NPSpinCoefficients[dyad][[2]]*iotadg[Adg]+omicron[A]*NPSpinCoefficients[dyad][[4]]*iotadg[Adg]*NPSpinCoefficients[dyad][[7]]-omicron[A]*NPSpinCoefficients[dyad][[4]]*iotadg[Adg]*Dagger@NPSpinCoefficients[dyad][[7]]+omicron[A]*NPSpinCoefficients[dyad][[3]]*iotadg[Adg]*NPSpinCoefficients[dyad][[10]]+omicron[A]*iotadg[Adg]*NPSpinCoefficients[dyad][[7]]*NPSpinCoefficients[dyad][[10]]-omicron[A]*NPSpinCoefficients[dyad][[3]]*iotadg[Adg]*Dagger@NPSpinCoefficients[dyad][[10]]-omicron[A]*iotadg[Adg]*NPSpinCoefficients[dyad][[6]]*NPSpinCoefficients[dyad][[11]]+iota[A]*iotadg[Adg]*pdnp[{1,-tetrad}][NPSpinCoefficients[dyad][[2]]]+omicron[A]*omicrondg[Adg]*pdnp[{2,-tetrad}][NPSpinCoefficients[dyad][[2]]]-omicron[A]*iotadg[Adg]*pdnp[{3,-tetrad}][NPSpinCoefficients[dyad][[1]]]-omicrondg[Adg]*iota[A]*pdnp[{3,-tetrad}][NPSpinCoefficients[dyad][[2]]],cde[A,Adg][NPSpinCoefficients[dyad][[3]]]==omicrondg[Adg]*$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"12",dyad][]*iota[A]+omicrondg[Adg]*Dagger@NPSpinCoefficients[dyad][[1]]*NPSpinCoefficients[dyad][[3]]*iota[A]+2*omicrondg[Adg]*NPSpinCoefficients[dyad][[2]]*NPSpinCoefficients[dyad][[3]]*iota[A]-omicrondg[Adg]*NPSpinCoefficients[dyad][[2]]*Dagger@NPSpinCoefficients[dyad][[3]]*iota[A]+omicron[A]*$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"3",dyad][]*iotadg[Adg]+omicron[A]*Dagger@NPSpinCoefficients[dyad][[2]]*NPSpinCoefficients[dyad][[3]]*iotadg[Adg]+omicron[A]*NPSpinCoefficients[dyad][[1]]*Dagger@NPSpinCoefficients[dyad][[3]]*iotadg[Adg]-omicron[A]*NPSpinCoefficients[dyad][[2]]*iotadg[Adg]*NPSpinCoefficients[dyad][[6]]-omicrondg[Adg]*NPSpinCoefficients[dyad][[1]]*iota[A]*Dagger@NPSpinCoefficients[dyad][[6]]-omicrondg[Adg]*NPSpinCoefficients[dyad][[2]]*iota[A]*NPSpinCoefficients[dyad][[7]]-omicron[A]*NPSpinCoefficients[dyad][[1]]*iotadg[Adg]*Dagger@NPSpinCoefficients[dyad][[7]]+omicron[A]*NPSpinCoefficients[dyad][[4]]*iotadg[Adg]*NPSpinCoefficients[dyad][[8]]+omicrondg[Adg]*NPSpinCoefficients[dyad][[4]]*iota[A]*Dagger@NPSpinCoefficients[dyad][[8]]+omicron[A]*iotadg[Adg]*NPSpinCoefficients[dyad][[8]]*NPSpinCoefficients[dyad][[10]]+omicrondg[Adg]*iota[A]*NPSpinCoefficients[dyad][[8]]*NPSpinCoefficients[dyad][[11]]-omicrondg[Adg]*NPSpinCoefficients[dyad][[3]]*iota[A]*NPSpinCoefficients[dyad][[12]]-omicron[A]*iotadg[Adg]*NPSpinCoefficients[dyad][[6]]*NPSpinCoefficients[dyad][[12]]-omicrondg[Adg]*iota[A]*NPSpinCoefficients[dyad][[7]]*NPSpinCoefficients[dyad][[12]]-omicron[A]*NPSpinCoefficients[dyad][[3]]*iotadg[Adg]*Dagger@NPSpinCoefficients[dyad][[12]]+iota[A]*iotadg[Adg]*pdnp[{1,-tetrad}][NPSpinCoefficients[dyad][[3]]]-omicron[A]*iotadg[Adg]*pdnp[{2,-tetrad}][NPSpinCoefficients[dyad][[1]]]-omicrondg[Adg]*iota[A]*pdnp[{2,-tetrad}][NPSpinCoefficients[dyad][[2]]]+omicron[A]*omicrondg[Adg]*pdnp[{2,-tetrad}][NPSpinCoefficients[dyad][[3]]],cde[A,Adg][NPSpinCoefficients[dyad][[4]]]==-$RiemannSign*$LambdaSign*Lambda[cde][]*omicron[A]*omicrondg[Adg]+$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"11",dyad][]omicron[A]*omicrondg[Adg]+$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"2",dyad][]omicron[A]*omicrondg[Adg]+2*omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[3]]*NPSpinCoefficients[dyad][[4]]+omicron[A]*omicrondg[Adg]*Dagger@NPSpinCoefficients[dyad][[3]]*NPSpinCoefficients[dyad][[4]]+omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[3]]*Dagger@NPSpinCoefficients[dyad][[4]]-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"1",dyad][]omicrondg[Adg]*iota[A]-omicrondg[Adg]*Dagger@NPSpinCoefficients[dyad][[1]]*NPSpinCoefficients[dyad][[4]]*iota[A]-omicrondg[Adg]*NPSpinCoefficients[dyad][[2]]*Dagger@NPSpinCoefficients[dyad][[4]]*iota[A]-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"10",dyad][]omicron[A]*iotadg[Adg]-2*omicron[A]*NPSpinCoefficients[dyad][[1]]*NPSpinCoefficients[dyad][[4]]*iotadg[Adg]-omicron[A]*Dagger@NPSpinCoefficients[dyad][[2]]*NPSpinCoefficients[dyad][[4]]*iotadg[Adg]+omicron[A]*NPSpinCoefficients[dyad][[1]]*Dagger@NPSpinCoefficients[dyad][[4]]*iotadg[Adg]-omicrondg[Adg]*NPSpinCoefficients[dyad][[3]]*iota[A]*NPSpinCoefficients[dyad][[5]]-omicron[A]*NPSpinCoefficients[dyad][[3]]*iotadg[Adg]*Dagger@NPSpinCoefficients[dyad][[5]]-omicron[A]*iotadg[Adg]*NPSpinCoefficients[dyad][[5]]*NPSpinCoefficients[dyad][[6]]-omicrondg[Adg]*iota[A]*NPSpinCoefficients[dyad][[5]]*NPSpinCoefficients[dyad][[7]]+omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[5]]*NPSpinCoefficients[dyad][[8]]-omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[2]]*NPSpinCoefficients[dyad][[9]]+omicron[A]*NPSpinCoefficients[dyad][[4]]*iotadg[Adg]*NPSpinCoefficients[dyad][[9]]-omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[1]]*Dagger@NPSpinCoefficients[dyad][[9]]+omicrondg[Adg]*NPSpinCoefficients[dyad][[4]]*iota[A]*Dagger@NPSpinCoefficients[dyad][[9]]+omicron[A]*NPSpinCoefficients[dyad][[1]]*iotadg[Adg]*NPSpinCoefficients[dyad][[10]]+omicron[A]*iotadg[Adg]*NPSpinCoefficients[dyad][[9]]*NPSpinCoefficients[dyad][[10]]+omicrondg[Adg]*NPSpinCoefficients[dyad][[2]]*iota[A]*Dagger@NPSpinCoefficients[dyad][[10]]+omicrondg[Adg]*NPSpinCoefficients[dyad][[1]]*iota[A]*NPSpinCoefficients[dyad][[11]]+omicrondg[Adg]*iota[A]*NPSpinCoefficients[dyad][[9]]*NPSpinCoefficients[dyad][[11]]+omicron[A]*NPSpinCoefficients[dyad][[2]]*iotadg[Adg]*Dagger@NPSpinCoefficients[dyad][[11]]-omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[1]]*NPSpinCoefficients[dyad][[12]]-omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[9]]*NPSpinCoefficients[dyad][[12]]-omicron[A]*omicrondg[Adg]*NPSpinCoefficients[dyad][[2]]*Dagger@NPSpinCoefficients[dyad][[12]]-omicron[A]*iotadg[Adg]*pdnp[{1,-tetrad}][NPSpinCoefficients[dyad][[1]]]-omicrondg[Adg]*iota[A]*pdnp[{1,-tetrad}][NPSpinCoefficients[dyad][[2]]]+omicron[A]*omicrondg[Adg]*pdnp[{1,-tetrad}][NPSpinCoefficients[dyad][[3]]]+iota[A]*iotadg[Adg]*pdnp[{1,-tetrad}][NPSpinCoefficients[dyad][[4]]]};
CovDNPSpinCoeffRules[dyad]^=Flatten[MakeRule[Evaluate[List@@#],MetricOn->All,ContractMetrics->True]&/@Join[tmprules,Map[Dagger,tmprules,{2}]]];
(* Dyad expansions of curvature *)
DyadExpansionEq[eps,dyad]^=eps[-A,-B]==ToCanonical[TraceBasisDummy@SeparateBasis[dyad]@eps[-A,-B]/.BasisToDyadRules[dyad]];
DyadExpansionEq[Evaluate[Psi[cde]],dyad]^=Psi[cde][-A,-B,-C,-D]==ToCanonical[TraceBasisDummy@SeparateBasis[dyad]@Psi[cde][-A,-B,-C,-D]/.BasisToDyadRules[dyad]];
DyadExpansionEq[Evaluate[Dagger@Psi[cde]],dyad]^=Dagger/@DyadExpansionEq[Evaluate[Psi[cde]],dyad];
DyadExpansionEq[Evaluate[Phi[cde]],dyad]^=Phi[cde][-A,-B,-Adg,-Bdg]==ToCanonical[TraceBasisDummy@SeparateBasis[dyad]@SeparateBasis[dyaddg]@Phi[cde][-A,-B,-Adg,-Bdg]/.BasisToDyadRules[dyad]];
]


FormatComponents[dyad_,SS_?xTensorQ,symbform_]:=(Union@Flatten@ToCanonical@ComponentArray[ToBasis[Dagger@dyad]@ToBasis[dyad]@GiveIndicesToTensor@SS]/.TT_?xTensorQ[inds___]:>Module[{dyadinds=Select[{inds},#[[2]]===-dyad&],dyaddginds=Select[{inds},#[[2]]===-Dagger[dyad]&]},UpSet[MakeBoxes[TT[inds],StandardForm],xAct`xTensor`Private`interpretbox[TT[inds],SubscriptBox[PrintAs[TT],StringJoin["",If[Length[dyadinds]>0,symbform@ToString[Plus@@(First/@dyadinds)],""],If[Length[dyaddginds]>0,StringJoin[symbform@ToString[Plus@@(First/@dyaddginds)],"'"],""],""]]]]])/;CompatibleSymQ[SlotsOfTensor[SS],SymmetryGroupOfTensor@SS,{VBundleOfBasis@dyad,Dagger@VBundleOfBasis@dyad}];


SetTexComponents[dyad_, SS_?xTensorQ, texform_]:=(Union@Flatten@ToCanonical@ComponentArray[ToBasis[Dagger@dyad]@ToBasis[dyad]@GiveIndicesToTensor@SS]/.TT_?xTensorQ[inds___]:>Module[{dyadinds=Select[{inds},#[[2]]===-dyad&],dyaddginds=Select[{inds},#[[2]]===-Dagger[dyad]&]},UpSet[Tex[TT[inds]],StringJoin[Tex[TT],"_{",If[Length[dyadinds]>0,texform@ToString[Plus@@(First/@dyadinds)],""],If[Length[dyaddginds]>0,StringJoin[texform@ToString[Plus@@(First/@dyaddginds)],"'"],""],"}"]]])/;CompatibleSymQ[SlotsOfTensor[SS],SymmetryGroupOfTensor@SS,{VBundleOfBasis@dyad,Dagger@VBundleOfBasis@dyad}];


GHPOrderOfTerm[prod_Times]:=Max@@(GHPOrderOfTerm/@(List@@prod))


GHPOrderOfTerm[a_]:=0


GHPOrderOfTerm[x:InertScalarOp[__][_]]:=Max[GHPOrderOfTerm/@xAct`xTensor`Private`ListOfTerms[Expand[x/.InertScalarOp->Function/.OrderedPlus->Plus]]]


SortByGHPOrder[expr_Equal]:=SortByGHPOrder/@expr


SortByGHPOrder[expr_]:=OrderedPlus@@SortBy[xAct`xTensor`Private`ListOfTerms[expr],(-GHPOrderOfTerm@#)&]


DefNPOperatorTools[dyad_,tetrad_,cde_,{a_,A_}]:=
With[{omicronA=SpinorsOfDyad[dyad][[1]][A],
iotaA=SpinorsOfDyad[dyad][[2]][A],omicrondgAdg=Dagger[SpinorsOfDyad[dyad][[1]][A]],iotadgAdg=Dagger[SpinorsOfDyad[dyad][[2]][A]],
\[Alpha]dyad=NPSpinCoefficients[dyad][[1]],
\[Beta]dyad=NPSpinCoefficients[dyad][[2]],
\[Gamma]dyad=NPSpinCoefficients[dyad][[3]],
\[Epsilon]dyad=NPSpinCoefficients[dyad][[4]],
\[Kappa]dyad=NPSpinCoefficients[dyad][[5]],
\[Lambda]dyad=NPSpinCoefficients[dyad][[6]],
\[Mu]dyad=NPSpinCoefficients[dyad][[7]],
\[Nu]dyad=NPSpinCoefficients[dyad][[8]],
\[Pi]dyad=NPSpinCoefficients[dyad][[9]],
\[Rho]dyad=NPSpinCoefficients[dyad][[10]],
\[Sigma]dyad=NPSpinCoefficients[dyad][[11]],
\[Tau]dyad=NPSpinCoefficients[dyad][[12]],
lambda=-$LambdaSign*$RiemannSign*Lambda[cde][],
psi0=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"0",dyad][],
psi1=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"1",dyad][],
psi2=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"2",dyad][],psi3=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"3",dyad][],psi4=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"4",dyad][],phi00=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"00",dyad][],
phi01=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"01",dyad][],
phi02=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"02",dyad][],
phi10=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"10",dyad][],
phi11=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"11",dyad][],
phi12=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"12",dyad][],
phi20=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"20",dyad][],
phi21=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"21",dyad][],
phi22=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"22",dyad][],
sigma=SolderingFormOfVBundle@VBundleOfIndex@A,exprsymb=Symbol["expr"]
},With[{pdnp=PDOfBasis@tetrad,Adg=DaggerIndex@A,
tangentpmQ=xAct`xTensor`Private`VBundleIndexPMQ[VBundleOfIndex[a]],spinpmQ=xAct`xTensor`Private`VBundleIndexPMQ[VBundleOfIndex[A]],spindgpmQ=xAct`xTensor`Private`VBundleIndexPMQ[VBundleOfIndex@DaggerIndex[A]]},
(* Commutators *)
CommuteNPOp[pdnp[{1, -tetrad}],pdnp[{2, -tetrad}],s___]:=pdnp[{1, -tetrad}][pdnp[{2, -tetrad}][expr_]] :> (pdnp[{2, -tetrad}][pdnp[{1, -tetrad}][expr]]
-(\[Gamma]dyad+Dagger@\[Gamma]dyad)*pdnp[{1, -tetrad}][expr]
-(\[Epsilon]dyad+Dagger@\[Epsilon]dyad)*pdnp[{2, -tetrad}][expr]
+(\[Pi]dyad+Dagger@\[Tau]dyad)*pdnp[{3, -tetrad}][expr]
+(\[Tau]dyad+Dagger@\[Pi]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{2, -tetrad}],pdnp[{1, -tetrad}],s___]:=pdnp[{2, -tetrad}][pdnp[{1, -tetrad}][expr_]] :> (pdnp[{1, -tetrad}][pdnp[{2, -tetrad}][expr]]
+(\[Gamma]dyad+Dagger@\[Gamma]dyad)*pdnp[{1, -tetrad}][expr]
+(\[Epsilon]dyad+Dagger@\[Epsilon]dyad)*pdnp[{2, -tetrad}][expr]
-(\[Pi]dyad+Dagger@\[Tau]dyad)*pdnp[{3, -tetrad}][expr]
-(\[Tau]dyad+Dagger@\[Pi]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{1, -tetrad}],pdnp[{3, -tetrad}],s___]:=pdnp[{1, -tetrad}][pdnp[{3, -tetrad}][expr_]] :> (pdnp[{3, -tetrad}][pdnp[{1, -tetrad}][expr]]
-(Dagger@\[Alpha]dyad+\[Beta]dyad-Dagger@\[Pi]dyad)*pdnp[{1, -tetrad}][expr]
-\[Kappa]dyad*pdnp[{2, -tetrad}][expr]
+(\[Epsilon]dyad-Dagger@\[Epsilon]dyad+Dagger@\[Rho]dyad)*pdnp[{3, -tetrad}][expr]
+\[Sigma]dyad*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{3, -tetrad}],pdnp[{1, -tetrad}],s___]:=pdnp[{3, -tetrad}][pdnp[{1, -tetrad}][expr_]] :> (pdnp[{1, -tetrad}][pdnp[{3, -tetrad}][expr]]
+(Dagger@\[Alpha]dyad+\[Beta]dyad-Dagger@\[Pi]dyad)*pdnp[{1, -tetrad}][expr]
+\[Kappa]dyad*pdnp[{2, -tetrad}][expr]
-(\[Epsilon]dyad-Dagger@\[Epsilon]dyad+Dagger@\[Rho]dyad)*pdnp[{3, -tetrad}][expr]
-\[Sigma]dyad*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{1, -tetrad}],pdnp[{4, -tetrad}],s___]:=pdnp[{1, -tetrad}][pdnp[{4, -tetrad}][expr_]] :> (pdnp[{4, -tetrad}][pdnp[{1, -tetrad}][expr]]
-(\[Alpha]dyad+Dagger@\[Beta]dyad-\[Pi]dyad)*pdnp[{1, -tetrad}][expr]
-Dagger@\[Kappa]dyad*pdnp[{2, -tetrad}][expr]
+Dagger@\[Sigma]dyad*pdnp[{3, -tetrad}][expr]
+(-\[Epsilon]dyad+Dagger@\[Epsilon]dyad+\[Rho]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{4, -tetrad}],pdnp[{1, -tetrad}],s___]:=pdnp[{4, -tetrad}][pdnp[{1, -tetrad}][expr_]] :> (pdnp[{1, -tetrad}][pdnp[{4, -tetrad}][expr]]
+(\[Alpha]dyad+Dagger@\[Beta]dyad-\[Pi]dyad)*pdnp[{1, -tetrad}][expr]
+Dagger@\[Kappa]dyad*pdnp[{2, -tetrad}][expr]
-Dagger@\[Sigma]dyad*pdnp[{3, -tetrad}][expr]
-(-\[Epsilon]dyad+Dagger@\[Epsilon]dyad+\[Rho]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];CommuteNPOp[pdnp[{2, -tetrad}],pdnp[{3, -tetrad}],s___]:=pdnp[{2, -tetrad}][pdnp[{3, -tetrad}][expr_]] :> (pdnp[{3, -tetrad}][pdnp[{2, -tetrad}][expr]]
+Dagger@\[Nu]dyad*pdnp[{1, -tetrad}][expr]
+(Dagger@\[Alpha]dyad+\[Beta]dyad-\[Tau]dyad)*pdnp[{2, -tetrad}][expr]
+(\[Gamma]dyad-Dagger@\[Gamma]dyad-\[Mu]dyad)*pdnp[{3, -tetrad}][expr]
-Dagger@\[Lambda]dyad*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{3, -tetrad}],pdnp[{2, -tetrad}],s___]:=pdnp[{3, -tetrad}][pdnp[{2, -tetrad}][expr_]] :> (pdnp[{2, -tetrad}][pdnp[{3, -tetrad}][expr]]
-Dagger@\[Nu]dyad*pdnp[{1, -tetrad}][expr]
-(Dagger@\[Alpha]dyad+\[Beta]dyad-\[Tau]dyad)*pdnp[{2, -tetrad}][expr]
-(\[Gamma]dyad-Dagger@\[Gamma]dyad-\[Mu]dyad)*pdnp[{3, -tetrad}][expr]
+Dagger@\[Lambda]dyad*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{2, -tetrad}],pdnp[{4, -tetrad}],s___]:=pdnp[{2, -tetrad}][pdnp[{4, -tetrad}][expr_]] :> (pdnp[{4, -tetrad}][pdnp[{2, -tetrad}][expr]]
+ \[Nu]dyad*pdnp[{1, -tetrad}][expr]
+(\[Alpha]dyad+Dagger@\[Beta]dyad-Dagger@\[Tau]dyad)*pdnp[{2, -tetrad}][expr]
-\[Lambda]dyad*pdnp[{3, -tetrad}][expr]
-(\[Gamma]dyad-Dagger@\[Gamma]dyad+Dagger@\[Mu]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{4, -tetrad}],pdnp[{2, -tetrad}],s___]:=pdnp[{4, -tetrad}][pdnp[{2, -tetrad}][expr_]] :> (pdnp[{2, -tetrad}][pdnp[{4, -tetrad}][expr]]
- \[Nu]dyad*pdnp[{1, -tetrad}][expr]
-(\[Alpha]dyad+Dagger@\[Beta]dyad-Dagger@\[Tau]dyad)*pdnp[{2, -tetrad}][expr]
+\[Lambda]dyad*pdnp[{3, -tetrad}][expr]
+(\[Gamma]dyad-Dagger@\[Gamma]dyad+Dagger@\[Mu]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{3, -tetrad}],pdnp[{4, -tetrad}],s___]:=pdnp[{3, -tetrad}][pdnp[{4, -tetrad}][expr_]] :> (pdnp[{4, -tetrad}][pdnp[{3, -tetrad}][expr]]
+(\[Mu]dyad-Dagger@\[Mu]dyad)*pdnp[{1, -tetrad}][expr]
+(\[Rho]dyad-Dagger@\[Rho]dyad)*pdnp[{2, -tetrad}][expr]
+(-\[Alpha]dyad+Dagger@\[Beta]dyad)*pdnp[{3, -tetrad}][expr]
-(\[Beta]dyad-Dagger@\[Alpha]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[{4, -tetrad}],pdnp[{3, -tetrad}],s___]:=pdnp[{4, -tetrad}][pdnp[{3, -tetrad}][expr_]] :> (pdnp[{3, -tetrad}][pdnp[{4, -tetrad}][expr]]
-(\[Mu]dyad-Dagger@\[Mu]dyad)*pdnp[{1, -tetrad}][expr]
-(\[Rho]dyad-Dagger@\[Rho]dyad)*pdnp[{2, -tetrad}][expr]
-(-\[Alpha]dyad+Dagger@\[Beta]dyad)*pdnp[{3, -tetrad}][expr]
+(\[Beta]dyad-Dagger@\[Alpha]dyad)*pdnp[{4, -tetrad}][expr]
)/;Or[Length@List@s==0 ,expr==s];
CommuteNPOp[pdnp[aa_][pdnp[bb_][expr_]]]:=CommuteNPOp[pdnp[aa],pdnp[bb],expr];
NPCommutatorsForward[dyad]^=CommuteNPOp[pdnp[{#[[1]],-tetrad}],pdnp[{#[[2]],-tetrad}]]&/@Select[Join@@Outer[List,Range@4,Range@4],#[[1]]>#[[2]]&];
NPCommutatorsBackward[dyad]^=CommuteNPOp[pdnp[{#[[2]],-tetrad}],pdnp[{#[[1]],-tetrad}]]&/@Select[Join@@Outer[List,Range@4,Range@4],#[[1]]>#[[2]]&];
Module[
(* Ricci *)
{tmpeqs=Evaluate[{pdnp[{2,-tetrad}][\[Epsilon]dyad]==lambda-phi11-psi2+2*\[Gamma]dyad*\[Epsilon]dyad+\[Kappa]dyad*\[Nu]dyad-\[Beta]dyad*\[Pi]dyad-\[Alpha]dyad*\[Tau]dyad-\[Pi]dyad*\[Tau]dyad+\[Epsilon]dyad*Dagger[\[Gamma]dyad]+\[Gamma]dyad*Dagger[\[Epsilon]dyad]-\[Alpha]dyad*Dagger[\[Pi]dyad]-\[Beta]dyad*Dagger[\[Tau]dyad]+pdnp[{1,-tetrad}][\[Gamma]dyad],pdnp[{2,-tetrad}][\[Kappa]dyad]==-phi01-psi1+3*\[Gamma]dyad*\[Kappa]dyad-\[Pi]dyad*\[Sigma]dyad-\[Epsilon]dyad*\[Tau]dyad-\[Rho]dyad*\[Tau]dyad+\[Kappa]dyad*Dagger[\[Gamma]dyad]+\[Tau]dyad*Dagger[\[Epsilon]dyad]-\[Rho]dyad*Dagger[\[Pi]dyad]-\[Sigma]dyad*Dagger[\[Tau]dyad]+pdnp[{1,-tetrad}][\[Tau]dyad],pdnp[{2,-tetrad}][\[Pi]dyad]==-phi21-psi3+3*\[Epsilon]dyad*\[Nu]dyad-\[Gamma]dyad*\[Pi]dyad-\[Mu]dyad*\[Pi]dyad-\[Lambda]dyad*\[Tau]dyad+\[Pi]dyad*Dagger[\[Gamma]dyad]+\[Nu]dyad*Dagger[\[Epsilon]dyad]-\[Lambda]dyad*Dagger[\[Pi]dyad]-\[Mu]dyad*Dagger[\[Tau]dyad]+pdnp[{1,-tetrad}][\[Nu]dyad],pdnp[{3,-tetrad}][\[Gamma]dyad]==phi12-2*\[Beta]dyad*\[Gamma]dyad+\[Beta]dyad*\[Mu]dyad-\[Nu]dyad*\[Sigma]dyad+\[Gamma]dyad*\[Tau]dyad+\[Mu]dyad*\[Tau]dyad-\[Gamma]dyad*Dagger[\[Alpha]dyad]+\[Beta]dyad*Dagger[\[Gamma]dyad]+\[Alpha]dyad*Dagger[\[Lambda]dyad]-\[Epsilon]dyad*Dagger[\[Nu]dyad]+pdnp[{2,-tetrad}][\[Beta]dyad],pdnp[{3,-tetrad}][\[Epsilon]dyad]==-psi1+\[Gamma]dyad*\[Kappa]dyad+\[Kappa]dyad*\[Mu]dyad-\[Alpha]dyad*\[Sigma]dyad-\[Pi]dyad*\[Sigma]dyad+\[Epsilon]dyad*Dagger[\[Alpha]dyad]+\[Beta]dyad*Dagger[\[Epsilon]dyad]-\[Epsilon]dyad*Dagger[\[Pi]dyad]-\[Beta]dyad*Dagger[\[Rho]dyad]+pdnp[{1,-tetrad}][\[Beta]dyad],pdnp[{3,-tetrad}][\[Kappa]dyad]==-psi0+3*\[Beta]dyad*\[Kappa]dyad-3*\[Epsilon]dyad*\[Sigma]dyad-\[Rho]dyad*\[Sigma]dyad+\[Kappa]dyad*\[Tau]dyad+\[Kappa]dyad*Dagger[\[Alpha]dyad]+\[Sigma]dyad*Dagger[\[Epsilon]dyad]-\[Kappa]dyad*Dagger[\[Pi]dyad]-\[Sigma]dyad*Dagger[\[Rho]dyad]+pdnp[{1,-tetrad}][\[Sigma]dyad],pdnp[{3,-tetrad}][\[Nu]dyad]==phi22+\[Gamma]dyad*\[Mu]dyad+\[Mu]dyad^2-3*\[Beta]dyad*\[Nu]dyad+\[Nu]dyad*\[Tau]dyad-\[Nu]dyad*Dagger[\[Alpha]dyad]+\[Mu]dyad*Dagger[\[Gamma]dyad]+\[Lambda]dyad*Dagger[\[Lambda]dyad]-\[Pi]dyad*Dagger[\[Nu]dyad]+pdnp[{2,-tetrad}][\[Mu]dyad],pdnp[{3,-tetrad}][\[Pi]dyad]==-2*lambda-psi2+\[Epsilon]dyad*\[Mu]dyad+\[Kappa]dyad*\[Nu]dyad-\[Beta]dyad*\[Pi]dyad-\[Lambda]dyad*\[Sigma]dyad+\[Pi]dyad*Dagger[\[Alpha]dyad]+\[Mu]dyad*Dagger[\[Epsilon]dyad]-\[Pi]dyad*Dagger[\[Pi]dyad]-\[Mu]dyad*Dagger[\[Rho]dyad]+pdnp[{1,-tetrad}][\[Mu]dyad],pdnp[{3,-tetrad}][\[Tau]dyad]==phi02-3*\[Gamma]dyad*\[Sigma]dyad+\[Mu]dyad*\[Sigma]dyad+\[Beta]dyad*\[Tau]dyad+\[Tau]dyad^2-\[Tau]dyad*Dagger[\[Alpha]dyad]+\[Sigma]dyad*Dagger[\[Gamma]dyad]+\[Rho]dyad*Dagger[\[Lambda]dyad]-\[Kappa]dyad*Dagger[\[Nu]dyad]+pdnp[{2,-tetrad}][\[Sigma]dyad],pdnp[{4,-tetrad}][\[Beta]dyad]==-lambda-phi11+psi2+2*\[Alpha]dyad*\[Beta]dyad-\[Epsilon]dyad*\[Mu]dyad-\[Gamma]dyad*\[Rho]dyad-\[Mu]dyad*\[Rho]dyad+\[Lambda]dyad*\[Sigma]dyad-\[Alpha]dyad*Dagger[\[Alpha]dyad]-\[Beta]dyad*Dagger[\[Beta]dyad]+\[Epsilon]dyad*Dagger[\[Mu]dyad]+\[Gamma]dyad*Dagger[\[Rho]dyad]+pdnp[{3,-tetrad}][\[Alpha]dyad],pdnp[{4,-tetrad}][\[Gamma]dyad]==psi3+\[Beta]dyad*\[Lambda]dyad-\[Epsilon]dyad*\[Nu]dyad-\[Nu]dyad*\[Rho]dyad+\[Lambda]dyad*\[Tau]dyad-\[Gamma]dyad*Dagger[\[Beta]dyad]-\[Alpha]dyad*Dagger[\[Gamma]dyad]+\[Alpha]dyad*Dagger[\[Mu]dyad]+\[Gamma]dyad*Dagger[\[Tau]dyad]+pdnp[{2,-tetrad}][\[Alpha]dyad],pdnp[{4,-tetrad}][\[Epsilon]dyad]==-phi10+2*\[Alpha]dyad*\[Epsilon]dyad+\[Kappa]dyad*\[Lambda]dyad-\[Epsilon]dyad*\[Pi]dyad-\[Alpha]dyad*\[Rho]dyad-\[Pi]dyad*\[Rho]dyad+\[Epsilon]dyad*Dagger[\[Beta]dyad]-\[Alpha]dyad*Dagger[\[Epsilon]dyad]+\[Gamma]dyad*Dagger[\[Kappa]dyad]-\[Beta]dyad*Dagger[\[Sigma]dyad]+pdnp[{1,-tetrad}][\[Alpha]dyad],pdnp[{4,-tetrad}][\[Kappa]dyad]==-phi00+3*\[Alpha]dyad*\[Kappa]dyad-\[Kappa]dyad*\[Pi]dyad-\[Epsilon]dyad*\[Rho]dyad-\[Rho]dyad^2+\[Kappa]dyad*Dagger[\[Beta]dyad]-\[Rho]dyad*Dagger[\[Epsilon]dyad]+\[Tau]dyad*Dagger[\[Kappa]dyad]-\[Sigma]dyad*Dagger[\[Sigma]dyad]+pdnp[{1,-tetrad}][\[Rho]dyad],pdnp[{4,-tetrad}][\[Mu]dyad]==-phi21+psi3+3*\[Beta]dyad*\[Lambda]dyad-\[Alpha]dyad*\[Mu]dyad-\[Mu]dyad*\[Pi]dyad-\[Nu]dyad*\[Rho]dyad-\[Lambda]dyad*Dagger[\[Alpha]dyad]-\[Mu]dyad*Dagger[\[Beta]dyad]+\[Pi]dyad*Dagger[\[Mu]dyad]+\[Nu]dyad*Dagger[\[Rho]dyad]+pdnp[{3,-tetrad}][\[Lambda]dyad],pdnp[{4,-tetrad}][\[Nu]dyad]==psi4+3*\[Gamma]dyad*\[Lambda]dyad+\[Lambda]dyad*\[Mu]dyad-3*\[Alpha]dyad*\[Nu]dyad-\[Nu]dyad*\[Pi]dyad-\[Nu]dyad*Dagger[\[Beta]dyad]-\[Lambda]dyad*Dagger[\[Gamma]dyad]+\[Lambda]dyad*Dagger[\[Mu]dyad]+\[Nu]dyad*Dagger[\[Tau]dyad]+pdnp[{2,-tetrad}][\[Lambda]dyad],pdnp[{4,-tetrad}][\[Pi]dyad]==-phi20+3*\[Epsilon]dyad*\[Lambda]dyad-\[Alpha]dyad*\[Pi]dyad-\[Pi]dyad^2-\[Lambda]dyad*\[Rho]dyad+\[Pi]dyad*Dagger[\[Beta]dyad]-\[Lambda]dyad*Dagger[\[Epsilon]dyad]+\[Nu]dyad*Dagger[\[Kappa]dyad]-\[Mu]dyad*Dagger[\[Sigma]dyad]+pdnp[{1,-tetrad}][\[Lambda]dyad],pdnp[{4,-tetrad}][\[Sigma]dyad]==-phi01+psi1-\[Kappa]dyad*\[Mu]dyad-\[Beta]dyad*\[Rho]dyad+3*\[Alpha]dyad*\[Sigma]dyad-\[Rho]dyad*\[Tau]dyad-\[Rho]dyad*Dagger[\[Alpha]dyad]-\[Sigma]dyad*Dagger[\[Beta]dyad]+\[Kappa]dyad*Dagger[\[Mu]dyad]+\[Tau]dyad*Dagger[\[Rho]dyad]+pdnp[{3,-tetrad}][\[Rho]dyad],pdnp[{4,-tetrad}][\[Tau]dyad]==2*lambda+psi2-\[Kappa]dyad*\[Nu]dyad-\[Gamma]dyad*\[Rho]dyad+\[Lambda]dyad*\[Sigma]dyad+\[Alpha]dyad*\[Tau]dyad-\[Tau]dyad*Dagger[\[Beta]dyad]-\[Rho]dyad*Dagger[\[Gamma]dyad]+\[Rho]dyad*Dagger[\[Mu]dyad]+\[Tau]dyad*Dagger[\[Tau]dyad]+pdnp[{2,-tetrad}][\[Rho]dyad]}]},
NPRicciRules[dyad]^=(Join[tmpeqs,Dagger/@tmpeqs]/.Equal->Rule);
(* Bianchi *)
tmpeqs={pdnp[{1,-tetrad}][phi01]==4*psi0*\[Alpha]dyad-2*phi00*\[Beta]dyad+2*phi01*\[Epsilon]dyad-2*psi1*\[Epsilon]dyad-2*phi11*\[Kappa]dyad+3*psi2*\[Kappa]dyad-psi0*\[Pi]dyad-4*psi1*\[Rho]dyad+2*phi10*\[Sigma]dyad-2*phi00*Dagger[\[Alpha]dyad]-phi02*Dagger[\[Kappa]dyad]+phi00*Dagger[\[Pi]dyad]+2*phi01*Dagger[\[Rho]dyad]+pdnp[{1,-tetrad}][psi1]+pdnp[{3,-tetrad}][phi00]-pdnp[{4,-tetrad}][psi0],pdnp[{1,-tetrad}][phi02]==-2*phi01*\[Beta]dyad-2*psi1*\[Beta]dyad+4*psi0*\[Gamma]dyad+2*phi02*\[Epsilon]dyad-2*phi12*\[Kappa]dyad-psi0*\[Mu]dyad+2*phi11*\[Sigma]dyad+3*psi2*\[Sigma]dyad-4*psi1*\[Tau]dyad-2*phi02*Dagger[\[Epsilon]dyad]-phi00*Dagger[\[Lambda]dyad]+2*phi01*Dagger[\[Pi]dyad]+phi02*Dagger[\[Rho]dyad]-pdnp[{2,-tetrad}][psi0]+pdnp[{3,-tetrad}][phi01]+pdnp[{3,-tetrad}][psi1],pdnp[{1,-tetrad}][phi11]==2*psi1*\[Alpha]dyad-phi21*\[Kappa]dyad+2*psi3*\[Kappa]dyad+psi0*\[Lambda]dyad-phi00*\[Mu]dyad+phi01*\[Pi]dyad-2*psi1*\[Pi]dyad-3*psi2*\[Rho]dyad+phi20*\[Sigma]dyad-2*phi10*Dagger[\[Alpha]dyad]-phi12*Dagger[\[Kappa]dyad]+phi10*Dagger[\[Pi]dyad]+2*phi11*Dagger[\[Rho]dyad]-pdnp[{1,-tetrad}][lambda]+pdnp[{1,-tetrad}][psi2]+pdnp[{3,-tetrad}][phi10]-pdnp[{4,-tetrad}][psi1],pdnp[{1,-tetrad}][phi22]==2*phi21*\[Beta]dyad+2*psi3*\[Beta]dyad-2*phi22*\[Epsilon]dyad-2*phi11*\[Mu]dyad-3*psi2*\[Mu]dyad+2*psi1*\[Nu]dyad+2*phi12*\[Pi]dyad+psi4*\[Sigma]dyad-2*psi3*\[Tau]dyad-2*phi22*Dagger[\[Epsilon]dyad]-phi20*Dagger[\[Lambda]dyad]+2*phi21*Dagger[\[Pi]dyad]+phi22*Dagger[\[Rho]dyad]-2*pdnp[{2,-tetrad}][lambda]-pdnp[{2,-tetrad}][psi2]+pdnp[{3,-tetrad}][phi21]+pdnp[{3,-tetrad}][psi3],pdnp[{1,-tetrad}][psi4]==2*phi21*\[Alpha]dyad+2*psi3*\[Alpha]dyad-2*phi20*\[Gamma]dyad-4*psi4*\[Epsilon]dyad-2*phi11*\[Lambda]dyad-3*psi2*\[Lambda]dyad+2*phi10*\[Nu]dyad+4*psi3*\[Pi]dyad+psi4*\[Rho]dyad+2*phi20*Dagger[\[Gamma]dyad]-phi20*Dagger[\[Mu]dyad]+phi22*Dagger[\[Sigma]dyad]-2*phi21*Dagger[\[Tau]dyad]-pdnp[{2,-tetrad}][phi20]+pdnp[{4,-tetrad}][phi21]+pdnp[{4,-tetrad}][psi3],pdnp[{2,-tetrad}][phi00]==-2*phi01*\[Alpha]dyad-2*psi1*\[Alpha]dyad+2*phi00*\[Gamma]dyad-2*psi3*\[Kappa]dyad-psi0*\[Lambda]dyad+2*psi1*\[Pi]dyad+2*phi11*\[Rho]dyad+3*psi2*\[Rho]dyad-2*phi10*\[Tau]dyad+2*phi00*Dagger[\[Gamma]dyad]-phi00*Dagger[\[Mu]dyad]+phi02*Dagger[\[Sigma]dyad]-2*phi01*Dagger[\[Tau]dyad]-2*pdnp[{1,-tetrad}][lambda]-pdnp[{1,-tetrad}][psi2]+pdnp[{4,-tetrad}][phi01]+pdnp[{4,-tetrad}][psi1],pdnp[{2,-tetrad}][phi11]==-2*psi3*\[Beta]dyad-phi02*\[Lambda]dyad+3*psi2*\[Mu]dyad+phi01*\[Nu]dyad-2*psi1*\[Nu]dyad+phi22*\[Rho]dyad-psi4*\[Sigma]dyad-phi21*\[Tau]dyad+2*psi3*\[Tau]dyad+2*phi12*Dagger[\[Beta]dyad]-2*phi11*Dagger[\[Mu]dyad]+phi10*Dagger[\[Nu]dyad]-phi12*Dagger[\[Tau]dyad]-pdnp[{2,-tetrad}][lambda]+pdnp[{2,-tetrad}][psi2]-pdnp[{3,-tetrad}][psi3]+pdnp[{4,-tetrad}][phi12],pdnp[{2,-tetrad}][phi21]==2*phi22*\[Alpha]dyad-4*psi4*\[Beta]dyad-2*phi21*\[Gamma]dyad+2*psi3*\[Gamma]dyad-2*phi12*\[Lambda]dyad+4*psi3*\[Mu]dyad+2*phi11*\[Nu]dyad-3*psi2*\[Nu]dyad+psi4*\[Tau]dyad+2*phi22*Dagger[\[Beta]dyad]-2*phi21*Dagger[\[Mu]dyad]+phi20*Dagger[\[Nu]dyad]-phi22*Dagger[\[Tau]dyad]+pdnp[{2,-tetrad}][psi3]-pdnp[{3,-tetrad}][psi4]+pdnp[{4,-tetrad}][phi22],pdnp[{3,-tetrad}][phi11]==-2*psi1*\[Gamma]dyad+phi22*\[Kappa]dyad+phi01*\[Mu]dyad+2*psi1*\[Mu]dyad-psi0*\[Nu]dyad-phi02*\[Pi]dyad-phi21*\[Sigma]dyad-2*psi3*\[Sigma]dyad+3*psi2*\[Tau]dyad+2*phi12*Dagger[\[Epsilon]dyad]+phi10*Dagger[\[Lambda]dyad]-2*phi11*Dagger[\[Pi]dyad]-phi12*Dagger[\[Rho]dyad]+pdnp[{1,-tetrad}][phi12]+pdnp[{2,-tetrad}][psi1]+pdnp[{3,-tetrad}][lambda]-pdnp[{3,-tetrad}][psi2],pdnp[{4,-tetrad}][phi02]==2*phi02*\[Alpha]dyad-2*phi01*\[Gamma]dyad+2*psi1*\[Gamma]dyad-2*psi1*\[Mu]dyad+psi0*\[Nu]dyad-2*phi12*\[Rho]dyad+2*psi3*\[Sigma]dyad+2*phi11*\[Tau]dyad-3*psi2*\[Tau]dyad-2*phi02*Dagger[\[Beta]dyad]+2*phi01*Dagger[\[Mu]dyad]-phi00*Dagger[\[Nu]dyad]+phi02*Dagger[\[Tau]dyad]+pdnp[{2,-tetrad}][phi01]-pdnp[{2,-tetrad}][psi1]+2*pdnp[{3,-tetrad}][lambda]+pdnp[{3,-tetrad}][psi2],pdnp[{4,-tetrad}][phi11]==phi20*\[Beta]dyad-phi21*\[Epsilon]dyad+3*psi3*\[Epsilon]dyad+(3*psi4*\[Kappa]dyad)/2+phi01*\[Lambda]dyad+3*psi1*\[Lambda]dyad-phi10*\[Mu]dyad-phi00*\[Nu]dyad+phi11*\[Pi]dyad-(9*psi2*\[Pi]dyad)/2-phi21*\[Rho]dyad-3*psi3*\[Rho]dyad+phi20*\[Tau]dyad-phi20*Dagger[\[Alpha]dyad]-2*phi10*Dagger[\[Gamma]dyad]-(phi22*Dagger[\[Kappa]dyad])/2+phi10*Dagger[\[Mu]dyad]+(phi20*Dagger[\[Pi]dyad])/2+phi21*Dagger[\[Rho]dyad]-phi12*Dagger[\[Sigma]dyad]+2*phi11*Dagger[\[Tau]dyad]-pdnp[{1,-tetrad}][phi21]/2+(3*pdnp[{1,-tetrad}][psi3])/2+pdnp[{2,-tetrad}][phi10]+pdnp[{3,-tetrad}][phi20]/2-(3*pdnp[{4,-tetrad}][psi2])/2,pdnp[{4,-tetrad}][Dagger[psi1]]==2*phi20*\[Epsilon]dyad+phi00*\[Lambda]dyad-2*phi10*\[Pi]dyad-phi20*\[Rho]dyad+2*phi10*Dagger[\[Beta]dyad]+2*Dagger[psi1]*Dagger[\[Beta]dyad]-4*Dagger[psi0]*Dagger[\[Gamma]dyad]-2*phi20*Dagger[\[Epsilon]dyad]+2*phi21*Dagger[\[Kappa]dyad]+Dagger[psi0]*Dagger[\[Mu]dyad]-2*phi11*Dagger[\[Sigma]dyad]-3*Dagger[psi2]*Dagger[\[Sigma]dyad]+4*Dagger[psi1]*Dagger[\[Tau]dyad]+pdnp[{1,-tetrad}][phi20]+pdnp[{2,-tetrad}][Dagger[psi0]]-pdnp[{4,-tetrad}][phi10],pdnp[{4,-tetrad}][Dagger[psi2]]==2*phi21*\[Epsilon]dyad-2*psi3*\[Epsilon]dyad-psi4*\[Kappa]dyad-2*psi1*\[Lambda]dyad+phi00*\[Nu]dyad-2*phi11*\[Pi]dyad+3*psi2*\[Pi]dyad+2*psi3*\[Rho]dyad-phi20*\[Tau]dyad+2*phi10*Dagger[\[Gamma]dyad]-2*Dagger[psi1]*Dagger[\[Gamma]dyad]+phi22*Dagger[\[Kappa]dyad]+2*Dagger[psi1]*Dagger[\[Mu]dyad]-Dagger[psi0]*Dagger[\[Nu]dyad]-phi20*Dagger[\[Pi]dyad]-2*Dagger[psi3]*Dagger[\[Sigma]dyad]-2*phi11*Dagger[\[Tau]dyad]+3*Dagger[psi2]*Dagger[\[Tau]dyad]+pdnp[{1,-tetrad}][phi21]-pdnp[{1,-tetrad}][psi3]-pdnp[{2,-tetrad}][phi10]+pdnp[{2,-tetrad}][Dagger[psi1]]+pdnp[{4,-tetrad}][psi2],pdnp[{4,-tetrad}][Dagger[psi3]]==2*phi21*\[Beta]dyad+2*psi3*\[Beta]dyad+phi02*\[Lambda]dyad-2*phi11*\[Mu]dyad-3*psi2*\[Mu]dyad+2*psi1*\[Nu]dyad-phi22*\[Rho]dyad+psi4*\[Sigma]dyad-2*psi3*\[Tau]dyad-2*phi12*Dagger[\[Beta]dyad]-2*Dagger[psi3]*Dagger[\[Beta]dyad]-phi20*Dagger[\[Lambda]dyad]+2*phi11*Dagger[\[Mu]dyad]+3*Dagger[psi2]*Dagger[\[Mu]dyad]-2*Dagger[psi1]*Dagger[\[Nu]dyad]+phi22*Dagger[\[Rho]dyad]-Dagger[psi4]*Dagger[\[Sigma]dyad]+2*Dagger[psi3]*Dagger[\[Tau]dyad]-pdnp[{2,-tetrad}][psi2]+pdnp[{2,-tetrad}][Dagger[psi2]]+pdnp[{3,-tetrad}][phi21]+pdnp[{3,-tetrad}][psi3]-pdnp[{4,-tetrad}][phi12],pdnp[{4,-tetrad}][Dagger[psi4]]==2*phi22*\[Beta]dyad-2*phi12*\[Mu]dyad+phi02*\[Nu]dyad-phi22*\[Tau]dyad+2*phi22*Dagger[\[Alpha]dyad]-4*Dagger[psi4]*Dagger[\[Beta]dyad]-2*phi12*Dagger[\[Gamma]dyad]+2*Dagger[psi3]*Dagger[\[Gamma]dyad]-2*phi21*Dagger[\[Lambda]dyad]+4*Dagger[psi3]*Dagger[\[Mu]dyad]+2*phi11*Dagger[\[Nu]dyad]-3*Dagger[psi2]*Dagger[\[Nu]dyad]+Dagger[psi4]*Dagger[\[Tau]dyad]-pdnp[{2,-tetrad}][phi12]+pdnp[{2,-tetrad}][Dagger[psi3]]+pdnp[{3,-tetrad}][phi22],pdnp[{3,-tetrad}][Dagger[psi0]]==-2*phi00*\[Alpha]dyad-phi20*\[Kappa]dyad+phi00*\[Pi]dyad+2*phi10*\[Rho]dyad+4*Dagger[psi0]*Dagger[\[Alpha]dyad]-2*phi00*Dagger[\[Beta]dyad]+2*phi10*Dagger[\[Epsilon]dyad]-2*Dagger[psi1]*Dagger[\[Epsilon]dyad]-2*phi11*Dagger[\[Kappa]dyad]+3*Dagger[psi2]*Dagger[\[Kappa]dyad]-Dagger[psi0]*Dagger[\[Pi]dyad]-4*Dagger[psi1]*Dagger[\[Rho]dyad]+2*phi01*Dagger[\[Sigma]dyad]-pdnp[{1,-tetrad}][phi10]+pdnp[{1,-tetrad}][Dagger[psi1]]+pdnp[{4,-tetrad}][phi00],pdnp[{3,-tetrad}][Dagger[psi1]]==-2*phi01*\[Alpha]dyad-2*psi1*\[Alpha]dyad-2*psi3*\[Kappa]dyad-psi0*\[Lambda]dyad+phi00*\[Mu]dyad+2*psi1*\[Pi]dyad+2*phi11*\[Rho]dyad+3*psi2*\[Rho]dyad-phi20*\[Sigma]dyad+2*phi10*Dagger[\[Alpha]dyad]+2*Dagger[psi1]*Dagger[\[Alpha]dyad]+2*Dagger[psi3]*Dagger[\[Kappa]dyad]+Dagger[psi0]*Dagger[\[Lambda]dyad]-phi00*Dagger[\[Mu]dyad]-2*Dagger[psi1]*Dagger[\[Pi]dyad]-2*phi11*Dagger[\[Rho]dyad]-3*Dagger[psi2]*Dagger[\[Rho]dyad]+phi02*Dagger[\[Sigma]dyad]-pdnp[{1,-tetrad}][psi2]+pdnp[{1,-tetrad}][Dagger[psi2]]-pdnp[{3,-tetrad}][phi10]+pdnp[{4,-tetrad}][phi01]+pdnp[{4,-tetrad}][psi1],pdnp[{3,-tetrad}][Dagger[psi2]]==-2*phi01*\[Gamma]dyad+2*psi1*\[Gamma]dyad-phi22*\[Kappa]dyad-2*psi1*\[Mu]dyad+psi0*\[Nu]dyad+phi02*\[Pi]dyad+2*psi3*\[Sigma]dyad+2*phi11*\[Tau]dyad-3*psi2*\[Tau]dyad-2*phi12*Dagger[\[Epsilon]dyad]+2*Dagger[psi3]*Dagger[\[Epsilon]dyad]+Dagger[psi4]*Dagger[\[Kappa]dyad]+2*Dagger[psi1]*Dagger[\[Lambda]dyad]-phi00*Dagger[\[Nu]dyad]+2*phi11*Dagger[\[Pi]dyad]-3*Dagger[psi2]*Dagger[\[Pi]dyad]-2*Dagger[psi3]*Dagger[\[Rho]dyad]+phi02*Dagger[\[Tau]dyad]-pdnp[{1,-tetrad}][phi12]+pdnp[{1,-tetrad}][Dagger[psi3]]+pdnp[{2,-tetrad}][phi01]-pdnp[{2,-tetrad}][psi1]+pdnp[{3,-tetrad}][psi2],pdnp[{3,-tetrad}][Dagger[psi3]]==-2*phi02*\[Gamma]dyad+phi02*\[Mu]dyad-phi22*\[Sigma]dyad+2*phi12*\[Tau]dyad-2*phi12*Dagger[\[Alpha]dyad]-2*Dagger[psi3]*Dagger[\[Alpha]dyad]+2*phi02*Dagger[\[Gamma]dyad]+4*Dagger[psi4]*Dagger[\[Epsilon]dyad]+2*phi11*Dagger[\[Lambda]dyad]+3*Dagger[psi2]*Dagger[\[Lambda]dyad]-2*phi01*Dagger[\[Nu]dyad]-4*Dagger[psi3]*Dagger[\[Pi]dyad]-Dagger[psi4]*Dagger[\[Rho]dyad]+pdnp[{1,-tetrad}][Dagger[psi4]]+pdnp[{2,-tetrad}][phi02]-pdnp[{3,-tetrad}][phi12],pdnp[{4,-tetrad}][lambda]==phi20*\[Beta]dyad-phi21*\[Epsilon]dyad+psi3*\[Epsilon]dyad+(psi4*\[Kappa]dyad)/2+psi1*\[Lambda]dyad-phi10*\[Mu]dyad+phi11*\[Pi]dyad-(3*psi2*\[Pi]dyad)/2-psi3*\[Rho]dyad-phi20*Dagger[\[Alpha]dyad]-(phi22*Dagger[\[Kappa]dyad])/2+(phi20*Dagger[\[Pi]dyad])/2+phi21*Dagger[\[Rho]dyad]-pdnp[{1,-tetrad}][phi21]/2+pdnp[{1,-tetrad}][psi3]/2+pdnp[{3,-tetrad}][phi20]/2-pdnp[{4,-tetrad}][psi2]/2};tmpeqs=RemoveSignFromEq/@tmpeqs;
NPBianchiRules[dyad]^=(tmpeqs/.Equal->Rule);
(* CovDToNPRules *)
CovDToNPRules[dyad]^={(Evaluate[HoldPattern[(cde[xAct`xTensor`Private`pattern[A,Blank[]],xAct`xTensor`Private`pattern[Adg,Blank[]]][xAct`xTensor`Private`pattern[exprsymb,Blank[]]])]/.xAct`xTensor`Private`pattern->Pattern]:>Module[{a},xAct`xCoba`TraceBasisDummy[sigma[{a,tetrad},A,Adg]pdnp[{-a,-tetrad}]@exprsymb]/.TetradSigmaRules[tetrad]])};
];
]]


$NPExtraRules={};
$NPExtraDyadExpansions={};
$DyadCalcInfo=True;


EqToRule1[expr_Equal]:=MakeRule[Evaluate[List@@expr],MetricOn->All,ContractMetrics->True]


CovDsToNP[expr_,dyad_]:=ToCanonical@Expand[Expand[expr/.CovDDyadRules[dyad]]/.CovDToNPRules[dyad]//.$NPExtraRules];


TensorNamesInExpr[expr_]:=Head/@Cases[expr,T_?xTensorQ[___],{0,Infinity}]


NonScalarTensorsInExpr[expr_,spin_]:=Select[TensorNamesInExpr[expr],Length[Select[SlotsOfTensor[#],Or[UpIndex[#]===spin,UpIndex[#]===Dagger@spin]&]]>0&]


NonScalarTensorsInExprNoDyad[expr_,dyad_]:=DeleteCases[NonScalarTensorsInExpr[expr,VBundleOfBasis[dyad]],Alternatives@@Flatten[{#,Dagger@#}&/@SpinorsOfDyad[dyad]]];


TensorsInExprNoDyad[expr_,dyad_]:=DeleteCases[TensorNamesInExpr[expr],Alternatives@@Flatten[{#,Dagger@#}&/@SpinorsOfDyad[dyad]]];


ToNP[expr_,dyad_]:=ToNP2[Expand[expr//.cd1_?CovDQ[cd1inds__][cd2_?CovDQ[cd2inds__][T_?xTensorQ[inds___]]]:>cd1[cd1inds][xAct`SymManipulator`Private`ToCovarD[cd2[cd2inds][T[inds]]]]],dyad];ToNP2[expr_,dyad_]:=Expand[Expand[CovDsToNP[Expand[expr/.Flatten[EqToRule1/@(DyadExpansionEq[#,dyad]&/@NonScalarTensorsInExprNoDyad[expr,dyad])]],dyad]]//.Flatten[NPComponentRules[#,dyad]&/@TensorsInExprNoDyad[expr,dyad]]//.$NPExtraRules];


NPComponentRules[xAct`SymManipulator`CovarD[cde_?CovDQ,TT_?xTensorQ,vbs_List],dyad_]:=NPComponentRules[xAct`SymManipulator`CovarD[cde,TT,vbs],dyad]=Module[{lhs=GiveIndicesToTensor[xAct`SymManipulator`CovarD[cde,TT,vbs]],rhs,spin=VBundleOfBasis@dyad},
(* Make sure the GHPWeights and Tex output are set by expanding into dyad *)
DyadExpansionEq[xAct`SymManipulator`CovarD[cde,TT,vbs],dyad];
If[$DyadCalcInfo,Print["Calculating the ",dyad,"components of ",lhs," in NP form."]];
rhs=xAct`SymManipulator`Private`ExpandCovarD@lhs;
If[Length[Select[SlotsOfTensor[TT],Or[UpIndex[#]===spin,UpIndex[#]===Dagger@spin]&]]>0,rhs=rhs/.EqToRule1@DyadExpansionEq[TT,dyad]/.$NPExtraDyadExpansions];
Expand[DyadComponents[lhs==Expand[ToCanonical[Expand[Expand[rhs]/.CovDDyadRules[dyad]]/.CovDToNPRules[dyad]//.$NPExtraRules]],dyad]/.NPComponentRules[TT,dyad]//.$NPExtraRules]/.Equal->Rule];


NPComponentRules[___]:={};


Options[DyadExpansionEq]={SetTexComponents->True,FormatComponents->True};


DyadExpansionEq[T_?xTensorQ,dyad_,options:OptionsPattern[]]:=DyadExpansionEq[T,dyad]^=Module[{X=GiveIndicesToTensor[T]},
If[$DyadCalcInfo,Print["GHP weights for ",X," components are generated, ",If[OptionValue[FormatComponents],"components are formatted, ",""],If[OptionValue[SetTexComponents],"Tex components are set, ",""],"and it is expanded into ", dyad]];
GHPWeightRules[T,dyad];
If[OptionValue[FormatComponents],FormatComponents[dyad,T]];
If[OptionValue[SetTexComponents],SetTexComponents[dyad,T]];
If[Or[HermitianQ[T]=!=True,AntihermitianQ[T]=!=True],GHPWeightRules[Dagger@T,dyad];If[OptionValue[FormatComponents],FormatComponents[dyad,Dagger@T]];
If[OptionValue[SetTexComponents],SetTexComponents[dyad,Dagger @T]];];
X==ToCanonical[TraceBasisDummy@SeparateBasis[Dagger@dyad]@SeparateBasis[dyad]@X/.BasisToDyadRules[dyad]]]


DyadComponents[expr_,dyad_]:=Union@Flatten[{ToCanonical@ComponentArray[ToBasis[dyad]/@ToBasis[Dagger@dyad]/@expr]}]


GHPWeightOf[num_?IntegerQ ]:={0,0};
GHPWeightOf[_Rational]:={0,0};
GHPWeightOf[_Complex]:={0,0};


GHPWeightOf[expr_Times]:=Apply[Plus,GHPWeightOf/@Level[expr,{1}]];


GHPWeightOf@expr_Plus:=If[Length@Union[GHPWeightOf/@Level[expr,1]]>1,Throw@Message[GHPWeightOf::Error],First@Union[GHPWeightOf/@Level[expr,1]]];
GHPWeightOf::Error="Inconsistent weights on Plus expression";


GHPWeightOf[expr_^(n_?IntegerQ)]:=n GHPWeightOf@expr;


GHPWeightOf[expr_]:={0,0};


getindexrange[k_,vbundle_]:=Module[{n=Length@Flatten[IndicesOfVBundle[vbundle]]},NewIndexIn[vbundle]&/@Range[k-n];Take[Flatten[IndicesOfVBundle[vbundle]],k]]


giveindicestotensor[TT_?xTensorQ,upvbundles_:{}]:=Module[{n=Length@SlotsOfTensor@TT,vbundles=UpIndex/@SlotsOfTensor@TT,gatheredslots,inds,slotrules},gatheredslots=GatherBy[Range@n,vbundles[[#]]&];
inds=Flatten[getindexrange@@@({Length@#,vbundles[[First@#]]}&/@gatheredslots)];
inds=If[MemberQ[upvbundles,VBundleOfIndex[#]],#,DownIndex[#]]&/@inds;
slotrules=Thread@Rule[Flatten@gatheredslots,inds];
TT@@(Range@n/.slotrules)]


GHPWeightRules[SS_?xTensorQ,dyad_]:=Union@Flatten@ToCanonical@ComponentArray[ToBasis[Dagger@dyad]@ToBasis[dyad]@giveindicestotensor@SS]/.TT_?xTensorQ[inds___]:>Module[{dyadinds=Select[{inds},#[[2]]===-dyad&],dyaddginds=Select[{inds},#[[2]]===-Dagger[dyad]&]},UpSet[GHPWeightOf[TT[inds]],{Length@dyadinds-2*(Plus@@(First/@dyadinds)),Length@dyaddginds-2*(Plus@@(First/@dyaddginds))}]];


DefGHPOperator[dyad_,tetrad_,cde_,{a_,A_},symbform_,texform_]:=
With[{omicronA=SpinorsOfDyad[dyad][[1]][A],
iotaA=SpinorsOfDyad[dyad][[2]][A],omicrondgAdg=Dagger[SpinorsOfDyad[dyad][[1]][A]],iotadgAdg=Dagger[SpinorsOfDyad[dyad][[2]][A]],
commutatorexpr1a=-$LambdaSign*$RiemannSign*Lambda[cde][]+$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"11",dyad][]+$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"2",dyad][]-GHPSpinCoefficients[dyad][[1]]*GHPSpinCoefficients[dyad][[5]]+GHPSpinCoefficients[dyad][[4]]*GHPSpinCoefficients[dyad][[8]],
commutatorexpr1b=(Dagger@GHPSpinCoefficients[dyad][[4]]-GHPSpinCoefficients[dyad][[8]]),
commutatorexpr2a=-$LambdaSign*$RiemannSign*Lambda[cde][]-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"11",dyad][]+$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"2",dyad][]-GHPSpinCoefficients[dyad][[2]]*GHPSpinCoefficients[dyad][[6]]+GHPSpinCoefficients[dyad][[3]]*GHPSpinCoefficients[dyad][[7]],commutatorexpr3a=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"1",dyad][]+GHPSpinCoefficients[dyad][[1]]*GHPSpinCoefficients[dyad][[6]]-GHPSpinCoefficients[dyad][[3]]*GHPSpinCoefficients[dyad][[8]],commutatorexpr4a=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"12",dyad][]+GHPSpinCoefficients[dyad][[5]]*GHPSpinCoefficients[dyad][[3]]-GHPSpinCoefficients[dyad][[6]]*GHPSpinCoefficients[dyad][[4]],commutatorexpr4b=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"3",dyad][]+GHPSpinCoefficients[dyad][[5]]*GHPSpinCoefficients[dyad][[2]]-GHPSpinCoefficients[dyad][[7]]*GHPSpinCoefficients[dyad][[4]],commutatorexpr5a=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"10",dyad][]+GHPSpinCoefficients[dyad][[1]]*GHPSpinCoefficients[dyad][[7]]-GHPSpinCoefficients[dyad][[2]]*GHPSpinCoefficients[dyad][[8]],commutatorexpr5b=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"1",dyad][]+GHPSpinCoefficients[dyad][[1]]*GHPSpinCoefficients[dyad][[6]]-GHPSpinCoefficients[dyad][[3]]*GHPSpinCoefficients[dyad][[8]],\[Kappa]dyad=GHPSpinCoefficients[dyad][[1]],
\[Rho]dyad=GHPSpinCoefficients[dyad][[2]],
\[Sigma]dyad=GHPSpinCoefficients[dyad][[3]],
\[Tau]dyad=GHPSpinCoefficients[dyad][[4]],
\[Kappa]pdyad=GHPSpinCoefficients[dyad][[5]],
\[Rho]pdyad=GHPSpinCoefficients[dyad][[6]],
\[Sigma]pdyad=GHPSpinCoefficients[dyad][[7]],
\[Tau]pdyad=GHPSpinCoefficients[dyad][[8]],
lambda=-$LambdaSign*$RiemannSign*Lambda[cde][],
psi0=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"0",dyad][],
psi1=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"1",dyad][],
psi2=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"2",dyad][],psi3=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"3",dyad][],psi4=-$PsiSign*$RiemannSign*SymbolJoin["Psi",cde,"4",dyad][],phi00=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"00",dyad][],
phi01=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"01",dyad][],
phi02=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"02",dyad][],
phi10=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"10",dyad][],
phi11=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"11",dyad][],
phi12=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"12",dyad][],
phi20=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"20",dyad][],
phi21=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"21",dyad][],
phi22=-$PhiSign*$RiemannSign*SymbolJoin["Phi",cde,"22",dyad][],
\[Alpha]dyad=NPSpinCoefficients[dyad][[1]],
\[Beta]dyad=NPSpinCoefficients[dyad][[2]],
\[Gamma]dyad=NPSpinCoefficients[dyad][[3]],
\[Epsilon]dyad=NPSpinCoefficients[dyad][[4]],
pdnp=PDOfBasis@tetrad},With[{theta=SymbolJoin["\[CapitalTheta]",dyad],Adg=DaggerIndex@A,sigmaexprtetrad=(NPSpinCoefficients[dyad][[3]]VectorsOfTetrad[tetrad][[1]][a]+NPSpinCoefficients[dyad][[4]]VectorsOfTetrad[tetrad][[2]][a]-NPSpinCoefficients[dyad][[1]]VectorsOfTetrad[tetrad][[3]][a]-NPSpinCoefficients[dyad][[2]]VectorsOfTetrad[tetrad][[4]][a]),sigmaexprdyad=(NPSpinCoefficients[dyad][[3]]*omicronA*omicrondgAdg+NPSpinCoefficients[dyad][[4]]*iotaA*iotadgAdg-NPSpinCoefficients[dyad][[1]]*omicronA*iotadgAdg-NPSpinCoefficients[dyad][[2]]*iotaA*omicrondgAdg),sigmaexprdyaddg=Dagger[(NPSpinCoefficients[dyad][[3]]*omicronA*omicrondgAdg+NPSpinCoefficients[dyad][[4]]*iotaA*iotadgAdg-NPSpinCoefficients[dyad][[1]]*omicronA*iotadgAdg-NPSpinCoefficients[dyad][[2]]*iotaA*omicrondgAdg)],
tangentpmQ=xAct`xTensor`Private`VBundleIndexPMQ[VBundleOfIndex[a]],spinpmQ=xAct`xTensor`Private`VBundleIndexPMQ[VBundleOfIndex[A]],spindgpmQ=xAct`xTensor`Private`VBundleIndexPMQ[VBundleOfIndex@DaggerIndex[A]]},
CovDQ[theta]^=True;
 xAct`xTensor`Private`MakeLinearDerivative[Evaluate[{theta[\!\(\*
TagBox[
StyleBox[
RowBox[{"xAct`xTensor`Private`pattern", "[", 
RowBox[{"A", ",", 
RowBox[{"BlankSequence", "[", "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)],theta[A]}/.xAct`xTensor`Private`pattern->Pattern],True];
SymmetryGroupOfCovD[theta]^=StrongGenSet[{},GenSet[]];
ManifoldOfCovD[theta]^=ManifoldOfCovD[cde];
VBundlesOfCovD[theta]^=VBundlesOfCovD[cde];
Dagger[Evaluate[(theta[\!\(\*
TagBox[
StyleBox[
RowBox[{"xAct`xTensor`Private`pattern", "[", 
RowBox[{"a", ",", 
RowBox[{"Blank", "[", "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)][expr_]/.xAct`xTensor`Private`pattern->Pattern)]]^:=theta[DaggerIndex[a]][Dagger[expr]];
SymbolOfCovD[theta]^={"?",symbform@"\[CapitalTheta]"};
(* GHP weights *)
GHPWeightOf@theta[{1,-tetrad}]^={1,1};
GHPWeightOf@theta[{2,-tetrad}]^={-1,-1};
GHPWeightOf@theta[{3,-tetrad}]^={1,-1};
GHPWeightOf@theta[{4,-tetrad}]^={-1,1};
GHPWeightOf[theta[ind_?CIndexQ]@sc_]:={Part[GHPWeightOf@theta[ind],1]+Part[GHPWeightOf@sc,1],Part[GHPWeightOf@theta[ind],2]+Part[GHPWeightOf@sc,2]};
(* Formating *)
xTensorFormStop[CovD];
FormatBasis[theta[{1,-tetrad}],symbform@"\[Thorn]"];
FormatBasis[theta[{2,-tetrad}],symbform@"\[Thorn]"<>"'"];
FormatBasis[theta[{3,-tetrad}],symbform@"\[Eth]"];
FormatBasis[theta[{4,-tetrad}],symbform@"\[Eth]"<>"'"];
xTensorFormStart[CovD];
FormatTexBasis[theta[{1,-tetrad}],StringJoin[texform@"\\tho"," "]];
FormatTexBasis[theta[{2,-tetrad}],StringJoin[texform@"\\tho","' "]];
FormatTexBasis[theta[{3,-tetrad}],StringJoin[texform@"\\edt"," "]];
FormatTexBasis[theta[{4,-tetrad}],StringJoin[texform@"\\edt","' "]];
$TexInitLatexExtraCode=DeleteDuplicates@Join[$TexInitLatexExtraCode,{"\\DeclareMathOperator{\\tho}{\\text{\\textthorn}}","\\DeclareMathOperator{\\edt}{\\eth}"}];
$TexInitLatexPackages=DeleteDuplicates@Append[$TexInitLatexPackages,"{tipa}"];
(* Conversion *)
CovDToGHPRules[dyad]^={Evaluate[(cde[PatternTest[xAct`xTensor`Private`pattern[a,Blank[]],tangentpmQ]][expr_]/.xAct`xTensor`Private`pattern->Pattern)]:> theta[a][expr]+(First@GHPWeightOf[expr]*sigmaexprtetrad+Last@GHPWeightOf[expr]*Dagger[sigmaexprtetrad])*expr,Evaluate[(cde[PatternTest[xAct`xTensor`Private`pattern[A,Blank[]],spinpmQ],PatternTest[xAct`xTensor`Private`pattern[Adg,Blank[]],spindgpmQ]][expr_]/.xAct`xTensor`Private`pattern->Pattern)]:> iotaA*iotadgAdg (theta[{1,-tetrad}][expr])+omicronA*omicrondgAdg(theta[{2,-tetrad}][expr])-iotaA*omicrondgAdg(theta[{3,-tetrad}][expr])-omicronA*iotadgAdg (theta[{4,-tetrad}][expr])+(First@GHPWeightOf[expr]*sigmaexprdyad+Last@GHPWeightOf[expr]*sigmaexprdyaddg)*expr};
(* Commutators *)
CommuteGHPOp[theta[{1, -tetrad}],theta[{2, -tetrad}],s___]:=theta[{1, -tetrad}][theta[{2, -tetrad}][expr_]] :> (theta[{2, -tetrad}][theta[{1, -tetrad}][expr]]+expr*(First@GHPWeightOf[expr]*commutatorexpr1a +Last@GHPWeightOf[expr]*Dagger@commutatorexpr1a )+commutatorexpr1b*theta[{3, -tetrad}][expr] +Dagger@commutatorexpr1b*theta[{4, -tetrad}][expr])/;Or[Length@List@s==0 ,expr==s];
CommuteGHPOp[theta[{2,-tetrad}],theta[{1,-tetrad}],s___]:=theta[{2,-tetrad}][theta[{1,-tetrad}][expr_]]:>(theta[{1,-tetrad}][theta[{2,-tetrad}][expr]]-expr*(First@GHPWeightOf[expr]*commutatorexpr1a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr1a)-commutatorexpr1b*theta[{3,-tetrad}][expr]-Dagger@commutatorexpr1b*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{3,-tetrad}],theta[{4,-tetrad}],s___]:=theta[{3,-tetrad}][theta[{4,-tetrad}][expr_]]:>(theta[{4,-tetrad}][theta[{3,-tetrad}][expr]]+expr*(-First@GHPWeightOf[expr]*commutatorexpr2a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr2a)+(-\[Rho]pdyad+Dagger@\[Rho]pdyad)*theta[{1,-tetrad}][expr]+(\[Rho]dyad-Dagger@\[Rho]dyad)*theta[{2,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{4,-tetrad}],theta[{3,-tetrad}],s___]:=theta[{4,-tetrad}][theta[{3,-tetrad}][expr_]]:>(theta[{3,-tetrad}][theta[{4,-tetrad}][expr]]-expr*(-First@GHPWeightOf[expr]*commutatorexpr2a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr2a)-(-\[Rho]pdyad+Dagger@\[Rho]pdyad)*theta[{1,-tetrad}][expr]-(\[Rho]dyad-Dagger@\[Rho]dyad)*theta[{2,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{1,-tetrad}],theta[{3,-tetrad}],s___]:=theta[{1,-tetrad}][theta[{3,-tetrad}][expr_]]:>(theta[{3,-tetrad}][theta[{1,-tetrad}][expr]]-(expr*(First@GHPWeightOf[expr]*commutatorexpr3a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr5a))-Dagger@\[Tau]pdyad*theta[{1,-tetrad}][expr]-\[Kappa]dyad*theta[{2,-tetrad}][expr]+Dagger@\[Rho]dyad*theta[{3,-tetrad}][expr]+\[Sigma]dyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{3,-tetrad}],theta[{1,-tetrad}],s___]:=theta[{3,-tetrad}][theta[{1,-tetrad}][expr_]]:>(theta[{1,-tetrad}][theta[{3,-tetrad}][expr]]+(expr*(First@GHPWeightOf[expr]*commutatorexpr3a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr5a))+Dagger@\[Tau]pdyad*theta[{1,-tetrad}][expr]+\[Kappa]dyad*theta[{2,-tetrad}][expr]-Dagger@\[Rho]dyad*theta[{3,-tetrad}][expr]-\[Sigma]dyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{2,-tetrad}],theta[{3,-tetrad}],s___]:=theta[{2,-tetrad}][theta[{3,-tetrad}][expr_]]:>
(theta[{3,-tetrad}][theta[{2,-tetrad}][expr]]+expr*(First@GHPWeightOf[expr]*commutatorexpr4a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr4b)-Dagger@\[Kappa]pdyad*theta[{1,-tetrad}][expr]-\[Tau]dyad*theta[{2,-tetrad}][expr]+\[Rho]pdyad*theta[{3,-tetrad}][expr]+Dagger@\[Sigma]pdyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{3,-tetrad}],theta[{2,-tetrad}],s___]:=theta[{3,-tetrad}][theta[{2,-tetrad}][expr_]]:>
(theta[{2,-tetrad}][theta[{3,-tetrad}][expr]]-expr*(First@GHPWeightOf[expr]*commutatorexpr4a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr4b)+Dagger@\[Kappa]pdyad*theta[{1,-tetrad}][expr]+\[Tau]dyad*theta[{2,-tetrad}][expr]-\[Rho]pdyad*theta[{3,-tetrad}][expr]-Dagger@\[Sigma]pdyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{1,-tetrad}],theta[{4,-tetrad}],s___]:=theta[{1,-tetrad}][theta[{4,-tetrad}][expr_]]:>
(theta[{4,-tetrad}][theta[{1,-tetrad}][expr]]-expr*(First@GHPWeightOf[expr]*commutatorexpr5a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr5b)-\[Tau]pdyad*theta[{1,-tetrad}][expr]-Dagger@\[Kappa]dyad*theta[{2,-tetrad}][expr]+Dagger@\[Sigma]dyad*theta[{3,-tetrad}][expr]+\[Rho]dyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{4,-tetrad}],theta[{1,-tetrad}],s___]:=theta[{4,-tetrad}][theta[{1,-tetrad}][expr_]]:>
(theta[{1,-tetrad}][theta[{4,-tetrad}][expr]]+expr*(First@GHPWeightOf[expr]*commutatorexpr5a+Last@GHPWeightOf[expr]*Dagger@commutatorexpr5b)+\[Tau]pdyad*theta[{1,-tetrad}][expr]+Dagger@\[Kappa]dyad*theta[{2,-tetrad}][expr]-Dagger@\[Sigma]dyad*theta[{3,-tetrad}][expr]-\[Rho]dyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{2,-tetrad}],theta[{4,-tetrad}],s___]:=theta[{2,-tetrad}][theta[{4,-tetrad}][expr_]]:>
(theta[{4,-tetrad}][theta[{2,-tetrad}][expr]]+expr*(First@GHPWeightOf[expr]*commutatorexpr4b+Last@GHPWeightOf[expr]*Dagger@commutatorexpr4a)-\[Kappa]pdyad*theta[{1,-tetrad}][expr]-Dagger@\[Tau]dyad*theta[{2,-tetrad}][expr]+\[Sigma]pdyad*theta[{3,-tetrad}][expr]+Dagger@\[Rho]pdyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[{4,-tetrad}],theta[{2,-tetrad}],s___]:=theta[{4,-tetrad}][theta[{2,-tetrad}][expr_]]:>
(theta[{2,-tetrad}][theta[{4,-tetrad}][expr]]-expr*(First@GHPWeightOf[expr]*commutatorexpr4b+Last@GHPWeightOf[expr]*Dagger@commutatorexpr4a)+\[Kappa]pdyad*theta[{1,-tetrad}][expr]+Dagger@\[Tau]dyad*theta[{2,-tetrad}][expr]-\[Sigma]pdyad*theta[{3,-tetrad}][expr]-Dagger@\[Rho]pdyad*theta[{4,-tetrad}][expr])/;Or[Length@List@s==0,expr==s];
CommuteGHPOp[theta[aa_][theta[bb_][expr_]]]:=CommuteGHPOp[theta[aa],theta[bb],expr];
GHPCommutatorsForward[dyad]^=CommuteGHPOp[theta[{#[[1]],-tetrad}],theta[{#[[2]],-tetrad}]]&/@Select[Join@@Outer[List,Range@4,Range@4],#[[1]]>#[[2]]&];
GHPCommutatorsBackward[dyad]^=CommuteGHPOp[theta[{#[[2]],-tetrad}],theta[{#[[1]],-tetrad}]]&/@Select[Join@@Outer[List,Range@4,Range@4],#[[1]]>#[[2]]&];
(* Ordering *)
GHPOrderOfTerm[theta[{i_,-tetrad}][expr_]]:=1+GHPOrderOfTerm[expr];
Module[
(* GHP Ricci equations *){tmpeqs=Evaluate[{theta[{2,-tetrad}][\[Kappa]dyad]==-phi01-psi1-\[Rho]dyad*\[Tau]dyad+\[Sigma]dyad*\[Tau]pdyad-\[Sigma]dyad*Dagger[\[Tau]dyad]+\[Rho]dyad*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][\[Tau]dyad],theta[{2,-tetrad}][\[Tau]pdyad]==phi21+psi3-\[Sigma]pdyad*\[Tau]dyad+\[Rho]pdyad*\[Tau]pdyad-\[Rho]pdyad*Dagger[\[Tau]dyad]+\[Sigma]pdyad*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][\[Kappa]pdyad],theta[{3,-tetrad}][\[Kappa]dyad]==-psi0-\[Sigma]dyad*(\[Rho]dyad+Dagger[\[Rho]dyad])+\[Kappa]dyad*(\[Tau]dyad+Dagger[\[Tau]pdyad])+theta[{1,-tetrad}][\[Sigma]dyad],theta[{3,-tetrad}][\[Kappa]pdyad]==-phi22-\[Rho]pdyad^2+\[Kappa]pdyad*\[Tau]dyad+\[Tau]pdyad*Dagger[\[Kappa]pdyad]-\[Sigma]pdyad*Dagger[\[Sigma]pdyad]+theta[{2,-tetrad}][\[Rho]pdyad],theta[{3,-tetrad}][\[Tau]dyad]==phi02-\[Rho]pdyad*\[Sigma]dyad+\[Tau]dyad^2+\[Kappa]dyad*Dagger[\[Kappa]pdyad]-\[Rho]dyad*Dagger[\[Sigma]pdyad]+theta[{2,-tetrad}][\[Sigma]dyad],theta[{3,-tetrad}][\[Tau]pdyad]==2*lambda+psi2+\[Kappa]dyad*\[Kappa]pdyad-\[Sigma]dyad*\[Sigma]pdyad-\[Rho]pdyad*Dagger[\[Rho]dyad]+\[Tau]pdyad*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][\[Rho]pdyad],theta[{4,-tetrad}][\[Kappa]dyad]==-phi00-\[Rho]dyad^2+\[Kappa]dyad*\[Tau]pdyad+\[Tau]dyad*Dagger[\[Kappa]dyad]-\[Sigma]dyad*Dagger[\[Sigma]dyad]+theta[{1,-tetrad}][\[Rho]dyad],theta[{4,-tetrad}][\[Kappa]pdyad]==-psi4-\[Sigma]pdyad*(\[Rho]pdyad+Dagger[\[Rho]pdyad])+\[Kappa]pdyad*(\[Tau]pdyad+Dagger[\[Tau]dyad])+theta[{2,-tetrad}][\[Sigma]pdyad],theta[{4,-tetrad}][\[Rho]pdyad]==phi21-psi3-\[Kappa]pdyad*\[Rho]dyad+\[Rho]pdyad*\[Tau]pdyad+\[Kappa]pdyad*Dagger[\[Rho]dyad]-\[Tau]pdyad*Dagger[\[Rho]pdyad]+theta[{3,-tetrad}][\[Sigma]pdyad],theta[{4,-tetrad}][\[Sigma]dyad]==-phi01+psi1+\[Kappa]dyad*\[Rho]pdyad-\[Rho]dyad*\[Tau]dyad+\[Tau]dyad*Dagger[\[Rho]dyad]-\[Kappa]dyad*Dagger[\[Rho]pdyad]+theta[{3,-tetrad}][\[Rho]dyad],theta[{4,-tetrad}][\[Tau]dyad]==2*lambda+psi2+\[Kappa]dyad*\[Kappa]pdyad-\[Sigma]dyad*\[Sigma]pdyad-\[Rho]dyad*Dagger[\[Rho]pdyad]+\[Tau]dyad*Dagger[\[Tau]dyad]+theta[{2,-tetrad}][\[Rho]dyad],theta[{4,-tetrad}][\[Tau]pdyad]==phi20-\[Rho]dyad*\[Sigma]pdyad+\[Tau]pdyad^2+\[Kappa]pdyad*Dagger[\[Kappa]dyad]-\[Rho]pdyad*Dagger[\[Sigma]dyad]+theta[{1,-tetrad}][\[Sigma]pdyad]}]},
GHPRicciRules[dyad]^=(Join[tmpeqs,Dagger/@tmpeqs]/.Equal->Rule);
(* GHP Bianchi equations *)
tmpeqs={theta[{3,-tetrad}][psi1]==2*phi12*\[Kappa]dyad-psi0*\[Rho]pdyad-2*phi11*\[Sigma]dyad-3*psi2*\[Sigma]dyad+4*psi1*\[Tau]dyad-phi02*Dagger[\[Rho]dyad]-phi00*Dagger[\[Sigma]pdyad]+2*phi01*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][phi02]+theta[{2,-tetrad}][psi0]-theta[{3,-tetrad}][phi01],theta[{3,-tetrad}][Dagger[psi1]]==-(phi00*\[Rho]pdyad)-phi20*\[Sigma]dyad+2*phi10*\[Tau]dyad+2*Dagger[psi3]*Dagger[\[Kappa]dyad]-2*phi11*Dagger[\[Rho]dyad]-3*Dagger[psi2]*Dagger[\[Rho]dyad]-Dagger[psi0]*Dagger[\[Sigma]pdyad]+2*phi01*Dagger[\[Tau]dyad]+2*Dagger[psi1]*Dagger[\[Tau]pdyad]+2*theta[{1,-tetrad}][lambda]+theta[{1,-tetrad}][Dagger[psi2]]+theta[{2,-tetrad}][phi00]-theta[{3,-tetrad}][phi10],theta[{3,-tetrad}][psi2]==phi22*\[Kappa]dyad+psi0*\[Kappa]pdyad-phi01*\[Rho]pdyad-2*psi1*\[Rho]pdyad-phi21*\[Sigma]dyad-2*psi3*\[Sigma]dyad+3*psi2*\[Tau]dyad+phi02*\[Tau]pdyad-phi12*Dagger[\[Rho]dyad]-phi10*Dagger[\[Sigma]pdyad]+2*phi11*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][phi12]+theta[{2,-tetrad}][psi1]+theta[{3,-tetrad}][lambda]-theta[{3,-tetrad}][phi11],theta[{3,-tetrad}][Dagger[psi2]]==-(phi01*\[Rho]pdyad)-phi21*\[Sigma]dyad+2*phi11*\[Tau]dyad+Dagger[psi4]*Dagger[\[Kappa]dyad]+phi00*Dagger[\[Kappa]pdyad]-phi12*Dagger[\[Rho]dyad]-2*Dagger[psi3]*Dagger[\[Rho]dyad]-phi10*Dagger[\[Sigma]pdyad]-2*Dagger[psi1]*Dagger[\[Sigma]pdyad]+phi02*Dagger[\[Tau]dyad]+3*Dagger[psi2]*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][Dagger[psi3]]+theta[{2,-tetrad}][phi01]+theta[{3,-tetrad}][lambda]-theta[{3,-tetrad}][phi11],theta[{3,-tetrad}][psi3]==2*psi1*\[Kappa]pdyad-2*phi11*\[Rho]pdyad-3*psi2*\[Rho]pdyad-psi4*\[Sigma]dyad+2*psi3*\[Tau]dyad+2*phi12*\[Tau]pdyad-phi22*Dagger[\[Rho]dyad]-phi20*Dagger[\[Sigma]pdyad]+2*phi21*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][phi22]+2*theta[{2,-tetrad}][lambda]+theta[{2,-tetrad}][psi2]-theta[{3,-tetrad}][phi21],theta[{3,-tetrad}][Dagger[psi3]]==-(phi02*\[Rho]pdyad)-phi22*\[Sigma]dyad+2*phi12*\[Tau]dyad+2*phi01*Dagger[\[Kappa]pdyad]-Dagger[psi4]*Dagger[\[Rho]dyad]-2*phi11*Dagger[\[Sigma]pdyad]-3*Dagger[psi2]*Dagger[\[Sigma]pdyad]+4*Dagger[psi3]*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][Dagger[psi4]]+theta[{2,-tetrad}][phi02]-theta[{3,-tetrad}][phi12],theta[{4,-tetrad}][phi00]==phi20*\[Kappa]dyad-2*phi10*\[Rho]dyad+phi00*\[Tau]pdyad+2*phi11*Dagger[\[Kappa]dyad]-3*Dagger[psi2]*Dagger[\[Kappa]dyad]+4*Dagger[psi1]*Dagger[\[Rho]dyad]-2*phi01*Dagger[\[Sigma]dyad]-Dagger[psi0]*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][phi10]-theta[{1,-tetrad}][Dagger[psi1]]+theta[{3,-tetrad}][Dagger[psi0]],theta[{4,-tetrad}][phi01]==phi21*\[Kappa]dyad-2*phi11*\[Rho]dyad-phi00*\[Rho]pdyad-phi20*\[Sigma]dyad+2*phi10*\[Tau]dyad+phi01*\[Tau]pdyad+phi12*Dagger[\[Kappa]dyad]-2*phi11*Dagger[\[Rho]dyad]-phi00*Dagger[\[Rho]pdyad]-phi02*Dagger[\[Sigma]dyad]+2*phi01*Dagger[\[Tau]dyad]+phi10*Dagger[\[Tau]pdyad]+3*theta[{1,-tetrad}][lambda]+theta[{1,-tetrad}][phi11]+theta[{2,-tetrad}][phi00]-theta[{3,-tetrad}][phi10],theta[{4,-tetrad}][phi02]==phi22*\[Kappa]dyad-2*phi12*\[Rho]dyad-phi01*\[Rho]pdyad-phi21*\[Sigma]dyad+2*phi11*\[Tau]dyad+phi02*\[Tau]pdyad+phi00*Dagger[\[Kappa]pdyad]-phi12*Dagger[\[Rho]dyad]-2*phi01*Dagger[\[Rho]pdyad]-phi10*Dagger[\[Sigma]pdyad]+phi02*Dagger[\[Tau]dyad]+2*phi11*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][phi12]+theta[{2,-tetrad}][phi01]+3*theta[{3,-tetrad}][lambda]-theta[{3,-tetrad}][phi11],theta[{4,-tetrad}][phi11]==phi00*\[Kappa]pdyad-phi21*\[Rho]dyad-2*phi10*\[Rho]pdyad-phi01*\[Sigma]pdyad+phi20*\[Tau]dyad+2*phi11*\[Tau]pdyad+phi22*Dagger[\[Kappa]dyad]-2*phi21*Dagger[\[Rho]dyad]-phi10*Dagger[\[Rho]pdyad]-phi12*Dagger[\[Sigma]dyad]+2*phi11*Dagger[\[Tau]dyad]+phi20*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][phi21]+theta[{2,-tetrad}][phi10]-theta[{3,-tetrad}][phi20]+3*theta[{4,-tetrad}][lambda],theta[{4,-tetrad}][phi12]==phi01*\[Kappa]pdyad-phi22*\[Rho]dyad-2*phi11*\[Rho]pdyad-phi02*\[Sigma]pdyad+phi21*\[Tau]dyad+2*phi12*\[Tau]pdyad+phi10*Dagger[\[Kappa]pdyad]-phi22*Dagger[\[Rho]dyad]-2*phi11*Dagger[\[Rho]pdyad]-phi20*Dagger[\[Sigma]pdyad]+phi12*Dagger[\[Tau]dyad]+2*phi21*Dagger[\[Tau]pdyad]+theta[{1,-tetrad}][phi22]+3*theta[{2,-tetrad}][lambda]+theta[{2,-tetrad}][phi11]-theta[{3,-tetrad}][phi21],theta[{4,-tetrad}][phi22]==2*phi11*\[Kappa]pdyad-3*psi2*\[Kappa]pdyad+4*psi3*\[Rho]pdyad-2*phi12*\[Sigma]pdyad-psi4*\[Tau]dyad+phi20*Dagger[\[Kappa]pdyad]-2*phi21*Dagger[\[Rho]pdyad]+phi22*Dagger[\[Tau]dyad]+theta[{2,-tetrad}][phi21]-theta[{2,-tetrad}][psi3]+theta[{3,-tetrad}][psi4],theta[{4,-tetrad}][psi0]==-2*phi11*\[Kappa]dyad+3*psi2*\[Kappa]dyad-4*psi1*\[Rho]dyad+2*phi10*\[Sigma]dyad+psi0*\[Tau]pdyad-phi02*Dagger[\[Kappa]dyad]+2*phi01*Dagger[\[Rho]dyad]-phi00*Dagger[\[Tau]pdyad]-theta[{1,-tetrad}][phi01]+theta[{1,-tetrad}][psi1]+theta[{3,-tetrad}][phi00],theta[{4,-tetrad}][psi1]==-(phi21*\[Kappa]dyad)+2*psi3*\[Kappa]dyad-3*psi2*\[Rho]dyad+phi00*\[Rho]pdyad+phi20*\[Sigma]dyad-psi0*\[Sigma]pdyad-phi01*\[Tau]pdyad+2*psi1*\[Tau]pdyad-phi12*Dagger[\[Kappa]dyad]+2*phi11*Dagger[\[Rho]dyad]-phi10*Dagger[\[Tau]pdyad]-theta[{1,-tetrad}][lambda]-theta[{1,-tetrad}][phi11]+theta[{1,-tetrad}][psi2]+theta[{3,-tetrad}][phi10],theta[{4,-tetrad}][Dagger[psi1]]==-(phi20*\[Rho]dyad)-phi00*\[Sigma]pdyad+2*phi10*\[Tau]pdyad+2*phi21*Dagger[\[Kappa]dyad]-Dagger[psi0]*Dagger[\[Rho]pdyad]-2*phi11*Dagger[\[Sigma]dyad]-3*Dagger[psi2]*Dagger[\[Sigma]dyad]+4*Dagger[psi1]*Dagger[\[Tau]dyad]+theta[{1,-tetrad}][phi20]+theta[{2,-tetrad}][Dagger[psi0]]-theta[{4,-tetrad}][phi10],theta[{4,-tetrad}][psi2]==psi4*\[Kappa]dyad-2*psi3*\[Rho]dyad+2*phi10*\[Rho]pdyad-2*psi1*\[Sigma]pdyad-2*phi11*\[Tau]pdyad+3*psi2*\[Tau]pdyad-phi22*Dagger[\[Kappa]dyad]+2*phi21*Dagger[\[Rho]dyad]-phi20*Dagger[\[Tau]pdyad]-theta[{1,-tetrad}][phi21]+theta[{1,-tetrad}][psi3]+theta[{3,-tetrad}][phi20]-2*theta[{4,-tetrad}][lambda],theta[{4,-tetrad}][Dagger[psi2]]==-(phi00*\[Kappa]pdyad)+2*phi10*\[Rho]pdyad-phi20*\[Tau]dyad+Dagger[psi0]*Dagger[\[Kappa]pdyad]+2*phi21*Dagger[\[Rho]dyad]-2*Dagger[psi1]*Dagger[\[Rho]pdyad]-2*Dagger[psi3]*Dagger[\[Sigma]dyad]-2*phi11*Dagger[\[Tau]dyad]+3*Dagger[psi2]*Dagger[\[Tau]dyad]-theta[{2,-tetrad}][phi10]+theta[{2,-tetrad}][Dagger[psi1]]+theta[{3,-tetrad}][phi20]-2*theta[{4,-tetrad}][lambda],theta[{4,-tetrad}][psi3]==2*phi10*\[Kappa]pdyad-psi4*\[Rho]dyad-2*phi11*\[Sigma]pdyad-3*psi2*\[Sigma]pdyad+4*psi3*\[Tau]pdyad-phi20*Dagger[\[Rho]pdyad]-phi22*Dagger[\[Sigma]dyad]+2*phi21*Dagger[\[Tau]dyad]+theta[{1,-tetrad}][psi4]+theta[{2,-tetrad}][phi20]-theta[{4,-tetrad}][phi21],theta[{4,-tetrad}][Dagger[psi3]]==-(phi01*\[Kappa]pdyad)+2*phi11*\[Rho]pdyad-phi21*\[Tau]dyad-phi10*Dagger[\[Kappa]pdyad]+2*Dagger[psi1]*Dagger[\[Kappa]pdyad]+phi22*Dagger[\[Rho]dyad]-3*Dagger[psi2]*Dagger[\[Rho]pdyad]-Dagger[psi4]*Dagger[\[Sigma]dyad]+phi20*Dagger[\[Sigma]pdyad]-phi12*Dagger[\[Tau]dyad]+2*Dagger[psi3]*Dagger[\[Tau]dyad]-theta[{2,-tetrad}][lambda]-theta[{2,-tetrad}][phi11]+theta[{2,-tetrad}][Dagger[psi2]]+theta[{3,-tetrad}][phi21],theta[{4,-tetrad}][Dagger[psi4]]==-(phi02*\[Kappa]pdyad)+2*phi12*\[Rho]pdyad-phi22*\[Tau]dyad-2*phi11*Dagger[\[Kappa]pdyad]+3*Dagger[psi2]*Dagger[\[Kappa]pdyad]-4*Dagger[psi3]*Dagger[\[Rho]pdyad]+2*phi21*Dagger[\[Sigma]pdyad]+Dagger[psi4]*Dagger[\[Tau]dyad]-theta[{2,-tetrad}][phi12]+theta[{2,-tetrad}][Dagger[psi3]]+theta[{3,-tetrad}][phi22]};
GHPBianchiRules[dyad]^=(tmpeqs/.Equal->Rule);
(* NP \[UndirectedEdge] GHP conversion *)
GHPToNPRules[dyad]^={theta[{1,-tetrad}][\[Eta]w_]:>(-(First[GHPWeightOf[\[Eta]w]]*\[Epsilon]dyad)-Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Epsilon]dyad])*\[Eta]w+pdnp[{1,-tetrad}][\[Eta]w],
theta[{2,-tetrad}][\[Eta]w_]:>(-(First[GHPWeightOf[\[Eta]w]]*\[Gamma]dyad)-Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Gamma]dyad])*\[Eta]w+pdnp[{2,-tetrad}][\[Eta]w],
theta[{3,-tetrad}][\[Eta]w_]:>(-(Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Alpha]dyad])-First[GHPWeightOf[\[Eta]w]]*\[Beta]dyad)*\[Eta]w+pdnp[{3,-tetrad}][\[Eta]w],
theta[{4,-tetrad}][\[Eta]w_]:>(-(First[GHPWeightOf[\[Eta]w]]*\[Alpha]dyad)-Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Beta]dyad])*\[Eta]w+pdnp[{4,-tetrad}][\[Eta]w]};
NPToGHPRules[dyad]^={pdnp[{1,-tetrad}][\[Eta]w_]:>First[GHPWeightOf[\[Eta]w]]*\[Epsilon]dyad*\[Eta]w+Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Epsilon]dyad]*\[Eta]w+theta[{1,-tetrad}][\[Eta]w],
pdnp[{2,-tetrad}][\[Eta]w_]:>First[GHPWeightOf[\[Eta]w]]*\[Gamma]dyad*\[Eta]w+Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Gamma]dyad]*\[Eta]w+theta[{2,-tetrad}][\[Eta]w],
pdnp[{3,-tetrad}][\[Eta]w_]:>Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Alpha]dyad]*\[Eta]w+First[GHPWeightOf[\[Eta]w]]*\[Beta]dyad*\[Eta]w+theta[{3,-tetrad}][\[Eta]w],
pdnp[{4,-tetrad}][\[Eta]w_]:>First[GHPWeightOf[\[Eta]w]]*\[Alpha]dyad*\[Eta]w+Last[GHPWeightOf[\[Eta]w]]*Dagger[\[Beta]dyad]*\[Eta]w+theta[{4,-tetrad}][\[Eta]w]};
];
]]


$GHPExtraRules={};
$GHPExtraDyadExpansions={};


CovDsToGHP[expr_,dyad_]:=ToCanonical@Expand[Expand[expr/.CovDDyadRules[dyad]/.NPToGHPSpinCoeffs[dyad]/.CovDDyadRules[dyad]/.CovDNPSpinCoeffRules[dyad]/.NPToGHPSpinCoeffs[dyad]]/.CovDToGHPRules[dyad]//.$GHPExtraRules];


ToGHP[expr_,dyad_]:=ToGHP2[Expand[expr//.cd1_?CovDQ[cd1inds__][cd2_?CovDQ[cd2inds__][T_?xTensorQ[inds___]]]:>cd1[cd1inds][xAct`SymManipulator`Private`ToCovarD[cd2[cd2inds][T[inds]]]]],dyad];ToGHP2[expr_,dyad_]:=Expand[Expand[CovDsToGHP[Expand[expr/.Flatten[EqToRule1/@(DyadExpansionEq[#,dyad]&/@NonScalarTensorsInExprNoDyad[expr,dyad])]],dyad]]//.Flatten[GHPComponentRules[#,dyad]&/@TensorsInExprNoDyad[expr,dyad]]//.$GHPExtraRules];


GHPComponentRules[xAct`SymManipulator`CovarD[cde_?CovDQ,TT_?xTensorQ,vbs_List],dyad_]:=GHPComponentRules[xAct`SymManipulator`CovarD[cde,TT,vbs],dyad]=Module[{lhs=GiveIndicesToTensor[xAct`SymManipulator`CovarD[cde,TT,vbs]],rhs,spin=VBundleOfBasis@dyad},
(* Make sure the GHPWeights and Tex output are set by expanding into dyad *)
DyadExpansionEq[xAct`SymManipulator`CovarD[cde,TT,vbs],dyad];
If[$DyadCalcInfo,Print["Calculating the ",dyad,"components of ",lhs," in GHP form."]];
rhs=xAct`SymManipulator`Private`ExpandCovarD@lhs;
If[Length[Select[SlotsOfTensor[TT],Or[UpIndex[#]===spin,UpIndex[#]===Dagger@spin]&]]>0,rhs=rhs/.EqToRule1@DyadExpansionEq[TT,dyad]/.$GHPExtraDyadExpansions];
Expand[DyadComponents[lhs==Expand[ToCanonical[Expand[Expand[rhs]/.CovDDyadRules[dyad]]/.CovDToGHPRules[dyad]/.NPToGHPSpinCoeffs[dyad]//.$GHPExtraRules]],dyad]/.GHPComponentRules[TT,dyad]//.$GHPExtraRules]/.Equal->Rule];


GHPComponentRules[(op_?FundSpinOpQ)[TT_?xTensorQ,extr___],dyad_]:=GHPComponentRules[op[TT,extr],dyad]=Module[{lhs=GiveIndicesToTensor[op[TT,extr]],rhs,spin=VBundleOfBasis@dyad},
(* Make sure the GHPWeights and Tex output are set by expanding into dyad *)
DyadExpansionEq[op[TT,extr],dyad];
If[$DyadCalcInfo,Print["Calculating the ",dyad,"components of ",lhs," in GHP form."]];
rhs=GHPFundSpinopExpandFunction@lhs;
If[Length[Select[SlotsOfTensor[TT],Or[UpIndex[#]===spin,UpIndex[#]===Dagger@spin]&]]>0,rhs=rhs/.EqToRule1@DyadExpansionEq[TT,dyad]/.$GHPExtraDyadExpansions];
Expand[DyadComponents[lhs==Expand[ToCanonical[Expand[Expand[rhs]/.CovDDyadRules[dyad]]/.CovDToGHPRules[dyad]/.NPToGHPSpinCoeffs[dyad]//.$GHPExtraRules]],dyad]/.GHPComponentRules[TT,dyad]//.$GHPExtraRules]/.Equal->Rule];


GHPFundSpinopExpandFunction[expr_]:=ExpandSym[ExpandFundSpinOp@expr,SmartExpand->True]


GHPComponentRules[___]:={};


$TexInertScalarOpSortingFunc=SortByGHPOrder;


SetAttributes[InertScalarOp,HoldAll]


Tex[InertScalarOp[x_,func_][expr_]]^:=StringJoin["(",TexFunc[x,func],")",Tex[expr]]


Tex[InertScalarOp[x_,func_][expr_Plus]]^:=StringJoin["(",xAct`SpinFrames`Private`TexFunc[x,func],")(",Tex[expr],")"]


InertScalarOp[x_,0][_]:=0;
InertScalarOp[x_,expr_][0]:=0;


TexFunc[x_,func_Plus]:=StringJoin@@Riffle[TexFuncTerm[x,xAct`xTensor`Private`ListOfFactors@#]&/@List@@($TexInertScalarOpSortingFunc@func),xAct`TexAct`Private`TexOperator[Plus]];


TexFunc[x_,a_*b_]:=StringJoin[Tex[a],xAct`TexAct`Private`TexOpen["("],TexFunc[x,b],xAct`TexAct`Private`TexClose[")"]]/;FreeQ[a,x]


TexFunc[x_,x_]:="";


TexFunc[x_,y_]:=TexFuncTerm[x,{y}];


TexFuncTerm[x_,{x_}]:="1";


TexFuncTerm[x_,{y___,x_,z___}]:=Tex[Times[y,z]];


TexFuncTerm[x_,{covd1_Symbol?CovDQ[inds1__][covd2_Symbol?CovDQ[inds2__][x_]]}]:=StringJoin[xAct`TexAct`Private`TexCovDCombine[covd1,"",IndexList[inds1],$CovDFormat],xAct`TexAct`Private`TexCovDCombine[covd2,"",IndexList[inds2],$CovDFormat]];


TexFuncTerm[x_,{y___,covd1_Symbol?CovDQ[inds1__][covd2_Symbol?CovDQ[inds2__][x_]],z___}]:=StringJoin[xAct`TexAct`Private`TexFactor[Times[y,z]],xAct`TexAct`Private`TexCovDCombine[covd1,"",IndexList[inds1],$CovDFormat],xAct`TexAct`Private`TexCovDCombine[covd2,"",IndexList[inds2],$CovDFormat]];


TexFuncTerm[x_,{covd_Symbol?CovDQ[inds__][x_]}]:=xAct`TexAct`Private`TexCovDCombine[covd,"",IndexList[inds],$CovDFormat];


TexFuncTerm[x_,{y___,covd_Symbol?CovDQ[inds__][x_],z___}]:=StringJoin[Tex[Times[y,z]],xAct`TexAct`Private`TexCovDCombine[covd,"",IndexList[inds],$CovDFormat]];


TexFuncTerm[x_,{InertScalarOp[y_,func_][covd_Symbol?CovDQ[inds__][x_]]}]:=StringJoin["(",TexFunc[y,func],")",xAct`TexAct`Private`TexCovDCombine[covd,"",IndexList[inds],$CovDFormat]];


TexFuncTerm[x_,{InertScalarOp[y_,func_][x_]}]:=StringJoin["(",TexFunc[y,func],")"];


TexFuncTerm[x_,{InertScalarOp[y1_,func1_][InertScalarOp[y2_,func2_][x_]]}]:=StringJoin["(",TexFunc[y1,func1],")(",TexFunc[y2,func2],")"];


BoxStartWithMinusQ=MatchQ[#,RowBox[{"-"|_?#0,__}]]&;


MakeBoxes[InertScalarOp[x_,func_][expr_],StandardForm]:=
With[{x0=Evaluate@Coefficient[func,x,0],x1=Evaluate@Coefficient[func,x,1]},If[(Expand[func-x0-x*x1]==0)===True,With[{x1box=MakeBoxes[x1,StandardForm]},xAct`xTensor`Private`interpretbox[InertScalarOp[x,func][expr],RowBox[{"(",If[x0=!=0,MakeBoxes[x0,StandardForm]/.MakeBoxes[x,StandardForm]->"",""],If[x1=!=0,Sequence@@{If[BoxStartWithMinusQ[x1box],"","+"],x1box},""],")",MakeBoxes[expr,StandardForm]}]]],
xAct`xTensor`Private`interpretbox[InertScalarOp[x,func][expr],RowBox[{"(",MakeBoxes[func,StandardForm]/.MakeBoxes[x,StandardForm]->"\[Bullet]",")",MakeBoxes[expr,StandardForm]}]]
]]


GHPWeightOf[y:InertScalarOp[x_,func_][expr_]]:=GHPWeightOf[Evaluate[y/.InertScalarOp->Function]]


End[];
EndPackage[];



