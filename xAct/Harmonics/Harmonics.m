xAct`Harmonics`$Version={"0.7.2",{2014,2,15}}


(* Harmonics, a free package for tensor computations with tensor harmonics in Mathematica *)

(* Copyright (C) 2005-2018 David Brizuela, Jose M. Martin-Garcia and Guillermo A. Mena Marugan *)

(* This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License,or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA. 
*)


(* :Title: Harmonics *)

(* :Author: David Brizuela, Jose M. Martin-Garcia and Guillermo A. Mena Marugan *)

(* :Summary: Free package for computations with tensor harmonics *)

(* :Brief Discussion:
   - TO FILL IN
*)
  
(* :Context: xAct`Harmonics` *)

(* :Package Version: 0.7.2 *)

(* :Copyright: David Brizuela, Jose M. Martin-Garcia and Guillermo A. Mena Marugan (2005-2018) *)

(* :History: see Harmonics.History *)

(* :Keywords: *)

(* :Source: Harmonics.nb *)

(* :Warning: *)

(* :Mathematica Version: 5.0 and later *)

(* :Limitations: *)


With[{xAct`Harmonics`Private`HarmonicsSymbols=DeleteCases[Join[Names["xAct`Harmonics`*"],Names["xAct`Harmonics`Private`*"]],"$Version"|"xAct`Harmonics`$Version"|"$HarmonicsVersionExpected"|"xAct`Harmonics`$xTensorVersionExpected"]},
Unprotect/@xAct`Harmonics`Private`HarmonicsSymbols;
Clear/@xAct`Harmonics`Private`HarmonicsSymbols;
]


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`Harmonics`"];


BeginPackage["xAct`Harmonics`",{"xAct`xCoba`","xAct`xTensor`","xAct`xPerm`","xAct`xCore`"}]


Print[xAct`xCore`Private`bars];
Print["Package xAct`Harmonics`  version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2005-2018, David Brizuela, Jose M. Martin-Garcia and Guillermo A. Mena Marugan, under the General Public License."];


Off[General::shdw]
xAct`Harmonics`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


If[xAct`xCore`Private`$LastPackage==="xAct`Harmonics`",
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]];


ReportSet[$PrePrint,ScreenDollarIndices];


ReportSet[$CovDFormat,"Postfix"];


ReportSetOption[ContractMetric,AllowUpperDerivatives->True];


ReportSetOption[MakeRule,MetricOn->All];


ReportSetOption[MakeRule,ContractMetrics->True];


calDEdmonds::usage="calDEdmonds[j, mp, m][\[Alpha], \[Beta], \[Gamma]] computes the Wigner matrix with labels j, mp and m for the SO(3) rotation described by Euler angles \[Alpha], \[Beta], \[Gamma], as given by Edmonds. Note that other authors use a different convention for this function.";
calD::usage="calD[LI[l], LI[mp], LI[m]] is the formal scalar denoting the Wigner matrix. It can be transformed into actual functions of the spherical angles using the rule calDRule.";
calDRule::usage="calDRule transforms the calD scalar into the function calDEdmonds with Euler angles \[Alpha]=0, \[Beta]=\[Theta], \[Gamma]=\[Phi].";


S2::usage="The package Harmonics constructs the 2-sphere S2 with indices IndexList[a, b, c, d, e, f] and ultraindex uS2, and its tangent vbundle TangentS2.";
TangentS2::usage=S2::usage;
a::usage=S2::usage;
b::usage=S2::usage;
c::usage=S2::usage;
d::usage=S2::usage;
e::usage=S2::usage;
f::usage=S2::usage;
uS2::usage=S2::usage;
etaUpS2::usage=S2::usage;
etaDownS2::usage=S2::usage;


gamma::usage="The package Harmonics defines the metric gamma (prints as \[Gamma]) on the 2-sphere S2. Its Levi-Civita connection is cd, with associated tensors Riemanncd, Riccicd, RicciScalarcd = 2, and vanishing Torsioncd, Einsteincd and Weylcd. There is also the Christoffelcd connection cd with PD, the antisymmetric tensor epsilongamma, and the absolute value of the determinant AbsDetgamma in the DefaultChart.";
cd::usage=gamma::usage;
Riemanncd::usage=gamma::usage;
Riccicd::usage=gamma::usage;
RicciScalarcd::usage=gamma::usage;
Torsioncd::usage=gamma::usage;
Einsteincd::usage=gamma::usage;
Weylcd::usage=gamma::usage;
Christoffelcd::usage=gamma::usage;
epsilongamma::usage=gamma::usage;
AbsDetgamma::usage=gamma::usage;


spherical::usage="The package Harmonics defines the chart spherical on the 2-sphere S2 and assigns values to the components of the metric tensors in this chart. The coordinate fields are called \[Theta][] and \[Phi][]. The associated parallel (partial) derivative is called PDspherical. The associated";
\[Theta]::usage=spherical::usage;
\[Phi]::usage=spherical::usage;
PDspherical::usage=spherical::usage;
TorsionPDspherical::usage=spherical::usage;
ChristoffelPDspherical::usage=spherical::usage;
RiemannPDspherical::usage=spherical::usage;
RicciPDspherical::usage=spherical::usage;


m::usage="m[a] and mbar[a] are the complex Newman-Penrose vectors on the 2-sphere. They are null vectors.";
mbar::usage=m::usage;
mmbarRule::usage="mmbarRule transforms the product of one m[a] and one mbar[b] vectors into a sum of a gamma[a,b] object and epsilongamma[a,b] with suitable coefficients.";


Z::usage="Z[LI[l], LI[m], a1, a2, ..., as] represents the polar generalized-GS tensor harmonics with labels l, m, and s indices.";
X::usage="X[LI[l], LI[m], a1, a2, ..., as] represents the axial generalized-GS tensor harmonics with labels l, m, and s indices.";
calZ::usage="calZ[LI[sign], LI[l], LI[m], a1, a2, ..., as] is a combined notation for the generalized-GS tensor harmonics. For sign=1 calZ = Z and for sign=-1 calZ = -I*X.";
ZToX::usage="ZToX and XToZ are rules which transform the Z tensor harmonics into X harmonics and viceversa, respectively.";
XToZ::usage=ZToX::usage;
Dropepsilon::usage="Dropepsilon is a rule which contracts epsilongamma with the tensor harmonics Z and X.";
coeffK::usage="coeffK[l, s] is the normalization factor for pure-spin harmonics.";
PureSpin::usage="PureSpin[l, sign, m][a1, ..., as] returns the pure-spin harmonic as a product of coeffK, calD and a number of complex NP vectors.";
ZXToY::usage="ZXToY is a rule which transforms the Z and X harmonics into PureSpin harmonics.";
Orbitalt::usage="Orbitalt[LI[m], a] are the complex Cartesian vectors. Orbitalt[LI[m], a1, ..., as] are the complex Cartesian tensors obtained by combination of the former.";
PureOrbital::usage="PureOrbital[j, l, m][a1, ..., as] returns the pure-orbital harmonic with labels j, l, m and s indices.";


HarmonicComponent::usage="HarmonicComponent[expr] returns the scalar expression as a function of the coordinates \[Theta][] and \[Phi][].";


coeffE::usage="coeffE[{l1, s1, m1}, {l2, s2, m2}, l] is the formal coefficient E in the expansion of a product of two tensor harmonics. It can be evaluated using the coeffERule.";
coeffERule::usage="coeffERule is a rule which evaluates the numerical value of the E coefficients.";
ProductRule::usage="ProductRule is a rule which expands the product of two tensor harmonics with integer labels.";
FormalProductRule::usage="FormalProductRule[l] is a rule which expresses the product of two tensor harmonics with non-integer labels, using l as last argument of the coefficients E.";


Begin["`Private`"]


dEdmonds[j_,mp_,m_][\[Beta]_]:=0/;Abs[mp]>j||Abs[m]>j
dEdmonds[j_,mp_,m_][\[Beta]_]:=Sqrt[(j+mp)!(j-mp)!(j+m)!(j-m)!]Sum[(-1)^(j-mp-\[Sigma])/((j-mp-\[Sigma])!(j-m-\[Sigma])!(mp+m+\[Sigma])!\[Sigma]!)Sin[\[Beta]/2]^(2j-mp-m-2\[Sigma])Cos[\[Beta]/2]^(mp+m+2\[Sigma]),{\[Sigma],Max[-mp-m,0],Min[j-mp,j-m]}]


calDEdmonds[j_,mp_,m_][\[Alpha]_,\[Beta]_,\[Gamma]_]:=Exp[I mp \[Alpha]]dEdmonds[j,mp,m][\[Beta]]Exp[I m \[Gamma]]


calDRule=calD[LI[l_],LI[mp_],LI[m_]]:>calDEdmonds[l,mp,m][0,\[Theta][],\[Phi][]]


DefManifold[S2,2,{a,b,c,d,e,f}]


DefMetric[1,gamma[-a,-b],cd,{":","D"},PrintAs->"\[Gamma]"]


RicciScalarcd[]:=2


DefTensor[calD[LI[l],LI[mp],LI[m]],S2,PrintAs->"\[GothicCapitalD]"]


DefChart[cartesian,S2,{2,3},{y[],z[]},ChartColor->RGBColor[0,0,1]]


DefTensor[x[],S2]
Coordinate[1,cartesian]:=x;


DefChart[spherical,S2,{2,3},{\[Theta][],\[Phi][]}]


DefTensor[r[],S2]
Coordinate[1,spherical]:=r;


$CVVerbose=False;


metricvalues=DiagonalMatrix[{1,Sin[\[Theta][]]^2}]


MetricInBasis[gamma,-spherical,metricvalues]


MetricInBasis[gamma,spherical,Inverse[metricvalues]]


ComponentValue[{gamma[{1,-spherical},{1,-spherical}],gamma[{1,-spherical},{2,-spherical}],gamma[{1,-spherical},{3,-spherical}],gamma[{2,-spherical},{1,-spherical}],gamma[{3,-spherical},{1,-spherical}]},{0,0,0,0,0}]


Sqrt[Det[metricvalues]]//PowerExpand


ComponentValue[Determinant[gamma[-a,-b],spherical],%]


Remove[metricvalues];


ComponentValue[ComponentArray[epsilongamma,{-spherical,-spherical}],$epsilonSign Detgammaspherical[]{{0,1},{-1,0}}//ToValues]


$BMUseValues=All;
$TUseValues=All;
$CVSimplify=Simplify;
$CCSimplify=Simplify;


ChangeComponents[epsilongamma[{a,spherical},{b,spherical}],epsilongamma[{-a,-spherical},{-b,-spherical}]]


AllComponentValues[Christoffel[cd,PDspherical][{-a,-spherical},{-b,-spherical},{-c,-spherical}]]


tmp=gamma[{-a,-spherical},{-b,-spherical}]


tmp=SeparateBasis[AIndex][tmp]-tmp


tmp=cd[-e][tmp]


tmp=tmp//ContractMetric//ContractBasis


tmp=tmp//ToBasis[spherical]


tmp=ComponentArray[tmp]//ToValues//Flatten


tmp=Solve[Map[#==0&,tmp],First/@Last[TensorValues[ChristoffelcdPDspherical]]]


tmp/.Rule->ComponentValue


ChangeComponents[ChristoffelcdPDspherical[{a,spherical},{-b,-spherical},{-c,-spherical}],ChristoffelcdPDspherical[{-a,-spherical},{-b,-spherical},{-c,-spherical}]]


change={x[]->r[]Sin[\[Theta][]]Cos[\[Phi][]],y[]->r[]Sin[\[Theta][]]Sin[\[Phi][]],z[]->r[]Cos[\[Theta][]]};


spherical/:CNumbersOf[spherical,TangentS2]={1,2,3};
cartesian/:CNumbersOf[cartesian,TangentS2]={1,2,3};


MatrixForm[changeL=ComponentArray[Basis,{cartesian,-spherical}]]


MatrixForm[changeLinv=ComponentArray[Basis,{spherical,-cartesian}]]


sphericalcoordinates=Table[Coordinate[i,spherical][],{i,3}]
cartesiancoordinates=Table[Coordinate[i,cartesian][],{i,3}]/.change


MatrixForm[changeR=Outer[D,cartesiancoordinates,sphericalcoordinates]]
MatrixForm[changeRinv=Simplify[Inverse[changeR]]]


ComponentValue[changeL,changeR]


ComponentValue[changeLinv,changeRinv]


r[]:=1


SetAttributes[identity,Orderless]
identity[a_Symbol,-b_Symbol]:=Basis[a,{1,-spherical}]Basis[-b,{1,spherical}]+Basis[a,{2,-spherical}]Basis[-b,{2,spherical}]+Basis[a,{3,-spherical}]Basis[-b,{3,spherical}]


DefTensor[m[a],S2]


DefTensor[mbar[a],S2,PrintAs->"\!\(\*OverscriptBox[\(m\), \(_\)]\)"]


m/:m[a_]m[-a_]:=0
mbar/:mbar[a_]mbar[-a_]:=0
mbar/:m[a_]mbar[-a_]:=1
m/:m[-a_]mbar[a_]:=1


ComponentValue[ComponentArray[m[{a,spherical}]],{0,1/Sqrt[2],(I Csc[\[Theta][]])/Sqrt[2]}]


ChangeComponents[m[{-a,-spherical}],m[{a,spherical}]]


ComponentValue[ComponentArray[mbar[{a,spherical}]],{0,1/Sqrt[2],-((I Csc[\[Theta][]])/Sqrt[2])}]


ChangeComponents[mbar[{-a,-spherical}],mbar[{a,spherical}]]


m/:m[a_]Basis[-a_,{b_,spherical}]:=m[{b,spherical}]
m/:m[-a_]Basis[a_,{b_,spherical}]:=m[{b,spherical}]
mbar/:mbar[a_]Basis[-a_,{b_,spherical}]:=mbar[{b,spherical}]
mbar/:mbar[-a_]Basis[a_,{b_,spherical}]:=mbar[{b,spherical}]


ChangeComponents[m[{a,cartesian}],m[{a,spherical}]]


ChangeComponents[m[{-a,-cartesian}],m[{-a,-spherical}]]


mmbarRule=mbar[a_]m[b_]->(gamma[a,b]+I epsilongamma[a,b])/2


DefConstantSymbol[l];


DefTensor[Z[LI[l],LI[m],AnyIndices[TangentS2]],S2]


DefTensor[X[LI[l],LI[m],AnyIndices[TangentS2]],S2]


DefTensor[calZ[LI[sign],LI[l],LI[m],AnyIndices[TangentS2]],S2,PrintAs->"\[GothicCapitalZ]"]


calZ[LI[1],LI[l_],LI[m_]]:=2Z[LI[l],LI[m]]
calZ[LI[1],LI[l_],LI[m_],inds___]:=Z[LI[l],LI[m],inds]
calZ[LI[-1],LI[l_],LI[m_],inds___]:=-I X[LI[l],LI[m],inds]


calZ[LI[c_. (-1)^(a_.+2b_)],LI[l_],LI[m_],inds___]:=calZ[LI[c (-1)^a],LI[l],LI[m],inds]


Z/:Z[LI[l_],LI[m_],inds___]:=0/;Length[{inds}]>l;
X/:X[LI[l_],LI[m_],inds___]:=0/;Length[{inds}]>l;
calZ/:calZ[LI[sign_],LI[l_],LI[m_],inds___]:=0/;Length[{inds}]>l;


SymmetryGroupOfTensor[Z[_LI,_LI,indices___]]^:=Symmetric[2+Range@Length@{indices},Cycles]


SymmetryGroupOfTensor[X[_LI,_LI,indices___]]^:=Symmetric[2+Range@Length@{indices},Cycles]


SymmetryGroupOfTensor[calZ[_LI,_LI,_LI,indices___]]^:=Symmetric[3+Range@Length@{indices},Cycles]


Z[_LI,_LI,___,a_,___,-a_,___]:=0
Z[_LI,_LI,___,-a_,___,a_,___]:=0
X[_LI,_LI,___,a_,___,-a_,___]:=0
X[_LI,_LI,___,-a_,___,a_,___]:=0
calZ[_LI,_LI,_LI,___,a_,___,-a_,___]:=0
calZ[_LI,_LI,_LI,___,-a_,___,a_,___]:=0


X[_LI,_LI]:=0


X/: Z[LI[l_],LI[m_],inds1__?AIndexQ]X[LI[l_],LI[m_],inds2__?AIndexQ]:=0/;Sort[xAct`xTensor`Private`TakePairs[IndexList[inds1,inds2]]]===Sort[IndexList[inds1,inds2]]


ZToX=Z[l_LI,m_LI,a_,b___]:>Module[{c},-Symmetrize[epsilongamma[a,c]X[l,m,-c,b],{a,b}]];
XToZ=X[l_LI,m_LI,a_,b___]:>Module[{c},Symmetrize[epsilongamma[a,c]Z[l,m,-c,b],{a,b}]];
calZTocalZ=calZ[LI[sign_],l_LI,m_LI,a_,b___]:>Module[{c},-I Symmetrize[epsilongamma[a,c]calZ[LI[-sign],l,m,-c,b],{a,b}]];


Dropepsilon=term:(_. epsilongamma[a_,b_](Z|X|calZ)[__]):>ReplaceAll[term,{ZToX,XToZ,calZTocalZ}]/;IsIndexOf[term,-a]||IsIndexOf[term,-b];


cd[a_][Z[LI[l_],LI[m_]]]:=Z[LI[l],LI[m],a]


cd[b_][Z[LI[l_],LI[m_],a_?AIndexQ]]:=Z[LI[l],LI[m],a,b]-l (l+1)/2gamma[a,b]Z[LI[l],LI[m]]


cd[b_][X[LI[l_],LI[m_],a_?AIndexQ]]:=X[LI[l],LI[m],a,b]-l (l+1)/2epsilongamma[a,b]Z[LI[l],LI[m]]


cd[b_][Z[LI[l_],LI[m_],a1_?AIndexQ,a2_?AIndexQ,a___?AIndexQ]]:=Module[{b$},With[{s=1+1+Length[{a}]},Z[LI[l],LI[m],a1,a2,a,b$]+(l+s)(l-s+1)/2Symmetrize[1/2gamma[a1,a2]Z[LI[l],LI[m],a,b$]-gamma[b$,a1]Z[LI[l],LI[m],a2,a],{a1,a2,a}]]/.b$->b]


cd[b_][X[LI[l_],LI[m_],a1_?AIndexQ,a2_?AIndexQ,a___?AIndexQ]]:=Module[{b$},With[{s=1+1+Length[{a}]},X[LI[l],LI[m],a1,a2,a,b$]+(l+s)(l-s+1)/2Symmetrize[1/2gamma[a1,a2]X[LI[l],LI[m],a,b$]-gamma[b$,a1]X[LI[l],LI[m],a2,a],{a1,a2,a}]]/.b$->b]


coeffK[l_,s_]:=Infinity/;s>l
coeffK[l_,s_]:=Sqrt[(2l+1)/(4Pi)/2^s (l+s)!/(l-s)!]


PureSpin[l_,1,mlabel_][inds___]:=With[{s=Length[{inds}]},coeffK[l,s]calD[LI@l,LI@s,LI@mlabel](-1)^s Times@@(m/@{inds})]
PureSpin[l_,-1,mlabel_][inds___]:=With[{s=Length[{inds}]},coeffK[l,s]calD[LI@l,LI@-s,LI@mlabel]Times@@(mbar/@{inds})]


ZXToY:={
Z[LI[l_],LI[m_]]:>PureSpin[l,1,m][],
Z[LI[l_],LI[m_],inds__]:>PureSpin[l,1,m][inds]+PureSpin[l,-1,m][inds],
X[LI[l_],LI[m_],inds__]:>I(PureSpin[l,1,m][inds]-PureSpin[l,-1,m][inds])}


spherical/:CNumbersOf[spherical,TangentS2]={2,3};
cartesian/:CNumbersOf[cartesian,TangentS2]={2,3};


HarmonicComponent[expr_]:=Simplify[ToValues[expr/.ZXToY/.calDRule]]


Format[coeffE[{s_,l_,m_},{sp_,lp_,mp_},lpp_]]:=Subsuperscript["E",StringJoin[ToString[s],",",ToString[l],",",ToString[m],";",ToString[lpp]],StringJoin[ToString[sp],",",ToString[lp],",",ToString[mp]]]


coeffE/:FindIndices[coeffE[___]]=IndexList[];


_?FirstDerQ[coeffE[__]]^=0;


IsIndexOf[coeffE[___],__]^:=False;


coeffERule=coeffE[{s_,l_,m_},{sp_,lp_,mp_},lpp_]:>coeffK[lp,Abs[sp]]coeffK[l,Abs[s]]/coeffK[lpp,Abs[s+sp]]ClebschGordan[{lp,mp},{l,m},{lpp,m+mp}]ClebschGordan[{lp,sp},{l,s},{lpp,s+sp}]


TTensor[1][ainds_List,binds_List]:=(-1)^Length[ainds]Times@@(mbar/@ainds)Times@@(m/@binds);
TTensor[-1][ainds_List,binds_List]:=(-1)^Length[ainds]Times@@(m/@ainds)Times@@(mbar/@binds);


calTTensor[sign_][ainds_List,binds_List]:=1/2 (TTensor[-1][ainds,binds]+sign  TTensor[1][ainds,binds]);


ProductRule:={rest_. (tp:(Z|X))[LI[lp_],LI[mp_],a___](t:(Z|X))[LI[l_],LI[mlabel_],b___]:>Module[{sp=Length[{a}],s=Length[{b}],ab,aa,sigma,sigmap,coeff,coeffp},
{coeffp,sigmap}=Switch[tp,Z,{1,1},X,{I,-1}];
{coeff,sigma}=Switch[t,Z,{1,1},X,{I,-1}];
ab=Sequence@@Drop[{a},s];
aa=Drop[{a},-sp+s];
rest  coeffp coeff If[s===0,1/2,1]If[sp===0,1/2,1]Sum[
coeffE[{sp,lp,mp},{s,l,mlabel},lpp]calZ[LI[(-1)^(l+lp-lpp)sigma sigmap],LI[lpp],LI[mp+mlabel],a,b]+sigma coeffE[{sp,lp,mp},{-s,l,mlabel},lpp](calTTensor[+1][aa,{b}]calZ[LI[(-1)^(l+lp-lpp)sigma sigmap],LI[lpp],LI[mp+mlabel],ab]+calTTensor[-1][aa,{b}]calZ[LI[-(-1)^(l+lp-lpp)sigma sigmap],LI[lpp],LI[mp+mlabel],ab]),{lpp,Abs[lp-l],lp+l}]]/;Length[{a}]>=Length[{b}],rest_. Z[LI[l_],LI[mlabel_]]^2:>
rest Sum[coeffE[{0,l,mlabel},{0,l,mlabel},lpp]Z[LI[lpp],LI[2mlabel]],{lpp,0,2l}]}


FormalProductRule[lc_]:={rest_. (ta:(Z|X))[LI[la_],LI[ma_],a___](tb:(Z|X))[LI[lb_],LI[mb_],b___]:>Module[{sa=Length[{a}],sb=Length[{b}],sigmaa,sigmab,coeffa,coeffb,indicesA,indicesZ},
{coeffa,sigmaa}=Switch[ta,Z,{1,1},X,{I,-1}];
{coeffb,sigmab}=Switch[tb,Z,{1,1},X,{I,-1}];
indicesZ=Sequence@@(Drop[{a},sb]);
indicesA=Drop[{a},sb-sa];rest  coeffa coeffb  If[sa===0,1/2,1]If[sb===0,1/2,1](coeffE[{sa,la,ma},{sb,lb,mb},lc]calZ[LI[(-1)^(lb+la-lc)sigmaa sigmab],LI[lc],LI[ma+mb],a,b]+sigmab coeffE[{sa,la,ma},{-sb,lb,mb},lc](calTTensor[+1][indicesA,{b}]calZ[LI[(-1)^(lb+la-lc)sigmaa sigmab],LI[lc],LI[ma+mb],indicesZ]+calTTensor[-1][indicesA,{b}]calZ[LI[-(-1)^(lb+la-lc)sigmaa sigmab],LI[lc],LI[ma+mb],indicesZ]))]/;Length[{a}]>=Length[{b}],rest_. Z[LI[la_],LI[ma_]]^2:>
rest  coeffE[{0,la,ma},{0,la,ma},lc]calZ[LI[(-1)^(-lc)],LI[lc],LI[2ma]]/2}


End[]


EndPackage[]



