(************************ 0. Info and copyright ***********************)


xAct`xTensor`$Version={"1.1.3",{2018,2,28}};
xAct`xTensor`$xPermVersionExpected={"1.2.3",{2015,8,23}};


(* xTensor, fast abstract tensor computer algebra *)

(* Copyright (C) 2002-2018 Jose M. Martin-Garcia *)

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


(* :Title: xTensor *)

(* :Author: Jose M. Martin-Garcia *)

(* :Summary: Efficient abstract tensor computer algebra *)

(* :Brief Discussion:
   - Perform generic abstract tensor calculations.
   - Define manifolds, vector bundles, tensors, covariant derivatives and metrics.
   - Define symmetries as groups of permutations. Package xPerm.
   - Define Lie derivatives, Lie brackets and variational derivatives.
   - Define constant-symbols, parameters and parametric derivatives.
   - Simplifications: use Portugal's algorithms. Package xPerm.
   - Define bases and charts. Compute components. Package xCoba.
   - Define the concept of tensor equation and tensor rule.
*)
  
(* :Context: xAct`xTensor` *)

(* :Package Version: 1.1.3 *)

(* :Copyright: Jose M. Martin-Garcia (2002-2018) *)

(* :History: see xTensor.History file *)

(* :Keywords: *)

(* :Source: xTensor.nb *)

(* :Warning: *)

(* :Mathematica Version: 6.0 and later *)

(* :Limitations: Many. xTensor knows what its author knows! *)


With[{xAct`xTensor`Private`xTensorSymbols=DeleteCases[Join[Names["xAct`xTensor`*"],Names["xAct`xTensor`Private`*"]],"$Version"|"xAct`xTensor`$Version"|"$xPermVersionExpected"|"xAct`xTensor`$xPermVersionExpected"|"xAct`xTensor`$ReadingVerbose"]},
Unprotect/@xAct`xTensor`Private`xTensorSymbols;
Clear/@xAct`xTensor`Private`xTensorSymbols;
];


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`xTensor`"];


xAct`xTensor`Symmetrize;
Off[System`Symmetrize::shdw]


BeginPackage["xAct`xTensor`",{"xAct`xCore`","xAct`xPerm`"}]


If[Not@OrderedQ@Map[Last,{$xPermVersionExpected,xAct`xPerm`$Version}],Throw@Message[General::versions,"xPerm",xAct`xPerm`$Version,$xPermVersionExpected]]


Print[xAct`xCore`Private`bars];
Print["Package xAct`xTensor`  version ",xAct`xTensor`$Version[[1]],", ",xAct`xTensor`$Version[[2]]];
Print["CopyRight (C) 2002-2018, Jose M. Martin-Garcia, under the General Public License."]


Off[General::shdw]
xAct`xTensor`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


If[xAct`xCore`Private`$LastPackage==="xAct`xTensor`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]]


(******************* 1. Symbols, names and messages *******************)


If[$ReadingVerbose,Print["Reading section 1: Usage messages."],Null,Null]


(* Versions and GPL disclaimer *)
xAct`xTensor`$Version::usage="$Version is a global variable giving the version of the package xTensor in use.";
xAct`xTensor`$xPermVersionExpected="$xPermVersionExpected is a global variable giving the oldest possible version of the package xPerm which is required by the version of the package xTensor in use.";
xAct`xTensor`Disclaimer::usage="Disclaimer[] gives the General Public License (GPL) disclaimer message.";


(* Verbose while reading the package? *)
xAct`xTensor`$ReadingVerbose::usage="$ReadingVerbose is a Boolean global variable that turns on log-messages while the xTensor package is being read. By default it is not set.";


(* Generic options in Def commands *)
$ProtectNewSymbols::usage="$ProtectNewSymbols is a Boolean global variable specifying the default value for the option ProtectNewSymbol of all DefType commands. The default initial value is False.";
ProtectNewSymbol::usage="ProtectNewSymbol is a Boolean option for all DefType commands that specifies whether the defined symbol will be protected or not. The default value is given by the global variable $ProtectNewSymbols.";
DefInfo::usage="DefInfo is an option for all DefType commands specifying an additional message to output at definition time. DefInfo[x] returns the DefInfo image associated with x at definition time.";
$DefInfoQ::usage="$DefInfoQ is a boolean global variable controlling whether the info definition messages are printed. By default it is True, but setting it to False deactivates all Def messages.";
$UndefInfoQ::usage="$UndefInfoQ is a boolean global variable controlling whether the info undefinition messages are printed. By default it is True, but setting it to False deactivates all Undef messages.";


(* Formatting of objects *)
xTensorFormStart::usage="xTensorFormStart[class] starts formatting for a class of objects. Valid classes are: ConstantSymbol, Parameter, InertHead, Tensor, CovD, LieD. xTensorFormStart[] starts formatting for all those classes.";
xTensorFormStop::usage="xTensorFormStop[class] stops formatting for a class of objects. Valid classes are: ConstantSymbol, Parameter, InertHead, Tensor, CovD, LieD. xTensorFormStop[] stops formatting for all those classes.";
PrintAs::usage="PrintAs[symbol] gives the output string or boxes for symbol in StandardForm.\n\nPrintAs is also an option for the DefType commands that specifies the output form of a defined symbol. It can be PrintAs -> string or PrintAs -> boxes. If boxes are to be constructed in a delayed way then use PrintAs -> Hold[delayedboxes]. Finally it can also be PrintAs -> function, such that the output will be ToString[function[symbol]].";
PrintAsCharacter::usage="PrintAsCharacter[symbol] returns a character to be used in the typesetting of fundamental objects in xTensor. For example \"R\" for Riemann, \"G\" for Einstein, etc.";


(* Relations among symbols *)
VisitorsOf::usage="VisitorsOf[symbol] gives the list of all symbols which require symbol to exist before they can be defined. symbol can only be removed if this list is empty.";
HostsOf::usage="HostsOf[symbol] gives the list of symbols which are required to exist previously, before symbol was defined, and which cannot be undefined as long as symbol exists.";
MasterOf::usage="MasterOf[servant] gives the master symbol of the symbol servant. The latter is defined and removed when, and only when, the former is defined and removed, respectively.";
ServantsOf::usage="ServantsOf[symbol] gives the list of symbols which have been simultaneously defined with symbol. They will be removed when, and only when, symbol is removed.";
Master::usage="Master is an option for DefType commands specifying the master symbol of the defined symbol. By default, no master is associated.";


(* Reserved words for types *)
AbstractIndex::usage="AbstractIndex represents the type for symbols denoting abstract indices of a vector bundle.";
Manifold::usage="Manifold represents the type for smooth, differentiable manifolds.";
Mapping::usage="Mapping represents the type for smooth mappings between manifolds.";
VBundle::usage="VBundle represents the type for smooth, differentiable vector bundles.";
Tensor::usage="Tensor represents the type for smooth tensor fields in the product of several vector bundles.";
Metric::usage="Metric represents the type for smooth metric tensor fields on a vector bundle.";
CovD::usage="CovD represents the type for smooth connections living on some manifold.";
InertHead::usage="InertHead represents the type for tensor wrappers of tensors.";
ScalarFunction::usage="ScalarFunction represents the type for scalar functions.";
ConstantSymbol::usage="ConstantSymbol represents the type for constant symbols.";
Parameter::usage="Parameter represents the type for parameter symbols.";
Basis::usage="Basis represents both the type and the tensor head for a basis vector/covector field.";
Chart::usage="Chart represents the type for smooth coordinate charts on manifolds.";


(* Data-bases *)
$AbstractIndices::usage="$AbstractIndices is a global variables storing the list of all currently defined abstract indices, including those internally generated.";
$ConstantSymbols::usage="$ConstantSymbols is a global variable storing the list of all currently defined constant symbols.";
$Tensors::usage="$Tensors is a global variable storing the list of all currently defined tensors.";
$CovDs::usage="$CovDs is a global variable storing the list of all currently defined covariant derivatives, including the fiducial partial derivative PD.";
$Manifolds::usage="$Manifolds is a global variable storing the list of all currently defined manifolds.";
$Mappings::usage="$Mappings is a global variable storing the list of all currently defined mappings.";
$VBundles::usage="$VBundles is a global variable storing the list of all currently defined vector bundles.";
$ProductManifolds::usage="$ProductManifolds is a global variable storing the list of all currently defined product manifolds. It is a subset of $Manifolds.";
$SumVBundles::usage="$SumVBundles is a global variable storing the list of all currently defined direct-sum vector bundles. It is a subset of $VBundles.";
$Metrics::usage="$Metrics is a global variable storing the list of all currently defined metrics. It is a subset of $Tensors.";
$ProductMetrics::usage="$ProductMetrics is a global variable storing the list of all currently defined product metrics. It is a subset of $Metrics.";
$InertHeads::usage="$InertHeads is a global variable storing the list of all currently defined inert heads of indexed expressions.";
$ScalarFunctions::usage="$ScalarFunctions is a global variable storing the list of all currently defined functions acting on scalars.";
$Rules::usage="$Rules is a global variable storing the list of all rules registered as global rules.";
$Bases::usage="$Bases is a global variable containing the list of all symbols registered as bases (not necessarily coordinate bases) on the bundles in $VBundles.";
$Charts::usage="$Charts is a global variable containing the list of all symbols registered as charts on the manifolds in $Manifolds. It is a subset of $Bases.";
$Parameters::usage="$Parameters is a global variable storing the list of all currently defined parameters.";
$Products::usage="$Products is a global variable storing the list of all currently defined products.";


(* Collecting function *)
FindAllOfType::usage="FindAllOfType[expr, type] gives the list of all occurrences of elements of the given type in expr, including their indices for types like Tensor, CovD, etc.";


(* Undefinition *)
Undef::usage="Undef[symbol] undefines the symbol, whatever its type may be. The only exception are abstract indices.";


(* Name validation in session *)
ValidateSymbolInSession::usage="ValidateSymbolInSession[symbol] throws an error if symbol is already used in this session as a constant-symbol, parameter, manifold, vector bundle, abstract index, tensor (or metric), form, covariant derivative, basis (or chart), inert head or scalar function. Otherwise it gives Null.";


(* Q-functions on symbols *)
AbstractIndexQ::usage="AbstractIndexQ[symbol] gives True if symbol has been registered as an abstract index. Note that it gives False on down-indices.";
ConstantSymbolQ::usage="ConstantSymbolQ[symbol] gives True if symbol has been registered as a constant symbol, and False otherwise.";
ManifoldQ::usage="ManifoldQ[expr] gives True if expr has been registered as a manifold, and False otherwise.";
MappingQ::usage="MappingQ[symbol] gives True if symbol has been registered as a mapping, and False otherwise.";
VBundleQ::usage="VBundleQ[symbol] gives True if symbol has been registered as a vector bundle, and False otherwise.";
ParameterQ::usage="ParameterQ[symbol] gives True if symbol has been registered as a parameter and False otherwise.";
xTensorQ::usage="xTensorQ[expr] gives True if expr has been registered as a tensor, and False otherwise. Do not confuse with Mathematica's TensorQ.";
CovDQ::usage="CovDQ[expr] gives True if expr has been registered as a covariant derivative operator, and False otherwise.";
MetricQ::usage="MetricQ[expr] gives True if expr has been registered as a metric, and False otherwise.";
BasisQ::usage="BasisQ[symbol] gives True if symbol has been registered as a basis or chart, and False otherwise. BasisQ[basis, vbundle] gives True if basis is one of the bases of the vector bundle vbundle, and False otherwise.";
ChartQ::usage="ChartQ[symbol] gives True if symbol has been registered as a chart, and False otherwise. ChartQ[chart, manifold] gives True if chart is one of the charts of manifold, and False otherwise.";
InertHeadQ::usage="InertHeadQ[head] gives True if head[object] must be treated as object form the point of view of symmetries and canonicalization in xTensor, and False otherwise.";
ScalarFunctionQ::usage="ScalarFunctionQ[f] gives True if f has been defined as a mathematical function acting on scalars, or a derivative of one of those, and False otherwise.";
ProductQ::usage="ProductQ[symbol] gives True if symbol has been registered as a product on indexed objects.";

(* Q-functions on other expressions *)
FirstDerQ::usage="FirstDerQ[expr] gives True if expr is of the form covd[i], LieD[v], ParamD[par] or OverDot, or False otherwise. In particular it gives False on multiple (nested) derivatives like ParamD[par1, par2].";
UpVectorQ::usage="UpVectorQ[expr] gives True if expr has only one free up-index, and False otherwise. UpVectorQ[expr, vbundle] gives True if also the free index of expr belongs to the vector bundle vbundle.";
DownVectorQ::usage="DownVectorQ[expr] gives True if expr has only one free down-index, and False otherwise. DownVectorQ[expr, vbundle] gives True if also the free index of expr belongs to the vector bundle vbundle.";
ConstantQ::usage="ConstantQ[x] gives True if x is a number or a numeric symbol or a constant symbol. Otherwise it gives False.";


(* Inert heads *)
DefInertHead::usage="DefInertHead[head] defines an inert head such that head[expr] is treated as expr from the tensorial point of view.";
UndefInertHead::usage="UndefInertHead[head] undefines the inert head.";
LinearQ::usage="LinearQ is a Boolean option for DefInertHead and DefProduct stating that the symbol defined is linear with respect to constants in its tensorial argument(s). The default value is False.\n\nLinearQ[head] gives True if head has been defined as a linear inert-head or product, and False otherwise.";
ContractThrough::usage="ContractThrough is an option for DefInertHead specifying a list of metrics and/or the delta tensor which can be contracted through the defined inert-head.";
ContractThroughQ::usage="ContractThroughQ[head, metric] says whether the given metric can be contracted through the inert head. The default value is False.";
ERROR::usage="ERROR[expr] is an expression where an error has been detected. ERROR is an inert head with a single argument.";
Keep::usage="Keep[expr] represents the tensorial expression expr, but prevents its expansion under the action of other functions.";
DependenciesOfInertHead::usage="DependenciesOfInertHead[ih] returns the implicit dependencies (manifols and/or parameters) of the inert head ih.";

(* Complex conjugation *)
Dagger::usage="Dagger[expr] returns the complex conjugate of an expression. Dagger is an inert head. Use DaggerIndex on indices.\n\nDagger is also an option for several DefType commands, specifying how they behave under complex conjugation: possible values are Real, Imaginary, Complex, Hermitian and Antihermitian, the latter two only applying to tensors.";
DaggerIndex::usage="DaggerIndex[ind] returns the conjugated index to ind. DaggerIndex[IndexList[i1, ..., in]] returns the list (also with head IndexList) of respective conjugated indices. Label indices are considered real by default.";
DaggerQ::usage="DaggerQ[expr] gives True if expr is complex (Dagger[expr] different from expr) and False otherwise.";
TransposeDagger::usage="TransposeDagger[tensor[inds]] exchange the positions of the complex conjugated indices following this rule: the n-th index of the complex vbundle C is exchanged with the n-th index of the vbundle Dagger[C]. If the number of indices in tensor belonging to C and Dagger[C] is not the same, an error is thrown. There is no conjugation in this process, simply swapping of indices. TransposeDagger[tensor[inds], vbs] only transposes indices of vbs (a vbundle or a list of them).";
Pair::usage="Pair[A, B\[Dagger]] represents a conjugated pair of indices on conjugated vbundles.";
Imaginary::usage="Imaginary is a possible value for the option Dagger.";
Hermitian::usage="Hermitian is a possible value for the option Dagger of DefTensor.";
Antihermitian::usage="Antihermitian is a possible value for the option Dagger of DefTensor.";
HermitianQ::usage="HermitianQ[tensor] gives True if tensor is a complex Hermitian tensor, and False otherwise. In particular, this function returns False on real tensors.";
AntihermitianQ::usage="AntihermitianQ[tensor] gives True if tensor is a complex antihermitian tensor, and False otherwise.";


(* Scalar functions *) 
DefScalarFunction::usage="DefScalarFunction[f] defines f to be a scalar function of one or several scalar arguments.";
UndefScalarFunction::usage="UndefScalarFunction[f] undefines the scalar function f.";

(* Parameters *)
DefParameter::usage="DefParameter[t] defines t to be a real parameter on which indexed objects can depend.";
UndefParameter::usage="UndefParameter[t] undefines the parameter t.";

(* Scalars *)
ScalarQ::usage="ScalarQ[expr] gives true if expr is a scalar, that is, it has no free index.";
Scalar::usage="Scalar[expr] denotes that expr is a scalar (no free indices) expression. The expression is simplified as much as possible extracting scalars and constants.";
$SeparateScalarsFromScalar::usage="$SeparateScalarsFromScalar is a global variable specifying whether Scalar[x] should be converted into x for a non-indexed scalar x.";
$ScalarColor::usage="$ScalarColor is a global variable specifying the color of the parentheses surrounding the formatting of a Scalar expression.";

(* Products *)
DefProduct::usage="DefProduct[prod, pbs, pos] defines prod to be an algebra product of indexed objects, in which pos is the product of scalars and pbs is the product of scalars by elements of the algebra. The second and third arguments may be omitted, both having default value Times.";
UndefProduct::usage="UndefProduct[prod] undefines the product prod.";
AssociativeProductQ::usage="AssociativeProductQ[prod] gives whether the product prod is associative or not.\n\nAssociativeProductQ is also a boolean option of DefProduct, specifying whether the product being defined is associative. Its default value is True.";
GradedProductQ::usage="GradedProductQ[prod] says whether the algebra associated to the product prod is graded or not. If it is, grades are computed using Grade[expr, prod].\n\nGradedProductQ is also a boolean option of DefProduct, specifying whether the product being defined is graded or not. Its default value is False.";
CommutativityOfProduct::usage="CommutativityOfProduct[prod] describes the symmetry of the product prod. It can be \"Commutative\", \"Anticommutative\", \"SuperCommutative\", \"SuperAnticommutative\" or None. It can also be a function f of two arguments returning the sign under exchange of any two consecutive factors.\n\nCommutativityOfProduct is also an option of DefProduct, specifying the symmetry of the product being defined. Its default value is None.";
IdentityElementOfProduct::usage="IdentityElementOfProduct[prod] returns the neutral element of the product prod.\n\nIdentityElementOfProduct is also an option of DefProduct, specifying the neutral element of the product being defined. Its default value is None.";
ScalarsOfProduct::usage="ScalarsOfProduct[prod] returns a function that gives True on the scalars of the product algebra prod, and gives False otherwise.\n\nScalarsOfProduct is also an option of DefProduct, specifying that function at definition time. Its default value is NumericQ.";
ProductByScalar::usage="ProductByScalar[prod] returns the product used to multiply elements of the algebra by scalars of the algebra defined by the product prod. The result may be the product pbs or {Left, pbs} or {Right, pbs} depending on whether pbs is orderless or must be used as pbs[scalar, x] or pbs[x, scalar], respectively.";
ProductOfScalars::usage="ProductOfScalars[prod] returns the product used to multiply scalars of the the product algebra defined by the product prod.";
DefineCommutator::usage="DefineCommutator is a boolean option of DefProduct, specifying whether the the associated product Commutator[prod] must be defined together with the product prod being defined. Its default value is False.";
Commutator::usage="Commutator[prod][a, b] represents the commutator associated to the product prod, that is prod[a, b] - prod[b, a]. Commutator[prod] is an Anticommutative product. If prod is associative then Commutator[prod] obeys the Jacobi identity and defines a Lie algebra.";
Anticommutator::usage="Anticommutator[prod][a, b] represents the anticommutator associated to the product prod, that is prod[a, b] + prod[b, a]. Commutator[prod] is a Commutative product.";
Supercommutator::usage="Supercommutator[prod][a, b] represents the supercommutator associated to the product prod, that is prod[a, b] - (-1)^(|a|\[CenterDot]|b|) prod[b, a], where |a| is the grade of a. Supercommutator[prod] is a SuperAnticommutative product. If prod is associative then Supercommutator[prod] obeys a graded Jacobi identity and defines a graded Lie algebra.";
ExpandCommutator::usage="ExpandCommutator[expr] expands all commutators, anticommutators, and supercommutators to their underlying base products.";
Grade::usage="Grade[expr, prod] returns the grade of the expression expr with respect to the product prod.";
GradeOfTensor::usage="GradeOfTensor[T, prod] gives the grade of the tensor T in the product prod.\n\nGradeOfTensor is also an option of DefTensor specifying the grade of the tensor being defined in various products, as in GradeOfTensor->{wedge->1, clifford->2}.";
GradeOfCovD::usage="GradeOfCovD[cd, prod] gives the grade of the covariant derivative cd in the product prod.";
GradeOfProduct::usage="GradeOfProduct[prod, prod2] returns the grade with respect to product prod2 added by the presence of a product prod of expressions.\n\nGradeOfProduct is also an option of DefProduct, specifying that the value of GradeOfProduct[prod, prod] when the product prod is defined.";
xSortOrder::usage="xSortOrder is an option of DefProduct specifying an Order-like function used to sort the factors of the product being defined. Its default value is Automatic, which represents the standard ordering used by the Times product in xTensor."


(* Conventions and signs *)
$epsilonSign::usage="$epsilonSign is a global sign giving the default value for the function epsilonOrientation. This variable is kept for backward compatibility.";
$RiemannSign::usage="$RiemannSign defines the global sign of the Riemann tensor from derivatives of a connection: Riemann[-a,-b,-c,d] = $RiemannSign * ( PD[-b][Christoffel[d,-a,-c] + ...)";
$RicciSign::usage="$RicciSign defines the global sign of the Ricci tensor from contraction of the Riemann tensor: Ricci[-a,-b] = $RicciSign * Riemann[-a,-c,-b,c]";
$TorsionSign::usage="$TorsionSign defines the global sign of the torsion tensor of a covariant derivative: cd[-a]@cd[-b]@f[] - cd[-b]@cd[-a]@f[] = - $TorsionSign Torsioncd[c,-a,-b] cd[-c]@f[].";
$ExtrinsicKSign::usage="$ExtrinsicKSign defines the global sign of the extrisic curvature tensor in terms of the derivative of a vector field: ExtrinsicK[-a,-b] = $ExtrinsicKSign Projector[ cd[-a][ n[-b] ] ].";
$AccelerationSign::usage="$AccelerationSign defines the global sign of the acceleration vector in terms of the derivative of a vector field: Acceleration[-a] = $AccelerationSign n[b] cd[-b][ n[-a] ] / norm.";


(* Creating names *)
GiveSymbol::usage="GiveSymbol[object, case] constructs a symbol for the required object in the special given case. Both must be symbols or strings.";
GiveOutputString::usage="GiveOutputString[object, case] gives the string to be used as output in StandardForm. Both object and case must be symbols or strings.";


(* Types of g-indices. BASIS1. BASIS2 is not yet documented *)
DefAbstractIndex::usage="DefAbstractIndex[symbol] defines symbol to be an abstract index without associating it to a vector bundle yet.";
UndefAbstractIndex::usage="UndefAbstractIndex[index] undefines index, making it unusable.";
GIndexQ::usage="GIndexQ[x] gives True if x is a valid generalized index (an abstract index, a label index, a valid basis index, or a valid direction). Patterns are not accepted. Otherwise it gives False. GIndexQ[x, vbundle] gives True if x is a valid generalized index belonging to the vector bundle vbundle.";
AIndexQ::usage="AIndexQ[x] gives True if x is recognized as a valid abstract index, and False otherwise. AIndexQ[x, vbundle] gives True if x is a valid abstract index belonging to the vector bundle vbundle.";
BIndexQ::usage="BIndexQ[x] gives True if x is a valid basis index: {i, basis}, where i is an abstract index. BIndexQ[x, basis] gives True if x is a valid basis index belonging to basis. BIndexQ[x, vbundle] gives True if x is a valid basis index belonging to the vector bundle vbundle.";
CIndexQ::usage="CIndexQ[x] gives True if x is a valid component index: {i, basis}, where i is an integer cnumber compatible with basis. CIndexQ[x, basis] gives True if x is a valid component index belonging to basis. CIndexQ[x, vbundle] gives True if x is a valid component index belonging to some basis of the vector bundle vbundle.";
DIndexQ::usage="DIndexQ[x] gives True if x is a valid directional index of the form Dir[v], where v is a covariant or contravariant vector. DIndexQ[x, vbundle] gives True if x is a valid directional index belonging to the vector bundle vbundle.";
LIndexQ::usage="LIndexQ[x] gives True if x is a label (an expression with head LI, perhaps multiplied by -1), and False otherwise.";
PIndexQ::usage="PIndexQ[ind] gives True if ind is a valid pattern for an index: _, _h, a_h, a_h?Q where a must be a valid abstract index and h must be one of Symbol, Dir, LI, List, Blank, Pattern, PatternTest. Other kinds of pattern, in particular Condition, are not accepted.";
ABIndexQ::usage="ABIndexQ[x] gives True if x is an ABIndex (abstract index or basis index) and False otherwise. ABIndexQ[x, basis] gives True if x is an ABIndex belonging to basis and False otherwise. ABIndexQ[x, vbundle] gives True if x is an ABIndex belonging to the vector bundle vbundle and False otherwise.";
BCIndexQ::usage="BCIndexQ[x] gives True if x is a BCIndex (basis index or component index), and False otherwise. BCIndexQ[x, basis] gives True if x is a BCIndex belonging to basis and False otherwise. BCIndexQ[x, vbundle] gives True if x is an BCIndex belonging to the vector bundle vbundle and False otherwise.";
CDIndexQ::usage="CDIndexQ[x] gives True if x is a CDIndex (component index or directional index), and False otherwise. CDIndexQ[x, basis] gives True if x is a CDIndex belonging to basis and False otherwise. CDIndexQ[x, vbundle] gives True if x is an CDIndex belonging to the vector bundle vbundle and False otherwise.";
Dir::usage="Dir[v] is a directional index in any tensorial slot. Its single argument v is assumed to be a vector or a covector.";
LI::usage="LI is the head for label indices.";
Labels::usage="Labels is the \"vector bundle\" to which all labels belong.";
UpIndex::usage="UpIndex[ind] converts ind into an up-index: On expressions of the form -expr it returns expr. On basis- and component-indices UpIndex[{1, basis}] and UpIndex[{1, -basis}] return {1, basis}. On directions Dir[...] the function UpIndex returns a covector. On other cases it returns the input expr.";DownIndex::usage="DownIndex[ind] converts ind into a down-index, returning opposite results to UpIndex.";
ChangeIndex::usage="ChangeIndex[ind] converts up-indices to down-indices and viceversa.";
UpIndexQ::usage="UpIndexQ[a] gives True if a is an upper index and False otherwise. It gives False on patterns.";
DownIndexQ::usage="DownIndexQ[a] gives True if a is a lower index and False otherwise. It gives False on patterns.";
EIndexQ::usage="EIndexQ[ind] identifies indices which can obey the Einstein convention, returning opposite results to BlockedQ. We call them Einstein, contractible or traceable indices.";
BlockedQ::usage="BlockedQ[ind] returns True on blocked indices: types C, D and L. On other cases (traceable indices) it returns False, including indices of types A, B and patterns.";
PairQ::usage="PairQ[a, b] returns True if indices a, b form an Einstein pair of contracted indices. Currently this is only possible for a pair a, -a of abstract indices or a pair {a, B}, {-a,-B} of basis indices of the same basis B.";
IndexList::usage="IndexList[i1, i2, ...] is a list of g-indices.";
$IndexListColor::usage="$IndexListColor is a global variable specifying the color to format the braces of IndexList expressions in StandardForm. The default color is blue, RGBColor[0,0,1].";
PatternIndex::usage="PatternIndex[name, type] constructs a pattern index for the given type (AIndex, DIndex or LIndex), with the given name. PatternIndex[name, type, character] also specifies the given character (Up or Down). PatternIndex[name, type, character, vbundle] restricts the pattern to the vector bundle vbundle (use character Null when accepting both Up and Down).";
AIndex::usage="AIndex represents the logic type for abstract indices. It also represents the abstract basis corresponding to abstract indices.";
BIndex::usage="BIndex represents the logic type for basis indices.";
CIndex::usage="CIndex represents the logic type for component indices.";
DIndex::usage="DIndex represents the logic type for directional indices.";
LIndex::usage="LIndex represents the logic type for label indices.";
PIndex::usage="PIndex represents the logic type for pattern indices.";
Off[General::shdw];
xAct`xTensor`Up::usage="Up represents the logic type for up-indices.";
xAct`xTensor`Down::usage="Down represents the logic type for down-indices.";
On[General::shdw];
Free::usage="Free represents the logic type for free indices.";
Dummy::usage="Dummy represents the logic type for dummy indices.";
Blocked::usage="Blocked represents the logic type for blocked indices (components, directions and labels).";
IModQ::usage="IModQ[expr] returns True if expr is a valid index-modifier, and False otherwise.";


(* Sorting g-indices *)
IndexSort::usage="IndexSort[list] sorts the elements of list (assumed to be g-indices) according to three priorities set up by SetIndexSortPriorities. Currently: ";
IndexOrderedQ::usage="IndexOrderedQ[{i1, i2, ...}] gives True if the list of g-indices is sorted according to IndexSort and False otherwise.";
DisorderedPairQ::usage="DisorderedPairQ[i1, i2] gives True if the list of indices {i1, i2} is not in canonical order according to IndexSort, and False otherwise.";
SetIndexSortPriorities::usage="SetIndexSortPriorities[p1, p2, p3] sets the three priorities for g-index sorting. The pi are strings among these: priorities p1 and p2 are chosen from two of the pairs `up\.b4/`down\.b4 and `free\.b4/`dummy\.b4. Priority 3 can be chosen from one of the pairs `lexicographic\.b4/`antilexicographic\.b4 and `positional\.b4/`antipositional\.b4.";


(* Finding indices *)
FindIndices::usage="FindIndices[expr] returns an IndexList with the (order kept) union of all-gindices of expr. Indices in directional arguments, Scalar expressions or scalar-functions are not found. FindIndices has attribute HoldFirst, and hence requires very often the use of Evaluate.";
AnyIndices::usage="AnyIndices is an internal symbol of xTensor returned by the expression FindIndices[0]. AnyIndices[vbundle] also represents an arbitrary sequence of abstract indices in the given vbundle.";
$FindIndicesAcceptedHeads::usage="$FindIndicesAcceptedHeas is a global variable giving the list of heads for which FindIndices does not complain and continues looking for indexed objects inside. It is initialized to {Hold}.";
FindFreeIndices::usage="FindFreeIndices[expr] gives an IndexList with the (order not kept) free indices of expr, throwing an error if they are not homogeneous in all terms of expr. FindFreeIndices[expr, selector] uses SelectIndices on FindFreeIndices[expr] with the given selector. FindFreeIndices has attribute HoldFirst, and hence requires very often the use of Evaluate.";
FindDummyIndices::usage="FindDummyIndices[expr] gives an IndexList with the (up-) indices wich are contracted in expr. FindDummyIndices[expr, selector] uses SelectIndices on FindDummyIndices[expr] with the given selector. FindDummyIndices has attribute HoldFirst, and hence requires very often the use of Evaluate.";
FindBlockedIndices::usage="FindBlockedIndices[expr] gives an IndexList with the (order not kept) blocked indices of expr. FindBlockedIndices[expr, selector] uses SelectIndices on FindBlockedIndices[expr] with the given selector. FindBlockedIndices has attribute HoldFirst, and hence requires very often the use of Evaluate.";
IsIndexOf::usage="IsIndexOf[object, index] gives True if (Verbatim) index is one of the indices of object, and False otherwise. IsIndexOf[object, index, tensor] gives IndexOf[object, index] unless object is an inert head expression with head IH; in that case there is the further check ContractThroughQ[IH, tensor].";
IndicesOf::usage="IndicesOf[selectors][expr] returns a list (head IndexList) of the indices in expression selected by the given selectors. Different selectors are applied succesively from left to right. Possible selectors are Free, Dummy, Blocked, Up, Down, AIndex, BIndex, CIndex, DIndex, LIndex, or a manifold, a vbundle, a basis, a tensor (including Basis), or a covd.";


(* Replacing indices *)
ReplaceIndex::usage="ReplaceIndex[expr, rule] replaces indices of objects in expr as given by rule (a single rule or a list of rules of the form i1->i2). Note that the rules a->b and -a->-b are independent and both must be given if this is what we want. ReplaceIndex[expr, {{rule1, ...}, {rule2, ...}, ...}] is automatically threaded, giving a list of expressions.";
ReplaceDummies::usage="ReplaceDummies[expr] replaces dummy indices in expr by new (dollar-) dummy indices. ReplaceDummies[expr, inds] chooses new indices from inds before starting generation of new indices.";
SameDummies::usage="SameDummies[expr] returns expr minimizing the number of different dummies used among the summands of the expr. No new dummy is introduced.";
$ComputeNewDummies::usage="$ComputeNewDummies is a global Boolean variable which specifies whether new (dollar-) indices must be generated during the action of ReplaceDummies.";
SplitIndex::usage="SplitIndex[expr, splitrules] returns an array whose elements are expr with certain indices replaced as given by the splitrules. A split-rule must be of the form index -> Indexlist[i1, i2, ...], meaning that index is to be replaced by i1, i2, ... respectively in each of the terms of the array. It is possible to give a list of splitrules or a compacted form IndexList[index1, index2, ...] -> IndexList[i1 ,i2, ...], where each of the indices indexi is to be splitted as IndexList[i1, i2, ...].";
TraceDummy::usage="TraceDummy[expr, trules] traces dummy indices in expr as given by the trace-rules trules. These are rules of the form index -> IndexList[i1, i2, ...], where index can be a pattern.";
$TraceDummyVerbose::usage="$TraceDummyVerbose is a global Boolean variable turning on and off log messages from the command TraceDummy.";


(* Formatting indices *)
IndexForm::usage="IndexForm[index] returns the string which will be used to format the generalized index (assumed not to have a minus sign in front). By default, abstract indices are simply converted into a string, basis and component indices are colored according to the basis they belong to, directional indices are converted into the name of the vector or into \"#\" when they are composite expressions, and patterns are underlined. (Component indices are dealt with through the associated function CIndexForm.) If changed, IndexForm must be manipulated with great care.";
$CIndexForm::usage="$CIndexForm is a Boolean global variable. If True the coordinate indices are represented in tensor slots as the name of the associated coordinate. If False (the default) no such replacemente is performed.";
CIndexForm::usage="CIndexForm[cnumber, chart] returns the string to be used to represent the coordinate index {cnumber, chart} in a tensor slot.";
PrimeDagger::usage="PrimeDagger[ind] for a daggered index ind returns the formatting of the index replacing the dagger by a prime.";


(* Manifolds *)
IndexRange::usage="IndexRange[a, p] and IndexRange[{a, p}] return the list of symbols between symbols a and p (using CharacterRange internally). IndexRange[{a1, p1}, {a2, p2}, ...] returns the union (using Union, and hence sorted) of the ranges defined by the given pairs of indices.";
DefManifold::usage="DefManifold[M, dim, {a, b, c,...}] defines M to be an n-dimensional differentiable manifold with dimension dim (a positive integer or a constant symbol) and tensor abstract indices a, b, c, ... .  DefManifold[M, {M1, ..., Mm}, {a, b, c,...}] defines M to be the product manifold of previously defined manifolds M1 ... Mm. For backward compatibility dim can be a list of positive integers, whose length is interpreted as the dimension of the defined manifold.";
UndefManifold::usage="UndefManifold[manifold] undefines manifold.";
DimOfManifold::usage="DimOfManifold[M] yields the dimension of manifold M.";

(* Cartesian products *)
CartesianProduct::usage="CartesianProduct[O1, O2, ...] represents the Cartesian product of the objects Oi.";

(* Manifold splitting *)
SplitManifold::usage="SplitManifold[M, {m1, ..., mn}] identifies the manifold M as the (external) Cartesian product of the manifolds mi. Both the M manifold and the mi manifolds are assumed to exist already. The dimension of M must coincide with the sum of the dimensions of the mi manifolds.";
SplittingsOfManifold::usage="SplittingsOfManifold[M] returns a list of known CartesianProduct splittings of the manifold M.";
SubmanifoldsOfManifold::usage="SubmanifoldsOfManifold has been removed from xTensor in version 1.1.0. Please use SplittingsOfManifold for similar functionality.";
SubmanifoldQ::usage="SubmanifoldQ[large, small] gives True if manifold small is contained in or equal to manifold large, and False otherwise.";
DisjointManifoldsQ::usage="DisjointManifoldsQ[manifolds1, manifolds2] gives True if none of the manifolds of the list manifolds1 is a submanifold or a supermanifold of the manifolds of the list manifolds2, and False otherwise.";
DisjointVBundlesQ::usage="DisjointVBundlesQ[vbundles1, vbundles2] gives True if none of the vbundles of the list vbundles1 is a subvbundle or a supervbundle of the vbundles of the list vbundles2, and False otherwise.";


(* Dependencies *)
DependenciesOf::usage="DependenciesOf[expr] gives the list of manifolds and parameters upon which expr depends.";
ManifoldsOf::usage="ManifoldsOf[expr] gives the list of manifolds on which expr lives.";
ParametersOf::usage="ParametersOf[expr] gives the list of parameters on which expr depends.";
AnyDependencies::usage="AnyDependencies is an internal symbol of xTensor returned by DependenciesOf acting on an unknown expression (unregistered symbols, patterns, etc.).";
OtherDependencies::usage="OtherDependencies is an option for DefCovD and DefMetric specifying that all derivatives and tensors defined have the given list of additional dependencies (manifolds and parameters) and not only the manifold on which the derivative or metric are defined.";


(* Boundaries *)
ManifoldBoundary::usage="ManifoldBoundary[M] represents the manifold (without boundary) of the manifold (with boundary) M.";
EmptyManifold::usage="EmptyManifold is a manifold with no points. ManifoldBoundary[M] of a manifold M without boundary returns EmptyManifold.";


(* Mappings *)
DefMapping::usage="DefMapping[phi, M1->M2] defines phi to be a mapping having manifold M1 as domain and manifold M2 as image.";
UndefMapping::usage="UndefMapping[phi] undefines the mapping phi.";
MappingDomain::usage="MappingDomain[phi] returns the domain manifold of the mapping phi.";
MappingImage::usage="MappingImage[phi] returns the image manifold of the mapping phi, in general a supermanifold of phi[domain].";
ImmersionQ::usage="ImmersionQ is a Boolean option of DefMapping. ImmersionQ[phi] returns True if the mapping phi is an immersion on its image, and False otherwise.";
SubmersionQ::usage="SubmersionsQ is a Booleann option of DefMapping. SubmersionQ[phi] returns True if the mapping phi is a submersion on its image, and False otherwise.";
DiffeomorphismQ::usage="DiffeomorphismQ[phi] returns True if the mapping phi is a local diffeomorphism between its domain and image, and False otherwise.";
InverseMapping::usage="InverseMapping[phi] returns the inverse of the mapping phi, if phi is invertible (at least left or right invertible), and throws an error otherwise.";
IdentityMapping::usage="IdentityMapping[M] represents the identity mapping from the manifold M to itself.";


(* Vector bundles *)
DefVBundle::usage="DefVBundle[vbundle, M, dim, {a, b, c,...}] defines vbundle to be a vector bundle with base manifold M and fiber vector space with dimension given by dim (a positive integer) and represented by the abstract indices {a, b, c, ...}.";
UndefVBundle::usage="UndefVBundle[vbundle] undefines the vector bundle vbundle.";
AnyVBundles::usage="AnyVBundles is an internal symbol of xTensor returned by SlotsOfTensor acting on the Zero tensor.";
BaseOfVBundle::usage="BaseOfVBundle[vbundle] gives the base manifold of the vector bundle vbundle.";
Tangent::usage="Tangent[M] returns the tangent bundle of the manifold M. For a symbolic M the name of the bundle is constructed by default prepending the name of the manifold with Tangent.\n\nTangent is also an option for DefManifold, giving the name of the tangent bundle.";
TangentBundleOfManifold::usage="TangentBundleOfManifold = Tangent. Kept for backward compatibility.";
IndicesOfVBundle::usage="IndicesOfVBundle[vbundle] returns the abstract indices associated to vector bundle vbundle. There are two lists: that of the registered indices and that of the internally generated indices.";
LastIndex::usage="LastIndex[vbundle] gives a pair {i, n} such that dollar-indices will be formed using the symbol i (see function DummyIn) and computer-generated indices will be formed using the symbol i and the integer n (see function NewIndexIn). LastIndex[vbundle, map] given the corresponding pair for the pullback vbundle PullBackVBundle[vbundle, map].";
DimOfVBundle::usage="DimOfVBundle[vbundle] yields the dimension of vector bundle vbundle.";
MetricEndowedQ::usage="MetricEndowedQ[vbundle] gives True if the vector bundle vbundle has at least one registered metric (which will be contained in MetricsOfVBundle[vbundle]) or False otherwise.";
MetricsOfVBundle::usage="MetricsOfVBundle[vbundle] gives the list of metrics that have been defined on the vector bundle vbundle. Only the first one will be used to raise and lower indices.";

(* Direct sums  and tensor products*)
CirclePlus::usage="CirclePlus[vb1, vb2, ...] represents the direct sum of the vector bundles vb1, vb2, ...";
CircleTimes::usage="CircleTimes[O1, O2, ...] represents the tensor product of the objects O1, O2, ...";

(* VBundle splitting *)
SplitVBundle::usage="SplitVBundle[V, {v1, ..., vn}] identifies the vbundle V as the direct sum of the vbundles vi. Both the V vbundle and the vi vbundles are assumed to exist already. The dimension of V must coincide with the sum of the dimensions of the vi vbundles.";
SplittingsOfVBundle::usage="SplittingsOfVBundle[vb] returns a list of known direct sum splittings of the vbundle vb.";
SubvbundlesOfVBundle::usage="SubvbundlesOfVBundle has been removed from xTensor in version 1.1.0. Please use SplittingsOfVBundle for similar functionality.";
SubvbundleQ::usage="SubvbundleQ[large, small] gives True if vector bundle small is contained in or equal to vector bundle large, and False otherwise.";
SubdummiesIn::usage="SubdummiesIn[vbundle] gives a list of unique (dollar) indices on the respective subvbundles of vbundle.";
SetSplitTensors::usage="SetSplitTensors[vbundle, Tlist] declares the tensors in the list Tlist to be projectors and their duals for the given vbundle, that must have been declared previously as a sum vbundle (using either DefVBundle or DefManifold). Currently xTensor can only decompose into two subvbundle, and hence Tlist must contain four tensors with character {down, up}, in any order. ";
SplitTensor::usage="SplitTensor[T] decomposes the tensor T as a sum of projected parts using the projectors declared with SetSplitTensors.";

Project::usage="Project[expr, P, inds] project the indices inds with the projector P, by explicit multiplication of expr by copies of P. The inds argument admits a single index, and IndexList or an IndicesOf expression.";
Projected::usage="Projected[T, {P, Q, ...}][a, b, ...] represents the tensor T projected with projector P in index a, Q in index b, ... Non-projected slots are denoted by the formal projector delta. Each projector can be replaced by a sum of projectors. The whole list of projectors can be replaced by a single projector or sum of projectors acting on all indices of the expression.";
ProjectorQ::usage="ProjectorQ[P] returns True if tensor P has been declared as a projector with MakeProjectors, and False otherwise.";
MakeProjectors::usage="MakeProjectors[{P1[-a, b], ..., Pn[-a, b]}, {dim1, ..., dimn}] defines tensors P1, ..., Pn, assumed to existe already, to be complementary projectors on the vbundle of the indices -a, b. Projector Pi projects onto a subspace of dimension dimi. The option OrthogonalWithMetric specifies a metric with respect to which the projectors are orthogonal.";
OrthogonalWithMetric::usage="OrthogonalWithMetric is an option of MakeProjectors specifiying the metric with respect to which a given set of complementary projectors are also a set of orthogonal projectors. The default value Null represents no metric.";
$ProjectedFormat::usage="$ProjectedFormat is a Boolean global variable selecting the type of formatting of indices of Projected tensors. With False the projectors are staggered with the indices, in gray font. With True the projectors are placed on top of upper indices or underneath lower indices. This should not be a Boolean variable...";
ContractProjector::usage="ContractProjector[expr, P] transforms products of projector P with other tensors in expr into Projected notation. ContractProjector[expr] contracts all projectors.";
FromProjected::usage="FromProjected[expr] converts Projected expressions into explicit products of tensors and projectors.";

(* Indices *)
VBundleOfIndex::usage="VBundleOfIndex[i] gives the vector bundle to which the g-index i belongs. Otherwise it throws an error message.";
RegisterIndices::usage="RegisterIndices[vb, inds] associates the given list inds of abstract indices to the vbundle vb.";
DummyIn::usage="DummyIn[vbundle] gives a unique abstract dollar-index on the vector bundle vbundle, using the last of the user-defined indices.";
DummyAs::usage="DummyAs[ind] gives a dummy abstract index in the vector bundle of ind, having the same up/down character.";
AddIndices::usage="AddIndices[vbundle, indlist] adds the abstract indices in indlist to the user-defined part of the indices associated to the the vector bundle vbundle.";
RemoveIndices::usage="RemoveIndices[vbundle, indlist] removes the abstract indices in indlist from the user-defined part of the indices associated to the vector bundle vbundle.";
NewIndexIn::usage="NewIndexIn[vbundle] gives and registers a new computer-defined abstract index associated to the vector bundle vbundle. The index is constructed using the last index of the user-defined part and a sequential number starting at 1. This is an internal function, and users are expected to use GetIndicesOfVBundle instead.";
GetIndicesOfVBundle::usage="GetIndicesOfVBundle[vbundle, n, list] gives a list of n abstract indices on the vector bundle vbundle, checking that they are not in list. If there are not enough indices in IndicesOfVBundle[vbundle] then new indices are generated using NewIndexIn.";


(* Tracing *)
TraceProductDummy::usage="TraceProductDummy[expr, index] expands the dummy index of a sum-vbundle in expr to dummy indices of its subvbundles. TraceProductDummy[expr] expands all dummies. TraceProductDummy[expr, {i1, i2, ...}] expands the dummies i1, i2,... TraceProductDummy[expr, {VBundle[VB1], i1, ...}] expands the indices i1,... and all dummies of the sum-vbundle VB1, ...";


DefConstantSymbol::usage="DefConstantSymbol[symbol] defines symbol to be a constant with respect to all derivatives (covariant derivatives, Lie derivatives, variational derivatives and parameter derivatives).";
UndefConstantSymbol::usage="UndefConstantSymbol[symbol] undefines the constant symbol.";


$TensorBoxes::usage="$TensorBoxes is a global variable specifying the internal notation to be used for indexed tensors. From version 1.0 of xTensor the default is \"GridBox\" but before it was \"ScriptBox\".";


(* Tensors *)
DefTensor::usage="DefTensor[T[-a, b, c, ...], {M1, ...}] defines T to be a tensor field on manifolds and parameters M1,... and the base manifolds associated to the vector bundles of its indices. DefTensor[T[-a, b, c, ...], {M1, ...}, symmetry] defines a tensor with symmetry given by a generating set or strong generating set of the associated permutation group.";
UndefTensor::usage="UndefTensor[tensor] undefines tensor.";
VanishingQ::usage="VanishingQ is a Boolean option for DefTensor stating that the defined tensor must be given a zero value.";
ForceSymmetries::usage="ForceSymmetries is a Boolean option for DefTensor. If True symmetries are registered even if involved indices do not have the same up/down character.";
FrobeniusQ::usage="FrobeniusQ is a Boolean option for DefTensor stating that the defined vector obeys the Frobenius condition with every torsionless covariant derivative.";
SetOrthogonal::usage="SetOrthogonal[tensor[inds], vector[ind]] defines automatic rules for tensor such that tensor[inds]*vector[ind] is zero. SetOrthogonal[tensor[inds], vector] defines those rules for all abstract indices of tensor[inds].";
OrthogonalTo::usage="OrthogonalTo is an option for several DefType functions. For DefTensor it gives a list of vectors which are orthogonal to the tensor defined, one for each index. For DefCovD it gives a vector which is orthogonal to the whole derivative.";
SetProjected::usage="SetProjected[tensor[inds], proj[i1, i2]] defines automatic rules for tensor such that tensor[inds]*proj[i1, i2] returns tensor again, but with new indices. SetProjected[tensor[inds], proj] defines those rules for all abstract indices of tensor[inds].";
ProjectedWith::usage="ProjectedWith is an option for several Def functions. For DefTensor it gives a list of 2-tensors which project the tensor defined into itself. For DefCovD it gives a projector for the whole derivative.";
OrthogonalToVectorQ::usage="OrthogonalToVectorQ[v][T] for a vector v and a tensor T returns True if T is orthogonal in all its slots to v, and False otherwise. The value is remembered as un upvalue of T.\n\nOrthogonalToVectorQ[v][expr] for a vector v and an indexed expression expr returns True if the expression is orthogonal in all its slots to v, and False otherwise. The value is not remembered.";
SymmetryGroupOfTensor::usage="SymmetryGroupOfTensor[tensor] gives the generating set or strong generating set of the symmetries of tensor, using Cycles notation on the numeric slots of the tensor.";
SymmetryTableauxOfTensor::usage="SymmetryTableauxOfTensor[tensor] gives the list of symmetry tableaux of tensor, using numeric slots.";
SlotsOfTensor::usage="SlotsOfTensor[tensor] gives the list of vector bundles with up/down marks corresponding to the slots of tensor as it was defined. Slots related by the symmetries of tensor must belong to the same vector bundle and must have the same up/down character, unless the option ForceSymmetries is used.";
TensorID::usage="TensorID[tensor] returns a list with information on how to compute the components of the tensor.";
WeightOfTensor::usage="WeightOfTensor[tensor] gives the weight of tensor as a tensorial density. It is always a linear combination of names of bases. It is 0 if tensor is not a density. Upvalues for tensor are stored only if tensor is a density. WeightOfTensor is also an option for DefTensor giving the weight of the defined tensor.";
WeightOf::usage="WeightOf[expr] returns the density weight of the expression. The weight of the basis-determinant of the metric is defined to be 2*basis.";
DependenciesOfTensor::usage="DependenciesOfTensor[tensor] gives the list of manifolds and parameters on which the tensor depends.";

(* Other *)
Zero::usage="Zero[e1, ..., en] is 0 no matter what the arguments are.";
HeadOfTensor::usage="HeadOfTensor[T[a, b, ...], {a, b, ...}] returns T. HeadOfTensor[k T[a, b, ...], {a, b, ...}] returns MultipliyHead[k, T].";
MultiplyHead::usage="MultiplyHead[k, T][a, b, ...] returns k T[a, b, ...].";
delta::usage="delta[-a, b] represents the identity tensor on any vector bundle. In the presence of a metric the object delta[b, -a] is also allowed, being related to the former as given by that metric.";
Gdelta::usage="Gdelta[-a1, ..., -an, b1, ..., bn] represents the generalized delta tensor on any vector bundle.";
ExpandGdelta::usage="ExpandGdelta[expr] expands Gdelta as a linear combination of products of delta.";
Sdelta::usage="Sdelta[sym][inds] represents a tensor with symmetry sym in the first half of its slots and symmetry sym in the second half of its slots.";
ExpandSdelta::usage="ExpandSdelta[expr] expands Sdelta[sym] as a linear combination of products of delta.";
SeparateDir::usage="SeparateDir[expr] converts all directional indices (expressions Dir[v]) in expr into products of the direction v and the corresponding tensor.";
ContractDir::usage="ContractDir[expr, v] converts contracted vectors v into Dir[v[.]] expressions in expr.";


(* PushForward and PullBack *)
PullBack::usage="PullBack[expr, phi, firules] computes the pull back of the tensor expression expr along the mapping phi, changing free indices of expr into free indices of the domain tangent vbundle as given by the list of rules firules."
PullBackVBundle::usage="PullBackVBundle[vb, phi] represents the pull back of the vector bundle bun under the mapping phi. It is also treated as a (nonatomic) vbundle.";
Precompose::usage="Precompose[T, phi] represents the precomposition of the tensor field T with the mapping phi, a section of a pullback vbundle. The result has dependencies given by the domain of the mapping phi.";
PullBackTensor::usage="PullBackTensor[T, phi] represents the pull back of the tensor T under the mapping phi. It is also treated as a (nonatomic) tensor.";
PullBackCovD::usage="PullBackCovD[CD, phi] represents the pull back of the covariant derivative CD under the mapping phi. It is also treated as a (nonatomic) covariant derivative.";
ExpandPullBack::usage="ExpandPullBack[expr] expands the PullBackTensor[T, phi] expressions into a contraction of Precompose[T, phi] with explicit dphi or Inv[dphi] factors.";
TangentTensor::usage="TangentTensor[phi] returns the tangent (or differential) of the mapping phi. This is a 2-tensor along the mapping phi, with its first slot being the cotangent vbundle of the domain of phi and the second slot being the pullback of the tangent bundle of the image of phi.";
PushForward::usage="PushForward[expr, phi, firules] computes the push forward of the tensor expression expr along the mapping phi, changing free indices of expr into free indices of the image tangent vbundle as given by the list of rules firules.";
LinearPush::usage="LinearPush[T, dphi] represents a tensor along the mapping phi, whose differential is dphi. It is equivalent to a contraction of the contravariant slots of T with dphi, and of the covariant slots of T with the inverse of dphi.";
InvertibleQ::usage="InvertibleQ[dphi] returns True if dphi is an invertible 2-tensor, and False otherwise. InvertibleQ[dphi, Left] returns True if dphi is left-invertible, and False otherwise. InvertibleQ[dphi, Right] returns True if dphi is right-invertible, and False otherwise.";
PushForwardTensor::usage="PushForwardTensor[T, phi] represents the push forward of the tensor T under the mapping phi. It is also treated as a (nonatomic) tensor.";
ExpandPushForward::usage="ExpandPushForward[expr] expands the PushForwardTensor[T, phi] expressions into a contraction of Precompose[T, iphi] with explicit diphi or Inv[diphi] factors, where iphi is the inverse mapping of phi.";
At::usage="At[texpr, sub] represents the restriction of the tensorial expression texpr to the submanifold sub, which may be a point.";


(* Rules *)
PermuteIndices::usage="PermuteIndices[expr, indices, perm] permutes the indices of expr given by the list indices (head IndexList) with the permutation perm, in general a linear combination of permutations.";
MakeRule::usage="MakeRule[{LHS, RHS}] gives a list of delayed rules with left and right hand sides as given by LHS and RHS respectively, but moving indices to consider all possible equivalent rules. See notes for its options PatternIndices, MetricOn, UseSymmetries and TestIndices. MakeRule[{LHS, RHS, conditions}] adds conditions to the rules.";
ToRule::ToRule="ToRule[lhs==rhs] evaluates the given equation and returns a rule lhs:>Module[{..}, rhs] constructed with MakeRule. ToRule also accepts rules and lists in input, and takes all options of MakeRule.";
AutomaticRules::usage="AutomaticRules[symbol, {rule1, rule2, ...}] declares the rules as upvalues or downvalues for symbol, or append them to $Rules if symbol is too deep in the rules.";
IndexRule::usage="IndexRule[lhs, rhs] construct a DelayedRule expression between indexed expressions lhs and rhs, evaluating rhs in advance.";
IndexRuleDelayed::usage="IndexRuleDelayed[lhs, rhs] construct a DelayedRule expression between indexed expressions lhs and rhs, without evaluating rhs.";
IndexSet::usage="IndexSet[lhs, rhs] defines a relation between indexed expressions lhs and rhs, evaluating rhs in advance.";
IndexSetDelayed::usage="IndexSet[lhs, rhs] defines a relation between indexed expressions lhs and rhs, withoug evaluating rhs.";
DoubleRightTee::usage="Infix form for IndexRule. Note that it has a very high precedence (much higher than that of = ).";
RightTeeArrow::usage="Infix form for IndexRule. Note that it has a very high precedence (much higher than that of -> ).";
PatternIndices::usage="PatternIndices is an option for MakeRule expressing the indices that will be converted into patterns in the final expression of the rule. Possible values are None (default), All or a list of indices.";
MetricOn::usage="MetricOn is an option for MakeRule. MetricOn->None (default) means that no index can be raised or lowered. MetricOn->All means that all indices can be raised or lowered. MetricOn->{i1, i2, ...} explicitly gives the list of indices that can be raised or lowered with the metric. This option is not relevant if there is no metric.";
TestIndices::usage="TestIndices is a Boolean option for MakeRule. TestIndices->True (default) means that patterntests are added to the indices of the rules to check that they belong to the right vector bundle.";
UseSymmetries::usage="UseSymmetries is a Boolean option for MakeRule. UseSymmetries->True (default) means that symmetries of the tensors are considered to construct the rules.";
ContractMetrics::usage="ContractMetrics is a Boolean option for MakeRule that specifies that all metric objects must be contracted (if possible) in the RHS of the rules. Its default value is False.";Verbose::usage="Verbose is a Boolean option for MakeRule, AutomaticRules and ToCanonical. Verbose->True gives lots of log-info about the internals of the calculation. Its default value is False.";


Leibnitz::usage="Leibnitz[expr][-a, b] gives a sum of copies of expr*delta in which each free upper index has been replaced by b and each lower index has been replaced by -a.";


(* Covariant derivatives *)
$CovDFormat::usage="$CovDFormat is a global variable containing a string with the formatting form of covariant derivatives. Possible values are currently: \"Prefix\" (default) for nabla-like notation, and \"Postfix\" for comma-like notation.";
$ParamDFormat::usage="$ParamDFormat is a global variable containing a string with the formatting form of parametric derivatives. Possible values are currently: \"Prefix\" (default) or \"SinglePrefix\" for partial-like notation, and \"Postfix\" for comma-like notation.";
DefCovD::usage="DefCovD[covd[-a], {post, pre}] defines covd to be a connection on the tangent bundle of index a. DefCovD[covd[-a], vbundle, {post, pre}] defines covd to be a connection on the tangent bundle of index a and the inner vbundle (which must have the same base manifold). The connection will be represented in StandardForm using the character pre in \"Prefix\" notation and the character post in \"Postfix\" notation.";
UndefCovD::usage="UndefCovD[covd] undefines covd.";
PD::usage="PD denotes the fiducial connection without curvature nor torsion (called ordinary derivative by Wald).";
Curvature::usage="Curvature is an option for DefCovD giving whether a newly defined covariant derivative has curvature or not. There are several possible values: Riemann (the connection has curvature only on the tangent bundle), FRiemann (only on the inner bundle, and its conjugate), True (on both vbundles), False (no curvature), a vbundle on which we want to have curvature, or a list of such vbundles. The default value is True.\n\nCurvature is also a Boolean option for DefMetric, saying whether a metric defined on a tangent bundle has a Levi-Civita connection with curvature or not.";
CurvatureRelations::usage="CurvatureRelations is a Boolean option for DefCovD giving whether automatic rules relating the curvature tensors of a newly define covariant derivative should be defined or not. Its default value is True.\n\n CurvatureRelations[covd, Riemann] gives a list of the relations between the contracted Riemann of cov and the Ricci tensor. CurvatureRelations[covd, Ricci] give the corresponding list of relations between the contracted Ricci and the RicciScalar if there is a metric. CurvatureRelations[covd] gives the union of those two lists.";
FromMetric::usage="FromMetric is an option for DefCovD giving the metric associated to the covariant derivative. Use FromMetric -> metric or FromMetric -> Null.";
ExtendedFrom::usage="ExtendedFrom is an option for DefCovD (generally used when defining a covd on an inner vbundle), specifying that the Riemann, Ricci, Christoffel and Torsion tensors must be shared with another, already existing, covd.";
WeightedWithBasis::usage="WeightedWithBasis is an option for DefCovD (though typically used through DefMetric) to extend a Levi-Civita connection on densities in a given basis (which can be AIndex as special case). Its default value is Null.\n\nWeightedBasis[covd] returns the basis along which the Levi-Civita connection covd has been extended, or Null otherwise.";
SymmetryGroupOfCovD::usage="SymmetryGroupOfCovD[covd] gives the symmetry group of the covariant derivative covd when it acts as a multiple index derivative.";
DependenciesOfCovD::usage="DependenciesOfCovD[covd] gives the list of dependencies (manifolds and parameters) on which the covariant derivative covd depends.";


(* Associated tensors *)
Christoffel::usage="Christoffel[covd1, covd2][a, -b, -c] returns the Christoffel tensor relating the connections covd1 and covd2, and defines it if it does not exist yet. Christoffel[covd][a, -b, -c] is equivalent to Christoffel[covd, PD][a, -b, -c].";
AChristoffel::usage="AChristoffel[covd1, covd2][A, -b, -C] returns the Christoffel tensor relating the connections covd1 and covd2 on an inner vbundle, and defines it if it does not exist yet. This proceeds by immediate conversion of AChristoffel into Christoffel. AChristoffel[covd][A, -b, -C} is equivalent to AChristoffel[covd, PD][A, -b, -C].";
Torsion::usage="Torsion[covd] returns the torsion tensor associated to a connection covd.\n\nTorsion is also a Boolean option for DefCovD that specifies whether the defined covariant derivative has torsion or not. Its default value is False.";
Riemann::usage="Riemann[covd] returns the Riemann curvature tensor associated to a connection covd acting on a tangent bundle.";
FRiemann::usage="FRiemann[covd] returns the Riemann curvature tensor associated to a connection covd acting on an inner (not tangent) vector bundle.";
RiemannDown::usage="RiemannDown[covd] returns the Riemann curvature tensor associated to the connection covd of a frozen metric, with its four indices being lower indices, so that it has full Riemann symmetry.";
Ricci::usage="Ricci[covd] returns the Ricci curvature tensor associated to a connection acting on a tangent bundle.";
TFRicci::usage="TFRicci[covd] returns the Traceless Ricci curvature tensor associated to a connection covd acting on a tangent bundle.";
RicciScalar::usage="RicciScalar[covd] returns the Ricci curvature scalar associated to a metric-compatible connection covd acting on a tangent bundle.";
Einstein::usage="Einstein[covd] returns the Einstein curvature tensor associated to a metric-compatible connection covd acting on a tangent bundle.";
Weyl::usage="Weyl[covd[ returns the Weyl curvature tensor associated to a metric-compatible connection covd acting on a tangent bundle.";
Kretschmann::usage="Kretschmann[covd] returns the Kretschmann curvature scalar associated to a metric-compatible connection covd acting on a tangent bundle.";
LC::usage="LC[g] returns the Levi-Civita covariant derivative associated to the metric g.";

Implode::usage="Implode[expr, covd] converts covd derivatives of tensors into new tensors formed from the names of the tensor and the covd.";
$ImplodeInfoQ="$ImplodeInfoQ is a boolean global variable which specifies whether definition of imploded tensors is to be reported or not.";
Explode::usage="Explode[expr, covd] converts imploded tensors into their corresponding form with explicit covd operators. Metric factors may appear in this process.";

epsilon::usage="epsilon[g] returns the totally antisymmetric tensor associate to the metric g.";
ExtrinsicK::usage="ExtrinsicK[h] returns the extrinsic curvature tensor associated a the induced metric h.";
Acceleration::usage="Acceleration[n] returns the acceleration vector associated to the vector n, registered as the vector orthogonal to some induced metric.";
Projector::usage="Projector[h] returns the projector tensor on the tangent subvbundle defined by the induced metric h.";


(* Info on covds *)
CurvatureQ::usage="CurvatureQ[covd] gives True if covd has been defined as a covariant derivative with curvature and False otherwise.";
TorsionQ::usage="TorsionQ[covd] gives True if covd has been defined as a covariant derivative with torsion and False otherwise.";
SymbolOfCovD::usage="SymbolOfCovD[covd] gives the pair of strings that represent the covariant derivative covd in StandardForm in formats \"Postfix\" and \"Prefix\" respectively.";
ManifoldOfCovD::usage="ManifoldOfCovD[covd] gives the manifold where the covariant derivative covd lives. It is always a dependency of covd.";
VBundlesOfCovD::usage="VBundlesOfCovD[covd] gives a list of vector bundles on which the derivative acts. The list can contain: one element (a tangent vector bundle), two elements (a tangent vector bundle and an inner real vector bundle) or three elements (a tangent bundle and an inner complex bundle with its conjugate).";
MetricOfCovD::usage="MetricOfCovD[covd] gives the metric from which covd derives, if covd is a metric covariant derivative. Otherwise it gives Null.";


(* Manipulation of covds *)
ContractCurvature::usage="ContractCurvature[expr] replaces all contracted Riemann tensors by Ricci and all contracted Ricci tensors by RicciScalar. ContractCurvature[expr, covd] does it only for the given covariant derivative. ContractCurvature[expr, covd, Riemann] only replaces Riemann by Ricci. ContractCurvature[expr, covd, Ricci] only replaces Ricci by RicciScalar.";
CheckZeroDerivative::usage="CheckZeroDerivative[expr] performs some checks on expr to see if there are derivatives of some manifolds acting on fields on other manifolds, which hence can be removed.";
$CheckZeroDerivativeVerbose::usage="$CheckZeroDerivativeVerbose is a Boolean global variable which says whether the action of CheckZeroDerivative must be reported or not. By default it is set to False.";
CheckZeroDerivativeStart::usage="CheckZeroDerivativeStart[covd] starts automatic zero checking of derivative covd.";
CheckZeroDerivativeStop::usage="CheckZeroDerivativeStop[covd] stops automatic zero checking of derivative covd.";
BreakChristoffel::usage="BreakChristoffel[expr, chr, covd] transforms all instances of chr[inds] (from covd1 to covd2) in expr into Christoffel[covd1, covd][inds]-Christoffel[covd2, covd][inds]. BreakChristoffel[expr, chr] is converted into BreakChristoffel[expr, chr, PD]. BreakChristoffel[expr, covd] breaks all Christoffel tensors using covd as intermediate connection. BreakChristoffel[expr] is converted into BreakChristoffel[expr, PD].";
ChangeTorsion::usage="ChangeTorsion[expr, covd1, covd2] changes the torsion tensor of covd1 by the torsion tensor of covd2 using the antisymmetric part of the Christoffel relating covd1 and covd2. The second argument is listable. ChangeTorsion[expr, covd1] is converted into ChangeTorsion[expr, covd1, PD]. ChangeTorsion[expr] is converted into ChangeTorsion[expr, $CovDs].";
TorsionToChristoffel::usage="TorsionToChristoffel = ChangeTorsion. Kept for backwards compatibility.";
ChangeCovD::usage="ChangeCovD[expr, covd1, covd2] changes any instance of the covariant derivative covd1 in expr into a covariant derivative covd2 and Christoffel tensors relating both connections. The second argument is listable. ChangeCovD[expr, covd1] is converted into ChangeCovD[expr, covd1, PD]. ChangeCovD[expr] is converted into ChangeCovD[expr, $CovDs].";
CovDToChristoffel::usage="CovDToChristoffel = ChangeCovD. Kept for backwards compatibility.";
ChangeCurvature::usage="ChangeCurvature[expr, covd1, covd2] changes the curvature tensors (Riemann, Ricci, RicciScalar and FRiemann) of covd1 into the curvature tensors of covd2 using the Christoffels relating covd1 and covd2. The second argument is listable. ChangeCurvature[expr, covd1] is converted into ChangeCurvature[expr, covd1, PD]. ChangeCurvature[expr] is converted into ChangeCurvature[expr, $CovDs].";
RiemannToChristoffel::usage="RiemannToChristoffel = ChangeCurvature. Kept for backwards compatibility.";
RiemannToWeyl::usage="RiemannToWeyl[expr, covd] expands expr expressing all Riemann tensors of covd in terms of the Weyl and other tensors of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. RiemannToWeyl[expr] expands all Riemann tensors.";
WeylToRiemann::usage="WeylToRiemann[expr, covd] expands expr expressing all Weyl tensors of covd in terms of the Riemann and other tensors of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. WeylToRiemann[expr] expands all Weyl tensors.";
RicciToTFRicci::usage="RicciToTFRicci[expr, covd] expands expr expressing all Ricci tensors of covd in terms of the TFRicci and other tensors of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. RicciToTFRicci[expr] expands all Ricci tensors.";
TFRicciToRicci::usage="TFRicciToRicci[expr, covd] expands expr expressing all TFRicci tensors of covd in terms of the Ricci and other tensors of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. TFRicciToRicci[expr] expands all TFRicci tensors.";
RicciToEinstein::usage="RicciToEinstein[expr, covd] expands expr expressing all Ricci tensors of covd in terms of the Einstein and RicciScalar tensors of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. RicciToEinstein[expr] expands all Ricci tensors.";
EinsteinToRicci::usage="EinsteinToRicci[expr, covd] expands expr expressing all Einstein tensors of covd in terms of the Ricci and RicciScalar tensors of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. EinsteinToRicci[expr] expands all Einstein tensors.";
RiemannToRiemannDown::usage="RiemannToRiemannDown[expr, covd] expands expr expressing all Riemann tensors of covd (the Levi-Civita derivative of a frozen metric) in terms of the RiemannDown tensor of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. RiemannToRiemannDown[expr] expands all Riemann tensors of frozen metrics.";
RiemannDownToRiemann::usage="RiemannDownToRiemann[expr, covd] expands expr expressing all RiemannDown tensors of covd (the Levi-Civita derivative of a frozen metric) in terms of the Riemann tensor of covd. If the second argument is a list of covariant derivatives the command is applied sequentially on expr. RiemannDownToRiemann[expr] expands all RiemannDown tensors of frozen metrics.";


(* Parametric derivatives *)
OverDot::usage="OverDot[expr] represents a simple ordinary derivative of expr with respect to some internal parameter (say time) which is independent of the coordinates of the manifolds where expr live. It commutes with partial derivatives, but not with other covariant derivatives or with Lie derivatives because in general the vector field of the Lie derivative could depend upon the parameter as well.";
ParamD::usage="ParamD[params][expr] represents the multiple ordinary derivative of expr with respect to the sequence params of parameters, automatically sorted into canonical order.";


(* Sorting derivatives *)
SortCovDs::usage="SortCovDs[expr, covd] exchange repeatedly two instances of the covariant derivative covd, introducing Riemann tensors if needed, until the indices of the derivatives are sorted in canonical order. ";
CommuteCovDs::usage="CommuteCovDs[expr, covd, {a, b}] converts subexpressions covd[b]@covd[a]@expr1 of expr into covd[a]@covd[b]@expr1 plus Riemann terms linear in expr1.";
SortCovDsStart::usage="SortCovDsStart[covd] automatizes the commutation of covariant derivative covd.";
SortCovDsStop::usage="SortCovDsStop[covd] removes automatic commutation of covariant derivative covd.";


TensorD::usage="TensorD[expr, der1, der2, ...] computes the given derivatives of expr, returning the result using TensorDerivative notation. The deri can be either covariant derivatives or Lie derivatives or parametric derivatives.";
TensorDerivative::usage="TensorDerivative[T, der] represents the name of the tensor obtained by differentitation of the tensor T. The derivative der can be either a covariant derivative or a Lie derivative or a parametric derivative. TensorDerivative[T, der1, der2, ...] represents a high-order derivative.";
ToTensorDerivative::usage="ToTensorDerivative[expr] converts derivatives in expr from standard covd[-a][T[b,c]] notation to TensorDerivative[T,covd][b,c,-a] notation. Note that the derivative index is placed last in the latter notation.";
FromTensorDerivative::usage="FromTensorDerivative[expr] converts derivatives in expr from TensorDerivative[T,covd][b,c,-a] notation to standard covd[-a][T[b,c]] notation. Note the the derivative index is the last in the former notation.";


(* Lie derivatives *)
LieD::usage="LieD[v][expr] represents the Lie derivative of expr along the contravariant vector field v. LieD[v, covd][expr] computes the Lie derivative of expr using the covariant derivative covd.";
Bracket::usage="Bracket[v1, v2][i] represents the Lie Bracket of vector fields v1 and v2, giving a vector field with index i.";
LieDToCovD::usage="LieDToCovD[expr, covd] transforms LieD[v][expr1] into LieD[v, covd][expr1] so that the Lie derivative of expr1 along v is computed in terms of the covariant derivative covd.";
DirCovDToLieD::usage="DirCovDToLieD[expr, v] changes all directional derivatives along the vector v to Lie derivatives along the same vector field. The vector v is the name of the vector, without an index.";
BracketToCovD::usage="BracketToCovD[expr, covd] changes all Lie brackets in expr by their expressions in terms of the derivative covd (with optional value PD).";


(* Variational derivatives *)
VarD::usage="VarD[T, covd][expr, rest] computes the variational derivative of expr*rest with respect to the tensor field T. Integrations by parts will be performed with the covariant derivative covd.";
ImplicitTensorDepQ::usage="ImplicitTensorDepQ[T1, T2] returns False if tensor T1 does not depend implicitly on tensor T2, an hence the variational derivative of T1 with respect to T2 is zero. This is the default behaviour for any two different tensors. The trivial subcase ImplicitTensorDepQ[T, T] returns True. Nontrivial tensor dependencies must be set by hand, if possible as upvalues for T1. ";


(* Stripping *)
ToStripped::usage="ToStripped[expr] returns the stripped form of the expression expr.";
FromStripped::usage="FromStripped[sexpr] converts back the stripped expression sexpr into a standard expression.";
Stripped::usage="Stripped[T, t][inds] represents the stripped form of the object t, of type T, with indices inds";


(* Monomials *)
BreakInMonomials::usage="BreakInMonomials[expr] returns expr with terms separated into products of monomials (head Monomial) and scalars.";
Monomial::usage="Monomial[expr] represents a monomial (a product of objects sharing contracted indices).";


(* Canonicalization *)
Symmetry::usage="Symmetry is the head of symmetry expressions.";
xSort::usage="xSort[expr] returns a unique sorted form of the expression expr, using the internal Object notation.";
UxSort::usage="UxSort[object] converts back the object, given in internal Object notation, into a standard tensorial expression.";
xSortPrecedence::usage="xSortPrecedence[t] returns an integer value that determines the precedence of tensor t in xSort. Precedence values are sorted with Sort and hence smaller values correspond to earlier places in the sorted list.";
$MixedDers="$MixedDers is a global variable stating whether the canonicalization process must take into account the fact that there are connections different from that of the metric. The initial default value is True.";
ToCanonical::usage="ToCanonical[expr] gives a canonical reorganization of the tensors and their indices in expr, according to the symmetries of tensors and positions of dummies and repeated indices.";
UseMetricOnVBundle::usage="UseMetricOnVBundle is an option for ToCanonical giving the vector bundles where the (first) metric (if any) should be used in the process of canonicalization. Possible values for this option are All, None or a list of vector bundles.";
ImposeSymmetry::usage="ImposeSymmetry[expr, inds, GS] imposes the symmetry given by the generating set (or strong generating set) GS on the indices inds of expr.";
xAct`xTensor`Symmetrize::usage="Symmetrize[expr, {i1, ..., in}] symmetrizes the expr with respect to the n abstract indices i1, ..., in. By convention, the result has a factor 1/n! multiplying. Symmetrize[expr] symmetrizes all free indices.";
Antisymmetrize::usage="Antisymmetrize[expr, {i1, ..., in}] antisymmetrizes the expr with respect to the n abstract indices i1, ..., in. By convention, the result has a factor 1/n! multiplying. Antisymmetrize[expr] antisymmetrizes all free indices.";
PairSymmetrize::usage="PairSymmetrize[expr, {{i1,j1}, ..., {in,jn}}] symmetrizes expr with respect to exchange of both indices among any two pairs, i.e., under simultaneous change of i3<->i7 and of j3<->j7.";
PairAntisymmetrize::usage="PairAntisymmetrize[expr, {{i1,j1}, ..., {in,jn}}] antisymmetrizes expr with respect to exchange of both indices among any two pairs, i.e., under simultaneous change of i3<->i7 and of j3<->j7.";
Cyclize::usage="Cyclize[expr, {i1, ..., in}] returns expr cyclizing the given indices. Note that the result depends on the order of the indices in the list."; 
SymmetryOf::usage="SymmetryOf[expr] gives a description (a result with head Symmetry) of the symmetry of the expression expr. This includes a generating set for that symmetry using Cycles notation on the indices of expr.";
CommutePDs::usage="CommutePDs is a Boolean option for SymmetryOf specifying whether it is possible or not to commute PD derivatives.";
ConstantMetric::usage="ConstantMetric is a Boolean option for SymmetryOf specifying whether all derivatives should be considered to be metric compatible when determining the symmetry of an expression.";
$CommuteCovDsOnScalars::usage="$CommuteCovDsOnScalars is a Boolean global variable stating whether two non-torsion equal covds must be considered as commuting by SymmetryOf. The default initial variable is True.";


(* Manipulation of Scalar *)
NoScalar::usage="NoScalar[expr] removes Scalar from expr, introducing new dummy indices.";
BreakScalars::usage="BreakScalars[expr] separates scalars inside a single Scalar head.";
PutScalar::usage="PutScalar[expr] surrounds scalars with the head Scalar.";


STFPart::usage="STFPart[expr, metric] returns the symmetric, trace-free part of expr with respect to all its free abstract indices. STFPart[expr, metric, indices] allows to specify a list of indices to symmetrize.";
Sym::usage="Sym[expr, inds, sym] represents the symmetrization of the expression expr in the indices inds as given by the symmetry group sym. It plays a similar role to ImposeSymmetry, but without expanding the expression.";


(* Metrics *)
DefMetric::usage="DefMetric[signdet, metric[-a,-b], covd, covdsymbol] defines metric[-a, -b] with signdet 1 or -1 and associates the covariant derivative covd[-a] to it.";
DefProductMetric::usage="DefProductMetric[metric[-a,-b], {{M1, scalar1[]}, ...}, covd, covdsymbol] defines the metric scalar1[]^2 metric1[-a,-b] + scalar2[]^2 metric2[-A,-B] + ... and its associated curvature tensors.";
UndefMetric::usage="UndefMetric[metric] undefines metric and all its associated servants.";
FlatMetric::usage="FlatMetric is a Boolean option for DefMetric specifying whether the defined metric is flat or not.";
Inv::usage="Inv is a prefix symbol used to define the inverse of a (frozen) metric or any linear operator (2-tensor).";
InducedFrom::usage="InducedFrom is an option for DefMetric with form {G, v}, stating that the defined metric is actually the induced metric G on a codimension-1 hypersurface orthogonal to the non-null vector v (with respect to metric G). For a non-induced metric the option has the value Null, which is the default.\n\nInducedFrom[hmetric] returns the pair {G, v} from which the projected metric hmetric has been induced, or Null if hmetric is not an induced metric.";
ConformalFactor::usage="ConformalFactor[metric1, metric2] returns an scalar expression X such that the metrics are related by the conformal transformation metric1[-a,-b] = X metric2[-a,-b].";
ConformalRules::usage="ConformalRules[tensor1, tensor2] returns a list of rules relating the given two tensors by a (general) conformal transformation. These rules are automatically stored by DefMetric when using the option ConformalTo. For other tensors the rules must be specified manually as a tagset for one of the tensors. ConformalRules[list1, list2] joins the lists of transformations for respective tensors in the lists, which hence must have the same length.";
ConformalTo::usage="ConformalTo is an option for DefMetric with form {metexpr, confactor} specifying that the defined metric is conformal to the product of metexpr and confactor, the former a metric expression and the latter a scalar expression to be used as conformal factor.";
SetConformalTo::usage="SetConformalTo[metric[-a, -b], {g[-a, -b], confactor}] declares metric[-a, -b] to be equal to confactor * g[-a, -b], that is they are metrics related by a conformal transformation. The conformal factor is stored as an upvalue for the head ConformalFactor, the conformal transformation is stored as an upvalue for ConformalRules, and the Christoffel relating their respective Levi-Civita connections is also stored.";
epsilonOrientationInBasis::usage="epsilonOrientationInBasis is an option for DefMetric specifying a pair {basis, o} where o is the factor between the epsilon tensor of the metric being defined and the eta tensors of the given basis. By default this pair is {AIndex, $epsilonSign}.";
epsilonOrientation::usage="epsilonOrientation[metric, basis] gives the factor between the epsilon tensor of metric and the eta tensors of basis. Its default value is $epsilonSign, for backward compatibility.";

FlatMetricQ::usage="FlatMetricQ[metric] gives True if metric has been defined as a flat metric, or False otherwise.";
SignDetOfMetric::usage="SignDetOfMetric[metric] gives the sign of the determinant of metric in real bases.";
SignatureOfMetric::usage="SignatureOfMetric[metric] gives the signature of the metric, in the form of a list of three elements: {p1s, m1s, zeros} giving the numbers of +1's, -1's and zeros, respectively, always in this order.";
Determinant::usage="Determinant[metric, basis][] returns the scalar density tensor (with weight +2 in basis) representing the determinant of metric with both indices expanded in the given basis. The tensor is defined if it does not exist yet. The default value for basis is AIndex.";
CovDOfMetric::usage="CovDOfMetric[metric] gives the covariant derivative operator associated to metric at definition time. CovDOfMetric[metric, torsion] gives the covariant derivative operator associated to metric and having the given torsion tensor. Specifying Zero torsion returns the Levi-Civita connection of the metric.";
VBundleOfMetric::usage="VBundleOfMetric[metric] gives the vector bundle where metric lives, or an error if metric is not known.";

ContractMetric::usage="ContractMetric[expr, metric] contracts all instances of metric in expr. If the second argument is a list of metrics the command is applied sequentially on expr. ContractMetric[expr] contracts all metrics.";
OverDerivatives::usage="OverDerivatives is a Boolean option for ContractMetric.";
SeparateMetric::usage="SeparateMetric[g, b][expr, i] separates the index i in the expression expr using the metric g and a dummy index in basis b. The default for b is AIndex, introducing an abstract index. The default for g is the first metric of the vbundle of the index i. The default for i is the list of indices in expr which do not have their natural character (that specified at definition time for tensors, or covariant for all derivatives).";
AllowUpperDerivatives::usage="AllowUpperDerivatives is a Boolean option for ContractMetric. If True, an inverse metric can be contracted with a covariant derivative giving a derivative with a contravariant index. If False, that cannot be done.";
ChristoffelToMetric::usage="ChristoffelToMetric = ChristoffelToGradMetric. Kept for backwards compatibility.";
ChristoffelToGradMetric::usage="ChristoffelToGradMetric[expr, metric] expands expr expressing all Christoffels of metric in terms of the metric. If the second argument is a list of metrics the command is applied sequentially on expr. ChristoffelToGradMetric[expr] expands all Christoffel symbols.";
GradMetricToChristoffel::usage="GradMetricToChristoffel[expr, metric, covd] expands expr expressing all covd-derivatives of metric as Christoffel tensors from the Levi-Civita connection of the metric to covd. If the second and/or third arguments are lists the command is applied sequentially on expr in all possible cases. GradMetricToChristoffel[expr, metric] expands to all defined covariant derivatives. GradMetricToChristoffel expands to all defined metrics and covariant derivatives.";
ChristoffelToGradConformal::usage="ChristoffelToGradConformal[expr, metric1, metric2] expands expr expressing all Christoffel[covd1, covd2], where covdi is the Levi-Civita connection of metrici, in terms of derivatives (using covd2) of the conformal factor relating those metrics. The covariant derivatives covdi can be used in input instead of the metrici.";

MetricToProjector::usage="MetricToProjector[expr, metric] expands expr expressing the metric tensor in terms of a projector and its orthogonal vector field. If the second argument is a list of metrics the command is applied sequentially on expr. MetricToProjector[expr] expands all metric tensors.";
ProjectorToMetric::usage="ProjectorToMetric[expr, proj] expands expr expressing the projector proj in terms of the corresponding metric and orthogonal vector field. If the second argument is a list of projectors the command is applied sequentially on expr. ProjectorToMetric[expr] expands all projectors.";
ProjectWith::usage="ProjectWith[projector][expr] projects all free indices of expression using the given projector. Note the double pair of brackets, which simplifies the replacement of the projecting inert head with the actual projecting function using a rule like ih -> ProjectWith[proj].";
GradNormalToExtrinsicK::usage="GradNormalToExtrinsicK[expr, imetric] transforms the derivatives of the normal vector to the induced metric imetric into extrinsic curvature tensors. If the second argument is a list then the function is applied sequentially on all elements. GradNormalToExtrinsicK[expr] expands all induced metrics.";
ExtrinsicKToGradNormal::usage="ExtrinsicKToGradNormal[expr, imetric] transforms the extrinsic curvature tensors associated to the induced metric imetric into derivatives of its normal vector. If the second argument is a list then the function is folded sequentially on all elements. ExtrinsicKToGradNormal[expr] expands all induced metrics.";
ProjectDerivative::usage="ProjectDerivative[expr, cd] converts the induced derivative cd into the original derivative but projected. If the second argument is a list then the function is folded sequentially on all elements. ProjectDerivative[expr] converts all induced derivatives.";
InducedDecomposition::usage="InducedDecomposition[expr, {imetric, vector}] decomposes the tensors in expr in their longitudinal and transversal parts with respect to the induced metric imetric and its orthogonal vector.";
ToInducedDerivative::usage="\!\(\*
StyleBox[\"ToInducedDerivative\",\nFontFamily->\"Courier\"]\)[expr, supercd, cd] converts the supercd-derivatives into the sum of an induced cd-derivative, a Lie derivative along the orthogonal vector of the projection and additional extrinsic curvature terms.";
VectorOfInducedMetric::usage="VectorOfInducedMetric[metric] returns the vector to which the induced metric is orthogonal.";
GaussCodazzi::usage="GaussCodazzi[expr, imetric] projects curvature tensors of the Levi-Civita connection of the metric from which imetric is induced into their projected components.";

(* Product metrics *)
MetricScalar::usage="MetricScalar[metric, subvbundle] gives the scalar that relates metric (of a product manifold) with the metric of the subvbundle.";
ExpandProductMetric::usage="ExpandProductMetric[expr, metric] expands expr decomposing all instances of the product metric and its derived curvature tensors in terms of the objects of the subvbundle that form the vbundle of metric. If the second argument is a list of product metrics the command is applied sequentially on expr.";


(* Tetra *)
Tetra::usage="Tetra[metric] returns the name of the tetra-metric associated to metric.";
TetraRule::usage="TetraRule[metric] gives the list of rules translating the tetra-metric of metric into products of metric and an imaginary term with the volume form of metric.";


(* Calculations *)
ScreenDollarIndices::usage="ScreenDollarIndices[expr] replaces internal dummies (dollar-indices) by new non-dollar dummies for output.";
Simplification::usage="Simplification[expr] simplifies expr by calling Simplify[ ToCanonical[expr] ].";
Validate::usage="Validate[expr] checks that the tensorial expression expr 1) does not have unknown heads, 2) does not have objects with invalid index structure, 3) does have homogeneous free indices and 4) does not have repeated indices. There is a wrapping Catch.";
(* UncatchedValidate::usage="UncatchedValidate[expr] is equivalente to Validate[expr] but without the wrapping Catch."; *)
IndexSolve::usage="IndexSolve[eqn, tensor] solves a linear equation eqn for tensor, returning a rule (see MakeRule). Currently that tensor cannot have contracted indices. Options to IndexSolve are understood as options to MakeRule.";
IndexCoefficient::usage="IndexCoefficient[expr, form] gives the coefficient of form (any indexed object with no division present) in expr.";
ChangeFreeIndices::usage="ChangeFreeIndices[expr, newfrees] changes the free indices in expr (after sorting them with IndexSort) by indices in the list newfrees, changing the dummies of expr which could interfere with any of those new indices.";
EqualExpressionsQ::usage="EqualExpressionsQ[expr1, expr2] returns True if there is a permutation of the free indices of expr1 such that its canonical form equals that of expr2, and False otherwise.";
IndexCollect::usage="IndexCollect[expr, form, function] imitates the action of Collect, but allowing indexed expressions in form. This is still a very limited function, almost experimental.";
SetCharacters::usage="SetCharacters[expr, tensor, chars] returns expr with all instances of tensor[inds] corrected to match the characters chars, a list of elements Up or Down. The command introduces deltas, which automatically transform into metrics or Basis objects if required.";


ColorTerms::usage="ColorTerms[expr] uses the function ColorPositions (from the package ExpressionManipulation) to color the elements of the first level of expr, tagging them with their respective positions.";
ColorPositionsOfPattern::usage="ColoPositionsOfPattern[pattern][expr] uses the function ColorPositions (from the package ExpressionManipulation) to color the occurrences of pattern in expr, tagging them with their respective positions. Options can be passed to ColorPositions within the first pair of brackets.";


Begin["`Private`"]


$xTensorNames=Names["xAct`xTensor`*"];


(******************************* 2.Types ******************************)


If[$ReadingVerbose,Print["Reading section 2: Types."],Null,Null]


$ProtectNewSymbols=False;


ServantsOf[Symbol]={delta,Gdelta,Sdelta,Basis,PD};
HostsOf[_]:={};
VisitorsOf[_]:={};
MasterOf[_]:=Null;
ServantsOf[_]:={};
SetNumberOfArguments[#,1]&/@{HostsOf,VisitorsOf,MasterOf,ServantsOf};
Protect[HostsOf,VisitorsOf,MasterOf,ServantsOf];


SymbolRelations[list_List,master_Symbol,hosts_List]:=SymbolRelations[#,master,hosts]&/@list;
SymbolRelations[symbol_Symbol,master_Symbol,hosts_List]:=With[{hosts2=Complement[hosts,FixedPointList[MasterOf,master]]},
If[master=!=Null,
MasterOf[symbol]^=master;
xUpAppendTo[ServantsOf[master],symbol]
];
If[hosts2=!={},xUpSet[HostsOf[symbol],Join[HostsOf[symbol],hosts2]]];
Map[If[!MemberQ[VisitorsOf[#],symbol],xUpAppendTo[VisitorsOf[#],symbol]]&,hosts2];
];


Protect[Master];


RemoveSymbol[symbol_]:=(Unprotect[symbol];Remove[symbol];);


CheckRemoveSymbol[symbol_Symbol]:=Which[
VisitorsOf[symbol]=!={},
Throw@Message[General::noundef,"Symbol",symbol,"it has visitors"],
ServantsOf[MasterOf[symbol]]=!={},
Throw@Message[General::noundef,"Symbol",symbol,"its master is still alive"],
True,
Null];


DropFromHosts[symbol_Symbol]:=Map[xUpDeleteCasesTo[VisitorsOf[#],symbol]&,HostsOf[symbol]];


$Types={AbstractIndex,Manifold,Mapping,VBundle,Tensor,Metric,CovD,InertHead,ScalarFunction,ConstantSymbol,Parameter,Product,Chart,Basis};


Protect/@$Types;


(* Main 13 data bases *)
$Manifolds={};
$Mappings={};
$VBundles={};
$AbstractIndices:=Flatten[IndicesOfVBundle/@$VBundles];
$ConstantSymbols={};
$Tensors={};
$CovDs={};
$Metrics={};
$InertHeads={};
$ScalarFunctions={};
$Parameters={};
$Products={};
$Bases={};
$Charts={};
(* Additional 4 data bases, apart from the private $FirstMetrics *)
$ProductManifolds={};
$SumVBundles={};
$ProductMetrics={};
$Rules={};


$QFunctions={AbstractIndexQ,ConstantSymbolQ,xTensorQ,CovDQ,MetricQ,ManifoldQ,MappingQ,VBundleQ,InertHeadQ,ScalarFunctionQ,BasisQ,ChartQ,ParameterQ,ProductQ};


SetDelayed[#[_],False]&/@$QFunctions;


SetNumberOfArguments[#,1]&/@$QFunctions;


Protect[Evaluate[$QFunctions]];


FindAllOfType[expr_,AbstractIndex]:=Cases[expr,_Symbol?AbstractIndexQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,ConstantSymbol]:=Cases[expr,_Symbol?ConstantSymbolQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Parameter]:=Cases[expr,_Symbol?ParameterQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Manifold]:=Cases[expr,_?ManifoldQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Mapping]:=Cases[expr,_?MappingQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,VBundle]:=Cases[expr,_?VBundleQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Tensor]:=Cases[expr,_?xTensorQ[___],{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Metric]:=Cases[expr,_Symbol?MetricQ[_,_],{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,CovD]:=Cases[expr,_?CovDQ[__][_]|CovD[_,__],{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,InertHead]:=Cases[expr,_?InertHeadQ[__],{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,ScalarFunction]:=Cases[expr,_?ScalarFunctionQ[___],{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Product]:=Cases[expr,_Symbol?ProductQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Chart]:=Cases[expr,_Symbol?ChartQ,{0,DirectedInfinity[1]},Heads->True];
FindAllOfType[expr_,Basis]:=Cases[expr,Basis[_,_],{0,DirectedInfinity[1]},Heads->True];
SetNumberOfArguments[FindAllOfType,2];
Protect[FindAllOfType];


Undef[symbol_Symbol?ManifoldQ]:=UndefManifold[symbol];
Undef[symbol_Symbol?MappingQ]:=UndefMapping[symbol];
Undef[symbol_Symbol?VBundleQ]:=UndefVBundle[symbol];
Undef[symbol_Symbol?ParameterQ]:=UndefParameter[symbol];
Undef[symbol_Symbol?xTensorQ]:=UndefTensor[symbol];
Undef[symbol_Symbol?CovDQ]:=UndefCovD[symbol];
Undef[symbol_Symbol?ConstantSymbolQ]:=UndefConstantSymbol[symbol];
Undef[symbol_Symbol?ScalarFunctionQ]:=UndefScalarFunction[symbol];
Undef[symbol_Symbol?InertHeadQ]:=UndefInertHead[symbol];
Undef[symbol_Symbol?ProductQ]:=UndefProduct[symbol];
Undef[symbol_Symbol?ChartQ]:=xAct`xCoba`UndefChart[symbol];
Undef[symbol_Symbol?BasisQ]:=xAct`xCoba`UndefBasis[symbol];
Undef[symbol_Symbol?AbstractIndexQ]:=Throw@Message[Undef::nouse,"Undef","an abstract index"];
Undef[symbol_]:=Throw@Message[Undef::unknown,"symbol type of",symbol];
SetNumberOfArguments[Undef,1];
Protect[Undef];


ValidateSymbolInSession[symbol_]:=With[{name=ToString[symbol]},
Which[
AbstractIndexQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as an abstract index"],
ConstantSymbolQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a constant-symbol"],
ParameterQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a parameter"],
ManifoldQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a manifold"],
MappingQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a mapping"],
VBundleQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a vector bundle"],
MetricQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a metric"],
xTensorQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a tensor"],
CovDQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a covariant derivative"],
ScalarFunctionQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a scalar function"],
InertHeadQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as an inert head"],
ProductQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a product"],
ChartQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a chart"],
BasisQ[symbol],
Throw@Message[ValidateSymbol::used,name,"as a basis"]
]
];
SetNumberOfArguments[ValidateSymbolInSession,1];
Protect[ValidateSymbolInSession];


SetAttributes[interpretbox,HoldFirst];
interpretbox[expr_,box_]:=InterpretationBox[
StyleBox[box,AutoSpacing->False,ShowAutoStyles->False],
expr,
Editable->False
];


SetAttributes[interpretboxcolor,HoldFirst];
interpretboxcolor[expr_,box_,color_]:=interpretbox[expr,StyleBox[box,Background->color]];


boxof[InterpretationBox[box_,__]]:=boxof[box];
boxof[StyleBox[box_,__]]:=boxof[box];
boxof[box_]:=box;


SetAttributes[MakeSequenceBox,HoldFirst];
MakeSequenceBox[list_List,char_,format_]:=RowBox@Riffle[Map[Function[Null,MakeBoxes[#,format],HoldFirst],Unevaluated[list]],char];


xTensorFormStart[]:=xTensorFormStart/@{InertHead,ConstantSymbol,Parameter,Tensor,CovD,LieD,ParamD};
xTensorFormStop[]:=xTensorFormStop/@{InertHead,ConstantSymbol,Parameter,Tensor,CovD,LieD,ParamD};
xTensorFormStart[x_]:=Throw@Message[xTensorFormStart::unknown,"class to format",ToString[x]];
xTensorFormStop[x_]:=Throw@Message[xTensorFormStop::unknown,"class to format",ToString[x]];
SetNumberOfArguments[xTensorFormStart,{0,1}];
SetNumberOfArguments[xTensorFormStop,{0,1}];


(* A string is a string. Null means "do not set typsetting". Anything else is interpreted as a function *)
StripContext[string_String]:=Last@StringSplit[string,"`"];
PrintAsString[symbol_,Null]:=Null;
PrintAsString[symbol_,pa_String]:=pa;
PrintAsString[symbol_,pa_Hold]:=pa;
PrintAsString[symbol_,pa_?System`Convert`TeXFormDump`BoxQ]:=pa;
PrintAsString[symbol_,pa_]:=StripContext[
With[{res=pa[symbol]},
If[StringQ[res],res,ToString[res]]
]
];
(* Handling of conjugate symbols: add a dagger if not already present *)
AddDaggerCharacter[boxes_Hold,_]:=Print["Don't know how to conjugate ",boxes];
AddDaggerCharacter[string_String,Conjugate]:=If[HasDaggerCharacterQ[string],string,StringJoin[string,$DaggerCharacter]];
AddDaggerCharacter[boxes_,Conjugate]:=RowBox[{boxes,$DaggerCharacter}];
AddDaggerCharacter[boxes_,dag_]:=boxes;
(* Main. The previous two functions will be used in its second argument, if needed *)
SetPrintAs[symbol_,Null]:=Null;
SetPrintAs[symbol_,Hold[boxes_]]:=xUpSetDelayed[PrintAs[symbol],boxes];
SetPrintAs[symbol_,string_]:=xUpSet[PrintAs[symbol],string];


SetAttributes[PrintAs,HoldFirst];
PrintAs[head_]:=ToString[Unevaluated[head]];
SetNumberOfArguments[PrintAs,1];
Protect[PrintAs];


(* STRINGPRINTAS *)
PrintAsSmaller[proj_]:="\*StyleBox[\""<>PrintAs[proj]<>"\",FontSize->Tiny]";


(* Default definition for the names of automatically-defined tensors. They must be symbols *)
GiveSymbol[symbol_Symbol,post___]:=SymbolJoin[symbol,post];
(* Do not put any restrictions here to make this definition be always the last downvalue. STRINGPRINTAS *)
GiveOutputString[symbol_Symbol,post___]:=StringJoin[PrintAsCharacter[symbol],PrintAs/@{post}];
SetAttributes[PrintAsCharacter,HoldFirst];
PrintAsCharacter[symbol_Symbol]:=First@Characters@ToString[Unevaluated[symbol]];
SetNumberOfArguments[GiveSymbol,{1,Infinity}];
SetNumberOfArguments[GiveOutputString,{1,Infinity}];
SetNumberOfArguments[PrintAsCharacter,1];


$StarCharacter="*";
$PrecomposeCharacter="\[SmallCircle]";
$LinearPushCharacter="\[CenterDot]";


PullBackString[string_String,phi_]:=StringJoin["(\!\(",PrintAs[phi],"\^",$StarCharacter,string,"\))"];
PrecomposeString[string_String,phi_]:=StringJoin["(",string,$PrecomposeCharacter,PrintAs[phi],")"];
PullBackBox[box_,phi_]:=RowBox[{SuperscriptBox[PrintAs[phi],$StarCharacter],box}];
PrecomposeBox[box_,phi_]:=RowBox[{box,$PrecomposeCharacter,PrintAs[phi]}];


PushForwardString[string_String,phi_]:=StringJoin["(\!\(",PrintAs[phi],"\_",$StarCharacter,string,"\))"];
LinearPushString[string_String,dphi_]:=StringJoin["(",PrintAs[dphi],$LinearPushCharacter,string,")"];
PushForwardBox[box_,phi_]:=RowBox[{SubscriptBox[PrintAs[phi],$StarCharacter],box}];


makespace[""]:="";
makespace[other_]:=StringJoin[other," "];


$DefInfoQ=True;


MakeDefInfo[_,_,False,vanishQ_:False]:=Null;
MakeDefInfo[command_Symbol,object_,{secondarytype_String,info_String,___},vanishQ_:False]:=If[$DefInfoQ,Print["** ",command,": Defining ",makespace@If[vanishQ,"vanishing "<>secondarytype,secondarytype], object,". ",info]];


DefInfo[x_]:=Throw@Message[DefInfo::error1,"Object `1` has not been registered using a DefType function.",x];


$UndefInfoQ=True;


MakeUndefInfo[command_Symbol,object_]:=MakeUndefInfo[command,object,DefInfo[object]];
MakeUndefInfo[command_Symbol,object_,False]:=Null;
MakeUndefInfo[command_Symbol,object_,{info_,_}]:=If[$UndefInfoQ,Print["** ",command,": Undefined ",info," ",object]];


(*************************** 3. Four simple types ***************************)


If[$ReadingVerbose,Print["Reading section 3: Simple types."],Null,Null]


Options[DefInertHead]={
LinearQ->False,
ContractThrough->{},
Master->Null,
PrintAs->Identity,
ProtectNewSymbol:>$ProtectNewSymbols,
DefInfo->{"inert head",""}
};
DefInertHead[list_List,rest___]:=Scan[DefInertHead[#,rest]&,list];
DefInertHead[ih_,options:OptionsPattern[]]:=Catch@With[{head=SubHead[ih]},
Module[{lin,cthrough,master,pa,pns,info,x,y},
{lin,cthrough,master,pa,pns,info}=OptionValue[{LinearQ,ContractThrough,Master,PrintAs,ProtectNewSymbol,DefInfo}];
(* Validate *)
ValidateSymbol[head];
ValidateSymbolInSession[head];
(* Register *)
MakeDefInfo[DefInertHead,ih,info];
MakexTensions[DefInertHead,"Beginning",ih,options];
AppendToUnevaluated[$InertHeads,head];
head/:InertHeadQ[ih]=True;
head/:DefInfo[ih]=info;
SetPrintAs[ih,PrintAsString[ih,pa]];
SymbolRelations[head,master,{}];
(* Metric / projector / delta contraction *)
xTagSet[{head,ContractThroughQ[ih,#]},True]&/@Flatten[{cthrough}];
(* Linearity. Only with respect to constants, not general scalar fields *)
head/:LinearQ[ih]=lin;
If[lin,MakeLinear[ih,NoPattern[ih]]];
MakexTensions[DefInertHead,"End",ih,options];
(* Protect *)
If[pns,Protect[head]];
]
];
SetNumberOfArguments[DefInertHead,{1,Infinity}];
Protect[DefInertHead];


MakeLinear[ihL_,ihR_]:=(
ihL[Verbatim[Times][l___,x_?ConstantQ,r___],z___]:=x ihR[Times[l,r],z];
ihL[sum_Plus,z___]:=ihR[#,z]&/@sum;
ihL[expr_SeriesData,z___]:=SeriesDataMap[ihR[#,z]&,expr];
ihL[0,z___]:=0;
);


DependenciesOfInertHead[ih_]:={};
SetNumberOfArguments[DependenciesOfInertHead,1];
Protect[DependenciesOfInertHead];


LinearQ[x_]:=Throw@Message[LinearQ::unknown,"inert-head",x];
SetNumberOfArguments[LinearQ,1];
Protect[LinearQ];


Protect[ContractThrough];


ContractThroughQ[ih_,metric_]:=False;
SetNumberOfArguments[ContractThroughQ,2];
Protect[ContractThroughQ];


UndefInertHead[list:{___?InertHeadQ}]:=Scan[UndefInertHead,list];
UndefInertHead[ih_]:=Catch@With[{head=SubHead[ih]},
With[{servants=ServantsOf[head]},
If[!InertHeadQ[ih],Throw@Message[UndefInertHead::unknown,"inert head",ih]];
CheckRemoveSymbol[head];
MakexTensions[UndefInertHead,"Beginning",ih];
xUpSet[ServantsOf[head],{}];
DropFromHosts[head];
Undef/@Reverse[servants];
$InertHeads=DeleteCases[$InertHeads,head];
MakexTensions[UndefInertHead,"End",ih];
MakeUndefInfo[UndefInertHead,ih];
RemoveSymbol[head];
]
];
SetNumberOfArguments[UndefInertHead,1];
Protect[UndefInertHead];


SetAttributes[ERROR,HoldFirst]
InertHeadQ[ERROR]^=True;
LinearQ[ERROR]^=False;
DefInfo[ERROR]^={"inert head","Generic head to wrap expressions with errors."};
PrintAs[ERROR]^=ColorString["ERROR",Hue[0]];
AppendToUnevaluated[$InertHeads,ERROR];
Protect[ERROR];


InerthHeadQ[Keep]^=True;
LinearQ[Keep]^=True;
DefInfo[Keep]^={"inert head","Generic head to avoid expression expansion."};
AppendToUnevaluated[$InertHeads,Keep];
Protect[Keep];


MakeBoxes[Keep[expr_,z___],StandardForm]:=RowBox[{"(",MakeBoxes[expr,StandardForm],")"}];


SetAttributes[HeldInertHeadQ,HoldAllComplete];
HeldInertHeadQ[expr_]:=InertHeadQ[Unevaluated[expr]];


xTensorFormStart[InertHead]:=(
MakeBoxes[ih_?HeldInertHeadQ[expr_,z___],StandardForm]:=interpretbox[ih[expr,z],RowBox[{PrintAs[Unevaluated[ih]],"[",MakeBoxes[expr,StandardForm],"]"}]]);
xTensorFormStop[InertHead]:=(MakeBoxes[ih_?HeldInertHeadQ[expr_,z___],StandardForm]=.);
xTensorFormStart[InertHead]


InertHeadQ[Dagger]^=True;
LinearQ[Dagger]^=False;
PrintAs[Dagger]^="Dagger";
AppendToUnevaluated[$InertHeads,Dagger];


(* Listability, linearity and product expansion *)
Dagger[expr_List]:=Dagger/@expr;
Dagger[expr_Equal]:=Dagger/@expr;
Dagger[expr_Plus]:=Dagger/@expr;
Dagger[HoldPattern[SeriesData[var_,orig_,coeffs_,rest__]]]:=SeriesData[Dagger[var],Dagger[orig],Dagger/@coeffs,rest];
Dagger[expr_Times]:=Dagger/@expr;
Dagger[HoldPattern@SeriesData[par_,origin_,coeffs_,args__]]:=SeriesData[Dagger[par],Dagger[origin],Dagger[coeffs],args];
(* Constant-symbols have an upvalue for Dagger *)
Dagger[x_?NumericQ]:=Conjugate[x];
(* All parameters assumed real *)
Dagger[param_Symbol?ParameterQ]:=param;
(* Integer powers *)
Dagger[Power[expr_,n_Integer]]:=Power[Dagger[expr],n];


(* Involutivity *)
Dagger[Dagger[expr_]]:=expr;


(* Action on composite types *)
Dagger[Scalar[expr_]]:=Scalar[Dagger[expr]];
Dagger[ParamD[ps__][expr_]]:=Map[Dagger,ParamD[ps]][Dagger[expr]];
Dagger[LieD[v_][expr_]]:=LieD[Dagger[v]][Dagger[expr]];
(* Covariant derivatives. So far we do not touch the covd head *)
Dagger[covd_?CovDQ[inds__][expr_]]:=DaggerCovD[covd[inds]][Dagger[expr]];
DaggerCovD[covd_[inds__]]:=Map[DaggerIndex,covd[inds]];
Dagger[CovD[expr_,covdinds__]]:=CovD[Dagger[expr],##]&@@Map[DaggerCovD,{covdinds}];
(* Action on tensors. Note that this includes Basis, Bracket and TensorDerivative *)
Dagger[tensor_?xTensorQ[inds___]]:=Apply[Dagger[tensor],DaggerIndex@IndexList[inds]];
(* QUESTION: Is there any "analyticity" assumption here? Why do we dagger sf itself? *)
Dagger[sf_?ScalarFunctionQ[args___]]:=Apply[Dagger[sf],Dagger/@{args}];
(* Scalar functions are assumed real by default *)
Dagger[sf_?ScalarFunctionQ]:=sf;
(* Mappings are taken as real because they are between real manifolds *)
Dagger[phi_?MappingQ]:=phi;


(* Generic action on symbols *)
Dagger[x_Symbol]:=Throw@Message[Dagger::unknown,"conjugate of symbol",x];


SetNumberOfArguments[Dagger,1];
Protect[Dagger];


DaggerQ[expr_]:=UnsameQ[expr,Dagger[expr]];
SetNumberOfArguments[DaggerQ,1];
Protect[DaggerQ];


SetDaggerPair[symbol1_Symbol,symbol2_Symbol]:=(xUpSet[Dagger[symbol2],symbol1];xUpSet[Dagger[symbol1],symbol2]);
SetDaggerPair[x_,_Symbol]:=Throw@Message[Validate::nouse,"SetDaggerPair",x];
SetDaggerPair[_Symbol,x_]:=Throw@Message[Validate::nouse,"SetDaggerPair",x];


Options[DefScalarFunction]={Dagger->Real,Master->Null,PrintAs->Identity,ProtectNewSymbol:>$ProtectNewSymbols,DefInfo->{"scalar function",""},Validate->True};
DefScalarFunction[list_List,rest___]:=Scan[DefScalarFunction[#,rest]&,list];
DefScalarFunction[sf_,options:OptionsPattern[]]:=Catch@Module[{dag,master,pa,pns,info,val},
{dag,master,pa,pns,info,val}=OptionValue[{Dagger,Master,PrintAs,ProtectNewSymbol,DefInfo,Validate}];

(* Checks *)
If[val,
ValidateSymbol[sf];
ValidateSymbolInSession[sf]];

(* Register *)
MakeDefInfo[DefScalarFunction,sf,info];
MakexTensions[DefScalarFunction,"Beginning",sf,options];
Switch[dag,
Complex,SetDaggerPair[sf,MakeDaggerSymbol[sf]];DefScalarFunction[Dagger[sf],Dagger->Conjugate,Master->sf,options],
Conjugate,Null,
Imaginary,Dagger[sf]^=MultiplyHead[-1,sf],
Real,Dagger[sf]^=sf,
_,Throw@Message[DefScalarFunction::unknown,"Dagger value",dag]
];
AppendToUnevaluated[$ScalarFunctions,sf];
ScalarFunctionQ[sf]^=True;
DefInfo[sf]^=info;
SetPrintAs[sf,AddDaggerCharacter[PrintAsString[sf,pa],dag]];
SymbolRelations[sf,master,{}];
MakexTensions[DefScalarFunction,"End",sf,options];
If[pns,Protect[sf]];
];
SetNumberOfArguments[DefScalarFunction,{1,Infinity}];
Protect[DefScalarFunction];


Unprotect[ScalarFunctionQ];
ScalarFunctionQ[Times|Plus]=False;
ScalarFunctionQ[Derivative[__Integer][f_]]:=ScalarFunctionQ[f];
ScalarFunctionQ[head_Symbol]:=MemberQ[Attributes[head],NumericFunction];
ScalarFunctionQ[_]:=False;
Protect[ScalarFunctionQ];


UndefScalarFunction[list:{___?ScalarFunctionQ}]:=Scan[UndefScalarFunction,list];
UndefScalarFunction[sf_]:=Catch@With[{servants=ServantsOf[sf]},
If[!ScalarFunctionQ[sf],Throw[Message[UndefScalarFunction::unknown,"scalar function",sf]]];
CheckRemoveSymbol[sf];
MakexTensions[UndefScalarFunction,"Beginning",sf];
xUpSet[ServantsOf[sf],{}];
DropFromHosts[sf];
Undef/@Reverse[servants];
$ScalarFunctions=DeleteCases[$ScalarFunctions,sf];
MakexTensions[UndefScalarFunction,"End",sf];
MakeUndefInfo[UndefScalarFunction,sf];
RemoveSymbol[sf];
];
SetNumberOfArguments[UndefScalarFunction,1];
Protect[UndefScalarFunction];


SetAttributes[HeldScalarFunctionQ,HoldAllComplete];
HeldScalarFunctionQ[expr_]:=ScalarFunctionQ[Unevaluated[expr]];


xTensorFormStart[ScalarFunction]:=(
MakeBoxes[sf_Symbol?HeldScalarFunctionQ,StandardForm]:=interpretbox[sf,RowBox[{PrintAs[Unevaluated[sf]]}]]);
xTensorFormStop[ScalarFunction]:=(MakeBoxes[sf_Symbol?HeldScalarFunctionQ,StandardForm]=.);
xTensorFormStart[ScalarFunction];


Options[DefConstantSymbol]={Dagger->Real,Master->Null,PrintAs->Identity,ProtectNewSymbol:>$ProtectNewSymbols,DefInfo->{"constant symbol",""},Validate->True};
DefConstantSymbol[list_List,rest___]:=Scan[DefConstantSymbol[#,rest]&,list];
DefConstantSymbol[symbol_,options:OptionsPattern[]]:=Catch@Module[{dag,master,pa,pns,info,val},
{dag,master,pa,pns,info,val}=OptionValue[{Dagger,Master,PrintAs,ProtectNewSymbol,DefInfo,Validate}];

(* Checks *)
If[val,
ValidateSymbol[symbol];
ValidateSymbolInSession[symbol]];

(* Register *)
MakeDefInfo[DefConstantSymbol,symbol,info];
MakexTensions[DefConstantSymbol,"Beginning",symbol,options];
Switch[dag,
Complex,SetDaggerPair[symbol,MakeDaggerSymbol[symbol]];DefConstantSymbol[Dagger[symbol],Dagger->Conjugate,Master->symbol,options],
Conjugate,Null,
Imaginary,Dagger[symbol]^=-symbol,
Real,Dagger[symbol]^=symbol,
_,Throw@Message[DefConstantSymbol::unknown,"Dagger value",dag]
];
AppendToUnevaluated[$ConstantSymbols,symbol];
ConstantSymbolQ[symbol]^=True;
SetAttributes[symbol,Constant];
DefInfo[symbol]^=info;
SetPrintAs[symbol,AddDaggerCharacter[PrintAsString[symbol,pa],dag]];
SymbolRelations[symbol,master,{}];
MakexTensions[DefConstantSymbol,"End",symbol,options];
If[pns,Protect[symbol]];
]
SetNumberOfArguments[DefConstantSymbol,{1,Infinity}];
Protect[DefConstantSymbol];


UndefConstantSymbol[list:{___?ConstantSymbolQ}]:=Scan[UndefConstantSymbol,list];
UndefConstantSymbol[symbol_]:=Catch@With[{servants=ServantsOf[symbol]},
If[!ConstantSymbolQ[symbol],Throw@Message[UndefConstantSymbol::unknown,"constant-symbol",symbol]];
CheckRemoveSymbol[symbol];
MakexTensions[UndefConstantSymbol,"Beginning",symbol];
xUpSet[ServantsOf[symbol],{}];
DropFromHosts[symbol];
Undef/@Reverse[servants];
$ConstantSymbols=DeleteCases[$ConstantSymbols,symbol];
MakexTensions[UndefConstantSymbol,"End",symbol];
MakeUndefInfo[UndefConstantSymbol,symbol];
RemoveSymbol[symbol];
]
SetNumberOfArguments[UndefConstantSymbol,1];
Protect[UndefConstantSymbol];


Unprotect[ConstantSymbolQ];
ConstantSymbolQ[x_Symbol]:=MemberQ[Attributes[x],Constant];
Protect[ConstantSymbolQ];


SetAttributes[HeldConstantSymbolQ,HoldAllComplete];
HeldConstantSymbolQ[expr_]:=ConstantSymbolQ[Unevaluated[expr]];


xTensorFormStart[ConstantSymbol]:=(
MakeBoxes[x_Symbol?HeldConstantSymbolQ,StandardForm]:=interpretbox[x,RowBox[{PrintAs[Unevaluated[x]]}]]);
xTensorFormStop[ConstantSymbol]:=(MakeBoxes[x_Symbol?HeldConstantSymbolQ,StandardForm]=.);
xTensorFormStart[ConstantSymbol];


Options[DefParameter]={Master->Null,PrintAs->Identity,ProtectNewSymbol:>$ProtectNewSymbols,DefInfo->{"parameter",""}};
DefParameter[list_List,rest___]:=Scan[DefParameter[#,rest]&,list];
DefParameter[param_,options:OptionsPattern[]]:=Catch@Module[{master,pa,pns,info},
{master,pa,pns,info}=OptionValue[{Master,PrintAs,ProtectNewSymbol,DefInfo}];
ValidateSymbol[param];
ValidateSymbolInSession[param];
MakeDefInfo[DefParameter,param,info];
MakexTensions[DefParameter,"Beginning",param,options];
AppendToUnevaluated[$Parameters,param];
ParameterQ[param]^=True;
DefInfo[param]^=info;
SetPrintAs[param,PrintAsString[param,pa]];
SymbolRelations[param,master,{}];
MakexTensions[DefParameter,"End",param,options];
If[pns,Protect[param]];
];
SetNumberOfArguments[DefParameter,{1,Infinity}];
Protect[DefParameter];


UndefParameter[list:{___?ParameterQ}]:=Scan[UndefParameter,list];
UndefParameter[param_]:=Catch@With[{servants=ServantsOf[param]},
If[!ParameterQ[param],Throw[Message[UndefParameter::unknown,"parameter",param]]];
CheckRemoveSymbol[param];
MakexTensions[UndefParameter,"Beginning",param];
xUpSet[ServantsOf[param],{}];
DropFromHosts[param];
Undef/@Reverse[servants];
$Parameters=DeleteCases[$Parameters,param];
MakexTensions[UndefParameter,"End",param];
MakeUndefInfo[UndefParameter,param];
RemoveSymbol[param];
];
SetNumberOfArguments[UndefParameter,1];
Protect[UndefParameter];


SetAttributes[HeldParameterQ,HoldAllComplete];
HeldParameterQ[expr_]:=ParameterQ[Unevaluated[expr]];


xTensorFormStart[Parameter]:=(MakeBoxes[param_Symbol?HeldParameterQ,StandardForm]:=interpretbox[param,RowBox[{PrintAs[Unevaluated[param]]}]]);
xTensorFormStop[Parameter]:=(MakeBoxes[param_Symbol?HeldParameterQ,StandardForm]=.);
xTensorFormStart[Parameter]


(*********************** 4. The mathematical input ***********************)


If[$ReadingVerbose,Print["Reading section 4: Mathematical input."],Null,Null]


MathInputExpand[Equal[lhs_,rhs_]]:=Equal[MathInputExpand[lhs],MathInputExpand[rhs]];
MathInputExpand[Rule[lhs_,rhs_]]:=Rule[MathInputExpand[lhs],MathInputExpand[rhs]];
MathInputExpand[expr_]:=FixedPoint[Expand,expr];


ListOfTerms[HoldPattern[expr:SeriesData[var_,origin_,coeffs_,args__]]]:=Append[ListOfTerms[Normal[expr]],SeriesData[var,origin,0 coeffs,args]];
ListOfTerms[expr_]:=If[Head[#]===Plus,List@@#,List@#]&@MathInputExpand[expr];


ListOfFactors[expr_]:=If[Head[#]===Times,List@@#,List@#]&@Factor[expr];


$MathematicaProductSymbols={Square,CircleTimes,CircleMinus,CircleDot,Diamond,CenterDot,Star,VerticalTilde,Backslash,Wedge,Vee,Cap,Cup};


Options[DefProduct]={
AssociativeProductQ->False,
IdentityElementOfProduct->None,
CommutativityOfProduct->None,
GradedProductQ->False,
GradeOfProduct->0,
ScalarsOfProduct->NumericQ,
DefineCommutator->False,
xSortOrder->Automatic,
Master->Null,
PrintAs->"\[Diamond]",
ProtectNewSymbol:>$ProtectNewSymbols,
DefInfo->{"product",""}
};
DefProduct[list_List,rest___]:=Scan[DefProduct[#,rest]&,list];
DefProduct[prod_,options___?OptionQ]:=DefProduct[prod,Times,Times,options];
DefProduct[prod_,epbs_,options___?OptionQ]:=DefProduct[prod,epbs,Times,options];
DefProduct[prod_,epbs_,pos_,options:OptionsPattern[]]:=Catch@Module[{asQ,ie,com,gp,gop,scQ,dc,xso,master,pa,pns,info,systemQ,pbs,pbsside,isQ},

{asQ,ie,com,gp,gop,scQ,dc,xso,master,pa,pns,info}=OptionValue[{AssociativeProductQ,IdentityElementOfProduct,CommutativityOfProduct,GradedProductQ,GradeOfProduct,ScalarsOfProduct,DefineCommutator,xSortOrder,Master,PrintAs,ProtectNewSymbol,DefInfo}];

(* Validate symbol prod *)
systemQ=MemberQ[$MathematicaProductSymbols,prod];
If[!systemQ,ValidateSymbol[prod]];
ValidateSymbolInSession[prod];

(* The product of scalars must be already a product, and it must be associative, unital and commutative, i.e. a commutative monoid *)
If[!ProductQ[pos],Throw@Message[DefProduct::unknown,pos,"product"]];
If[!AssociativeProductQ[pos],Throw@Message[DefProduct::error1,pos,"is not associative"]];
If[CommutativityOfProduct[pos]=!="Commutative",Throw@Message[DefProduct::error1,pos,"is not commutative"]];
If[IdentityElementOfProduct[pos]===None,Throw@Message[DefProduct::error1,pos,"dos not have an identity element"]];

(* Check structure of the product-by-scalar argument *)
If[ListQ[epbs]&&Length[epbs]===2,
pbsside=First[epbs];
pbs=Last[epbs],
pbsside=All;
pbs=epbs
];
If[pbsside=!=Left&&pbsside=!=Right&&pbsside=!=All,
Throw[Message[DefProduct::invalid,pbsside,"sideness of the product by scalars"]]
];
If[pbsside===All&&MemberQ[Attributes@@{pbs},Flat],pbsside=Left];

(* Register *)
MakeDefInfo[DefProduct,prod,info];
MakexTensions[DefProduct,"Beginning",prod,options];
AppendToUnevaluated[$Products,prod];
SymbolRelations[prod,master,{}];
DefInfo[prod]^=info;
ProductQ[prod]^=True;
GradedProductQ[prod]^=gp;
If[gp,GradeOfProduct[prod,prod]^=gop];
ScalarsOfProduct[prod]^=scQ;
ProductByScalar[prod]^=epbs;
ProductOfScalars[prod]^=pos;

(* Formatting *)
If[systemQ,
SetPrintAs[prod,ToExpression["\"\\["<>ToString[prod]<>"]\""]],
SetPrintAs[prod,PrintAsString[prod,pa]];
(* TODO: add parentheses for nested nonassociative cases *)
MakeBoxes[prod[factors__],StandardForm]:=interpretbox[prod[factors],MakeSequenceBox[{factors},pa,StandardForm]];
];

(* Symmetry. Currently we define no automatic simplification. ToCanonical will do that *)
CommutativityOfProduct[prod]^=com;
If[!GradedProductQ[prod]&&(com==="SuperCommutative"||"SuperAnticommutative"),
Throw@Message[DefProduct::error,"Only graded products can be supercommutative or superanticommutative."]];
If[xso=!=Automatic,
With[{order=xso},
ObjectOrder[prod,freesQ_]=Function[xso[UxSort[First[#1]],UxSort[First[#2]]]]
]
];

(* Distributivity of prod (ADL/R) *)
MakeDistributiveProduct[prod,asQ,0];

(* Linearity of prod with respect to pbs (LINL/R) *)
MakeLinearProduct[prod,asQ,scQ,pbs,pbsside];

(* If pbs is not yet a product, add its properties *)
If[!ProductQ[pbs],
ProductQ[pbs]^=True;
With[{one=IdentityElementOfProduct[pos],zero=0},
If[pbsside===Left||pbsside===All,
pbs[one,x_]:=x;
pbs[zero,x_]:=zero;
(* MP Left *)
pbs[n_,pbs[m_,x_]]:=pbs[pos[n,m],x];
];
If[pbsside===Right||pbsside===All,
pbs[x_,one_]:=x;
pbs[x_,zero]:=zero;
(* MP Right *)
pbs[pbs[x_,n_],m_]:=pbs[x,pos[n,m]];
];
(* Distributivity of pbs (MDL/R). Slight abuse of notation, not using pbsside and the same zero *)
MakeDistributiveProduct[pbs,False,zero];
]
];

(* Associated commutator product *)
If[dc,
(* TODO: This check should be either removed or extended to all commutators *)
If[com==="Commutative",
Print["** DefProduct: Commutator of a commutative product vanishes identically."];
Commutator[prod][__]:=0
];
MakeLinearProduct[Commutator[prod],False,scQ,pbs,pbsside];
MakeLinearProduct[Anticommutator[prod],False,scQ,pbs,pbsside];
MakeLinearProduct[Supercommutator[prod],False,scQ,pbs,pbsside];
];

(* Associativity *)
AssociativeProductQ[prod]^=asQ;
If[asQ,
SetAttributes[prod,{Flat,OneIdentity}];
Verbatim[prod][expr_]:=expr;,
prod[]:=Throw@Message[prod::error,"Found expression "<>ToString[prod]<>"[]."];
];

(* Identity element *)
IdentityElementOfProduct[prod]^=ie;
If[ie===None,
If[asQ,Throw@Message[DefProduct::error,"An associative product must be defined with an identity element."]],
If[gp&&Grade[ie,prod]=!=0,Throw@Message[DefProduct::error1,"The identity element of a graded algebra must have grade 0, instead of",Grade[ie,prod]]
];
(* Is the identity also a scalar? If scQ[ie] stays unevaluated, we assume it is not *)
isQ=TrueQ[scQ[ie]];
If[asQ,
Verbatim[prod][]:=ie;
If[isQ,
Verbatim[prod][left___,x_?scQ,right___]:=pbs[x,prod[left,right]],
Verbatim[prod][left___,ie,right___]:=prod[left,right]
],
If[isQ,
Verbatim[prod][expr_,x_?scQ]:=pbs[x,expr];
Verbatim[prod][x_?scQ,expr_]:=pbs[x,expr],
Verbatim[prod][expr_,ie]:=expr;
Verbatim[prod][ie,expr_]:=expr;
]
];
If[dc,
Commutator[prod][x_,ie]:=0;
Commutator[prod][ie,x_]:=0;
];
];

(* Products are declared to be listable by default, mainly to allow automatic threading through Equal *)
SetAttributes[prod,Listable];

(* Finalize *)
MakexTensions[DefProduct,"End",prod,options];
If[pns,Protect[prod]];
];
SetNumberOfArguments[DefProduct,{1,Infinity}];
Protect[DefProduct];


(* Distributivity (automatic expansion) *)
MakeDistributiveProduct[prod_,True,zero_]:=(
prod[left___,zero,right___]:=zero;
prod[left___,sum_Plus,right___]:=prod[left,#,right]&/@sum;
(* This is incorrect. It needs a generalization of SeriesDataMap *)
prod[left___,sum_SeriesData,right___]:=SeriesDataMap[prod[left,#,right]&,sum];
);
MakeDistributiveProduct[prod_,False,zero_]:=(
prod[zero,x_]:=zero;
prod[x_,zero]:=zero;
prod[sum_Plus,x_]:=prod[#,x]&/@sum;
prod[x_,sum_Plus]:=prod[x,#]&/@sum;
(* These are incorrect. They need a generalization of SeriesDataMap *)
prod[sum_SeriesData,x_]:=SeriesDataMap[prod[#,x]&,sum];
prod[x_,sum_SeriesData]:=SeriesDataMap[prod[x,#]&,sum];
);
(* Linearity *)
MakeLinearProduct[prod_,True,scQ_,pbs_,Left]:=(
prod[left___,pbs[number_?scQ,x_],right___]:=pbs[number,prod[left,x,right]];
);
MakeLinearProduct[prod_,True,scQ_,pbs_,Right]:=(
prod[left___,pbs[x_,number_?scQ],right___]:=pbs[prod[left,x,right],number];
);
MakeLinearProduct[prod_,False,scQ_,pbs_,Left]:=(
prod[pbs[number_?scQ,x_],y_]:=pbs[number,prod[x,y]];
prod[x_,pbs[number_?scQ,y_]]:=pbs[number,prod[x,y]];
);
MakeLinearProduct[prod_,False,scQ_,pbs_,Right]:=(
prod[pbs[x_,number_?scQ],y_]:=pbs[prod[x,y],number];
prod[x_,pbs[y_,number_?scQ]]:=pbs[prod[x,y],number];
);
MakeLinearProduct[prod_,asQ_,scQ_,pbs_,All]:=(
MakeLinearProduct[prod,asQ,scQ,pbs,Left];
MakeLinearProduct[prod,asQ,scQ,pbs,Right];
);


UndefProduct[list:{___?ProductQ}]:=Scan[UndefProduct,list];
UndefProduct[prod_Symbol]:=Catch@With[{servants=ServantsOf[prod]},
If[!ProductQ[prod],Throw@Message[UndefInertHead::unknown,"product",prod]];
CheckRemoveSymbol[prod];
MakexTensions[UndefProduct,"Beginning",prod];
xUpSet[ServantsOf[prod],{}];
DropFromHosts[prod];
Undef/@Reverse[servants];
$Products=DeleteCases[$Products,prod];
MakexTensions[UndefProduct,"End",prod];
MakeUndefInfo[UndefProduct,prod];
If[MemberQ[$MathematicaProductSymbols,prod],
Clear[prod],
RemoveSymbol[prod]
]
];
SetNumberOfArguments[UndefProduct,1];
Protect[UndefProduct];


ScalarsOfProduct[prod_]:=Throw@Message[ScalarsOfProduct::unknown,"product",prod];
SetNumberOfArguments[ScalarsOfProduct,1];
Protect[ScalarsOfProduct];


ProductByScalar[prod_]:=Throw@Message[ProductByScalar::unknown,"product",prod];
SetNumberOfArguments[ProductByScalar,1];
Protect[ProductByScalar];


GradedProductQ[prod_]:=Throw@Message[GradedProductQ::unknown,"product",prod];
SetNumberOfArguments[GradedProductQ,1];
Protect[GradedProductQ];


IdentityElementOfProduct[prod_]:=Throw@Message[IdentityElementOfProduct::unknown,"product",prod];
SetNumberOfArguments[IdentityElementOfProduct,1];
Protect[IdentityElementOfProduct];


CommutativityOfProduct[prod_]:=Throw@Message[CommutativityOfProduct::unknown,"product",prod];
SetNumberOfArguments[CommutativityOfProduct,1];
Protect[CommutativityOfProduct];


(* Compute flipping signs for the various possibilities *)
CommutativitySign[prod_,"Commutative"][x_,y_]:=1;
CommutativitySign[prod_,"Anticommutative"][x_,y_]:=-1;
CommutativitySign[prod_,"SuperCommutative"][x_,y_]:=(-1)^(Grade[x,prod]Grade[y,prod]);
CommutativitySign[prod_,"SuperAnticommutative"][x_,y_]:=-(-1)^(Grade[x,prod]Grade[y,prod]);
CommutativitySign[prod_,{"SuperCommutative",p_Integer}][x_,y_]:=(-1)^((Grade[x,prod]-p)(Grade[y,prod]-p));
CommutativitySign[prod_,{"SuperAnticommutative",p_Integer}][x_,y_]:=-(-1)^((Grade[x,prod]-p)(Grade[y,prod]-p));
CommutativitySign[prod_,com:(_String|_List)][x_,y_]:=Throw@Message[prod::error1,"xTensor not yet ready to handle symmetry ",com];
CommutativitySign[prod_,f_][x_,y_]:=f[x,y];


Unprotect[Times];
ProductQ[Times]^=True;
AssociativeProductQ[Times]^=True;
CommutativityOfProduct[Times]^="Commutative";
IdentityElementOfProduct[Times]^=1;
GradedProductQ[Times]^=False;
ProductByScalar[Times]^=Times;
Protect[Times];


(* This is a modified version *)
MakeDistributiveProduct[prodL_,prodR_,True,zero_]:=(
prodL[left___,zero,right___]:=zero;
prodL[left___,sum_Plus,right___]:=prodR[left,#,right]&/@sum;
(* This is incorrect. It needs a generalization of SeriesDataMap *)
prodL[left___,sum_SeriesData,right___]:=SeriesDataMap[prodR[left,#,right]&,sum];
);


DeclareCommutator[commutator_,commutativity_]:=(
ProductQ[commutator[prod_]]^:=ProductQ[prod];
ScalarsOfProduct[commutator[prod_]]^:=ScalarsOfProduct[prod];
ProductByScalar[commutator[prod_]]^:=ProductByScalar[prod];
CommutativityOfProduct[commutator[prod_]]^:=commutativity;
AssociativeProductQ[commutator[prod_]]^:=False;
IdentityElementOfProduct[commutator[prod_]]^:=None;
(* Commutator inherits gradation from prod *)
GradedProductQ[commutator[prod_]]^:=GradedProductQ[prod];
MakeDistributiveProduct[commutator[prod_],commutator[prod],True,0];
);
DeclareCommutator[Commutator,"Anticommutative"];
DeclareCommutator[Anticommutator,"Commutative"];
DeclareCommutator[Supercommutator,"SuperAnticommutative"];


MakeBoxes[Commutator[prod_][a_,b_],StandardForm]:=interpretbox[Commutator[prod][a,b],SubscriptBox[RowBox[{"[",MakeBoxes[a,StandardForm],",",MakeBoxes[b,StandardForm],"]"}],PrintAs[prod]]];
MakeBoxes[Anticommutator[prod_][a_,b_],StandardForm]:=interpretbox[Anticommutator[prod][a,b],SubscriptBox[RowBox[{"{",MakeBoxes[a,StandardForm],",",MakeBoxes[b,StandardForm],"}"}],PrintAs[prod]]];
MakeBoxes[Supercommutator[prod_][a_,b_],StandardForm]:=interpretbox[Supercommutator[prod][a,b],SubscriptBox[RowBox[{"\[LeftDoubleBracket]",MakeBoxes[a,StandardForm],",",MakeBoxes[b,StandardForm],"\[RightDoubleBracket]"}],PrintAs[prod]]];


ExpandCommutatorRule={Commutator[prod_][a_,b_]:>prod[a,b]-prod[b,a]};
ExpandAnticommutatorRule={Anticommutator[prod_][a_,b_]:>prod[a,b]+prod[b,a]};
ExpandSupercommutatorRule={Supercommutator[prod_][a_,b_]:>
prod[a,b]-(-1)^(Grade[a,prod]Grade[b,prod])prod[b,a]};


ExpandCommutator[expr_]:=expr//.Join[ExpandCommutatorRule,ExpandAnticommutatorRule,ExpandSupercommutatorRule];


checkLengthOneGrade[{x_}]:=True;
checkLengthOneGrade[x:{_,__}]:=Throw@Message[Validate::inhom,"grades",ToString[x]];


Grade[sum_Plus,prod_Symbol?ProductQ]:= With[{glist=Union[Grade[#,prod]&/@List@@sum]},
If[checkLengthOneGrade[glist],First[glist]]
];


Grade[prod1_?ProductQ[factors___],prod2_Symbol?ProductQ]:=With[{ppgrade=GradeOfProduct[prod1,prod2]},
Apply[Plus,Grade[#,prod2]&/@{factors}]+ppgrade/;IntegerQ[ppgrade]
];
SetNumberOfArguments[GradeOfProduct,2];
Protect[GradeOfProduct];


(* Definitions for Grade. Check that the product is graded *)
Grade[c_?ConstantQ,prod_?GradedProductQ]:=0;
Grade[p_?ParameterQ,prod_?GradedProductQ]:=0;
Grade[Scalar[expr_],prod_?GradedProductQ]:=Grade[expr,prod];
Grade[tensor_?xTensorQ[inds___],prod_?GradedProductQ]:=GradeOfTensor[tensor,prod]+Plus@@(GradeOfIndex[#,prod]&/@{inds});
Grade[der_?FirstDerQ[expr_],prod_?GradedProductQ]:=Grade[expr,prod]+GradeOfDer[der,prod];
Grade::sfg0="Arguments `1` of scalar function `2` must all have grade 0, but have `3` instead.";
Grade[sf:_?ScalarFunctionQ[args___],prod_?GradedProductQ]:=With[{glist=Grade[#,prod]&/@{args}},
If[Union[glist]==={0},
0,
Throw@Message[Grade::sfg0,{args},sf,glist]
]
];
(* Grades of inert-head expressions must be provided by the user *)
Grade[ih:_?InertHeadQ[__],prod_?GradedProductQ]:=Throw@Message[Grade::unknown,"grade of inert-head expression",ih];
(* Default: for graded products it is 0. In other cases throw an error message *)
Grade::nogr="Product `1` is not graded.";
Grade[expr_,prod_?GradedProductQ]:=0;
Grade[expr_,prod_?ProductQ]:=Throw@Message[Grade::nogr,prod];
Grade[expr_,prod_]:=Throw@Message[Grade::unknown,"product",prod];

(* Secondary definitions. We have already checked that the product is graded *)
GradeOfTensor[TensorDerivative[tensor_,ders__],prod_]:=GradeOfTensor[tensor,prod]+Plus@@(GradeOfDer[#,prod]&/@{ders});
GradeOfTensor[Bracket[v1_,v2_],prod_]:=Grade[v1,prod]+Grade[v2,prod];
GradeOfDer[covd_?CovDQ[inds__],prod_]:=GradeOfCovD[covd,prod]+Plus@@(GradeOfIndex[#,prod]&/@{inds});
GradeOfDer[LieD[v_],prod_]:=Grade[v,prod];
GradeOfIndex[Dir[v_],prod_]:=Grade[v,prod];
GradeOfIndex[{i_,basis_},prod_]:=GradeOfBIndex[{i,basis},prod];
(* Zero defaults for the secondary definitions *)
GradeOfTensor[tensor_,prod_]:=0;
GradeOfDer[der_,prod_]:=0;
GradeOfCovD[covd_,prod_]:=0;
GradeOfIndex[i_,prod_]:=0;
GradeOfBIndex[bi_,prod_]:=0;


SetNumberOfArguments[Grade,2];
Protect[Grade];


DefSign[sign_,printas_,default_]:=(
DefConstantSymbol[sign,PrintAs->printas,Validate->False,ProtectNewSymbol->False,DefInfo->False];
sign/:Power[sign,n_Integer]:=If[EvenQ[n],1,sign];
sign=default
);


DefSign[$epsilonSign,"\!\(\*SubscriptBox[\(s\), \(\[Epsilon]\)]\)",1]


DefSign[$RiemannSign,"\!\(\*SubscriptBox[\(s\), \(R\)]\)",1]


DefSign[$RicciSign,"\!\(\*SubscriptBox[\(s\), \(r\)]\)",1]


$RicciSign=1;


DefSign[$TorsionSign,"\!\(\*SubscriptBox[\(s\), \(T\)]\)",1]


ConstantQ[_Integer|_Rational|_Real|_Complex]:=True;
ConstantQ[x_]:=NumericQ[x]||ConstantSymbolQ[x];
ConstantQ[_?ScalarFunctionQ[___?ConstantQ]]:=True;
SetNumberOfArguments[ConstantQ,1];
Protect[ConstantQ];


(* Shortcuts *)
NonIndexedScalarQ[Scalar[_]]:=True;
NonIndexedScalarQ[_?xTensorQ[]]:=True;
NonIndexedScalarQ[_?ScalarFunctionQ[__]]:=True;
NonIndexedScalarQ[_Symbol?ParameterQ]:=True;
NonIndexedScalarQ[_?ConstantQ]:=True;
NonIndexedScalarQ[expr_Plus]:=Apply[And,Map[NonIndexedScalarQ,Apply[List,expr]]];
NonIndexedScalarQ[HoldPattern@SeriesData[var_,origin_,coeffs_,__]]:=Apply[And,Map[NonIndexedScalarQ,coeffs]]&&NonIndexedScalarQ[var]&&NonIndexedScalarQ[origin];
NonIndexedScalarQ[expr_]:=False;
(* Pattern or Slot expressions are not scalars! *)
ScalarQ[_Blank|_Pattern|_PatternTest|_Condition|_Slot]:=False;
(* Identify objects are not scalars *)
ScalarQ[_Object]:=False;
ScalarQ[VerbatimProduct[_][___]]:=False;
ScalarQ[CommutingObjects[_][___]]:=False;
(* Generic definition *)
ScalarQ[expr_]:=NonIndexedScalarQ[expr]||FindFreeIndices[expr]===IndexList[] ;
SetNumberOfArguments[ScalarQ,1];
Protect[ScalarQ];


Scalar[x_Plus]:=Scalar/@x;
Scalar[x_SeriesData]:=SeriesDataMap[Scalar,x];
Scalar[x_?ConstantQ y_.]:=x Scalar[y];
Scalar[x_?NonIndexedScalarQ]:=x/;$SeparateScalarsFromScalar;
Scalar[x_?NonIndexedScalarQ y_]:=Scalar[x] Scalar[y];
Scalar[1]:=1;
SetNumberOfArguments[Scalar,1];
Protect[Scalar];


$SeparateScalarsFromScalar=True;


$ScalarColor=RGBColor[1,0,0];
xTensorFormStart[Scalar]:=(
MakeBoxes[Scalar[expr_],StandardForm]:=interpretbox[Scalar[expr],RowBox[{StyleBox["(",FontColor->$ScalarColor],MakeBoxes[expr,StandardForm],StyleBox[")",FontColor->$ScalarColor]}]]);
xTensorFormStop[Scalar]:=(MakeBoxes[Scalar[expr_],StandardForm]=.);
xTensorFormStart[Scalar];


FirstDerQ[_?CovDQ[_]]:=True;
FirstDerQ[LieD[_]]:=True;
FirstDerQ[ParamD[_]]:=True;
FirstDerQ[OverDot]:=True;
FirstDerQ[_]:=False;
SetNumberOfArguments[FirstDerQ,1];
Protect[FirstDerQ];


SetAttributes[CartesianProduct,{Flat,OneIdentity}];


Verbatim[CartesianProduct][x_]:=x;


Dagger[x_CartesianProduct]^:=Dagger/@x;


CartesianProduct::len="Incompatible lengths `1` of Cartesian products.";
ThreadCartesianProduct[prod_,{cprods__}]:=With[{lengths=Length/@{cprods}},
CartesianProduct@@MapThread[prod,List@@@{cprods},1]/;SameQ@@lengths||Throw@Message[CartesianProduct::len,lengths]
];


PrintAs[x_CartesianProduct]^:=StringJoin@@Riffle[PrintAs/@List@@x,"\[Times]"];
MakeBoxes[CartesianProduct[x__],StandardForm]:=MakeSequenceBox[{x},"\[Times]",StandardForm];


phi_?MappingQ[c_CartesianProduct]


(****************************** 5. Indices *****************************)


If[$ReadingVerbose,Print["Reading section 5: Indices."],Null,Null];


$IndexListColor=RGBColor[0,0,1];


MakeBoxes[IndexList[inds___],StandardForm]:=interpretbox[IndexList[inds],RowBox[Flatten[{StyleBox["{",FontColor->$IndexListColor],MakeSequenceBox[{inds},",\[ThinSpace]",StandardForm],StyleBox["}",FontColor->$IndexListColor]}]]]


ValidateSymbol::capital="System name `1` is overloaded as an abstract index.";


ValidateIndex[symbol_,capitalmessage_:True]:=(
ValidateSymbol[symbol];
ValidateSymbolInSession[symbol];
If[capitalmessage&&xAct`xCore`Private`SystemCapitalQ[symbol],Message[ValidateSymbol::capital,ToString[symbol]]];
);


Options[DefAbstractIndex]={PrintAs->Identity,ProtectNewSymbol:>$ProtectNewSymbols};
DefAbstractIndex[list_List,rest___]:=Scan[DefAbstractIndex[#,rest]&,list];
DefAbstractIndex[symbol_Symbol,options:OptionsPattern[]]:=Module[{pa,pns},
{pa,pns}=OptionValue[{PrintAs,ProtectNewSymbol}];
(* Validate index *)
ValidateIndex[symbol];
(* Value for NoDollar. Store $ModuleNumber to remove associated dollar-indices *)
If[xAct`xCore`Private`SystemCapitalQ[symbol],NoDollar[symbol]=symbol,NoDollar[symbol]^=symbol];
ModuleNumberOfIndex[symbol]=$ModuleNumber;
(* Register index *)
xUpSet[AbstractIndexQ[symbol],True];
SetPrintAs[symbol,PrintAsString[symbol,pa]];
If[pns,Protect[symbol]];
]
SetNumberOfArguments[DefAbstractIndex,{1,Infinity}];
Protect[DefAbstractIndex];


UndefAbstractIndex[list:{___?AbstractIndexQ}]:=Scan[UndefAbstractIndex,list];
UndefAbstractIndex[symbol_]:=Module[{},
If[!AbstractIndexQ[symbol],Throw@Message[UndefAbstractIndex::unknown,"index",symbol]];
If[xAct`xCore`Private`SystemCapitalQ[symbol],xUpSet[UpValues[symbol],{}];Return[]];
CheckRemoveSymbol[symbol];
RemoveDollarIndices[ModuleNumberOfIndex[symbol],symbol];
RemoveSymbol[symbol];
];
SetNumberOfArguments[UndefAbstractIndex,1];
Protect[UndefAbstractIndex];


RemoveDollarIndices[number_Integer]:=Map[RemoveDollarIndices[number,#]&,$AbstractIndices];
RemoveDollarIndices[number_Integer,index_Symbol]:=Remove/@Cases[numberpair/@Names[StringJoin[ToString[index],"$*"]],{x_,n_}:>x/;n>=number];
numberpair[string_String]:={string,ToExpression@StringJoin[Characters[string]//.{{___,"$"}->{"0"},{___,"$",n__}->{n}}]};


IModQ[_Symbol?BasisQ]:=True;
IModQ[_]:=False;
SetNumberOfArguments[IModQ,1];
Protect[IModQ];


(* Exception: D-indices *)
UpIndex[Dir[v_]]:=With[{u=UltraindexOf[v]},Dir[ReplaceIndex[v,u->DownIndex[u]]]];
DownIndex[Dir[v_]]:=With[{u=UltraindexOf[v]},Dir[ReplaceIndex[v,u->UpIndex[u]]]];
ChangeIndex[Dir[v_]]:=With[{u=UltraindexOf[v]},Dir[ReplaceIndex[v,u->ChangeIndex[u]]]];


(* Exception: C-indices. BASIS1 *)
UpIndex[{a_Integer,basis_}]:={a,UpIndex[basis]};
DownIndex[{a_Integer,basis_}]:={a,DownIndex[basis]};
ChangeIndex[{a_Integer,basis_}]:={a,-basis};


(* Exception: B-Indices. BASIS1 *)
UpIndex[{a_,basis_}]:={UpIndex[a],UpIndex[basis]};


(* New exception: integers *)
UpIndex[a_Integer?Negative]:=-a;


(* Main definitions, handling all other cases *)
UpIndex[-a_]:=a;
UpIndex[a_]:=a;
DownIndex[a_]:=-UpIndex[a];
ChangeIndex[a_]:=-a;


SetNumberOfArguments[#,1]&/@{UpIndex,DownIndex,ChangeIndex};
Protect[UpIndex,DownIndex,ChangeIndex];


upQ[a_]:=a===UpIndex[a]
downQ[a_]:=a===DownIndex[a]


(* Accepted notations for up-indices *)
UpIndexQ[a_Symbol]:=True;
UpIndexQ[a_LI]:=True;
UpIndexQ[a_Dir]:=DownIndexQ[UltraindexOf[a]];
UpIndexQ[a_Integer]:=Positive[a];
UpIndexQ[{a_,b_Symbol}]:=True; (* BASIS1 *);
UpIndexQ[(b_?IModQ)[a_]]:=True; (* BASIS2 & IMOD *);
UpIndexQ[a_]:=False;
SetNumberOfArguments[UpIndexQ,1];
Protect[UpIndexQ];


(* Accepted notations for down-indices *)
DownIndexQ[-a_Symbol]:=True;
DownIndexQ[-a_LI]:=True;
DownIndexQ[a_Dir]:=UpIndexQ[UltraindexOf[a]];
DownIndexQ[a_Integer]:=Negative[a];
DownIndexQ[{a_,-b_Symbol}]:=True; (* BASIS1 *);
DownIndexQ[-(b_?IModQ)[a_]]:=True; (* BASIS2 & IMOD *);
DownIndexQ[a_]:=False;
SetNumberOfArguments[DownIndexQ,1];
Protect[DownIndexQ];


(* Signs are irrelevant for contractibility *)
EIndexQ[-a_]:=EIndexQ[a];
BlockedQ[-a_]:=BlockedQ[a];
(* Abstract indices are always contractible *)
EIndexQ[_Symbol]:=True;
Blocked[_Symbol]:=False;


(* Priority to BlockedQ *)
BlockedQ[{_Integer,_}]:=True; (* BASIS1 *);
BlockedQ[_Symbol?BasisQ[_Integer]]:=True; (* BASIS2 *);
BlockedQ[_LI]:=True;
BlockedQ[_Dir]:=True;
BlockedQ[_]:=False;
EIndexQ[a_]:=Not@BlockedQ[a];
SetNumberOfArguments[#,1]&/@{EIndexQ,BlockedQ};
Protect[EIndexQ,BlockedQ];


(* Separate E-indices from blocked indices *)
TakeEIndices[list_]:=Select[list,EIndexQ];
TakeBlocked[list_]:=Select[list,BlockedQ];

(* Take or drop pairs *)
TakePairs[list1_,list2_]:=Intersection[list1,ChangeIndex/@list2];
DropPairs[list1_,list2_]:=Complement[list1,ChangeIndex/@list2];
TakePairs[list_]:=TakePairs[list,list];
DropPairs[list_]:=DropPairs[list,list];

(* Take or drop E-pairs *)
TakeEPairs[list_]:=TakePairs[list,TakeEIndices[list]];
DropEPairs[list_]:=DropPairs[list,TakeEIndices[list]];

TakeFrees[list_]:=DropPairs[TakeEIndices[list]];
TakeOrderedEPairs[list_]:=DeleteCases[list,Alternatives@@Verbatim/@DropEPairs[list]];


(* Contracted B-indices *)
PairQ[{a_Symbol,basis_Symbol},{-a_Symbol,-basis_Symbol}]:=True; (* BASIS1 *);
PairQ[{-a_Symbol,-basis_Symbol},{a_Symbol,basis_Symbol}]:=True; (* BASIS1 *);
(* Contracted AB-indices, including BASIS2 & IMOD *)
PairQ[a_,-a_?EIndexQ]:=True;
PairQ[-a_,a_?EIndexQ]:=True;
(* Other cases *)
PairQ[_,_]:=False;
SetNumberOfArguments[PairQ,2];
Protect[PairQ];


TakeRepeated[list_IndexList]:=First/@DeleteCases[Split@Sort[list],IndexList[_]]


TakeUp[list_]:=Select[list,upQ];


(* Patterns. PINDEX *)
NoDollar[a_Blank]:=a;
NoDollar[a:(_Pattern|_PatternTest)]:=ReplacePart[a,NoDollar[First[a]],1];
(* Symbols. Use remembered values as upvalues of the index. Remove from the first dollar *)
NoDollar[a_Symbol]:=NoDollar[a]^=ToExpression[StringJoin[Characters[ToString[a]]/.{i___,"$",___}:>{i}]];
(* Signs: anything but integers *)
NoDollar[-a_]:=-NoDollar[a];
(* Integers *)
NoDollar[a_Integer]:=a;
(* Directions *)
NoDollar[v_Dir]:=v;
NoDollar[a:-_Dir]:=Throw@Message[Validate::nouse,"NoDollar",ToString[a]];
(* Labels *)
NoDollar[a_LI]:=a;
(* Component and basis indices *)
NoDollar[{a_,basis_}]:={NoDollar[a],basis}; (* BASIS1 *);
NoDollar[imod_?IModQ[a_]]:=imod[NoDollar[a]]; (* BASIS2 & IMOD *);
(* Other *)
NoDollar[]:=Throw@Message[Validate::nouse,"NoDollar","0 arguments"];
NoDollar[_,__]:=Throw@Message[Validate::nouse,"NoDollar","more than 1 arguments"];
NoDollar[a_]:=(Message[General::warning,"NoDollar has been used on "<>ToString[a],$WarningFrom];a);


(* Detect dollar indices *)
DollarQ[_LI|_Dir|_Blank|_Integer]:=False;
DollarQ[-a_]:=DollarQ[a];
DollarQ[symbol_Symbol]:=StringMatchQ[SymbolName[symbol],"*$*"];
DollarQ[x:(_Pattern|_PatternTest)]:=DollarQ[First[x]]; (* PINDEX *); 
DollarQ[{a_,_}]:=DollarQ[a]; (* BASIS1 *);
DollarQ[imod_?IModQ[a_]]:=DollarQ[a]; (* BASIS2 & IMOD *);


(* Thread over IndexList *)
DaggerIndex[list_IndexList]:=DaggerIndex/@list;
(* Abstract indices *)
DaggerIndex[-x_]:=-DaggerIndex[x];
DaggerIndex[x_Symbol]:=If[DaggerQ@VBundleOfIndex[x],
With[{dgx=MakeDaggerSymbol[x]},xUpSet[DaggerIndex[dgx],x];xUpSet[DaggerIndex[x],dgx]],
xUpSet[DaggerIndex[x],x]
];
(* Formal sequence of abstract indices *)
DaggerIndex[AnyIndices[vbundle_?VBundleQ]]:=AnyIndices[Dagger[vbundle]];
(* By default labels are considered real *)
DaggerIndex[x_LI]:=x;
(* Directional indices *)
DaggerIndex[Dir[v_]]:=Dir[Dagger[v]];
(* Component indices *)
DaggerIndex[ind:{a_Integer,basis_}]:=xAct`xCoba`DaggerCIndex[basis,ind]; (* BASIS1 *);
DaggerIndex[ind:basis_Symbol?BasisQ[a_Integer]]:=xAct`xCoba`DaggerCIndex[basis,ind]; (* BASIS2 *);
(* Basis indices *)
DaggerIndex[ind:{a_,basis_}]:=xAct`xCoba`Private`DaggerBIndex[basis,ind]; (* BASIS1 *);
DaggerIndex[ind:basis_Symbol?BasisQ[a_]]:=xAct`xCoba`Private`DaggerBIndex[basis,ind]; (* BASIS2 *);
(* Index modifiers *)
DaggerIndex[ind:imod_?IModQ[a_]]:=DaggerIMod[imod,ind]; (* IMOD *)
(* Integers *)
DaggerIndex[ind_Integer]:=(Message[DaggerIndex::error,"DaggerIndex on an integer. Assuming everything is real."];ind);
(* Default *)
DaggerIndex[x_]:=Throw@Message[DaggerIndex::unknown,"index",x];
SetNumberOfArguments[DaggerIndex,1];
Protect[DaggerIndex];


DaggerIndexQ[-x_]:=DaggerIndexQ[x];
DaggerIndexQ[x_Symbol]:=HasDaggerCharacterQ[x];
DaggerIndexQ[v_Dir]:=DaggerIndexQ[UltraindexOf[v]];
DaggerIndexQ[x_LI]:=False;
DaggerIndexQ[x_Integer]:=False;
DaggerIndexQ[{x_,basis_}]:=HasDaggerCharacterQ[basis]; (* BASIS1 *);
DaggerIndexQ[basis_Symbol?BasisQ[x_]]:=HasDaggerCharacterQ[basis]; (* BASIS2 *);
DaggerIndexQ[imod_?IModQ[x_]]:=HasDaggerCharacterQ[x]; (* IMOD *)
DaggerIndexQ[list_IndexList]:=UnsameQ[DaggerIndex[list],list];
(* The default is False. True? *)
DaggerIndexQ[x_]:=False;


PullBackIndex[x_,_IdentityMapping]:=x;
PullBackIndex[-x_,phi_]:=-PullBackIndex[x,phi];
PullBackIndex[x_Symbol,phi_]:=xTagSet[{x,PullBackIndex[x,phi]},RegisterPullBackIndex[MakePullBackIndex[x,phi],PullBackVBundle[VBundleOfIndex[x],phi]]];
PullBackIndex[AnyIndices[vbundle_?VBundleQ],phi_]:=AnyIndices[PullBackVBundle[vbundle,phi]];
PullBackIndex[x_LI,phi_]:=x;
(* We don't give freeindex rules. We let the system choose the indices *)
PullBackIndex[Dir[v_],phi_]:=Dir[PullBack[v,phi]];
(* We assume that a basis and its pullback have the same cnumbers, and in the same order *)
PullBackIndex[a_Integer,phi_]:=a;
PullBackIndex[{a_,basis_},phi_]:={PullBackIndex[a,phi],PullBackBasis[basis,phi]}; (* BASIS1 *)
PullBackIndex[basis_Symbol?BasisQ[a_],phi_]:=PullBackBasis[basis,phi][PullBackIndex[a,phi]]; (* BASIS2 *)
PullBackIndex[imod_?IModQ[a_],phi_]:=PullBackIMod[imod,phi][PullBackIndex[a,phi]]; (* IMOD *)
PullBackIndex[x_,phi_]:=Throw@Message[PullBack::unknown,"index",x];
SetNumberOfArguments[PullBackIndex,2];
Protect[PullBackIndex];


PullBackCharacter[phi_Symbol]:=$StarCharacter;
PullBackCharacter[SmallCircle[phis__]]:=ConstantArray[$StarCharacter,Length[{phis}]];


MakePullBackIndex[x_Symbol,phi_]:=With[{pbx=JoinCharacters[phi,x]},
SetPrintAs[pbx,"\!\("<>PrintAs[x]<>"\^"<>PullBackCharacter[phi]<>"\)"];
pbx
];


SetAttributes[CharacterList,HoldFirst];
CharacterList[phi_Symbol]:=Characters[ToString[phi]];
CharacterList[SmallCircle[phis__Symbol]]:=Flatten[CharacterList/@Reverse[{phis}]];
CharacterList[phi_]:=Throw@Message[PullBack::error1,"Cannot handle pullback of index under map",phi];
JoinCharacters[phi_,index_]:=ToExpression[StringJoin[Join[CharacterList[phi],CharacterList[index]]]];


RegisterPullBackIndex[index_,vbundle_PullBackVBundle]:=With[{inds=IndicesOfVBundle[vbundle]},
If[!MemberQ[Flatten@inds,index],
xUpSet[AbstractIndexQ[index],True];
If[!DollarQ[index],AddIndexInSecondList[vbundle,index]]
];
index
];


IndexOnQ[index_,vbundle_]:=SubvbundleQ[vbundle,VBundleOfIndex[index]];


VBundleIndexQ[vbundle_]:=SymbolJoin@@Append[VBundleNameList[vbundle],"`Q"];
VBundleIndexPMQ[vbundle_]:=SymbolJoin@@Append[VBundleNameList[vbundle],"`pmQ"];
VBundleNameList[vbundle_Symbol]:={vbundle};
VBundleNameList[xAct`xCoba`TangentReals[dim_]]:={"TR",ToString[dim]};
MappingNameList[phi_Symbol]:={phi};
MappingNameList[SmallCircle[phis__Symbol]]:=Reverse[{phis}];
VBundleNameList[PullBackVBundle[vbundle_,phi_]]:=Join[MappingNameList[phi],VBundleNameList[vbundle]];
VBundleNameList[vbundle_]:=Throw@Message[VBundleIndexQ::error1,"TODO: Don't know yet how to construct the `Q and `pmQ functions of vbundle ",vbundle];


AIndexQ[-i_Symbol]:=AIndexQ[i];
AIndexQ[i_Symbol]:=AbstractIndexQ[NoDollar[i]];
AIndexQ[_]:=False;
AIndexQ[i_,vbundle_]:=AIndexQ[i]&&IndexOnQ[i,vbundle];
(* Special definition for a formal sequence of abstract indices *)
AIndexQ[AnyIndices[vbundle_]]:=VBundleQ[vbundle];
SetNumberOfArguments[AIndexQ,{1,2}];
Protect[AIndexQ];


(* A funny trick: AIndex is a basis *)
BasisQ[AIndex]^=True;
xAct`xCoba`BasisColor[AIndex]^=RGBColor[0,0,0];
xAct`xCoba`PDOfBasis[AIndex]^=PD;
xAct`xCoba`DependenciesOfBasis[AIndex]^={};
Dagger[AIndex]^=AIndex;


(* One argument: check consistency of abstract index and basis *)
BIndexQ[-i_]:=BIndexQ[i];
BIndexQ[{i_?AIndexQ,basis_?BasisQ}]:=True; (* BASIS1 *);
BIndexQ[{i_?AIndexQ,-basis_?BasisQ}]:=True; (* BASIS1 *);
BIndexQ[basis_?BasisQ[i_?AIndexQ]]:=True; (* BASIS2 *);
BIndexQ[i_]:=False;
(* Two arguments: index and basis/vbundle *)
BIndexQ[-i_,x_]:=BIndexQ[i,x];
BIndexQ[i:{_,basis_},basis_]:=BIndexQ[i]; (* BASIS1 *);
BIndexQ[i:{_,-basis_},basis_]:=BIndexQ[i]; (* BASIS1 *);
BIndexQ[i:basis_[_],basis_]:=BIndexQ[i]; (* BASIS2 *);
BIndexQ[i_,vbundle_?VBundleQ]:=BIndexQ[i]&&IndexOnQ[i,vbundle];
BIndexQ[i_,_]:=False;
SetNumberOfArguments[BIndexQ,{1,2}];
Protect[BIndexQ];


(* One argument: check consistency of integer and basis *)
CIndexQ[-i_]:=CIndexQ[i];
CIndexQ[{i_Integer,basis_}]:=MemberQ[xAct`xCoba`CNumbersOf[basis],i]; (* BASIS1 *);
CIndexQ[basis_[i_Integer]]:= MemberQ[xAct`xCoba`CNumbersOf[basis],i]; (* BASIS2 *);
CIndexQ[i_]:=False;
(* Two arguments: index and basis/vbundle *)
CIndexQ[-i_,x_]:=CIndexQ[i,x];
CIndexQ[i:{_Integer,basis_},basis_]:=CIndexQ[i]; (* BASIS1 *);
CIndexQ[i:{_Integer,-basis_},basis_]:=CIndexQ[i]; (* BASIS1 *);
CIndexQ[i:basis_[_Integer],basis_]:=CIndexQ[i]; (* BASIS2 *);
CIndexQ[i_,vbundle_?VBundleQ]:=CIndexQ[i]&&IndexOnQ[i,vbundle];
CIndexQ[i_,_]:=False;
SetNumberOfArguments[CIndexQ,{1,2}];
Protect[CIndexQ];


LIndexQ[-_LI]:=True;
LIndexQ[_LI]:=True;
LIndexQ[_]:=False;
SetNumberOfArguments[LIndexQ,1];
Protect[LIndexQ];


DIndexQ[dir_Dir]:=dir===ValidateDir[dir];
DIndexQ[x_]:=False;
DIndexQ[dir_,vbundle_]:=DIndexQ[dir]&&IndexOnQ[UltraindexOf[dir],vbundle]
SetNumberOfArguments[DIndexQ,{1,2}];
Protect[DIndexQ];


MIndexQ[_?IModQ[i_]]:=GIndexQ[i];
MIndexQ[_]:=False;
SetNumberOfArguments[MIndexQ,1];
Protect[MIndexQ];


ABIndexQ[i_]:=AIndexQ[i]||BIndexQ[i];
ABIndexQ[i_,master_]:=AIndexQ[i,master]||BIndexQ[i,master];
SetNumberOfArguments[ABIndexQ,{1,2}];
Protect[ABIndexQ];


BCIndexQ[i_]:=BIndexQ[i]||CIndexQ[i];
BCIndexQ[i_,master_]:=BIndexQ[i,master]||CIndexQ[i,master];
SetNumberOfArguments[BCIndexQ,{1,2}];
Protect[BCIndexQ];


CDIndexQ[i_]:=CIndexQ[i]||DIndexQ[i];
CDIndexQ[i_,master_]:=CIndexQ[i,master]||DIndexQ[i,master];
SetNumberOfArguments[CDIndexQ,{1,2}];
Protect[CDIndexQ];


(* Signs *)
GIndexQ[-_Dir]:=False;
GIndexQ[-x_]:=GIndexQ[x];
(* Patterns. Dropped in version 0.8 *)
(* GIndexQ[x:(_Blank|_Pattern|_PatternTest)]:=PIndexQ[x] *)
(* Known heads *)
GIndexQ[i_Symbol]:=AIndexQ[i];
GIndexQ[x_List]:=BCIndexQ[x]; (* BASIS1 *);
GIndexQ[x:_Symbol?BasisQ[_]]:=BCIndexQ[x]; (* BASIS2 *);
GIndexQ[x_Dir]:=DIndexQ[x];
GIndexQ[x_LI]:=True;
GIndexQ[_?IModQ[i_]]:=GIndexQ[i];
(* VBundle check *)
GIndexQ[i_,vbundle_]:=GIndexQ[i]&&IndexOnQ[i,vbundle];
(* Other *)
GIndexQ[_]:=False;
SetNumberOfArguments[GIndexQ,{1,2}];
Protect[GIndexQ];


ValidateGIndex[i_]:=If[GIndexQ[i],i,Throw@Message[GIndexQ::unknown,"GIndex",i]];


ValidateIndexList[head_[inds___]]:=ValidateGIndex/@IndexList[inds];


(* Pattern recognition. BASIS1. PINDEX *)
PIndexQ[-x_]:=PIndexQ[x];
PIndexQ[{x_,basis_}]:=PIndexQ[x];
PIndexQ[Verbatim[Blank[]]]:=(Message[General::warning,"PIndexQ used on an empty Blank.",$WarningFrom];True);
PIndexQ[Verbatim[Blank[Symbol]]]:=True;
PIndexQ[Verbatim[Blank[List]]]:=True;
PIndexQ[Verbatim[Blank[Dir]]]:=True;
PIndexQ[Verbatim[Blank[LI]]]:=True;
PIndexQ[Verbatim[Blank[Blank]]]:=True;
PIndexQ[Verbatim[Blank[Pattern]]]:=True;
PIndexQ[Verbatim[Blank[PatternTest]]]:=True;
PIndexQ[x_Blank]:=(Message[Validate::nouse,"PIndexQ",ToString[x]];False);
PIndexQ[x_Pattern]:=PIndexQ[x[[2]]]&&AIndexQ[First[x]];
PIndexQ[x_PatternTest]:=PIndexQ[First[x]];
PIndexQ[_Condition|_Optional|_Alternatives|_BlankSequence|_BlankNullSequence|_Repeated|_RepeatedNull]:=Throw[Message[General::nouse,"PIndexQ","a forbidden pattern"]];
PIndexQ[_]:=False;
SetNumberOfArguments[PIndexQ,1];
Protect[PIndexQ];


(* Translate pattern to index. Note that Blank patterns ARE accepted, but not converted. PINDEX *)
PatternToIndex[-x_]:=-PatternToIndex[x];
PatternToIndex[x_Blank]:=Throw[Message[Validate::nouse,"PatternToIndex","a Blank pattern "<>ToString[x]]];
PatternToIndex[x_Pattern]:=PatternToIndex[First[x]];
PatternToIndex[x_PatternTest]:=PatternToIndex[First[x]];
PatternToIndex[{x_,basis_}]:={PatternToIndex[x],basis}; (* BASIS1 *);
PatternToIndex[imod_?IModQ[x_]]:=imod[PatternToIndex[x]]; (* BASIS2 & IMOD *);
PatternToIndex[x_Integer]:=x;
PatternToIndex[_Condition|_Optional|_Alternatives|_BlankSequence|_BlankNullSequence|_Repeated|_RepeatedNull]:=Throw[Message[Validate::nouse,"PatternToIndex","a forbidden pattern"]];
PatternToIndex[x_]:=x;


Off[RuleDelayed::rhs];
(* Abstract indices *)
PatternIndex[name_Symbol,AIndex,_:Null]:=PatternTest[Pattern[name,Blank[]],AIndexQ];
PatternIndex[name_Symbol,AIndex,xAct`xTensor`Up]:=PatternTest[Pattern[name,Blank[Symbol]],AIndexQ];
PatternIndex[name_Symbol,AIndex,xAct`xTensor`Down]:=-PatternTest[Pattern[name,Blank[Symbol]],AIndexQ];
PatternIndex[name_Symbol,AIndex,Null,vbundle_]:=With[{pmQ=VBundleIndexPMQ[vbundle]},PatternTest[Pattern[name,Blank[]],pmQ]];
PatternIndex[name_Symbol,AIndex,xAct`xTensor`Up,vbundle_]:=With[{mQ=VBundleIndexQ[vbundle]},PatternTest[Pattern[name,Blank[Symbol]],mQ]];
PatternIndex[name_Symbol,AIndex,xAct`xTensor`Down,vbundle_]:=With[{mQ=VBundleIndexQ[vbundle]},-PatternTest[Pattern[name,Blank[Symbol]],mQ]];
(* Directional indices *)
PatternIndex[name_Symbol,DIndex,_:Null]:=Pattern[name,Blank[Dir]];
PatternIndex[name_Symbol,DIndex,xAct`xTensor`Up]:=PatternTest[Pattern[name,Blank[Dir]],UpIndexQ];
PatternIndex[name_Symbol,DIndex,xAct`xTensor`Down]:=PatternTest[Pattern[name,Blank[Dir]],DownIndexQ];
PatternIndex[name_Symbol,DIndex,Null,vbundle_]:=PatternTest[Pattern[name,Blank[Dir]],(DIndexQ[#,vbundle])&];
PatternIndex[name_Symbol,DIndex,xAct`xTensor`Up,vbundle_]:=PatternTest[Pattern[name,Blank[Dir]],(UpIndexQ[#]&&DIndexQ[#,vbundle])&];
PatternIndex[name_Symbol,DIndex,xAct`xTensor`Down,vbundle_]:=PatternTest[Pattern[name,Blank[Dir]],(DownIndexQ[#]&&DIndexQ[#,vbundle])&];
(* Label indices *)
PatternIndex[name_Symbol,LIndex,_:Null]:=PatternTest[Pattern[name,Blank[]],LIndexQ];
PatternIndex[name_Symbol,LIndex,xAct`xTensor`Up]:=Pattern[name,Blank[LI]];
PatternIndex[name_Symbol,LIndex,xAct`xTensor`Down]:=-Pattern[name,Blank[LI]];
On[RuleDelayed::rhs];

SetNumberOfArguments[PatternIndex,{2,4}];
Protect[PatternIndex];


IndexOrderedQ[list_List]:=IndexOrderedQ[ValidateIndexList[list]];
IndexOrderedQ[list_IndexList]:=list===IndexSort[list];
SetNumberOfArguments[IndexOrderedQ,1];
Protect[IndexOrderedQ];


DisorderedPairQ[a_,b_]:=IndexSort[IndexList[a,b]]===IndexList[b,a];
SetNumberOfArguments[DisorderedPairQ,2];
Protect[DisorderedPairQ];


Sort0[-x_]:=Sort0[x];
(* Labels *)
Sort0[_LI]:=1;
(* Abstract indices *)
Sort0[_Symbol]:=2;
(* Basis indices *)
Sort0[{_Symbol|-_Symbol,_}]:=3; (* BASIS1 *);
Sort0[_?BasisQ[_Symbol]]:=3.1; (* BASIS2 *);
(* Modified indices. We need this definition after that for bases, though it is marked with a lower number *)
Sort0[_?IModQ[_]]:=2.5;
(* Component indices *)
Sort0[{_Integer,_}]:=4; (* BASIS1 *);
Sort0[_?BasisQ[_Integer]]:=4.1; (* BASIS2 *);
Sort0[_Integer]:=4.2;
(* Directions *)
Sort0[_Dir]:=5;
(* Patterns. PINDEX *)
Sort0[_Blank]:=6;
Sort0[_Pattern]:=7;
Sort0[_PatternTest]:=8;
(* Other? *)
Sort0[_]:=9;


Set[validpriority[#],Null]&/@{"up","down","free","dummy","positional","antipositional","lexicographic","antilexicographic"};
validpriority[p_]:=Throw[Message[SetIndexSortPriorities::invalid,p,"priority for index sorting"]]


(* Two variables which are blocked in IndexSort and hence can be used here *)
DummySorted;
LexicSorted;
(* Recall that False is sorted before True *)
SortPriority["up"]=Hold[downQ[#]&];
SortPriority["down"]=Hold[upQ[#]&];
SortPriority["free"]=Hold[MemberQ[DummySorted,Verbatim[#]]&];
SortPriority["dummy"]=Hold[FreeQ[DummySorted,Verbatim[#]]&];
SortPriority["lexicographic"]=Hold[Position[LexicSorted,UpIndex@PatternToIndex[#],{1}]&];
SortPriority["antilexicographic"]=Hold[Position[Reverse@LexicSorted,UpIndex@PatternToIndex[#],{1}]&];
SortPriority["positional"]=Hold[Position[$AbstractIndices,NoDollar@UpIndex@PatternToIndex@#,{1}]&];
SortPriority["antipositional"]=Hold[Position[Reverse@$AbstractIndices,NoDollar@UpIndex@PatternToIndex@#,{1}]&];


SetIndexSortPriorities[p1_String,p2_String,p3_String]:=Module[{f},
validpriority/@{p1,p2,p3};
Sort1=SortPriority[p1];
Sort2=SortPriority[p2];
Sort3=SortPriority[p3];
IndexSort::usage=StringTake[IndexSort::usage,144]<>"first: "<>p1<>"; second: "<>p2<>"; third: "<>p3<>".";
]
SetIndexSortPriorities[x_/;Head[x]=!=String,_,_]:=Message[SetIndexSortPriorities::invalid,x,"string"]
SetIndexSortPriorities[_,x_/;Head[x]=!=String,_]:=Message[SetIndexSortPriorities::invalid,x,"string"]
SetIndexSortPriorities[_,_,x_/;Head[x]=!=String]:=Message[SetIndexSortPriorities::invalid,x,"string"]
SetNumberOfArguments[SetIndexSortPriorities,3];
Protect[SetIndexSortPriorities];


IndexSort[IndexList[]]:=IndexList[];
IndexSort[IndexList[ind_]]:=IndexList[ind];
IndexSort[list_List]:=IndexSort[Apply[IndexList,list]];
IndexSort[list_IndexList]:=Block[{DummySorted=TakeEIndices@TakeEPairs[list],LexicSorted=Union[UpIndex/@PatternToIndex/@list]},
Last/@Sort@ReleaseHold[({Sort0[#],Sort1[#],Sort2[#],Sort3[#],#}&)/@list]
];
SetNumberOfArguments[IndexSort,1];
Protect[IndexSort];


SetIndexSortPriorities["free","lexicographic","up"];


IndexPosition[list_,index_]:=Position[list,Verbatim[index],{1}]


(* Names of indices. Sign is kept. Single-index of a Dim1 spaces should also be declared with name 0 *)
IndexName[-i_]:=-IndexName[i];
IndexName[i_Symbol]:=i;
(* Blocked indices and blanks get name 0 *)
IndexName[i:(_Dir|_LI|_Blank|_BlankSequence|_BlankNullSequence|_Integer)]:=0;
(* These cases should never happen. This is a safety feature *)
IndexName[i:(_String|_Rational|_Real|_Complex)]:=(Print["Found index ",i];0);
(* Basis indices and headed patterns. BASIS1 and BASIS2 & IMOD *)
IndexName[i_]:=IndexName[First[i]];


Validate::repeated="Found indices with the same name `1`.";
(* Driver *)
CheckRepeated[]:=IndexList[];
CheckRepeated[IndexList[]..]:=IndexList[];
CheckRepeated[___,IndexList[AnyIndices],___]:=IndexList[AnyIndices];
CheckRepeated[lists__IndexList]:=CheckRepeated1[Index[#,IndexName[#]]&/@Join[lists]];
(* Give error or return original list *)
CheckRepeated1[list:IndexList[___Index,Index[_,i:(_Symbol|-_Symbol)],___Index,Index[_,i_],___Index]]:=Throw@Message[Validate::repeated,ToString[i]];
CheckRepeated1[list:IndexList[__Index]]:=First/@list;


General::inhom="Found inhomogeneous `1`: `2`.";
checkLengthOne[{x_}]:=True;
checkLengthOne[x:{_,__}]:=Throw@Message[Validate::inhom,"indices",ToString[x]];
CheckHomogeneity[x___,IndexList[AnyIndices],y___]:=CheckHomogeneity[x,y];
CheckHomogeneity[]:=IndexList[];
CheckHomogeneity[IndexList[]..]:=IndexList[];
CheckHomogeneity[lists__IndexList]:=If[
checkLengthOne@Union@Map[TakeFrees,Map[DeleteCases[#,0]&,Map[IndexName,{lists},{2}]]],
DeleteDuplicates[Join[lists]]
];


SetAttributes[FindIndices,HoldFirst];
SetAttributes[FindIndices1,HoldFirst];


FindIndices[0]:=IndexList[AnyIndices];
FindIndices[_Symbol|_String|_Integer|_Rational|_Real|_Complex]:=IndexList[];


FindIndices[_[]]:=IndexList[];


FindIndices[expr_Times]:=FindIndices1[expr,Times];
(* These two should not be needed, but they can correct evaluation leaks in canonicalization *)
FindIndices[expr:VerbatimProduct[_][___]]:=FindIndices1[expr,Times];
FindIndices[expr:CommutingObjects[_][___]]:=FindIndices1[expr,Times];


FindIndices[expr_Plus]:=FindIndices1[expr,Plus];
FindIndices[expr_List]:=FindIndices1[expr,Plus];
FindIndices[expr_Equal]:=FindIndices1[expr,Plus];
FindIndices[Rule[lhs_,rhs_]]:=FindIndices1[Rule[lhs,Evaluate[rhs]],Plus];
FindIndices[RuleDelayed[lhs_,rhs_]]:=FindIndices1[RuleDelayed[lhs,rhs],Plus];
FindIndices[HoldPattern[Set[lhs_,rhs_]]]:=FindIndices1[Set[lhs,rhs],Plus];
FindIndices[HoldPattern[SetDelayed[lhs_,rhs_]]]:=FindIndices1[SetDelayed[lhs,rhs],Plus];


FindIndices[expr_Module]:=FindIndices1[expr,2];
FindIndices[expr_Monomial]:=FindIndices1[expr,1];
FindIndices[expr_Condition]:=FindIndices1[expr,1];


FindIndices[HoldPattern[SeriesData[_,_,{RepeatedNull[0]},__]]]:=IndexList[AnyIndices];
FindIndices[expr_SeriesData]:=FindIndices1[expr,3];


FindIndices1[expr:_[elements__],Times]:=CheckRepeated@@(FindIndices/@Hold[elements]);
FindIndices1[expr:_[elements__],Plus]:=CheckHomogeneity@@(FindIndices/@Hold[elements]);
FindIndices1[expr:_[elements__],None]:=Join@@(FindIndices/@Hold[elements]);
FindIndices1[expr_,n_Integer]:=FindIndices@@Extract[Unevaluated[expr],n,xHold];


FindIndices[Dagger[expr_]]:=DaggerIndex/@FindIndices[expr];


CovDIndices[_?CovDQ[inds__]]:=Sequence[inds];
CovDIndices[x_]:=Throw@Message[FindIndices::invalid,x,"derivative"];


(* Basic objects *)
FindIndices[_?xTensorQ[inds___]]:=CheckRepeated[IndexList[inds]];
FindIndices[_?CovDQ[inds__][expr_]]:=CheckRepeated[FindIndices[expr],IndexList[inds]];
FindIndices[CovD[expr_,ders__]]:=CheckRepeated[FindIndices[expr],CovDIndices/@IndexList[ders]];
FindIndices[ParamD[__][expr_]]:=FindIndices[expr];
FindIndices[OverDot[expr_]]:=FindIndices[expr];
FindIndices[LieD[_][expr_]]:=FindIndices[expr];
FindIndices[_?InertHeadQ[expr_,z___]]:=DeleteDuplicates[Join@@Join[{FindIndices[expr]},Cases[{z},_IndexList]]];
FindIndices[expr:_?ProductQ[___]]:=FindIndices1[expr,Times];
(* Scalar shields indices! *)
FindIndices[_Scalar]:=IndexList[];
FindIndices[_?ScalarFunctionQ[___]]:=IndexList[];
FindIndices[Derivative[__Integer][_?ScalarFunctionQ][__]]:=IndexList[];


FindIndices[pattern_PatternTest[inds___]]:=CheckRepeated[IndexList[inds]]/;Last[pattern]===xTensorQ;
FindIndices[pattern_PatternTest[inds__][expr_]]:=CheckRepeated[FindIndices[expr],IndexList[inds]]/;Last[pattern]===CovDQ;


FindIndices[expr:(_HoldPattern|_ReplaceDummies)]:=FindIndices1[expr,1];


(* Again, this should not be needed, but it may fix evaluation leaks *)
FindIndices[Object[_,_,{allinds_,__}]]:=allinds;


$FindIndicesAcceptedHeads={Hold,HoldForm};
FindIndices[expr:head_[__]]:=If[MemberQ[$FindIndicesAcceptedHeads,head],FindIndices1[expr,None],IndexList[]];


SetNumberOfArguments[FindIndices,1];
Protect[FindIndices];


SetAttributes[FindFreeIndices,HoldFirst];
FindFreeIndices[expr_,selector_]:=SelectIndices[FindFreeIndices[expr],selector];
FindFreeIndices[expr_]:=TakeFrees@FindIndices[expr];
SetNumberOfArguments[FindFreeIndices,{1,2}];
Protect[FindFreeIndices];


SetAttributes[FindDummyIndices,HoldFirst];
FindDummyIndices[expr_,selector_]:=SelectIndices[FindDummyIndices[expr],selector];
FindDummyIndices[expr_]:=TakeUp@TakeOrderedEPairs@FindIndices[expr];
SetNumberOfArguments[FindDummyIndices,{1,2}];
Protect[FindDummyIndices];


SetAttributes[FindBlockedIndices,HoldFirst];
FindBlockedIndices[expr_,selector_]:=SelectIndices[FindBlockedIndices[expr],selector];
FindBlockedIndices[expr_]:=TakeBlocked@FindIndices[expr];
SetNumberOfArguments[FindBlockedIndices,{1,2}];
Protect[FindBlockedIndices];


SetAttributes[FindFreeAndDummyIndices,HoldFirst];
FindFreeAndDummyIndices[expr_]:={TakeFrees[#],TakeUp@TakeOrderedEPairs[#]}&@FindIndices[expr];


SetAttributes[IsIndexOf,HoldFirst];
(* In general we are not trying to contract anything: use delta *)
IsIndexOf[expr_,index_]:=IsIndexOf[expr,index,delta];
(* Compound expressions. Or in Times (any index), And in Plus (only free indices ensured) *)
IsIndexOf[expr:(_Times|CommutingObjects[_][___]|VerbatimProduct[_][___]|_?ProductQ[___]),index_,f_]:=Or@@(IsIndexOf[#,index,f]&/@Hold@@Unevaluated[expr]);
IsIndexOf[{},index_,f_]:=False;
IsIndexOf[expr:(_Plus|_List),index_,f_]:=And@@(IsIndexOf[#,index,f]&/@Hold@@Unevaluated[expr]);
IsIndexOf[HoldPattern[SeriesData[_,_,coeffs_,__]],index_,f_]:=IsIndexOf[coeffs,index,f];
(* Dagger: temptative definition *)
IsIndexOf[Dagger[expr_],index_,f_]:=IsIndexOf[expr,DaggerIndex[index],f];
(* Tensors *)
IsIndexOf[_?xTensorQ[inds___],index_,f_]:=MemberQ[IndexList[inds],index];
(* Inert heads *)
IsIndexOf[list_IndexList,index_,f_]:=MemberQ[list,index];
IsIndexOf[list:{___IndexList},index_,f_]:=Or@@(IsIndexOf[#,index,f]&/@list);
IsIndexOf[head_?InertHeadQ[expr_,z___],index_,f_]:=If[ContractThroughQ[head,f],IsIndexOf[expr,index,f]||With[{cases=Cases[{z},_IndexList]},IsIndexOf[cases,index,f]],False];
(* Derivatives: no check of compatibility of derivative and tensor f *)
IsIndexOf[_?CovDQ[inds__][expr_],index_,f_]:=MemberQ[IndexList[inds],index]||IsIndexOf[expr,index,f];
IsIndexOf[CovD[expr_,ders__],index_,f_]:=MemberQ[CovDIndices/@IndexList[ders],index]||IsIndexOf[expr,index,f];
IsIndexOf[ParamD[__][expr_],index_,f_]:=IsIndexOf[expr,index,f];
IsIndexOf[OverDot[expr_],index_,f_]:=IsIndexOf[expr,index,f];
IsIndexOf[LieD[_][expr_],index_,f_]:=IsIndexOf[expr,index,f];
(* Scalar structures: shield *)
IsIndexOf[_?ScalarFunctionQ[args__],index_,f_]:=False;
IsIndexOf[Derivative[__Integer][_?ScalarFunctionQ][args__],index_,f_]:=False;
IsIndexOf[_Scalar,index_,f_]:=False;
(* Atoms *)
IsIndexOf[_Symbol,index_,f_]:=False;
IsIndexOf[_Integer|_Rational|_Real|_Complex,index_,f_]:=False;
IsIndexOf[_String,index_,f_]:=False;
(* Other *)
IsIndexOf[expr_,index_,f_]:=Throw[Message[IsIndexOf::nouse,IsIndexOf,expr];ERROR[expr]];
SetNumberOfArguments[IsIndexOf,{2,3}];
Protect[IsIndexOf];


(* We need several stupid special cases to avoid messages from the Information commands from covds. This is ugly *)
UltraindexOf[_Pattern _.]:=None;
UltraindexOf[x_Pattern+y_Pattern]:=None;
UltraindexOf[x_Symbol]:=None;
UltraindexOf[x_Slot]:=None;
UltraindexOf[0]:=None;
(* Main function. Recall that it returns the free ultraindex *)
UltraindexOf[Dir[expr_]]:=UltraindexOf[expr];
UltraindexOf[expr_]:=checkultraindex[FindFreeIndices[expr],expr];
(* Internal function. It returns the ultraindex or an error *)
checkultraindex[IndexList[i_],_]:=i;
checkultraindex[_,expr_]:=Throw@Message[Validate::error,"Invalid or no ultraindex."];


(* Trivial case *)
SelectIndices[expr_,IndexList[],selectors_]:=IndexList[];
(* Threading as Or *)
SelectIndices[expr_,list_IndexList,selectors_List]:=Apply[Join,Map[SelectIndices[expr,list,#]&,selectors]];
(* Not *)
SelectIndices[expr_,list_IndexList,Not[selector_]]:=Complement[list,SelectIndices[expr,list,selector]];
(* Select according to states *)
SelectIndices[expr_,list_IndexList,Free]:=TakeFrees[list];
SelectIndices[expr_,list_IndexList,Dummy]:=TakeEPairs[list];
SelectIndices[expr_,list_IndexList,Blocked]:=Select[list,BlockedQ];
(* Select according to character *)
SelectIndices[expr_,list_IndexList,xAct`xTensor`Up]:=Select[list,UpIndexQ];
SelectIndices[expr_,list_IndexList,xAct`xTensor`Down]:=Select[list,DownIndexQ];
(* Select according to type *)
SelectIndices[expr_,list_IndexList,AIndex]:=Select[list,AIndexQ];
SelectIndices[expr_,list_IndexList,BIndex]:=Select[list,BIndexQ];
SelectIndices[expr_,list_IndexList,CIndex]:=Select[list,CIndexQ];
SelectIndices[expr_,list_IndexList,DIndex]:=Select[list,DIndexQ];
SelectIndices[expr_,list_IndexList,LIndex]:=Select[list,LIndexQ];
(* Select according to associations. BASIS1 and BASIS2 *)
SelectIndices[expr_,list_IndexList,vbundle_?VBundleQ]:=Select[list,IndexOnQ[#,vbundle]&];
SelectIndices[expr_,list_IndexList,basis_Symbol?BasisQ]:=IndexList@@Cases[list,{_,basis|-basis}|basis[_]|-basis[_]];
SelectIndices[expr_,list_IndexList,Basis[basis_]]:=Intersection[list,Flatten[IndexList@@Cases[expr,Basis[index1___,index2:({_,basis|-basis}|basis[_]|-basis[_]),index3___]->IndexList[index1,index2,index3],{0,Infinity}],Infinity,IndexList]];
SelectIndices[expr_,list_IndexList,tensor_?xTensorQ]:=Intersection[list,Flatten[IndexList@@Cases[expr,tensor[inds__]->IndexList[inds],{0,Infinity}],Infinity,IndexList]];
SelectIndices[expr_,list_IndexList,covd_?CovDQ]:=Intersection[list,Join[Flatten[IndexList@@Cases[expr,HoldPattern[covd[inds__][_]]->IndexList[inds],{0,Infinity}],Infinity,IndexList],Flatten[IndexList@@Cases[expr,HoldPattern[CovD[_,ders__]]:>(CovDIndices/@IndexList[ders]),{0,Infinity}],Infinity,IndexList]]];
(* Other cases: error *)
SelectIndices[expr_,list_IndexList,x_]:=Throw@Message[IndicesOf::invalid,x,"index selector"];


IndicesOf[][expr_]:=FindIndices[expr];
IndicesOf[selectors___,selector1_][expr_]:=SelectIndices[expr,IndicesOf[selectors][expr],selector1];
Protect[IndicesOf];


SetAttributes[{ToIndexExpr,IndexExpr},HoldFirst];


(* One argument: default function: Identity *)
ToIndexExpr[expr_]:=ToIndexExpr[expr,Identity];
(* Tensors *)
ToIndexExpr[Bracket[v1_,v2_][inds__],function_]:=With[{iv1=ToIndexExpr[v1,function],iv2=ToIndexExpr[v2,function]},function@IndexExpr[Bracket[iv1,iv2][inds],IndexList[inds],IndexList[]]];
ToIndexExpr[expr:_?xTensorQ[inds___],function_]:=function@IndexExpr[expr,TakeFrees[IndexList[inds]],TakeEPairs[IndexList[inds]]];
(* Tensor products, including Times. Note: iexpr is a List of IndexExpr's *)
ToIndexExpr[HoldPattern[Times[factors__]],function_]:=With[{iexpr=ToIndexExpr[#,function]&/@Unevaluated[{factors}]},With[{all=Apply[Join,freesof/@iexpr]},With[{dummies=TakeEPairs[all]},
function@ReplacePart[IndexExpr[iexpr,Complement[all,dummies],dummies],Times,{1,0}] ]]];
ToIndexExpr[HoldPattern[prod_?ProductQ[factors__]],function_]:=With[{iexpr=ToIndexExpr[#,function]&/@Unevaluated[{factors}]},With[{all=Apply[Join,freesof/@iexpr]},With[{dummies=TakeEPairs[all]},
function@ReplacePart[IndexExpr[iexpr,Complement[all,dummies],dummies],prod,{1,0}] ]]];
(* Covariant derivatives *)
ToIndexExpr[expr:covd_?CovDQ[inds__][expr1_],function_]:=With[{iexpr1=ToIndexExpr[expr1,function]},
With[{all=Join[freesof[iexpr1],IndexList[inds]]},
With[{dummies=TakeEPairs[all]},
function@IndexExpr[covd[inds][iexpr1],Complement[all,dummies],dummies] ]]];
ToIndexExpr[expr:CovD[expr1_,ders__],function_]:=With[{iexpr1=ToIndexExpr[expr1,function]},
With[{all=Join[freesof[iexpr1],CovDIndices/@IndexList[ders]]},
With[{dummies=TakeEPairs[all]},
function@IndexExpr[CovD[iexpr1,ders],Complement[all,dummies],dummies] ]]];
(* Thread over sums. No nesting of IndexExpr *)
ToIndexExpr[expr_Plus,function_]:=ToIndexExpr[#,function]&/@Unevaluated[expr];
(* Series. Keep structure *)
ToIndexExpr[HoldPattern[SeriesData[var_,orig_,coeffs_,rest__]],function_]:=SeriesData[ToIndexExpr[var,function],ToIndexExpr[orig,function],ToIndexExpr[#,function]&/@Unevaluated[coeffs],rest];
(* Other cases: no new dummies *)
ToIndexExpr[LieD[v_][expr_],function_]:=With[{iv=ToIndexExpr[v,function],iexpr=ToIndexExpr[expr,function]},function@IndexExpr[LieD[iv][iexpr],freesof[iexpr],IndexList[]]];
ToIndexExpr[OverDot[expr_],function_]:=With[{iexpr=ToIndexExpr[expr,function]},function@IndexExpr[OverDot[iexpr],freesof[iexpr],IndexList[]]];
ToIndexExpr[ParamD[ps__][expr_],function_]:=With[{iexpr=ToIndexExpr[expr,function]},function@IndexExpr[ParamD[ps][iexpr],freesof[iexpr],IndexList[]]];
ToIndexExpr[symbol_Symbol,function_]:=function@IndexExpr[symbol,IndexList[],IndexList[]];
ToIndexExpr[constant_?ConstantQ,function_]:=function@IndexExpr[constant,IndexList[],IndexList[]];
ToIndexExpr[ih_?InertHeadQ[expr_,z___],function_]:=With[{iexpr=ToIndexExpr[expr,function]},function@IndexExpr[ih[iexpr,z],freesof[iexpr],IndexList[]]];
ToIndexExpr[sf_Symbol?ScalarFunctionQ[expr_],function_]:=With[{iexpr=ToIndexExpr[expr,function]},function@IndexExpr[sf[iexpr],IndexList[],IndexList[]]];
ToIndexExpr[sf_Symbol?ScalarFunctionQ[expr1_,expr2_],function_]:=With[{iexpr1=ToIndexExpr[expr1,function],iexpr2=ToIndexExpr[expr2,function]},function@IndexExpr[sf[iexpr1,iexpr2],IndexList[],IndexList[]]];
(* Other heads *)
ToIndexExpr[head_[expr_],function_]:=If[MemberQ[$FindIndicesAcceptedHeads,head],ReplacePart[head[expr],ToIndexExpr[expr,function],{1}],head[expr]];
(* General definition: do nothing *)
ToIndexExpr[expr_,function_]:=function@IndexExpr[expr,IndexList[],IndexList[]];


SetNumberOfArguments[ToIndexExpr,{1,2}];
SetNumberOfArguments[IndexExpr,3];
Protect[ToIndexExpr,IndexExpr];


freesof[expr_Plus]:=freesof[First[expr]];
freesof[expr_SeriesData]:=freesof[Normal[expr]];
freesof[IndexExpr[_,frees_,_]]:=frees;
freesof[expr_]:=IndexList[];


FromIndexExpr[iexpr_,wrap_:Identity]:=Identity@@(xHold[wrap[iexpr]]//.IndexExpr[expr_,_,_]:>expr);


replacepattern[i_,rules_,sign_]:=ReplacePart[i,sign ReplaceOneIndex[sign First[i],rules],1];


Off[Rule::rhs]


(* Symbols *)
ReplaceOneIndex[i:(_Symbol|-_Symbol),rules_]:=Replace[i,rules];
(* Directions *)
ReplaceOneIndex[i_Dir,rules_]:=Replace[i,rules];
(* Labels *)
ReplaceOneIndex[i:(_LI|-_LI),rules_]:=Replace[i,rules];
(* Components *)
ReplaceOneIndex[i:{_Integer,_},rules_]:=Replace[i,rules]; (* BASIS1 *);
ReplaceOneIndex[i:(_. _Symbol?BasisQ[_Integer]),rules_]:=Replace[i,rules];(* BASIS2 *);
(* Basis indices *)
ReplaceOneIndex[i:{_,_},rules_]:=ReplaceBasis1Index[i,Replace[i,rules],rules]; (* BASIS1 *);
ReplaceOneIndex[i:(_. _?IModQ[_]),rules_]:=ReplaceBasis2Index[i,Replace[i,rules],rules]; (* BASIS2 & IMOD *);
(* Patterns. PINDEX *)
ReplaceOneIndex[i_Blank,rules_]:=(Message[ReplaceIndex::nopat,i,$WarningFrom];i);
ReplaceOneIndex[i_Pattern,rules_]:=replacepattern[i,rules,1];
ReplaceOneIndex[i_PatternTest,rules_]:=replacepattern[i,rules,1];
ReplaceOneIndex[-i_Pattern,rules_]:=-replacepattern[i,rules,-1];
ReplaceOneIndex[-i_PatternTest,rules_]:=-replacepattern[i,rules,-1];
(* Slots *)
ReplaceOneIndex[i:slot[_],rules_]:=Replace[i,rules];
(* Other cases: throw error *)
ReplaceOneIndex[i_,rules_]:=Throw@Message[ReplaceIndex::invalid,i,"index to be replaced"];


(* BASIS1 *)
ReplaceBasis1Index[{old_,basis_},{old_,basis_},rules_]:=CheckBasis1Index[{ReplaceOneIndex[old,rules],basis}];
ReplaceBasis1Index[old_,new_,rules_]:=new;
CheckBasis1Index[{-a_,basis_Symbol}]:={-a,-basis};
CheckBasis1Index[{a_Integer,basis_}]:={a,basis};
CheckBasis1Index[{a_?UpIndexQ,-basis_Symbol}]:={a,basis};
CheckBasis1Index[index_]:=index;


(* BASIS2 *)
ReplaceBasis2Index[-imod_[old_],-imod_[old_],rules_]:=CheckBasis2Index[imod[ReplaceOneIndex[-old,rules]],-1];
ReplaceBasis2Index[imod_[old_],imod_[old_],rules_]:=CheckBasis2Index[imod[ReplaceOneIndex[old,rules]],1];
ReplaceBasis2Index[old_,new_,rules_]:=new;
CheckBasis2Index[basis_[a_Integer],-1]:=-basis[a];
CheckBasis2Index[imod_[-a_],_]:=-imod[a];
CheckBasis2Index[index_,_]:=index;


ReplaceIndexList[list_IndexList,rules_]:=ReplaceOneIndex[#,rules]&/@list


SetAttributes[HoldReplaceIndex,HoldFirst]


HoldReplaceIndex[expr_,{}]:=xHold[expr]


(* Action on atoms *)
HoldReplaceIndex[x:(_Integer|_Rational|_Real|_Complex|_String|_Symbol),rules_]:=xHold[x];


(* Action on derivatives *)
HoldReplaceIndex[covd_?CovDQ[inds__][expr_],rules_]:=With[{newcovd=Apply[covd,ReplaceIndexList[IndexList[inds],rules]]},ReplacePart[xHold[newcovd[expr]],HoldReplaceIndex[expr,rules],{1,1}]];
HoldReplaceIndex[CovD[expr_,ders__],rules_]:=With[{newders=Sequence@@Map[ReplaceOneIndex[#,rules]&,{ders},{2}]},ReplacePart[xHold[CovD[expr,newders]],HoldReplaceIndex[expr,rules],{1,1}]];
HoldReplaceIndex[LieD[v_][expr_],rules_]:=ReplacePart[xHold[LieD[v][expr]],HoldReplaceIndex[expr,rules],{{1,1}}];
HoldReplaceIndex[ParamD[ps__][expr_],rules_]:=ReplacePart[xHold[ParamD[ps][expr]],HoldReplaceIndex[expr,rules],{{1,1}}];
HoldReplaceIndex[OverDot[expr_],rules_]:=ReplacePart[xHold[OverDot[expr]],HoldReplaceIndex[expr,rules],{{1,1}}];


(* Action on elementary objects. Nonindexed tensors considered below *)
HoldReplaceIndex[head_?xTensorQ[inds__],rules_]:=ReplacePart[xHold[head[inds]],ReplaceIndexList[IndexList[inds],rules],Thread[{1,Range@Length@{inds}}],Thread[{Range@Length@{inds}}]]


(* Change indices in additional arguments of an inert head *)
changez[expr:xHold[_[_]],rules_]:=expr;
changez[xHold[ih_[expr_,z__]],rules_]:=changez2[xHold[ih[expr,z]],Replace[{z},list_IndexList:>ReplaceIndexList[list,rules],{1}]];
changez2[xHold[ih_[expr_,z__]],{z2__}]:=xHold[ih[expr,z2]];


(* Action on inert heads *)
HoldReplaceIndex[ih_?InertHeadQ[expr_,z___],rules_]:=changez[ReplacePart[xHold[ih[expr,z]],HoldReplaceIndex[expr,rules],{{1,1}}],rules];


(* Shield scalars *)
HoldReplaceIndex[expr:_?ScalarFunctionQ[___],rules_]:=xHold[expr];
HoldReplaceIndex[expr:Derivative[__Integer][_?ScalarFunctionQ][__],rules_]:=xHold[expr];
HoldReplaceIndex[expr_Scalar,rules_]:=xHold[expr];


(* Special cases *)
HoldReplaceIndex[HoldPattern[Module[list_List,expr_]],rules_]:=Apply[xHold[Module[##]]&,{List@@ReplaceIndexList[IndexList@@list,rules],HoldReplaceIndex[expr,rules]}]


HoldReplaceIndex[head_[],rules_]:=head[]
HoldReplaceIndex[head_[elements__],rules_]:=ReplacePart[xHold[head[elements]],Map[Function[{x},HoldReplaceIndex[x,rules],{HoldAll}],Unevaluated@{elements}],Thread[{1,Range@Length@Unevaluated@{elements}}],Thread[{Range@Length@Unevaluated@{elements}}]]


SetAttributes[ReplaceIndex,HoldFirst]


ReplaceIndex[expr_,rules:{(_List)..},f_:Identity]:=ReplaceIndex[expr,#,f]&/@rules;


ReplaceIndex[expr_,rules_,f_:Identity]:=f@@(Hold[Evaluate[HoldReplaceIndex[expr,rules]]]//.xHold[x_]->x)


(* Other *)
SetNumberOfArguments[ReplaceIndex,{2,3}];
Protect[ReplaceIndex];


SetAttributes[FastReplaceDummies,HoldFirst];
FastReplaceDummies[expr_]:=Module@@{Union[IndexName/@FindDummyIndices[expr]],Unevaluated[expr]}


ReplaceDummies[expr_?NonIndexedScalarQ,_]:=expr;
ReplaceDummies[expr_,newdummies_IndexList:IndexList[]]:=ReplaceDummies2[expr,dummypool[newdummies,FindFreeAndDummyIndices[expr]]];


$ComputeNewDummies=True;
dummypool[newdummies_IndexList,{frees_IndexList,dummies_IndexList}]:=Complement[Join[IndexName/@newdummies,If[$ComputeNewDummies,DummyIn/@VBundleOfIndex/@dummies,IndexList[]]],IndexName/@frees];


SameDummies[expr:(_Plus|_SeriesData|_Equal|_List)]:=ReplaceDummies2[expr,Union[IndexName/@FindDummyIndices[expr]]];
SameDummies[expr_]:=expr;


ReplaceDummies2[expr:(_Plus|_Equal|_List),newdummies_IndexList]:=ReplaceDummies2[#,newdummies]&/@expr;
ReplaceDummies2[HoldPattern[SeriesData[var_,orig_,coeffs_,rest__]],newdummies_IndexList]:=SeriesData[ReplaceDummies2[var,newdummies],ReplaceDummies2[orig,newdummies],ReplaceDummies2[#,newdummies]&/@coeffs,rest];
ReplaceDummies2[expr_,newdummies_IndexList]:=ReplaceIndex[expr,SameVBundleRules[Sort[IndexName/@FindDummyIndices[expr]],newdummies]]


md[i_]:={VBundleOfIndex[i],i};
SameVBundleRules[IndexList[],_]:={};
SameVBundleRules[indices_IndexList,newindices_IndexList]:=SameVBundleRules[indices,newindices,$VBundles];
SameVBundleRules[indices_,newindices_,{vbundle_}]:=OneVBundleRules[indices,newindices];
SameVBundleRules[indices_,newindices_,vbundles_List]:=SameVBundleRules2[md/@indices,md/@newindices,vbundles];
SameVBundleRules2[indices_,newindices_,vbundles_]:=Apply[Join,SameVBundleRules[Last/@IndexList@@Cases[indices,{#,_}],Last/@IndexList@@Cases[newindices,{#,_}],{#}]&/@vbundles];
OneVBundleRules[indices_,newindices_]:=With[{tmp=newindices[[Range[Length[indices]]]]},Inner[Rule,indices,tmp,List]~Join~Inner[Rule,Minus/@indices,Minus/@tmp,List]];


SetNumberOfArguments[ReplaceDummies,{1,2}];
SetNumberOfArguments[SameDummies,1];
Protect[ReplaceDummies,SameDummies];


DuplicateRule[HoldPattern[Rule[a_,b_]]]:=Sequence[Rule[a,b],Rule[ChangeIndex[a],ChangeIndex[b]]];
DuplicateRule[list_List]:=DuplicateRule/@list;
DuplicateRule[list_IndexList]:=DuplicateRule/@list;


ArrangeSplitRules[rule:(Rule|RuleDelayed)[_?GIndexQ,_]]:=IndexList[rule];
ArrangeSplitRules[rules:(Rule|RuleDelayed)[_IndexList,_]]:=Thread[MapAt[Reverse,rules,1],IndexList,1];
ArrangeSplitRules[list_IndexList]:=Flatten[ArrangeSplitRules/@Reverse[list],1,IndexList];
ArrangeSplitRules[list_List]:=ArrangeSplitRules[IndexList@@list];


InterpretSplitRules[rule:Rule[_,IndexList[]]]:=Throw@Message[SplitIndex::invalid,rule,"replacement rule"];
InterpretSplitRules[rule:Rule[_,_IndexList]]:=Apply[List,List/@Thread[rule,IndexList]];
InterpretSplitRules[List[rules___]]:=InterpretSplitRules/@IndexList[rules];


changefirst[rule_[a_,b_],Rule[a_,c_]]:=rule[c,b];
SplitIndex[expr_,rules_]:=Module[{splitrules,firsts,dollars,rules1,rules2},
splitrules=ArrangeSplitRules[rules];
firsts=First/@splitrules;
dollars=DummyAs/@firsts;
rules1=Inner[Rule,firsts,dollars,List];
rules2=InterpretSplitRules[Inner[changefirst,splitrules,IndexList@@rules1,List]];
Fold[ReplaceAll,ReplaceIndex[expr,rules1],rules2]
];
SetNumberOfArguments[SplitIndex,2];
Protect[SplitIndex];


TraceDummy[expr_List,args__]:=TraceDummy[#,args]&/@expr;
TraceDummy[expr_,rules_,function_:Identity]:=FromIndexExpr[ToIndexExpr[expr,TraceDummyExpr[#,rules,function]&]];
SetNumberOfArguments[TraceDummy,{2,3}];
Protect[TraceDummy];


$TraceDummyVerbose=False;
(* Always a list of rules *)
TraceDummyExpr[expr_,rule:(_Rule|_RuleDelayed),function_]:=TraceDummyExpr[expr,{rule},function];
(* No dummies or no rules: do nothing *)
TraceDummyExpr[expr:IndexExpr[_,_,IndexList[]],_,_]:=expr;
TraceDummyExpr[expr_IndexExpr,{},_]:=expr;
(* General case: dummies and rules *)
TraceDummyExpr[expr:IndexExpr[expr1_,frees_,dummies_],rules:{((Rule|RuleDelayed)[_,_])..},function_]:=With[{tracerules=ChooseRules[dummies,rules]},
With[{traceinds=First/@tracerules},
If[$TraceDummyVerbose,Print["Tracing indices ",traceinds," in ",FromIndexExpr@HoldForm[expr1]]];
ReplacePart[IndexExpr[expr1,frees,Complement[dummies,traceinds,ChangeIndex/@traceinds]],TraceIndex[expr1,tracerules,function],{1},{1}]]];


ChooseRules[dummies_,rules_]:=Block[{HEAD},With[{updummies=Union[UpIndex/@HEAD@@dummies]},DeleteCases[Inner[Rule,updummies,Replace[updummies,rules,1],IndexList],HoldPattern[Rule[x_,x_]]]]];


SetAttributes[TraceIndex,HoldFirst];
TraceIndex[expr_,IndexList[],function_:Identity]:=Hold[function[expr]];
TraceIndex[expr_,rules_,function_:Identity]:=Module[{splitrules,firsts,dollars,rules1,rules2,terms},
splitrules=ArrangeSplitRules[rules];
firsts=First/@splitrules;
dollars=DummyAs/@firsts;
rules1=Inner[Rule,firsts,dollars,List];
rules2=InterpretSplitRules[Inner[changefirst,splitrules,IndexList@@rules1,List]];
rules1=DuplicateRule/@rules1;
rules2=DuplicateRule/@rules2;
terms=Flatten@Fold[ReplaceAll,ReplaceIndex[expr,rules1,Hold],rules2];
(* function is mapped inside a Hold expression, and hence not yet evaluated. Fixed after Leo found a problem *)
function/@ReplacePart[Thread[terms,Hold],Plus,{1,0}]
];


(***************************** 6.Manifolds ****************************)


If[$ReadingVerbose,Print["Reading section 6: Manifolds."],Null,Null]


checkDefManifold[manifold_,supermanifold_,tbundle_,dim_,indices_]:=(
(* 1.- Validate manifold *)
ValidateSymbol[manifold];
ValidateSymbolInSession[manifold];
(* 2.- Validate supermanifold *)
If[supermanifold=!=Null,
If[!ManifoldQ[supermanifold],Throw@Message[DefManifold::unknown,"supermanifold",supermanifold]];
If[NumberQ[dim]&&NumberQ[DimOfManifold[supermanifold]]&&DimOfManifold[supermanifold]<=dim,
Throw@Message[DefManifold::error,"Dimension of submanifold must be smaller than that of its supermanifold."]]
];
(* 3.- Validate tbundle *)
Switch[tbundle,
Automatic,Null,
manifold,Throw@Message[DefManifold::error,"A manifold and its tangent bundle cannot share name."],
_,
ValidateSymbol[tbundle];
ValidateSymbolInSession[tbundle]
];
(* 4.- Validate indices *)
If[dim=!=0,
If[Head[indices]=!=List,Throw@Message[DefManifold::invalid,indices,"list of indices"]];
If[indices==={}&&dim=!=0,Throw@Message[DefManifold::empty,3,"indices"]];
(*Dim1: If[dim===1&&Length[indices]=!=1,Throw@Message[DefManifold::invalid,indices,"list of indices for a 1-dim manifold"]];*);
ValidateIndex[#,False]&/@indices
];
)


GiveSymbol[Tangent,manifold_Symbol?ManifoldQ]:=SymbolJoin[Tangent,manifold];
Tangent[manifold_Symbol?ManifoldQ]:=GiveSymbol[Tangent,manifold];
PrintAsCharacter[Tangent]="\[DoubleStruckCapitalT]";


VBundleQ[Tangent[manifold_?ManifoldQ]]^:=True;


MakeBoxes[Tangent[manifold_?HeldManifoldQ],StandardForm]:=interpretbox[Tangent[manifold],RowBox[{"T",MakeBoxes[manifold,StandardForm]}]];


TangentBundleOfManifold=Tangent;


ManifoldQ[manifold_CartesianProduct]^:=And@@(ManifoldQ/@List@@manifold);


DimOfManifold[manifold_CartesianProduct]^:=Plus@@(DimOfManifold/@List@@manifold);
Tangent[manifold_CartesianProduct]^:=CirclePlus@@(Tangent/@List@@manifold);
SplittingsOfManifold[manifold_CartesianProduct]^:={manifold};


Options[DefManifold]={Tangent->Automatic,Master->Null,PrintAs->Identity,ProtectNewSymbol:>$ProtectNewSymbols,DefInfo->{"manifold",""}};
DefManifold[list_List,rest___]:=Scan[DefManifold[#,rest]&,list];
(* Backwards compatibility (three definitions) *)
DefManifold[manifold_,dim_,indices_List,ultraindex_Symbol,options___?OptionQ]:=
(Print["** DefManifold (legacy): Ultraindex "<>ToString[ultraindex]<>" converted into normal index."];DefManifold[manifold,dim,Append[indices,ultraindex],options]);
DefManifold[manifold_,{integers___Integer?Positive},indices_List,options___?OptionQ]:=
(Print["** DefMainfold (legacy): CNumbers "<>ToString[{integers}]<>" only used to compute dimension."];DefManifold[manifold,Length[{integers}],indices,options]);
DefManifold[supermanifold_,submanifolds:(_List|_CartesianProduct),indices_List,options___?OptionQ]:=
(DefManifold[supermanifold,Apply[Plus,DimOfManifold/@submanifolds],indices,options];
SplitManifold[supermanifold,submanifolds]);
(* Single manifold *)
DefManifold[manifold_,dim_,indices_List,options:OptionsPattern[]]:=
Catch@Module[{tbundle,master,pa,pns,info},
{tbundle,master,pa,pns,info}=OptionValue[{Tangent,Master,PrintAs,ProtectNewSymbol,DefInfo}];
checkDefManifold[manifold,Null,tbundle,dim,indices];
(* Register manifold structure *)
MakeDefInfo[DefManifold,manifold,info];
MakexTensions[DefManifold,"Beginning",manifold,dim,indices,options];
AppendToUnevaluated[$Manifolds,manifold];
ManifoldQ[manifold]^=True;
DefInfo[manifold]^=info;
SetPrintAs[manifold,PrintAsString[manifold,pa]];
SymbolRelations[manifold,master,{}];
DimOfManifold[manifold]^=dim;
SplittingsOfManifold[manifold]^={};
(* Define (co)tangent bundle. DefVBundle assumes that the manifold has been already registered *)
If[dim===0,
If[$DefInfoQ,Print["** DefManifold:  Tangent bundle does not exist for a 0-dim manifold"]],
If[tbundle===Automatic,
tbundle=GiveSymbol[Tangent,manifold],
Tangent[manifold]^=tbundle
];
DefVBundle[tbundle,manifold,dim,indices,Master->manifold,PrintAs:>GiveOutputString[Tangent,manifold],options];
];
MakexTensions[DefManifold,"End",manifold,dim,indices,options];
If[pns,Protect[manifold]];
];
(* Submanifold of another manifold *)
DefManifold[submanifold_,dim_,supermanifold_?ManifoldQ,indices_List,options:OptionsPattern[]]:=
Catch@Module[{tbundle,master,pa,pns,info},
{tbundle,master,pa,pns,info}=OptionValue[DefManifold,{DefInfo->{"manifold","Submanifold of "<>ToString[supermanifold]<>"."}},{Tangent,Master,PrintAs,ProtectNewSymbol,DefInfo}];
checkDefManifold[submanifold,supermanifold,tbundle,dim,indices];
(* Register manifold structure *)
MakeDefInfo[DefManifold,submanifold,info];
MakexTensions[DefManifold,"Beginning",submanifold,dim,supermanifold,indices,options];
Append[$Manifolds,submanifold];
ManifoldQ[submanifold]^=True;
DefInfo[submanifold]^=info;
SetPrintAs[submanifold,PrintAsString[submanifold,pa]];
SymbolRelations[submanifold,master,{supermanifold}];
submanifold/:SubmanifoldQ[supermanifold,submanifold]=True;
SplittingsOfManifold[submanifold]^={};
DimOfManifold[submanifold]^=dim;
(* Define (co)tangent bundle. DefVBundle assumes that the manifold has been already registered *)
If[dim===0,
If[$DefInfoQ,Print["** DefManifold:  Tangent bundle does not exist for a 0-dim manifold"]],
If[tbundle===Automatic,
tbundle=GiveSymbol[Tangent,submanifold],
Tangent[submanifold]^=tbundle;
];
DefVBundle[tbundle,submanifold,dim,indices,Master->submanifold,PrintAs:>GiveOutputString[Tangent,submanifold],options];
];
MakexTensions[DefManifold,"End",submanifold,dim,supermanifold,indices,options];
(* We do not define an explicit restricion of the bundle Tangentsupermanifold to submanifold *)
If[pns,Protect[submanifold]];
];
(* A zero-dimensional manifold does not need indices *)
DefManifold[manifold_,0,options___?OptionQ]:=DefManifold[manifold,0,{},options];
DefManifold[manifold_,0,supermanifold_?ManifoldQ,options___?OptionQ]:=DefManifold[manifold,0,supermanifold,{},options];
(* Other *)
SetNumberOfArguments[DefManifold,{2,Infinity}];
Protect[DefManifold];


DimOfManifold[x_]:=Throw@Message[DimOfManifold::unknown,"manifold",ToString[x]];
SetNumberOfArguments[DimOfManifold,1];
Protect[DimOfManifold];


SplittingsOfManifold[manifold_]:=Throw@Message[SplittingsOfManifold::unknown,"manifold", manifold];
SetNumberOfArguments[SplittingsOfManifold,1];
Protect[SplittingsOfManifold];


SubmanifoldQ[manifold_,manifold_]:=True;
SubmanifoldQ[_,_]:=False;
SetNumberOfArguments[SubmanifoldQ,2];
Protect[SubmanifoldQ];


DisjointManifoldsQ[manifolds1_List,manifold2_]:=And@@(DisjointManifoldsQ[#,manifold2]&/@manifolds1);
DisjointManifoldsQ[manifold1_,manifolds2_List]:=And@@(DisjointManifoldsQ[manifold1,#]&/@manifolds2);
DisjointManifoldsQ[manifold1_,manifold2_]:=Not[SubmanifoldQ[manifold1,manifold2]||SubmanifoldQ[manifold2,manifold1]];
SetNumberOfArguments[DisjointManifoldsQ,2];
Protect[DisjointManifoldsQ];


(* Cartesian product manifold *)
SplitManifold[supermanifold_,submanifolds_CartesianProduct]:=SplitManifold[supermanifold,List@@submanifolds];
SplitManifold[supermanifold_,submanifolds_List]:=
Catch@Module[{prot=Unprotect[supermanifold]},
(* Checks. DimOfManifold effectively checks that submanifolds contains manifolds *)
If[DimOfManifold[supermanifold]=!=Apply[Plus,DimOfManifold/@submanifolds],Throw@Message[DefManifold::error,"Invalid dimension of supermanifold."]];
(* Register *)
xUpAppendTo[SplittingsOfManifold[supermanifold],CartesianProduct@@submanifolds];TagSet[supermanifold,SubmanifoldQ[supermanifold,#],True]&/@submanifolds;
AppendToUnevaluated[$ProductManifolds,supermanifold];
(*SymbolRelations[supermanifold,Null,submanifolds];*)
Protect[Evaluate[prot]];
(* Split tangent bundle *)
SplitVBundle[Tangent[supermanifold],Tangent/@submanifolds];
];


UndefManifold[list:{___?ManifoldQ}]:=Scan[UndefManifold,list];
UndefManifold[manifold_]:=Catch@With[{servants=ServantsOf[manifold]},
(* Checks *)
If[Not@MatchQ[manifold,_Symbol],Throw@Message[UndefManifold::error,"Only symbols can be undefined."]];
If[Not@ManifoldQ[manifold],Throw@Message[UndefManifold::unknown,"manifold",manifold]];
CheckRemoveSymbol[manifold];
MakexTensions[UndefManifold,"Beginning",manifold];
xUpSet[ServantsOf[manifold],{}];
DropFromHosts[manifold];
(* Undefine servants, including the tangent bundle *)
Undef/@Reverse[servants];
$Manifolds=DeleteCases[$Manifolds,manifold];
$ProductManifolds=DeleteCases[$ProductManifolds,manifold];
MakexTensions[UndefManifold,"End",manifold];
(* Eliminate symbol *)
MakeUndefInfo[UndefManifold,manifold];
RemoveSymbol[manifold];
];
SetNumberOfArguments[UndefManifold,1];
Protect[UndefManifold];


SetAttributes[HeldManifoldQ,HoldAllComplete];
HeldManifoldQ[expr_]:=ManifoldQ[Unevaluated[expr]];


(* ManifoldBoundary *)
ManifoldQ[ManifoldBoundary[_?ManifoldQ]]^:=True;
DimOfManifold[ManifoldBoundary[M_?ManifoldQ]]^:=DimOfManifold[M]-1;
(* We assume no corners *)
ManifoldBoundary[ManifoldBoundary[M_?ManifoldQ]]:=EmptyManifold;
SetNumberOfArguments[ManifoldBoundary,1];
Protect[ManifoldBoundary];
(* Formatting *)
MakeBoxes[ManifoldBoundary[M_],fmt_]:=RowBox[{"\[PartialD]",MakeBoxes[M,fmt]}];


(* Empty manifold *)
ManifoldQ[EmptyManifold]^:=True;
ManifoldBoundary[EmptyManifold]^:=EmptyManifold;
DimOfManifold[EmptyManifold]^:=0;
Protect[EmptyManifold];
(* Formatting *)
MakeBoxes[EmptyManifold,fmt_]:="\[EmptySet]";


SortDependencies[list_List]:=DeleteDuplicates@Join[Sort@Select[list,ParameterQ],Sort@Select[list,ManifoldQ]];


DependenciesOfIndices[]:={};
DependenciesOfIndices[inds__]:=Apply[Union,DependenciesOfIndex/@IndexList[inds]];
DependenciesOfIndex[Dir[expr_]]:=DependenciesOf[expr];
(* If different elements of the basis have different dependencies, then this (private) function must be overwritten *)
DependenciesOfIndex[{a_,basis_}]:=xAct`xCoba`DependenciesOfBasis[basis]; (* BASIS1 *);
DependenciesOfIndex[basis_?BasisQ[a_]]:=xAct`xCoba`DependenciesOfBasis[basis]; (* BASIS2 *);
DependenciesOfIndex[-x_]:=DependenciesOfIndex[x];
DependenciesOfIndex[_Symbol]:={};
DependenciesOfIndex[_LI]:={};
(* Other cases, including patterns *)
DependenciesOfIndex[_]:={AnyDependencies};


DependenciesOf[expr:(_Plus|_Times)]:=Union@@(DependenciesOf/@List@@expr);
DependenciesOf[HoldPattern[SeriesData[var_,orig_,coeffs_,rest__]]]:=Union[DependenciesOf[var],DependenciesOf[orig],DependenciesOf/@coeffs];
DependenciesOf[tensor_?xTensorQ[inds___]]:=Union[DependenciesOfTensor[tensor],DependenciesOfIndices[inds]];
DependenciesOf[covd_?CovDQ[inds__][expr_]]:=Union[DependenciesOfCovD[covd],DependenciesOf[expr],DependenciesOfIndices[inds]];
DependenciesOf[CovD[expr_,ders___,covd__?CovDQ[inds__]]]:=Union[DependenciesOf[CovD[expr,ders]],DependenciesOfCovD[covd],DependenciesOfIndices[inds]];
DependenciesOf[LieD[v_][expr_]]:=Union[DependenciesOf[v],DependenciesOf[expr]];
DependenciesOf[ParamD[__][expr_]]:=DependenciesOf[expr];
DependenciesOf[OverDot[expr_]]:=DependenciesOf[expr];
DependenciesOf[param_Symbol?ParameterQ]:={param};
(* This must be overwritten if invalid *)
DependenciesOf[ih_?InertHeadQ[expr_,z___]]:=Union[DependenciesOf[expr],DependenciesOfInertHead[ih]];
DependenciesOf[prod_?ProductQ[exprs___]]:=Union@Flatten[DependenciesOf/@{exprs}];
DependenciesOf[sf_?ScalarFunctionQ[args__]]:=Union@Flatten[DependenciesOf/@{args}];
DependenciesOf[Derivative[__Integer][sf_?ScalarFunctionQ][args__]]:=Union@Flatten[DependenciesOf/@{args}];
DependenciesOf[x_Symbol?ParameterQ]:={x};
DependenciesOf[x_Symbol?ConstantSymbolQ]:={};
DependenciesOf[expr_?NumericQ]:={};
DependenciesOf[Scalar[expr_]]:=DependenciesOf[expr];
DependenciesOf[_]:={AnyDependencies};
SetNumberOfArguments[DependenciesOf,1];
Protect[DependenciesOf];


ManifoldsOf[expr_]:=Select[DependenciesOf[expr],ManifoldQ];
ManifoldsOfTensor[tensor_]:=Select[DependenciesOfTensor[tensor],ManifoldQ];
SetNumberOfArguments[ManifoldsOf,1];
Protect[ManifoldsOf];


ParametersOf[expr_]:=Select[DependenciesOf[expr],ParameterQ];
ParametersOfTensor[head_]:=Select[DependenciesOfTensor[head],ParameterQ];
SetNumberOfArguments[ParametersOf,1];
Protect[ParametersOf];


ManifoldQ[AnyDependencies]^=True;
ParameterQ[AnyDependencies]^=True;
Protect[AnyDependencies];


InertHeadQ[At]^=True;
LinearQ[At]^=True;
At[tensor_Plus,P_]:=At[#,P]&/@tensor;
At[tensor_Times,P_]:=At[#,P]&/@tensor;
At[x_,P_]:=x/;ManifoldsOf[x]==={};
DefInfo[At]^={"inert head","Generic head to denote restrictions to submanifolds."};
AppendToUnevaluated[$InertHeads,At];


DependenciesOf[At[expr_,sub_]]^:=Replace[DependenciesOf[expr],dep_?(SubmanifoldQ[#,sub]&):>sub];


MakeBoxes[At?HeldInertHeadQ[expr_,sub_],StandardForm]:=RowBox[{MakeBoxes[expr,StandardForm],SubscriptBox["|",MakeBoxes[sub,StandardForm]]}];


SetNumberOfArguments[At,2];
Protect[At];


MappingQ[IdentityMapping[M_?ManifoldQ]]^:=True;
InverseMapping[IdentityMapping[M_]]^:=IdentityMapping[M];
MappingDomain[IdentityMapping[M_]]^:=M;
MappingImage[IdentityMapping[M_]]^:=M;
ImmersionQ[IdentityMapping[M_]]^:=True;
SubmersionQ[IdentityMapping[M_]]^:=True;
TangentTensor[IdentityMapping[M_]]^:=delta;


IdentityMapping[M_][point_]:=point/;SubmanifoldQ[M,point];


PrintAs[IdentityMapping[M_]]^:=StringJoin["\!\(Id\_",PrintAs[M],"\)"];
MakeBoxes[IdentityMapping[M_],StandardForm]:=SubscriptBox["Id",MakeBoxes[M,StandardForm]];


Options[DefMapping]={
ImmersionQ->Automatic,
SubmersionQ->Automatic,
InverseMapping->Automatic,
Master->Null,
PrintAs->Identity,
ProtectNewSymbol:>$ProtectNewSymbols,
DefInfo->{"mapping",""}
};
DefMapping[list_List,rest___]:=Scan[DefMapping[#,rest]&,list];
DefMapping[phi_,domain_->image_,options:OptionsPattern[]]:=
Catch@Module[{type,tbundle,immQ,subQ,inv,master,pa,pns,info,definvQ,domdim,imdim,tdom,tim,tindsQ,iindsQ},
{immQ,subQ,inv,master,pa,pns,info}=OptionValue[{ImmersionQ,SubmersionQ,InverseMapping,Master,PrintAs,ProtectNewSymbol,DefInfo}];

(* 1. Name validation: the symbol might be a chart already *)
If[ChartQ[phi],
If[MappingQ[phi],
(* The mapping already exists. This is a duplicated name. Probably this never happens *)
Throw@Message[ValidateSymbol::used,phi,"as a mapping"],
(* Coordinate mapping being defined. Symbol validation already performed. Do nothing *)
Null
],
(* Noncoordinate mapping. Perform symbol validation *)
ValidateSymbol[phi];
ValidateSymbolInSession[phi]
];

(* 2. Consistency checks *)
If[!ManifoldQ[domain],Throw@Message[DefMapping::unknown,"manifold",domain]];
If[!ManifoldQ[image],Throw@Message[DefMapping::unknwon,"manifold",image]];
domdim=DimOfManifold[domain];
imdim=DimOfManifold[image];
If[immQ===Automatic,
immQ=TrueQ[domdim<=imdim],
If[!TrueOrFalse[immQ],Throw@Message[DefMapping::unknown,"value of option ImmersionQ",immQ]];
If[immQ&&domdim>imdim,Throw@Message[DefMapping::error,"Mapping cannot be an immersion because image dimension is smaller than domain dimension."]];
];
If[subQ===Automatic,
subQ=TrueQ[domdim>=imdim],
If[!TrueOrFalse[subQ],Throw@Message[DefMapping::unknown,"value of option SubmersionQ",subQ]];
If[subQ&&domdim<imdim,Throw@Message[DefMapping::error,"Mapping cannot be a submersion because image dimension is larger than domain dimension."]];
];
(* Check for the inverse mapping *)
definvQ=(immQ||subQ)&&Quiet[Catch[InverseMapping[phi]]]===Null;
If[definvQ,
If[inv===Automatic,inv=GiveSymbol[InverseMapping,phi]];
ValidateSymbol@@{inv};
ValidateSymbolInSession[inv];
];

(* 3. Register mapping properties *)
MakeDefInfo[DefMapping,phi,info];
MakexTensions[DefMapping,"Beginning",phi,{domain,image},options];
AppendToUnevaluated[$Mappings,phi];
MappingQ[phi]^=True;
DefInfo[phi]^=info;
SetPrintAs[phi,PrintAsString[phi,pa]];
SymbolRelations[phi,master,{domain,image}];
MappingDomain[phi]^=domain;
MappingImage[phi]^=image;
ImmersionQ[phi]^=immQ;
SubmersionQ[phi]^=subQ;
(* This is the main consequence of being a submersion *)
If[subQ,
phi[domain]=image,
If[immQ,phi/:SubmanifoldQ[image,phi[domain]]=True]
];

(* 4. Register InverseMapping *)
If[definvQ,
With[{iphi=inv},
InverseMapping[phi]^=iphi;
InverseMapping[iphi]^=phi;
(* Define inverse mapping and its differential *)
DefMapping[iphi,image->domain,ImmersionQ->subQ,SubmersionQ->immQ,Master->phi,PrintAs:>GiveOutputString[InverseMapping,phi],DefInfo:>Which[immQ&&subQ,{"inverse mapping",""},immQ,{"left-inverse mapping","Assuming arbitrary extension."},subQ,{"right-inverse mapping","Not uniquely defined."}]];
(* For a submersion there is a right inverse *)
If[subQ,
phi[iphi[x_]]:=x/;SubmanifoldQ[phi[domain],x];
phi/:SmallCircle[phi,iphi]=IdentityMapping[image];
];
(* For an immersion there is a left inverse *)
If[immQ,
iphi[phi[x_]]^:=x/;SubmanifoldQ[domain,x];
phi/:SmallCircle[iphi,phi]=IdentityMapping[domain];
]
]
];

(* Before proceeding, if the domain/image is Reals[_] check whether there are indices already *)
tdom=Tangent[domain];
tim=Tangent[image];
If[Head[domain]===Reals && Quiet[Catch[IndicesOfVBundle[tdom]]]===Null,
tindsQ=True;
Quiet@RegisterIndices[tdom,Take[DeleteCases[xAct`xCoba`Private`$TangentRealsTemporaryIndices,_?AbstractIndexQ],4]];
];
If[Head[image]===Reals&& Quiet[Catch[IndicesOfVBundle[tim]]]===Null,
iindsQ=True;
Quiet@RegisterIndices[tim,Take[DeleteCases[xAct`xCoba`Private`$TangentRealsTemporaryIndices,_?AbstractIndexQ],4]];
];

(* 5. Define the tangent mapping tensor *)
With[{dphi=GiveSymbol[TangentTensor,phi]},
Module[{index1,index2},
{index1}=GetIndicesOfVBundle[tdom,1];
(* This declares the pullback-vundle indices for the first time *)
{index2}=GetIndicesOfVBundle[PullBackVBundle[tim,phi],1];
DefTensor[dphi[-index1,index2],{domain},Master->phi,PrintAs:>GiveOutputString[TangentTensor,phi],DefInfo->{"mapping differential tensor",""}];
]
];

(* 6. Relations between the tangent mappings. Both dphi and diphi are declared at this moment *)
If[definvQ ,
With[{dphi=TangentTensor[phi],diphi=TangentTensor[inv],iphi=inv},
Inv[dphi]^=Precompose[diphi,phi];
Inv[diphi]^=Precompose[dphi,iphi];
Module[{index1,index2,index3,index4,index5,index6,index7,index8},
{index1,index3}=GetIndicesOfVBundle[tdom,2];
{index2,index4}=GetIndicesOfVBundle[PullBackVBundle[tim,phi],2];
{index5,index7}=GetIndicesOfVBundle[tim,2,{index1,index3}];
{index6,index8}=GetIndicesOfVBundle[PullBackVBundle[tdom,iphi],2,{index2,index4}];
If[ImmersionQ[phi],
xTagSetDelayed[{dphi,dphi[-index1_,index2_]Precompose[diphi,phi][-index2_,index3_]},delta[-index1,index3]];
xTagSetDelayed[{diphi,diphi[-index5_,index6_]Precompose[dphi,iphi][-index8_,index5_]},delta[-index8,index6]];
xTagSet[{dphi,InvertibleQ[dphi,Right]},True];
xTagSet[{diphi,InvertibleQ[diphi,Left]},True];
];
If[SubmersionQ[phi],
xTagSetDelayed[{dphi,dphi[-index1_,index2_]Precompose[diphi,phi][-index4_,index1_]},delta[-index4,index2]];
xTagSetDelayed[{diphi,diphi[-index5_,index6_]Precompose[dphi,iphi][-index6_,index7_]},delta[-index5,index7]];
xTagSet[{dphi,InvertibleQ[dphi,Left]},True];
xTagSet[{diphi,InvertibleQ[diphi,Right]},True];
];
]
]
];

(* Remove temporary indices *)
If[tindsQ,RemoveIndices[tdom,All]];
If[iindsQ,RemoveIndices[tim,All]];

MakexTensions[DefMapping,"End",phi,{domain,image},options];
If[pns,Protect[phi]];
];
(* Other *)
SetNumberOfArguments[DefMapping,{2,Infinity}];
Protect[DefMapping];


UndefMapping[list:{___?MappingQ}]:=Scan[UndefMapping,list];
UndefMapping[phi_]:=Catch@With[{servants=ServantsOf[phi]},
(* Checks *)
If[Not@MappingQ[phi],Throw@Message[UndefMapping::unknown,"mapping",phi]];
CheckRemoveSymbol[phi];
MakexTensions[UndefMapping,"Beginning",phi];
xUpSet[ServantsOf[phi],{}];
DropFromHosts[phi];
(* Undefine servants, including the tangent map *)
Undef/@Reverse[servants];
(* Undefine indices of the pullback bundle tangent to the image. TODO: Does not work for TangentReals *)
UndefAbstractIndex/@Flatten@IndicesOfVBundle[PullBackVBundle[Tangent[MappingImage[phi]],phi]];
$Mappings=DeleteCases[$Mappings,phi];
MakexTensions[UndefMapping,"End",phi];
(* Eliminate symbol *)
MakeUndefInfo[UndefMapping,phi];
RemoveSymbol[phi];
];
SetNumberOfArguments[UndefMapping,1];
Protect[UndefMapping];


MappingDomain[phi_]:=Throw@Message[MappingDomain::unknown,"mapping",phi];
SetNumberOfArguments[MappingDomain,1];
Protect[MappingDomain];


MappingImage[phi_]:=Throw@Message[MappingImage::unknown,"mapping",phi];
SetNumberOfArguments[MappingImage,1];
Protect[MappingImage];


Unprotect[ManifoldQ];
ManifoldQ[phi_?MappingQ[x_]]:=ManifoldQ[x];
Protect[ManifoldQ];


Unprotect[DimOfManifold];
DimOfManifold[phi_?MappingQ[x_?ManifoldQ]]:=If[ImmersionQ[phi],DimOfManifold[x],Throw@Message[DimOfManifold::error1,"Cannot deduce dimensions of manifold",phi[x]]];
Protect[DimOfManifold];


SetAttributes[SmallCircle,{Flat,OneIdentity}];
Verbatim[SmallCircle][phi_?MappingQ]:=phi;
Verbatim[SmallCircle][left___?MappingQ,_IdentityMapping,right___?MappingQ]:=SmallCircle[left,right];


MappingQ[SmallCircle[phis__?MappingQ]]^:=True;


MappingDomain[SmallCircle[__,phi_?MappingQ]]^:=MappingDomain[phi];
MappingImage[SmallCircle[phi_?MappingQ,__]]^:=MappingImage[phi];


ImmersionQ[SmallCircle[phis__?MappingQ]]:=And@@(ImmersionQ/@{phis});
SubmersionQ[SmallCircle[phis__?MappingQ]]:=And@@(SubmersionQ/@{phis});


InverseMapping[SmallCircle[phis__?InvertibleMappingQ]]:=SmallCircle@@(InverseMapping/@Reverse[{phis}]);


PrintAs[SmallCircle[phis__?MappingQ]]^:=StringJoin["(",Riffle[PrintAs/@{phis},$PrecomposeCharacter],")"];


SmallCircle[phis__?MappingQ][point_]:=Composition[phis][point];


ImmersionQ[phi_?MappingQ]:=False;
ImmersionQ[phi_]:=Throw@Message[ImmersionQ::unknown,"mapping",phi];
SetNumberOfArguments[ImmersionQ,1];
Protect[ImmersionQ];


SubmersionQ[phi_?MappingQ]:=False;
SubmersionQ[phi_]:=Throw@Message[SubmersionQ::unknown,"mapping",phi];
SetNumberOfArguments[SubmersionQ,1];
Protect[SubmersionQ];


GiveSymbol[InverseMapping,phi_]:=SymbolJoin["i",phi];
GiveOutputString[InverseMapping,phi_]:="\!\("<>PrintAs[phi]<>"\^-1\)";


InverseMapping::noinv="Mapping `1` is not invertible.";
InverseMapping[phi_?MappingQ]:=Throw@Message[InverseMapping::noinv,phi];
InverseMapping[phi_]:=Throw@Message[InverseMapping::unknown,"mapping",phi];
SetNumberOfArguments[InverseMapping,1];
Protect[InverseMapping];


InvertibleMappingQ[phi_]:=ImmersionQ[phi]||SubmersionQ[phi];


DiffeomorphismQ[phi_?MappingQ]:=ImmersionQ[phi]&&SubmersionQ[phi];
DiffeomorphismQ[phi_]:=Throw@Message[DiffeomorphismQ::unknown,"mapping",phi];
SetNumberOfArguments[DiffeomorphismQ,1];
Protect[DiffeomorphismQ];


MappingQ[phi_CartesianProduct]^:=And@@(MappingQ/@List@@phi);
MappingDomain[phi_CartesianProduct]^:=MappingDomain/@phi;
MappingImage[phi_CartesianProduct]^:=MappingImage/@phi;
InverseMapping[phi_CartesianProduct]^:=InverseMapping/@phi;
ImmersionQ[phi_CartesianProduct]^:=And@@(ImmersionQ/@List@@phi);
SubmersionQ[phi_CartesianProduct]^:=And@@(SubmersionQ/@List@@phi);


CartesianProduct/:SmallCircle[phi1_CartesianProduct,phi2_CartesianProduct]:=ThreadCartesianProduct[SmallCircle,{phi1,phi2}];


phi1_CartesianProduct[phi2_CartesianProduct[point_]]:=ThreadCartesianProduct[SmallCircle,{phi1,phi2}][point];


phi_CartesianProduct[point_CartesianProduct]:=ThreadCartesianProduct[Compose,{phi,point}];


(***************************** 7.Vector bundles ****************************)


If[$ReadingVerbose,Print["Reading section 7: Vector bundles."],Null,Null]


checkDefVBundle[vbundle_,manifold_,dimF_,indices_,dag_]:=(
(* 1.- Validate vector bundle *)
ValidateSymbol[vbundle];
ValidateSymbolInSession[vbundle];
(* 2.- Validate manifold *)
If[!ManifoldQ[manifold],Throw@Message[DefVBundle::unknown,"manifold",manifold]];
(* 3.- Validate indices *)
If[Head[indices]=!=List,Throw@Message[DefVBundle::invalid,indices,"list of indices"]];
If[indices=={},Throw@Message[DefVBundle::empty,4,"indices"]];
ValidateIndex[#,False]&/@indices;
(* 4.- Validate dag option *)
If[FreeQ[{Real,Complex,Conjugate},dag],Throw@Message[DefVBundle::invalid,dag,"value for option Dagger"]]
);


RegisterVBundle[vbundle_Symbol,manifold_]:=(
(* 1.- Register symbol *)
AppendToUnevaluated[$VBundles,vbundle];
VBundleQ[vbundle]^=True;
(* 2.- Register base manifold *)
BaseOfVBundle[vbundle]^=manifold;
(* 3.- There is no associated metric by default *)
MetricsOfVBundle[vbundle]^={};
);


BaseOfVBundle[-vbundle_]:=BaseOfVBundle[vbundle];
BaseOfVBundle[x_]:=Throw@Message[BaseOfVBundle::unknown,"vbundle",x];
SetNumberOfArguments[BaseOfVBundle,1];
Protect[BaseOfVBundle];


MetricsOfVBundle[-vbundle_]:=MetricsOfVBundle[vbundle];
MetricsOfVBundle[x_]:=Throw@Message[MetricsOfVBundle::unknown,"vbundle", x];
SetNumberOfArguments[MetricsOfVBundle,1];
Protect[MetricsOfVBundle];


FirstMetricOfVBundle[vbundle_,mess_:True]:=firstmetricofvbundle[vbundle,MetricsOfVBundle[vbundle],mess];
firstmetricofvbundle[vbundle_,{},True]:=Throw@Message[MetricsOfVBundle::missing,"metric",vbundle];
firstmetricofvbundle[vbundle_,{},False]:=Null;
firstmetricofvbundle[vbundle_,list_List,_]:=First[list];
firstmetricofvbundle[vbundle_,list_,_]:=Throw@Message[MetricsOfVBundle::invalid,list,"list of metrics"];


FirstMetricQ[Null,___]:=False;
FirstMetricQ[metric_]:=FirstMetricQ[metric,VBundleOfMetric[metric]];
FirstMetricQ[metric_,vbundle_]:=metric===FirstMetricOfVBundle[vbundle,True];


$FirstMetrics:=DeleteCases[FirstMetricOfVBundle[#,False]&/@$VBundles,Null];


MetricEndowedQ[vbundle_]:=UnsameQ[MetricsOfVBundle[vbundle],{}];
SetNumberOfArguments[MetricEndowedQ,1];
Protect[MetricEndowedQ];


IndexRange[first_String,last_String]:=ToExpression/@CharacterRange[first,last];
IndexRange[first_Symbol,last_]:=IndexRange[ToString[first],last];
IndexRange[first_,last_Symbol]:=IndexRange[first,ToString[last]];
IndexRange[range_List]:=IndexRange@@range;
IndexRange[ranges__List]:=Union@@(IndexRange/@{ranges});
IndexRange[x___]:=Throw[Message[IndexRange::invalid,HoldForm[x],"input for IndexRange"]];
Protect[IndexRange];


RegisterIndexInVBundle[index_Symbol,vbundle_,options___]:=With[{vbQ=VBundleIndexQ[vbundle]},
DefAbstractIndex[index,options];
xUpSet[VBundleOfIndex[index],vbundle];
xUpSet[vbQ[index],True]
];


RegisterIndices[vbundle_?VBundleQ,indices:{__Symbol},options___]:=With[{vbQ=VBundleIndexQ[vbundle],vbpmQ=VBundleIndexPMQ[vbundle]},
(* 1.- Properties of the indices *)
RegisterIndexInVBundle[#,vbundle,options]&/@indices;
(* 2.- Properties of the vector bundle. Separate the case of pullback vbundles *)
If[Head[vbundle]===PullBackVBundle,
With[{vb=First[vbundle],map=Last[vbundle]},
xTagSet[{vb,IndicesOfVBundle[vb,map]},{indices,{}}]
],
IndicesOfVBundle[vbundle]^={indices,{}};
];
SetLastIndex[vbundle,{Last[indices],1}];
(* 3.- Q-functions of the vector bundle *)
vbpmQ[-index_]:=vbQ[index];
vbpmQ[index_]:=vbQ[index];
vbQ[-index_]:=False;
vbQ[index_Symbol?DollarQ]:=vbQ[index]^=vbQ[NoDollar@index];
vbQ[_]:=False;
(* 4.- Complex structure *)
DaggerIndex/@indices;
(* 5.- Return indices *)
{indices,{}}
];


IndicesOfVBundle[-vbundle_]:=IndicesOfVBundle[vbundle];
(* Pullback vbundle: switch to two argument form *)
IndicesOfVBundle[PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]]^:=IndicesOfVBundle[vbundle,phi];
(* First call of IndicesOfVBundle on a PullBackVBundle does this and gets cached. PrintAs will be handled somewhere else *)
IndicesOfVBundle[vbundle_?VBundleQ,phi_?MappingQ]:=
RegisterIndices[PullBackVBundle[vbundle,phi],MakePullBackIndex[#,phi]&/@First[IndicesOfVBundle[vbundle]],PrintAs->Null];
(* Tangent of a submanifold: consider as a pullback vbundle *)
IndicesOfVBundle[Tangent[phi_?MappingQ[manifold_?ManifoldQ]]]:=IndicesOfVBundle[Tangent[MappingImage[phi]],phi];
IndicesOfVBundle[vbundle_]:=Throw@Message[IndicesOfVBundle::unknown,"vector bundle",vbundle];
SetNumberOfArguments[IndicesOfVBundle,1];
Protect[IndicesOfVBundle];


FirstIndexOfVBundle[-vbundle_]:=FirstIndexOfVBundle[vbundle];
FirstIndexOfVBundle[vbundle_]:=IndicesOfVBundle[vbundle][[1,1]];


LastIndex[-vbundle_]:=LastIndex[vbundle];
LastIndex[PullBackVBundle[vbundle_, phi_]] := LastIndex[vbundle, phi];
SetNumberOfArguments[LastIndex,{1,2}];
Protect[LastIndex];


SetLastIndex[-vbundle_,update_]:=SetLastIndex[vbundle,update];
SetLastIndex[PullBackVBundle[vb_Symbol,phi_],1]:=xTagSet[{vb,LastIndex[vb,phi]},LastIndex[vb,phi]+{0,1}];
SetLastIndex[PullBackVBundle[vb_Symbol,phi_],new_]:=xTagSet[{vb,LastIndex[vb,phi]},new];
SetLastIndex[vbundle_,1]:=xUpSet[LastIndex[vbundle],LastIndex[vbundle]+{0,1}];
SetLastIndex[vbundle_,new_]:=xUpSet[LastIndex[vbundle],new];
(* Remove upvalue *)
SetLastIndex[vbundle_]:=xUpSet[LastIndex[vbundle]];


VBundleOfIndex[x:-_Dir]:=Throw@Message[VBundleOfIndex::invalid,x,"index"];
(* Note that there is no minus sign on the RHS *)
VBundleOfIndex[-x_]:=VBundleOfIndex[x];
VBundleOfIndex[_Blank]:=Throw@Message[VBundleOfIndex::nouse,"VBundleOfIndex","a Blank pattern index"];
(* PINDEX *)
VBundleOfIndex[x_Pattern]:=VBundleOfIndex[First[x]];
VBundleOfIndex[x_PatternTest]:=VBundleOfIndex[First[x]];
VBundleOfIndex[v_Dir]:=VBundleOfIndex[UltraindexOf[v]];
(* C indices *)
VBundleOfIndex[{_Integer,basis_}]:=xAct`xCoba`VBundleOfBasis[basis]; (* BASIS1 *);
VBundleOfIndex[basis_Symbol?BasisQ[_Integer]]:=xAct`xCoba`VBundleOfBasis[basis]; (* BASIS2 *);
(* B indices. No check of consistency of abstract index and basis *)
VBundleOfIndex[{x_,_}]:=VBundleOfIndex[x]; (* BASIS1 *);
(* Other index modifiers *)
VBundleOfIndex[phi_?MappingQ[x_]]:=With[{vb=VBundleOfIndex[x]},vb/;vb===Tangent[MappingImage[phi]]];
VBundleOfIndex[imod_Symbol?IModQ[x_]]:=VBundleOfIndex[x];(* BASIS2 *);
(* Special case for definition of tensors with variable rank *)
VBundleOfIndex[AnyIndices[vbundle_?VBundleQ]]:=vbundle;
VBundleOfIndex[index_Symbol?AIndexQ]:=VBundleOfIndex@NoDollar@index;
VBundleOfIndex[x_LI]:=Labels;
(* VBundleOfIndex[index_]:=VBundleOfIndex[ToExpression@SymbolName@index]/;FreeQ[$ContextPath,Context[index]]*)
VBundleOfIndex[index_]:=Message[VBundleOfIndex::unknown,"index", index];
SetNumberOfArguments[VBundleOfIndex,1];
Protect[VBundleOfIndex];


SignedVBundleOfIndex[AnyIndices[vbundle_?VBundleQ]]:=AnyIndices[vbundle];
SignedVBundleOfIndex[index_?UpIndexQ]:=VBundleOfIndex[index];
SignedVBundleOfIndex[index_?DownIndexQ]:=-VBundleOfIndex[index];


VBundleUpQ[index_]:={VBundleOfIndex[index],UpIndexQ[index]};


(* These upvalues are used *)
VBundleQ[Labels]^:=True;
PrintAs[Labels]^:=Labels;
MetricsOfVBundle[Labels]^:={};
(* Are these other used too? We print a message in case they are *)
xAct`xCoba`CNumbersOf[Labels]^:=(Print["CNumbersOf[Labels] called. Tell JMM"];{});
ManifoldQ[Labels]^:=(Print["ManifoldQ[Labels] called. Tell JMM"];True);
SplittingsOfManifold[Labels]^:=(Print["SplittingsOfManifold[Labels] called. Tell JMM"];{});
DimOfManifold[Labels]^:=(Print["DimOfManifold[Labels] called. Tell JMM"];0);
DimOfVBundle[Labels]^:=(Print["DimOfVBundle[Labels] called. Tell JMM"];0);
VisitorsOf[Labels]^:=(Print["VisitorsOf[Labels] called. Tell JMM"];{});
ServantsOf[Labels]^:=(Print["ServantsOf[Labels] called. Tell JMM"];{});
IndicesOfVBundle[Labels]^:=(Print["IndicesOfVBundle[Labels] called. Tell JMM"];{});
BaseOfVBundle[Labels]^:=(Print["BaseOfVBundle[Labels] called. Tell JMM"];Null);


Options[DefVBundle]={Dagger->Real,Master->Null,PrintAs->Identity,ProtectNewSymbol:>$ProtectNewSymbols,DefInfo->{"vbundle",""}};
DefVBundle[list_List,rest___]:=Scan[DefVBundle[#,rest]&,list];
(* Backwards compatibility *)
DefVBundle[vbundle_,manifold_,dimF_,indices_,ultraindex_Symbol,options___]:=
(Print["** DefVBundle (legacy): Ultraindex "<>ToString[ultraindex]<>" converted into normal index."];DefVBundle[vbundle,manifold,dimF,Append[indices,ultraindex],options]);
(* Backwards compatibility *)
DefVBundle[vbundle_,manifold_,subvbundles_List,indices_,options___?OptionQ]:=
(DefVBundle[vbundle,manifold,Apply[Plus,DimOfVBundle/@subvbundles],indices,options];
SplitVBundle[vbundle,subvbundles]);
(* Simple vector bundle *)
DefVBundle[vbundle_,manifold_,dimF_,indices_,options:OptionsPattern[]]:=
Catch@Module[{dag,master,pa,pns,info},
{dag,master,pa,pns,info}=OptionValue[{Dagger,Master,PrintAs,ProtectNewSymbol,DefInfo}];
checkDefVBundle[vbundle,manifold,dimF,indices,dag];
MakeDefInfo[DefVBundle,vbundle,info];
MakexTensions[DefVBundle,"Beginning",vbundle,manifold,dimF,indices,ultraindex,options];
Switch[dag,
Real,Dagger[vbundle]^=vbundle,
Complex,SetDaggerPair[vbundle,MakeDaggerSymbol[vbundle]],
Conjugate,Null
];
RegisterVBundle[vbundle,manifold];
RegisterIndices[vbundle,indices];
(*Dim1: If[dimF===1,RegisterDim1VBundle[vbundle,First[indices]]]; *)
If[dag===Complex,
DefVBundle[Dagger[vbundle],manifold,dimF,DaggerIndex/@indices,Dagger->Conjugate,Master->vbundle,options,DefInfo->{"conjugated vbundle","Assuming fixed anti-isomorphism between "<>ToString[vbundle]<>" and "<>ToString[Dagger[vbundle]]}]];
DimOfVBundle[vbundle]^=dimF;
SplittingsOfVBundle[vbundle]^={};
DefInfo[vbundle]^=info;
SetPrintAs[vbundle,AddDaggerCharacter[PrintAsString[vbundle,pa],dag]];
SymbolRelations[vbundle,master,{manifold}];
MakexTensions[DefVBundle,"End",vbundle,manifold,dimF,indices,ultraindex,options];
If[pns,Protect[vbundle]];
];
(* Other cases. Errors *)
SetNumberOfArguments[DefVBundle,{4,Infinity}];
Protect[DefVBundle];


SplitVBundle[vbundle_,splitting_CirclePlus]:=SplitVBundle[vbundle,List@@splitting];
SplitVBundle[vbundle_,{}]:=Null;
SplitVBundle[vbundle_,subvbundles_List]:=Catch@Module[{prot=Unprotect[vbundle],dag,dagsplits},
(* Checks. DimOfVBundle effectively checks that subvbundles contains vbundles *)
If[DimOfVBundle[vbundle]=!=Apply[Plus,DimOfVBundle/@subvbundles],Throw@Message[DefVBundle::error,"Invalid dimension of subvbundle."]];
dagsplits=SplittingsOfVBundle[Dagger@vbundle];
dag=(Or@@(DaggerQ/@Prepend[subvbundles,vbundle]))&&
(dagsplits==={}||List@@Last[dagsplits]=!=Dagger/@subvbundles)&&
(DaggerQ[vbundle]||Throw@Message[SplitVBundle::error,"Real vbundle cannot be splitted in complex subvbundles."]);
(* Register *)
xUpAppendTo[SplittingsOfVBundle[vbundle],CirclePlus@@subvbundles];
TagSet[vbundle,SubvbundleQ[vbundle,#],True]&/@subvbundles;
AppendToUnevaluated[$SumVBundles,vbundle];
(*SymbolRelations[vbundle,Null,subvbundles];*)
Protect[Evaluate[prot]];
If[dag,SplitVBundle[Dagger[vbundle],Dagger/@subvbundles]];
];
SetNumberOfArguments[SplitVBundle,2];
Protect[SplitVBundle];


DimOfVBundle[-vbundle_]:=DimOfVBundle[vbundle];
DimOfVBundle[x_]:=Throw@Message[DimOfVBundle::unknown,"vbundle",x];
SetNumberOfArguments[DimOfVBundle,1];
Protect[DimOfVBundle];


UndefVBundle[list:{___?VBundleQ}]:=Scan[UndefVBundle,list];
UndefVBundle[vbundle_]:=Catch@With[{servants=ServantsOf[vbundle],manifold=BaseOfVBundle[vbundle]},
If[!VBundleQ[vbundle],Throw@Message[UndefVBundle::unknown,"bundle",vbundle]];
CheckRemoveSymbol[vbundle];
MakexTensions[UndefVBundle,"Beginning",vbundle];
xUpSet[ServantsOf[vbundle],{}];
DropFromHosts[vbundle];
Undef/@Reverse[servants];
UndefAbstractIndex/@Flatten@IndicesOfVBundle[vbundle];
$VBundles=DeleteCases[$VBundles,vbundle];
$SumVBundles=DeleteCases[$SumVBundles,vbundle];
Remove[Evaluate@VBundleIndexQ[vbundle],Evaluate@VBundleIndexPMQ[vbundle]];
MakexTensions[UndefVBundle,"End",vbundle];
MakeUndefInfo[UndefVBundle,vbundle];
RemoveSymbol[vbundle];
];
SetNumberOfArguments[UndefVBundle,1];
Protect[UndefVBundle];


SetAttributes[CirclePlus,{Flat,OneIdentity}];
Verbatim[CirclePlus][vbundle_]:=vbundle;


VBundleQ[vbundle_CirclePlus]^:=And@@(VBundleQ/@List@@vbundle);
BaseOfVBundle[vbundle_CirclePlus]^:=CartesianProduct@@DeleteDuplicates[BaseOfVBundle/@List@@vbundle];
DimOfVBundle[vbundle_CirclePlus]^:=Plus@@(DimOfVBundle/@List@@vbundle);
IndicesOfVBundle[vbundle_CirclePlus]^:=TODO;
LastIndex[vbundle_CirclePlus]^:=TODO;
(* Metric tensors by blocks using respective members of the MetricsOfVBundle lists *)
MetricsOfVBundle[vbundle_CirclePlus]^:=Module[{metrics,min},
metrics=MetricsOfVBundle/@List@@vbundle;
min=Min[Length/@metrics];
metrics=Take[#,min]&/@metrics;
BlockTensor[DiagonalArray[#,2,Zero]]&/@Transpose[metrics]
];
SplittingsOfVBundle[vbundle_CirclePlus]^:={vbundle};
CirclePlus/:SubvbundleQ[large_CirclePlus,small_CirclePlus]:=If[Length[large]===Length[small],Inner[SubvbundleQ,large,small,And],False];
Dagger[vbundle_CirclePlus]^:=Dagger/@vbundle;


PrintAs[vbundle_CirclePlus]^:=StringJoin@@Riffle[PrintAs/@List@@vbundle,"\[CirclePlus]"];


PullBackVBundle[vbundle_?VBundleQ,_IdentityMapping]:=vbundle;


PullBackVBundle::vbmap="Base of vbundle `1` does not coincide with the image of mapping `2`.";
PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]:=$Failed/;MappingImage[phi]=!=BaseOfVBundle[vbundle]&&Throw@Message[PullBackVBundle::vbmap,vbundle,phi]


VBundleQ[PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]]^:=True;
PullBackVBundle[-vbundle_,phi_]:=-PullBackVBundle[vbundle,phi];
(* It is a right-action with respect to composition *)
PullBackVBundle[PullBackVBundle[vbundle_?VBundleQ,phi1_?MappingQ],phi2_?MappingQ]:=PullBackVBundle[vbundle,SmallCircle[phi1,phi2]];


(* The dimension of a bundle is that of its standard fiber, shared by a bundle and its pullbacks *)
DimOfVBundle[PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]]^:=DimOfVBundle[vbundle];


BaseOfVBundle[PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]]^:=MappingDomain[phi];
Dagger[PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]]^:=PullBackVBundle[Dagger[vbundle],Dagger[phi]];
HasDaggerCharacterQ[PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]]^:=HasDaggerCharacterQ[vbundle];
MetricsOfVBundle[PullBackVBundle[vbundle_?VBundleQ,phi_?MappingQ]]^:=Precompose[#,phi]&/@MetricsOfVBundle[vbundle];
SplittingsOfVBundle[PullBackVBundle[vbundle_,phi_]]^:=PullBackVBundle[#,phi]&/@SplittingsOfVBundle[vbundle];
SubvbundleQ[PullBackVBundle[large_,phi_],PullBackVBundle[small_,phi_]]^:=SubvbundleQ[large,small];


PrintAs[PullBackVBundle[vbundle_,phi_]]^:=StringJoin["(\!\(",PrintAs[vbundle],"\^*\))"];


MakeBoxes[PullBackVBundle[vbundle_?HeldVBundleQ,phi_?MappingQ],StandardForm]:=PullBackBox[MakeBoxes[vbundle,StandardForm],phi];


SetAttributes[HeldVBundleQ,HoldAllComplete];
HeldVBundleQ[expr_]:=VBundleQ[Unevaluated[expr]];


xTensorFormStart[VBundle]:=(MakeBoxes[vb_Symbol?HeldVBundleQ,StandardForm]:=interpretbox[vb,PrintAs[Unevaluated[vb]]]);
xTensorFormStop[VBundle]:=(MakeBoxes[vb_Symbol?HeldVBundleQ,StandardForm]=.);
xTensorFormStart[VBundle]


SplittingsOfVBundle[-vbundle_]:=-SplittingsOfVBundle[vbundle];
SplittingsOfVBundle[x_]:=Throw@Message[SplittingsOfVBundle::unknown,"vbundle", x];
SetNumberOfArguments[SplittingsOfVBundle,1];
Protect[SplittingsOfVBundle];


SubvbundleQ[vbundle_,vbundle_]:=True;
SubvbundleQ[All,vbundle_]:=True;
SubvbundleQ[_,_]:=False;
SetNumberOfArguments[SubvbundleQ,2];
Protect[SubvbundleQ];


SubdummiesIn[vbundle_]:=Apply[IndexList,DummyIn/@List@@(Last@SplittingsOfVBundle[vbundle])];
SetNumberOfArguments[SubdummiesIn,1];
Protect[SubdummiesIn];


DisjointVBundlesQ[vbundles1_List,vbundle2_]:=And@@(DisjointVBundlesQ[#,vbundle2]&/@vbundles1);
DisjointVBundlesQ[vbundle1_,vbundles2_List]:=And@@(DisjointVBundlesQ[vbundle1,#]&/@vbundles2);
DisjointVBundlesQ[vbundle1_,vbundle2_]:=Not[SubvbundleQ[vbundle1,vbundle2]||SubvbundleQ[vbundle2,vbundle1]];
SetNumberOfArguments[DisjointVBundlesQ,2];
Protect[DisjointVBundlesQ];


DummyIn[-vbundle_]:=-DummyIn[vbundle];
DummyIn[Labels]:=LI[General];
DummyIn[vbundle_?VBundleQ]:=(
VBundleIndexQ[vbundle][#1]^=True;
NoDollar[#1]^=#2;
AbstractIndexQ[#1]^=True;
#1)&@@{Unique[#],#}&@First[LastIndex[vbundle]];
DummyIn[vbundle_,AIndex|-AIndex]:=DummyIn[vbundle];
DummyIn[vbundle_,basis_]:={DummyIn[vbundle],basis};
DummyIn[vbundle_]:=Throw@Message[DummyIn::unknown,"vector bundle",vbundle];
SetNumberOfArguments[DummyIn,{1,2}];
Protect[DummyIn];


DummyAs[i:(_LI|-_LI)]:=i;
DummyAs[i_]:=If[upQ[i],UpIndex,DownIndex][DummyIn[VBundleOfIndex[i]]];


DummyAs[i_,AIndex]:=DummyAs[i];
DummyAs[i:_Symbol|-_Symbol,Automatic]:=DummyAs[i,AIndex];


DummyAs[i_,basis_]:=With[{f=If[upQ[i],UpIndex,DownIndex]},{f[DummyIn[VBundleOfIndex[i]]],f[basis]}];
DummyAs[i:{_,basis_},Automatic]:=DummyAs[i,basis];


SetNumberOfArguments[DummyAs,1];
Protect[DummyAs];


AddIndicesInFirstList[PullBackVBundle[vb_Symbol?VBundleQ,phi_],indices_]:=Module[{inds=IndicesOfVBundle[vb,phi]},
xTagSet[{vb,IndicesOfVBundle[vb,phi]},{Join[First@inds,indices],Last@inds}];
];
AddIndicesInFirstList[vbundle_Symbol?VBundleQ,indices_]:=Module[{inds=IndicesOfVBundle[vbundle]},
xUpSet[IndicesOfVBundle[vbundle],{Join[First@inds,indices],Last@inds}];
];


AddIndices[-vbundle_,indices_List]:=AddIndices[vbundle,UpIndex/@indices];
AddIndices[vbundle_?VBundleQ,indices_List]:=(
RegisterIndexInVBundle[#,vbundle]&/@indices;
AddIndicesInFirstList[vbundle,indices];
SetLastIndex[vbundle,{Last@indices,Last@LastIndex[vbundle]}];
If[DaggerQ[vbundle]&&!HasDaggerCharacterQ[vbundle],AddIndices[Dagger[vbundle],MakeDaggerSymbol/@indices]];
);
AddIndices[vbundle_,_]:=Throw@Message[AddIndices::unknown,"vector bundle",vbundle];
SetNumberOfArguments[AddIndices,2];
Protect[AddIndices];


RemoveIndicesInFirstList[PullBackVBundle[vb_Symbol?VBundleQ,phi_],indices_]:=Module[{inds=IndicesOfVBundle[vb,phi]},
xTagSet[{vb,IndicesOfVBundle[vb,phi]},{DeleteCases[First@inds,Alternatives@@indices],Last@inds}];
RemoveSymbol/@Intersection[indices,First@inds];
];
RemoveIndicesInFirstList[vbundle_Symbol?VBundleQ,indices_]:=Module[{inds=IndicesOfVBundle[vbundle]},
xUpSet[IndicesOfVBundle[vbundle],{DeleteCases[First@inds,Alternatives@@indices],Last@inds}];
RemoveSymbol/@Intersection[indices,First@inds];
];


RemoveIndices[vbundle_,All]:=RemoveIndices[vbundle,First@IndicesOfVBundle[vbundle]];
RemoveIndices[-vbundle_,indices_]:=RemoveIndices[vbundle,UpIndex/@indices];
RemoveIndices[vbundle_?VBundleQ,indices_List]:=Module[{vbinds},
If[DaggerQ[vbundle]&&!HasDaggerCharacterQ[vbundle],RemoveIndices[Dagger[vbundle],DaggerIndex/@indices]];
RemoveIndicesInFirstList[vbundle,indices];
vbinds=IndicesOfVBundle@vbundle;
If[vbinds==={{},{}},
SetLastIndex[vbundle],
SetLastIndex[vbundle,{Last@First@vbinds,Last@LastIndex[vbundle]}];
]
];
RemoveIndices[vbundle_,_]:=Throw@Message[RemoveIndices::unknown,"vector bundle",vbundle];
SetNumberOfArguments[RemoveIndices,2];
Protect[RemoveIndices];


AddIndexInSecondList[vbundle:PullBackVBundle[vb_Symbol?VBundleQ,phi_],ind_]:=Module[{inds=IndicesOfVBundle[vb,phi]},
xTagSet[{vb,IndicesOfVBundle[vb,phi]},{First@inds,Append[Last@inds,ind]}];
];
AddIndexInSecondList[vbundle_Symbol?VBundleQ,ind_]:=Module[{inds=IndicesOfVBundle[vbundle]},
xUpSet[IndicesOfVBundle[vbundle],{First@inds,Append[Last@inds,ind]}];
];


NewIndexIn[-vbundle_]:=NewIndexIn[vbundle];
NewIndexIn[vbundle_?HasDaggerCharacterQ]:=NewIndexIn[Dagger[vbundle]];
NewIndexIn[vbundle_?VBundleQ]:=Module[{ind},
(* Decide what the next index will be *)
Label[trynext];
ind=SymbolJoin@@Prepend[LastIndex[vbundle],Context@@{First@LastIndex[vbundle]}];
Check[Catch@ValidateIndex[ind],SetLastIndex[vbundle,1];Goto[trynext]];
(* Register new index *)
RegisterIndexInVBundle[ind,vbundle];
AddIndexInSecondList[vbundle,ind];
SetLastIndex[vbundle,1];
(* Synchronize conjugate vbundle *)
If[DaggerQ[vbundle]&&!HasDaggerCharacterQ[vbundle],
With[{vbdagger=Dagger[vbundle],inddagger=MakeDaggerSymbol[ind]},
RegisterIndexInVBundle[inddagger,vbdagger];
AddIndexInSecondList[vbdagger,inddagger];
SetLastIndex[vbdagger,{LastIndex[vbdagger][[1]],LastIndex[vbundle][[2]]}];
]
];
(* Return new index *)
ind
];
NewIndexIn[vbundle_]:=Message[NewIndexIn::unknown,"vector bundle",vbundle];
SetNumberOfArguments[NewIndexIn,1];
Protect[NewIndexIn];


GetIndicesOfVBundle[vbundle_,number_]:=GetIndicesOfVBundle[vbundle,number,{}];
GetIndicesOfVBundle[vbundle_,number_,list_IndexList]:=GetIndicesOfVBundle[vbundle,number,List@@list];
GetIndicesOfVBundle[-vbundle_,number_,list_List]:=GetIndicesOfVBundle[vbundle,number,list];
GetIndicesOfVBundle[vbundle_?VBundleQ,number_Integer?NonNegative,list_List]:=Module[{ind,output},
output=Complement[Flatten@IndicesOfVBundle@vbundle,list];
While[Length[output]<number,
ind=NewIndexIn[vbundle];
If[FreeQ[list,ind],AppendTo[output,ind]]
];
output[[Range[number]]]];
GetIndicesOfVBundle[_,_,list_/;Head[list]=!=List]:=Throw@Message[GetIndicesOfVBundle::invalid,list,"list of indices"];
GetIndicesOfVBundle[_Symbol?VBundleQ,number_,_List]:=Throw@Message[GetIndicesOfVBundle::invalid,number,"nonnegative integer"];
GetIndicesOfVBundle[vbundle_,_,_List]:=Throw@Message[GetIndicesOfVBundle::invalid,vbundle,"vector bundle"];
SetNumberOfArguments[GetIndicesOfVBundle,{2,3}];
Protect[GetIndicesOfVBundle];


(**************************** 8.Formatting ****************************)


If[$ReadingVerbose,Print["Reading section 8: Formatting."],Null,Null]


$CIndexForm=False;
(* Treat dual indices in the same way *)
CIndexForm[ind_Integer,-basis_]:=CIndexForm[ind,basis];
CIndexForm[ind_Integer,chart_?ChartQ]:=If[$CIndexForm,With[{c=xAct`xCoba`Coordinate[ind,chart]},PrintAs[c]],ToString[ind]];
CIndexForm[ind_Integer,basis_]:=ToString[ind];


(* Throw error if a minus is received. This should never happen *)
IndexForm[-x_]:=(Message[IndexForm::nouse,"IndexForm",-x];PrintAs[-x]);
(* Directional index. Long expressions give # *)
IndexForm[Dir[v_?xTensorQ[_]]]:=PrintAs[v];
IndexForm[Dir[v_]]:="#";
(* Labels. Use PrintAs again *)
IndexForm[LI[]]:=" ";
IndexForm[LI[x_]]:=PrintAs[x];
IndexForm[LI[x__]]:=Apply[StringJoin,PrintAs/@{x}];
(* Component index. Color according to basis *)
IndexForm[{ind_Integer,basis_}]:=ColorString[CIndexForm[ind,basis],xAct`xCoba`BasisColor[basis]]; (* BASIS1 *);
IndexForm[basis_?BasisQ[ind_Integer]]:=ColorString[CIndexForm[ind,basis],xAct`xCoba`BasisColor[basis]]; (* BASIS2 *);
(* Basis index. Color according to basis *)
IndexForm[{ind_,basis_}]:=ColorString[IndexForm[ind],xAct`xCoba`BasisColor[basis]]; (* BASIS1 *);
IndexForm[basis_?BasisQ[ind_]]:=ColorString[IndexForm[ind],xAct`xCoba`BasisColor[basis]]; (* BASIS2 *);
(* QUESTION: What happens for general index modifiers ? *)
(* Pattern or PatternTest index. Underline. PINDEX *)
IndexForm[ind_PatternTest]:=Underline[IndexForm[First[ind]]];
IndexForm[ind_Pattern]:=Underline[IndexForm[First[ind]]];
(* Blank family. Use an underscore as suggested by Thomas *)
IndexForm[ind_Blank]:="_";
IndexForm[ind_BlankSequence]:="_";
IndexForm[ind_BlankNullSequence]:="_";
(* Abstract index *)
IndexForm[ind_Symbol]:=PrintAs[ind];
(* Derivative in Postfix notation *)
IndexForm[ind_,der_]:=First[SymbolOfCovD[der]]<>IndexForm[ind];
(* Projected indices *)
IndexForm[ind_,Null]:=IndexForm[ind];
IndexForm[ind_,Null,{_,delta}]:=IndexForm[ind];
IndexForm[ind_,der_,{_,delta}]:=IndexForm[ind,der];
IndexForm[ind_,Null,{xAct`xTensor`Up,proj_}]:=StringJoin["\!\(",IndexForm[ind],"\&",PrintAsSmaller[proj],"\)"]
IndexForm[ind_,Null,{xAct`xTensor`Down,proj_}]:=StringJoin["\!\(",IndexForm[ind],"\+",PrintAsSmaller[proj],"\)"];
IndexForm[ind_,der_,pr_]:=First[SymbolOfCovD[der]]<>IndexForm[ind,Null,pr];
(* List of indices (with head IndexList) *)
IndexForm[list_IndexList]:=Apply[StringJoin,IndexForm/@list];
(* Slots *)
IndexForm[ind_Slot]:=ToString[ind];
IndexForm[ind_SlotSequence]:=ToString[ind];
(* Any other case *)
IndexForm[x_]:=(Message[IndexForm::nouse,"IndexForm",ToString[x],$WarningFrom];ToString[x]);
SetNumberOfArguments[IndexForm,{1,2}];
Protect[IndexForm];


PrimeDagger[ind_]:=If[DaggerIndexQ@ind,StringJoin[StringDrop[ToString@ind,-StringLength@$DaggerCharacter],"'"],ind];


IndexPair[ind_,other___]:=Block[{$WarningFrom="Index Formatting"},UpIndexPair[ind,UpIndex[ind],other]];
(* Standard index *)
UpIndexPair[upind_,upind_]:={blanks[IndexLength[upind]],IndexForm[upind]};
UpIndexPair[downind_,upind_]:={IndexForm[upind],blanks[IndexLength[upind]]};
(* Derivative index in Postfix notation *)
UpIndexPair[upind_,upind_,der_]:={blanks[IndexLength[upind,der]],IndexForm[upind,der]};
UpIndexPair[downind_,upind_,der_]:={IndexForm[upind,der],blanks[IndexLength[upind,der]]};
(* Projected index *)
$ProjectedFormat=True;
UpIndexPair[upind_,upind_,der_,proj_]:=If[$ProjectedFormat||proj===delta,{blanks[IndexLength[upind,der]],IndexForm[upind,der,{xAct`xTensor`Up,proj}]},{ColorString[PrintAs[proj],GrayLevel[0.6]],IndexForm[upind,der]}];
UpIndexPair[downind_,upind_,der_,proj_]:=If[$ProjectedFormat||proj===delta,{IndexForm[upind,der,{xAct`xTensor`Down,proj}],blanks[IndexLength[upind,der]]},{IndexForm[upind,der],ColorString[PrintAs[proj],GrayLevel[0.6]]}];
(* Printing length of an index *)
IndexLength[upind_BlankSequence]:=1;
IndexLength[upind_BlankNullSequence]:=1;
(* This is inconvenient with indices containing linear syntax, like pullback indices
IndexLength[upind_]:=Length@Characters@ToString@PrintAs@IndexName[upind];*)
IndexLength[upind_]:=1;
IndexLength[upind_,Null]:=IndexLength[upind];
(* This is inconvenient with StyleBox[...]
DerSymbolLength[symbol_String]:=Length@Characters[symbol]*)
DerSymbolLength[symbol_String]:=1;
IndexLength[upind_,der_]:=IndexLength[upind]+DerSymbolLength[First@SymbolOfCovD[der]];
blanks[length_]:=StringJoin@@Table[" ",{length}];
(* New simpler formatting
blanks[__]:=" ";
*)


AddIndex[strings_List,IndexList[a_,b___],der_]:=Fold[AddIndex,AddIndex[strings,a,der],IndexList[b]];


AddIndex[{downstring_String,upstring_String},ind__]:={downstring<>#1,upstring<>#2}&@@IndexPair[ind];


SetAttributes[screenindices,HoldAll];
screenindices[exprs__]:=Union[UpIndex/@DeleteCases[IndexName/@Join@@(FindIndices/@Unevaluated[{exprs}]),0]]


screenrules[indices_IndexList]:=screenrules[indices,Select[indices,DollarQ]];
screenrules[indices_,dollarindices_]:=screenrules[Complement[indices,dollarindices],dollarindices,{}];
screenrules[used_,IndexList[],rules_]:=rules;
getindex[index_,used_]:=First@GetIndicesOfVBundle[VBundleOfIndex[index],1,List@@used];
screenrules[used_,IndexList[index_,indices___],rules_]:=screenrules[used,IndexList[indices],rules,index,getindex[index,used]];screenrules[used_,indices_,rules_,index_,newindex_]:=screenrules[Append[used,newindex],indices,Join[rules,{index->newindex,-index->-newindex}]];


MapOnIsolated[expr_,f_]:=EvaluateHeld[EvaluateHeld[RecursiveIsolated[expr,f],RecursiveIsolated],f];


RecursiveIsolated[expr_,f_]:=Unevaluated[expr]/.{
LieD[v_][x_]:>LieD[RecursiveIsolated[f[v],f]][RecursiveIsolated[x,f]],
Scalar[v_]:>Scalar[RecursiveIsolated[f[v],f]],
Dir[v_]:>Dir[RecursiveIsolated[f[v],f]],
Bracket[v1_,v2_]:>Bracket[RecursiveIsolated[f[v1],f],RecursiveIsolated[f[v2],f]]};


EvaluateHeld[expr_,f_]:=xEvaluateAt[expr,Drop[#,-1]&/@Position[expr,f]];


SetAttributes[ExtractIsolated,HoldFirst];
ExtractIsolated[expr_,f_]:=Join[ExtractIsolated[expr,f,LieD],ExtractIsolated[expr,f,Dir],ExtractIsolated[expr,f,Scalar],ExtractIsolated[expr,f,Bracket]];
next[{list___,0}]:={list,1};
ExtractIsolated[expr_,f_,head:(LieD|Dir|Scalar)]:=Extract[Hold[expr],next/@Position[Hold[expr],head],f];
next2[{list___,0,0}]:=Sequence[{list,0,1},{list,0,2}];
ExtractIsolated[expr_,f_,Bracket]:=Extract[Hold[expr],next2/@Position[Hold[expr],Bracket],f];


SetAttributes[ScreenDollarIndices,Listable]


ScreenDollarIndices[ScreenDollarIndices]:=ScreenDollarIndices;


ScreenDollarIndices[expr_Plus]:=ScreenDollarIndices/@expr;
ScreenDollarIndices[HoldPattern[SeriesData[var_,orig_,coeffs_,rest__]]]:=SeriesData[ScreenDollarIndices[var],ScreenDollarIndices[orig],ScreenDollarIndices/@coeffs,rest];
ScreenDollarIndices[Equal[lhs_,rhs_]]:=Equal[ScreenDollarIndices[lhs],ScreenDollarIndices[rhs]];


ScreenDollarIndices[x_HoldForm]:=x


ScreenDollarIndices[expr_]:=Block[{$WarningFrom="ScreenDollarIndices"},MapOnIsolated[ReplaceIndex[expr,screenrules@screenindices[expr]],ScreenDollarIndices]
];


SetNumberOfArguments[ScreenDollarIndices,1];
Protect[ScreenDollarIndices];


(****************************** 9.Tensors *****************************)


If[$ReadingVerbose,Print["Reading section 9: Tensors."],Null,Null]


checkSetOrthogonal[list_,index_]:=Which[
!AIndexQ[index],Throw@Message[DefTensor::error,"Index of orthogonal vector must be abstract."],
FreeQ[list,ChangeIndex@index],Throw@Message[DefTensor::error,"Index of orthogonal vector cannot be contracted."],
True,Null];


DefTensor::noorth="Tensor `1` could not be made orthogonal to vector `2`.";


(* Orthogonality in all indices of tensor. Assume there is a metric *)
SetOrthogonal[tensor_?xTensorQ[AnyIndices[vbundle_?VBundleQ]],vector_?xTensorQ]:=With[{vbQ=VBundleIndexQ[vbundle],i=DummyIn[vbundle]},
xTagSetDelayed@@Hold[{tensor, tensor[___,i_,___]vector[-i_?vbQ]},0];
xTagSetDelayed@@Hold[{tensor, tensor[___,-i_?vbQ,___]vector[i_]},0];
xUpSet[OrthogonalToVectorQ[vector][tensor],True];
];
SetOrthogonal[tensor_?xTensorQ[inds___],vector_?xTensorQ]:=(Map[SetOrthogonal[tensor[inds],vector[#]]&,Map[ChangeIndex,Select[IndexList[inds],AIndexQ]]];
xUpSet[OrthogonalToVectorQ[vector][tensor],True];
);
(* Orthogonality in a given index *)
SetOrthogonal[tensor_?xTensorQ[inds___],vector_?xTensorQ[index_]]:=If[tensor[inds] vector[index]=!=0,
checkSetOrthogonal[IndexList[inds],index];
AutomaticRules[tensor,MakeRule[{tensor[inds]vector[index],0},MetricOn->All,ContractMetrics->True],Verbose->False];
If[tensor[inds]vector[index]=!=0,Message[DefTensor::noorth,tensor[inds],vector[index]]]
];
SetNumberOfArguments[SetOrthogonal,2];
Protect[SetOrthogonal];


checkSetProjected[list_,IndexList[i1_,i2_]]:=Which[
!AIndexQ[i1]||!AIndexQ[i2],Throw@Message[DefTensor::error,"Indices of projector must be abstract."],
Sort[{Length@TakeEPairs@Append[list,i1],Length@TakeEPairs@Append[list,i2]}]=!={0,2},Throw@Message[DefTensor::error,"One and only one pair of indices must be contracted to project a tensor."],
True,Null];


SetProjected[tensor_?xTensorQ[AnyIndices[vbundle_?VBundleQ]],projector_?xTensorQ]:=With[{vbQ=VBundleIndexQ[vbundle],i=DummyIn[vbundle],j=DummyIn[vbundle]},
xTagSetDelayed@@Hold[{tensor,tensor[left___,i_,right___]projector[-i_,j_]},tensor[left,j,right]];
xTagSetDelayed@@Hold[{tensor,tensor[left___,-i_,right___]projector[i_,j_]},tensor[left,j,right]];
xTagSetDelayed@@Hold[{tensor,tensor[left___,i_,right___]projector[j_,-i_]},tensor[left,j,right]];
xTagSetDelayed@@Hold[{tensor,tensor[left___,-i_,right___]projector[j_,i_]},tensor[left,j,right]];
];
SetProjected[tensor_?xTensorQ[inds__],projector_?xTensorQ]:=SetProjected[tensor[inds],projector[DummyAs[#],ChangeIndex[#]]]&/@Select[IndexList[inds],AIndexQ];
SetProjected[tensor_?xTensorQ[inds__],projector_?xTensorQ[i1_,i2_]]:=Module[{product=tensor[inds]projector[i1,i2],newtensor},
checkSetProjected[IndexList[inds],IndexList[i1,i2]];
If[product===Unevaluated[tensor[inds]projector[i1,i2]]||product===Unevaluated[projector[i1,i2]tensor[inds]],
newtensor=ReplaceIndex[tensor[inds],{-i1->i2,-i2->i1}];
AutomaticRules[tensor,MakeRule[Evaluate@{tensor[inds]projector[i1,i2],newtensor},MetricOn->All,ContractMetrics->True],Verbose->False]]
];
SetNumberOfArguments[SetProjected,2];
Protect[SetProjected];


(* Tensor names. Incompatible with the use of variable-rank tensors *)
OrthogonalToVectorQ[vector_][tensor:(delta|Gdelta|Basis)]:=Throw@Message[DefTensor::nouse,"OrthogonalToVectorQ",tensor];
OrthogonalToVectorQ[vector_][tensor_?xTensorQ]:=xUpSet[
OrthogonalToVectorQ[vector][tensor],
Module[{inds=SlotsOfTensor[tensor]},
If[MemberQ[inds,AnyIndices[_]],
Throw@Message[OrthogonalToVectoQ::error,"Cannot decide orthogonality to the variable-rank tensor "<>ToString[tensor]],
inds =DummyIn/@inds;
OToVcheck[vector,tensor@@inds,Select[inds,EIndexQ]]
]
]
];
(* TensorDerivative with induced derivatives. Contributed by Cyril *)
OrthogonalToVectorQ[vector_][TensorDerivative[tensor_?xTensorQ,ders___,covd_Symbol?InducedCovDQ][inds___,ind_]]:=Last@InducedFrom@MetricOfCovD@covd===vector&&OrthogonalToVectorQ[vector][TensorDerivative[tensor,ders][inds]];
OrthogonalToVectorQ[vector_][TensorDerivative[tensor_?xTensorQ,ders___,LieD[vector_[i_]]][inds___]]:=With[{tensor1=TensorDerivative[tensor,ders]},OrthogonalToVectorQ[vector][tensor1[inds]]&&(IndicesOf[Free,xAct`xTensor`Up][tensor1@@(DummyIn/@SlotsOfTensor[tensor1])]===IndexList[] || LieD[vector[i]][vector[-i]]===0)];
(* General expressions. QUESTION: Why does the general definition look only at the free indices? *)
OrthogonalToVectorQ[vector_][tensor_?xTensorQ[inds___]]:=OrthogonalToVectorQ[vector][tensor];
OrthogonalToVectorQ[vector_][LieD[vector_?xTensorQ[i_]][expr_]]:=OrthogonalToVectorQ[vector][expr]&&(IndicesOf[Free,xAct`xTensor`Up][expr]===IndexList[]||LieD[vector[i]][vector[-i]]===0);
OrthogonalToVectorQ[vector_][expr_]:=With[{uexpr=UxSort[expr]},OToVcheck[vector,uexpr, List@@FindFreeIndices[uexpr]]];
(* Actual check *)
OToVcheck[vector_,expr_,inds_List]:=And@@Map[ContractMetric@GradNormalToExtrinsicK[#]===0&,expr (vector/@ChangeIndex/@Select[inds,EIndexQ])];
(* Variant added by Thomas *)
HasOrthogonalIndexQ[expr_,vector_[ind_]]:=And[MemberQ[FindFreeIndices[expr],-ind,1],OToVcheck[vector,expr,{-ind}]]


Protect[OrthogonalToVectorQ];


Off[RuleDelayed::rhs];
(* PINDEX *)
makepattern[a_Symbol]:=Pattern[a,Blank[]];
makepattern[-a_Symbol]:=makepattern[a];
makepattern[a_]:=a;
On[RuleDelayed::rhs];


baseifbundle[manifold_?ManifoldQ]:=manifold;
baseifbundle[param_Symbol?ParameterQ]:=param;
baseifbundle[vbundle_?VBundleQ]:=BaseOfVBundle[vbundle];
baseifbundle[x_]:=Throw@Message[DefTensor::unknown,"dependency",x];


DefTensor::wrongsym="Symmetry properties are inconsistent with indices of tensor.";
DefTensor::nodummy="Tensor cannot be defined with dummy indices.";
DefTensor::zero="Symmetry makes tensor zero. Use VanishingQ instead.";


DefTensorCheckIndices[indices___]:=
Module[{},
(* Only types A and L are accepted *)
If[Not[AIndexQ[#]||LIndexQ[#]],Throw@Message[DefTensor::invalid,#,"index at definition"]]&/@{indices};
(* If we accept a single index for Dim1 spaces then we need to delete that index from the list in this check *)
If[TakeEPairs@IndexList[indices]=!=IndexList[],Throw@Message[DefTensor::nodummy]]
];


DefTensorChecks[head_[indices___],dependencies_,sym_,{dag_,forcesym_,weight_,grade_,frobeniusQ_,ot_,pw_,anynumberQ_}]:=Module[{alldependencies,manifolds,parameters,vbundles,SGS,tableaux},

(* 1. Validate name *)
ValidateSymbol[head];
ValidateSymbolInSession[head];

(* 2. Check indices and consistency with the vbundles *)
DefTensorCheckIndices[indices];
vbundles=SignedVBundleOfIndex/@{indices};

(* 3. Check dependencies: manifolds and parameters *)
alldependencies=SortDependencies[baseifbundle/@Flatten[{dependencies}]];
manifolds=Select[alldependencies,ManifoldQ];
parameters=Select[alldependencies,ParameterQ];
Throw[Message[DefTensor::unknown,"manifold or parameter",#]]&/@Complement[alldependencies,manifolds,parameters];

(* 4. Check Dagger option and decide whether it is compatible with the indices of the tensor *)
If[FreeQ[{Real,Imaginary,Complex,Conjugate,Hermitian,Antihermitian},dag],Throw@Message[DefTensor::invalid,dag,"value for option Dagger"]];
If[DaggerIndex@IndexList[indices]===IndexList[indices],
If[MemberQ[{Hermitian,Antihermitian},dag],Throw@Message[DefTensor::invalid,dag,"value for Dagger: only real indices"]],
If[MemberQ[{Real,Imaginary},dag],Throw@Message[DefTensor::invalid,dag,"value for Dagger: complex indices"]];
If[MemberQ[{Hermitian,Antihermitian},dag],TransposeDaggerRules[IndexList[indices],$VBundles]]
];

(* 5. Check weight *)
checkweight[head,weight];

(* 6. Check grades *)
checkgrade[grade];

(* If tensor has a variable number of slots we cannot check much more... *)
If[anynumberQ,

(* Print["** DefTensor: No checks on indices for a variable-rank tensor."] *)
Null,

(* 7. Check symmetries *)
{SGS,tableaux}=DefTensorSymmetryChecks[head[indices],sym,forcesym,vbundles];

(* 8. Check orthogonal vectors and projectors *)
If[ot=!={}&&TakeEIndices[{indices}]==={},Throw@Message[DefTensor::error,"A scalar cannot be orthogonal to a vector"]];
Map[If[!xTensorQ[Head[#]],Throw@Message[DefTensor::unknown,"vector",#]]&,ot];
If[pw=!={}&&TakeEIndices[{indices}]==={},
Throw@Message[DefTensor::error,"A scalar cannot be projected"]];
Map[If[!xTensorQ[Head[#]],Throw@Message[DefTensor::unknown,"projector",#]]&,pw];

];

{SortDependencies[alldependencies],vbundles,SGS,tableaux}

];


checkweight[0]:=Null;
checkweight[weight_Plus]:=checkweight/@weight;
checkweight[basis_Symbol?BasisQ]:=Null;
checkweight[n_?ConstantQ basis_]:=checkweight[basis];
checkweight[x_]:=Throw@Message[DefTensor::invalid,x,"weight for tensor"];


checkgrade[0]:=Null;
checkgrade[list_List]:=checkgrade/@list;
checkgrade[prod_?ProductQ->grade_Integer]:=Null;
checkgrade[graderule_]:=Throw@Message[DefTensor::invalid,graderule,"grade specification"];


validsymmetryhead[_GenSet|_StrongGenSet|_xAct`xTableau`Tableau|_Symmetric|_Antisymmetric]:=Null;
validsymmetryhead[list_List]:=validsymmetryhead/@list;
validsymmetryhead[x_]:=Throw@Message[DefTensor::invalid,x,"symmetry specification"];


symslots[StrongGenSet[base_,GS_]]:=symslots[GS];
symslots[Symmetric[list_]]:=list;
symslots[Antisymmetric[list_]]:=list;
symslots[GenSet[]]:={};
symslots[GenSet[cycs__]]:=Union@Flatten[symslots/@{cycs}];
symslots[-xAct`xPerm`Cycles[cycs___]]:={cycs};
symslots[xAct`xPerm`Cycles[cycs___]]:={cycs};
symslots[xAct`xTableau`Tableau[lists___]]:=Union@Flatten[{lists}];
symslots[list_List]:=symslots/@list;


movedpoints[-perm_]:=movedpoints[perm];
movedpoints[xAct`xPerm`Cycles[cycs___]]:=Join[cycs];
movedpoints[perm_]:=movedpoints[TranslatePerm[perm,xAct`xPerm`Cycles]];
support[GS_]:=Apply[Union,movedpoints/@GS]


SGSofsym[GenSet[]]:=StrongGenSet[{},GenSet[]];
SGSofsym[SGS_StrongGenSet]:=SGS;
SGSofsym[sym:(_Symmetric|_Antisymmetric)]:=SchreierSims[{},sym];
SGSofsym[GS_GenSet]:=Check[Catch@SchreierSims[support[GS],GS],Throw@Message[DefTensor::invalid,GS,"symmetry identification"]];
SGSofsym[tab_xAct`xTableau`Tableau]:=xAct`xTableau`StrongGenSetOfTableau[tab];
SGSofsym[x_]:=Throw@Message[DefTensor::unknown,"symmetry",x]


DefTensorSymmetryChecks[head_[indices___],sym_,forcesym_,slots_]:=Module[{rules,numsym,symnumslots,tableaux,SGS,order,group},

(* Check structure of sym argument *)
validsymmetryhead[sym];

(* Transform indices to numeric slots; permutations in Cycles notation *)
rules=Thread@Rule[{indices},Range@Length@{indices}];
numsym=Flatten[{sym}]/.rules/.gs_GenSet:>TranslatePerm[gs,xAct`xPerm`Cycles];

(* Check that different symmetry arguments are disjoint *)
symnumslots=Flatten@symslots[numsym];
If[Union@symnumslots=!=Sort@symnumslots,Throw@Message[DefTensor::error,"Repeated slots in symmetry."]];

(* Select tableaux and construct SGS *)
tableaux=Cases[numsym,_xAct`xTableau`Tableau];
SGS=Apply[JoinSGS,SGSofsym/@numsym];

(* First consistency check only on small groups (up to 6 symmetry-slots) *)
If[Length[symnumslots]<=6,
group=Dimino[GenSet@SGS];
If[Intersection[group,Map[Minus,group]]=!=Group[],Throw@Message[DefTensor::zero]]];

(* Consistency check on all groups *)
If[PermMemberQ[-xAct`xPerm`Cycles[],SGS],Throw@Message[DefTensor::zero]];

(* ForceSymmetries check only on small groups if there are indices with different character or vbundle *)
If[!forcesym,
If[Length@Union@slots[[symnumslots]]>1,
If[(order=OrderOfGroup[SGS])>10000,
Print["  Order of group of symmetry: ",order," > 10000. ForceSymmetry check not performed."],
If[Head[group]===Symbol,group=Dimino[GenSet@SGS]];If[Length@Union[PermuteList[slots,#]&/@List@@group]=!=1,Throw@Message[DefTensor::wrongsym]]
]
]
];

{SGS,tableaux}
];


Options[DefTensor]={
Dagger->Real,
Master->Null,
PrintAs->Identity,
VanishingQ->False,
ForceSymmetries->False,
WeightOfTensor->0,
GradeOfTensor->0,
FrobeniusQ->False,
OrthogonalTo->{},
ProjectedWith->{},
ProtectNewSymbol:>$ProtectNewSymbols,
DefInfo->{"tensor",""},
TensorID->{}
};
DefTensor[list_List,rest___]:=Scan[DefTensor[#,rest]&,list];
(* No symmetry *)
DefTensor[tensor_,dependencies_,options___?OptionQ]:=DefTensor[tensor,dependencies,GenSet[],options];
(* General definition *)
DefTensor[head_[indices___],dependencies_,sym_,options:OptionsPattern[]]:=Catch@Module[{
dag,pa,van,forcesym,weight,grade,frobeniusQ,ot,pw,pns,master,info,tensorID,
anynumberQ,alldependencies,vbundles,uniqueslots,SGS,tableaux},

(* Options *)
{dag,pa,van,forcesym,weight,grade,frobeniusQ,ot,pw,pns,master,info,tensorID}=OptionValue[{Dagger,PrintAs,VanishingQ,ForceSymmetries,WeightOfTensor,GradeOfTensor,FrobeniusQ,OrthogonalTo,ProjectedWith,ProtectNewSymbol,Master,DefInfo,TensorID}];
ot=Flatten[{ot},1];
pw=Flatten[{pw},1];
anynumberQ=MemberQ[{indices},AnyIndices[_?VBundleQ]];
If[van &&anynumberQ,Throw@Message[DefTensor::error,"Cannot define orthogonal tensor with variable rank."]];

(* Checks *)
{alldependencies,vbundles,SGS,tableaux}=DefTensorChecks[head[indices],dependencies,sym,{dag,forcesym,weight,grade,frobeniusQ,ot,pw,anynumberQ}];

(* DefInfo *)
If[Length[info]>2,info=If[dag===Conjugate,info[[{1,3}]],info[[{1,2}]]]];
MakeDefInfo[DefTensor,head[indices],info,van];

MakexTensions[DefTensor,"Beginning",head[indices],dependencies,sym,options];

(* Register tensor symmetries. Not possible for a variable rank tensor *)
If[!anynumberQ,
SymmetryGroupOfTensor[head]^=SGS;
If[tableaux=!={},SymmetryTableauxOfTensor[head]^=tableaux]];

(* Register tensor name *)
AppendToUnevaluated[$Tensors,head];
xTensorQ[head]^=True;

(* Vbundles and dependencies *)
SlotsOfTensor[head]^=vbundles;
DependenciesOfTensor[head]^=alldependencies;

(* Other *)
uniqueslots=Union@DeleteCases[UpIndex/@(vbundles/.AnyIndices->Identity/.PullBackVBundle[vb_,phi_]:>Sequence[vb,phi]),Labels];
SymbolRelations[head,master,DeleteCases[Union[alldependencies,uniqueslots],AnyDependencies]];
If[frobeniusQ,FrobeniusQ[head]^=frobeniusQ];
DefInfo[head]^=info;
TensorID[head]^=tensorID;
SetPrintAs[head,AddDaggerCharacter[PrintAsString[head,pa],dag]];
If[weight=!=0,WeightOfTensor[head]^=weight;PrintAs[head]^=addweight[PrintAs[head],weight]];
If[grade=!=0,
grade=Flatten[{grade}];
With[{prod=#[[1]],int=#[[2]]},TagSet[head,GradeOfTensor[head,prod],int]]&/@grade
];

(* Complex conjugation *)
Switch[dag,
Conjugate,Null,
Real,Dagger[head]^=head,
Imaginary,Dagger[head]^=MultiplyHead[-1,head],
_,
With[{daghead=MakeDaggerSymbol[head]},
SetDaggerPair[head,daghead];
DefTensor[Apply[daghead,DaggerIndex@IndexList[indices]],dependencies,SGS,Dagger->Conjugate,Master->head,options];
With[{prot=Unprotect[daghead]},
Switch[dag,
Hermitian,daghead[inds___]:=TransposeDagger[head[inds]];
HermitianQ[head]^=True,
Antihermitian,daghead[inds___]:=-TransposeDagger[head[inds]];
AntihermitianQ[head]^=True,
Complex,Null
];
Protect[prot];
]
]
];

(* Vanishing and projections *)
If[van,
SetDelayed[Evaluate[makepattern/@head[indices]],0],
Map[SetOrthogonal[head[indices],#]&,ot];
Map[SetProjected[head[indices],#]&,pw]
];

MakexTensions[DefTensor,"End",head[indices],dependencies,sym,options];

(* Protection *)
If[pns,Protect[head]];

];
SetNumberOfArguments[DefTensor,{2,Infinity}];
Protect[DefTensor];


HermitianQ[tensor_]:=False;
AntihermitianQ[tensor_]:=False;
SetNumberOfArguments[HermitianQ,1];
SetNumberOfArguments[AntihermitianQ,1];
Protect[HermitianQ,AntihermitianQ];


addweight[string_String,weight_Plus]:=Fold[addweight,string,List@@weight];
addweight[string_String,n_. basis_Symbol?BasisQ]:=TildeString[string,n,xAct`xCoba`BasisColor[basis]];


SymmetryGroupOfTensor[0]:=StrongGenSet[{1},GenSet[-xAct`xPerm`Cycles[]]];
SymmetryGroupOfTensor[tensor_?xTensorQ[inds___]]:=SymmetryGroupOfTensor[tensor];
SymmetryGroupOfTensor[tensor_?xTensorQ]:=StrongGenSet[{},GenSet[]];
SymmetryGroupOfTensor[tensor_]:=Throw@Message[SymmetryGroupOfTensor::unknown,"tensor",tensor];
SymmetryTableauxOfTensor[tensor_?xTensorQ[inds___]]:=SymmetryTableauxOfTensor[tensor];
SymmetryTableauxOfTensor[tensor_?xTensorQ]:={};
SymmetryTableauxOfTensor[tensor_]:=Throw@Message[SymmetryTableauxOfTensor::unknown,"tensor",tensor];
SlotsOfTensor[tensor_]:=Throw@Message[SlotsOfTensor::unknown,"tensor",tensor];
DependenciesOfTensor[tensor_]:=Throw@Message[DependenciesOfTensor::unknown,"tensor",tensor];
TensorID[tensor_]:={};
FrobeniusQ[tensor_?xTensorQ]:=False;
FrobeniusQ[tensor_]:=Throw@Message[FrobeniusQ::unknown,"tensor",tensor];
WeightOfTensor[tensor_?xTensorQ]:=0;
WeightOfTensor[tensor_]:=Throw@Message[WeightOfTensor::unknown,"tensor",tensor];
tmp={SymmetryGroupOfTensor,SymmetryTableauxOfTensor,SlotsOfTensor,DependenciesOfTensor,TensorID,FrobeniusQ,WeightOfTensor};
SetNumberOfArguments[#,1]&/@tmp;
Protect[Evaluate[tmp]];


UndefTensor[list:{___?xTensorQ}]:=Scan[UndefTensor,list];
UndefTensor[tensor_]:=Catch@With[{servants=ServantsOf[tensor]},
If[!xTensorQ[tensor],Throw@Message[UndefTensor::unknown,"tensor",tensor]];
If[MetricQ[tensor],UndefMetric[tensor];Return[]];
CheckRemoveSymbol[tensor];
MakexTensions[UndefTensor,"Beginning",tensor];
xUpSet[ServantsOf[tensor],{}];
DropFromHosts[tensor];
Undef/@Reverse[servants];
$Tensors=DeleteCases[$Tensors,tensor];
MakexTensions[UndefTensor,"End",tensor];
MakeUndefInfo[UndefTensor,tensor];
RemoveSymbol[tensor];
]
SetNumberOfArguments[UndefTensor,1];
Protect[UndefTensor];


xTensorQ[Zero]^=True;
SlotsOfTensor[Zero]^:=AnyVBundles;
SymmetryGroupOfTensor[Zero]^:=StrongGenSet[{1},GenSet[-xAct`xPerm`Cycles[]]];
Zero[___]:=0;
Protect[Zero];
MultiplyHead[k_,tensor_Symbol][inds___]:=k tensor[inds];
HeadOfTensor[tensor_?xTensorQ[inds___],{inds___}]:=tensor;
HeadOfTensor[k_?ConstantQ tensor_?xTensorQ[inds___],{inds___}]:=MultiplyHead[k,tensor];
HeadOfTensor[0,{___}]:=Zero;
TensorPlus[tensors__][inds___]:=Plus@@Through[{tensors}[inds]];
TensorPlus[][___]:=0;


(* Register delta *)
xTensorQ[delta]^=True;
DependenciesOfTensor[delta]^={};
PrintAs[delta]^="\[Delta]";
SlotsOfTensor[delta]^={-All,All};
MasterOf[delta]^=Symbol;
Dagger[delta]^=delta;


(* DU delta *)
delta[-a_Symbol,a_Symbol]:=DimOfVBundle@VBundleOfIndex[a];
delta/:(f_Symbol/;f===Times)[expr1___,delta[-a_Symbol,b_Symbol],expr2___]:=ReplaceIndex[expr1 expr2,-b->-a]/;IsIndexOf[expr1 expr2,-b,delta];
delta/:(f_Symbol/;f===Times)[expr1___,delta[-a_Symbol,b_Symbol],expr2___]:=ReplaceIndex[expr1 expr2,a->b]/;IsIndexOf[expr1 expr2,a,delta];
delta[a_,b:{_,_Symbol}]:=Basis[a,b];
delta[a:{_,-_Symbol},b_]:=Basis[a,b];
delta[a,b:_?BasisQ[_]]:=Basis[a,b];
delta[a:-_?BasisQ[_],b_]:=Basis[a,b];
delta/:(_?FirstDerQ)[delta[-_Symbol,_Symbol]]:=0;
ParamD[__][delta[-_Symbol,_Symbol]]:=0;


(* UD delta. Use SymmetryOfIndex *)
delta[a_Symbol,-a_Symbol]:=SymmetryOfIndex[a]DimOfVBundle@VBundleOfIndex[a];
delta/:(f_Symbol/;f===Times)[expr1___,delta[a_Symbol,-b_Symbol],expr2___]:=SymmetryOfIndex[a]ReplaceIndex[expr1 expr2,b->a]/;IsIndexOf[expr1 expr2,b,delta];
delta/:(f_Symbol/;f===Times)[expr1___,delta[a_Symbol,-b_Symbol],expr2___]:=SymmetryOfIndex[a]ReplaceIndex[expr1 expr2,-a->-b]/;IsIndexOf[expr1 expr2,-a,delta];
delta[a_,b:{_,-_Symbol}]:=SymmetryOfIndex[a]Basis[b,a];
delta[a:{_,_Symbol},b_]:=SymmetryOfIndex[b]Basis[b,a];
delta[a_,b:-_?BasisQ[_]]:=SymmetryOfIndex[a]Basis[b,a];
delta[a:_?BasisQ[_],b_]:=SymmetryOfIndex[b]Basis[b,a];
delta/:(_?FirstDerQ)[delta[_Symbol,-_Symbol]]:=0;
ParamD[__][delta[_Symbol,-_Symbol]]:=0;


(* Symmetry of delta is symmetry of metric *)
SymmetryGroupOfTensor[delta[a_,b_]]^:=With[{metric=FirstMetricOfVBundle[VBundleOfIndex[a],False]},If[metric===Null,StrongGenSet[{},GenSet[]],SymmetryGroupOfTensor[metric]]];


(* Conversion to metric if both indices are abstract and have same height *)
delta[a_Symbol,b_Symbol]:=With[{metric=FirstMetricOfVBundle[VBundleOfIndex[a],True]},
If[SymmetryOfMetric[metric]=!=1,Message[delta::warning,"delta converted into non-symmetric "<>ToString[metric]]];
metric[a,b]
];
delta[-a_Symbol,-b_Symbol]:=With[{metric=FirstMetricOfVBundle[VBundleOfIndex[a],True]},
If[SymmetryOfMetric[metric]=!=1,Message[delta::warning,"delta converted into non-symmetric "<>ToString[metric]]];
metric[-a,-b]
];
(* Protection *)
SetNumberOfArguments[delta,2];
SetAttributes[delta,{ReadProtected,Protected}];


xUpSet[xTensorQ[Basis],True];


(* Register Gdelta *)
xTensorQ[Gdelta]^=True;
DependenciesOfTensor[Gdelta]^={};
PrintAs[Gdelta]^="\[Delta]";
SlotsOfTensor[Gdelta]^={AnyIndices[All]};
MasterOf[Gdelta]^=Symbol;
Dagger[Gdelta]^=Gdelta;
(* Two basic cases *)
Gdelta[]:=1;
Gdelta[a_,b_]:=delta[a,b];
(* The symmetry is computed from the number of indices *)
SymmetryGroupOfTensor[Gdelta]^:=StrongGenSet[{},GenSet[]];
SymmetryGroupOfTensor[Gdelta[inds__]]^:=Gdeltasym[Length[{inds}]/2,SymmetryOfIndex[First[{inds}],False]];
(* Derivatives *)
Gdelta/:HoldPattern[(_?FirstDerQ)[Gdelta[l:(-_Symbol)..,r:(_Symbol)..]]]:=0/;Length[{l}]===Length[{r}];


Gdeltasym[n_Integer,sym_Integer]:=StrongGenSet[Range[2n],GenSet@@Join[Minus/@xAct`xPerm`Cycles/@Partition[Range[n],2,1],Minus/@xAct`xPerm`Cycles/@Partition[Range[n+1,2n],2,1],If[sym=!=0,{sym^n xAct`xPerm`Cycles@@Transpose[{Range[n],Range[n+1,2n]}]},{}]]];
Gdeltasym[_]:=Throw@Message[Gdelta::error,"Found Gdelta with odd number of arguments."];


Gdelta[a___,-b_Symbol,c___,b_Symbol,d___]:=contractGdelta[{a,c,d},Length[{a,-b,c,b,d}]/2,Length[{a,-b}],Length[{a,-b,c,b}]];
Gdelta[a___,b_Symbol,c___,-b_Symbol,d___]:=SymmetryOfIndex[b]contractGdelta[{a,c,d},Length[{a,b,c,-b,d}]/2,Length[{a,b}],Length[{a,b,c,-b}]];
contractGdelta[list2_,semil_,p1_,p2_]:=Which[
!IntegerQ[semil],Throw@Message[Gdelta::error,"Found Gdelta with odd number of arguments."],
1<=p1<=semil&&1<=p2<=semil,0,
1<=p1-semil<=semil&&1<=p2-semil<=semil,0,
True,(-1)^(p1+p2-semil)(DimOfVBundle[VBundleOfIndex[First[list2]]]-semil+1)Apply[Gdelta,list2]];


Gdelta/:(f_/;f===Times)[expr1___,gdelta_Gdelta,expr2___]:=With[{tinds=ChangeIndex/@Select[FindIndices[Times[expr1,expr2]],ABIndexQ]},
contractGdeltaTimes[IndexList@@gdelta,tinds,Length[gdelta]/2,Expand@Times[expr1,expr2]]/;Intersection[IndexList@@gdelta,tinds]=!=IndexList[]
];


contractGdeltaTimes[ginds_IndexList,tinds_IndexList,semil_Integer,sum_Plus]:=contractGdeltaTimes[ginds,tinds,semil,#]&/@sum;
contractGdeltaTimes[ginds_IndexList,tinds_IndexList,semil_Integer,0]:=0;
contractGdeltaTimes[ginds_IndexList,tinds_IndexList,semil_Integer,expr_]:=Module[{linds,rinds,ablocks,blocks,ctr},
(* Separate two subsets of indices of delta (both antisymmetric blocks) *)
If[!IntegerQ[semil],Throw@Message[Gdelta::error,"Found Gdelta with odd number of arguments."]];
linds=Take[ginds,semil];
rinds=Take[ginds,-semil];
(* Separate the antisymmetric blocks of the expression expr *)
ablocks=AntisymmetricBlocksOf[expr];
(* Find block pairs containing a contraction. Select one of longest length *)
ctr=Flatten[Outer[{#1,#2,Intersection[#1,#2]}&,{linds,rinds},Map[ChangeIndex,ablocks,{2}]],1];
ctr=First@SortBy[ctr,-Length[Last[#]]&];
(* If the result will not contain Gdelta and there is just one contraction then change to ExpandGdelta, which is faster
If[Length[ctr[[1]]]\[LessEqual]2&&Length[ctr[[2]]]\[LessEqual]2&&Length[ctr[[3]]]\[Equal]1,
Return[Expand[ExpandGdelta[Gdelta@@ginds] expr]]
];*)
(* As usual, try to contract first the second half of the indices of delta *)
If[ctr[[1]]===rinds,
contractGdeltaExpansion[Right,linds,rinds,ctr[[3]],expr],
contractGdeltaExpansion[Left,rinds,linds,ctr[[3]],expr]
]
];


AntisymmetricBlocksOf[Gdelta[inds__]]:=IndexList@@@Partition[{inds},Length[{inds}]/2];
AntisymmetricBlocksOf[Times[gdelta_Gdelta,rest__]]:=Join[AntisymmetricBlocksOf[gdelta],AntisymmetricBlocksOf[Times[rest]]];
AntisymmetricBlocksOf[expr_]:=With[{sym=SymmetryOf[expr]},
Extract[IndexList@@(Last/@sym[[3]]),List/@AntisymmetricBlocks[sym[[4]],Length[sym[[3]]]]]
];


AntisymmetricBlocks[sgs_]:=AntisymmetricBlocks[sgs,PermDeg[sgs]];
AntisymmetricBlocks[Antisymmetric[range_],n_]:=Join[{range},List/@Complement[Range[n],range]];
AntisymmetricBlocks[Symmetric[range_],n_]:=List/@Range[n];
AntisymmetricBlocks[sgs_,n_]:=Join[#,List/@Complement[Range[n],Sequence@@#]]&@(Sort/@ConnectedComponents[Graph[UndirectedEdge@@@Identity@@@(Minus/@Select[Minus/@xAct`xPerm`Cycles/@Subsets[Range[n],{2}],PermMemberQ[#,sgs]&])]]);


contractGdeltaExpansion[side_,oinds_,cinds_,ictrs_,expr_]:=With[{crest=DeleteCases[cinds,Alternatives@@ictrs],len=Length[ictrs]},
len!(-1)^(Plus@@Flatten[Position[cinds,Alternatives@@ictrs]]+len (len+1)/2)Total[
With[{orest=DeleteCases[oinds,Alternatives@@#]},
Times[
Signature[Join[#,orest]],
Gdelta@@join[orest,crest,side],
ReplaceIndex[expr,Thread[List@@(ChangeIndex/@ictrs)->List@@#]]
]
]&/@Subsets[oinds,{len}]
]
];
join[list1_,list2_,Right]:=Join[list1,list2];
join[list1_,list2_,Left]:=Join[list2,list1];


ExpandGdelta[expr_]:=expr/.Gdelta[inds__]:>expandGdelta[delta][inds];


expandGdelta[metric_][a_,b_]:=metric[a,b];
expandGdelta[metric_][indices___]:=Module[{semil=Length[{indices}]/2},
If[!IntegerQ[semil],Throw@Message[Gdelta::error,"Found Gdelta with odd number of indices."]];Det@Outer[metric,{indices}[[Range[semil]]],{indices}[[Range[semil+1,2semil]]],1]];


(* General info *)
xTensorQ[Sdelta[sym_]]^:=True;
PrintAs[Sdelta[sym_]]^="\[Delta]";
SlotsOfTensor[Sdelta[sym_]]^:={AnyIndices[All]};
DependenciesOfTensor[Sdelta[sym_]]^:={};
(* Symmetry info *)
Sdelta[sym_GenSet][inds___]:=Sdelta[SchreierSims[{},sym]][inds];
SymmetryGroupOfTensor[Sdelta[sym_][inds___]]^:=With[{n=Length[{inds}]},JoinSGS[sym,sym/.p_Integer?Positive:>p+n/2]/;Or[EvenQ[n],Throw[]]];
(* Other info *)
Dagger[Sdelta[sym_]]^:=Sdelta[sym];
DefInfo[Sdelta[sym_]]^:={"tensor","super delta of adjustable symmetry"};
HostsOf[Sdelta[sym_]]^:={};
TensorID[Sdelta[sym_]]^:={};
Protect[Sdelta];


ExpandSdelta[expr_]:=expr/.Sdelta[sym_][inds___]:>expandSdelta[sym,delta][inds];
oneSterm[metric_,first_List,last_List][-perm_]:=-oneSterm[metric,first,last][perm];
oneSterm[metric_,first_List,last_List][perm_]:=Inner[metric,first,PermuteList[last,perm],Times];
expandSdelta[sym_,metric_][inds___]:=With[{first=Take[{inds},Length[{inds}]/2],last=Take[{inds},-Length[{inds}]/2],grouplist=Dimino[sym]},Apply[Plus,oneSterm[metric,first,last]/@grouplist]/Length[grouplist]];


RemoveSdelta[expr_]:=expr/.Sdelta[_][inds___]:>With[{n=Length[{inds}]/2},Inner[delta,Take[{inds},n],Take[{inds},-n],Times]];


$TensorBoxes="GridBox";


xTensorBox[Projected[tensor_,projs_List],inds_List]:=MakeTensorBoxes[$TensorBoxes][PrintAs[tensor],IndexArray[$TensorBoxes][inds,projs]];


xTensorBox[tensor_,inds_List]:=MakeTensorBoxes[$TensorBoxes][PrintAs[tensor],IndexArray[$TensorBoxes][inds]];


MakeTensorBoxes["ScriptBox"]=MakeTensorScriptBoxes;
MakeTensorBoxes["GridBox"]=MakeTensorGridBoxes;


IndexArray["ScriptBox"]=SSSBinds;
IndexArray["GridBox"]=IndexGrid;


MakeTensorScriptBoxes[stem_String,{downstring_String,upstring_String}]:=SubsuperscriptBox[stem,downstring,upstring];


SSSBinds[inds_List]:=Fold[AddIndex,{"",""},inds];


SSSBinds[inds_List,projs_List]:=Fold[AddIndex[#1,#2[[1]],Null,#2[[2]]]&,{"",""},Transpose[{inds,projs}]];


MakeTensorGridBoxes[stem_,{}]:=stem;


MakeTensorGridBoxes[stem_,indexgrid_List]:=GridBox[{{
stem,
StyleBox[
GridBox[
indexgrid,
RowSpacings->0,ColumnSpacings->0.05,GridFrameMargins->{{0,0},{0,0}}
],FontSize->3/4CurrentValue[FontSize]
]
}},
ColumnSpacings->0.05,RowAlignments->Center
];


IndexGrid[{},projs_]:={};
IndexGrid[{}]:={};
IndexGrid[inds_List,projs_]:=Reverse@Transpose@Inner[IndexPair[#1,Null,#2]&,inds,projs,List];
IndexGrid[inds_List]:=Reverse@Transpose[IndexPair/@inds];


SetAttributes[HeldxTensorQ,HoldAllComplete];
HeldxTensorQ[expr_]:=xTensorQ[Unevaluated[expr]];


xTensorFormStart[Tensor]:=(
MakeBoxes[tensor_?HeldxTensorQ[inds___],StandardForm]:=Block[{$WarningFrom="Tensor Formatting"},interpretbox[tensor[inds],xTensorBox[tensor,{inds}]]]);
xTensorFormStop[Tensor]:=(MakeBoxes[tensor_?HeldxTensorQ[inds___],StandardForm]=.);
xTensorFormStart[Tensor];


SeriesDataMap[f_?FirstDerQ,sd:HoldPattern[SeriesData[var_,orig_,coeffs_List,rest__]]]:=If[f[var]==0,SeriesData[var,orig,Map[f,coeffs],rest],f[var] D[sd,var]];


Dir/:(tensor_?xTensorQ)[x___,Dir[v_Plus],y___]:=Map[tensor[x,Dir[#],y]&,v];
Dir/:(tensor_?xTensorQ)[x___,Dir[v_SeriesData],y___]:=SeriesDataMap[tensor[x,Dir[#],y]&,v];
Dir/:(tensor_?xTensorQ)[x___,Dir[scalar_?ScalarQ v_],y___]:=scalar tensor[x,Dir[v],y];
Dir/:(tensor_?xTensorQ)[___,Dir[0],___]:=0;


SetAttributes[ValidateDir,HoldFirst];
ValidateDir[x:Dir[_Blank|_Pattern|_PatternTest]]:=x;
ValidateDir[x:Dir[_[___,_Blank|_Pattern|_PatternTest,___]]]:=x;
ValidateDir[x:Dir[0]]:=x;
ValidateDir[x:Dir[expr_]]:=Block[{$WarningFrom="Dir Validation"},
(* Check expression *)
UncatchedValidate[expr];
(* Check free ultraindex *)
UltraindexOf[expr];
(* Return index if everything OK *)
x];
ValidateDir[x_]:=Throw[Message[Validate::invalid,x,"directional index"];ERROR[x]];


SeparateDir[expr_]:=expr//.{
tensor:_?xTensorQ[___,dir_Dir,___]:>separateDir[tensor,dir],
covd:_?CovDQ[dir_Dir][_]:>separateDir[covd,dir],
covd:CovD[_,___,_?CovDQ[dir_Dir],dersR___]:>CovD[separateDir[covd,dir],dersR]
};
SeparateDir[expr_,list_List]:=Fold[SeparateDir,expr,list];
SeparateDir[expr_,v_?xTensorQ]:=expr//.{
tensor:_?xTensorQ[___,dir:Dir[_v],___]:>separateDir[tensor,dir],
covd:_?CovDQ[dir:Dir[_v]][_]:>separateDir[covd,dir],
covd:CovD[_,___,_?CovDQ[dir:Dir[_v]],dersR___]:>CovD[separateDir[covd,dir],dersR]
};
SetNumberOfArguments[SeparateDir,{1,2}];
Protect[SeparateDir];
separateDir[expr_,Dir[v_]]:=Module[{ultraindex=UltraindexOf[v],dummy},
dummy=DummyAs[ultraindex];
changeDir[expr,-dummy]ReplaceIndex[v,ultraindex->dummy]];
changeDir[tensor_?xTensorQ[indsL___,_Dir,indsR___],mdummy_]:=tensor[indsL,mdummy,indsR];
changeDir[covd_Symbol[_Dir][expr_],mdummy_]:=covd[mdummy][expr];
changeDir[CovD[expr_,ders___,covd_Symbol[_Dir]],mdummy_]:=CovD[expr,ders,covd[mdummy]];


Options[ContractDir]={OverDerivatives->True};
(* IsIndexOf used without third argument *)
ContractDir[expr_,vector_?xTensorQ,options:OptionsPattern[]]:=expr/.vector[a_?AIndexQ]expr1_:>ContractDir1[vector,OptionValue[OverDerivatives]][expr1 vector[a]]/;IsIndexOf[expr1,-a];
SetNumberOfArguments[ContractDir,2];
Protect[ContractDir];


(* Main *)
(CM:ContractDir1[vector_,od_])[rest_. tensor_?xTensorQ[indsL___,b_,indsR___]vector_[a_]]:=CM[rest tensor[indsL,Dir[vector[a]],indsR]]/;PairQ[a,b];
(CM:ContractDir1[vector_,od_])[rest_. covd_?CovDQ[indsL___,b_,indsR___][expr1_]vector_[a_]]:=CM[rest covd[indsL,Dir[vector[a]],indsR][expr1]]/;PairQ[a,b];
(* First-derivatives *)
(CM:ContractDir1[vector_,od_])[rest_. der_?FirstDerQ[expr_]vector_[a_]]:=
Module[{dv=der[vector[a]],result},
If[(dv===0||od)&&differentexpressionsQ[result=CM[expr vector[a]],{expr,vector[a]}],
CM[rest der[result]]-CM[rest dv expr],
CM[rest vector[a]]der[expr]]
]/;(IsIndexOf[expr,-a]&&Head[expr]=!=vector);
(* Default *)
ContractDir1[vector_,od_][expr_]:=expr;


Validate::pat="Found pattern index `1` . Cannot validate.";
SetAttributes[ValidateTensor,HoldFirst];
ValidateTensor[x:tensor_[inds___]]:=Catch@Block[{$WarningFrom="Tensor Validation"},Module[{newinds,bundles,rightbundles,metricsQ,ups,rightups,strtensor,valid,anynumberQ},
(*** Recognize tensor name ***)
If[!xTensorQ[tensor],Throw[Message[Validate::unknown,"tensor",tensor];ERROR[x]]];
(*** Discard patterns and validate g-indices ***)
If[PIndexQ[#],Message[Validate::pat,#];Return[x]]&/@IndexList[inds];
If[!GIndexQ[#],Throw[Message[Validate::unknown,"index",#];ERROR[x]]]&/@IndexList[inds];
(*** Validate index-structure. Not possible for a variable rank tensor ***)
anynumberQ=MemberQ[SlotsOfTensor[tensor],AnyIndices[_]];
If[!anynumberQ,
(* Definitions *)
rightbundles=UpIndex/@SlotsOfTensor[tensor];
newinds={inds}/.d_Dir:>ChangeIndex@UltraindexOf[d];
bundles=VBundleOfIndex/@newinds;
rightups=UpIndexQ/@SlotsOfTensor[tensor];
ups=UpIndexQ/@newinds;
metricsQ=MetricEndowedQ/@bundles;
(* Validate number of indices *)
If[Length[bundles]=!=Length[rightbundles],Throw[Message[Validate::error,"Invalid number of indices of tensor "<>ToString[tensor]];ERROR[x]]];
(* Validate indices at slots *)
If[Not@Inner[SubvbundleQ[#1,#2]&,rightbundles,bundles,And],Throw[Message[Validate::error,"Invalid index at slot of tensor "<>ToString[tensor]];ERROR[x]]];
(* Validate up/down character of indices *)
If[Not@Inner[Or,metricsQ,Inner[Equal,ups,rightups,List],And],Throw[Message[Validate::error,"Invalid character of index in tensor "<>ToString[tensor]];ERROR[x]]];
(* Validate EIndices are not repeated *)
Check[CheckRepeated[IndexList[inds]],Throw[Print["Sending error"];ERROR[x]]];
];
(* Else, return tensor *)
x
]]


(* One argument *)
WeightOf[_Integer|_Rational|_Real|_Complex]:=0;
WeightOf[_Symbol?ConstantSymbolQ]:=0;
(* Indices assumed not to change weights *)
WeightOf[tensor_?xTensorQ[___]]:=WeightOfTensor[tensor];
(* Derivatives assumed not to change weights *)
WeightOf[_?FirstDerQ[expr_]]:=WeightOf[expr];
WeightOf[ParamD[__][expr_]]:=WeightOf[expr];
WeightOf[expr_Times]:=Plus@@(WeightOf/@List@@expr);
WeightOf[_?ProductQ[exprs___]]:=Plus@@(WeightOf/@{exprs});
WeightOf[expr_Plus]:=Check[JustOne@Union[Expand/@WeightOf/@List@@expr],Throw[Message[WeightOf::error,"Inhomogeneous sum of densities."];ERROR[expr]],Validate::notone];
WeightOf[expr_SeriesData]:=WeightOf[Normal[expr]];
WeightOf[Power[expr_,n_]]:=n WeightOf[expr];
WeightOf[Dagger[expr_]]:=WeightOf[expr];
(* Weight not defined on inertheads and other scalar functions *)
WeightOf[ih_?InertHeadQ[__]]:=Throw[Message[WeightOf::error,"WeightOf is generically undefined on inert heads."]];
WeightOf[sf_?ScalarFunctionQ[x___]]:=If[MemberQ[Attributes[sf],NumericFunction]&&Length[{x}]===1,
If[WeightOf[x]===0,
0,
Throw[Message[WeightOf::error,"A numeric scalar function must have a zero-weight argument."]]
],
Throw[Message[WeightOf::error,"WeightOf is generically undefined on scalar functions."]]
];
WeightOf[Derivative[__Integer][sf_?ScalarFunctionQ][__]]:=Throw[Message[WeightOf::error,"WeightOf is generically undefined on scalar functions."]];
(* Two arguments *)
WeightOf[expr_,Null]:=0;
WeightOf[expr_,basis_]:=Coefficient[WeightOf[expr],basis];
SetNumberOfArguments[WeightOf,{1,2}];
Protect[WeightOf];


checkoneindex[IndexList[a_],f_]:=f[a];
checkoneindex[IndexList[a_],f_,vb_]:=f[a]&&VBundleOfIndex[a]===vb;
checkoneindex[__]:=False;


UpVectorQ[expr_]:=checkoneindex[FindFreeIndices[expr],UpIndexQ];
UpVectorQ[expr_,vbundle_?VBundleQ]:=checkoneindex[FindFreeIndices[expr],UpIndexQ,vbundle];
DownVectorQ[expr_]:=checkoneindex[FindFreeIndices[expr],DownIndexQ];
DownVectorQ[expr_,vbundle_?VBundleQ]:=checkoneindex[FindFreeIndices[expr],DownIndexQ,vbundle];
SetNumberOfArguments[#,{1,2}]&/@{UpVectorQ,DownVectorQ};
Protect[UpVectorQ,DownVectorQ];


(* Separate pairs in a vbundle. Always three arguments *)
TransposeDaggerPairsOneVBundle[inds_IndexList,{vbundle_,vbundledag_},tf_]:=
Module[{rules,
vinds=Select[inds,GIndexQ[#,vbundle]&],
vdinds=Select[inds,GIndexQ[#,vbundledag]&]},

(* Different numbers of indices *)
If[Length[vinds]=!=Length[vdinds],
If[tf,
Throw@Message[TransposeDagger::error3,"Different number of indices of",vbundle,"and its conjugate",vbundledag],
If[Length@vinds>Length@vdinds,
vinds=vinds[[Range@Length@vdinds]],
vdinds=vdinds[[Range@Length@vinds]]
]
]
];

(* Thread over IndexList. Return an IndexList of Pair pairs *)
If[Sort[vinds]===Sort[vdinds],IndexList[],Thread[Pair[vinds,vdinds],IndexList]]
];


(* Detect reordering pairs. One, two or three arguments *)
TransposeDaggerPairs[x_]:=TransposeDaggerPairs[x,$VBundles];
TransposeDaggerPairs[tensor_?xTensorQ[inds___],vbs_,tf_:True]:=TransposeDaggerPairs[IndexList[inds],vbs,tf];
TransposeDaggerPairs[inds_IndexList,vbundle_?VBundleQ,tf_:True]:=TransposeDaggerPairs[inds,{vbundle},tf];
TransposeDaggerPairs[inds_IndexList,vbundles_List,tf_:True]:=
Apply[Join,TransposeDaggerPairsOneVBundle[inds,#,tf]&/@MakeVBundlePairs[vbundles]];

(* List of pairs of complex conjugated vbundles. Trick: VB always sorted before VB\[Dagger] *)
MakeVBundlePairs[vbundles_List]:=Union@Cases[MakeVBundlePair/@vbundles,{_,_}];
MakeVBundlePair[-vbundle_]:=MakeVBundlePair[vbundle];
MakeVBundlePair[vbundle_]:=Union[{vbundle,Dagger[vbundle]}]


(* Construct rules for index reordering. Always two arguments *)
TransposeDaggerRules[x_,vbundles_List]:=Module[{pairs=TransposeDaggerPairs[x,vbundles],rules},
rules=Apply[Rule,pairs,1];
List@@Join[rules,Reverse/@rules]];


(* Main *)
TransposeDagger[expr_]:=TransposeDagger[expr,$VBundles];
(* On tensors. Complicated construction to avoid confusing repeated indices *)
TransposeDagger[expr:tensor_?xTensorQ[inds___],vbs_]:=If[DaggerQ[tensor],
tensor@@TransposeDagger[IndexList[inds]],
expr];
TransposeDagger[inds_IndexList,vbs_]:=With[{dummies=DummyAs/@inds},
dummies/.TransposeDaggerRules[dummies,vbs]/.Inner[Rule,dummies,inds,List]
];
(* Error on any other type of expression *)
TransposeDagger[expr_,vbs_]:=Throw[Message[TransposeDagger::notyet,"TransposeDagger on objects other than tensors"];ERROR[TransposeDagger[expr,vbs]]];
SetNumberOfArguments[TransposeDagger,{1,2}];
Protect[TransposeDagger];


Unprotect[Tensor];
(* Objects with a head Tensor are tensor heads *)
xTensorQ[Tensor[__]]^:=True;
(* Tensor info *)
PrintAs[Tensor[pa_,__]]^:=pa;
SlotsOfTensor[Tensor[pa_,slots_,__]]^:=slots;
DependenciesOfTensor[Tensor[pa_,slots_,deps_,___]]^:=deps;
(* Tensor symmetry info *)
Tensor[pa_,slots_,deps_,other___?OptionQ]:=Tensor[pa,slots,deps,StrongGenSet[{},GenSet[]],other];
Tensor[pa_,slots_,deps_,sym_GenSet,other___]:=Tensor[pa,slots,deps,SchreierSims[{},sym],other];
SymmetryGroupOfTensor[Tensor[args__][inds___]]^:=SymmetryGroupOfTensor[Tensor[args]];
SymmetryGroupOfTensor[Tensor[pa_,slots_,deps_,sym_,___]]^:=sym;
SymmetryGroupOfTensor[Tensor[pa_,slots_,deps_]]^:=StrongGenSet[{},GenSet[]];
(* Other info. TODO: I'm not sure these defaults are the good ones *)
Dagger[tensor:Tensor[_,_,_,_,other___]]^:=Dagger/.CheckOptions[other]/.Dagger->tensor;
DefInfo[Tensor[_,_,_,_,other___]]^:=DefInfo/.CheckOptions[other]/.DefInfo->{"tensor",""};
HostsOf[Tensor[_,_,_,_,other___]]^:=HostsOf/.CheckOptions[other]/.HostsOf->{};
TensorID[Tensor[_,_,_,_,other___]]^:=TensorID/.CheckOptions[other]/.TensorID->{};
(* Protect again *)
Protect[Tensor];


SmallCircle[tensor_?xTensorQ,phis__?MappingQ]:=Precompose[tensor,SmallCircle[phis]];


Precompose[tensor_,_IdentityMapping]:=tensor;
(* Precompose and SmallCircle are essentially the same function, so we keep the order *)
Precompose[Precompose[tensor_,phi1_],phi2_]:=Precompose[tensor,SmallCircle[phi1,phi2]];


Precompose[Null,phi_]:=Null; (* Null is returned by MetricOfX functions *)
Precompose[delta,phi_]:=delta;


(* Essential tensor properties *)
xTensorQ[Precompose[tensor_?xTensorQ,phi_?MappingQ]]^:=True;
SlotsOfTensor[Precompose[tensor_,phi_]]^:=PrecomposeVBundles[SlotsOfTensor[tensor],phi];
DependenciesOfTensor[Precompose[tensor_,phi_]]^:=PrecomposeDependencies[DependenciesOfTensor[tensor],phi];
SymmetryGroupOfTensor[Precompose[tensor_,phi_]]^:=SymmetryGroupOfTensor[tensor];
Dagger[Precompose[tensor_,phi_]]^:=Precompose[Dagger[tensor],Dagger[phi]];
WeightOfTensor[Precompose[tensor_,phi_]]^:=PrecomposeWeight[WeightOfTensor[tensor],phi];


(* Particular case *)
VBundleOfMetric[Precompose[metric_,phi_]]^:=PullBackVBundle[VBundleOfMetric[metric],phi];


(* List of pullback vbundles. Beware there is also PullBackVBundles, which is different *)
PrecomposeVBundles[vbs_List,phi_]:=With[{im=MappingImage[phi]},
If[SubmanifoldQ[BaseOfVBundle[#],im],PullBackVBundle[#,phi],#]&/@vbs
];
(* Pull back dependencies *)
PrecomposeDependency[dep_,phi_]:=If[SubmanifoldQ[dep,MappingImage[phi]],MappingDomain[phi],dep];
PrecomposeDependencies[deps_List,phi_]:=With[{dom=MappingDomain[phi],im=MappingImage[phi]},
If[SubmanifoldQ[#,im],dom,#]&/@deps
];


PrintAs[Precompose[tensor_,phi_]]^:=PrecomposeString[PrintAs[tensor],phi];


SetNumberOfArguments[Precompose,2];
Protect[Precompose];


PrecomposeWeight[sum_Plus,phi_]:=PrecomposeWeight[#,phi]&/@sum;
PrecomposeWeight[prod_Times,phi_]:=PrecomposeWeight[#,phi]&/@prod;
PrecomposeWeight[n:(_Integer|_Rational|_Real|_Complex),phi_]:=n;
PrecomposeWeight[basis_?BasisQ,phi_]:=xAct`xCoba`PrecomposeBasis[basis,phi];


PullBackTensor[tensor_?xTensorQ,_IdentityMapping]:=tensor;
PullBackTensor[delta,phi_?MappingQ]:=delta;


PullBackVBundles[vbs_List,phi_]:=With[{tdom=Tangent[MappingDomain[phi]],tim=Tangent[MappingImage[phi]]},
Which[SameQ[#,tim],tdom,SameQ[#,-tim],-tdom,True,#]&/@vbs
];


xTensorQ[PullBackTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=True;
SlotsOfTensor[PullBackTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=PullBackVBundles[SlotsOfTensor[tensor],phi];(* We could generalize this by using SubmanifoldQ, instead of equality *)
DependenciesOfTensor[PullBackTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=PrecomposeDependencies[DependenciesOfTensor[tensor],phi];
SymmetryGroupOfTensor[PullBackTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=SymmetryGroupOfTensor[tensor];
Dagger[PullBackTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=PullBackTensor[Dagger[tensor],Dagger[phi]];


(* Particular case *)
VBundleOfMetric[PullBackTensor[metric_,phi_]]^:=Tangent[MappingDomain[phi]]/;VBundleOfMetric[metric]===Tangent[MappingImage[phi]];


PrintAs[PullBackTensor[tensor_,phi_]]^:=PullBackString[PrintAs[tensor],phi];


(* TODO: Will the delta have a starred index? *)
LinearPush[tensor_?xTensorQ,delta]:=tensor;


xTensorQ[LinearPush[tensor_?xTensorQ,dphi_?xTensorQ]]^:=True;
SlotsOfTensor[LinearPush[tensor_?xTensorQ,dphi_?xTensorQ]]^:=LinearPushVBundles[SlotsOfTensor[tensor],dphi];
DependenciesOfTensor[LinearPush[tensor_?xTensorQ,dphi_?xTensorQ]]^:=DependenciesOfTensor[tensor];
SymmetryGroupOfTensor[LinearPush[tensor_?xTensorQ,dphi_?xTensorQ]]^:=SymmetryGroupOfTensor[tensor];
Dagger[LinearPush[tensor_?xTensorQ,dphi_?xTensorQ]]^:=LinearPush[Dagger[tensor],Dagger[dphi]];
WeightOfTensor[LinearPush[tensor_?xTensorQ,dphi_?xTensorQ]]^:=Throw@Print["TODO: WeightOfTensor[LinearPush[tensor,dphi]]"];


LinearPush::cova="Cannot push covariant slot `1` with differential `2` because the latter is not invertible.";


(* List of pullback vbundles, by pushing *)
LinearPushVBundles[vbs_List,dphi_]:=With[{tdom=-SlotsOfTensor[dphi][[1]],ptim=SlotsOfTensor[dphi][[2]]},
If[SameQ[tdom,UpIndex[#]],
If[UpIndexQ[#],ptim,
If[InvertibleQ[dphi],-ptim,
Throw@Message[LinearPush::cova,#,dphi]
]
],
#]&/@vbs
];


PrintAs[LinearPush[tensor_,dphi_]]^:=LinearPushString[PrintAs[tensor],dphi];


PushForwardTensor[tensor_?xTensorQ,_IdentityMapping]:=tensor;


xTensorQ[PushForwardTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=True;
SlotsOfTensor[PushForwardTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=PushVBundles[SlotsOfTensor[tensor],phi];
DependenciesOfTensor[PushForwardTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=DependenciesOfTensor[tensor];
SymmetryGroupOfTensor[PushForwardTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=SymmetryGroupOfTensor[tensor];
Dagger[PushForwardTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=PushForwardTensor[Dagger[tensor],Dagger[phi]];
WeightOfTensor[PushForwardTensor[tensor_?xTensorQ,phi_?MappingQ]]^:=PushForwardBasis[WeightOfTensor[tensor],phi];


(* Change tangent bundle of the domain to tangent bundle of the image *)
PushVBundles[vbs_List,phi_]:=With[{tdom=Tangent[MappingDomain[phi]],tim=Tangent[MappingImage[phi]]},
Which[SameQ[tdom,#],tim,SameQ[-tdom,#],-tim,True,#]&/@vbs
];


PushForwardBasis[basisorweight_,phi_]:=Throw@Print["TODO: PushForwardBasis[basis-or-weight, phi]"];


PrintAs[PushForwardTensor[tensor_,phi_]]^:=PushForwardString[PrintAs[tensor],phi];


TangentTensor[phi_Symbol?MappingQ]:=GiveSymbol[TangentTensor,phi];
GiveSymbol[TangentTensor,phi_Symbol?MappingQ]:=SymbolJoin["d",phi];
PrintAsCharacter[TangentTensor]="\[DifferentialD]";
(* Exception to general GiveOutputString to use ToString instead of PrintAs *)
GiveOutputString[TangentTensor,phi_Symbol?MappingQ]:=StringJoin[PrintAsCharacter[TangentTensor],ToString[phi]];


TangentTensor[SmallCircle[phirest__,phi_]][ind1_,ind2_]:=With[{dummy=DummyIn[PullBackVBundle[Tangent[MappingImage[phi]],phi]]},
TangentTensor[phi][ind1,dummy]Precompose[TangentTensor[SmallCircle[phirest]],phi][-dummy,ind2]
];


InvertibleQ[op_?xTensorQ]:=InvertibleQ[op,Left]&&InvertibleQ[op,Right];
InvertibleQ[op_?xTensorQ,Left|Right]:=False;
InvertibleQ[op_,Left|Right]:=Throw@Message[InvertibleQ::unknown,"2-tensor",op];
SetNumberOfArguments[InvertibleQ,{1,2}];
Protect[InvertibleQ];


xTensorQ[BlockTensor[array_]]^:=True;
SlotsOfTensor[BlockTensor[array_]]^:=With[{slotarray=Map[SlotsOfTensor,array,{ArrayDepth[array]}]},
TODO
];


BlockGridBox[vector_?VectorQ]:=GridBox[List/@vector,RowLines->True,ColumnLines->True];
BlockGridBox[matrix_?MatrixQ]:=GridBox[matrix,RowLines->True,ColumnLines->True];
BlockGridBox[array_?ArrayQ]:=BlockGridBox[Map[BlockGridBox,array,{2}]];


PrintAs[BlockTensor[array_]]^:=RowBox[{"(",BlockGridBox[Map[PrintAs,array,{ArrayDepth[array]}]],")"}];


DiagonalArray[diagonal_List,rank_,zero_:Zero]:=Normal@SparseArray[Band@ConstantArray[1,rank]->diagonal,ConstantArray[Length[diagonal],rank],zero];


$Projectors={};


General::nomet="Invalid index configuration of  `1`.";


Options[MakeProjectors]={OrthogonalWithMetric->Null};
MakeProjectors[projectors_,dims_,options:OptionsPattern[]]:=Module[{complementary,heads,vbundle,inds,owm,fm},
{owm}=OptionValue[{OrthogonalWithMetric}];

{heads,vbundle}=CheckMakeProjectors[projectors,dims,owm];
xUpSet[ProjectorQ[#],True]&/@heads;
$Projectors=Join[$Projectors,heads];
If[owm=!=Null,Set[OrthogonalWithMetricQ[#,owm],True]&/@heads];

inds=GetIndicesOfVBundle[vbundle,3];

fm=FirstMetricOfVBundle[vbundle,False];

(* Orthogonal projectors are symmetric *)
If[fm=!=Null&&fm===owm,xUpSet[SymmetryGroupOfTensor[#],Symmetric[{1,2}]]&/@heads];

With[{mu=inds[[1]],nu=inds[[2]],lambda=inds[[3]],metric=fm},

(* Properties of a single projector *)
complementary[head_,head_]:=(
xTagSet[{head,head[mu_,lambda_]head[-lambda_,nu_]},head[mu,nu]];
If[metric===Null,
head[a_Symbol,b_Symbol]:=$Failed/;Message[Validate::nomet,Unevaluated[head[a,b]]];
head[-a_Symbol,-b_Symbol]:=$Failed/;Message[Validate::nomet,Unevaluated[head[-a,-b]]];
head[a_Symbol,-b_Symbol]:=$Failed/;Message[Validate::nomet,Unevaluated[head[a,-b]]];,
xTagSet[{head,head[mu_,-lambda_]head[lambda_,nu_]},head[mu,nu]];
If[metric===owm,
xTagSet[{head,head[lambda_,mu_]head[-lambda_,nu_]},head[mu,nu]];
xTagSet[{head,head[mu_,lambda_]head[nu_,-lambda_]},head[mu,nu]];
xTagSet[{metric,Projected[metric,{head,head}]},head];
]]
);
(* Properties of pairs of projectors *)
complementary[head1_,head2_]:=(
ComplementaryProjectorsQ[head1,head2]=True;
ComplementaryProjectorsQ[head2,head1]=True;
xTagSet[{head1,head1[mu_,lambda_]head2[-lambda_,nu_]},0];
If[fm=!=Null,
xTagSet[{head1,head1[mu_,-lambda_]head2[lambda_,nu_]},0];
If[fm===owm,
xTagSet[{head1,head1[lambda_,mu_]head2[-lambda_,nu_]},0];
xTagSet[{head1,head1[mu_,lambda_]head2[nu_,-lambda_]},0];
xTagSet[{metric,Projected[metric,{head1,head2}]},Zero];
]]
);

Outer[complementary,heads,heads];

(* Set dimensions *)
Inner[xTagSet[{#1,#1[-mu_,mu_]},#2]&,heads,dims,List];
If[metric=!=Null,
Inner[xTagSet[{#1,#1[mu_,-mu_]},#2]&,heads,dims,List]
];

];

];


ComplementaryProjectorsQ[_,_]:=False;
OrthogonalWithMetricQ[_,_]:=False;


CheckMakeProjectors[projectors_,dims_,owm_]:=Module[{heads,vbundle},

(* Check we have a list of at least two elements *)
If[Head[projectors]=!=List,Throw@Message[MakeProjectors::invalid,projectors,"list of projectors"]];
If[Length[projectors]<2,Throw@Message[MakeProjectors::error,"A list of projectors must have at least two elements."]];
(* Check that the heads are tensors *)
heads=Head/@projectors;
If[!xTensorQ[#],Throw@Message[MakeProjectors::invalid,#," is not a tensor."]]&/@heads;
(* Check that the indices belong to the same vbundle. We require down-up objects *)
vbundle=VBundleOfIndex[projectors[[1,2]]];

(* Check we have a list of dimensions of correct length *)
If[Head[dims]=!=List,Throw@Message[MakeProjectors::invalid,dims,"list of dimensions"]];
If[Length[dims]=!=Length[projectors],Throw@Message[MakeProjectors::error,"List of dimensions has wrong length."]];
If[Not@Apply[And,ConstantQ/@dims],Throw@Message[MakeProjectors::invalid,dims,"list of constants"]];
If[Plus@@dims=!=DimOfVBundle[vbundle],Throw@Message[MakeProjectors::error,"Dimensions do not add up to the dimension of the complete vbundle."]];

(* Check metric *)
If[owm=!=Null && !FirstMetricQ[owm,vbundle],Throw@Message[MakeProjectors::invalid,owm,"first metric"]];

(* Return heads and vbundle *)
{heads,vbundle}
];


xTensorQ[Projected[tensor_,projs_]]^:=xTensorQ[tensor];


SlotsOfTensor[Projected[tensor_,projs_]]^:=SlotsOfTensor[tensor];


PrintAs[Projected[tensor_,projs_]]^:=PrintAs[tensor];


Projected[tensor_,{delta...}][inds___]:=tensor[inds];


Projected[_,{___,Zero,___}][___]:=0;


Projected[delta,{proj_,proj_}][a_,b_]:=proj[a,b];
Projected[delta,{delta,proj_}][a_,b_]:=proj[a,b];
Projected[delta,{proj_,delta}][a_,b_]:=proj[a,b];
Projected[delta,{proj1_,proj2_}][a_,b_]:=0/;ComplementaryProjectorsQ[proj1,proj2];


Projected[tensor_,{projsL___,sum_Plus,projsR___}][inds___]:=Projected[tensor,{projsL,#,projsR}][inds]&/@sum;
Projected[tensor_,{projsL___,sum_SeriesData,projsR___}][inds___]:=SeriesDataMap[Projected[tensor,{projsL,#,projsR}][inds]&,sum];


Projected[tensor_,expr:(_Plus|_?ProjectorQ)][inds___]:=Projected[tensor,expr&/@{inds}][inds];


(* up-down contraction *)
(expr:Projected[tensor_,projs_][left___,a_,center___,-a_,right___]):=With[{lproj=projs[[Length[{left,a}]]],rproj=projs[[Length[{left,a,center,-a}]]]},
Which[
ComplementaryProjectorsQ[lproj,rproj],0,
lproj===delta,ReleaseHold@ReplacePart[Hold[expr],rproj,{1,0,2,Length[{left,a}]}],
rproj===delta,ReleaseHold@ReplacePart[Hold[expr],lproj,{1,0,2,Length[{left,a,center,-a}]}]
]/;lproj=!=rproj
];
(* down-up contraction *)
(expr:Projected[tensor_,projs_][left___,-a_,center___,a_,right___]):=With[{lproj=projs[[Length[{left,a}]]],rproj=projs[[Length[{left,a,center,-a}]]]},
Which[
ComplementaryProjectorsQ[lproj,rproj],0,
lproj===delta,ReleaseHold@ReplacePart[Hold[expr],rproj,{1,0,2,Length[{left,-a}]}],
rproj===delta,ReleaseHold@ReplacePart[Hold[expr],lproj,{1,0,2,Length[{left,-a,center,a}]}]
]/;lproj=!=rproj
];


Projected/:(expr1:Projected[tensor1_,projs1_][left1___,a_,___])(expr2:Projected[tensor2_,projs2_][left2___,-a_,___]):=With[{proj1=projs1[[Length[{left1,a}]]],proj2=projs2[[Length[{left2,-a}]]]},
Which[
ComplementaryProjectorsQ[proj1,proj2],0,
proj1===delta,ReleaseHold@ReplacePart[Hold[expr1],proj2,{1,0,2,Length[{left1,a}]}]expr2,
proj2===delta,ReleaseHold@ReplacePart[Hold[expr2],proj1,{1,0,2,Length[{left2,-a}]}]expr1
]/;proj1=!=proj2
];


(* Upper index in the Projected expression *)
Projected/:Projected[tensor_,projs_][left___,a_,right___]projector_?ProjectorQ[-a_,b_]:=With[{proj=projs[[Length[{left,a}]]]},Which[
ComplementaryProjectorsQ[proj,projector],0,
proj===projector,Projected[tensor,projs][left,b,right],
proj===delta,ReplacePart[Projected[tensor,projs][left,b,right],projector,{0,2,Length[{left,a}]}]
]
];
(* Lower index in the Projected expression *)
Projected/:Projected[tensor_,projs_][left___,-b_,right___]projector_?ProjectorQ[-a_,b_]:=With[{proj=projs[[Length[{left,-b}]]]},Which[
ComplementaryProjectorsQ[proj,projector],0,
proj===projector,Projected[tensor,projs][left,-a,right],
proj===delta,ReplacePart[Projected[tensor,projs][left,-a,right],projector,{0,2,Length[{left,-b}]}]
]
];


(* Upper index in the Projected expression *)
Projected/:Projected[tensor_,projs_][left___,a_,right___]projector_?ProjectorQ[b_,-a_]:=With[{proj=projs[[Length[{left,a}]]]},Which[
ComplementaryProjectorsQ[proj,projector],0,
proj===projector,Projected[tensor,projs][left,b,right],
proj===delta,ReplacePart[Projected[tensor,projs][left,b,right],projector,{0,2,Length[{left,a}]}]
]
]/;orthoQ[projector,a];
(* Lower index in the Projected expression *)
Projected/:Projected[tensor_,projs_][left___,-b_,right___]projector_?ProjectorQ[b_,-a_]:=With[{proj=projs[[Length[{left,-b}]]]},Which[
ComplementaryProjectorsQ[proj,projector],0,
proj===projector,Projected[tensor,projs][left,-a,right],
proj===delta,ReplacePart[Projected[tensor,projs][left,-a,right],projector,{0,2,Length[{left,-b}]}]
]
]/;orthoQ[projector,a];


Projected/:(expr1:Projected[tensor1_,projs1_][left1___,a_,right1___])(expr2:tensor2_?xTensorQ[left2___,-a_,right2___]):=expr1 Projected[tensor2,ReplacePart[Table[delta,{Length[expr2]}],projs1[[Length[{left1,a}]]],Length[{left2,-a}]]][left2,-a,right2]/;Head[tensor2]=!=Projected;


ComposeProjectors[proj_,proj_]:=proj;
ComposeProjectors[delta,proj_]:=proj;
ComposeProjectors[proj_,delta]:=proj;
ComposeProjectors[proj1_,proj2_]:=Zero/;ComplementaryProjectorsQ[proj1,proj2];
ComposeProjectors[projs1___,ComposeProjectors[projs2__],projs3___]:=ComposeProjectors[projs1,projs2,projs3];


Projected[Projected[tensor_,projs1_],projs2_]:=Projected[tensor,Inner[ComposeProjectors,projs1,projs2,List]];


Project[expr_,proj_]:=Project[expr,proj,IndicesOf[Free]];
Project[expr_,proj_,f_IndicesOf]:=Project[expr,proj,f[expr]];
Project[expr_,proj_,inds_IndexList]:=Fold[Project[#1,proj,#2]&,expr,inds];
Project[expr_Plus,args__]:=Project[#,args]&/@expr;
Project[expr_SeriesData,args__]:=SeriesDataMap[Project[#,args]&,expr];
Project[expr_,proj_?ProjectorQ,ind_?GIndexQ]:=expr/.tensor_?xTensorQ[left___,ind,right___]:>With[{dummy=DummyAs[ind]},tensor[left,dummy,right]proj@@If[UpIndexQ[ind],{-dummy,ind},{ind,-dummy}]]/;VBundleOfIndex[ind]===SlotsOfTensor[proj][[2]];
Project[expr_,projs:{___?ProjectorQ},ind_?GIndexQ]:=expr/.tensor_?xTensorQ[left___,ind,right___]:>With[{dummy=DummyAs[ind]},tensor[left,dummy,right]Through[(Plus@@projs)@@If[UpIndexQ[ind],{-dummy,ind},{ind,-dummy}]]]/;VBundleOfIndex[ind]===Union[SlotsOfTensor/@projs][[1,2]];


SetNumberOfArguments[Project,{1,3}];
Protect[Project];


Options[ContractProjector]={OverDerivatives->False};
ContractProjector[expr_,projs_List,options___]:=Fold[ContractProjector[#1,#2,options]&,expr,projs];
ContractProjector[expr_,proj_Symbol?ProjectorQ,options:OptionsPattern[]]:=ContractProjector0[OptionValue[OverDerivatives],proj][expr];
ContractProjector[expr_,proj_Symbol,options___]:=Throw@Message[ContractProjector::unknown,"projector",proj];
ContractProjector[expr_,options___?OptionQ]:=ContractProjector[expr,$Projectors,options];
SetNumberOfArguments[ContractProjector,{1,Infinity}];
Protect[ContractProjector];


ContractProjector0[case__][expr_Times]:=ContractProjector1[case][MathInputExpand[expr]];
ContractProjector0[case__][expr_]:=ContractProjector1[case][expr];


(* Automatic threading *)
ContractProjector1[case__][expr_Plus]:=ContractProjector0[case]/@expr;
ContractProjector1[case__][expr_SeriesData]:=SeriesDataMap[ContractProjector0[case],expr];
ContractProjector1[case__][list_List]:=ContractProjector0[case]/@list;
ContractProjector1[case__][eq_Equal]:=ContractProjector0[case]/@eq;


orthoQ[proj_,ind_]:=OrthogonalWithMetricQ[proj,FirstMetricOfVBundle[VBundleOfIndex[ind],False]];
originalQ[tensor_,slot_]:=SignedVBundleOfIndex[tensor[[slot]]]===SlotsOfTensor[Head[tensor]][[slot]];


(* Original projectors: Orthogonal projectors or original tensors *)
(CP:ContractProjector1[od_,proj_])[rest_. proj_[a_,b_] T:(tensor_?xTensorQ[left___,-b_,right___])]:=CP[rest Projected[tensor,ReplacePart[Table[delta,{Length[T]}],proj,Length[{left,-b}]]][left,a,right]]/;orthoQ[proj,b]||originalQ[T,Length[{left,-b}]];
(CP:ContractProjector1[od_,proj_])[rest_. proj_[-a_,b_] T:(tensor_?xTensorQ[left___,a_,right___])]:=CP[rest Projected[tensor,ReplacePart[Table[delta,{Length[T]}],proj,Length[{left,a}]]][left,b,right]]/;orthoQ[proj,a]||originalQ[T,Length[{left,a}]];


(* Non-original projectors: Orthogonal projectors or not original tensors *)
(CP:ContractProjector1[od_,proj_])[rest_. proj_[b_,a_] T:(tensor_?xTensorQ[left___,-b_,right___])]:=CP[rest Projected[tensor,ReplacePart[Table[delta,{Length[T]}],proj,Length[{left,-b}]]][left,a,right]]/;orthoQ[proj,b]|| !originalQ[T,Length[{left,-b}]];
(CP:ContractProjector1[od_,proj_])[rest_. proj_[b_,-a_] T:(tensor_?xTensorQ[left___,a_,right___])]:=CP[rest Projected[tensor,ReplacePart[Table[delta,{Length[T]}],proj,Length[{left,a}]]][left,b,right]]/;orthoQ[proj,a]||!originalQ[T,Length[{left,a}]];


ContractProjector1[_,_][expr_]:=expr;


FromProjected[expr_]:=expr/.Projected[tensor_,projs_List][inds___]:>BreakProjected[tensor,projs][inds];


BreakProjected[tensor_,projs_][inds___]:=
Times[Times@@#1,tensor@@#2]&@@Transpose[ArrangeProjector/@Transpose[{projs,{inds},DummyAs/@{inds}}]];
ArrangeProjector[{proj_,ind_Symbol,dummy_}]:={proj[-dummy,ind],dummy};
ArrangeProjector[{proj_,-ind_Symbol,dummy_}]:={proj[-ind,-dummy],dummy};


SetNumberOfArguments[FromProjected,1];
Protect[FromProjected];


(* Main function *)
SetSplitTensors[vbundle_,spts_List]:=Module[{structure=checksplittensors[vbundle,spts],upspts,downspts,projs},
upspts=Cases[structure,{_Symbol,_}];
downspts=Cases[structure,{-_Symbol,_}];

(* Construct relations among projectors *)
projs=Diagonal@Outer[defproj[vbundle],upspts,downspts,1];

(* Construct relations among ambient projectors *)
Outer[defproj2[vbundle],projs,projs];

];
(* Same subspace *)
defproj[vbundle_][{vb_,upspt_},{-vb_,downspt_}]:=With[{vbundleQ=VBundleIndexQ[vbundle],vbQ=VBundleIndexQ[vb],proj=GiveSymbol[Projector,upspt,downspt]},
Module[{mu,nu},

(* Define inner projector *)
{mu,nu}=GetIndicesOfVBundle[vbundle,2];
DefTensor[proj[-mu,nu],BaseOfVBundle[vbundle]];

(* Products of split-tensors *)
TagSet[downspt,downspt[-b_,mu_]upspt[-mu_?vbundleQ,a_],delta[-b,a]];
TagSet[upspt,upspt[-mu_,a_]downspt[-a_?vbQ,nu_],proj[-mu,nu]];

(* Store split-tensors and projectors in a private function *)
VBundleProjector[vbundle,vb,xAct`xTensor`Down]=downspt;
VBundleProjector[vbundle,vb,xAct`xTensor`Up]=upspt;
VBundleProjector[vbundle,vb,Inner]=proj;

(* Return inner projector *)
proj
]
];
VBundleProjector[vb_,vb_,Inner]:=delta;
VBundleProjector[vbundle_,vb_,_]:=Throw@Message[SplitTensor::error,"Unknown projector from "<>ToString[vbundle]<>" to "<>ToString[vb]<"."];
(* Different subspaces *)
defproj[vbundle_][{_,upspt_},{-_,downspt_}]:=With[{vbundleQ=VBundleIndexQ[vbundle]},
TagSet[downspt,downspt[-b_,mu_]upspt[-mu_?vbundleQ,a_],0]
];
(* Ambient projectors: same subspace *)
defproj2[vbundle_][proj_,proj_]:=With[{vbundleQ=VBundleIndexQ[vbundle]},
TagSet[proj,proj[-mu_,lambda_]proj[-lambda_?vbundleQ,nu_],proj[-mu,nu]]];
(* Ambient projectors: different subspace *)
defproj2[vbundle_][proj1_,proj2_]:=With[{vbundleQ=VBundleIndexQ[vbundle]},
TagSet[proj1,proj1[-mu_,lambda_]proj2[-lambda_?vbundleQ,nu_],0];
TagSet[proj2,proj2[-mu_,lambda_]proj1[-lambda_?vbundleQ,nu_],0];
];


GiveSymbol[Projector,Y_,X_]:=SymbolJoin[Y,X];


checksplittensor[vbundle_][spt_[-a_Symbol,b_Symbol]]:=With[{vba=VBundleOfIndex[a],vbb=VBundleOfIndex[b]},
Which[
!xTensorQ[spt],Throw@Message[DefSplitTensors::unknown,"tensor",spt],
vba===vbundle&&SubvbundleQ[vbundle,vbb],{vbb,spt},
vbb===vbundle&&SubvbundleQ[vbundle,vba],{-vba,spt},
True,Throw@Message[DefSplitTensors::invalid,"split-tensor",spt[-a,b]]
]
];
checksplittensor[_][spt_]:=Throw@Message[DefSplitTensors::invalid,spt,"split-tensor"];
checksplittensors[vbundle_,spts_List]:=Module[{subvbs,structure},
If[!VBundleQ[vbundle],Throw@Message[DefProjection::unknown,"vbundle",vbundle]];
If[(subvbs=SplittingsOfVBundle[vbundle])==={},Throw@Message[DefProjection::error,"VBundle has not yet been decomposed."],
subvbs=Last[subvbs]
];
structure=checksplittensor[vbundle]/@spts;
If[Sort[First/@structure]=!=Sort@Join[subvbs,-subvbs],Throw@Message[DefProjection::error,"Invalid subvbundles."]];
structure
];


SplitTensor[tensor_?xTensorQ[]]:=tensor[];
SplitTensor[tensor_?xTensorQ[indices__]]:=Total[Outer[SplitTensor1[tensor],Sequence@@(SplitTensorIndex[#,VBundleOfIndex[#]]&/@{indices}),1],3];
SplitTensor1[tensor_][trios__List]:=Times[ProjectTensor[tensor,#2]@@#1,Sequence@@#3]&@@Transpose[{trios}];


SplitTensorIndex[a_,Labels]:={{a,LI,1}};
SplitTensorIndex[a_,vbundle_]:=With[{vbs=SplittingsOfVBundle[vbundle]},If[vbs==={},{{a,delta,1}},vbs=Last[vbs];projectindex[a,vbundle,#]&/@vbs]];
projectindex[a_Symbol,vbundle_,vb_]:=With[{dummy=DummyIn[vb]},{dummy,VBundleProjector[vbundle,vb,xAct`xTensor`Up],VBundleProjector[vbundle,vb,xAct`xTensor`Down][-dummy,a]}];
projectindex[-a_Symbol,vbundle_,vb_]:=With[{dummy=DummyIn[vb]},{-dummy,VBundleProjector[vbundle,vb,xAct`xTensor`Down],VBundleProjector[vbundle,vb,xAct`xTensor`Up][-a,dummy]}];


ProjectTensor[tensor_,projs_List][inds__]:=With[{symbol=SymbolJoin[tensor,Sequence@@projs]},If[!xTensorQ[symbol],
DefTensor[symbol[inds],DependenciesOf[tensor],PrintAs:>StringJoin["\!\((",PrintAs[tensor],"\_",StringJoin@@(PrintAs/@projs),")\)"]]
];
symbol[inds]
];


DefRigging[Null,_]:=Null;
DefRigging[rigging_Symbol,vbundle_]:=With[{
base=BaseOfVBundle[vbundle],
Uname=SymbolJoin["U",rigging],
Dname=SymbolJoin["D",rigging]},
Module[{i1,i2,i3},
{i1,i2,i3}=GetIndicesOfVBundle[vbundle,3];
DefTensor[Uname[-i1,i2],base,Master->vbundle];
DefTensor[Dname[i1,-i2],base,Master->vbundle];
DefTensor[rigging[LI[vbundle],i1,-i2],base,Master->vbundle];
TagSetDelayed[Uname,Uname[-i1_,i3_]Dname[i2_,-i3_],If[VBundleOfIndex[i1]===VBundleOfIndex[i2],delta[-i1,i2],0]];
TagSetDelayed[Uname,Uname[-i3_,i1_]Dname[i3_,-i2_],rigging[LI[VBundleOfIndex[i3]],i1,-i2]];
TagSetDelayed[rigging,rigging[LI[vb1_],i1_,-i3_]rigging[LI[vb2_],i3_,-i2_],If[vb1===vb2,rigging[LI[vb1],i1,-i2],0]];
]];


(****************************** 10.Rules ******************************)


If[$ReadingVerbose,Print["Reading section 10: Rules."],Null,Null]


(* Identity *)
PermuteIndices[expr_,indices_IndexList,ID]:=expr;
PermuteIndices[expr_,indices_IndexList,_[]]:=expr;
PermuteIndices[expr_,IndexList[],_]:=expr;
(* Linear combination of permutations *)
PermuteIndices[expr_,indices_IndexList,perm_Plus]:=PermuteIndices[expr,indices,#]&/@perm;
PermuteIndices[expr_,indices_IndexList,k_ perm_?PermQ]:=k PermuteIndices[expr,indices,perm];
(* Generic permutation *)
PermuteIndices[expr_,indices_IndexList,perm_?PermQ]:=Module[{
iperm=InversePerm[perm],
points=Complement[Range@PermDeg@perm,StablePoints@perm]},
ReplaceIndex[expr,Inner[Rule,indices[[points]],indices[[OnPoints[points,iperm]]],List]]];


symperms[inds_IndexList,syminds_IndexList]:=Last@SGSofsym@Symmetric@Flatten[symperms1[inds,Union@syminds]];
symperms1[inds_,IndexList[]]:={};
symperms1[inds_,IndexList[a_,other___]]:=With[{pos=IndexPosition[inds,a]},Prepend[symperms1[ReplacePart[inds,Null,pos],IndexList[other]],pos]];


(* Corner case added, as suggested by Thomas *)
SymmetryPerms[GenSet[],inds_IndexList,subsets___IndexList]:={ID};
SymmetryPerms[GS_,inds_IndexList,subsets___IndexList]:=Module[{g,biggroup,smallgroup,coset,result={}},
biggroup=Dimino[TranslatePerm[GS,xAct`xPerm`Cycles]];
smallgroup=Dimino[GenSet@@Union[symperms[inds,#]&/@{subsets}]];
If[Length[smallgroup]>1,
While[Length[biggroup]>0,
g=First@biggroup;
AppendTo[result,g];
coset=PermProduct[#,g]&/@smallgroup;
biggroup=DeleteCases[biggroup,Alternatives@@Join[coset,PermProduct[-ID,#]&/@coset]]
],
result=List@@biggroup];
result];


SymmetryEquivalentsOfTensor[head_?xTensorQ[inds__],subsets___IndexList]:=PermuteIndices[head[inds],IndexList[inds],#]&/@SymmetryPerms[SymmetryGroupOfTensor[head[inds]],IndexList[inds],subsets];


SymmetryEquivalentsOf[expr_,subsets___IndexList]:=With[{inds=FindIndices[expr]},PermuteIndices[expr,inds,#]&/@SymmetryPerms[Last@SymmetryOf[expr],inds,subsets]];


SetAttributes[ConflictingDummies,{HoldAll,SequenceHold}];
(* PINDEX *)
ConflictingDummies[lhs_,rhs_]:=Intersection[PatternToIndex/@Select[FindDummyIndices[lhs],PIndexQ],FindDummyIndices[rhs]];
SetAttributes[ReplaceConflictingDummies,{HoldFirst,SequenceHold}];
ReplaceConflictingDummies[{lhs_,rhs_}]:=ReleaseHold@ReplacePart[Hold[ReplaceConflictingDummies[rhs,ConflictingDummies[lhs,rhs]]],ReplaceIsolated[Hold[rhs]],{1,1},1];
ReplaceConflictingDummies[rhs_,IndexList[]]:=Hold[rhs];
ReplaceConflictingDummies[rhs_,cdummies_IndexList]:=ReplaceIndex[rhs,DuplicateRule@Inner[Rule,cdummies,DummyAs/@cdummies,List],Hold];


SetAttributes[{ReplaceIsolated,replall},HoldFirst];
ReplaceIsolated[expr_]:=MapOnIsolated[expr,replall];
replall[expr_]:=Module[{inds=FindFreeAndDummyIndices[expr],frees,dummies},
frees=Rule[#,DummyAs[#]]&/@First[inds];
dummies=DuplicateRule/@(Rule[#,DummyAs[#]]&/@Last[inds]);
ReplaceIndex[expr,List@@Join[frees,dummies]]];


DropIndexBasis[{index_,basis_}]:=index;
DropIndexBasis[index_]:=index;
ConstructModule[Hold[rhs_]]:=ReplacePart[Hold[Module[{},rhs]],DropIndexBasis/@(List@@FindDummyIndices[rhs]),{1,1}];


SetAttributes[IndexSetDelayed,{HoldAll,SequenceHold}];
IndexSetDelayed[lhs_,rhs_]:=ReleaseHold@ReplacePart[Hold[SetDelayed[lhs,rhs]],ConstructModule[ReplaceConflictingDummies[{lhs,rhs}]],{1,2},{1}]


SetAttributes[IndexSet,{HoldFirst,SequenceHold}];
IndexSet[lhs_,rhs_]:=(IndexSetDelayed[lhs,rhs];rhs);


SetAttributes[IndexRuleDelayed,{HoldRest,SequenceHold}];
IndexRuleDelayed[lhs_,rhs_]:=ReleaseHold@ReplacePart[Hold[RuleDelayed[HoldPattern[lhs],rhs]],ConstructModule[ReplaceConflictingDummies[{lhs,rhs}]],{1,2},{1}];


SetAttributes[IndexRule,SequenceHold];
IndexRule[lhs_,rhs_]:=IndexRuleDelayed[lhs,rhs];


DoubleRightTee=IndexSet;
RightTeeArrow=IndexRule;


MakeRule::terms="There is more than one term on the LHS of rule.";
MakeRule::inhom="Inhomogeneous indices in rule: `1`, `2`.";
MakeRule::pats="Patterns are not accepted on the `1` hand side of rule.";


(* A non-pattern index is just kept (possibly changing name) *)
FormatIndexRule[{index_Symbol,_,_,_,False,name_Symbol}]:=DuplicateRule[index->name];
(* Non-tested pattern *)
FormatIndexRule[{index_Symbol,_,_,False,True,name_Symbol}]:=DuplicateRule[index->pattern[name,Blank[Symbol]]];
(* Tested pattern. Free. Metric. DuplicateRule not needed *)
FormatIndexRule[{index_Symbol,True,True,True,True,name_Symbol}]:=index->patternTest[pattern[name,Blank[]],Evaluate@VBundleIndexPMQ[VBundleOfIndex@index]];
(* Tested pattern. Dummy, or free+nometric *)
FormatIndexRule[{index_Symbol,_,_,True,True,name_Symbol}]:=DuplicateRule[index->patternTest[pattern[name,Blank[Symbol]],VBundleIndexQ[VBundleOfIndex@index]]];


(* We always get up \[Rule] up rules *)
FormatIndexRule[{-index_,f_,m_,t_,p_,-name_Symbol}]:=FormatIndexRule[{index,f,m,t,p,name}];
(* Blocked indices get name 0. Throw error *)
FormatIndexRule[{0,other__}]:=Throw@Message[MakeRule::nouse,"FormatIndexRule","a blocked index"];
(* Basis indices converted to basis indices *)
FormatIndexRule[{index_,other__}]:=FormatIndexRule[{IndexName[index],other}];


HoldTimes[Hold[0],Hold[expr2_]]:=Hold[0];
HoldTimes[Hold[expr1_],Hold[expr2_]]:=Hold[Times[expr1,expr2]];
HoldTimes[Hold[expr1_],1]:=Hold[expr1];
HoldTimes[Hold[expr1_],expr2_]:=Hold[Times[expr1, expr2]];
(* SPINOR. free and dummy do not always coincide in character; hence use Hold *)
putmetric[expr_Hold,metric_,{free_,dummy_}]:=HoldTimes[expr,If[UpIndexQ[dummy],Hold[metric[free,dummy]],Hold[metric[dummy,free]]]];


duplicateRuleforfree[HoldPattern[LHS_Hold->RHS_Hold],index_]:={LHS->RHS,ReplaceIndex[LHS,index->-index]-> With[{dummy=DummyAs[index]},putmetric[ReplaceIndex[RHS,index->dummy],FirstMetricOfVBundle[VBundleOfIndex[index],True],{-index,-dummy}]]};
duplicateRuleforfree[list_List,index_]:=Flatten[duplicateRuleforfree[#,index]&/@list]


duplicateRulefordummy[HoldPattern[LHS_Hold->RHS_Hold],index_]:=With[{sign=SymmetryOfMetric@FirstMetricOfVBundle[VBundleOfIndex[index],True]},{LHS->RHS,ReplaceIndex[LHS,{index->-index,-index->index}]->HoldTimes[RHS,sign]}];
duplicateRulefordummy[list_List,index_]:=Flatten[duplicateRulefordummy[#,index]&/@list]


changeRuleforfreepmQ[HoldPattern[LHS_Hold->RHS_Hold],index_]:=ReplaceIndex[LHS,index->UpIndex[index]]->With[{dummy=DummyAs[index]},putmetric[ReplaceIndex[RHS,index->dummy],FirstMetricOfVBundle[VBundleOfIndex[index],True],{UpIndex[index],-dummy}]];
changeRuleforfreepmQ[list_List,index_]:=Flatten[changeRuleforfreepmQ[#,index]&/@list];


FlattenTimes[Rule[Hold[Times[-1,x_]],Hold[rhs_]]]:=Rule[Hold[x],Hold[-rhs]];
FlattenTimes[Rule[Hold[Times[x___,Times[-1,y__],z___]],Hold[rhs_]]]:=FlattenTimes[Rule[Hold[Times[x,y,z]],Hold[-rhs]]];
FlattenTimes[Rule[lhs_Hold,rhs_Hold]]:=Rule[lhs,rhs];


addconditions[rule_RuleDelayed,conds_]:=If[conds===Hold[],rule,rule/.RuleDelayed[lhs_,rhs_]:>RuleDelayed[lhs,Condition[rhs,conds]]]


ConstructLHSinfo[indsLHS_List,freesLHS_List,{mo_,ti_,patindsLHS_}]:=
Module[{LHSinfo,tmp},

(* 1. Name of index in input rule *)
LHSinfo=Partition[indsLHS,1];

(* 2. Free character *)
LHSinfo=Append[#,MemberQ[freesLHS,First@#]]&/@LHSinfo;

(* 3. MetricOn. up/down character in mo is corrected *)
tmp=Switch[mo,
None,{},
All,indsLHS,
_List,Intersection[mo~Join~(ChangeIndex/@mo),indsLHS],
_,Throw@Message[MakeRule::unknown,"indices",mo]
];
LHSinfo=Append[#,MemberQ[tmp,First@#]&&MetricEndowedQ@VBundleOfIndex@First@#]&/@LHSinfo;

(* 4. TestIndices *)
LHSinfo=Append[#,ti]&/@LHSinfo;

(* 5. Pattern character in final rule. up/down character in patindsLHS is corrected *)
tmp=Switch[patindsLHS,
None,{},
All,indsLHS,
_List,Intersection[patindsLHS~Join~(ChangeIndex/@patindsLHS),indsLHS],
_,Throw@Message[MakeRule::unknown,"indices",patindsLHS]
];
LHSinfo=Append[#,MemberQ[tmp,First@#]]&/@LHSinfo;

(* 6. Name in final rule. Currently we do not change names *)
LHSinfo=Append[#,First@#]&/@LHSinfo;

(* 7. Return LHSinfo *)
LHSinfo
];


Options[MakeRule]={PatternIndices->All,TestIndices->True,MetricOn->None,UseSymmetries->True,Verbose->False,ContractMetrics->False,Evaluate->False};
SetAttributes[MakeRule,HoldFirst];
MakeRule[{LHS_,RHS_,conditions___},options:OptionsPattern[]]:=
With[{HeldLHS=ReplaceIsolated[Hold[LHS]],HeldRHS=ReplaceDummies[Hold[RHS]]},
Module[{
(* Contractible indices *)
indsLHS=List@@TakeEIndices[FindIndices[LHS]],
freesLHS=List@@TakeEIndices[FindFreeIndices[LHS]],
indsRHS=List@@TakeEIndices[FindIndices[RHS]],
freesRHS=List@@TakeEIndices[FindFreeIndices[RHS]],
isolated=ExtractIsolated[HeldLHS,Hold],
patindsLHS,ti,mo,us,verb,contr,ev,
dupfreemetric,pmQinds,pairs,rulesLHS,
symtensorsLHS,
newLHSs,
rules,conds},

(* Options *)
{patindsLHS,ti,mo,us,verb,contr,ev}=OptionValue[{PatternIndices,TestIndices,MetricOn,UseSymmetries,Verbose,ContractMetrics,Evaluate}];

(**** A. Checks ****)
If[Or@@(PIndexQ/@indsLHS),Throw@Message[MakeRule::pats,"left"]];
If[Or@@(PIndexQ/@indsRHS),Throw@Message[MakeRule::pats,"right"]];
If[LHS===0,Throw@Message[MakeRule::error,"LHS of rule evaluates to zero."]];
If[(Sort@freesLHS)=!=(Sort@freesRHS)&&RHS=!=0,Throw@Message[MakeRule::inhom,freesLHS,freesRHS]];
If[Length@ListOfTerms[LHS]>1,Throw@Message[MakeRule::terms]];
If[mo=!=None&&careQ[Identify[LHS]],Print["** MakeRule: Potential problems moving indices on the LHS."]];

(**** B. Construct and interpret LHSinfo ****)
With[{
LHSinfo=ConstructLHSinfo[IndexName/@indsLHS,IndexName/@freesLHS,{mo,ti,patindsLHS}]},
If[verb,Print["LHSinfo: ",LHSinfo]];
(* Index replacement rules *)
rulesLHS=(FormatIndexRule/@LHSinfo)/.{patternTest->PatternTest,pattern->Pattern};
If[verb,Print["rulesLHS: ",rulesLHS]];
(* Indices that give pmQ patterns (always converted to upindices) *)
pmQinds=Cases[LHSinfo,{index_,True,True,True,True,_}->index];
(* Pairs of contracted indices. They do not need duplication by symmetry *)
pairs=Partition[List@@TakeEPairs[IndexList@@Cases[LHSinfo,{index_,False,True,_,_,_}->index]],2];
(* Free indices that give duplicatefree rules *)
dupfreemetric=Complement[Cases[LHSinfo,{index_,True,True,_,_,_}->index],pmQinds];
];

(**** C. Symmetries on the LHS ****)
(* Detect non-isolated tensors with symmetries *)
symtensorsLHS=Cases[Complement[FindAllOfType[HeldLHS,Tensor],FindAllOfType[isolated,Tensor]],tensor_/;OrderOfGroup[SymmetryGroupOfTensor[tensor]]=!=1];
If[verb,Print["symtensorsLHS: ",symtensorsLHS]];
If[!us||symtensorsLHS==={},
newLHSs={HeldLHS},
(* Create a list of equivalents to symtensorsLHS due to the symmetries of tensors, with metric pairs eliminated because they will give duplicated rules below *)
Module[{groups,newtensors,tmp,equivalents},
groups=Join[{IndexList@@freesLHS},Apply[IndexList,pairs,{1}]];
newtensors=SymmetryEquivalentsOfTensor[#,Sequence@@groups]&/@symtensorsLHS;
If[verb,Print["newtensors: ",newtensors]];
equivalents=Flatten[Outer[tmp,Sequence@@newtensors]]/.tmp->List;
If[verb,Print["equivalents: ",equivalents]];
(* The strange Rule[#2,#1]& construction below is needed because Inner is asymmetric when the lists have different depths *)
newLHSs=HeldLHS/.Inner[Rule[#2,#1]&,equivalents,symtensorsLHS,List]//.HoldPattern[der_?FirstDerQ[-expr_]]->-der[expr];
]
];
If[verb,Print["newLHS after symmetry handling: ",newLHSs]];
(* Isolated expressions are handled through a recursive call to MakeRule *)
Module[{isolated2,isorules},
If[verb,Print["isolated: ",isolated]];
If[isolated=!={},
Block[{FlattenTimes=Identity},
isolated2=Transpose[Flatten[Apply[Outer[IndexList,##]&,patternisolated[#,options]&/@isolated]]/.IndexList->List]
];
If[verb,Print["isolated2: ",isolated2]];
isorules=Inner[makerule,isolated,isolated2,List];
If[verb,Print["isorules: ",isorules]];
newLHSs=Flatten[newLHSs/.isorules];
]
];
If[verb,Print["newLHSs after handling of isolated expressions: ",newLHSs]];

(**** D. Construct rules ****)
(* 1. Duplication *)
rules=Inner[Rule,newLHSs,HeldRHS&/@newLHSs,List];
(* Optional evaluation at this point only *)
If[ev,rules=Map[Evaluate,rules,{3}]];
If[verb,Print["Initial rules: ",InputForm[rules]]];
rules=rules/.Rule[lhs_Hold,rhs_Hold]:>FlattenTimes[Rule[lhs,rhs]];
If[verb,Print["rules after arranging minus signs: ",rules]];
rules=Fold[changeRuleforfreepmQ,rules,pmQinds];
If[verb,Print["rules after changRuleforfreepmQ: ",InputForm[rules]]];
rules=Fold[duplicateRuleforfree,rules,dupfreemetric];
If[verb,Print["rules after duplicateRuleforfree: ",InputForm[rules]]];
pairs=Union[UpIndex/@Flatten[pairs,1]];
rules=Fold[duplicateRulefordummy,rules,pairs];
If[verb,Print["rules after duplication: ",InputForm[rules]]];
If[contr,rules=rules/.Rule[lhs_Hold,Hold[rhs_]]:>Rule[lhs,With[{contrrhs=ContractMetric[rhs]},Hold[contrrhs]]];
If[verb,Print["rules after metric contraction: ",rules]]];

(* 2. Replace indices *)
rules=rules/.Rule[lhs_Hold,rhs_Hold]:>Rule[ReplaceIndex[lhs,rulesLHS],rhs];
If[verb,Print["rules after introducing patterns: ",rules]];

(* 3. Add Module on the RHS *)
rules=rules/.Rule->IndexRule;

(* 4. Add conditions *)
conds=Hold[conditions];
If[verb,Print["Conditions: ",conds]];
rules=(addconditions[#,conds]&/@rules)/.Hold[cond_]->cond;

(* 5. Return rules *)
rules
]
];


patternisolated[Hold[expr_],options___]:=First/@MakeRule[{expr,expr},options];
makerule[Hold[lhs_],rhs_HoldPattern]:=ReplacePart[RuleDelayed[HoldPattern[lhs],rhs],rhs,2,1];


Options[ToRule]=Options[MakeRule];
ToRule[expr:(_Equal|_Rule|_RuleDelayed),options___]:=With[{list=List@@expr},MakeRule[list,options]];
ToRule[{}]:={};
ToRule[list:{(_Equal|_Rule|_RuleDelayed|_List)..},options___]:=Join@@(ToRule[#,options]&/@list);
ToRule[{lhs_,rhs_,cond___},options___]:=MakeRule[{lhs,rhs,cond},options];


minimumdepth[expr_,symbol_]:=Min[Length/@Cases[Position[expr/.HoldPattern->Identity,symbol],{1,x___,0}->{x}]]


SetAttributes[AutomaticRules,HoldFirst];
Options[AutomaticRules]={Verbose->True};
AutomaticRules[symbol_Symbol,rules_List,options:OptionsPattern[]]:=Module[{pos=minimumdepth[#,symbol]&/@rules,
upvs,downvs,othervs,
verbose=OptionValue[Verbose]},
(* Select downvalues and upvalues *)
upvs=Flatten[Position[pos,1]];
downvs=Flatten[Position[pos,0]];
othervs=Complement[Range[Length[rules]],upvs,downvs];
If[Length[downvs]>0,AppendTo[DownValues[symbol],#]&/@rules[[downvs]];
If[verbose,Print["   Rules ",Shallow[downvs]," have been declared as DownValues for ",symbol,"."]]];
If[Length[upvs]>0,AppendTo[UpValues[symbol],#]&/@rules[[upvs]];If[verbose,Print["   Rules ",Shallow[upvs]," have been declared as UpValues for ",symbol,"."]]];
If[Length[othervs]>0,AppendTo[$Rules,#]&/@rules[[othervs]];If[verbose,Print["   Rules ",Shallow[othervs]," have been declared as generic Rules."]]]; 
];


SetNumberOfArguments[AutomaticRules,{2,Infinity}];
Protect[AutomaticRules,Verbose];


(*********************** 11.Covariant derivatives *********************)


If[$ReadingVerbose,Print["Reading section 11: Covariant derivatives."],Null,Null]


VBundlesOfCovD[covd_[__]]:=VBundlesOfCovD[covd];
ManifoldOfCovD[covd_[__]]:=ManifoldOfCovD[covd];
SetNumberOfArguments[VBundlesOfCovD,1];
SetNumberOfArguments[ManifoldOfCovD,1];
Protect[VBundlesOfCovD,ManifoldOfCovD];


TangentBundleOfCovD[covd_]:=First@VBundlesOfCovD[covd];


Leibnitz[expr_][-a_,b_]:=With[{frees=FindFreeIndices[expr],vbQ=VBundleIndexQ[VBundleOfIndex[a]]},Plus@@Map[(delta[-a,#]ReplaceIndex[expr,#->b])&,Select[frees,vbQ]]-Plus@@Map[(delta[-#,b]ReplaceIndex[expr,-#->-a])&,Select[ChangeIndex/@frees,vbQ]]
];
Protect[Leibnitz];


$Christoffels={};


(* Main driver *)
CompatibleCovDsQ[covd_,covd_?CovDQ]:=True;
CompatibleCovDsQ[PD,covd_?CovDQ]:=True;
CompatibleCovDsQ[covd_?CovDQ,PD]:=True;
CompatibleCovDsQ[covd1_?CovDQ,covd2_?CovDQ]:=compareCovDs[VBundlesOfCovD[covd1],VBundlesOfCovD[covd2],False];
CompatibleCovDsQ[covd1_,covd2_?CovDQ]:=Throw@Message[Christoffel::unknown,"covariant derivative",covd1];
CompatibleCovDsQ[covd1_,covd2_]:=Throw@Message[Christoffel::unknown,"covariant derivative",covd2];
(* Actual decision. The third argument throws an error if the covds are incompatible *)
compareCovDs[{tb_},{tb_},_]:=True;
compareCovDs[{tb_,__},{tb_},_]:=True;
compareCovDs[{tb_},{tb_,__},_]:=True;
compareCovDs[{tb_,vb1__},{tb_,vb2__},mess_]:=compareCovDserror[Complement[{vb1},{vb2}]==={}||Complement[{vb2},{vb1}]==={},mess];
compareCovDs[{tb1_,___},{tb2_,___},mess_]:=If[mess,Message[Christoffel::base,tb1,tb2];Zero,False];
compareCovDserror[True,_]:=True;
compareCovDserror[False,mess_]:=If[mess,Throw@Message[Christoffel::error,"Incompatible vector bundles."],False];


Christoffel::base="Connections have different base manifolds `1` and `2`. Assuming their Christoffel vanishes.";


ChangeCovD[expr_,All,covd2_:PD]:=ChangeCovD[expr,$CovDs,covd2];
ChangeCovD[expr_,None,covd2_:PD]:=ChangeCovD[expr,{},covd2];
ChangeCovD[expr_,covd_?CovDQ,covd_]:=expr;
ChangeCovD[expr_,covd_?CovDQ,covd2_:PD]:=If[CompatibleCovDsQ[covd,covd2],changeCovD[expr,covd,covd2],expr];
ChangeCovD[expr_,list_List,covd2_:PD]:=Fold[ChangeCovD[#1,#2,covd2]&,expr,list];
ChangeCovD[expr_,x_,_:PD]:=Throw@Message[ChangeCovD::unknown,"covariant derivative",x];
ChangeCovD[expr_]:=ChangeCovD[expr,$CovDs];
SetNumberOfArguments[ChangeCovD,{1,3}];
Protect[ChangeCovD];


(* Defaults for the fourth and fifth arguments *)
changeCovD[expr_,covd_,covd2_]:=changeCovD[expr,covd,covd2,Identity1,Identity];
changeCovD[expr_,covd_,covd_,_,_]:=expr;
(* CovDTODO *)
changeCovD[expr_,covd_,covd2_?CovDQ,tmpcovdhead_,tmpchrhead_]:=With[{
tb=TangentBundleOfCovD[If[covd===PD,covd2,covd]]},expr/.HoldPattern[covd[ind_?(IndexOnQ[#,tb]&)][expr1_]]:>makechangeCovD[changeCovD[expr1,covd,covd2,tmpcovdhead,tmpchrhead],covd,covd2,ind,tmpcovdhead,tmpchrhead]
];
changeCovD[expr_,_,x_,_,_]:=Throw[Message[ChangeCovD::unknown,"covariant derivative",x];ERROR[expr]];


Identity1[a_,b___]:=a;


xTensorQ[CHR]^=True;


(* Convert covd[ind][expr] into covd2[ind][expr]+stuff. CovDTODO *)
makechangeCovD[expr_,covd_,covd2_,ind_,tmpcovdhead_,tmpchrhead_]:=With[{vbundles=Apply[Union,VBundlesOfCovD/@DeleteCases[{covd,covd2},PD]]},
tmpcovdhead[covd2[ind][expr],covd]+
(Plus@@Map[addChristoffel[expr,ind],selecton[Select[FindFreeIndices[expr],AIndexQ],vbundles]]-addChristoffelDensity[expr,covd,ind,tmpchrhead]+addChristoffelDensity[expr,covd2,ind,tmpchrhead]/.CHR[indices__]:>tmpchrhead[Christoffel[covd,covd2][indices]])
];


selecton[indices_IndexList,vbundles_List]:=Apply[Union,selecton[indices,#]&/@vbundles];
selecton[indices_IndexList,vbundle_?VBundleQ]:={#,DummyIn[vbundle]}&/@Select[indices,IndexOnQ[#,vbundle]&];


addChristoffel[expr_,ind_][{oldind_?upQ,dummy_}]:=CHR[oldind,ind,-dummy]ReplaceIndex[expr,oldind->dummy];
addChristoffel[expr_,ind_][{oldind_?downQ,dummy_}]:=-CHR[dummy,ind,oldind]ReplaceIndex[expr,oldind->-dummy];
addChristoffelDensity[expr_,covd_?WeightedCovDQ,ind_,tmpchrhead_]:=With[{basis=WeightedWithBasis[covd],dummy=DummyIn@VBundleOfIndex[ind]},WeightOf[expr,basis]tmpchrhead[Christoffel[covd,xAct`xCoba`PDOfBasis[basis]][dummy,ind,-dummy]]expr];
addChristoffelDensity[_,_,_,_]:=0;


CovDToChristoffel=ChangeCovD;
Protect[CovDToChristoffel];


(* One argument *)
Christoffel[covd_][i1_,i2_,i3_]:=Christoffel[covd,PD][i1,i2,i3];
(* Check of Dagger for indices. Assume both i1, i3 have same dagger-character *)
Christoffel[covd1_,covd2_][i1_?DaggerIndexQ,i2_,i3_]:=Dagger[Christoffel[covd1,covd2][DaggerIndex[i1],i2,DaggerIndex[i3]]];
(* Three special cases *)
Christoffel[covd_?CovDQ,covd_][i1_,i2_,i3_]:=0;
Christoffel[covd_?CovDQ,PD][i1_,i2_,i3_]:=makeChristoffel[{covd,PD}][i1,i2,i3];
Christoffel[PD,covd_?CovDQ][i1_,i2_,i3_]:=-makeChristoffel[{covd,PD}][i1,i2,i3];
(* General case *)
Christoffel[covd1_?CovDQ,covd2_?CovDQ][i1_,i2_,i3_]:=Order[covd1,covd2]makeChristoffel[Sort[{covd1,covd2}]][i1,i2,i3];
(* Errors *)
Christoffel[_?CovDQ,covd_][i1_,i2_,i3_]:=Throw[Message[Christoffel::unknown,"covariant derivative",covd]];
Christoffel[covd_,_][i1_,i2_,i3_]:=Throw[Message[Christoffel::unknown,"covariant derivative",covd]];
SetNumberOfArguments[Christoffel,{1,2}];
Protect[Christoffel];


AChristoffel[covd1_,covd2_:PD][i1_,i2_,i3_]:=Christoffel[covd1,covd2][i1,i2,i3];
SetNumberOfArguments[AChristoffel,{1,2}];
Protect[AChristoffel];


(* (A)Christoffel with covd and PD. No compatibility check needed. The tensor has already been defined *)
makeChristoffel[{covd_,PD}][i1_,i2_,i3_]:=Module[{chrtype,tbundle,vbundle},
{chrtype,tbundle,vbundle}=checkChristoffelIndices[VBundlesOfCovD[covd],VBundleOfIndex/@IndexList[i1,i2,i3]];
GiveSymbol[chrtype,covd][i1,i2,i3]
];


makeChristoffel[{covd1_,covd2_}][i1_,i2_,i3_]:=Module[{chrtype,chrname,vbundles1=VBundlesOfCovD[covd1],vbundles2=VBundlesOfCovD[covd2],tbundle,vbundle},
If[compareCovDs[vbundles1,vbundles2,True]===Zero,
0,
{chrtype,tbundle,vbundle}=checkChristoffelIndices[DeleteDuplicates@Flatten[{vbundles1,vbundles2}],VBundleOfIndex/@IndexList[i1,i2,i3]];
chrname=GiveSymbol[chrtype,covd1,covd2];
If[!xTensorQ[chrname],defChristoffel[chrname,{tbundle,vbundle},{chrtype,covd1,covd2}]];
chrname[i1,i2,i3]
]
];


Christoffel::ind="Index `1` of (A)Christoffel is not compatible with derivative.";
Christoffel::inds13="Indices 1 and 3 of (A)Christoffel must belong to the same vbundle.";
checkChristoffelIndices[vbundles_List,IndexList[vb1_,vb2_,vb3_]]:=With[{
tb=First[vbundles],
svb1=Cases[vbundles,vb_/;SubvbundleQ[vb,vb1]],
svb2=Cases[vbundles,vb_/;SubvbundleQ[vb,vb2]],
svb3=Cases[vbundles,vb_/;SubvbundleQ[vb,vb3]]
},
Which[
svb2=!={tb},Throw[Message[Christoffel::ind,2]],
svb1==={},Throw[Message[Christoffel::ind,1]],
svb3==={},Throw[Message[Christoffel::ind,3]],
svb1=!=svb3,Throw@Message[Christoffel::inds13],
svb1===svb2,{Christoffel,tb,tb},
True,{AChristoffel,tb,First[svb1]}]
];


defChristoffel[chrname_,{tb_,vb_},{chrtype_,covd1_,covd2_},options___]:=
Module[{inds,vbinds=GetIndicesOfVBundle[vb,3],tbinds=GetIndicesOfVBundle[tb,2]},
inds=Sequence[vbinds[[1]],-tbinds[[2]],-vbinds[[3]]];
DefTensor[chrname[inds],Union[{tb,vb},DependenciesOfCovD[covd1],DependenciesOfCovD[covd2]],If[tb===vb&&Torsion[covd1][inds]===Torsion[covd2][inds],Symmetric[{2,3}],GenSet[]],PrintAs:>GiveOutputString[chrtype,covd1,covd2],TensorID->{chrtype,covd1,covd2},Dagger:>If[DaggerIndexQ@IndexList[inds],Complex,Real],options];
AppendTo[$Christoffels,{chrname,{chrtype,covd1,covd2},DeleteDuplicates[{tb,vb}]}];
];


BreakChristoffel[expr_]:=BreakChristoffel[expr,PD];
BreakChristoffel[expr_,covd_?CovDQ]:=expr/.Map[makeChristoffelRule[covd],$Christoffels];
BreakChristoffel[expr_,chr_?xTensorQ,covd_:PD]:=expr/.makeChristoffelRule[covd][Flatten[Cases[$Christoffels,{chr,_,_}],1]];
SetNumberOfArguments[BreakChristoffel,{1,3}];
Protect[BreakChristoffel];


makeChristoffelRule[PD][{chrname_,{_,covd1_,covd2_},_}]:=chrname[inds__]:>breakChristoffel[covd1,covd2,PD][inds];
makeChristoffelRule[covd_][{chrname_,{_,covd1_,covd2_},_}]:=If[CompatibleCovDsQ[covd,covd1]&&CompatibleCovDsQ[covd,covd2],
chrname[inds__]:>breakChristoffel[covd1,covd2,covd][inds],
{}];
makeChristoffelRule[_][{}]:={};


breakChristoffel[covd1_,covd2_,covd_][inds__]:=Christoffel[covd1,covd][inds]-Christoffel[covd2,covd][inds];


ChangeTorsion[expr_,covd1_Symbol?TorsionQ,covd2_:PD]:=expr/.makeChangeTorsionRule[covd2][covd1];
ChangeTorsion[expr_,list_List,covd2_:PD]:=Fold[ChangeTorsion[#1,#2,covd2]&,expr,list];
ChangeTorsion[expr_,_,_:PD]:=expr;
ChangeTorsion[expr_]:=ChangeTorsion[expr,$CovDs];
SetNumberOfArguments[ChangeTorsion,{1,3}];
Protect[ChangeTorsion];


changeTorsion[covd1_,covd2_][a_,b_,c_]:=With[{chr=HeadOfTensor[Christoffel[covd1,covd2][a,b,c],{a,b,c}]},Torsion[covd2][a,b,c]+$TorsionSign(chr[a,b,c]-chr[a,c,b])];
makeChangeTorsionRule[covd2_][covd1_]:=If[TorsionQ[covd1]&&CompatibleCovDsQ[covd1,covd2],Torsion[covd1][inds__]:>changeTorsion[covd1,covd2][inds],{}];


TorsionToChristoffel=ChangeTorsion;
Protect[TorsionToChristoffel];


ChangeCurvature[expr_,covd_,covd_]:=expr;
ChangeCurvature[expr_,covd1_Symbol?CurvatureQ,covd2_:PD]:=If[CompatibleCovDsQ[covd1,covd2],expr/.makeChangeCurvatureRules[covd2,metricsof[covd1]][covd1],expr];
ChangeCurvature[expr_,list_List,covd2_:PD]:=Fold[ChangeCurvature[#1,#2,covd2]&,expr,list];
ChangeCurvature[expr_,_,_:PD]:=expr;
ChangeCurvature[expr_]:=ChangeCurvature[expr,$CovDs];
SetNumberOfArguments[ChangeCurvature,{1,3}];
Protect[ChangeCurvature];


(* Return the first-metric, or delta if there is none *)
metricof[vb_]:=FirstMetricOfVBundle[vb,False]/.Null->delta;
(* List of metrics. We assume VBundlesOfCovD return 1, 2 or 3 vbundles *)
metricsof[covd_Symbol]:=metricsof[VBundlesOfCovD[covd]];
metricsof[{tb_}]:={metricof[tb],delta,delta};
metricsof[{tb_,vb_}]:={metricof[tb],metricof[vb],delta};
metricsof[{tb_,vb_,vbdag_}]:={metricof[tb],metricof[vb],metricof[vbdag]};


makeChangeCurvatureRules[covd2_,metrics_][covd1_]:=With[{vbundles=VBundlesOfCovD[covd1]},
Join[
If[CurvatureQ[covd1,First[vbundles]],
Flatten[{
makeChangeRiemannRule[covd2,metrics,Riemann][covd1],
makeChangeRicciRule[covd2,delta][covd1],
makeChangeRicciScalarRule[covd2,MetricOfCovD[covd1]][covd1]}
],{}],
If[Length[vbundles]>1&&CurvatureQ[covd1,Last[vbundles]],Flatten[{makeChangeRiemannRule[covd2,metrics,FRiemann][covd1]}],{}]]]


(* CovDTODO *)
(* With no need of a metric. riemann can be either Riemann or FRiemann *)
changeRiemann[covd1_,covd2_,{_,_,_},riemann_][-c_Symbol,-d_Symbol,-b_Symbol,a_Symbol]:=With[{chr=HeadOfTensor[Christoffel[covd1,covd2][a,-c,-b],{a,-c,-b}]},Module[{e=DummyAs[a]},
riemann[covd2][-c,-d,-b,a]+$RiemannSign(-covd2[-c][chr[a,-d,-b]]+covd2[-d][chr[a,-c,-b]]-chr[a,-c,-e]chr[e,-d,-b]+chr[a,-d,-e]chr[e,-c,-b]+If[riemann===Riemann,-$TorsionSign Torsion[covd2][e,-c,-d]chr[a,-e,-b],0])]];
(* All other cases *)
changeRiemann[covd1_,covd2_,{tmetric_,vmetric_,vdagmetric_},riemann_][c_,d_,b_,a_]:=With[{vbundle=VBundleOfIndex[a],tbundle=VBundleOfIndex[c],vdmetric=If[HasDaggerCharacterQ[riemann],vdagmetric,vmetric]},Module[{c1=DummyIn[tbundle],d1=DummyIn[tbundle],b1=DummyIn[vbundle],a1=DummyIn[vbundle]},tmetric[c,c1]tmetric[d,d1]vdmetric[b,b1]vdmetric[a,-a1]changeRiemann[covd1,covd2,{delta,delta,delta},riemann][-c1,-d1,-b1,a1]]];
(* Construct rule *)
makeChangeRiemannRule[covd2_,{tmetric_,vmetric_,vdagmetric_},riemann_][covd1_]:=With[{riemann1=riemann[covd1]},HoldPattern[riemann1[c_,d_,b_,a_]]:>changeRiemann[covd1,covd2,{tmetric,vmetric,vdagmetric},riemann][c,d,b,a]];


changeRicci[covd1_,covd2_,_][-c_Symbol,-b_Symbol]:=Module[{a=DummyAs[c]},$RicciSign changeRiemann[covd1,covd2,{delta,delta,delta},Riemann][-c,-a,-b,a]];
changeRicci[covd1_,covd2_,tmetric_][c_,b_]:=With[{vbundle=VBundleOfIndex[c]},Module[{c1=DummyIn[vbundle],b1=DummyIn[vbundle]},tmetric[c,c1]tmetric[b,b1]changeRicci[covd1,covd2,HELLO][-c1,-b1]]];
makeChangeRicciRule[covd2_,metric_][covd1_]:=With[{ricci=Ricci[covd1]},HoldPattern[ricci[inds__]]:>changeRicci[covd1,covd2,metric][inds]];


changeRicciScalar[covd1_,covd2_,metricofcovd1_][]:=Module[{a=DummyIn[TangentBundleOfCovD[covd1]],b=DummyIn[TangentBundleOfCovD[covd1]]},Scalar[Inv[metricofcovd1][a,b]changeRicci[covd1,covd2,HELLO][-a,-b]]];
makeChangeRicciScalarRule[covd2_,Null][covd1_]:={};
makeChangeRicciScalarRule[covd2_,metricofcovd1_][covd1_]:=With[{ricciscalar=RicciScalar[covd1]},HoldPattern[ricciscalar[]]:>changeRicciScalar[covd1,covd2,metricofcovd1][]];


RiemannToChristoffel=ChangeCurvature;
Protect[RiemannToChristoffel];


(* CovDTODO *)
CommuteCovDs[expr_,covd_,{a_,b_}]:=expr/.HoldPattern[covd[b][covd[a][expr1_]]]:>makeCommuteCovDs[expr1,covd,{a,b}];


(* CovDTODO *)
SetAttributes[makeCommuteCovDs,HoldFirst];
makeCommuteCovDs[expr_,covd_,{a_,b_}]:=covd[a][covd[b][expr]]+addCurvature[expr,covd,{a,b},Select[FindFreeIndices[expr],AIndexQ],VBundlesOfCovD[covd]]+addTorsion[expr,covd,{a,b}]+addOther[expr,covd,{a,b}];


SetAttributes[{addCurvature,addCurvature1,addTorsion,addOther,addOther1},HoldFirst];


addCurvature[args__,{}]:=0;
addCurvature[args__,{tb_}]:=addCurvature1[args,tb,Riemann];
addCurvature[args__,{tb_,vb_}]:=addCurvature1[args,tb,Riemann]+addCurvature1[args,vb,FRiemann];
addCurvature[args__,{tb_,vb_,vbdag_}]:=addCurvature1[args,tb,Riemann]+addCurvature1[args,vb,FRiemann]+addCurvature1[args,vbdag,FRiemann];
addCurvature1[expr_,covd_,{a_,b_},frees_,vb_,riemann_]:=$RiemannSign With[{dummy=DummyIn[vb],vbQ=VBundleIndexQ[vb],curv=If[HasDaggerCharacterQ[vb],Dagger,Identity][riemann[covd]]},
Plus@@Map[(curv[a,b,-dummy,#]ReplaceIndex[expr,#->dummy])&,Select[frees,vbQ]]-Plus@@Map[(curv[a,b,-#,dummy]ReplaceIndex[expr,-#->-dummy])&,Select[ChangeIndex/@frees,vbQ]]];


addTorsion[expr_,covd_,{a_,b_}]:=$TorsionSign With[{dummy=DummyIn[VBundleOfIndex[a]]},Torsion[covd][dummy,a,b]covd[-dummy][expr]];


addOther[expr_,covd_,{a_,b_}]:=With[{proj=If[InducedCovDQ[covd],MetricOfCovD[covd],delta]},addOther1[expr,{covd,proj},{a,b}]-addOther1[expr,{covd,proj},{b,a}]];
addOther1[expr_,{covd_,proj_},{-_Symbol,_}]:=0;
(* CovDTODO *)
addOther1[expr_,{covd_,proj_},{a_,b_}]:=With[{vb=TangentBundleOfCovD[covd]},Module[{dummy=DummyIn[vb]},covd[b][proj[a,dummy]]covd[-dummy][expr]]];


$MultiIndexCovDs={};
$ImplodeInfoQ=False;


niceindices[list_List]:=Replace[list,screenrules[UpIndex/@IndexList@@list],1];


ImplodedName[PD[inds__],manifold_,tensor_?xTensorQ]:=If[Length@ManifoldsOfTensor[tensor]===1,SymbolJoin[PD,tensor],SymbolJoin[PD,manifold,tensor]];
ImplodedName[cd_Symbol?CovDQ[__],manifold_,tensor_?xTensorQ]:=SymbolJoin[cd,tensor];


CovDTensor[cd_Symbol[inds__],manifold_,tensor_Symbol[tinds___],ltinds_Integer]:=CovDTensor1[cd[inds],manifold,tensor[tinds],niceindices@Join[DummyAs/@{tinds},DownIndex/@DummyAs/@{inds}],Union[DependenciesOfTensor[tensor],DependenciesOfCovD[cd]]];
CovDTensor1[cd_Symbol[inds__],manifold_,tensor_Symbol[tinds___],{indices___},dependencies_List]:=With[{cdtensor=ImplodedName[cd[inds],manifold,tensor],linds=Length[{inds}],vector=If[InducedCovDQ[cd],Last@InducedFrom@MetricOfCovD[cd],Null]},
If[!xTensorQ[cdtensor],
DefTensor[cdtensor[indices],dependencies,CovDTensorSym[cd[inds],Length[{inds}],tensor[tinds],Length[{tinds}]],PrintAs:>StringJoin[Last[SymbolOfCovD[cd]],PrintAs[tensor]],Dagger:>If[DaggerQ[tensor],Complex,Real],TensorID:>{CovD,cd,linds,tensor},DefInfo:>If[$ImplodeInfoQ,{"tensor",""},False],Master->tensor,ForceSymmetries:>SameQ[cd,PD]];
If[DaggerQ[tensor],With[{dag=Dagger[cdtensor]},TensorID[dag]^={CovD,cd,linds,Dagger[tensor]}]];
If[cd=!=PD &&!MemberQ[FixedPointList[MasterOf,cdtensor],cd],
xUpAppendTo[VisitorsOf[cd],cdtensor];
xUpAppendTo[HostsOf[cdtensor],cd];
];
];
If[vector=!=Null&&OrthogonalToVector[vector][tensor],xUpSet[OrthogonalToVector[vector][cdtensor],True]];
(* Remember associated tensor *)
(* CovDTensor[cd[__],manifold,tensor[___],Length[{tinds}]]=cdtensor; *)
cdtensor];


CovDTensorSym[cd_[inds__],linds_,tensor_[tinds___],ltinds_]:=Module[{SGS=JoinSGS[SymmetryGroupOfTensor[tensor[tinds]],DisplaceSlots[SymmetryGroupOfCovD[cd[inds]],ltinds]]},
If[MatchQ[TensorID[tensor],{CovD,cd,linds,__}]&& Not@TorsionQ[cd]&&(Not@CurvatureQ[cd]||Length[DeleteCases[{tinds},_LI|-_LI]]===1),
SGS=JoinSGSoverlap[SGS,StrongGenSet[{ltinds-linds+1},GenSet[Inner[List,ltinds+Range[-linds+1,0],ltinds+Range[1,linds],xAct`xPerm`Cycles]]]]
];
SGS];


ParamDTensor[ParamD[ps__],tensor_Symbol]:=ParamDTensor[ParamD[ps],tensor,niceindices[DummyIn/@SlotsOfTensor[tensor]],DependenciesOfTensor[tensor]];
ParamDTensor[ParamD[ps__],tensor_Symbol,{indices___},dependencies_List]:=With[{pdtensor=SymbolJoin["ParamD",ps,tensor]},
If[!xTensorQ[pdtensor],
DefTensor[pdtensor[indices],dependencies,SymmetryGroupOfTensor[tensor],PrintAs:>StringJoin["ParamD",PrintAs[ps],PrintAs[tensor]],Dagger:>If[DaggerQ[tensor],Complex,Real],TensorID:>{ParamD,ps,tensor},DefInfo:>If[$ImplodeInfoQ,{"tensor",""}]]
];
ParamDTensor[ParamD[ps],tensor]=pdtensor;
pdtensor];


OverDotTensor[OverDot,tensor_Symbol]:=OverDotTensor[OverDot,tensor,niceindices[DummyIn/@SlotsOfTensor[tensor]],DependenciesOfTensor[tensor]];
OverDotTensor[OverDot,tensor_Symbol,{indices___},dependencies_List]:=With[{pdtensor=SymbolJoin["OverDot",tensor]},
If[!xTensorQ[pdtensor],
DefTensor[pdtensor[indices],dependencies,SymmetryGroupOfTensor[tensor],PrintAs:>StringJoin["OverDot",PrintAs[tensor]],Dagger:>If[DaggerQ[tensor],Complex,Real],TensorID:>{OverDot,tensor},DefInfo:>If[$ImplodeInfoQ,{"tensor",""}]]
];
OverDotTensor[OverDot,tensor]=pdtensor;
pdtensor];


LieDTensor[LieD[v_Symbol[a_]],tensor_Symbol]:=LieDTensor[LieD[v[a]],tensor,niceindices[DummyIn/@SlotsOfTensor[tensor]],Union[DependenciesOfTensor[tensor],DependenciesOfTensor[v]]];
LieDTensor[LieD[v_],tensor_]:=Throw@Message[Implode::invalid,v,"contravariant vector field"];
LieDTensor[LieD[v_Symbol[_]],tensor_Symbol,{indices___},dependencies_List]:=With[{lietensor=SymbolJoin["LieD",v,tensor]},
If[!xTensorQ[lietensor],
DefTensor[lietensor[indices],dependencies,SymmetryGroupOfTensor[tensor],PrintAs:>StringJoin["\[ScriptCapitalL]",PrintAs[v],PrintAs[tensor]],Dagger:>If[DaggerQ[tensor],Complex,Real],TensorID:>{LieD,v,tensor},DefInfo:>If[$ImplodeInfoQ,{"tensor",""}]]
];
LieDTensor[LieD[v],tensor]=lietensor;
lietensor];


If[!MemberQ[$Packages,"xAct`xCoba`"],
xAct`xCoba`SeparateBasis[][expr_]:=expr;
xAct`xCoba`ContractBasis[expr_]:=expr;
];


Implode[expr_]:=Implode[expr,{$CovDs,$MultiIndexCovDs,ParamD,OverDot,LieD}];
Implode[expr_,list_List]:=Fold[Implode,expr,list];
(* CovDTODO *)
Implode[expr_,covd_Symbol?CovDQ]:=expr//.(covd[inds__][tensor_?xTensorQ[indices___]]:>xAct`xCoba`ContractBasis@ContractMetric[covd[inds][SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[indices]]]]/.covd[inds1__][tensor1_?xTensorQ[indices1___]]:>CovDTensor[covd[inds1],ManifoldOfCovD[covd[inds1]],tensor1[indices1],Length[{indices1}]][indices1,inds1]]);
Implode[expr_,ParamD]:=expr//.(ParamD[ps__][tensor_?xTensorQ[inds___]]:>xAct`xCoba`ContractBasis@ContractMetric[ParamD[ps][SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[inds]]]]/.ParamD[ps1__][tensor1_?xTensorQ[inds1___]]:>ParamDTensor[ParamD[ps1],tensor1][inds1]]);
Implode[expr_,OverDot]:=expr//.(OverDot[tensor_?xTensorQ[inds___]]:>xAct`xCoba`ContractBasis@ContractMetric[OverDot[SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[inds]]]]/.OverDot[tensor1_?xTensorQ[inds1___]]:>OverDotTensor[OverDot,tensor1][inds1]]);
Implode[expr_,LieD]:=expr//.(LieD[v_][tensor_?xTensorQ[inds___]]:>xAct`xCoba`ContractBasis@ContractMetric[LieD[v][SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[inds]]]]/.LieD[v][tensor1_?xTensorQ[inds1___]]:>LieDTensor[LieD[v],tensor1][inds1]]);
SetNumberOfArguments[Implode,{1,2}];
Protect[Implode];


ImplodedQ[{}]:=False;
ImplodedQ[delta]:=False;
ImplodedQ[tensor_?xTensorQ[___]]:=ImplodedQ[tensor];
ImplodedQ[tensor_?xTensorQ]:=ImplodedQ[tensor]=ImplodedQ[TensorID[tensor]];
ImplodedQ[{CovD,__}]:=True;
ImplodedQ[{ParamD,__}]:=True;
ImplodedQ[{OverDot,__}]:=True;
ImplodedQ[{LieD,__}]:=True;
ImplodedQ[_]:=False;
Explode[expr_]:=Explode[expr,Select[Head/@FindAllOfType[expr,Tensor],ImplodedQ]];
Explode[expr_,list_List]:=Fold[Explode,expr,list];
Explode[expr_,tensor_?xTensorQ]:=expr/.tensor[inds___]:>ExplodeTensor[tensor[inds]];
SetNumberOfArguments[Explode,{1,2}];
Protect[Explode];


ExplodeTensor[tensor_Symbol[inds___]]:=ExplodeTensor[tensor[inds],TensorID[tensor]];
ExplodeTensor[tensor_Symbol[inds___],{CovD,covd_,linds_Integer,intensor_}]:=SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[inds]]]/.tensor[inds1___]:>(covd@@Take[{inds1},-linds])[intensor@@Drop[{inds1},-linds]];
ExplodeTensor[tensor_Symbol[inds___],{ParamD,ps__,intensor_}]:=SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[inds]]]/.tensor[inds1___]:>ParamD[ps][intensor[inds1]];
ExplodeTensor[tensor_Symbol[inds___],{OverDot,intensor_}]:=SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[inds]]]/.tensor[inds1___]:>OverDot[intensor[inds1]];
ExplodeTensor[tensor_Symbol[inds___],{LieD,v_,intensor_}]:=SeparateMetric[][xAct`xCoba`SeparateBasis[][tensor[inds]]]/.tensor[inds1___]:>LieD[v[UpIndex@DummyIn[First@SlotsOfTensor[v]]]][intensor[inds1]];
ExplodeTensor[tensor_Symbol[inds___],_]:=tensor[inds];


multiD[der_,f_[args__]]:=(Derivative[Sequence@@#][f][args]&/@IdentityMatrix[Length[{args}]]).(der/@ReplaceDummies/@{args})


(* CovDTODO *)
CheckZeroDerivative[expr_]:=expr/.HoldPattern[covd_?CovDQ[i__][expr1_]]:>0/;ZeroDerivativeQ[covd[i],expr1];
SetNumberOfArguments[CheckZeroDerivative,1];
Protect[CheckZeroDerivative];


(* Avoid checking slotted indices *)
ZeroDerivativeQ[covd_[__slot],_]:=False;
(* Tensors *)
ZeroDerivativeQ[covd_[a__],tensor_?xTensorQ[inds___?AIndexQ]]:=ZeroDerivativeQ[covd[a],tensor];
ZeroDerivativeQ[covd_[a__],tensor_?xTensorQ]:=With[{vbQ=VBundleIndexPMQ[VBundleOfIndex[First[{a}]]]},
Module[{prot=Unprotect[tensor],result},TagSet[tensor,ZeroDerivativeQ[covd[PatternTest[__,vbQ]],tensor],result=DisjointManifoldsQ[ManifoldsOfTensor[tensor],{ManifoldOfCovD[covd[a]],AnyDependencies}]];
Protect[Evaluate[prot]];
result
]
];
(* Other expressions *)
ZeroDerivativeQ[covd_[a__],expr_]:=DisjointManifoldsQ[ManifoldsOf[expr],{ManifoldOfCovD[covd[a]],AnyDependencies}];


(* CovDTODO *)
$CheckZeroDerivativeVerbose=False;
CheckZeroDerivativeStart[covd_]:=Module[{prot=Unprotect[covd]},(*If[Length[$Manifolds]<2,Throw[Message[CheckZeroDerivativeStart::error,"With less than two manifolds there is no need for CheckZeroDerivative."]]];*)
covd[a__?GIndexQ][tensor_?xTensorQ[___?AIndexQ]]:=Condition[0,If[$CheckZeroDerivativeVerbose,Print["Checking ",covd," on ",tensor]];ZeroDerivativeQ[covd[a],tensor]];
covd[a__?GIndexQ][expr_]:=Condition[0,If[$CheckZeroDerivativeVerbose,Print["Checking ",covd[a]," on ",expr]];ZeroDerivativeQ[covd[a],expr]];
Protect[Evaluate[prot]];];
CheckZeroDerivativeStop[covd_]:=Module[{prot=Unprotect[covd]},
covd[a__?GIndexQ][tensor_?xTensorQ[___?AIndexQ]]=.;
covd[a__?GIndexQ][expr_]=.;
Protect[Evaluate[prot]];];
SetNumberOfArguments[CheckZeroDerivativeStart,1];
SetNumberOfArguments[CheckZeroDerivativeStop,1];
Protect[CheckZeroDerivativeStart,CheckZeroDerivativeStop];


SetAttributes[{MakeLinearDerivative,MakeTensorialDerivative},HoldFirst]


MakeLinearDerivative[{covdL_,covdR_},leibnitz_]:=(
covdL[expr_Plus]:=Map[covdR,expr];
covdL[expr_SeriesData]:=SeriesDataMap[covdR,expr];
If[leibnitz,covdL[x_ y_]:=covdR[x]y+x covdR[y]];
covdL[_?ConstantQ]:=0;
covdL[f_?ScalarFunctionQ[args___]]:=multiD[covdR,f[args]];
covdL[Scalar[expr_]]:=covdR[ReplaceDummies[expr]];
covdL[list_List]:=Map[covdR,list];
);
MakeLinearDerivative[covd_Symbol,leibnitz_]:=MakeLinearDerivative[{covd[a_],covd[a]},leibnitz];


(* Special properties *)
Unprotect[CovD];
CovD[expr_]:=expr;
CovD[CovD[expr_,ders1__],ders2__]:=CovD[expr,ders1,ders2];
CovD[expr_,l___,a_Symbol,r___]:=CovD[expr,l,PD[a],r];
CovD[expr_,l___,-a_Symbol,r___]:=CovD[expr,l,PD[-a],r];
(* Derivative properties *)
CovD[expr_Plus,ders__]:=CovD[#,ders]&/@expr;
CovD[expr_SeriesData,ders___]:=SeriesDataMap[CovD[#,ders]&,expr];
CovD[x_ y_,der_,ders___]:=CovD[CovD[x,der]y+x CovD[y,der],ders];
CovD[_?ConstantQ,___]:=0;
CovD[f_?ScalarFunction[args___],der_,ders___]:=CovD[multiD[CovD[#,der]&,f[args]],ders];
CovD[Scalar[expr_],ders__]:=CovD[ReplaceDummies[expr],ders];
CovD[list_List,ders__]:=Map[CovD[#,list]&,list];
CovD[expr_,covd_?CovDQ[Dir[v_+w_]],ders___]:=CovD[expr,covd[Dir[v]],ders]+CovD[expr,covd[Dir[w]],ders];
CovD[expr_,covd_?CovDQ[Dir[(x_?ScalarQ)v_]],ders___]:=CovD[x CovD[expr,covd[Dir[v]]],ders];
CovD[expr_,covd_?CovDQ[Dir[0]],ders___]:=0;
Protect[CovD];


MakeTensorialDerivative[{covdL_,covdR_}]:=(
covdR[Dir[v_Plus]][expr_]:=Map[covdL[Dir[#]][expr]&,v];
covdR[Dir[v_SeriesData]][expr_]:=SeriesDataMap[covdL[Dir[#]][expr]&,v];
covdR[Dir[(x_?ScalarQ)v_]][expr_]:=x covdL[Dir[v]][expr];
covdR[Dir[0]][expr_]:=0;
);
MakeTensorialDerivative[covd_]:=MakeTensorialDerivative[{covd,covd}];


(* CovDTODO *)
MakeOrthogonalDerivative[covd_,projector_[_,_],vector_[_]]:=(
TagSet[covd,vector[i_Symbol]covd[-i_Symbol][expr_],0];
TagSet[covd,vector[-i_Symbol]covd[i_Symbol][expr_],0];
TagSetDelayed[covd,vector[i_?AIndexQ]covd[_][expr_],Condition[0,IsIndexOf[expr,-i,projector]]];
)


(* CovDTODO *)
MakeProjectedDerivative[covd_,projector_[_,_],vector_[_]]:=(
TagSetDelayed[covd,projector[i_Symbol,-j_Symbol]covd[k_][expr_],Condition[ReplaceIndex[covd[k][expr],-i->-j],IsIndexOf[covd[k][expr],-i,projector]&&OrthogonalToVectorQ[vector][expr]]];
TagSetDelayed[covd,projector[i_Symbol,-j_Symbol]covd[k_][expr_],Condition[ReplaceIndex[covd[k][expr],j->i],IsIndexOf[covd[k][expr],j,projector]&&OrthogonalToVectorQ[vector][expr]]];
TagSetDelayed[covd,projector[-i_Symbol,j_Symbol]covd[k_][expr_],Condition[ReplaceIndex[covd[k][expr],-j->-i],IsIndexOf[covd[k][expr],-j,projector]&&OrthogonalToVectorQ[vector][expr]]];
TagSetDelayed[covd,projector[-i_Symbol,j_Symbol]covd[k_][expr_],Condition[ReplaceIndex[covd[k][expr],i->j],IsIndexOf[covd[k][expr],i,projector]&&OrthogonalToVectorQ[vector][expr]]];
)


(* Register *)
AppendToUnevaluated[$CovDs,PD];
CovDQ[PD]^=True;
SymbolOfCovD[PD]^={",","\[PartialD]"};
CurvatureQ[PD]^=False;
CurvatureQ[PD,vbundle_]^=False;
TorsionQ[PD]^=False;
DependenciesOfCovD[PD]^:={};
WeightedWithBasis[PD]^:=AIndex;
ManifoldOfCovD[PD[a_]]^:=BaseOfVBundle@VBundleOfIndex[a];
ManifoldOfCovD[PD]^:=(Print["ManifoldOfCovD[PD] has been used."];{});
VBundlesOfCovD[PD[a_]]^:={VBundleOfIndex[a]};
VBundlesOfCovD[PD]^:=(Print["VBundlesOfCovD[PD] has been used."];{});
MetricOfCovD[PD]^=Null;
MasterOf[PD]^=Symbol;

(* Define derivative *)
MakeLinearDerivative[PD,True];
MakeTensorialDerivative[PD];

(* CovDTODO *)
(* Commutativity *)
SortCovDs[expr_,PD]:=expr//.$SortPDsRules;

(* No associated curvature or torsion tensors *)
RiemannToWeylRules[PD]={};
WeylToRiemannRules[PD]={};
RicciToTFRicciRules[PD]={};
TFRicciToRicciRules[PD]={};
RicciToEinsteinRules[PD]={};
EinsteinToRicciRules[PD]={};
RiemannToRiemannDownRules[PD]={};
RiemannDownToRiemannRules[PD]={};

(* All associated tensors are Zero *)
TagSet[PD,GiveSymbol[Riemann|RiemannDown|Ricci|TFRicci|RicciScalar|Einstein|Weyl|Kretschmann|Torsion|FRiemann,PD],Zero];

(* We do not add definitions for numbers of arguments other than one *)
Protect[PD];


$SortPDsRules={PD[-b_Symbol][PD[-a_Symbol][expr1_]]:>PD[-a][PD[-b][expr1]]/;DisorderedPairQ[-a,-b]};


checkcovdcurvature[True,vbundles_List]:=vbundles;
checkcovdcurvature[False,vbundles_List]:={};
checkcovdcurvature[Riemann,{tb_,___}]:={tb};
checkcovdcurvature[FRiemann,{tb_,vb__}]:={vb};
checkcovdcurvature[FRiemann,{tb_}]:=Throw@Message[DefCovD::error,"Cannot define inner vbundle curvature only having the tangent bundle."];
checkcovdcurvature[vb_?VBundleQ,vbundles_List]:=If[MemberQ[vbundles,vb],{vb},Throw@Message[DefCovD::error,"Cannot define curvature on vbundle "<>ToString[vb]<>"."]];
checkcovdcurvature[list_List,vbundles_List]:=DeleteDuplicates@Flatten[checkcovdcurvature[#,vbundles]&/@list];
checkcovdcurvature[x_,vbundles_List]:=Throw@Message[DefCovD::unknown,"value of Curvature option",x];


DefCovDCheck[covd_,manifold_,vbundles_,symbol_,{torsion_,curvature_,metric_,curvrels_,ef_,odeps_,wwb_}]:=(
(* CovD *)
If[covd===PD,Throw@Message[DefCovD::error,"CovD PD denotes the fiducial ordinary derivative."]];
ValidateSymbol[covd];
ValidateSymbolInSession[covd];
(* Vector bundles *)
If[!VBundleQ[#],Throw@Message[DefCovD::invalid,#,"vbundle"]]&/@Flatten[{vbundles}];
If[Tangent[manifold]=!=First@vbundles,Throw@Message[DefCovD::error,"Index of non-tangent vbundle."]];
(* Symbol of derivative *)
If[Head[symbol]=!=List||Length@symbol=!=2,Throw@Message[DefCovD::invalid,symbol,"pair of strings for derivative output"]];
If[Head[First@symbol]=!=String ||Length@Characters@ToString@First@symbol=!=1,Throw@Message[DefCovD::invalid,First@symbol,"Postfix symbol for a derivative"]];
If[Head[Last@symbol]=!=String ,Throw@Message[DefCovD::invalid,Last@symbol,"Prefix symbol for a derivative"]];
(* Additional dependencies *)
If[!Or[ManifoldQ[#],ParameterQ[#]],Throw@Message[DefCovD::unknown,"dependency",#]]&/@odeps;
(* Metric *)
If[metric=!=Null&&!MetricQ[metric],Throw@Message[DefCovD::unknown,"metric",metric]];
(* Weight extension *)
If[wwb=!=Null,
If[!BasisQ[wwb],Throw@Message[DefCovD::unknown,"basis",wwb]];
If[metric===Null,Throw@Message[DefCovD::error,"Only Levi-Civita connections can be weight-extended."]]];
(* Torsion *)
If[!TrueOrFalse[torsion],Throw[Message[DefCovD::invalid,torsion,"value for option Torsion"]]];
(* If[torsion &&metric=!=Null&&!FirstMetricQ[metric],Throw@Message[DefCovD::error,"Connections with torsion can only be associated to a first-metric."]];*)
(* CurvatureRelations *)
If[!TrueOrFalse[curvrels],Throw[Message[DefCovD::invalid,curvrels,"value for option CurvatureRelations"]]];
(* Extended derivative *)
If[ef=!=Null&&!CovDQ[ef],Throw[Message[DefCovD::invalid,ef," covariant derivative to extend"]]];
(* Curvature. Return bundles with curvature *)
checkcovdcurvature[curvature,vbundles]
)


Set[CovDTensorQ[#],True]&/@{Torsion,Christoffel,AChristoffel,Riemann,FRiemann,Ricci,RicciScalar,RiemannDown,Einstein,TFRicci,Weyl,Kretschmann};
CovDTensorQ[_]=False;


PrintAsCharacter[Torsion]="T";
PrintAsCharacter[Christoffel]="\[CapitalGamma]";
PrintAsCharacter[AChristoffel]="A";
PrintAsCharacter[Riemann]="R";
PrintAsCharacter[FRiemann]="F";
PrintAsCharacter[Ricci]="R";
PrintAsCharacter[RicciScalar]="R";
PrintAsCharacter[RiemannDown]="\[ScriptCapitalR]";
PrintAsCharacter[Einstein]="G";
PrintAsCharacter[TFRicci]="S";
PrintAsCharacter[Weyl]="W";
PrintAsCharacter[Kretschmann]="K";


(* Special formatting of tensors associated to connections *)
GiveOutputString[tensor_Symbol?CovDTensorQ,covd_]:=StringJoin[PrintAsCharacter[tensor],"[",SymbolOfCovD[covd][[2]],"]"];
GiveOutputString[tensor_Symbol?CovDTensorQ,covd1_,covd2_]:=StringJoin[PrintAsCharacter[tensor],"[",SymbolOfCovD[covd1][[2]],",",SymbolOfCovD[covd2][[2]],"]"];


DefCovD[covd_[ind_],symbol:{_String,_String},options___]:=DefCovD[covd[ind],VBundleOfIndex[ind],SymbolOfCovD->symbol,options];
DefCovD[covd_[ind_],vbundles_,symbol:{_String,_String},options___]:=DefCovD[covd[ind],vbundles,SymbolOfCovD->symbol,options];
DefCovD[covd_[ind_],options___?OptionQ]:=DefCovD[covd[ind],VBundleOfIndex[ind],options];


Options[DefCovD]={
SymbolOfCovD->{";","\[EmptyDownTriangle]"},
Torsion->False,
Curvature->True,
FromMetric->Null,
CurvatureRelations->True,
ExtendedFrom->Null,
OtherDependencies->{},
OrthogonalTo->{},
ProjectedWith->{},
WeightedWithBasis->Null,
ProtectNewSymbol:>$ProtectNewSymbols,
Master->Null,
DefInfo->{"covariant derivative",""}};
DefCovD[list_List,rest___]:=Scan[DefCovD[#,rest]&,list];
DefCovD[covd_[ind_],vbundles_,options:OptionsPattern[]]:=
Catch@(If[Not[AIndexQ[ind]&&DownIndexQ[ind]],Throw[Message[DefCovD::invalid,"covariant abstract-index",ind]]];
With[{tbundle=VBundleOfIndex[ind],manifold=BaseOfVBundle@VBundleOfIndex[ind]},
Module[{allbundles,symbol,torsion,curvature,metric,curvrels,ef,odeps,wwb,pns,master,info,ot,pw,curvedbundles},

(* Options *)
{symbol,torsion,curvature,metric,curvrels,ot,pw,ef,odeps,wwb,pns,master,info}=OptionValue[{SymbolOfCovD,Torsion,Curvature,FromMetric,CurvatureRelations,OrthogonalTo,ProjectedWith,ExtendedFrom,OtherDependencies,WeightedWithBasis,ProtectNewSymbol,Master,DefInfo}];
ot=Flatten[{ot},1];
pw=Flatten[{pw},1];

(* Checks *)
allbundles=DeleteDuplicates@Flatten[{tbundle,vbundles,Dagger/@{vbundles}}];
curvedbundles=DefCovDCheck[covd,manifold,allbundles,symbol,{torsion,curvature,metric,curvrels,ef,odeps,wwb}];

(* Register *)
MakeDefInfo[DefCovD,covd[ind],info];
MakexTensions[DefCovD,"Beginning",covd[ind],vbundles,options];
CovDQ[covd]^=True;
AppendToUnevaluated[$CovDs,covd];
SymbolOfCovD[covd]^=symbol;
ManifoldOfCovD[covd]^=manifold;
VBundlesOfCovD[covd]^=allbundles;
DependenciesOfCovD[covd]^=SortDependencies@Join[{manifold},odeps];
TorsionQ[covd]^=torsion;
TagSet[covd,CurvatureQ[covd,#],True]&/@curvedbundles;
TagSet[covd,CurvatureQ[covd,#],False]&/@Complement[allbundles,curvedbundles];
MetricOfCovD[covd]^=metric;
WeightedWithBasis[covd]^=wwb;
SymbolRelations[covd,master,If[ef===Null,{manifold},{manifold,ef}]];
DefInfo[covd]^=info;

(* Define derivative operator *)
MakeLinearDerivative[covd,ot==={}];
MakeTensorialDerivative[covd];
If[ot=!={},MakeOrthogonalDerivative[covd,First[pw],First[ot]]];
If[pw=!={},MakeProjectedDerivative[covd,First[pw],First[ot]]];

(* Define curvature tensors and other properties *)
If[info=!=False,info=$DefInfoQ];
If[ef=!=Null,
ExtendedFrom[covd]^=ef;
defcovdTangent[covd,tbundle,{MetricOfCovD[ef],ef},{ot,pw,info}],
defcovdTangent[covd,tbundle,{torsion,CurvatureQ[covd,tbundle],metric,curvrels},{ot,pw,info}]];
If[Length[allbundles]>1,defcovdFiber[covd,tbundle,allbundles[[2]],MemberQ[curvedbundles,allbundles[[2]]],info]
];

MakexTensions[DefCovD,"End",covd[ind],vbundles,options];

If[pns,Protect[covd]];
]
]);
SetNumberOfArguments[DefCovD,{1,Infinity}];
Protect[DefCovD];


CurvatureQ[covd_Symbol?CovDQ,vbundle_?VBundleQ]:=False;
CurvatureQ[x_,vbundle_?VBundleQ]:=Throw@Message[CurvatureQ::unknown,"covariant derivative",x];
CurvatureQ[covd_Symbol?CovDQ_,x_]:=Throw@Message[CurvatureQ::unknown,"vector bundle",x];
CurvatureQ[covd_]:=Apply[Or,Map[CurvatureQ[covd,#]&,VBundlesOfCovD[covd]]];
TorsionQ[covd_Symbol?CovDQ]:=False;
TorsionQ[x_]:=Throw@Message[TorsionQ::unknown,"covariant derivative",x];
MetricOfCovD[x_]:=Throw@Message[MetricOfCovD::unknown,"covariant derivative",x];
DependenciesOfCovD[x_]:=Throw@Message[DependenciesOfCovD::unknown,"covariant derivative",x];
ExtendedFrom[x_]:=Null;
WeightedWithBasis[covd_?CovDQ]:=Null;
WeightedWithBasis[x_]:=Throw@Message[WeightedWithBasis::unknown,"covariant derivative",x];
WeightedCovDQ[covd_]:=WeightedWithBasis[covd]=!=Null;
MetricCovDQ[covd_]:=MetricOfCovD[covd]=!=Null;


General::nomet="Found nonmetric connection `1`.";


(* Always defined *)
Torsion[covd_Symbol?CovDQ]:=GiveSymbol[Torsion,covd];
Riemann[covd_Symbol?CovDQ]:=GiveSymbol[Riemann,covd];
FRiemann[covd_Symbol?CovDQ]:=GiveSymbol[FRiemann,covd];
Ricci[covd_Symbol?CovDQ]:=GiveSymbol[Ricci,covd];
(* Defined only for metric-derived connections *)
RicciScalar[covd_Symbol?CovDQ]:=If[MetricCovDQ[covd],GiveSymbol[RicciScalar,covd],Throw@Message[RicciScalar::nomet,covd]];
Einstein[covd_Symbol?CovDQ]:=If[MetricCovDQ[covd],GiveSymbol[Einstein,covd],Throw@Message[Einstein::nomet,covd]];
TFRicci[covd_Symbol?CovDQ]:=If[MetricCovDQ[covd],GiveSymbol[TFRicci,covd],Throw@Message[TFRicci::nomet,covd]];
Weyl[covd_Symbol?CovDQ]:=If[MetricCovDQ[covd],GiveSymbol[Weyl,covd],Throw@Message[Weyl::nomet,covd]];
Kretschmann[covd_Symbol?CovDQ]:=If[MetricCovDQ[covd],GiveSymbol[Kretschmann,covd],Throw@Message[Kretschmann::nomet,covd]];
(* Defined only for connections associated to a frozen metric *)
RiemannDown[covd_Symbol?CovDQ]:=If[MetricCovDQ[covd],
If[FrozenMetricQ[MetricOfCovD[covd]],GiveSymbol[RiemannDown,covd],GiveSymbol[Riemann,covd]],
Throw@Message[RiemannDown::nomet,covd]
];


SetDelayed[#1[x_],Throw@Message[#::unknown,"connection",x]]&/@{Torsion,Riemann,FRiemann,Ricci,RiemannDown,RicciScalar,Einstein,TFRicci,Weyl,Kretschmann};


CurvatureRelations[]:=Apply[Join,Map[CurvatureRelations,$CovDs]];
CurvatureRelations[covd_Symbol?CovDQ]:=Join[CurvatureRelations[covd,Riemann],If[MetricOfCovD[covd]=!=Null,CurvatureRelations[covd,Ricci],{}]];
CurvatureRelations[___]:={};


ContractCurvature[expr_,args___]:=expr//.CurvatureRelations[args];


SetNumberOfArguments[#,1]&/@{TorsionQ,MetricOfCovD,SymbolOfCovD,ExtendedFrom,DependenciesOfCovD,WeightedWithBasis};
SetNumberOfArguments[CurvatureQ,{1,2}];
Protect[FRiemann,Curvature,Torsion,FromMetric,CurvatureRelations,CurvatureQ,TorsionQ,MetricOfCovD,SymbolOfCovD,ContractCurvature,ExtendedFrom,DependenciesOfCovD,WeightedWithBasis];


(* By default the symmetry group of a derivative is trivial *)
SymmetryGroupOfCovD[covd_[___]]:=SymmetryGroupOfCovD[covd];
SymmetryGroupOfCovD[covd_Symbol]:=StrongGenSet[{},GenSet[]];


UndefCovD[PD]:=Throw@Message[UndefCovD::noundef,"CovD",PD,"it is the fiducial origin of derivatives"];
UndefCovD[covd_]:=($MultiIndexCovDs=DeleteCases[$MultiIndexCovDs,covd];RemoveSymbol[covd])/;MemberQ[$MultiIndexCovDs,covd];
UndefCovD[list:{___?CovDQ}]:=Scan[UndefCovD,list];
UndefCovD[covd_]:=Catch@With[{manifold=ManifoldOfCovD[covd],ef=ExtendedFrom[covd]},Module[{servants=ServantsOf[covd],christoffels},

If[!CovDQ[covd],Throw[Message[UndefCovD::unknown,"covariant derivative",covd]]];
CheckRemoveSymbol[covd];
MakexTensions[UndefCovD,"Beginning",covd];
xUpSet[ServantsOf[covd],{}];
christoffels=Cases[$Christoffels,{_,{___,covd,___},_}];
$Christoffels=Complement[$Christoffels,christoffels];
DropFromHosts[covd];
Undef/@Union[First/@christoffels,servants];
If[ef=!=Null,
xUpDeleteCasesTo[VisitorsOf[ef],covd];RemoveSymbol/@(StringJoin[#,ToString[covd]]&/@Join[{"Christoffel","Riemann","Ricci","Torsion"},If[MetricOfCovD[ef]=!=Null,{"RicciScalar","Weyl","TFRicci","Einstein","Kretschmann"},{}]])];
$CovDs=DeleteCases[$CovDs,covd];
If[MetricOfCovD[covd]=!=Null,
Unset[RiemannToWeylRules[covd]];
Unset[WeylToRiemannRules[covd]];
Unset[RicciToTFRicciRules[covd]];
Unset[TFRicciToRicciRules[covd]];
Unset[RicciToEinsteinRules[covd]];
Unset[EinsteinToRicciRules[covd]];
];
Unset[SortCovDs[expr_,covd]];
MakexTensions[UndefCovD,"End",covd];
MakeUndefInfo[UndefCovD,covd];
RemoveSymbol[covd];
]
];
SetNumberOfArguments[UndefCovD,1];
Protect[UndefCovD];


SortCovDs[expr_]:=SortCovDs[expr,$CovDs];
SortCovDs[expr_,list_List]:=Fold[SortCovDs,expr,list];
SetNumberOfArguments[SortCovDs,{1,2}];


deflistableCovDs[function_]:=With[{rules=SymbolJoin[function,"Rules"]},
function[expr_]:=function[expr,$CovDs];
function[expr_,list_List]:=Fold[function,expr,list];
function[expr_,covd_Symbol?CovDQ]:=expr/.rules[covd];
SetNumberOfArguments[function,{1,2}];
Protect[function];
];


deflistableCovDs/@{RiemannToWeyl,WeylToRiemann,RicciToTFRicci,TFRicciToRicci,RicciToEinstein,EinsteinToRicci,RiemannToRiemannDown,RiemannDownToRiemann};


defmessage[adjective_String ,tensor_String,output_,comment_:""]:=Print["** DefCovD:  Defining "<>If[adjective==="","",adjective<>" "]<>tensor<>" tensor ",output,comment]


CovD::noleib="Cannot apply Leibnitz rule for `1` on `2`."


defcovdTangent[covd_,tbundle_,{torQ_,curvQ_,metric_,curvrels_},{ov_,pw_,info_}]:=With[
(* Reinterpretation of input *)
{indexlist=GetIndicesOfVBundle[tbundle,9],
dim=If[pw==={},DimOfVBundle[tbundle],DimOfVBundle[tbundle]-1],
metricQ=(metric=!=Null),
orthogonalQ=(ov=!={}),
projectedQ=(pw=!={}),
endowedQ=MetricEndowedQ[tbundle],
frozenQ=If[metric=!=Null,FrozenMetricQ[metric],False]},
With[
(* Secondary definitions *)
{tbQ=VBundleIndexQ[tbundle],
tbpmQ=VBundleIndexPMQ[tbundle],
invmetric=If[metricQ,If[frozenQ,GiveSymbol[Inv,metric],metric],False],
TorsionName=GiveSymbol[Torsion,covd],
ChristoffelName=GiveSymbol[Christoffel,covd],
RiemannName=GiveSymbol[Riemann,covd],
RiemannDownName=If[metricQ,GiveSymbol[If[frozenQ,RiemannDown,Riemann],covd],$Failed],
RicciName=GiveSymbol[Ricci,covd],
WeylName=If[metricQ,GiveSymbol[Weyl,covd]],
TFRicciName=If[metricQ,GiveSymbol[TFRicci,covd]],
EinsteinName=If[metricQ,GiveSymbol[Einstein,covd]],
RicciScalarName=If[metricQ,GiveSymbol[RicciScalar,covd]],
KretschmannName=If[metricQ,GiveSymbol[Kretschmann,covd]],
i1=indexlist[[1]],
i2=indexlist[[2]],
i3=indexlist[[3]],
i4=indexlist[[4]],
i5=indexlist[[5]],
i1d=indexlist[[6]],
i2d=indexlist[[7]],
i3d=indexlist[[8]],
i4d=indexlist[[9]],
integerdimQ=IntegerQ[dim]},
Module[{prot,vector,projector,tmp,vanishQ},

(* 0. Check orthogonality and projections *)
If[!endowedQ&&(projectedQ||orthogonalQ),Throw@Message[DefCovD::error,"xTensor cannot yet project in a vector bundle without metric."]];

If[orthogonalQ,vector=ov[[1,0]]];
If[projectedQ,projector=pw[[1,0]]];

(* 1. Define torsion tensor *)
vanishQ=!torQ;
DefTensor[TorsionName[i1,-i2,-i3],DependenciesOfCovD[covd],Antisymmetric[{2,3}],
PrintAs:>GiveOutputString[Torsion,covd],
VanishingQ->vanishQ,
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[-i1],vector[i2],vector[i3]},{}],
ProjectedWith:>If[projectedQ,{projector[-i1,i1d],projector[i2,-i2d],projector[i3,-i3d]},{}],
DefInfo:>If[info,{"torsion tensor",""},False],
TensorID->{Torsion,covd}];

(* 2. Define Christoffel tensor relating covd to PD. Always nonzero *)
DefTensor[ChristoffelName[i1,-i2,-i3],DependenciesOfCovD[covd],If[torQ,StrongGenSet[{},GenSet[]],Symmetric[{2,3}]],
PrintAs:>GiveOutputString[Christoffel,covd],
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[-i1],vector[i2],vector[i3]},{}],
ProjectedWith:>If[projectedQ,{projector[-i1,i1d],projector[i2,-i2d],projector[i3,-i3d]},{}],
DefInfo:>If[info,{If[torQ,"non-symmetric ","symmetric "]<>"Christoffel tensor",""},False],
TensorID->{Christoffel,covd,PD}];
AppendTo[$Christoffels,{ChristoffelName,{Christoffel,covd,PD},{tbundle}}];

(* 3. Automatic commutativity of scalars, even if there is torsion. Only for covariant derivatives with abstract indices *)
(* CovDTODO *)
covd[-b_?tbQ][covd[-a_?tbQ][scalar_?xTensorQ[]]]:=covd[-a][covd[-b][scalar[]]]+$TorsionSign Module[{dummy=DummyIn[tbundle]},TorsionName[dummy,-a,-b]covd[-dummy][scalar[]]]/;$CommuteCovDsOnScalars&&DisorderedPairQ[a,b];

(* 4. Metric compatibility *)
(* CovDTODO *)
If[metricQ,
If[frozenQ,
covd[a_][invmetric[b_Symbol?tbQ,c_Symbol?tbQ]]:=0;
covd[a_][metric[-b_Symbol?tbQ,-c_Symbol?tbQ]]:=0;
covd/:TensorDerivative[invmetric,covd,___]:=Zero;
covd/:TensorDerivative[metric,covd,___]:=Zero,
covd[a_][metric[b_?tbpmQ,c_?tbpmQ]]:=0;
covd/:TensorDerivative[metric,covd,___]:=Zero
]
];

(* 5. Only if there is a metric and no torsion the RiemannDown tensor has its full symmetries *)
vanishQ=!curvQ||If[integerdimQ,dim<2,False];
If[metricQ,
DefTensor[RiemannDownName[-i1,-i2,-i3,-i4],DependenciesOfCovD[covd],If[torQ,JoinSGS[Antisymmetric[{1,2}],Antisymmetric[{3,4}]],RiemannSymmetric[{1,2,3,4}]],
PrintAs:>GiveOutputString[If[frozenQ,RiemannDown,Riemann],covd],
VanishingQ->vanishQ,
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[i1],vector[i2],vector[i3],vector[i4]},{}],
ProjectedWith:>If[projectedQ,{projector[i1,-i1d],projector[i2,-i2d],projector[i3,-i3d],projector[i4,-i4d]},{}],
DefInfo:>If[info,{"Riemann tensor",If[torQ,"Antisymmetric pairs cannot be exchanged.",""]},False],
TensorID->{Riemann,covd}]];
If[!metricQ||(metricQ&&frozenQ),
DefTensor[RiemannName[-i1,-i2,-i3,i4],DependenciesOfCovD[covd],Antisymmetric[{1,2}],
PrintAs:>GiveOutputString[Riemann,covd],
VanishingQ->vanishQ,
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[i1],vector[i2],vector[i3],vector[-i4]},{}],
ProjectedWith:>If[projectedQ,{projector[i1,-i1d],projector[i2,-i2d],projector[i3,-i3d],projector[-i4,i4d]},{}],
DefInfo:>If[info,{"Riemann tensor",If[curvQ,"Antisymmetric only in the first pair.",""]},False],
TensorID->{Riemann,covd}]
];

(* 6. Only if there is a metric and no torsion the Ricci tensor is symmetric *)
vanishQ=!curvQ||If[integerdimQ,dim<2,False];
DefTensor[RicciName[-i1,-i2],DependenciesOfCovD[covd],If[metricQ&&!torQ,Symmetric[{1,2}],StrongGenSet[{},GenSet[]]],
PrintAs:>GiveOutputString[Ricci,covd],
VanishingQ->vanishQ,
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[i1],vector[i2]},{}],
ProjectedWith:>If[projectedQ,{projector[i1,-i1d],projector[i2,-i2d]},{}],
DefInfo:>If[info,{If[vanishQ,"",If[metricQ&&!torQ,"symmetric ","non-symmetric "]]<>"Ricci tensor",""},False],
TensorID->{Ricci,covd}];
If[!vanishQ,
covd/:CurvatureRelations[covd,Riemann]=MakeRule[{RiemannName[-i1,-i2,-i3,i2],$RicciSign RicciName[-i1,-i3]},MetricOn->All];
If[curvrels,
If[info,Print["** DefCovD:  Contractions of Riemann automatically replaced by Ricci."]];
prot=Unprotect[RiemannName];
AutomaticRules[RiemannName,CurvatureRelations[covd,Riemann],Verbose->False];
Protect[Evaluate[prot]];
]
];

(* 7. The Ricci scalar only exists for a metric connection, even if there is torsion *)
If[metricQ,
vanishQ=!curvQ||If[integerdimQ,dim<2,False];
DefTensor[RicciScalarName[],DependenciesOfCovD[covd],
PrintAs:>GiveOutputString[Ricci,covd],
VanishingQ->vanishQ,
Master->covd,
DefInfo:>If[info,{"Ricci scalar",""},False],
TensorID->{RicciScalar,covd}];
If[!vanishQ,
covd/:CurvatureRelations[covd,Ricci]=Join[MakeRule[{invmetric[i1,i2]RicciName[-i1,-i2],RicciScalarName[]},MetricOn->All],If[!frozenQ,MakeRule[{RicciName[-i1,i1],RicciScalarName[]},MetricOn->All],{}]];
If[curvrels,
If[info,Print["** DefCovD:  Contractions of Ricci automatically replaced by RicciScalar."]];
prot=Unprotect[RicciName];
AutomaticRules[RicciName,CurvatureRelations[covd,Ricci],Verbose->False];
Protect[Evaluate[prot]];
]
]
];

(* 8. The Einstein tensor only exists for a metric connection, even if there is torsion *)
If[metricQ,
vanishQ=!curvQ||If[integerdimQ,dim<3,False];
DefTensor[EinsteinName[-i1,-i2],DependenciesOfCovD[covd],If[torQ,StrongGenSet[{},GenSet[]],Symmetric[{1,2}]],
PrintAs:>GiveOutputString[Einstein,covd],
VanishingQ->vanishQ,
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[i1],vector[i2]},{}],
ProjectedWith:>If[projectedQ,{projector[i1,-i1d],projector[i2,-i2d]},{}],
DefInfo:>If[info,{If[vanishQ,"",If[torQ,"non-symmetric ","symmetric "]]<>"Einstein tensor",""},False],
TensorID->{Einstein,covd}];
If[!vanishQ,
prot=Unprotect[invmetric];
AutomaticRules[invmetric,MakeRule[{invmetric[i2,i3]covd[-i3][EinsteinName[-i1,-i2]],$TorsionSign invmetric[i2,i3]RicciName[-i4,-i2]TorsionName[i4,-i1,-i3]-$TorsionSign $RicciSign/2 invmetric[i2,i3]RiemannName[-i1,-i4,-i2,i5]TorsionName[i4,-i3,-i5]}],Verbose->False];
Protect[Evaluate[prot]];
If[!frozenQ ,
prot=Unprotect[EinsteinName];
AutomaticRules[EinsteinName,MakeRule[{covd[-i2][EinsteinName[-i1,i2]],$TorsionSign metric[i2,i3]RicciName[-i4,-i2]TorsionName[i4,-i1,-i3]-$TorsionSign $RicciSign/2 metric[i2,i3]RiemannName[-i1,-i4,-i2,i5]TorsionName[i4,-i3,-i5]},MetricOn->All],Verbose->False];
Protect[Evaluate[prot]];
]
]
];

(* 9. We define the Weyl tensor only for a metric connection, even with torsion *)
If[metricQ,
(* Weyl *)
vanishQ=!curvQ||If[integerdimQ,dim<4,False];
DefTensor[WeylName[-i1,-i2,-i3,-i4],DependenciesOfCovD[covd],If[torQ,JoinSGS[Antisymmetric[{1,2}],Antisymmetric[{3,4}]],RiemannSymmetric[{1,2,3,4}]],
PrintAs:>GiveOutputString[Weyl,covd],
VanishingQ->vanishQ,
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[i1],vector[i2],vector[i3],vector[i4]},{}],
ProjectedWith:>If[projectedQ,{projector[i1,-i1d],projector[i2,-i2d],projector[i3,-i3d],projector[i4,-i4d]},{}],
DefInfo:>If[info,{"Weyl tensor",If[torQ,"Antisymmetric pairs cannot be exchanged.",""]},False],
TensorID->{Weyl,covd}];
If[!vanishQ,
prot=Unprotect[WeylName];
AutomaticRules[WeylName,MakeRule[{invmetric[i2,i4]WeylName[-i1,-i2,-i3,-i4],0},MetricOn->All],Verbose->False];
If[!frozenQ,
AutomaticRules[WeylName,MakeRule[{WeylName[i1,-i2,-i1,-i3],0},MetricOn->All],Verbose->False];
];
Protect[Evaluate[prot]];
]
];

(* 10. TFRicci for metric connections, even with torsion *)
If[metricQ,
vanishQ=!curvQ||If[integerdimQ,dim<3,False];
DefTensor[TFRicciName[-i1,-i2],DependenciesOfCovD[covd],If[torQ,StrongGenSet[{},GenSet[]],Symmetric[{1,2}]],
PrintAs:>GiveOutputString[TFRicci,covd],
VanishingQ->vanishQ,
Master->covd,
OrthogonalTo:>If[orthogonalQ,{vector[i1],vector[i2]},{}],
ProjectedWith:>If[projectedQ,{projector[i1,-i1d],projector[i2,-i2d]},{}],
DefInfo:>If[info,{If[vanishQ,"",If[torQ,"non-symmetric ","symmetric "]]<>"TFRicci tensor",""},False],
TensorID->{TFRicci,covd}];
If[!vanishQ,
prot=Unprotect[TFRicciName];
AutomaticRules[TFRicciName,MakeRule[{invmetric[i1,i2]TFRicciName[-i1,-i2],0},MetricOn->All],Verbose->False];If[!frozenQ,
AutomaticRules[TFRicciName,MakeRule[{TFRicciName[i1,-i1],0},MetricOn->All],Verbose->False];
Protect[Evaluate[prot]];
]
]
];

(* 10b. Kretschmann for metric connections, even with torsion *)
If[metricQ,
vanishQ=!curvQ;
DefTensor[KretschmannName[],DependenciesOfCovD[covd],
PrintAs:>GiveOutputString[Kretschmann,covd],
VanishingQ->vanishQ,
Master->covd,
DefInfo:>If[info,{"Kretschmann scalar",""},False],
TensorID->{Kretschmann,covd}];
];

(* 11. Commutation of derivatives with abstract indices *)
If[metricQ &&!frozenQ,
SortCovDs[expr_,covd]:=(expr//.
HoldPattern[covd[b_?tbpmQ][covd[a_?tbpmQ][expr1_]]]:>makeCommuteCovDs[expr1,covd,{a,b}]/;DisorderedPairQ[a,b]),
SortCovDs[expr_,covd]:=(expr//.HoldPattern[covd[-b_?tbQ][covd[-a_?tbQ][expr1_]]]:>makeCommuteCovDs[expr1,covd,{-a,-b}]/;DisorderedPairQ[-a,-b])
];

(* 12. Riemann versus Weyl *)
RiemannToWeylRules[covd]=If[curvQ&&metricQ&&If[integerdimQ,dim>1,True],If[info,Print["** DefCovD:  Computing RiemannToWeylRules for dim ",dim]];
Which[
dim===2,
MakeRule[{RiemannDownName[i1,i2,i3,i4],$RicciSign RicciScalarName[](metric[i1,i3]metric[i2,i4]-metric[i1,i4]metric[i2,i3])/2},MetricOn->All,ContractMetrics->True],
dim===3,MakeRule[{RiemannDownName[i1,i2,i3,i4],$RicciSign (metric[i1,i3]RicciName[i2,i4]+metric[i2,i4]RicciName[i1,i3]-metric[i1,i4]RicciName[i2,i3]-metric[i2,i3]RicciName[i1,i4])/(dim-2)-$RicciSign RicciScalarName[](metric[i1,i3]metric[i2,i4]-metric[i1,i4]metric[i2,i3])/(dim-1)/(dim-2)},MetricOn->All,ContractMetrics->True],
True,MakeRule[{RiemannDownName[i1,i2,i3,i4],WeylName[i1,i2,i3,i4]+$RicciSign (metric[i1,i3]RicciName[i2,i4]+metric[i2,i4]RicciName[i1,i3]-metric[i1,i4]RicciName[i2,i3]-metric[i2,i3]RicciName[i1,i4])/(dim-2)-$RicciSign RicciScalarName[](metric[i1,i3]metric[i2,i4]-metric[i1,i4]metric[i2,i3])/(dim-1)/(dim-2)},MetricOn->All,ContractMetrics->True]
],
{}];
WeylToRiemannRules[covd]=If[If[integerdimQ,dim>=4,True]&&metricQ&&curvQ,MakeRule[{WeylName[i1,i2,i3,i4],RiemannDownName[i1,i2,i3,i4]-$RicciSign (metric[i1,i3]RicciName[i2,i4]+metric[i2,i4]RicciName[i1,i3]-metric[i1,i4]RicciName[i2,i3]-metric[i2,i3]RicciName[i1,i4])/(dim-2)+$RicciSign RicciScalarName[](metric[i1,i3]metric[i2,i4]-metric[i1,i4]metric[i2,i3])/(dim-1)/(dim-2)},MetricOn->All,TestIndices->False,ContractMetrics->True],
{}];

RiemannToRiemannDownRules[covd]=If[metricQ&&frozenQ&&curvQ,MakeRule[{RiemannName[i1,i2,i3,i4],RiemannDownName[i1,i2,i3,-i1d]invmetric[i1d,i4]},MetricOn->All,TestIndices->False,ContractMetrics->True],
{}];
RiemannDownToRiemannRules[covd]=If[metricQ&&frozenQ&&curvQ,MakeRule[{RiemannDownName[i1,i2,i3,i4],RiemannName[i1,i2,i3,i1d]metric[-i1d,i4]},MetricOn->All,TestIndices->False,ContractMetrics->True],
{}];

(* 13. Ricci versus TFRicci. QUESTION: Are dimensions right when orthogonalQ or projectedQ? *)
RicciToTFRicciRules[covd]=If[curvQ&&metricQ&&If[integerdimQ,dim>1,True]&&!frozenQ,If[info,Print["** DefCovD:  Computing RicciToTFRicci for dim ",dim]];
Which[
dim===2,
MakeRule[{RicciName[i1,i2],RicciScalarName[]metric[i1,i2]/2},MetricOn->All,ContractMetrics->True],
True,MakeRule[{RicciName[i1,i2],TFRicciName[i1,i2]+ RicciScalarName[]metric[i1,i2]/dim},MetricOn->All,ContractMetrics->True]
],
{}];
TFRicciToRicciRules[covd]=If[If[integerdimQ,dim>=3,True]&&metricQ&&curvQ&&!frozenQ,MakeRule[{TFRicciName[i1,i2],RicciName[i1,i2]- RicciScalarName[]metric[i1,i2]/dim},MetricOn->All,TestIndices->False,ContractMetrics->True],
{}];

(* 14. Ricci versus Einstein *)
RicciToEinsteinRules[covd]=
If[curvQ&&metricQ&&If[integerdimQ,dim>1,True],
If[info,Print["** DefCovD:  Computing RicciToEinsteinRules for dim ",dim]];
Which[
dim===2,
MakeRule[{RicciName[i1,i2],metric[i1,i2]/2RicciScalarName[]},MetricOn->All,TestIndices->False,ContractMetrics->True],
True,
MakeRule[{RicciName[i1,i2],EinsteinName[i1,i2]+metric[i1,i2]/2RicciScalarName[]},MetricOn->All,TestIndices->False,ContractMetrics->True]
],
{}];
EinsteinToRicciRules[covd]=If[If[integerdimQ,dim>=3,True]&&metricQ&&curvQ,MakeRule[{EinsteinName[i1,i2],RicciName[i1,i2]-metric[i1,i2]/2RicciScalarName[]},MetricOn->All,TestIndices->False,ContractMetrics->True],
{}];

]
]
]


defcovdTangent[covd_,tbundle_,{metric_,covd2_},{ov_,pw_,info_}]:=With[
(* Reinterpretation of input *)
{metricQ=(metric=!=Null),
orthogonalQ=(ov=!={}),
projectedQ=(pw=!={}),
endowedQ=MetricEndowedQ[tbundle],
frozenQ=If[metric=!=Null,FrozenMetricQ[metric],False]},
With[
(* Secondary definitions *)
{tbQ=VBundleIndexQ[tbundle],
tbpmQ=VBundleIndexPMQ[tbundle],
TorsionName=GiveSymbol[Torsion,covd],
ChristoffelName=GiveSymbol[Christoffel,covd],
Christoffel12Name=GiveSymbol[Christoffel,##]&@@Sort[{covd,covd2}],
RiemannName=GiveSymbol[Riemann,covd],
RiemannDownName=If[metricQ,GiveSymbol[If[frozenQ,RiemannDown,Riemann],covd]],
RicciName=GiveSymbol[Ricci,covd],
WeylName=If[metricQ,GiveSymbol[Weyl,covd]],
TFRicciName=If[metricQ,GiveSymbol[TFRicci,covd]],
EinsteinName=If[metricQ,GiveSymbol[Einstein,covd]],
RicciScalarName=If[metricQ,GiveSymbol[RicciScalar,covd]],
KretschmannName=If[metricQ,GiveSymbol[Kretschmann,covd]]},

(* 0. Check orthogonality and projections *)
If[!endowedQ&&(projectedQ||orthogonalQ),Throw[Message[DefCovD::error,"xTensor cannot yet project in a vector bundle without metric."]]];

(* 1. Define torsion tensor *)
TorsionQ[covd]^=TorsionQ[covd2];
TorsionName=GiveSymbol[Torsion,covd2];

(* 2. Define Christoffel tensor relating covd to PD. Always nonzero *)
ChristoffelName=GiveSymbol[Christoffel,covd2];
(* The Christoffel relating a derivative and its extension is zero, but not the AChristoffel *)
defChristoffel[Christoffel12Name,{tbundle,tbundle},{Christoffel,Sequence@@Sort[{covd,covd2}]},VanishingQ->True];
(* Christoffel12Name[_,_,_]:=0;*)

(* CovDTODO *)
(* 3. Automatic commutativity of scalars, even if there is torsion. Only for covariant derivatives with abstract indices *)
covd[-b_?tbQ][covd[-a_?tbQ][scalar_?xTensorQ[]]]:=covd[-a][covd[-b][scalar[]]]+$TorsionSign Module[{dummy=DummyIn[tbundle]},TorsionName[dummy,-a,-b]covd[-dummy][scalar[]]]/;$CommuteCovDsOnScalars&&DisorderedPairQ[a,b];

(* 4. Only if there is a metric the Riemann tensor has its full symmetries *)
RiemannName=GiveSymbol[Riemann,covd2];
If[frozenQ,RiemannDownName=GiveSymbol[RiemannDown,covd2]];

(* 5. Only if there is a metric the Ricci tensor is symmetric *)
RicciName=GiveSymbol[Ricci,covd2];

(* 6. The Ricci scalar only exists for a metric connection *)
If[metricQ,RicciScalarName=GiveSymbol[RicciScalar,covd2]];

(* 7. Metric compatibility is automatically extended *)
If[metricQ,covd[_][metric[_?AIndexQ,_?AIndexQ]]:=0];

(* 8. The Einstein tensor only exists for a metric connection *)
If[metricQ,EinsteinName=GiveSymbol[Einstein,covd2]];

(* 9. The Weyl and TFRicci tensors only exist for a metric connection *)
If[metricQ,
WeylName=GiveSymbol[Weyl,covd2];
TFRicciName=GiveSymbol[TFRicci,covd2];
KretschmannName=GiveSymbol[Kretschmann,covd2]];

(* 10. Commutation of derivatives with abstract indices *)
(* CovDTODO *)
If[metricQ &&!frozenQ,
SortCovDs[expr_,covd]:=(expr//.
HoldPattern[covd[b_?tbpmQ][covd[a_?tbpmQ][expr1_]]]:>makeCommuteCovDs[expr1,covd,{a,b}]/;DisorderedPairQ[a,b]),
SortCovDs[expr_,covd]:=(expr//.HoldPattern[covd[-b_?tbQ][covd[-a_?tbQ][expr1_]]]:>makeCommuteCovDs[expr1,covd,{-a,-b}]/;DisorderedPairQ[-a,-b])
];

If[metricQ,
(* 11. Riemann versus Weyl. Are dimensions right when orthogonalQ or projectedQ? *)
RiemannToWeylRules[covd]=RiemannToWeylRules[covd2];
WeylToRiemannRules[covd]=WeylToRiemannRules[covd2];

(* 12. Ricci versus TFRicci. Are dimensions right when orthogonalQ or projectedQ? *)
RicciToTFRicciRules[covd]=RicciToTFRicciRules[covd2];
TFRicciToRicciRules[covd]=TFRicciToRicciRules[covd2];

(* 13. Ricci versus Einstein *)
RicciToEinsteinRules[covd]=RicciToEinsteinRules[covd2];
EinsteinToRicciRules[covd]=EinsteinToRicciRules[covd2];
];

(* 14. Conversion to derivative on the tangent bundle. Deactivated.
covd[a_][expr_]:=covd2[a][expr]/;DeleteCases[FindIndices[expr],x_/;GIndexQ[x,tbundle]]===IndexList[];
*)

]
];


defcovdFiber[covd_,tbundle_,fbundle_,curvQ_,info_]:=With[
(* Reinterpretation of input *)
{tindexlist=GetIndicesOfVBundle[tbundle,2],
findexlist=GetIndicesOfVBundle[fbundle,4]},
With[
(* Secondary definitions *)
{AChristoffelName=GiveSymbol[AChristoffel,covd],
FRiemannName=GiveSymbol[FRiemann,covd],
i1=tindexlist[[1]],
i2=tindexlist[[2]],
fi1=findexlist[[1]],
fi2=findexlist[[2]],
fi3=findexlist[[3]],
fi4=findexlist[[4]]},

(* 1. Define Christoffel tensor relating covd to PD. Always nonzero *)
DefTensor[AChristoffelName[fi1,-i2,-fi3],DependenciesOfCovD[covd],StrongGenSet[{},GenSet[]],
PrintAs:>GiveOutputString[AChristoffel,covd],
Dagger:>If[DaggerQ[fbundle],Complex,Real],
Master->covd,
DefInfo:>If[info,{"nonsymmetric AChristoffel tensor",""},False],
TensorID->{AChristoffel,covd,PD}];
AppendTo[$Christoffels,{AChristoffelName,{AChristoffel,covd,PD},{tbundle,fbundle}}];

(* 2. Define FRiemann tensor *)
DefTensor[FRiemannName[-i1,-i2,-fi3,fi4],DependenciesOfCovD[covd],Antisymmetric[{1,2}],
PrintAs:>GiveOutputString[FRiemann,covd],
Dagger:>If[DaggerQ[fbundle],Complex,Real],
VanishingQ:>!curvQ,
Master->covd,
DefInfo:>If[info,{"FRiemann tensor",If[curvQ,"Antisymmetric only in the first pair.",""]},False],
TensorID->{FRiemann,covd}
];

]
]


InducedCovDQ[covd_]:=False;


(* CovDTODO *)
SortCovDsStart[PD]:=Module[{},
If[Not@$CommuteCovDsOnScalars,Print["Note that $CommuteCovDsOnScalars is still False."]];
Unprotect[PD];
PD[-b_Symbol][PD[-a_Symbol][expr1_]]:=PD[-a][PD[-b][expr1]]/;DisorderedPairQ[-a,-b];
If[PD===CovDOfMetric@FirstMetricOfVBundle[#,False],
With[{vbQ=VBundleIndexPMQ[#]},
PD[b_?vbQ][PD[a_?vbQ][expr1_]]:=PD[a][PD[b][expr1]]/;DisorderedPairQ[-a,-b]]
]&/@Select[$VBundles,MetricEndowedQ];
Protect[PD];
];


SortCovDsStart[covd_?CovDQ]:=With[{
tbQ=VBundleIndexQ[TangentBundleOfCovD[covd]],
tbpmQ=VBundleIndexPMQ[TangentBundleOfCovD[covd]],
metric=MetricOfCovD[covd]},
If[Not@$CommuteCovDsOnScalars,Print["Note that $CommuteCovDsOnScalars is still False."]];
If[metric=!=Null&&!FrozenMetricQ[metric],
covd[b_?tbpmQ][covd[a_?tbpmQ][expr1_]]:=makeCommuteCovDs[expr1,covd,{a,b}]/;DisorderedPairQ[a,b],
covd[-b_?tbQ][covd[-a_?tbQ][expr1_]]:=makeCommuteCovDs[expr1,covd,{-a,-b}]/;DisorderedPairQ[-a,-b]
]
];


SortCovDsStart[list_List]:=SortCovDsStart/@list
SortCovDsStart[covd_]:=Message[SortCovDsStart::unknown,"covariant derivative",covd]
SetNumberOfArguments[SortCovDsStart,1];
Protect[SortCovDsStart];


(* CovDTODO *)
SortCovDsStop[PD]:=Module[{},
If[$CommuteCovDsOnScalars,Print["Note that $CommuteCovDsOnScalars is still True."]];
Unprotect[PD];
PD/:PD[-b_Symbol][PD[-a_Symbol][expr1_]]=.;
If[PD===CovDOfMetric@FirstMetricOfVBundle[#,False],
With[{vbQ=VBundleIndexPMQ[#]},
PD/:PD[b_?vbQ][PD[a_?vbQ][expr1_]]=.]
]&/@Select[$VBundles,MetricEndowedQ];
Protect[PD];
];


SortCovDsStop[covd_?CovDQ]:=With[{
tbQ=VBundleIndexQ[TangentBundleOfCovD[covd]],
tbpmQ=VBundleIndexPMQ[TangentBundleOfCovD[covd]],
metric=MetricOfCovD[covd]
},
If[$CommuteCovDsOnScalars,Print["Note that $CommuteCovDsOnScalars is still True."]];
If[metric=!=Null&&!FrozenMetricQ[metric],
covd[b_?tbpmQ][covd[a_?tbpmQ][expr1_]]=.,
covd[-b_?tbQ][covd[-a_?tbQ][expr1_]]=.
]
];


SortCovDsStop[covd_]:=Message[SortCovDsStop::unknown,"covariant derivative",covd]
SetNumberOfArguments[SortCovDsStop,1];
Protect[SortCovDsStop];


xTensorQ[TensorDerivative[tensor_,ders___]]^:=And[xTensorQ[tensor],And@@(TensorDerQ/@{ders})];


TensorDerQ[covd_?CovDQ]:=True;
TensorDerQ[LieD[v_?xTensorQ[_]]]:=True;
TensorDerQ[ParamD[_Symbol?ParameterQ]]:=True;
TensorDerQ[OverDot]:=True;
TensorDerQ[_]:=False;


SlotOfDerivative[PD]:=-All;
(* Recall that the first vbundle of a covd is the tangent covd *)
SlotOfDerivative[covd_?CovDQ]:=-First@VBundlesOfCovD[covd];
SlotOfDerivative[_ParamD|OverDot|_LieD]:=Sequence[];
SlotsOfTensor[TensorDerivative[tensor_,ders__]]^:=Join[SlotsOfTensor[tensor],SlotOfDerivative/@{ders}];


PrefixFormatDer[ParamD[par_]]:=SubscriptBox["\[PartialD]",PrintAs[par]];
PrefixFormatDer[OverDot]:=SubscriptBox["\[PartialD]","#"];


PrefixFormatDer[covd_?HeldCovDQ]:=Last[SymbolOfCovD[covd]];


PrintAs[TensorDerivative[tensor_,ders___]]^:=RowBox[Append[PrefixFormatDer/@Reverse[{ders}],PrintAs[tensor]]];


Dagger[TensorDerivative[tensor_,ders__]]^:=TensorDerivative[Dagger[tensor],ders];


SymmetryGroupOfTensor[TensorDerivative[tensor_,ders__][inds___]]^:=JoinSGS[SymmetryGroupOfTensor[tensor],Apply[Sequence,SymDers/@Split[Transpose[{{ders},Range[Length[{ders}]]+Length[{inds}]-Length[{ders}]}],First[#1]===First[#2]&]]];
SymDers[{{PD,n1_},{PD,_}...,{PD,nl_}}]:=SGSofsym@Symmetric[Range[n1,nl]];
SymDers[{{covd_?CovDQ,n1_},{covd_,_}...,{covd_,nl_}}]:=SGSofsym@Symmetric[Range[n1,nl]]/;!CurvatureQ[covd]&&!TorsionQ[covd];
SymDers[list_]:=StrongGenSet[{},GenSet[]];


TensorDerivative[tensor_?xTensorQ][inds___]:=tensor[inds];
TensorDerivative[TensorDerivative[tensor_,ders1___],ders2___][inds___]:=TensorDerivative[tensor,ders1,ders2][inds];
TensorDerivative[expr_Plus,ders___][inds___]:=TensorDerivative[#,ders][inds]&/@expr;
TensorDerivative[expr_SeriesData,ders___][inds___]:=SeriesDataMap[TensorDerivative[#,ders][inds]&,expr];


ToTensorDerivative[expr_]:=expr//.{
covd_?CovDQ[index_][tensor_?xTensorQ[indices___]]:>ToTensorDerivative1[covd[index],tensor[indices]],
LieD[v_][tensor_?xTensorQ[indices___]]:>ToTensorDerivative1[LieD[v],tensor[indices]]
};


ToTensorDerivative1[der_,tensor_[indices___]]:=With[{tslots=SlotsOfTensor[tensor],islots=SignedVBundleOfIndex/@{indices}},
If[Length[tslots]=!=Length[islots],Throw@Message[ToTensorDerivative::error,"Incompatible number of indices."]];
TDI[der,tensor][indices]+Apply[Plus,MapIndexed[ToTensorDerivativeIndex[der,tensor[indices]],Transpose[{{indices},tslots,islots}]]]
];


TDI[covd_?CovDQ[index_],tensor_][indices___]:=TensorDerivative[tensor,covd][indices,index];
TDI[der_,tensor_][indices___]:=TensorDerivative[tensor,der][indices];


(* On abstract indices: {index, vb-should-be, vb-it-is} *)
ToTensorDerivativeIndex[_,t_][{_LI|-_LI,_,_},{n_}]:=0;
ToTensorDerivativeIndex[_,t_][{i_?AIndexQ,All,_Symbol},{n_}]:=0;
ToTensorDerivativeIndex[_,t_][{i_?AIndexQ,-All,-_Symbol},{n_}]:=0;
ToTensorDerivativeIndex[_,t_][{i_?AIndexQ,vb_,vb_},{n_}]:=0;
ToTensorDerivativeIndex[der_,t_][{i_?AIndexQ,All,-vb_?VBundleQ},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb]},ReplacePart[t,n:>dummy]TDI[der,metric][-dummy,i]];
ToTensorDerivativeIndex[der_,t_][{i_?AIndexQ,vb_,-vb_},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb]},ReplacePart[t,n:>dummy]TDI[der,metric][-dummy,i]];
ToTensorDerivativeIndex[der_,t_][{i_?AIndexQ,-All,vb_?VBundleQ},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb]},-ReplacePart[t,n:>-dummy]TDI[der,metric][dummy,i]];
ToTensorDerivativeIndex[der_,t_][{i_?AIndexQ,-vb_,vb_},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb]},-ReplacePart[t,n:>-dummy]TDI[der,metric][dummy,i]];
(* On basis indices *)
ToTensorDerivativeIndex[der_,t_][{i:{_,basis_?BasisQ},vb_,vb_?VBundleQ},{n_}]:=With[{dummy=DummyIn[vb],dummyb=DummyIn[vb,basis]},ReplacePart[t,n:>dummyb]ReplaceAll[der[Basis[-dummy,i]],-dummy->-dummyb]];
ToTensorDerivativeIndex[der_,t_][{i:{_,-basis_?BasisQ},-vb_,-vb_?VBundleQ},{n_}]:=With[{dummy=DummyIn[vb],dummyb=DummyIn[vb,basis]},ReplacePart[t,n:>-dummyb]ReplaceAll[der[Basis[i,dummy]],dummy->dummyb]];
ToTensorDerivativeIndex[der_,t_][{i:{_,basis_?BasisQ},-vb_,vb_?VBundleQ},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb],dummyb=DummyIn[vb,basis]},ReplacePart[t,n:>-dummyb](-TDI[der,metric][dummyb,i]+ReplaceAll[der[Basis[-dummy,i]],-dummy->dummyb])];
ToTensorDerivativeIndex[der_,t_][{i:{_,-basis_?BasisQ},vb_,-vb_?VBundleQ},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb],dummyb=DummyIn[vb,basis]},ReplacePart[t,n:>dummyb](TDI[der,metric][-dummyb,i]+ReplaceAll[der[Basis[i,dummy]],dummy->-dummyb])];


FromTensorDerivative[expr_]:=SameDummies@Expand[expr//.{
TensorDerivative[tensor_,ders___,covd_?CovDQ][inds___,ind_]:>FromTensorDerivative1[covd[ind],TensorDerivative[tensor,ders][inds]],
TensorDerivative[tensor_,ders___,LieD[v_]][inds___]:>FromTensorDerivative1[LieD[v],TensorDerivative[tensor,ders][inds]]
}];


FromTensorDerivative1[der_,tensor_[indices___]]:=With[{tslots=SlotsOfTensor[tensor],islots=SignedVBundleOfIndex/@{indices}},
der[tensor[indices]]+Apply[Plus,MapIndexed[FromTensorDerivativeIndex[der,tensor[indices]],Transpose[{{indices},tslots,islots}]]]];


(* On abstract indices *)
FromTensorDerivativeIndex[_,t_][{i_?AIndexQ,vb_,vb_},{n_}]:=0;
FromTensorDerivativeIndex[_,t_][{i_?AIndexQ,All,_Symbol},{n_}]:=0;
FromTensorDerivativeIndex[_,t_][{i_?AIndexQ,-All,-_Symbol},{n_}]:=0;
FromTensorDerivativeIndex[der_,t_][{i_?AIndexQ,vb_,-vb_},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb]},-ReplacePart[t,n:>dummy]der[metric[-dummy,i]]];
FromTensorDerivativeIndex[der_,t_][{i_?AIndexQ,-vb_,vb_},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb]},ReplacePart[t,n:>-dummy]der[metric[dummy,i]]];
(* On basis indices *)
FromTensorDerivativeIndex[der_,t_][{i:{_,basis_?BasisQ},vb_?VBundleQ,vb_},{n_}]:=With[{dummy=DummyIn[vb],dummyb=DummyIn[vb,basis]},-ReplacePart[t,n:>dummyb]ReplaceAll[der[Basis[-dummy,i]],-dummy->-dummyb]];
FromTensorDerivativeIndex[der_,t_][{i:{_,-basis_?BasisQ},-vb_?VBundleQ,-vb_},{n_}]:=With[{dummy=DummyIn[vb],dummyb=DummyIn[vb,basis]},-ReplacePart[t,n:>-dummyb]ReplaceAll[der[Basis[i,dummy]],dummy->dummyb]];
FromTensorDerivativeIndex[der_,t_][{i:{_,basis_?BasisQ},-vb_?VBundleQ,vb_},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb],dummyb=DummyIn[vb,basis],dummyc=DummyIn[vb]},-ReplacePart[t,n:>-dummyb](-metric[dummyb,dummy]metric[i,dummyc]FromTensorDerivative1[der,metric[-dummy,-dummyc]]+ReplaceAll[der[Basis[-dummy,i]],-dummy->dummyb])];
FromTensorDerivativeIndex[der_,t_][{i:{_,-basis_?BasisQ},vb_,-vb_?VBundleQ},{n_}]:=With[{metric=FirstMetricOfVBundle[vb],dummy=DummyIn[vb],dummyb=DummyIn[vb,basis]},-ReplacePart[t,n:>dummyb](FromTensorDerivative1[der,metric[-dummyb,i]]+ReplaceAll[der[Basis[i,dummy]],dummy->-dummyb])];


(* Derivative properties *)
TensorD[expr_,der1_,ders__]:=TensorD[TensorD[expr,der1],ders];
TensorD[expr_Plus,der_]:=TensorD[#,der]&/@expr;
TensorD[expr_SeriesData,der_]:=SeriesDataMap[TensorD[#,der]&,expr];
(* Assumes Leibnitz. This could be wrong for induced derivatives *)
TensorD[expr1_ expr2_,der_]:=TensorD[expr1,der]expr2+expr1 TensorD[expr2,der];
TensorD[_?ConstantQ,der_]:=0;
TensorD[f_?ScalarFunctionQ[args___],der_]:=multiD[TensorD[#,der]&,f[args]];
TensorD[Scalar[expr_],der_]:=TensorD[ReplaceDummies[expr],der];
TensorD[list_List,der_]:=TensorD[#,der]&/@list;
(* On particular tensors *)
TensorD[t:(_?xTensorQ[__]),der_]:=ToTensorDerivative1[der,t];


PullBackCovD[covd_?CovDQ,_IdentityMapping]:=covd;


(* Basic definitions *)
CovDQ[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=True;
VBundlesOfCovD[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=PrecomposeVBundles[BundlesOfCovD[covd],phi];
DependenciesOfCovD[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=PrecomposeDependencies[DependenciesOfCovD[covd],phi];
ManifoldOfCovD[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=PrecomposeDependency[ManifoldOfCovD[covd],phi];
MetricOfCovD[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=Precompose[MetricOfCovD[covd],phi];
SymbolOfCovD[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:={#1,PullBackString[#2,phi]}&@@SymbolOfCovD[covd];
WeightedWithBasis[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=PrecomposeWeight[WeightedWithBasis[covd],phi];


(* Declare as a covariant derivative *)
MakeLinearDerivative[{PullBackCovD[covd_?CovDQ,phi_?MappingQ][a_],PullBackCovD[covd,phi][a]},True];
MakeTensorialDerivative[{PullBackCovD[covd_?CovDQ,phi_?MappingQ],PullBackCovD[covd,phi]}];


(* Curvature: we assume it is preserved *)
CurvatureQ[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=CurvatureQ[covd];
TorsionQ[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=TorsionQ[covd];
(* Associated tensors *)
Riemann[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=Precompose[Riemann[covd],phi];
Ricci[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=Precompose[Ricci[covd],phi];
RicciScalar[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=Precompose[RicciScalar[covd],phi];
Einstein[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=Precompose[Einstein[covd],phi];
Weyl[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=Precompose[Weyl[covd],phi];
TFRicci[PullBackCovD[covd_?CovDQ,phi_?MappingQ]]^:=Precompose[TFRicci[covd],phi];


PrintAs[PullBackCovD[covd_,phi_]]^:=PullBackString[PrintAs[covd],phi];


SetAttributes[ValidateCovD,HoldFirst];


(* CovDTODO *)
ValidateCovD[x:der_[ind_][expr_]]:=Catch[
(* Check derivative name *)
If[!CovDQ[der],Throw[Message[Validate::unknown,"covariant derivative",der];ERROR[x]]];
(* Check index. Patterns are not accepted *)
If[!GIndexQ[ind],Throw[Message[Validate::unknown,"g-index",ind];ERROR[x]]];
(* Check expression *)
UncatchedValidate[Unevaluated[expr]];
(* Check that derivative and index are compatible *)
If[!IndexOnQ[ind,TangentBundleOfCovD[der[ind]]],
Throw[Message[Validate::invalid,ind,"index for derivative"<>ToString[der]];ERROR[x]]];
(* Check that there is a metric if index is an up-index *)
If[UpIndexQ[ind]&&!MetricEndowedQ[VBundleOfIndex[ind]],Throw[Message[Validate::invalid,ind,"index for a derivative because ther is no metric in its vector bundle"];ERROR[x]]];
x];


SetAttributes[HeldCovDQ,HoldAllComplete];
HeldCovDQ[expr_]:=CovDQ[Unevaluated[expr]];


$CovDFormat="Prefix";
xTensorFormStart[CovD]:=
(MakeBoxes[covd_Symbol?HeldCovDQ[inds__][expr_],StandardForm]:=Block[{$WarningFrom="CovD Formatting"},interpretbox[covd[inds][expr],MakeBoxesCovD[Unevaluated[covd][inds][boxof@BracketizedBoxesIfTimes[expr,StandardForm]],$CovDFormat]]];
MakeBoxes[CovD[expr_,ders__,covd_?HeldCovDQ[inds__]],StandardForm]:=Block[{$WarningFrom="CovD Formatting"},interpretbox[CovD[expr,ders,covd[inds]],MakeBoxesCovD[Unevaluated[covd][inds][boxof@MakeBoxes[CovD[expr,ders],StandardForm]],$CovDFormat]]];MakeBoxes[CovD[expr_,covd_?HeldCovDQ[inds__]],StandardForm]:=Block[{$WarningFrom="CovD Formatting"},interpretbox[CovD[expr,covd[inds]],MakeBoxesCovD[Unevaluated[covd][inds][boxof@MakeBoxes[expr,StandardForm]],$CovDFormat]]]);
xTensorFormStop[CovD]:=(
MakeBoxes[covd_Symbol?HeldCovDQ[inds__][expr_],StandardForm]=.;MakeBoxes[CovD[expr_,ders__,covd_?HeldCovDQ[inds__]],StandardForm]=.;MakeBoxes[CovD[expr_,covd_?HeldCovDQ[inds__]],StandardForm]=.);
xTensorFormStart[CovD]


SetAttributes[BracketizedBoxesIfTimes,HoldFirst];
BracketizedBoxesIfTimes[expr_Times,format_]:=RowBox[{"[",MakeBoxes[expr,format],"]"}];
BracketizedBoxesIfTimes[expr_,format_]:=MakeBoxes[expr,format];


(* Non-directional derivatives in Postfix notation. Handle both SubsuperscriptBox and GridBox *)
MakeBoxesCovD[der_[i_/;Head[i]=!=Dir][SubsuperscriptBox[head_,downstr_,upstr_]],"Postfix"]:=Block[{$WarningFrom="CovD Formatting"},SubsuperscriptBox[head,#1,#2]&@@AddIndex[{downstr,upstr},i,der]];
MakeBoxesCovD[der_[i_/;Head[i]=!=Dir][gridbox_GridBox],"Postfix"]:=Block[{$WarningFrom="CovD Formatting"},ReplacePart[gridbox,AppendIndexPair[gridbox[[1,1,2,1,1]],Reverse@IndexPair[i,der]],{1,1,2,1,1}]];
(* Other cases: Prefix notation *)
MakeBoxesCovD[der_[i_][MBexpr_],str_String]:=Block[{$WarningFrom="CovD Formatting"},
FlattenRowBox@RowBox[{If[downQ[i],SubscriptBox,SuperscriptBox][Last@SymbolOfCovD@der,IndexForm@UpIndex[i]],MBexpr}]];


AppendIndexPair[grid_,pair_List]:=Transpose@Append[Transpose[grid],pair];
AppendIndexPair[grid_,pairs_IndexList]:=Transpose@Join[Transpose[grid],List@@pairs];


(* Postfix notation *)
MakeBoxesCovD[der_[inds__][SubsuperscriptBox[head_,downstr_,upstr_]],"Postfix"]:=Block[{$WarningFrom="CovD Formatting"},SubsuperscriptBox[head,#1,#2]&@@AddIndex[{downstr,upstr},IndexList[inds],der]];
MakeBoxesCovD[der_[ind_,inds__][gridbox_GridBox],"Postfix"]:=Block[{$WarningFrom="CovD Formatting"},
ReplacePart[gridbox,AppendIndexPair[AppendIndexPair[gridbox[[1,1,2,1,1]],Reverse@IndexPair[ind,der]],Reverse/@IndexPair/@IndexList[inds]],{1,1,2,1,1}]];
(* Prefix notation *)
MakeBoxesCovD[der_[inds__][MBexpr_],"Prefix"]:=Block[{$WarningFrom="CovD Formatting"},
FlattenRowBox@RowBox[{SubsuperscriptBox[Last@SymbolOfCovD@der,#1,#2]&@@SSSBinds[{inds}],MBexpr}]];


FlattenRowBox[RowBox[{box1_,RowBox[{boxes___}]}]]:=RowBox[{box1,boxes}];
FlattenRowBox[expr_]:=expr;


Unprotect[OverDot];
(* Nest high-order derivatives *)
HoldPattern[OverDot[expr_,1]]:=OverDot[expr];
OverDot[expr_,n_Integer?Positive]:=OverDot[OverDot[expr,n-1]];
(* Derivative properties *)
OverDot[expr_Plus]:=OverDot/@expr;
OverDot[expr_SeriesData]:=SeriesDataMap[OverDot,expr];
OverDot[x_ y_]:=OverDot[x]y+x OverDot[y];
OverDot[x_?ConstantQ]:=0;
(* Scalar functions *)
OverDot[f_?ScalarFunctionQ[args___]]:=multiD[OverDot,f[args]];
(* No need to replace dummies internally *)
OverDot[Scalar[expr_]]:=Scalar[OverDot[expr]];
(* Listability *)
OverDot[list_List]:=Map[OverDot,list];


OverDot[PD[-ind_][expr_]]:=PD[-ind][OverDot[expr]]
OverDot[PD[Dir[v_]][expr_]]:=PD[Dir[v]][OverDot[expr]]+PD[Dir[OverDot[v]]][expr]


SetNumberOfArguments[OverDot,{1,2}];
Protect[OverDot];


ParamD[][expr_]:=expr;
(* Unnest parametric derivatives, contrary to OverDot. Sort parameters, always assumed to commute *)
ParamD[p1__][ParamD[p2__][expr_]]:=(ParamD@@Sort[{p1,p2}])[expr]
(* Properties of a derivative *)
ParamD[p__][expr_Plus]:=ParamD[p]/@expr;
ParamD[p__][expr_SeriesData]:=SeriesDataMap[ParamD[p],expr];
ParamD[p___,p1_][x_ y_]:=ParamD[p][ParamD[p1][x]y+x ParamD[p1][y]];
ParamD[p___,p1_][p1_]:=ParamD[p][1];
ParamD[p__][_?ConstantQ]:=0;
ParamD[p__][expr_]:=0/;FreeQ[ParametersOf[expr],Alternatives[p,AnyDependencies]];
(* Scalar functions: sequentially apply chain rule *)
ParamD[p___,p1_][f_?ScalarFunctionQ[args___]]:=ParamD[p][multiD[ParamD[p1],f[args]]];
(* On Scalar. No need to replace internal dummies *)
ParamD[p__][Scalar[expr_]]:=Scalar[ParamD[p][expr]];
(* Listability *)
ParamD[p__][list_List]:=Map[ParamD[p],list];


SymbolOfCovD[ParamD]^={",","\[PartialD]"};


$ParamDFormat="Prefix";
xTensorFormStart[ParamD]:=
(MakeBoxes[ParamD[ps__][expr_],StandardForm]:=Block[{$WarningFrom="ParamD Formatting"},interpretbox[ParamD[ps][expr],MakeBoxesParamD[{ps},boxof@MakeBoxes[expr,StandardForm],$ParamDFormat,StandardForm]]]
);
xTensorFormStop[ParamD]:=(
MakeBoxes[ParamD[ps__][expr_],StandardForm]=.
);
xTensorFormStart[ParamD];


(* Postfix notation *)
MakeBoxesParamD[pslist_List,SubsuperscriptBox[head_,downstr_,upstr_],"Postfix",StandardForm]:=Block[{$WarningFrom="ParamD Formatting"},SubsuperscriptBox[head,#1,#2]&@@AddIndex[{downstr,upstr},Minus/@IndexList@@pslist,ParamD]];
MakeBoxesParamD[pslist_List,gridbox_GridBox,"Postfix",StandardForm]:=Block[{$WarningFrom="ParamD Formatting"},
ReplacePart[gridbox,AppendIndexPair[AppendIndexPair[gridbox[[1,1,2,1,1]],{"",First@SymbolOfCovD[ParamD]}],IndexPair/@IndexList@@pslist],{1,1,2,1,1}]];
MakeBoxesParamD[pslist_List,boxes_,"Postfix",StandardForm]:=Block[{$WarningFrom="ParamD Formatting"},SubscriptBox[boxes,StringJoin[First@SymbolOfCovD[ParamD],Sequence@@(PrintAs/@pslist)]]];
(* SinglePrefix notation *)
MakeBoxesParamD[pslist_List,boxes_,"SinglePrefix",StandardForm]:=Block[{$WarningFrom="ParamD Formatting"},RowBox[{SubscriptBox[Last@SymbolOfCovD[ParamD],StringJoin@@(PrintAs/@pslist)],boxes}]];
(* Prefix notation *)
MakeBoxesParamD[{},boxes_,"Prefix",StandardForm]:=boxes;
MakeBoxesParamD[{rest___,p_},boxes_,"Prefix",StandardForm]:=Block[{$WarningFrom="ParamD Formatting"},MakeBoxesParamD[{rest},FlattenRowBox@RowBox[{SubscriptBox[Last@SymbolOfCovD[ParamD],PrintAs[p]],boxes}],"Prefix",StandardForm]];


(******************* 12.Lie derivatives and brackets ******************)


If[$ReadingVerbose,Print["Reading section 12: Lie derivatives."],Null,Null]


(* Properties of a derivative *)
LieD[v_][expr_Plus]:=LieD[v]/@expr;
LieD[v_][expr_SeriesData]:=SeriesDataMap[LieD[v],expr];
LieD[v_][expr1_ expr2_]:=LieD[v][expr1]expr2+expr1 LieD[v][expr2];
LieD[v_][_?ConstantQ]:=0;
LieD[v_][f_?ScalarFunctionQ[args___]]:=multiD[LieD[v],f[args]];
(* On Scalar. No need to replace internal dummies *)
LieD[v_][Scalar[expr_]]:=Scalar[LieD[v][expr]];
(* Listability *)
LieD[v_][list_List]:=Map[LieD[v],list];
(* Vector field argument. Not tensorial! Just linear *)
LieD[v_Plus][expr_]:=LieD[#][expr]&/@v;
LieD[v_SeriesData][expr_]:=SeriesDataMap[LieD[#][expr]&,v];
LieD[(x_?ConstantQ)v_][expr_]:=x LieD[v][expr];
LieD[0][expr_]:=0;
(* Two special useful cases *)
LieD[v_?xTensorQ[_Symbol]][v_[_Symbol]]:=0;
LieD/:v_?xTensorQ[i_Symbol]LieD[v_[_Symbol]][expr_]:=0/;HasOrthogonalIndexQ[expr,v[i]];


LieD[vector_,covd_][f_?ScalarFunctionQ[args___]]:=multiD[LieD[vector,covd],f[args]];
LieD[vector_,covd_][Scalar[expr_]]:=Scalar[LieD[vector,covd][expr]];
LieD[vector_,covd_][expr_]:=ExpandLieD[ReplaceDummies[vector],covd,False,Identity1][expr];
ExpandLieD[vector_,covd_,dirQ_,tmpcovdhead_][expr_]:=
Expand[tmpcovdhead[ If[dirQ,Identity,SeparateDir][covd[Dir[vector]][expr]],LieD ]+lieDcovDdiff[expr,ReplaceDummies[vector],covd,VBundleOfIndex@UltraindexOf[vector],Identity]
];
CovDToLieD[covd_[Dir[vector_]][expr_],tmpchrhead_]:=LieD[vector][expr]-lieDcovDdiff[expr,ReplaceDummies[vector],covd,VBundleOfIndex[Dir[vector]],tmpchrhead];
lieDcovDdiff[expr_,vector_,covd_,vb_,tmpchrhead_]:=With[{indv=UltraindexOf[vector],vbQ=VBundleIndexQ[vb],inds=FindFreeIndices[expr],dummy=DummyIn[vb],dummy2=DummyIn[vb]},

(* Checks *)
If[!CovDQ[covd],Throw[Message[LieD::unknown,"derivative",covd];ERROR[LieD[vector,covd][expr]]]];
ValidateDir[Dir[vector]];
If[covd=!=PD&&ManifoldOfCovD[covd]=!=BaseOfVBundle[vb],Throw[Message[LieD::error,"Invalid derivative in Lie Derivative."];ERROR[LieD[vector,covd][expr]]]];

(* CovDTODO *)
(* Additional terms *)
With[{torsion=Torsion[covd],newv=If[TorsionQ[covd],ReplaceIndex[vector,indv->dummy2],0]},
Expand[
Plus@@Map[(tmpchrhead[covd[-#][ReplaceIndex[vector,indv->dummy]]]+$TorsionSign newv torsion[dummy,-dummy2,-#])ReplaceIndex[expr,-#->-dummy]&,Select[ChangeIndex/@inds,vbQ] ]-Plus@@Map[(tmpchrhead[covd[-dummy][ReplaceIndex[vector,indv->#]]]+$TorsionSign newv torsion[#,-dummy2,-dummy])ReplaceIndex[expr,#->dummy]&,Select[inds,vbQ] ]+
If[WeightedCovDQ[covd],
With[{weight=WeightOf[expr,WeightedWithBasis[covd]]},
If[weight=!=0,weight expr covd[-indv][vector],0]
],
0
]
]
]
];
(* Messages *)
LieD[_][]:=Throw@Message[LieD::argx,LieD,0]
LieD[][_,x__]:=Throw@Message[LieD::argx,LieD,1+Length[{x}]]
LieD[][_]:=Throw@Message[LieD::error,"There is no contravariant vector"]
LieD[_,_,__][_]:=Throw@Message[LieD::error,"Too many arguments for LieD."]
Protect[LieD];


LieDToCovD[expr_,covd_:PD]:=expr//.LieD[vector_]:>LieD[vector,covd];
SetNumberOfArguments[LieDToCovD,{1,2}];
Protect[LieDToCovD];


DirCovDToLieD[expr_,vector_]:=expr/.(rest_. vector[a_]covd_?CovDQ[-a_][expr1_]|rest_. vector[-a_]covd_?CovDQ[a_][expr1_]|rest_. covd_?CovDQ[Dir[vector[a_]]][expr1_]):>rest CovDToLieD[covd[Dir[vector[a]]][expr1],Identity];
SetNumberOfArguments[DirCovDToLieD,2];
Protect[DirCovDToLieD];


SetAttributes[ValidateLieD,HoldFirst];
ValidateLieD[x:LieD[v_][expr_]]:=Catch[
ValidateDir[Dir[v]];
UncatchedValidate[Unevaluated[expr]];
x
];
Protect[ValidateLieD];


PrefixFormatDer[LieD[v_?HeldxTensorQ[i_]]]:=SubscriptBox["\[ScriptCapitalL]",PrintAs[v]];
PrefixFormatDer[LieD[v_?HeldxTensorQ[label_LI,i_]]]:=SubscriptBox["\[ScriptCapitalL]",boxof@MakeBoxes[v[label],StandardForm]];
PrefixFormatDer[LieD[v_]]:=SubscriptBox["\[ScriptCapitalL]",boxof@MakeBoxes[Short[v],StandardForm]];


xTensorFormStart[LieD]:=(MakeBoxes[LieD[v_][expr_],StandardForm]:=interpretbox[LieD[v][expr],RowBox[{PrefixFormatDer[LieD[v]],boxof@MakeBoxes[expr,StandardForm]}]]);
xTensorFormStop[LieD]:=(MakeBoxes[LieD[v_][expr_],StandardForm]=.);
xTensorFormStart[LieD];


xTensorQ[Bracket[v1_,v2_]]^:=True;
SlotsOfTensor[Bracket[v1_,v2_]]^:={VBundleOfIndex[First@FindFreeIndices[v1]]};
SymmetryGroupOfTensor[Bracket[v1_,v2_]]^:=StrongGenSet[{},GenSet[]];
DependenciesOfTensor[Bracket[v1_,v2_]]^:=Union[DependenciesOf[v1],DependenciesOf[v2]];
WeightOfTensor[Bracket[v1_,v2_]]^:=WeightOf[v1]+WeightOf[v2];
Dagger[Bracket[v1_,v2_]]^:=Bracket[Dagger[v1],Dagger[v2]];


SetAttributes[ValidateBracket,HoldFirst];
ValidateBracket[x:Bracket[v1_,v2_][i_]]:=Catch@Module[{
vb=VBundleOfIndex[i],
i1=UltraindexOf[v1],
i2=UltraindexOf[v2]},
If[DownIndexQ[i1]||DownIndexQ[i2],Throw[Message[Validate::error,"Indices in the vectors of Bracket must be all contravariant"];ERROR[x]]];
If[VBundleOfIndex[i1]=!=VBundleOfIndex[i2],Throw[Message[Validate::error,"Vectors in Bracket belong to different manifolds"];ERROR[x]]];
If[VBundleOfIndex[i1]=!=vb,Throw[Message[Validate::error,"Index in Bracket is incompatible with indices of vectors"];ERROR[x]]];
ValidateDir[Dir[v1]];
ValidateDir[Dir[v2]];
x
];
ValidateBracket[0]:=0;


Bracket[v_,v_]:=Zero;


Bracket[0,v2_]:=Zero;
Bracket[v1_,0]:=Zero;
Bracket[v1_Plus,v2_][a__]:=Bracket[#,v2][a]&/@v1;
Bracket[v1_SeriesData,v2_][a__]:=SeriesDataMap[Bracket[#,v2][a]&,v1];
Bracket[v1_,v2_Plus][a__]:=Bracket[v1,#][a]&/@v2;
Bracket[v1_,v2_SeriesData][a__]:=SeriesDataMap[Bracket[v1,#][a]&,v2];
Bracket[c_?ConstantQ v1_,v2_][a__]:=c Bracket[v1,v2][a];
Bracket[v1_,c_?ConstantQ v2_][a__]:=c Bracket[v1,v2][a];


Bracket[s_?ScalarQ v1_,v2_][a_]:=With[{rs=ReplaceDummies[s]},
rs Bracket[v1,v2][a]-PD[Dir[v2]][rs]ReplaceIndex[v1,UltraindexOf[v1]->a]
];
Bracket[v1_,s_?ScalarQ v2_][a_]:=With[{rs=ReplaceDummies[s]},
rs Bracket[v1,v2][a]+PD[Dir[v1]][rs]ReplaceIndex[v2,UltraindexOf[v2]->a]
];


BracketToCovD[expr_,covd_:PD]:=expr/.Bracket[v1_,v2_][a_]:>With[{u1=UltraindexOf[v1],u2=UltraindexOf[v2]},With[{b=DummyAs[u1],c=DummyAs[u2],rv1=ReplaceDummies[v1],rv2=ReplaceDummies[v2]},
ReplaceIndex[rv1,u1->b]covd[-b][ReplaceIndex[rv2,u2->a]]-ReplaceIndex[rv2,u2->b]covd[-b][ReplaceIndex[rv1,u1->a]]-$TorsionSign Torsion[covd][a,-b,-c]ReplaceIndex[rv1,u1->b]ReplaceIndex[rv2,u2->c]
]
];


PrintAs[Bracket[v1_,v2_]]^:=Block[{$WarningFrom="Bracket Formatting"},RowBox[{"[",boxof@MakeBoxes[v1,StandardForm],",",boxof@MakeBoxes[v2,StandardForm],"]"}]
];


Bracket[expr1_,expr2_][i_]:=-Bracket[expr2,expr1][i]/;OrderedQ[{expr2,expr1}]


PullBack[expr_Plus,phi_,firules_]:=PullBack[#,phi,firules]&/@expr;
PullBack[expr_,phi_,firules_]:=
With[{dom=MappingDomain[phi],im=MappingImage[phi],fdindices=FindFreeAndDummyIndices[expr]},
With[{allirules=PBIndexRules[fdindices,firules,Tangent[dom],Tangent[im]]},
Apply[Times,PullBack1[#,phi,allirules]&/@ListOfFactors[expr]]
]
];


PullBack1[tensor_?xTensorQ[inds___],phi_,allirules_List]:=With[{newinds=ReplaceIndexList[IndexList[inds],allirules]},
If[newinds===IndexList[inds],tensor[inds],PullBackTensor[tensor,phi]@@newinds]
];
(* TODO: What happens if covd[_][dphi] is not zero? *)
PullBack1[covd_?CovDQ[inds__][expr_],phi_,allirules_List]:=Apply[PullBackCovD[covd,phi],ReplaceIndexList[IndexList[inds],allirules]][PullBack1[expr,phi,allirules]];
(* TODO: We need something special for the ultraindex *)
PullBack1[LieD[v_][expr_],phi_,allirules_List]:=LieD[PullBack1[v,phi,allirules]][PullBack1[expr,phi,allirules]];
PullBack1[c_?ConstantQ,phi_,allirules_List]:=c;


PullBack::frees="Missing pull-back rules for free indices `1` of tangent bundle `2`.";


PBIndexRules[{frees_IndexList,dummies_IndexList},firules_List,tdom_,tim_]:=With[{timpmQ=VBundleIndexPMQ[tim]},
If[#=!={},Throw[Message[PullBack::frees,#,tim]]]&@Complement[Select[List@@frees,timpmQ],First/@firules];Join[firules,Flatten[{Rule@@#,Rule@@(-#)}&/@({#,DummyIn[tdom]}&/@Select[List@@dummies,timpmQ])]]
];


ExpandPullBack[expr_]:=expr/.{
PullBackTensor[tensor_?xTensorQ,phi_?MappingQ][inds___]:>ExpandPullBackTensor[tensor,phi,{inds}],
PullBackCovD[covd_?CovDQ,phi_?MappingQ][ind_][tensor_?xTensorQ[inds___]]:>ExpandPullBackCovD[covd,phi,{ind},tensor,{inds}],
TensorDerivative[Precompose[tensor_?xTensorQ,phi_],PullBackCovD[covd_?CovDQ,phi_?MappingQ]][inds___,ind_]:>ExpandPullBackCovD[covd,phi,{ind},tensor,{inds}]
};


ExpandPullBackTensor[tensor_,phi_,inds_List]:=With[{dphi=TangentTensor[phi],tim=Tangent[MappingImage[phi]]},
With[{ilist=Transpose[pullfactor1[#,dphi,tim,PullBackVBundle[tim,phi]]&/@Transpose[{SlotsOfTensor[tensor],inds}]]},
Precompose[tensor,phi]@@First[ilist]Times@@Last[ilist]
]
];
pullfactor1[{-tim_,ind_?DownIndexQ},dphi_,tim_,ptim_]:=With[{new=DummyIn[ptim]},{-new,dphi[ind,new]}];
pullfactor1[{tim_,ind_?UpIndexQ},dphi_,tim_,ptim_]:=With[{new=DummyIn[ptim]},{new,Inv[dphi][-new,ind]}];
pullfactor1[{_,ind_},dphi_,tim_,ptim_]:={ind,1};


ExpandPullBackCovD[covd_,phi_,{ind_},tensor_,inds_List]:=With[{dphi=TangentTensor[phi],new=DummyIn[PullBackVBundle[VBundleOfIndex[ind],phi]]},
dphi[ind,new]Precompose[TensorDerivative[tensor,covd],phi]@@Join[inds,{-new}]
];


PushForward[expr_Plus,phi_,firules_]:=PushForward[#,phi,firules]&/@expr;
PushForward[expr_,phi_,firules_]:=
With[{dom=MappingDomain[phi],im=MappingImage[phi],fdindices=FindFreeAndDummyIndices[expr]},
With[{allirules=PFIndexRules[fdindices,firules,Tangent[dom],Tangent[im]]},
Apply[Times,PushForward1[#,phi,allirules]&/@ListOfFactors[expr]]
]
];


PushForward1[tensor_?xTensorQ[inds___],phi_,allirules_List]:=With[{newinds=ReplaceIndexList[IndexList[inds],allirules]},
If[newinds===IndexList[inds],tensor[inds],PushForwardTensor[tensor,phi]@@newinds]
];
(* What is the push-forward of a covariant derivative? *)
PushForward1[covd_?CovDQ[inds__][expr_]]:=TODO;
(* TODO: We need something special for the ultraindices *)
PushForward1[LieD[v_][expr_],phi_,allirules_List]:=LieD[PushForward1[v,phi,allirules]][PushForward1[expr,phi,allirules]];
PushForward1[c_?ConstantQ,phi_,allirules_List]:=c;


PushForward::frees="Missing push-forward rules for free indices `1` of tangent bundle `2`.";


PFIndexRules[{frees_IndexList,dummies_IndexList},firules_List,tdom_,tim_]:=With[{tdompmQ=VBundleIndexPMQ[tdom]},
If[#=!={},Throw[Message[PushForward::frees,#,tdom]]]&@Complement[Select[List@@frees,tdompmQ],First/@firules];Join[firules,Flatten[{Rule@@#,Rule@@(-#)}&/@({#,DummyIn[tim]}&/@Select[List@@dummies,tdompmQ])]]
];


ExpandPushForward[expr_]:=expr/.{
LinearPush[tensor_?xTensorQ,dphi_?xTensorQ][inds___]:>ExpandLinearPush[tensor,dphi,{inds}],
PushForwardTensor[tensor_?xTensorQ,phi_?MappingQ][inds___]:>ExpandPushForwardTensor[tensor,phi,{inds}]
};


ExpandLinearPush[tensor_,dphi_,inds_List]:=With[{tdom=-SlotsOfTensor[dphi][[1]]},
With[{ilist=Transpose[pushfactor1[#,dphi,tdom]&/@Transpose[{SlotsOfTensor[tensor],inds}]]},
tensor@@First[ilist]Times@@Last[ilist]
]
];
pushfactor1[{tdom_,ind_?UpIndexQ},dphi_,tdom_]:=With[{new=DummyIn[tdom]},{new,dphi[-new,ind]}];
pushfactor1[{-tdom_,ind_?DownIndexQ},dphi_,tdom_]:=With[{new=DummyIn[tdom]},{-new,Inv[dphi][ind,new]}];
pushfactor1[{_,ind_},dphi_,tdom_]:={ind,1};


ExpandPushForwardTensor[tensor_,phi_,inds_List]:=ExpandPullBackTensor[tensor,InverseMapping[phi],inds];


(********************** 13.Variational derivatives ********************)


If[$ReadingVerbose,Print["Reading section 13: Variational derivatives."],Null,Null]


ImplicitTensorDepQ[tensor_,tensor_]:=True;
ImplicitTensorDepQ[list_List,tensor_]:=Apply[Or,ImplicitTensorDepQ[#,tensor]&/@list];
(* Default behaviour *)
ImplicitTensorDepQ[tensor1_,tensor2_]:=False;
SetNumberOfArguments[ImplicitTensorDepQ,2];
Protect[ImplicitTensorDepQ];


(* Assume PD derivative *)
VarD[tensor_][expr_]:=VarD[tensor,PD][expr];
(* Generate rest. Replace dummies in expr. This does not act on scalar arguments of functions *)
VarD[tensor_,der_][expr_]:=If[ScalarQ[expr],
VarD[tensor,der][ReplaceDummies[expr],1],
Message[VarD::nouse,"VarD","a non-scalar expression"];
NonScalarVarD[tensor,der][expr]
];
(* Thread over Plus *)
VarD[tensor_,der_][expr_Plus,rest_]:=VarD[tensor,der][#,rest]&/@expr;
VarD[tensor_,der_][expr_SeriesData,rest_]:=SeriesDataMap[VarD[tensor,der][#,rest]&,expr];
(* VarD on products: sum of VarDtake's of elements *)
VarD[tensor_,der_][expr_Times,rest_]:=Sum[VarDtake[tensor,der,rest,List@@expr,count],{count,Length@expr}];
(* VarD element n of a list of factors *)
VarDtake[tensor_,der_,rest_,list_List,n_Integer]:=VarD[tensor,der][list[[n]],Times@@ReplacePart[list,rest,n]];
(* Scalar functions
VarD[tensor_,der_][func_?ScalarFunctionQ[expr_],rest_]:=VarD[tensor,der][ReplaceDummies[expr],func'[expr]rest];
*)
(* Leo suggested the exponent term 
VarD[tensor_,der_][Power[base_,exponent_],rest_]:=VarD[tensor,der][ReplaceDummies[base],exponent Power[base,exponent-1]rest]+VarD[tensor,der][ReplaceDummies[exponent],Power[base,exponent]Log[base]rest];
*)
(* Scalar functions. Multiargument generalization contributed by Leo. multiD is not enough here *)
VarD[tensor_,der_][func_?ScalarFunctionQ[args__],rest_]:=With[{repargs=ReplaceDummies/@{args}},Plus@@MapThread[VarD[tensor,der][#1,rest (Derivative@@#2)@func@@repargs]&,{repargs,IdentityMatrix@Length@repargs}]];
(* Remove Scalar head because in general the result is not a scalar *)
VarD[tensor_,der_][Scalar[expr_],rest_]:=VarD[tensor,der][ReplaceDummies[expr],rest];
(* Constants *)
VarD[tensor_,der_][x_?ConstantQ,rest_]:=0;
(* Same tensor: metric. Do not use ContractMetric, which hides the metric *)
VarD[metric_[a_,b_],der_][metric_Symbol?MetricQ[c_,d_],rest_]:=metricsign[a,b,c,d]ToCanonical[rest (metric[ChangeIndex@a,c]metric[ChangeIndex@b,d]+metric[ChangeIndex@a,d]metric[ChangeIndex@b,c])/2,UseMetricOnVBundle->None];
metricsign[_Symbol,_Symbol,_Symbol,_Symbol]:=1;
metricsign[-_Symbol,-_Symbol,-_Symbol,-_Symbol]:=1;
metricsign[-_Symbol,-_Symbol,_Symbol,_Symbol]:=-1;
metricsign[_Symbol,_Symbol,-_Symbol,-_Symbol]:=-1;
metricsign[_,_,_,_]:=0;
(* Same tensor. Place indices in proper delta positions. QUESTION: could this be problematic for spinors? *)
varddelta[ind1_?UpIndexQ,ind2_?DownIndexQ]:=delta[ind2,ind1];
varddelta[ind1_,ind2_]:=delta[ind1,ind2];
VarD[tensor_[inds1___],der_][tensor_?xTensorQ[inds2___],rest_]:=With[{clist=ChangeIndex/@IndexList[inds1]},
ToCanonical[ImposeSymmetry[Inner[varddelta,clist,IndexList[inds2],Times],clist,SymmetryGroupOfTensor[tensor[inds1]]]rest,UseMetricOnVBundle->None]];
(* A different tensor *)
VarD[tensor1_[inds1___],der_][tensor2_?xTensorQ[inds2___],rest_]:=0/;!ImplicitTensorDepQ[tensor2,tensor1];
(* Same connection: integration by parts *)
VarD[tensor_,covd_][covd_?CovDQ[ind_][expr_],rest_]:=-VarD[tensor,covd][expr,covd[ind][rest]];
(* Different connection: ChangeCovD *)
VarD[tensor_,covd1_?CovDQ][expr:covd2_?CovDQ[_][_],rest_]:=VarD[tensor,covd1][ChangeCovD[expr,covd2,covd1],rest]/;CompatibleCovDsQ[covd1,covd2];
(* Support for parametric derivatives *)
VarD[tensor_,OverDot][OverDot[expr_],rest_]:=-VarD[tensor,OverDot][expr,OverDot[rest]];
VarD[tensor_,ParamD[p_]][ParamD[pl___,p_,pr___][expr_],rest_]:=-VarD[tensor,ParamD[p]][ParamD[pl,pr][expr],ParamD[p][rest]];


NonScalarVarD[tensor_,der_][expr_]:=
With[{inds=ChangeIndex/@FindFreeIndices[expr]},With[{tmp=Tensor["TMP",SignedVBundleOfIndex/@inds,DependenciesOf[expr]]@@inds},IndexCoefficient[VarD[tensor,der][tmp expr],tmp]
]
];


(****************************** 14.Metric *****************************)


If[$ReadingVerbose,Print["Reading section 14: Metrics."],Null,Null]


FrozenMetricQ[metric_]:=And[Not@FirstMetricQ[metric],InducedFrom[metric]===Null];


DefMetric::old="There are already metrics `1` in vbundle `2`.";
DefMetric::inds="Indices `1` and `2` do not belong to the same vector bundle.";
DefMetric::notan="Metrics can only be defined on a tangent bundle.";
DefMetric::no1smetric="Metrics can only be induced from the first-metric of a vector bundle.";


DefMetricCheck[signdet_,metric_[-ind1_,-ind2_],covd_,covdsymbol_,flat_,inducedfrom_,confto_,odeps_,wwb_,eoib_]:=Module[{vb,supermetric,vector},
(* Indices *)
If[!AbstractIndexQ[ind1],Throw@Message[DefMetric::unknown,"abstract index",ind1]];
If[!AbstractIndexQ[ind2],Throw@Message[DefMetric::unknown,"abstract index",ind2]];
vb=VBundleOfIndex[ind1];
If[vb=!=VBundleOfIndex[ind2],Throw@Message[DefMetric::inds,ind1, ind2]];
If[Tangent[BaseOfVBundle@vb]=!=vb,Throw@Message[DefMetric::notan]];
(* Metric *)
If[MetricEndowedQ[vb],Message[DefMetric::old,MetricsOfVBundle[vb],vb]];
ValidateSymbol[metric];
ValidateSymbolInSession[metric];
(* Dependencies *)
If[!Or[ManifoldQ[#],ParameterQ[#]],Throw@Message[DefMetric::unknown,"dependency",#]]&/@odeps;
(* Sign Det. A phase Exp[I alpha] is accepted if Abs returns 1 on it *)
Which[
Abs[signdet]===1,Null,
signdet===0,Null,
MatchQ[signdet,{_Integer?NonNegative,_Integer?NonNegative,_Integer?NonNegative}],Null,
True,Throw@Message[DefMetric::invalid,signdet,"signdet of metric"]
];
(* The Levi-Civita covd *)
Which[
!TrueOrFalse[flat],Throw@Message[DefMetric::invalid,flat,"value for option FlatMetric"],
covd===PD &&!flat,Throw@Message[DefMetric::error,"PD can only be associated to a flat metric."],
covd===PD&&flat&&covdsymbol=!=SymbolOfCovD[PD],Message[DefMetric::error,"Using standard "<>ToString[SymbolOfCovD[PD]]<>" as symbols for PD."],
covd=!=PD,DefCovDCheck[covd,covdsymbol,{False,!flat,Null,False,False,wwb}]];
(* Induced metric *)
Switch[inducedfrom,
Null,Null,
{_,_},{supermetric,vector}=inducedfrom;
If[!MetricQ[supermetric],Throw@Message[DefMetric::unknown,"metric",supermetric]];If[!FirstMetricQ[supermetric],Throw@Message[DefMetric::no1smetric]];
If[!xTensorQ[vector]||Length[SlotsOfTensor[vector]]=!=1,Throw@Message[DefMetric::unknown,"vector",vector]],
_,Throw@Message[DefMetric::invalid,inducedfrom,"value for option InducedFrom"]
];
(* Conformal relations *)
Switch[confto,
Null,Null,
{_,_},{originalmetric,confactor}=confto;
If[!MetricQ[Head[originalmetric]],Throw@Message[DefMetric::unknown,"conformally related metric ",Head@originalmetric]];
If[!ScalarQ[confactor],Throw@Message[DefMetric::invalid,confactor,"conformal scalar factor"]];
];
(* epsilon orientation in a basis *)
Which[
Head[eoib]=!=List||Length[eoib]=!=2,Throw@Message[DefMetric::invalid,eoib,"value for option epsilonOrientationInMetric"],
!BasisQ[eoib[[1]]],Throw@Message[DefMetric::unknown,"basis",eoib[[1]]],
_,Null
];
]


Set[MetricTensorQ[#],True]&/@{epsilon,ExtrinsicK,Acceleration,Projector,Inv,Tetra};
MetricTensorQ[_]=False;


PrintAsCharacter[epsilon]="\[Epsilon]";
PrintAsCharacter[ExtrinsicK]="K";
PrintAsCharacter[Acceleration]="A";
PrintAsCharacter[Projector]="P";
PrintAsCharacter[Inv]="i";
PrintAsCharacter[Tetra]="G";


(* Exception to general GiveOutputString to have a subscript *)
GiveOutputString[Projector,metric_]:=StringJoin["\!\(",PrintAsCharacter[Projector],"\+",PrintAs[metric],"\)"];


LC[metric_?MetricQ]:=GiveSymbol[LC,metric];
epsilon[metric_?MetricQ]:=GiveSymbol[epsilon,metric];
ExtrinsicK[metric_?MetricQ]:=GiveSymbol[ExtrinsicK,metric];
Acceleration[vector_?xTensorQ]:=GiveSymbol[Acceleration,vector];
Projector[metric_?MetricQ]:=GiveSymbol[Projector,metric];
Inv[metric_?MetricQ]:=GiveSymbol[Inv,metric];
Tetra[metric_?MetricQ]:=GiveSymbol[Tetra,metric];


SetDelayed[#[x_],Throw@Message[#::unknown,"metric",x]]&/@{LC,epsilon,Tetra};
SetDelayed[#[x_],Throw@Message[#::unknown,"induced metric",x]]&/@{ExtrinsicK,Projector};
Inv[x_]:=Throw@Message[Inv::unknown,"metric or linear operator",x];
Acceleration[x_]:=Throw@Message[Acceleration::unknown,"metric-orthogonal vector",x];


(* Backwards compatibility *)
DefMetric[signdet_,metric_,covd_,covdsymbol:{_String,_String},options:OptionsPattern[]]:=DefMetric[signdet,metric,covd,SymbolOfCovD->covdsymbol,options];


Off[RuleDelayed::rhs];
Options[DefMetric]:={
PrintAs->Identity,
FlatMetric->False,
InducedFrom->Null,
ConformalTo->Null,
OtherDependencies->{},
WeightedWithBasis->Null,
epsilonOrientationInBasis:>{AIndex,$epsilonSign},
Master->Null,
ProtectNewSymbol:>$ProtectNewSymbols,
DefInfo->{"",""}}
DefMetric[signdet_,metric_[-ind1_,-ind2_],covd_,options:OptionsPattern[]]:=Catch@With[{
vbundle=VBundleOfIndex[ind1]},

Module[{covdsymbol,flat,inducedfrom,confto,odeps,wwb,eoib,pns,info,flatPD,inducedQ,dim,integerdimQ,frozenQ,firstQ,invertQ,firstmetric,supermetric,vector,LCmetric},

(* Options and checks *)
{covdsymbol,flat,inducedfrom,confto,odeps,wwb,eoib,pns,info}=OptionValue[{DefMetric,DefCovD},{options},{SymbolOfCovD,FlatMetric,InducedFrom,ConformalTo,OtherDependencies,WeightedWithBasis,epsilonOrientationInBasis,ProtectNewSymbol,DefInfo}];

DefMetricCheck[signdet,metric[-ind1,-ind2],covd,covdsymbol,flat,inducedfrom,confto,odeps,wwb,eoib];

(* Explore *)
flatPD=flat&&(covd===PD);
dim=DimOfVBundle[vbundle];
integerdimQ=IntegerQ[dim];
(* One and only one of firstQ, inducedQ, frozenQ is True, and the other two are False *)
firstQ=MetricsOfVBundle[vbundle]==={};
inducedQ=And[!firstQ,inducedfrom=!=Null];
frozenQ=And[!firstQ,!inducedQ];
If[inducedQ,{supermetric,vector}=inducedfrom];
If[frozenQ,firstmetric=First[MetricsOfVBundle[vbundle]]];

With[{
manifold=BaseOfVBundle[vbundle],
deps=Union[{BaseOfVBundle[vbundle]},odeps],
vbQ=VBundleIndexQ[vbundle],
vbpmQ=VBundleIndexPMQ[vbundle],
invmetric=If[frozenQ,GiveSymbol[Inv,metric],metric]},

(* Avoid messages from options of DefMetric not accepted in DefTensor or DefCovD *)
Off[OptionValue::nodef];

(* Define metric tensor. Do not protect symbol. Do not use ProjectedWith. It is always symmetric *)
DefTensor[metric[-ind1,-ind2],deps,Symmetric[{1,2}],
ProtectNewSymbol->False,
OrthogonalTo:>If[inducedQ,{vector[ind1]},{}],
DefInfo:>If[info===False,False,{"symmetric metric tensor",""}],
options];

MakexTensions[DefMetric,"Beginning",signdet,metric[-ind1,-ind2],covd,options];

(* Register structure *)
AppendToUnevaluated[$Metrics,metric];
MetricQ[metric]^=True;
CovDOfMetric[metric]^=covd;
VBundleOfMetric[metric]^=vbundle;
xUpAppendTo[MetricsOfVBundle[vbundle],metric];
FlatMetricQ[metric]^=flat;
InducedFrom[metric]^=inducedfrom;

(* Signature *)
If[Head[signdet]===List &&Length[signdet]===3,
SignatureOfMetric[metric]^=signdet;
SignDetOfMetric[metric]^=If[signdet[[3]]===0,(-1)^signdet[[2]],0],
SignDetOfMetric[metric]^=signdet;
];
(* Invertibility *)
invertQ=SignDetOfMetric[metric]=!=0;
(* Define inverse metric. TODO: Orthogonal and Projected? *)
If[invertQ,
If[frozenQ ,
DefTensor[invmetric[ind1,ind2],deps,Symmetric[{1,2}],
ProtectNewSymbol->False,
Master->metric,
DefInfo:>If[info===False,False,{"inverse metric tensor","Metric is frozen!"}],
TensorID->{InvMetric,metric},
PrintAs:>GiveOutputString[Inv,metric],
options],
Inv[metric]^=metric
],
Print["** DefMetric: non-invertible metric being defined! Dangerous!"]
];

(* Directional indices are not allowed in metrics. Why only for first metrics? *)
If[firstQ,
metric[Dir[expr_],b_]:=ReplaceIndex[expr,UltraindexOf[expr]->b];
metric[a_,Dir[expr_]]:=ReplaceIndex[expr,UltraindexOf[expr]->a];
];

(* Define epsilon tensor, with covariant indices. Do not protect symbol *)
metric/:epsilonOrientation[metric,eoib[[1]]]=eoib[[2]];
With[{epsilonname=GiveSymbol[epsilon,metric],
inds=GetIndicesOfVBundle[vbundle,If[integerdimQ,If[inducedQ,dim-1,dim],2]]},
DefTensor[epsilonname@@(ChangeIndex/@inds),deps,Antisymmetric@Range@Length@inds,
PrintAs:>GiveOutputString[epsilon,metric],
ProtectNewSymbol->False,
Master->metric,
OrthogonalTo:>If[inducedQ,{vector[First[inds]]},{}],
ProjectedWith:>If[inducedQ,{metric[-DummyIn[vbundle],First[inds]]},{}],
DefInfo:>If[info===False,False,{"antisymmetric tensor",""}],
TensorID->{epsilon,metric}];
If[Not@integerdimQ && Not@inducedQ,
TagSetDelayed[epsilonname,SymmetryGroupOfTensor[epsilonname[inds1__]],Antisymmetric[Range@Length@{inds1}]];
TagUnset[epsilonname,SymmetryGroupOfTensor[epsilonname]];
TagSetDelayed[epsilonname,SymmetryGroupOfTensor[epsilonname],Antisymmetric[Range@DimOfVBundle@vbundle]]
];

(* Products of two epsilons. In the induced and frozen cases expansions give metric and not delta *)
If[inducedQ ||frozenQ,
epsilonname/:epsilonname[inds1__]epsilonname[inds2__]:=SignDetOfMetric[metric]expandGdelta[metric][inds1,inds2],
epsilonname/:epsilonname[inds1__]epsilonname[inds2__]:=SignDetOfMetric[metric]ExpandGdelta[Gdelta[inds1,inds2]]
];

(* Derivatives of epsilon. Note that we have not yet defined covd (!?) *)
If[frozenQ,
covd[a_][epsilonname[(-b_?vbQ)..]]^=0,
covd[a_][epsilonname[b__?vbpmQ]]^=0
];
TensorDerivative[epsilonname,covd,___]:=Zero;
If[!inducedQ,
If[!frozenQ,
epsilonname/:LieD[v_][epsilonname[inds1__?UpIndexQ]]:=Module[{dummy=DummyIn[vbundle]},-covd[-dummy][ReplaceIndex[v,{UltraindexOf[v]->dummy}]epsilonname[inds1]]]
];
epsilonname/:LieD[v_][epsilonname[inds1__?DownIndexQ]]:=Module[{dummy=DummyIn[vbundle]},covd[-dummy][ReplaceIndex[v,{UltraindexOf[v]->dummy}]epsilonname[inds1]]];
epsilonname/:TensorDerivative[epsilonname,LieD[vhead_?xTensorQ[_]]]:=With[{dummy=DummyIn[vbundle]},MultiplyHead[TensorDerivative[vhead,covd][dummy,-dummy],epsilonname]];
];

(* Special relation in 2d, valid for all types of indices *)
If[dim===2&&firstQ,
epsilonname[a_,b_]epsilonname[c_,d_]^=SignDetOfMetric[metric] (metric[a,c]metric[b,d]-metric[a,d]metric[b,c]);
];

(* Define Tetra, only in dimension 4. This is an algebraic construction, hence valid for frozen metrics *) 
If[dim==4,
With[{TetraName=GiveSymbol[Tetra,metric]},
Module[{i1,i2,i3,i4},
{i1,i2,i3,i4}=GetIndicesOfVBundle[vbundle,4];
DefTensor[TetraName[-i1,-i2,-i3,-i4],deps,GenSet[xAct`xPerm`Cycles[{1,2},{3,4}],xAct`xPerm`Cycles[{1,3},{2,4}]],
Dagger->Complex,
PrintAs:>GiveOutputString[Tetra,metric],
Master->metric,
ProtectNewSymbol->False,
DefInfo:>If[info===False,False,{"tetrametric",""}]
];
TetraRule[metric]^:=MakeRule[{TetraName[i1,i2,i3,i4],I/2epsilonOrientation[metric,AIndex]epsilonname[i1,i2,i3,i4]+metric[i1,i4]metric[i2,i3]/2-metric[i1,i3]metric[i2,i4]/2+metric[i1,i2]metric[i3,i4]/2},Evaluate->True];
With[{TetraNamedag=Dagger[TetraName]},
TetraNamedag[i1_,i2_,i3_,i4_]:=TetraName[i1,i4,i3,i2]
]
];
If[$ProtectNewSymbols,Protect[TetraName,Evaluate[Dagger@TetraName]]]
]
];

If[$ProtectNewSymbols,Protect[epsilonname]]
];

(* Self-contractions. Transformation to delta or metric for A-indices contraction of two metrics (this could be generalized). Projectors not automatically converted into delta *)
If[frozenQ,
invmetric/:metric[a_,-b_?vbQ]invmetric[b_?EIndexQ,c_]:=firstmetric[a,c];
invmetric/:metric[-b_?vbQ,a_]invmetric[b_?EIndexQ,c_]:=firstmetric[a,c];
invmetric/:metric[a_,-b_?vbQ]invmetric[c_,b_?EIndexQ]:=firstmetric[a,c];
invmetric/:metric[-b_?vbQ,a_]invmetric[c_,b_?EIndexQ]:=firstmetric[a,c];
invmetric/:metric[a_,b_?vbQ]invmetric[-b_?EIndexQ,c_]:=firstmetric[a,c];
invmetric/:metric[b_?vbQ,a_]invmetric[-b_?EIndexQ,c_]:=firstmetric[a,c];
invmetric/:metric[a_,b_?vbQ]invmetric[c_,-b_?EIndexQ]:=firstmetric[a,c];
invmetric/:metric[b_?vbQ,a_]invmetric[c_,-b_?EIndexQ]:=firstmetric[a,c],
If[inducedQ,
metric[a_?vbQ,-a_?EIndexQ]:=dim-1;
metric[-a_?vbQ,a_?EIndexQ]:=dim-1
];
If[firstQ,
metric[a_?UpIndexQ,b_?DownIndexQ]:=delta[b,a];
metric[a_?DownIndexQ,b_?UpIndexQ]:=delta[a,b]
];
HoldPattern[metric[a_,b_?vbQ]metric[-b_?EIndexQ,c_]]^:=metric[a,c];
HoldPattern[metric[a_,b_?vbQ]metric[c_,-b_?EIndexQ]]^:=metric[a,c];
HoldPattern[metric[b_?vbQ,a_]metric[-b_?EIndexQ,c_]]^:=metric[a,c];
HoldPattern[metric[b_?vbQ,a_]metric[c_,-b_?EIndexQ]]^:=metric[a,c];
];

(* Define covariant derivative(s). Do not protect symbol *)
If[flatPD,
(* Flat metric with PD. Derivative already defined *)
Print["** DefMetric: Associating fiducial flat derivative PD to metric."];
Unprotect[PD];
MetricOfCovD[PD]^=metric;
PD[c_][metric[a_?vbpmQ,b_?vbpmQ]]^=0;
metric/:TensorDerivative[metric,PD]:=Zero;
If[!frozenQ,
(* Place rule for this metric second, after the general rule for lower indices *)
AppendTo[$SortPDsRules,PD[b_?vbpmQ][PD[a_?vbpmQ][expr1_]]:>PD[a][PD[b][expr1]]/;DisorderedPairQ[a,b]]
],
(* Other cases. Define derivative. For a frozen metric the associated tensors are wrong! TODO *)
DefCovD[covd[-ind1],covdsymbol,
FromMetric->metric,
Curvature:>Not[flat],
OtherDependencies->odeps,
OrthogonalTo:>If[inducedQ,{vector[ind1]},{}],
ProjectedWith:>If[inducedQ,{metric[ind1,-ind2]},{}],
Master->metric,
ProtectNewSymbol->False,
DeleteCases[{options},_[PrintAs,_]]
]
];
InducedCovDQ[covd]=inducedQ;
(* Torsion. TODO: modify the covdsymbol in the LCmetric *)
LCmetric=If[TorsionQ[covd],LC[metric],covd];
If[!CovDQ[LCmetric],
DefCovD[LCmetric[-ind1],covdsymbol,
FromMetric->metric,
Torsion->False,
Curvature->True,
OtherDependencies->odeps,
OrthogonalTo:>If[inducedQ,{vector[ind1]},{}],
ProjectedWith:>If[inducedQ,{metric[ind1,-ind2]},{}],
Master->metric,
ProtectNewSymbol->False,
DeleteCases[{options},_[PrintAs,_]]
]
];
metric/:CovDOfMetric[metric,Torsion[covd]]=covd;
metric/:CovDOfMetric[metric,Torsion[LCmetric]]=LCmetric;
metric/:CovDOfMetric[metric,Zero]=LCmetric;
metric/:LC[metric]=LCmetric;

If[flatPD,Protect[PD]];
If[$ProtectNewSymbols,Protect[covd]];

(* Derivatives of the metric are defined in DefCovD *)
(* Other covariant derivatives on the metric with upstairs abstract indices *)
If[!inducedQ,
invmetric/:HoldPattern[der_?FirstDerQ[invmetric[b_?vbQ,c_?vbQ]]]:=Module[{b1=DummyIn[vbundle],c1=DummyIn[vbundle]},-invmetric[b,b1]invmetric[c,c1]der[metric[-b1,-c1]]]];
(* There is no simple generalization for induced metrics.
QUESTION: Is there an equivalent rule for the epsilon tensor?*)

(* Determinant in the basis AIndex. Has to be defined after the covd and the invmetric *)
If[info===False,
Block[{$DefInfoQ=False},Determinant[metric,AIndex][]],
Determinant[metric,AIndex][]
];

(* Define induced metric *)
If[inducedQ,DefInducedMetric[metric[-ind1,-ind2],{manifold},covd,{vector,supermetric,CovDOfMetric[supermetric]},flat]];

(* Store conformal relations *)
If[confto=!=Null,
SetConformalTo[metric[-ind1,-ind2],confto]
];

MakexTensions[DefMetric,"End",signdet,metric[-ind1,-ind2],covd,options];

On[OptionValue::nodef];

If[pns,Protect[metric]];
]
]
];
DefMetric[_,metric_,_,OptionsPattern[]]:=Message[DefMetric::invalid,metric,"metric"];
SetNumberOfArguments[DefMetric,{3,Infinity}];
Protect[DefMetric];
On[RuleDelayed::rhs];


CovDOfMetric[metric_]:=Throw@Message[CovDOfMetric::unknown,"metric",metric];
CovDOfMetric[metric_,torsion_?xTensorQ]:=Throw@Message[CovDOfMetric::unknown,"metric",metric];
CovDOfMetric[metric_?xTensorQ,torsion_]:=Throw@Message[CovDOfMetric::unknown,"torsion",torsion];
LC[metric_]:=Throw@Message[LC::unknown,"metric",metric];
SignatureOfMetric[metric_]:=Throw@Message[SignatureOfMetric::unknown,"(signature of) metric",metric];
SignDetOfMetric[metric_]:=Throw@Message[SignDetOfMetric::unknown,"metric",metric];
VBundleOfMetric[metric_]:=Throw@Message[VBundleOfMetric::unknown,"metric",metric];
InducedFrom[metric_]:=Throw@Message[InducedFrom::unknown,"metric",metric];
SetNumberOfArguments[#,1]&/@{LC,SignatureOfMetric,SignDetOfMetric,VBundleOfMetric,InducedFrom};
SetNumberOfArguments[CovDOfMetric,{1,2}];
Protect[CovDOfMetric,SignatureOfMetric,SignDetOfMetric,VBundleOfMetric,FlatMetricQ,FlatMetric,InducedFrom];


epsilonOrientation[_?MetricQ,_?BasisQ]:=$epsilonSign;
SetNumberOfArguments[epsilonOrientation,2];
Protect[epsilonOrientation];


UndefMetric[list:{___?MetricQ}]:=Scan[UndefMetric,list];
UndefMetric[metric_]:=Catch@With[{servants=DeleteCases[ServantsOf[metric],PD],vbundle=VBundleOfMetric[metric]},
(* Check that it is really a metric *)
If[!MetricQ[metric],Throw@Message[UndefMetric::unknown,"metric",metric]];
CheckRemoveSymbol[metric];
MakexTensions[UndefMetric,"Beginning",metric];
xUpSet[ServantsOf[metric],{}];
DropFromHosts[metric];
Undef/@Reverse[servants];
(* Unregister *)
xUpDeleteCasesTo[MetricsOfVBundle[vbundle],metric];
$Metrics=DeleteCases[$Metrics,metric];
Unprotect[metric];
If[MetricOfCovD[PD]===metric,
xUpSet[MetricOfCovD[PD],Null];
$SortPDsRules=Take[$SortPDsRules,1]; (* Preserve only the general rule *)
];
If[MemberQ[$ProductMetrics,metric],
Unset[ProductMetricRules[metric,Metric]];
Unset[ProductMetricRules[metric,CovD]];
Unset[ProductMetricRules[metric,Christoffel]];
Unset[ProductMetricRules[metric,Riemann]];
$ProductMetrics=DeleteCases[$ProductMetrics,metric]];
(* Remove tensor *)
TagUnset[metric,MetricQ[metric]];
MakexTensions[UndefMetric,"End",metric];
UndefTensor[metric];
];
SetNumberOfArguments[UndefMetric,1];
Protect[UndefMetric];


(* Three arguments: expr, metric, covd. Main *)
GradMetricToChristoffel[expr_,metric_Symbol?MetricQ,covd_?CovDQ]:=gradMetricToChristoffel[expr,{CovDOfMetric[metric],metric,Inv[metric]},covd];
(* Other cases *)
GradMetricToChristoffel[expr_,metric_,list_List]:=Fold[GradMetricToChristoffel[#1,metric,#2]&,expr,list];
GradMetricToChristoffel[expr_,list_List,covd_]:=Fold[GradMetricToChristoffel[#1,#2,covd]&,expr,list];
GradMetricToChristoffel[_,x_,covd_?CovDQ]:=Throw@Message[GradMetricToChristoffel::unknown,"metric",x];
GradMetricToChristoffel[_,_,y_]:=Throw@Message[GradMetricToChristoffel::unknown,"covariant derivative",y];
(* Two arguments: expr, metric *)
GradMetricToChristoffel[expr_,list_List]:=Fold[GradMetricToChristoffel,expr,list];
GradMetricToChristoffel[expr_,metric_]:=GradMetricToChristoffel[expr,metric,$CovDs];
(* One argument: expr *)
GradMetricToChristoffel[expr_]:=GradMetricToChristoffel[expr,$Metrics];
SetNumberOfArguments[GradMetricToChristoffel,{1,3}];
Protect[GradMetricToChristoffel];


gradMetricToChristoffel[expr_,{metriccovd_,metric_,invmetric_},covd_]:=expr/.{
covd[i_][metric[i1_?DownIndexQ,i2_?DownIndexQ]]:>gradMetricTerm[covd[i],metriccovd,metric,i1,i2]+gradMetricTerm[covd[i],metriccovd,metric,i2,i1],covd[i_][invmetric[i1_?UpIndexQ,i2_?UpIndexQ]]:>gradInvMetricTerm[covd[i],metriccovd,invmetric,i1,i2]+gradInvMetricTerm[covd[i],metriccovd,invmetric,i2,i1]
};


gradMetricTerm[covd_[a_],metriccovd_,metric_,-b_Symbol,c_]:=With[{d=DummyAs[b]},metric[-b,-d]Christoffel[metriccovd,covd][d,a,c]];
gradInvMetricTerm[covd_[a_],metriccovd_,invmetric_,b_Symbol,c_]:=With[{d=DummyAs[b]},-invmetric[b,d]Christoffel[covd,metriccovd][c,a,-d]];
gradMetricTerm[covd_[a_],metriccovd_,metric_,b:({_,-basis_Symbol}|-basis_?BasisQ[_]),c_]:=With[{d=-DummyAs[b]},metric[b,-d]Christoffel[metriccovd,xAct`xCoba`PDOfBasis[basis]][d,a,c]]; (* BASIS1 and BASIS2 *);
gradInvMetricTerm[covd_[a_],metriccovd_,invmetric_,b:({_,basis_Symbol}|basis_?BasisQ[_]),c_]:=With[{d=DummyAs[b]},-invmetric[b,d]Christoffel[xAct`xCoba`PDOfBasis[basis],metriccovd][c,a,-d]]; (* BASIS1 and BASIS2 *);


(* Three arguments: expr, metric, covd. Main *)
ChristoffelToGradMetric[expr_,metric_Symbol?MetricQ,covd_?CovDQ]:=christoffelToGradMetric[expr,{CovDOfMetric[metric],metric,Inv[metric]},covd];
(* Other cases *)
ChristoffelToGradMetric[expr_,metric_,list_List]:=Fold[ChristoffelToGradMetric[#1,metric,#2]&,expr,list];
ChristoffelToGradMetric[expr_,list_List,covd_]:=Fold[ChristoffelToGradMetric[#1,#2,covd]&,expr,list];
ChristoffelToGradMetric[_,x_,covd_?CovDQ]:=Throw@Message[ChristoffelToGradMetric::unknown,"metric",x];
ChristoffelToGradMetric[_,_,y_]:=Throw@Message[ChristoffelToGradMetric::unknown,"covariant derivative",y];
(* Two arguments: expr, metric *)
ChristoffelToGradMetric[expr_,list_List]:=Fold[ChristoffelToGradMetric,expr,list];
ChristoffelToGradMetric[expr_,metric_]:=ChristoffelToGradMetric[expr,metric,$CovDs];
(* One argument: expr *)
ChristoffelToGradMetric[expr_]:=ChristoffelToGradMetric[expr,$Metrics];
SetNumberOfArguments[ChristoffelToGradMetric,{1,3}];
Protect[ChristoffelToGradMetric];


orderChristoffels[PD,covd_]:=-1;
orderChristoffels[covd_,PD]:=1;
orderChristoffels[covd1_,covd2_]:=Order[covd1,covd2];


(* If the metricovd is PD then we have a Cartesian metric, and it makes no sense to differentiate it *)
christoffelToGradMetric[expr_,{PD,_,_},_]:=expr;
christoffelToGradMetric[expr_,{metriccovd_,_,_},metriccovd_]:=expr;
christoffelToGradMetric[expr_,{metriccovd_,metric_,invmetric_},covd_]:=With[{vbundle=VBundleOfMetric[metric],name=Cases[$Christoffels,{chr_,{_,metriccovd,covd}|{_,covd,metriccovd},_}:>chr]},
If[name==={},
expr,
expr/.First[name][i1_,i2_,i3_]:>With[{ii1=DummyIn[vbundle],ii2=DummyIn[vbundle],ii3=DummyIn[vbundle]},invmetric[i1,ii1]delta[i2,ii2]delta[i3,ii3]orderChristoffels[metriccovd,covd](covd[-ii2][metric[-ii3,-ii1]]+covd[-ii3][metric[-ii2,-ii1]]-covd[-ii1][metric[-ii2,-ii3]]+$TorsionSign(Torsion[covd][-ii2,-ii3,-ii1]+Torsion[covd][-ii3,-ii2,-ii1]-Torsion[covd][-ii1,-ii2,-ii3]-Torsion[metriccovd][-ii2,-ii3,-ii1]-Torsion[metriccovd][-ii3,-ii2,-ii1]+Torsion[metriccovd][-ii1,-ii2,-ii3]))/2]
]
];


ChristoffelToMetric=ChristoffelToGradMetric;
Protect[ChristoffelToMetric];


ChristoffelToGradConformal::nomet="Covariant derivative `1` does not have an associated metric.";
ChristoffelToGradConformal[expr_,list_List,new_]:=Fold[ChristoffelToGradConformal[#1,#2,new]&,expr,list];
ChristoffelToGradConformal[expr_,old_,old_]:=expr;
ChristoffelToGradConformal[expr_,old_?CovDQ,new_]:=With[{metric=MetricOfCovD[old]},If[metric=!=Null,ChristoffelToGradConformal[expr,metric,new],Throw@Message[ChristoffelToGradConformal::nomet,old]]];
ChristoffelToGradConformal[expr_,old_,new_?CovDQ]:=With[{metric=MetricOfCovD[new]},If[metric=!=Null,ChristoffelToGradConformal[expr,old,metric],
Throw@Message[ChristoffelToGradConformal::nomet,new]]];
ChristoffelToGradConformal[expr_,old_?MetricQ,new_?MetricQ]:=With[{vb=-First[SlotsOfTensor[new]],oldcovd=CovDOfMetric[old],newcovd=CovDOfMetric[new]},With[{confactor=ConformalFactor[old,new],invnew=Inv[new],chr=Head[(Christoffel@@Sort[{oldcovd,newcovd}])[DummyIn[vb],-DummyIn[vb],-DummyIn[vb]]],sign=Order[oldcovd,newcovd]},
If[confactor===Null,
expr,
expr/.chr[c_,a_,b_]:>Module[{d=DummyIn[vb]},Expand[sign/2(delta[a,c]newcovd[b][confactor]+delta[b,c]newcovd[a][confactor]-new[a,b]invnew[c,d]newcovd[-d][confactor])/confactor]]
]
]
];
SetNumberOfArguments[ChristoffelToGradConformal,3];
Protect[ChristoffelToGradConformal];


ConformalFactor[x_,x_?MetricQ]:=1;
ConformalFactor[x_,y_]:=Switch[{MetricQ[x],MetricQ[y]},
{True,True},Throw@Message[ConformalFactor::unknown,"conformal relation of metric",x],
{True,False},Throw@Message[ConformalFactor::unknown,"metric",y],
{False,_},Throw@Message[ConformalFactor::unknown,"metric",x]
];
SetNumberOfArguments[ConformalFactor,2];
Protect[ConformalFactor];


ConformalRules[x_List,y_List]:=Inner[ConformalRules,x,y,Join];
ConformalRules[x_,x_]:={};
ConformalRules[x_,y_]:=Throw@Message[ConformalRules::unknown,"conformal relation of",x];
SetNumberOfArguments[ConformalRules,2];
Protect[ConformalRules];


Off[RuleDelayed::rhs];
SetConformalTo[metric_?MetricQ[-i1_,-i2_],{originalmetric_?MetricQ[-i1_,-i2_],confactor_}]:=Module[{prot=Unprotect[metric]},
metric/:ConformalFactor[metric,originalmetric]=confactor;
metric/:ConformalFactor[originalmetric,metric]=1/confactor;
metric/:ConformalRules[metric,originalmetric]={
metric[i1_,i2_]->confactor originalmetric[i1,i2],
Inv[metric][i1_,i2_]->1/confactor Inv[originalmetric][i1,i2],
Determinant[metric][]->confactor^DimOfVBundle[VBundleOfMetric[metric]] Determinant[originalmetric][]
};
metric/:ConformalRules[originalmetric,metric]=
Join[
If[FirstMetricQ[originalmetric],
{originalmetric[i1_?DownIndexQ,i2_?DownIndexQ]->1/confactor metric[i1,i2],
originalmetric[i1_?UpIndexQ,i2_?UpIndexQ]->confactor Inv[metric][i1,i2]},
{originalmetric[i1_,i2_]->1/confactor metric[i1,i2],
Inv[originalmetric][i1_,i2_]->confactor Inv[metric][i1,i2]}
],
{Determinant[originalmetric][]->Determinant[metric][]/confactor^DimOfVBundle[VBundleOfMetric[metric]]}
];
Protect[Evaluate[prot]];
];
On[RuleDelayed::rhs];


Determinant/:GiveSymbol[Determinant,metric_,AIndex]:=SymbolJoin["Det",metric];
Determinant/:GiveSymbol[Determinant,metric_,basis_]:=SymbolJoin["Det",metric,basis];
GiveOutputString[Determinant,metric_,basis_]:=PrintAs[metric];


Determinant[metric_][]:=Determinant[metric,AIndex][];
Determinant[metric_,basis_?BasisQ][]:=With[{
cd=CovDOfMetric[metric],
dmb=GiveSymbol[Determinant,metric,basis],
vbundle=VBundleOfMetric[metric],
invmetric=Inv[metric]},
(* Define tensor *)
DefTensor[dmb[],Union[DependenciesOfTensor[metric],xAct`xCoba`DependenciesOfBasis[basis]],WeightOfTensor->2basis,Master->metric,PrintAs:>GiveOutputString[Determinant,metric,basis],DefInfo:>If[$DefInfoQ===False,False,{"weight +2 density","Determinant."}],TensorID->{Determinant,metric,basis},ProtectNewSymbol->False];
(* Derivatives of the determinant *)
If[WeightedWithBasis[cd]===basis,
cd[a_][dmb[]]^:=0;
dmb/:TensorDerivative[dmb,cd,___]:=Zero];
der_?FirstDerQ[dmb[]]^:=dmb[]Module[{b1=DummyIn[vbundle],c1=DummyIn[vbundle]},invmetric[b1,c1]der[metric[-b1,-c1]]];
dmb/:TensorDerivative[dmb,covd_?CovDQ]:=Function[{ind_},dmb[]Module[{b1=DummyIn[vbundle],c1=DummyIn[vbundle]},invmetric[b1,c1]TensorDerivative[metric,covd][-b1,-c1,ind]]];
(* Variational derivatives of the determinant *)
dmb/:VarD[invmetric[a_Symbol,b_Symbol],PD][dmb[],rest_]:=-rest metric[-a,-b]dmb[];dmb/:VarD[metric[-a_Symbol,-b_Symbol],PD][dmb[],rest_]:=rest invmetric[a,b]dmb[];
(* Remember result *)
xTagSet[{metric,Determinant[metric,basis]},dmb];
dmb[]
];
Protect[Determinant];


(* SymmetryOfMetric *)
SymmetryOfMetric[Null]=0;
SymmetryOfMetric[metric_]:=xUpSet[SymmetryOfMetric[metric],If[MemberQ[Last@SymmetryGroupOfTensor[metric],-1,{2}],-1,1]];
(* Important convention on the index positioning of an antisymmetric metric *)
SMP[metric_,_?UpIndexQ,_?DownIndexQ]:=1;
SMP[metric_,_?DownIndexQ,_?UpIndexQ]:=SymmetryOfMetric[metric];
SMP[metric_,a_,b_]:=(Message[SymmetryOfMetric::unknown,"symmetry of metric",metric[a,b]];1);


SymmetryOfIndex[ind_,tf_:True]:=SymmetryOfMetric@FirstMetricOfVBundle[VBundleOfIndex[ind],tf];


$NonFrozenMetrics:=DeleteCases[$Metrics,_?FrozenMetricQ];


ContractMetric::frozen="Cannot contract frozen metric `1`.";
NormalVectorOf[metric_]:=Which[
FirstMetricQ[metric],Null,
InducedFrom[metric]=!=Null,VectorOfInducedMetric[metric],
True,Throw@Message[ContractMetric::frozen,metric]];


Options[ContractMetric]={AllowUpperDerivatives->False,OverDerivatives->False};
ContractMetric[expr_,metrics_List,options___]:=Fold[ContractMetric[#1,#2,options]&,expr,metrics];
ContractMetric[expr_,metric_Symbol?MetricQ,options:OptionsPattern[]]:=ContractMetric0[OptionValue[{OverDerivatives,AllowUpperDerivatives}],{metric,NormalVectorOf[metric]}][expr];
ContractMetric[expr_,metric_Symbol,options___]:=Throw@Message[ContractMetric::unknown,"metric",metric];
ContractMetric[expr_,options___?OptionQ]:=ContractMetric[expr,$NonFrozenMetrics,options];
SetNumberOfArguments[ContractMetric,{1,Infinity}];
Protect[ContractMetric];


ContractMetric0[case__][expr_Times]:=ContractMetric1[case][MathInputExpand[expr]];
ContractMetric0[case__][expr_]:=ContractMetric1[case][expr];


(* Automatic threading *)
ContractMetric1[case__][expr_Plus]:=ContractMetric0[case]/@expr;
ContractMetric1[case__][expr_SeriesData]:=SeriesDataMap[ContractMetric0[case],expr];
ContractMetric1[case__][list_List]:=ContractMetric0[case]/@list;
ContractMetric1[case__][eq_Equal]:=ContractMetric0[case]/@eq;


differentexpressionsQ[expr1_Times,expr2_List]:=differentexpressionsQ[List@@expr1,expr2];differentexpressionsQ[expr1_List,expr2_List]:=UnsameQ[Sort[expr1],Sort[expr2]];
(* All other cases in which the LHS is a single object *)
differentexpressionsQ[expr1_,expr2_]:=True;


(* Contraction metric*der[metric] is left untouched to avoid infinite recursion *)
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. der_?FirstDerQ[metric_[a_,b_]]metric_[c_,d_]]:=CM[rest metric[c,d]]der[metric[a,b]]/;(PairQ[a,d]||PairQ[b,d]||PairQ[a,c]||pair[b,c]);


(* Contractions inside first-derivatives. This is a complicated definition *)
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. der_?FirstDerQ[expr_]met:metric_[b_,c_]]:=
Module[{dm=der[met],result},
If[(od||dm===0)&&differentexpressionsQ[result=CM[expr met],{expr,met}],
CM[rest der[result]]-CM[rest dm expr],
CM[rest met]der[expr]]
]/;(MemberQ[FindFreeIndices[expr],ChangeIndex[c]|ChangeIndex[b]]&&Head[expr]=!=metric);


(* Code provided by Guillaume to contract through multiple parametric derivatives *)
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. ParamD[param1_,params__][expr_] met:metric_[b_,c_]]:=Module[{dm=ParamD[param1][met],result},If[(od||dm===0)&&differentexpressionsQ[result=CM[ParamD[params][expr] met],{ParamD[params][expr],met}],CM[rest ParamD[param1][result]]-CM[rest dm ParamD[params][expr]],CM[rest met] ParamD[param1,params][expr]]]/;(MemberQ[FindFreeIndices[expr],ChangeIndex[c]|ChangeIndex[b]]&&Head[expr]=!=metric);


(* nv=Null: allow contraction *)
PRJ[Null,__]:=True;
(* nv=!=Null: allow only valid cases: *)
(*          Contraction of projector on an spatial tensor *)
PRJ[nv_,tensor_?xTensorQ,indumetric_]:=OrthogonalToVectorQ[nv][tensor];
(*          Contraction of projector through its Levi-Civita connection *)
PRJ[nv_,covd_?CovDQ,indumetric_]:=CovDOfMetric[indumetric]===covd;


(* Non-induced metric: nothing to do *)
ContractMetric2[{od_,aud_},{metric_,Null,_}][expr_,met_]:=ContractMetric1[{od,aud},{metric,Null}][expr met];
(* Induced metric. Both indices are contracted: nothing to do *)
ContractMetric2[{od_,aud_},{metric_,nv_,True}][expr_,met_]:=ContractMetric1[{od,aud},{metric,nv}][expr met];
(* Induced metric. Other index not contracted: pass supermetric inside *)
ContractMetric2[{od_,aud_},{metric_,nv_,False}][expr_,met_]:=With[{supermetric=First@InducedFrom[metric]},ContractMetric1[{od,aud},{supermetric,Null}][expr supermetric@@met]];


(* Second index of metric *)
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. tensor_?xTensorQ[i1___,a_,i2___]metric_[b_,c_]]:=SMP[metric,c,a]CM[rest tensor[i1,b,i2]]/;PairQ[c,a]&&PRJ[nv,tensor,metric];
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. covd_?CovDQ[i1___,a_,i2___][expr_]metric_[b_,c_]]:=SMP[metric,c,a]CM[rest covd[i1,b,i2][expr]]/;PairQ[c,a]&&(aud||DownIndexQ[b])&&PRJ[nv,covd,metric];
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. prod_?ProductQ[left___,tensor_?xTensorQ[i1___,a_,i2___],right___]metric_[b_,c_]]:=SMP[metric,c,a]CM[rest prod[left,tensor[i1,b,i2],right]]/;PairQ[c,a]&&PRJ[nv,tensor,metric] &&ScalarsOfProduct[prod][metric[b,c]];


(* First index of metric *)
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. tensor_?xTensorQ[i1___,a_,i2___]metric_[b_,c_]]:=SMP[metric,a,b]CM[rest tensor[i1,c,i2]]/;PairQ[a,b]&&PRJ[nv,tensor,metric];
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. covd_?CovDQ[i1___,a_,i2___][expr_]metric_[b_,c_]]:=SMP[metric,a,b]CM[rest covd[i1,c,i2][expr]]/;PairQ[a,b]&&(aud||DownIndexQ[c])&&PRJ[nv,covd,metric];
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. prod_?ProductQ[left___,tensor_?xTensorQ[i1___,a_,i2___],right___]metric_[b_,c_]]:=SMP[metric,a,b]CM[rest prod[left,tensor[i1,c,i2],right]]/;PairQ[a,b]&&PRJ[nv,tensor,metric] &&ScalarsOfProduct[prod][metric[b,c]];


(* Second index of metric *)
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. head_?InertHeadQ[expr_,z___]metric_[b_,c_]]:=CM[rest head[ContractMetric2[{od,aud},{metric,nv,IsIndexOf[expr,-b,metric]}][expr,metric[b,c]],contractz[{z},-c,b]]]/;IsIndexOf[head[expr,z],-c,metric];
(* First index of metric *)
(CM:ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. head_?InertHeadQ[expr_,z___]metric_[b_,c_]]:=CM[rest head[ContractMetric2[{od,aud},{metric,nv,IsIndexOf[expr,-c,metric]}][expr,metric[b,c]],contractz[{z},-b,c]]]/;IsIndexOf[head[expr,z],-b,metric];


(* Arrange change of indices in the additional z arguments *)
contractz[{},_,_]:=Sequence[];
contractz[zlist_List,index1_,index2_]:=Apply[Sequence,zlist/.IndexList[i1___,index1,i2___]->IndexList[i1,index2,i2]];


(* Default case *)
ContractMetric1[__][expr_]:=expr;


(* Localize individual tensors *)
SeparateMetric[args1___][expr_Plus,args2___]:=SeparateMetric[args1][#,args2]&/@expr;
SeparateMetric[args1___][expr_SeriesData,args2___]:=SeriesDataMap[SeparateMetric[args1][#,args2]&,expr];
SeparateMetric[args1___][expr_Times,args2___]:=SeparateMetric[args1][#,args2]&/@expr;


(* Shortcuts to 2 arguments in the first pair of brackets *)
SeparateMetric[][args__]:=SeparateMetric[Automatic,AIndex][args];
SeparateMetric[metric_][args__]:=SeparateMetric[metric,AIndex][args];
(* Second argument of the second pair of brackets *)
SeparateMetric[metric_,basis_][expr_,All]:=SeparateMetric[metric,basis][expr,IndicesOf[][expr]];
SeparateMetric[metric_,basis_][expr_,vb_?VBundleQ]:=SeparateMetric[metric,basis][IndicesOf[vb][expr]];
SeparateMetric[metric_,basis_][expr_,basis2_Symbol?BasisQ]:=SeparateMetric[metric,basis][expr,IndicesOf[basis2][expr]];
SeparateMetric[metric_,basis_][expr_,f_IndicesOf]:=SeparateMetric[metric,basis][expr,f[expr]];
(* Default action of the second argument: Indices not in their original character *)
SeparateMetric[metric_,basis_][expr_]:=SeparateMetric[metric,basis][expr,Automatic];
SeparateMetric[metric_,basis_][expr_,Automatic]:=With[{tensors=FindAllOfType[expr,Tensor],covds=FindAllOfType[expr,CovD]},SeparateMetric[metric,basis][expr,Flatten@Apply[IndexList,movedindices/@Join[tensors,covds]]]];
(* Expand list of indices *)
SeparateMetric[metric_,basis_][expr_,list_IndexList]:=Fold[SeparateMetric[metric,basis],expr,DeleteCases[list,_LI|-_LI]];


(* Errors *)
SeparateMetric[___][]:=Throw[];
SeparateMetric[_,_,__][__]:=Throw[];
SeparateMetric[___][_,_,__]:=Throw[];


(* Tensors *)
movedindices[Basis[_,_]]:=IndexList[];
movedindices[delta[_,_]]:=IndexList[];
movedindices[tensor_?xTensorQ[inds___]]:=movedindices[tensor,IndexList[inds],SlotsOfTensor[tensor],SignedVBundleOfIndex/@{inds}];
(* TODO: I don't think we need this definition *)
movedindices[ih_?InertHeadQ[expr_,z___]]:=movedindices[expr];
movedindices[tensor_,inds_,slots1:{AnyIndices[vb_]},slots2_]:=movedindices[tensor,inds,Table[vb,{Length[slots2]}],slots2];
movedindices[tensor_,inds_,slots1:{l___,a_AnyIndices,r___},slots2_]:=With[{ll=Length[{l}],rl=Length[{r}],sl=Length[slots2]}, Join[
movedindices[tensor,inds,{l},Take[slots2,ll]],
movedindices[tensor,inds,{a},Take[slots2,{ll+1,sl-rl}]],
movedindices[tensor,inds,{r},Take[slots2,rl-sl]]
]
];
movedindices[tensor_,inds_,slots1_,slots2_]:=If[Length[slots1]=!=Length[slots2],
Throw[Message[Validate::error,"Wrong number of indices of tensor "<>ToString[tensor]]],
inds[[Flatten[Position[Apply[samevbQ,Transpose[{slots1,slots2}],1],False]]]]];
(* Special definition for Gdelta *)
Gdelta/:movedindices[gd_Gdelta]:=With[{il=IndexList@@gd,n=Length[gd]/2},
Join[Select[Take[il,n],UpIndexQ],Select[Drop[il,n],DownIndexQ]]
];
(* CovDs *)
movedindices[covd_?CovDQ[inds___][expr_]]:=Join[Select[IndexList[inds],UpIndexQ],movedindices[expr]];
(* Individual vbundles *)
samevbQ[vb_,vb_]:=True
samevbQ[vb_?VBundleQ,All]:=True;
samevbQ[All,vb_?VBundleQ]:=True;
samevbQ[-vb_?VBundleQ,-All]:=True;
samevbQ[-All,-vb_?VBundleQ]:=True;
samevbQ[_,_]:=False;


SeparateMetric[metric_,basis_][expr_,index_?GIndexQ]:=SeparateMetric1[basis,metric,FirstMetricOfVBundle[VBundleOfIndex[index]]][expr,index];


SeparateMetric1[basis_,Automatic,metric1_][expr_,index_]:=SeparateMetric2[basis,metric1][expr,index];
SeparateMetric1[basis_,metric1_,metric1_][expr_,index_]:=SeparateMetric2[basis,metric1][expr,index];
(* TODO: Is it clear that it is frozen what must be passed to SeparateMetric2, instead of metric1? *)
SeparateMetric1[basis_,frozen_?FrozenMetricQ,metric1_][expr_,index_]:=SeparateMetric2[basis,frozen][expr,index]/;VBundleOfMetric[frozen]===VBundleOfMetric[metric1];


(* SeparateMetric *)
SeparateMetric2[AIndex,metric_][metric_[i1_,i2_],index_]:=metric[i1,i2];
SeparateMetric2[basis_,metric_][expr_,index_]:=expr/.{tensor_?xTensorQ[i1___,index,i2___]:>With[{dummy=DummyAs[index,basis]},If[UpIndexQ[index],metric[index,dummy],metric[dummy,index]]tensor[i1,ChangeIndex[dummy],i2]],covd_?CovDQ[i1___,index,i2___][expr1_]:>With[{dummy=DummyAs[index,basis]},If[UpIndexQ[index],metric[index,dummy],metric[dummy,index]]covd[i1,ChangeIndex[dummy],i2][expr1]]}


DefSign[$ExtrinsicKSign,"\!\(\*SubscriptBox[\(s\), \(K\)]\)",1];
DefSign[$AccelerationSign,"\!\(\*SubscriptBox[\(s\), \(A\)]\)",1];


Validate::nonproj="Induced derivative acting on the non-projected expression `1`."


DefInducedMetric[metric_[-ind1_,-ind2_],dependencies_,covd_,{vector_,supermetric_,superCD_},flat_]:=
With[{vbundle=VBundleOfIndex[ind1]},
With[{
extrinsicKname=GiveSymbol[ExtrinsicK,metric],
accelerationname=GiveSymbol[Acceleration,vector],
projectorname=GiveSymbol[Projector,metric],
epsilonname=GiveSymbol[epsilon,metric],
superepsilonname=GiveSymbol[epsilon,supermetric],
proj=ProjectWith[metric],
norm=Scalar@Simplify@ContractMetric[supermetric[-ind1,-ind2]vector[ind1]vector[ind2],supermetric],
indexlist=GetIndicesOfVBundle[vbundle,3]},

With[{i1=indexlist[[1]],i2=indexlist[[2]],i3=indexlist[[3]]},

(* Register pair metric/vector *)
xUpSet[VectorOfInducedMetric[metric],vector];

(* Define associated tensors *)
DefTensor[extrinsicKname[i1,i2],dependencies,Symmetric[{1,2}],
PrintAs:>GiveOutputString[ExtrinsicK,metric],
OrthogonalTo->{vector[-i1]},
ProjectedWith->{metric[i3,-i2]},
ProtectNewSymbol->False,
Master->metric,
DefInfo->{"extrinsic curvature tensor","Associated to vector "<>ToString[vector]},
TensorID->{ExtrinsicK,metric}];
DefTensor[accelerationname[i1],dependencies,
PrintAs:>GiveOutputString[Acceleration,vector],
OrthogonalTo->{vector[-i1]},
ProjectedWith->{metric[i2,-i1]},
ProtectNewSymbol->False,
Master->metric,
DefInfo->{"acceleration vector","Associated to vector "<>ToString[vector]},
TensorID->{Acceleration,vector}];
(* Relations among them and the derivatives. Improved by Thomas, to use HasOrthogonalIndexQ *)
GradNormalToExtrinsicKRules[metric]={
superCD[a_][vector[b_]]:>$ExtrinsicKSign extrinsicKname[a,b]+$AccelerationSign vector[a]accelerationname[b],vector[-a_]superCD[b_][expr_]:>-superCD[b][vector[-a]]expr/;HasOrthogonalIndexQ[expr,vector[-a]],
vector[a_]superCD[b_][expr_]:>-superCD[b][vector[a]]expr/;HasOrthogonalIndexQ[expr,vector[a]],LieD[vector[_]][expr_]vector[-a_]:>-$AccelerationSign norm accelerationname[-a]expr/;HasOrthogonalIndexQ[expr,vector[-a]](*,LieD[vector[_]][expr_]vector[a_]\[RuleDelayed]0/;HasOrthogonalIndexQ[expr,vector[a]]*)
};
ExtrinsicKToGradNormalRules[metric]=extrinsicKname[a_,b_]:>Module[{c=DummyIn@vbundle},$ExtrinsicKSign (supermetric[a,c]superCD[-c][vector[b]]-$AccelerationSign vector[a]accelerationname[b])];
(* Projectors and metrics *)
ProjectorToMetricRules[metric]=metric[i1_,i2_]->supermetric[i1,i2]-vector[i1]vector[i2]/norm;
MetricToProjectorRules[metric]=supermetric[i1_,i2_]->metric[i1,i2]+vector[i1]vector[i2]/norm;

(* Define projector inert-head *)
DefInertHead[projectorname,
LinearQ->True,
Master->metric,
PrintAs:>GiveOutputString[Projector,metric],
ProtectNewSymbol->False,
DefInfo->{"projector inert-head",""}];
projectorname[supermetric[a_,b_]]:=metric[a,b];
(* The metric, but not the supermetric, can be contracted through the projector *)
xTagSet[{projectorname,ContractThroughQ[projectorname,metric]},True];
(* The supermetric is converted into metric when contracted with the projector or covd *)
xTagSetDelayed[{projectorname,supermetric[i1_,i2_]projectorname[expr_]},metric[i1,i2]projectorname[expr]/;Or[IsIndexOf[expr,-i1,metric],IsIndexOf[expr,-i2,metric]]];
xTagSetDelayed[{covd,supermetric[i1_,i2_]covd[i3_][expr_]},metric[i1,i2]covd[i3][expr]/;Or[IsIndexOf[covd[i3][expr],-i1,metric],IsIndexOf[covd[i3][expr],-i2,metric]]];
(* Projection rule with vector *)
xTagSetDelayed[{projectorname,vector[i_]projectorname[expr_]},0/;IsIndexOf[expr,-i,metric]];
(* Particular cases *)
projectorname[1]:=1;
projectorname[rest_. x_?ScalarQ]:=Scalar[x]projectorname[rest];
projectorname[vector[i_]expr_.]:=0/;Not@IsIndexOf[expr,-i,supermetric];
projectorname[projectorname[expr_]]:=projectorname[expr];
projectorname[tensor_?xTensorQ[inds__]]:=tensor[inds]/;OrthogonalToVectorQ[vector][tensor];
projectorname[covd[k_][expr_]]:=covd[k][expr];
ProjectDerivativeRules[covd]={covd[i_][expr_]:>If[IsIndexOf[expr,-i],
With[{dummy=DummyAs[i]},metric[i,-dummy]projectorname[superCD[dummy][expr]]],
projectorname[superCD[i][expr]]
]};

Module[{prot=Unprotect[covd]},
(* Leibnitz rule. Three cases considered *)
covd[i1_][x_Scalar y_Scalar]:=x covd[i1][y]+y covd[i1][x];
(* Special definitions suggested by Cyril *)
covd[i1_][supermetric[a_?AIndexQ,b_?AIndexQ]]:=0;
(*    Cyril suggests removing the OrthogonalToVectorQ check to handle the many-supermetrics case *)
covd[i1_][x_ supermetric[a_?AIndexQ,b_?AIndexQ]]:=metric[a,b]covd[i1][x]/;OrthogonalToVectorQ[vector][x];
covd[i1_][x_ delta[a_?AIndexQ,b_?AIndexQ]]:=metric[a,b]covd[i1][x]/;OrthogonalToVectorQ[vector][x];
covd[i1_][vector[a_?AIndexQ]vector[b_?AIndexQ]x_.]:=0/;And[!IsIndexOf[x,ChangeIndex@a],!IsIndexOf[x,ChangeIndex@b],!PairQ[a,b]];
(* Product of two, perhaps contracted, expressions *)
covd[i1_][x_ y_]:=Module[{res},
res=Which[
(* Both are orthogonal in all their indices. We can use the Leibnitz rule *)
OrthogonalToVectorQ[vector][x]&&OrthogonalToVectorQ[vector][y],
covd[i1][x]y+covd[i1][y]x,
(* The expression is not globally orthogonal: complain and return unevaluated *)
Not@OrthogonalToVectorQ[vector][x y],
Message[Validate::nonproj,x y];$Failed,
(* Expression is orthogonal, but factors are not. Avoid infinite recursion with this hack *)
FreeQ[{x,y},vector],
covd[i1][Expand@GradNormalToExtrinsicK@Expand[InducedDecomposition[x,{metric,vector}]InducedDecomposition[y,{metric,vector}]]],
(* This should never happen *)
True,
$Failed
];
res/;res=!=$Failed
];
(* Induced derivatives of non-spatial objects are not accepted, not even divergencies *)
covd[_?GIndexQ][expr_]:=$Failed/;Not@isProductQ[Head[expr],Times]&&Not@OrthogonalToVectorQ[vector][expr]&&Message[Validate::nonproj,expr];
Protect[Evaluate[prot]];
];

(* Special definitions *)
metric/:LieD[vector[_]][metric[-a_Symbol,-b_Symbol]]:=$ExtrinsicKSign(extrinsicKname[-a,-b]+extrinsicKname[-b,-a]);
metric/: LieD[vector[_]][metric[a_Symbol,-b_Symbol]]:=-$AccelerationSign accelerationname[-b]vector[a];
metric/: LieD[vector[_]][metric[-a_Symbol,b_Symbol]]:=-$AccelerationSign accelerationname[-a]vector[b];
metric/:LieD[vector[_]][metric[a_Symbol,b_Symbol]]:=-$ExtrinsicKSign(extrinsicKname[a,b]+extrinsicKname[b,a])-$AccelerationSign(vector[a]accelerationname[b]+vector[b]accelerationname[a]);
vector/: LieD[vector[_]][vector[a_Symbol]]:=0;
vector/: LieD[vector[_]][vector[-a_Symbol]]:=$AccelerationSign norm accelerationname[-a];
Module[{prot=Unprotect[{superepsilonname,epsilonname}]},
superepsilonname/: LieD[vector[_]][superepsilonname[inds__?DownIndexQ]]:=Module[{dummy=DummyIn[vbundle]},$ExtrinsicKSign extrinsicKname[dummy,-dummy]superepsilonname[inds]];
superepsilonname/: LieD[vector[_]][superepsilonname[inds__?UpIndexQ]]:=Module[{dummy=DummyIn[vbundle],first=First[{inds}]},-$ExtrinsicKSign extrinsicKname[dummy,-dummy]superepsilonname[inds]];
epsilonname/: LieD[vector[_]][epsilonname[inds__?DownIndexQ]]:=Module[{dummy=DummyIn[vbundle]},$ExtrinsicKSign extrinsicKname[dummy,-dummy]epsilonname[inds]];
epsilonname/: LieD[vector[_]][epsilonname[inds__?UpIndexQ]]:=Module[{dummy=DummyIn[vbundle],first=First[{inds}]},-$ExtrinsicKSign extrinsicKname[dummy,-dummy]epsilonname[inds]+$AccelerationSign norm accelerationname[-dummy]superepsilonname[dummy,inds]];
Protect[Evaluate[prot]]
];
];

(* Gauss Codazzi rule, for abstract indices. Only for Riemann. Norms are wrong *)
With[{riemann=Riemann[covd],superRiemann=Riemann[superCD],superRicci=Ricci[superCD],superRicciScalar=RicciScalar[superCD],K=extrinsicKname,AA=accelerationname},
GaussCodazziRules[metric]:={superRiemann[a_?AIndexQ,b_?AIndexQ,c_?AIndexQ,d_?AIndexQ]:>Module[{e=DummyIn@vbundle,PDK},
PDK[x_,y_]:=projectorname[vector[e] superCD[-e]@K[x,y]];
riemann[a,b,c,d]+$RiemannSign(-K[a,c] K[b,d]/norm+K[a,d] K[b,c]/norm-AA[b] AA[d] vector[a] vector[c]/norm-K[b,e] K[d,-e] vector[a] vector[c]/norm^2+AA[a] AA[d] vector[b] vector[c]/norm+K[a,e] K[d,-e] vector[b] vector[c]/norm^2+AA[b] AA[c] vector[a] vector[d]/norm+K[b,e] K[c,-e] vector[a] vector[d]/norm^2-AA[a] AA[c] vector[b] vector[d]/norm-K[a,e] K[c,-e] vector[b]vector[d]/norm^2+$ExtrinsicKSign (vector[b] vector[c]PDK[a,d]/norm^2+vector[a] vector[d]PDK[b,c]/norm^2-vector[a] vector[c]PDK[b,d]/norm^2-vector[b] vector[d]PDK[a,c]/norm^2 +vector[d] covd[a]@K[b,c]/norm-vector[c] covd[a]@K[b,d]/norm-vector[d] covd[b]@K[a,c]/norm+vector[c] covd[b]@K[a,d]/norm+vector[b] covd[c]@K[a,d]/norm-vector[a] covd[c]@K[b,d]/norm-vector[b] covd[d]@K[a,c]/norm+vector[a] covd[d]@K[b,c]/norm)+$AccelerationSign (vector[b] vector[d] covd[c]@AA[a]/norm-vector[a] vector[d] covd[c]@AA[b]/norm-vector[b] vector[c] covd[d]@AA[a]/norm+vector[a] vector[c] covd[d]@AA[b]/norm))],
superRicci[a_?AIndexQ,b_?AIndexQ]:>Module[{c=DummyIn@vbundle},ReleaseHold[Hold[superRiemann[a,-c,b,c]]/.GaussCodazziRules[metric]]],
superRicciScalar[]:>Module[{a=DummyIn@vbundle,b=DummyIn@vbundle},Expand[(metric[a,b]+vector[a]vector[b]/norm)ReleaseHold[Hold[superRicci[-a,-b]]/.GaussCodazziRules[metric]]]]}
];

If[$ProtectNewSymbols,Protect[extrinsicKname,accelerationname,projectorname]];

]
];
VectorOfInducedMetric[metric_Symbol?MetricQ]:=Null;
VectorOfInducedMetric[x_]:=Throw@Message[VectorOfInducedMetric::unknown,"induced metric",x];
SetNumberOfArguments[VectorOfInducedMetric,1];
Protect[VectorOfInducedMetric];


deflistableCovDs[ProjectDerivative];


deflistableMetrics[function_]:=With[{rules=SymbolJoin[function,"Rules"]},
function[expr_]:=function[expr,$Metrics];
function[expr_,list_List]:=Fold[function,expr,list];
function[expr_,metric_Symbol?MetricQ]:=expr/.rules[metric];
SetNumberOfArguments[function,{1,2}];
Protect[function];]
deflistableMetrics/@{MetricToProjector};


$Projectors:=Select[$Metrics,(InducedFrom[#]=!=Null)&]


deflistableProjectors[function_]:=With[{rules=SymbolJoin[function,"Rules"]},
function[expr_]:=function[expr,$Projectors];
function[expr_,list_List]:=Fold[function,expr,list];
function[expr_,metric_Symbol?MetricQ]:=expr//.rules[metric];
SetNumberOfArguments[function,{1,2}];
Protect[function];]
deflistableProjectors/@{ExtrinsicKToGradNormal,GradNormalToExtrinsicK,ProjectorToMetric,GaussCodazzi};


MetricToProjectorRules[_]:={};
ProjectorToMetricRules[_]:={};
ExtrinsicKToGradNormalRules[_]:={};
GradNormalToExtrinsicKRules[_]:={};
ProjectDerivativeRules[_]:={};


project[projector_][expr_,index_]:=Module[{dummy=DummyAs[index]},projector[index,-dummy]ReplaceIndex[expr,index->dummy]]


ProjectWith[projector_][0]:=0;
ProjectWith[projector_][expr_]:=Expand@Fold[project[projector],expr,List@@FindFreeIndices[expr]];
Protect[ProjectWith];


(* Pair of indices for the pair vector[.]vector[.] *)
pair[index_]:={1,inout[index,DummyAs[index]]};
(* vectors inside the projector *)
inside[inouts_,vector_]:=inouts/.inout[_,index_]:>vector[ChangeIndex@index];
(* vectors outside the projector *)
outside[inouts_,vector_]:=inouts/.inout[index_,_]->vector[index];
(* Index rules to change indices of expr *)
inoutrules[1]:={};
inoutrules[inouts_]:=ListOfFactors[inouts]/.inout->Rule;
(* Construct expression inside the projector *)
inoutproject[expr_,inouts_,vector_]:=ReplaceIndex[expr,inoutrules[inouts]]inside[inouts,vector]
(* Construct a term v*v*...*projector[v*v*...*expr] *)
inoutlength[1]:=0;
inoutlength[_inout]:=1;
inoutlength[list__Times]:=Length[list];
inoutall[expr_,projector_,vector_,norm_][inouts_]:=projector[inoutproject[expr,inouts,vector]]outside[inouts,vector]/norm^(inoutlength[inouts])


InducedDecomposition[expr_,{metric_,vector_}]:=
inducedDecomposition[Expand[expr],{Projector[metric],vector,normofvector[vector,metric]}];
SetNumberOfArguments[InducedDecomposition,{2,Infinity}];
Protect[InducedDecomposition];


normofvector[vector_,metric_]:=With[{vbundle=VBundleOfMetric[metric]},Module[{dummy1=DummyIn[vbundle],dummy2=DummyIn[vbundle]},Scalar[ContractMetric[FirstMetricOfVBundle[vbundle][-dummy1,-dummy2]vector[dummy1]vector[dummy2]]]]];


(* Recursive *)
inducedDecomposition[expr_Plus,struct_]:=inducedDecomposition[#,struct]&/@expr;
inducedDecomposition[expr_SeriesData,struct_]:=SeriesDataMap[inducedDecomposition[#,struct]&,expr];
inducedDecomposition[expr_Times,struct_]:=inducedDecomposition[#,struct]&/@expr;
inducedDecomposition[expr:_?ProductQ[___],struct_]:=inducedDecomposition[#,struct]&/@expr;
(* Perform actual decomposition *)
inducedDecomposition[expr:_?xTensorQ[__],struct_]:=inducedDecompositionObject[expr,struct];
inducedDecomposition[expr:_?InertHeadQ[_,z___],struct_]:=inducedDecompositionObject[expr,struct];
inducedDecomposition[expr:_?FirstDerQ[_],struct_]:=inducedDecompositionObject[expr,struct];
(* Other cases *)
inducedDecomposition[expr_,struct_]:=expr;


inducedDecompositionObject[expr_,{projector_,vector_,norm_}]:=Module[{frees=FindFreeIndices[expr]},If[frees===IndexList[],expr,
Plus@@Map[inoutall[expr,projector,vector,norm],Flatten@Outer[Times,Sequence@@(pair/@frees)]]]];


ToInducedDerivative[expr_,supercd_,cd_]:=
With[{indmetric=MetricOfCovD[cd]},With[{vector=Last@InducedFrom[indmetric]},expr/.supercd[ind_][expr1:(_?xTensorQ[___]|_?InertHeadQ[___]|cd[_][_]|LieD[_][_])]:>With[{frees=FindFreeIndices[expr1],norm=normofvector[vector,MetricOfCovD[supercd]]},cd[ind][expr1]+With[{dummy=DummyAs[ind]},vector[ind]/norm DirCovDToLieD[vector[-dummy] supercd[dummy][expr1],vector]]-$ExtrinsicKSign/norm Plus@@(With[{dummy2=DummyAs[#]},vector[#]ExtrinsicK[indmetric][ind,-dummy2]ReplaceIndex[expr1,#->dummy2]]&/@frees)/;OrthogonalToVectorQ[vector][expr1]
]
]
];
SetNumberOfArguments[ToInducedDerivative,3];
Protect[ToInducedDerivative];


buildmetric[metric_]:=Apply[metric,-IndicesOfVBundle[VBundleOfMetric@metric][[1,{1,2}]]];


(* Safety definition *)
ZeroDerOnMetricQ[der_,Null]:=(Print["Checking ",der," on metric Null"];True);
(* Parametric derivatives. Different parameters commute *)
ZeroDerOnMetricQ[OverDot,metric_]:=xTagSet[{metric,ZeroDerOnMetricQ[OverDot,metric]},SameQ[OverDot[buildmetric[metric]],0]];
ZeroDerOnMetricQ[ParamD[par_],metric_]:=xTagSet[{metric,ZeroDerOnMetricQ[ParamD[par],metric]},SameQ[ParamD[par][buildmetric[metric]],0]];
ZeroDerOnMetricQ[ParamD[pars__],metric_]:=Apply[Or,ZeroDerOnMetricQ[ParamD[#],metric]&/@{pars}];
(* Lie derivatives *)
ZeroDerOnMetricQ[LieD[canv_],metric_]:=xTagSet[{metric,ZeroDerOnMetricQ[LieD[canv],metric]},SameQ[LieD[canv][buildmetric[metric]],0]];
(* Covariant derivatives with CD-indices: store index *)
ZeroDerOnMetricQ[covd_?CovDQ[ind__?CDIndexQ],metric_]:=ZeroDerOnMetricQ[derwithindex[covd,ind],metric];
ZeroDerOnMetricQ[derwithindex[covd_,ind__],metric_]:=xTagSet[{metric,ZeroDerOnMetricQ[derwithindex[covd,ind],metric]},SameQ[CheckZeroDerivative[covd[ind][buildmetric[metric]]],0]];
(* Other covariant derivatives: store vbundle *)
ZeroDerOnMetricQ[covd_?CovDQ[ind_?ABIndexQ],metric_]:=ZeroDerOnMetricQ[deronvbundle[covd,VBundleOfIndex[ind]],metric];
ZeroDerOnMetricQ[deronvbundle[covd_,vbundle_],metric_]:=xTagSet[{metric,ZeroDerOnMetricQ[deronvbundle[covd,vbundle],metric]},True]/;SameQ[CovDOfMetric[metric],covd];
ZeroDerOnMetricQ[deronvbundle[covd_,vbundle_],metric_]:=With[{tmpmetric=changetoinducedmetric[covd,metric]},xTagSet[{metric,ZeroDerOnMetricQ[deronvbundle[covd,vbundle],metric]},ZeroDerOnMetricQ[deronvbundle[covd,vbundle],tmpmetric]]/;tmpmetric=!=metric];
ZeroDerOnMetricQ[deronvbundle[covd_,vbundle_],metric_]:=xTagSet[{metric,ZeroDerOnMetricQ[deronvbundle[covd,vbundle],metric]},SameQ[CheckZeroDerivative[covd[-DummyIn[vbundle]][buildmetric[metric]]],0]];
(* Multi-index derivative *)
ZeroDerOnMetricQ[covd_?CovDQ[inds__],metric_]:=SameQ[CheckZeroDerivative[covd[inds][buildmetric[metric]]],0];


changetoinducedmetric[covd_,metric_]:=changetoinducedmetric[covd,metric,MetricOfCovD[covd]];
changetoinducedmetric[covd_,metric_,Null]:=metric;
changetoinducedmetric[covd_,metric_,metric2_]:=changetoinducedmetric[covd,metric,metric2,InducedFrom[metric2]];
changetoinducedmetric[covd_,metric_,metric2_,{metric_,_}]:=metric2;
changetoinducedmetric[covd_,metric_,metric2_,_]:=metric;


sortder[OverDot]:=OverDot;
sortder[ParamD[pars__]]:=ParamD[pars];
sortder[LieD[v_]]:=LieD[ToCanonicalDir[v]];
sortder[covd_?CovDQ[ind_?CDIndexQ]]:=derwithindex[covd,ind];
sortder[covd_?CovDQ[ind_]]:=deronvbundle[covd,VBundleOfIndex[ind]];


$MixedDers=True;


DersIn[sortedexpr_Object]:=Union[sortder/@Cases[sortedexpr,MyDer[der_]->der,{0,Infinity}]];
MetricsIn[sortedexpr_Object]:=FirstMetricOfVBundle/@Select[Union[VBundleOfIndex/@Apply[List,First@Last[sortedexpr]]],MetricEndowedQ];
riskydermetricQ[der_,metric_]:=If[ZeroDerOnMetricQ[der,metric],Null,risk[der,metric]];
BasesIn[sortedexpr_Object]:=Union[Cases[sortedexpr,({_,_. basis_?BasisQ}):>basis,{0,Infinity}]];
riskyderbasisQ[derwithindex[der_,_],basis_]:=riskyderbasisQ[der,basis];
riskyderbasisQ[der_,basis_]:=If[der===xAct`xCoba`PDOfBasis[basis],Null,risk[der,basis]];
RiskyDers[sortedexpr_Object]:=With[{dersin=DersIn[sortedexpr]},
Cases[Union@Join[
Flatten@Outer[riskydermetricQ,dersin,MetricsIn[sortedexpr]],Flatten@Outer[riskyderbasisQ,dersin,BasesIn[sortedexpr]]
],_risk]
];


careQ[sortedexpr_Object,options___]:=RiskyDers[sortedexpr]=!={};


SetCharacters[expr_,tensor_?xTensorQ,chars_List]:=expr/.rest_. tensor[inds__]:>setcharacters[rest,tensor[inds],chars/.{Up->1,Down->-1}];
SetCharacters[expr_,tensor_?xTensorQ]:=SetCharacters[expr,tensor,SlotsOfTensor[tensor]/.{-_Symbol->-1,_Symbol->1}];
SetCharacters[expr_,list_List]:=Fold[SetCharacters,expr,list];
SetCharacters[expr_]:=SetCharacters[expr,Head/@FindAllOfType[expr,Tensor]];
SetNumberOfArguments[SetCharacters,{1,3}];
Protect[SetCharacters];


setcharacters[rest_,tensor_[inds__],chars_]:=Module[{deltas,newinds=chars (DummyIn[VBundleOfIndex[#]]&/@{inds})},
deltas=Inner[delta,IndexList[inds],ChangeIndex/@IndexList@@newinds,Times];
ContractMetric[rest deltas]tensor@@newinds]


(****************** 15.Symmetries and canonicalization ****************)


If[$ReadingVerbose,Print["Reading section 15: Symmetries and canonicalization."],Null,Null]


(* Default for $TCOptions *)
$TCOptions={};
(* Canonical form for a scalar (with or without Scalar head) *)
ToCanonicalScalar[Scalar[expr_]]:=Scalar[ToCanonicalScalar[expr]];
ToCanonicalScalar[expr_]:=ReplaceDummies[ToCanonical[expr,$TCOptions],IndexList@@$AbstractIndices];
(* Canonical form for a direction (with or without Dir head) *)
ToCanonicalDir[Dir[expr_]]:=Dir[ToCanonicalDir[expr]];
ToCanonicalDir[expr_]:=With[{indices=IndexList@@$AbstractIndices,index=UltraindexOf[expr]},
Simplify@ReplaceIndex[
ReplaceDummies[ToCanonical[expr,$TCOptions],Rest[indices]],index:>If[UpIndexQ[index],UpIndex,DownIndex][First[indices]]
]
];
(* Canonicalize Dir indices in the expression *)
ToCanonicalIndex[expr_]:=expr/.v_Dir:>ToCanonicalDir[v];


isProductQ[prod_,prod_]:=True;
isProductQ[VerbatimProduct[prod_],prod_]:=True;
isProductQ[CommutingObjects[prod_],prod_]:=True;
isProductQ[_,prod_]:=False;


(* Metric state of an index *)
metricstate[index_]:=metricstate1[SignedVBundleOfIndex[index]];
metricstate1[vbundle_]:={vbundle,FirstMetricOfVBundle[vbundle,False]};
(* Modification induced by a derivative *)
metricderstate[{vbundle_,Null},der_]:={vbundle,Null};
metricderstate[{vbundle_,metric_},der_]:=If[ZeroDerOnMetricQ[der,metric],{vbundle,metric},{vbundle,metric,der}];
metricderstate[{vbundle_,metric_,other__},der_]:={vbundle,metric,other,der};


EmptyILs:={IndexList[],IndexList[],IndexList[],IndexList[],IndexList[]};


Identify[x_?NumericQ]:=Object[x,{Constant,1,x},EmptyILs];
Identify[x_String]:=Throw@Message[Validate::nouse,"Identify",x];


Identify[list_List]:=Identify/@list;


Identify[tensor_?xTensorQ[]]:=With[{t=ToCanonicalTensorHead[tensor]},Object[t[],{Tensor,1,t},EmptyILs]];
Identify[tensor_?xTensorQ[inds__]]:=
With[{ilist=ToCanonicalIndex[IndexList[inds]],t=ToCanonicalTensorHead[tensor]},
With[{free=TakeFrees[ilist],dum=TakeEPairs[ilist]},
Object[t@@ilist,{Tensor,OrderOfGroup@SymmetryGroupOfTensor[t[inds]],t},{ilist,free,dum,Complement[ilist,free,dum],metricstate/@ilist}]
]
];
Identify[cs_Symbol?ConstantSymbolQ]:=Object[cs,{ConstantSymbol,1,cs},EmptyILs];
Identify[p_Symbol?ParameterQ]:=Object[p,{Parameter,1,p},EmptyILs];


Identify[covd_?CovDQ[ind__][expr_]]:=addderivative[ToCanonicalIndex[covd[ind]],Identify[expr]];
Identify[OverDot[expr_]]:=addderivative[OverDot,Identify[expr]];
Identify[ParamD[ps__][expr_]]:=addderivative[ParamD[ps],Identify[expr]];
Identify[LieD[v_][expr_]]:=addderivative[LieD[ToCanonicalDir[v]],Identify[expr]];


appendindex[ind_,{allinds_,frees_,dummies_,blocked_,mstates_}]:=With[{cind=ChangeIndex[ind]},
Which[
(* Pair in free indices: add to dummies *)
MemberQ[frees,cind],
{Append[allinds,ind],DeleteCases[frees,cind],Append[Append[dummies,cind],ind],blocked,Append[mstates,metricstate[ind]]},
(* Traceable index: add to frees *)
EIndexQ[ind],
{Append[allinds,ind],Append[frees,ind],dummies,blocked,Append[mstates,metricstate[ind]]},
(* Blocked index: add to blocked *)
BlockedQ[ind],
{Append[allinds,ind],frees,dummies,Append[blocked,ind],Append[mstates,metricstate[ind]]},
True,
Throw@Message[ToCanonical::nouse,"appendindex",ind]
]
];


(* Combine indices in products *)
TimesIndices[{alls_,frees_,dums_,bls_,mss_}]:=
With[{free=Join@@frees},
If[DuplicateFreeQ[free],
With[{all=Join@@alls,newfree=TakeFrees[free],blocked=Join@@bls},
{all,newfree,Complement[all,newfree,blocked],blocked,Union@@mss}
],
Throw@Message[Validate::repeated,Complement[free,DeleteDuplicates[free]]]]
];


(* Combine indices in sums *)
PlusIndices[{alls_,frees_,dums_,bls_,mss_}]:=
If[SameQ@@(Sort/@frees),
{Union@@alls,First@frees,Union@@dums,Union@@bls,Union@@mss},
Throw@Message[Validate::inhom,"indices",frees]
];


freeindices[Object[_,_,{_,frees_,_,_,_}]]:=frees;
freeindices[CommutingObjects[_][exprs___]]:=TakeFrees[Join@@(freeindices/@{exprs})];
freeindices[VerbatimProduct[_][exprs___]]:=TakeFrees[Join@@(freeindices/@{exprs})];
freeindices[expr_VerbatimPlus]:=freeindices[First[expr]];


dername[covd_?CovDQ[ind__]]:=covd;
dername[der_LieD]:=der;
dername[der_ParamD]:=der;
dername[OverDot]:=OverDot;
addderivative[der_,Object[object_,{type_,sym_,name_},inds_]]:=Object[
der[object],
{type,
Flatten@{sym,OrderOfGroup@Last@SymmetryOf[der[object]]},
{dername[der],name,MyDer[der]}
},
appendderindex[der,recheckmetricstates[der,inds]]
];


recheckmetricstates[der_,{allinds_,frees_,dummies_,blocked_,mstates_}]:={allinds,frees,dummies,blocked,metricderstate[#,der]&/@mstates};
appendderindex[covd_?CovDQ[ind_],inds_]:=appendindex[ind,inds];
appendderindex[covd_?CovDQ[ind_,other__],inds_]:=appendderindex[covd[other],appendindex[ind,inds]];
appendderindex[der_,inds_]:=inds;


SetAttributes[{addhead1A,addhead1B,addheadnA,addheadnB},HoldFirst];


(* Inert-head *)
Identify[x:(ih_?InertHeadQ[expr_,z___])]:=Module[{canonz={z}/.list_IndexList:>ToCanonicalIndex[list],head},addhead1A[x,Function@@{head[#,Sequence@@canonz]}/.head->ih,expr]];
(* Scalar-function of a single argument *)
Identify[x:(sf_?ScalarFunctionQ[expr_])]:=addhead1A[x,sf,Scalar[expr]];
Identify[x:(Derivative[der_Integer][sf_?ScalarFunctionQ][expr_])]:=addhead1A[x,Derivative[der][sf],Scalar[expr]];
(* Scalar-function of several arguments *)
Identify[x:(sf_?ScalarFunctionQ[exprs___])]:=addheadnA[x,sf,Scalar/@{exprs}];
Identify[s:(Derivative[ders__Integer][sf_?ScalarFunctionQ][exprs__])]:=addheadnA[x,Derivative[ders][sf],Scalar/@{exprs}];


(* One argument. Note that the x expressions are ignored in the B step *)
addhead1A[x_,head_,expr_]:=addhead1B[x,head,Identify[expr]];
addhead1B[x_,head_,Object[object_,{type_,sym_,name_},inds_]]:=Object[head[object],{type,sym,{head,name}},inds];
(* Several arguments. The result has type1 and inds1 *)
addheadnA[x_,head_,exprs_List]:=addheadnB[x,head,Identify/@exprs];
addheadnB[x_,head_,objects:{Object[_,{type1_,sym1_,_},inds1_],__}]:=Object[head@@(First/@objects),{type1,sym1,{head,Sequence@@(objects[[All,2,-1]])}},inds1];


Identify[expr_Scalar]:=With[{expr1=ToCanonicalScalar[expr]},
Object[expr1,{Scalar,1,expr1},EmptyILs]
];


Identify[expr_Plus]:=ToObject[Identify/@VerbatimPlus@@expr];


(* Products. Identify always acts on original expressions, never on Object or VP expressions *)
Identify[expr_Times]:=ToObject[ObjectSort[Identify/@VerbatimProduct[Times]@@expr]];
Identify[expr:prod_?ProductQ[__]]:=ToObject[ObjectSort[Identify/@VerbatimProduct[prod]@@expr]];
Identify[expr:VerbatimProduct[prod_][___]]:=Throw@Message[xSort::error,"Identify on a VP. This should not happen."];


ToCanonical::noident="Unknown expression not canonicalized: `1` .";
Identify[expr_]:=(Message[ToCanonical::noident,expr];Object[expr,{ABBA,1,expr},EmptyILs]);


(* Combine symmetry orders using the *flat* operator op, Times for products or GCD for sums *)
makeList[list_List]:=list;
makeList[x_]:={x};
symListCombine[{},op_]:=Throw["Error in symListCombine"];
symListCombine[{a_},op_]:=a;
symListCombine[list_List,op_]:=With[{llist=makeList/@list},
With[{length=Max[Length/@llist]},
op@@(PadRight[#,length,1]&/@llist)
]
];


(* Handle signs *)
ToObject[AddedSign[1,expr_]]:=ToObject[expr];
ToObject[AddedSign[-1,expr_]]:=MapAt[VerbatimProduct[Times][-1,#]&,ToObject[expr],1];
ToObject[AddedSign[0,expr_]]:=Identify[0];
(* Products of objects *)
ToObject[vprod:(VerbatimProduct|CommutingObjects)[prod_][objects__Object]]:=Object[
vprod,
With[{list=Transpose[{objects}[[All,2]]]},
{
{prod,list[[1]]},
symListCombine[list[[2]],Times],
{prod,list[[3]]}
}
],
TimesIndices[Transpose[{objects}[[All,3]]]]
];
ToObject[expr:(VerbatimProduct|CommutingObjects)[prod_][___]]:=(Print["INFO: ToObject acting on a VP with not all Objects."];ToObject[ToObject/@expr]);
(* Sum (VerbatimPlus) of objects. Note that the first argument contain full objects *)
ToObject[expr:VerbatimPlus[objects__Object]]:=Object[
expr,
With[{list=Transpose[{objects}[[All,2]]]},
{
{Plus,list[[1]]},
symListCombine[list[[2]],GCD],
{Plus,list[[3]]}
}
],
PlusIndices[Transpose[{objects}[[All,3]]]]
];
ToObject[expr_VerbatimPlus]:=(Print["INFO: ToObject acting on a VPlus with not all Objects."];ToObject[ToObject/@expr]);
(* Other cases *)
ToObject[expr_]:=Throw@Message[xSort::error1,"INFO: ToObject: I don't know what to do with",expr];


symnds[sym_,nds_]:=Reverse@PadLeft[Flatten[{sym}],nds,1]


(* Information used to sort with the AutomaticObjectOrder *)
ObjectOrderItems[False][Object[object_,{type_,sym_,name_},{all_,free_,dum_,bl_,ms_}],nds_]:=With[{frees=Intersection[free,$FreesForSorting]},{xSortPrecedence[name],type,symnds[sym,nds],SortingName[name],Length[all],-Length[frees],mstate/@ms}];
ObjectOrderItems[True][Object[object_,{type_,sym_,name_},{all_,free_,dum_,bl_,ms_}],nds_]:=With[{frees=Intersection[free,$FreesForSorting]},{xSortPrecedence[name],type,symnds[sym,nds],SortingName[name],Length[all],-Length[frees],mstate/@ms,bl,IndexSort[free]}];


xSortPrecedence[_]:=100;
SetNumberOfArguments[xSortPrecedence,1];
Protect[xSortPrecedence];


SortingName[name_List]:=name/.MyDer[covd_?CovDQ[_]]:>MyDer[covd];
SortingName[name_Symbol]:=name;


(* Final metric-state *)
mstate[{-vbundle_,__}]:=vbundle;
mstate[{vbundle_,__}]:=vbundle;


AutomaticObjectOrder[freesQ_][object1_Object,object2_Object]:=With[{nds=Max[1,Length[object1[[2,2]]],Length[object2[[2,2]]]]},
Order[ObjectOrderItems[freesQ][object1,nds],ObjectOrderItems[freesQ][object2,nds]]
];


ObjectOrder[prod_,freesQ_]:=AutomaticObjectOrder[freesQ];


ObjectSort[expr:VerbatimProduct[prod_][__Object]]:=Block[{$FreesForSorting=freeindices[expr]},
With[{com=CommutativityOfProduct[prod]},
ProductSplit[com][ProductSort[expr,com]]
]
];


(* No symmetry. Nothing to do *)
ProductSort[expr:VerbatimProduct[prod_][__Object],None]:=AddedSign[1,expr];


(* Commutative product. Move everything freely *)
ProductSort[expr:VerbatimProduct[prod_][__Object],"Commutative"]:=AddedSign[1,Sort[expr,ObjectOrder[prod,False][#1,#2]>=0&]];


(* Two arguments *)
ProductSort[expr:VerbatimProduct[prod_][x_Object,y_Object _],com_]:=Module[{order,sign},
Which[
order=ObjectOrder[prod,com=!="Commutative"][x,y];
order===1, (* x and y correctly sorted. Do nothing *)
	AddedSign[1,expr],
sign=CommutativitySign[prod,com][UxSort[x],UxSort[y]];
order===-1, (* x and y incorrectly sorted. Reverse them and add the sign *)
	AddedSign[sign,Reverse[expr]],
order===0&&sign===1, (* x and y identical, but product is commutative. Do nothing *)
	AddedSign[1,expr],
order===0&&sign===-1, (* x and y identical and produc is anticommutative. Expression is 0 *)
	AddedSign[0,expr]
]
];
(* Associative case *)
nonzeroQ[0]:=False;
nonzeroQ[_]:=True;
ProductSort[expr:VerbatimProduct[prod_][__Object],com_]:=Module[{uxsort},
uxsort[object_]:=uxsort[object]=UxSort@First[object];
ReplaceRepeated[
AddedSign[1,expr],
AddedSign[s_?nonzeroQ,head_[left___Object,x_Object,y_Object,right___Object]]:>Module[{order,sign},
order=ObjectOrder[prod,com=!="Commutative"][x,y];
sign=CommutativitySign[prod,com][uxsort[x],uxsort[y]];
AddedSign[
If[order===0&&sign==-1,0,s sign],
head[left,y,x,right]
]/;(order<0||(order===0&&sign===-1))
]
]
];


ProductSplit[None][expr_]:=expr;
ProductSplit[com_][AddedSign[sign_,expr:VerbatimProduct[prod_][x_Object,y_Object]]]:=AddedSign[sign,If[ObjectOrder[prod,False][x,y]===0,CommutingObjects[prod][x,y],expr]];
ProductSplit[com_][AddedSign[sign_,expr:VerbatimProduct[prod_][__Object]]]:=AddedSign[sign,ApplyCommutingObjects/@Split[expr,ObjectOrder[prod,False][#1,#2]===0&]];
ApplyCommutingObjects[VerbatimProduct[prod_][x_Object]]:=x;
ApplyCommutingObjects[VerbatimProduct[prod_][x__Object]]:=ToObject[CommutingObjects[prod][x]];


xSort[expr_,options:OptionsPattern[]]:=Block[{$TCOptions=CheckOptions[options]},Identify[expr]];
SetNumberOfArguments[xSort,{1,Infinity}];
Protect[xSort];


UxSort[expr_]:=ReleaseHold[Hold[expr]//.{Object[expr1_,__]:>expr1,VerbatimProduct[prod_]:>prod,CommutingObjects[prod_]:>prod,VerbatimPlus->Plus}];
SetNumberOfArguments[UxSort,1];
Protect[UxSort];


MakeBoxes[Object[expr_,rest__],StandardForm]:=TagBox[TooltipBox[UnderscriptBox[MakeBoxes[expr,StandardForm],"_"],MakeBoxes["Object"[expr,rest],StandardForm]],#&];
MakeBoxes[VerbatimProduct[prod_],StandardForm]:=TagBox[TooltipBox[UnderscriptBox[MakeBoxes[prod,StandardForm],"_"],MakeBoxes["VerbatimProduct"[prod],StandardForm]],#&];
MakeBoxes[CommutingObjects[prod_],StandardForm]:=TagBox[TooltipBox[UnderscriptBox[MakeBoxes[prod,StandardForm],"_"],MakeBoxes["CommutingObjects"[prod],StandardForm]],#&];
MakeBoxes[VerbatimPlus[exprs___],StandardForm]:=MakeBoxes["VerbatimPlus"[exprs],StandardForm];
MakeBoxes[MyDer[der_],StandardForm]:=MakeBoxes["MyDer"[der],StandardForm];


Protect[Symmetry];


EmptySymmetry[x_]:=Symmetry[0,x,{},StrongGenSet[{},GenSet[]]];
EmptySymmetry[x_,inds_List]:=Symmetry[Length[inds],x,MapIndexed[Rule[slot@First[#2],#1]&,inds],StrongGenSet[{},GenSet[]]];


$slotSymbol="\[FilledCircle]";
Format[slot[n_Integer]]:=$slotSymbol<>ToString[n];
Unprotect[IndexForm];
IndexForm[slot[n_Integer]]:=$slotSymbol<>ToString[n];
Protect[IndexForm];


slot/:UpIndex[x_slot]:=x;
slot/:NoDollar[x_slot]:=x;


Protect[slot];


ToSlotRules[n_Integer,indices_IndexList]:=List@@MapIndexed[Rule[slot@@(n+#2),#1]&,indices];


DisplaceSlots[GS_GenSet,d_Integer]:=TranslatePerm[GS,xAct`xPerm`Cycles]/.xAct`xPerm`Cycles[cycs__]:>xAct`xPerm`Cycles@@({cycs}+d);
DisplaceSlots[StrongGenSet[base_List,GS_GenSet],d_Integer]:=StrongGenSet[base+d,DisplaceSlots[GS,d]];
DisplaceSlots[Symmetric[list_],d_Integer]:=Symmetric[list+d];
DisplaceSlots[Antisymmetric[list_],d_Integer]:=Antisymmetric[list+d];


DisplaceSlots[Symmetry[n_,expr_,slotlist_,SGS_],d_Integer]:=Symmetry[n+d,expr/.slot[x_]->slot[x+d],slotlist/.slot[x_]->slot[x+d],DisplaceSlots[SGS,d]];


addonetolast[{x___,0}]:={x,1};
DisplaceSlots[expr_,d_Integer]:=xEvaluateAt[expr/.slot[x_]->slot[x+d],addonetolast/@Position[expr,slot]];


JoinSGSoverlap[left___,sym:(_Symmetric|_Antisymmetric),right___]:=JoinSGSoverlap[left,SGSofsym[sym],right];
JoinSGSoverlap[SGSs__StrongGenSet]:=StrongGenSet[MyUnion@@#1,Union@@#2]&@@Transpose[Apply[List,{SGSs},1]];
JoinSGSoverlap[]:=StrongGenSet[{},GenSet[]];
MyUnion[base_List,base1_List]:=Fold[MyUnion1,base,base1];
MyUnion1[base:{___,x_,___},x_]:=base;
MyUnion1[{x___,y_,z___},p_]:={x,p,y,z}/;y>p;
MyUnion1[{x___},p_]:={x,p};


(* Main *)
Symmetry1D[Symmetry[n_,expr_,inds_,sym_]]:=Symmetry[n,expr,inds,Symmetry1D[sym,inds]];
Symmetry1D[sym:(_Symmetric|_Antisymmetric),inds_]:=Symmetry1D[SGSofsym[sym],inds];
(* On a SGS, with the list of slot->aindex *)
Symmetry1D[sym_StrongGenSet,inds_]:=
JoinSGSoverlap[sym,Symmetry1D[Cases[List@@inds,_[_slot,_?AIndexQ]]]];
(* Construct objects {vb, dim, upQ, slot} *)
Symmetry1D[inds_List]:=Module[{vbundles=VBundleOfIndex/@Last/@inds,dims},
dims=DimOfVBundle/@vbundles;
If[FreeQ[dims,1],
StrongGenSet[{},GenSet[]],
Apply[JoinSGS,sym1vbundle/@Split[
Sort@Cases[
Transpose[{vbundles,dims,UpIndexQ/@Last/@inds,First/@First/@inds}],{_,1,_,_}],(First[#1]===First[#2])&]
]
]
];
(* One vbundle: check whether there is a metric *)
sym1vbundle[inds_List]:=sym1vbundle[inds,MetricEndowedQ[inds[[1,1]]]];
(* There is a metric: symmetry on all indices *)
sym1vbundle[inds_List,True]:=SGSofsym@Symmetric[Last/@inds];
(* There is no metric: separate upper and lower indices *)
sym1vbundle[inds_List,False]:=MapAt[Sort,Apply[JoinSGS,SGSofsym/@Symmetric/@Map[Last,Split[inds,(#1[[3]]===#2[[3]])&],{2}]],1];


Options[SymmetryOf]={CommutePDs->True,ConstantMetric->False};


SymmetryOf[tensor_?xTensorQ[indices___],options___]:=Symmetry[Length[{indices}],tensor@@(First/@#),#,SymmetryGroupOfTensor[tensor[indices]]]&@ToSlotRules[0,IndexList[indices]];


SymmetryOf[covd_?CovDQ[ind__][expr_],options___]:=CovDSymmetryOf[covd[ind]][expr,options];


symmetryz[symmetry_,{}]:=symmetry;
symmetryz[symmetry:Symmetry[_,_,rules_,_],{z__}]:=symmetryz1[symmetry,{z},appendzrules[rules,{z}]];
appendzrules[rules_,{z__}]:=Join[rules,ToSlotRules[Length[rules],DeleteCases[IndexList@@Join@@Cases[{z},_IndexList],Alternatives@@(Last/@rules)]]];
symmetryz1[symmetry_,{z__},rules_]:=
symmetryz2[symmetry,{z}/.list_IndexList:>Replace[list,Reverse/@rules,{1}],rules];
symmetryz2[Symmetry[n_,ih_[expr_,z1__],rules1_,sym_],{z2__},rules2_]:=Symmetry[Length[rules2],ih[expr,z2],rules2,sym];


SymmetryOf[LieD[v_][expr_],options___]:=MapAt[LieD[v],SymmetryOf[expr,options],2];
SymmetryOf[ParamD[ps__][expr_],options___]:=MapAt[ParamD[ps],SymmetryOf[expr,options],2];
SymmetryOf[OverDot[expr_],options___]:=MapAt[OverDot,SymmetryOf[expr,options],2];
SymmetryOf[ih_?InertHeadQ[expr_,z___],options___]:=symmetryz[MapAt[ih[#,z]&,SymmetryOf[expr,options],2],{z}];


SymmetryOf[x_Scalar,options___]:=EmptySymmetry[x];
SymmetryOf[x:_?ScalarFunctionQ[___],options___]:=EmptySymmetry[x];
SymmetryOf[x:Derivative[ders__][_?ScalarFunctionQ][__],options___]:=EmptySymmetry[x];


SymmetryOf[(f_Symbol/;f===Times)[x___,y_?NonIndexedScalarQ,z___],options___]:=MapAt[Times[y,#]&,SymmetryOf[Times[x,z],options],2];
SymmetryOf[x:Plus[__?NonIndexedScalarQ],options___]:=EmptySymmetry[x];


SymmetryOf[expr:(VerbatimProduct|CommutingObjects)[prod_][___],options___]:=
SymmetryOfVerbatimProduct[expr,options];


SymmetryOf[expr_Object,options___]:=SymmetryOfObject[expr,options];


SymmetryOf[expr_,options___]:=SymmetryOfObject[xSort@MathInputExpand[expr],options];


SymmetryOf[expr_Plus,options___]:=Throw[Message[SymmetryOf::nouse,"SymmetryOf",expr]];


SetNumberOfArguments[SymmetryOf,{1,Infinity}];
Protect[SymmetryOf];


JoinExprs[prod_][Null,expr2_]:=Flatten[VerbatimProduct[prod][expr2],1,VerbatimProduct[prod]];
JoinExprs[prod_][expr1_,expr2_]:=Flatten[VerbatimProduct[prod][expr1,expr2],1,VerbatimProduct[prod]];


JoinSymmetries[prod_][Symmetry[n1_,expr1_,rules1_,SGS1_],Symmetry[n2_,expr2_,rules2_,SGS2_]]:=
Symmetry[
n1+n2,
JoinExprs[prod][expr1,DisplaceSlots[expr2,n1]],
Join[rules1,DisplaceSlots[rules2,n1]],
JoinSGS[SGS1,DisplaceSlots[SGS2,n1]]
];


SymmetryOfVerbatimProduct[expr:VerbatimProduct[prod_][___],options___]:=Fold[JoinSymmetries[prod],EmptySymmetry[Null],SymmetryOf[#,options]&/@List@@expr];


CommutingCycles[sign_,ns_Integer,ns_Integer]:=GenSet[];
CommutingCycles[0,nstotal_Integer,ns_Integer]:=GenSet[];
CommutingCycles[sign_,nstotal_Integer,ns_Integer]:=Join[
CommutingCycles[sign,nstotal-ns,ns],
GenSet[
sign xAct`xPerm`Cycles@@Transpose[{Range[nstotal-2ns+1,nstotal-ns],Range[nstotal-ns+1,nstotal]}]
]
];


JoinSGSCommuting[sign_,SGS_,0,0]:=SGS;
JoinSGSCommuting[sign_,StrongGenSet[base_,gs_],nstotal_,ns_]:=Module[{cycles=CommutingCycles[sign,nstotal,ns],elements=Partition[Range[nstotal],ns],newbase=base},If[FreeQ[base,Alternatives@@#],AppendTo[newbase,First[#]]]&/@elements;
StrongGenSet[newbase,Join[gs,cycles]]
];


SymmetryOfVerbatimProduct[CommutingObjects[prod_][exprs__],options___]:=Module[{no=Length[{exprs}],nstotal,sym,SGS,sign},
(* Construct Symmetry object apart from generating set *)
sym=Fold[JoinSymmetries[prod],EmptySymmetry[Null],SymmetryOf[#,options]&/@{exprs}];
(* Number of slots in the expression *)
nstotal=Length[sym[[3]]];
(* New generating set *)
SGS=SGSofsym[Last[sym]];
sign=CommutativitySign[prod,CommutativityOfProduct[prod]]@@Take[{exprs},2];
If[sign=!=0,SGS=JoinSGSCommuting[sign,SGS,nstotal,nstotal/no]];
(* New Symmetry object *)
ReplacePart[sym,SGS,{4}]
];


CovDSymmetryOf[cds__][covd_?CovDQ[ind__][expr_],options___]:=CovDSymmetryOf[covd[ind],cds][expr,options]
CovDSymmetryOf[cds__][expr_,options___]:=AD[Append[SymmetryOf[expr,options],CovDTMP[cds]],options]


(* If there are no more derivatives return the symmetry *)
AD[Symmetry[n_,expr_,rules_,SGS_,CovDTMP[]],options___]:=Symmetry[n,expr,rules,SGS]


(* If the first covd cannot commute, remove it *)
AD[Symmetry[n_,expr_,rules_,SGS_,CovDTMP[covd_[i_/;Not@AIndexQ[i]],y___]],options___]:=AD[Symmetry[n+1,covd[slot[n+1]][expr],Append[rules,slot[n+1]->i],SGS,CovDTMP[y]],options];


(* If the first and second derivatives are different, remove the first *)
AD[Symmetry[n_,expr_,rules_,SGS_,CovDTMP[covd1_[i1_],covd2_[i2_],y___]/;covd1=!=covd2],options___]:=AD[Symmetry[n+1,covd1[slot[n+1]][expr],Join[rules,{slot[n+1]->i1}],SGS,CovDTMP[covd2[i2],y]],options];


$CommuteCovDsOnScalars=True;


(* Two equal derivatives with lower abstract indices of a scalar commute if the derivative has no torsion. We check for curvate; the non-curvature case is treated below *)
AD[Symmetry[n_,expr_,rules_,SGS_,CovDTMP[covd_[i1_?AIndexQ],covd_[i2_?AIndexQ],y___]],options___]:=With[{firstmetric=FirstMetricOfVBundle[VBundleOfIndex[i1],False],metric=MetricOfCovD[covd]},AD[Symmetry[n+2,covd[slot[n+2]][covd[slot[n+1]][expr]],Join[rules,{slot[n+1]->i1,slot[n+2]->i2}],JoinSGS[SGS,SGSofsym@Symmetric[{n+1,n+2}]],CovDTMP[y]],options]/;$CommuteCovDsOnScalars&&
CurvatureQ[covd]&&
Not@TorsionQ[covd]&&
ScalarQ[expr/.rules]&&(DownIndexQ[i1]&&DownIndexQ[i2]||(metric=!=Null&&(metric===firstmetric ||MatchQ[InducedFrom@metric,{firstmetric,_}]))
)
];


(* Other derivatives with curvature or torsion are not commuted, even if they do commute; this is left to the user *)
AD[Symmetry[n_,expr_,rules_,SGS_,CovDTMP[covd_[i_]/;Or[CurvatureQ[covd],TorsionQ[covd]],y___]],options___]:=AD[Symmetry[n+1,covd[slot[n+1]][expr],Append[rules,slot[n+1]->i],SGS,CovDTMP[y]],options];


(* Drivers for the three remaining cases *)
AD[Symmetry[n_,expr_,rules_,SGS:StrongGenSet[base_,GS_],CovDTMP[y:covd_[_?AIndexQ]..,covd1_[i_],z___]],options___]:=ADSym[Symmetry[n,expr,rules,SGS,CovDTMP[y],CovDTMP[covd1[i],z]],options]/;covd=!=covd1;
AD[Symmetry[n_,expr_,rules_,SGS:StrongGenSet[base_,GS_],CovDTMP[y:covd_[_?AIndexQ]..,covd_[i_/;Not@AIndexQ[i]],z___]],options___]:=ADSym[Symmetry[n,expr,rules,SGS,CovDTMP[y],CovDTMP[covd[i],z]],options];
AD[Symmetry[n_,expr_,rules_,SGS:StrongGenSet[base_,GS_],CovDTMP[y:covd_[_?AIndexQ]..]],options___]:=ADSym[Symmetry[n,expr,rules,SGS,CovDTMP[y],CovDTMP[]],options];


AD[Symmetry[n_,expr_,rules_,SGS_,CovDTMP[covd_[i_,is__],other___]],options___]:=AD[With[{length=Length[{i,is}]},Symmetry[n+length,Apply[covd,slot/@Range[n+1,n+length]][expr],Join[rules,MapThread[Rule,{slot/@Range[n+1,n+length],{i,is}},1]],JoinSGS[SGS,DisplaceSlots[SymmetryGroupOfCovD[covd[i,is]],n]],CovDTMP[other]]],options];


(* If there are several derivatives that can commute, give GS *)
ADSym[Symmetry[n_,expr_,rules_,SGS_,CovDTMP[y__],CovDTMP[z___]],options:OptionsPattern[]]:=Module[{der,comPD,constmet,inds,newSGS,slotrules,slots},{comPD,constmet}=OptionValue[SymmetryOf,{options},{CommutePDs,ConstantMetric}];
der=Head[First[{y}]];
inds=Apply[Identity,IndexList[y],{1}];
slotrules=ToSlotRules[n,inds];
slots=First/@First/@slotrules;
newSGS=If[comPD&&(constmet||(And@@(DownIndexQ/@inds))),JoinSGS[SGS,SGSofsym@Symmetric[slots]],SGS];
AD[Symmetry[n+Length[inds],Last@ComposeList[der/@slot/@slots,expr],Join[rules,slotrules],newSGS,CovDTMP[z]]]
];


SymmetryOfObject[Object[expr:(VerbatimProduct|CommutingObjects)[_][___],__],options___]:=
SymmetryOfVerbatimProduct[expr//.Object[expr1_,__]:>expr1,options];


SimpleObjectQ[Object[_,_,{_,IndexList[],IndexList[],_,_}]]:=True;
SimpleObjectQ[_]:=False;


SymmetryOfObject[object:Object[expr_,_,_],options___]:=
If[SimpleObjectQ[object],
EmptySymmetry[expr],
SymmetryOf[expr//.Object[expr1_,__]:>expr1,options]
];


SymmetryOfObject[Object[expr_VerbatimPlus,__],options___]:=Throw[Message[SymmetryOf::nouse,"SymmetryOf",UxSort[expr]]];


Options[ToCanonical]:={
Verbose->False,
UseMetricOnVBundle->All,
Method->{ChangeCovD,"ExpandChristoffel"->False},
MathLink:>$xpermQ,
TimeVerbose->False
};


(* 1. ToCanonical on atoms: Identity *)
ToCanonical[x_Symbol,options___]:=x;
ToCanonical[x:(_Integer|_Rational|_Real|_Complex),options___]:=x;
ToCanonical[x_String,options___]:=x;


(* 2. ToCanonical is threaded over lists, equations and sums *)
ToCanonical[list_List,options___]:=Map[ToCanonical[#,options]&,list];
ToCanonical[Equal[lhs_,rhs_],options___]:=Apply[Equal,Map[ToCanonical[#,options]&,SameDummies[{lhs,rhs}]]];
ToCanonical[expr_Plus,options___]:=MapIfPlus[ToCanonical[#,options]&,SameDummies@MathInputExpand[expr]];


(* 3. ToCanonical on rules: do nothing *)
ToCanonical[expr_Rule,options___]:=expr;
ToCanonical[expr_RuleDelayed,options___]:=expr;


(* 4. Special simple cases. These are exceptions to the general algorithm *)
ToCanonical[tensor_?xTensorQ[],options___]:=ToCanonicalTensorHead[tensor][];
ToCanonical[tensor_?xTensorQ[ind_],options___]:=Block[{$TCOptions=CheckOptions[options]},ToCanonicalTensorHead[tensor][ToCanonicalIndex[ind]]];
ToCanonical[covd_?CovDQ[ind_][tensor_?xTensorQ[]],options___]:=Block[{$TCOptions=CheckOptions[options]},covd[ToCanonicalIndex[ind]][ToCanonicalTensorHead[tensor][]]];


(* 5. A single tensor. An exception to the general algorithm *)
ToCanonical[tensor_?xTensorQ[inds__],options___]:=With[{t=ToCanonicalTensorHead[tensor],newinds=ToCanonicalIndex[IndexList[inds]]},
puthead[
t,
Sow[ToCanonicalOne[newinds,Union[UpIndex/@TakeEPairs[newinds]],SymmetryGroupOfTensor[t@@newinds],options],"NewIndices"]
]
];
puthead[tensor_,{sign_,inds_}]:=sign tensor@@inds;


(* 6. ToCanonical on unsorted expressions: apply xSort, then canonicalize, and then UxSort *)
ToCanonical[expr_,options___]:=
UxSort[
ToCanonicalObject[
xSort[
SameDummies@MathInputExpand[expr],
options
],options
]
];


SetNumberOfArguments[ToCanonical,{1,Infinity}];
Protect[ToCanonical];


(* Lie brackets of vectors with ultraindices *)
ToCanonicalTensorHead[Bracket[v1_,v2_]]:=With[{cv1=ToCanonicalDir[v1],cv2=ToCanonicalDir[v2]},
Which[
v1===v2,Zero,
OrderedQ[{cv1,cv2}],Bracket[cv1,cv2],
True,MultiplyHead[-1,Bracket[cv2,cv1]]
]
];
(* Other. Match the behaviour of ToCanonical pre1.1 *)
ToCanonicalTensorHead[tensor_]:=tensor;


ToCanonicalObject[Object[expr_VerbatimPlus,__],options___]:=ToCanonicalObject[#,options]&/@expr;


ToCanonicalObject[object_,options:OptionsPattern[]]:=Module[
{sym=SymmetryOfObject[object],verb,mms,indices,dummies,vbundles,newindices,riskyders,result},
{verb,mms}=OptionValue[ToCanonical,{options},{Verbose,UseMetricOnVBundle}];

If[verb,
Print["***********************************************************"];
Print["ToCanonical:: object: ",object];
Print["ToCanonical:: sym: ",sym]
];

(* 0. Indices. Use sym and not object to ensure the right ordering of indices *)
indices=IndexList@@(Last/@sym[[3]]);
dummies=Union[UpIndex/@object[[3,3]]];
vbundles=Union[VBundleOfIndex/@List@@indices];

(* 1. Group theory *)
newindices=ToCanonicalOne[indices,dummies,sym[[4]],options];
If[verb,Print["ToCanonical:: newindices: ",newindices]];

(* 2. List of risky derivatives on metrics. QUESTION: what about bases? And inert heads? Dietmar Theiss has reported a xCoba case in which this leads to a bug *)
Switch[mms,
None,mms={},
All,mms=$VBundles];
riskyders=If[
Or[!$MixedDers,
newindices===0,
Intersection[vbundles,mms]==={},
(UpIndexQ/@indices)===(UpIndexQ/@Last@newindices)],
{},
Apply[List,RiskyDers[object],{1}]/.deronvbundle->Sequence
];
If[verb,Print["ToCanonical:: riskyders: ",riskyders]];

(* 3. Compute result from newindices *)
result=Which[
newindices==={0,indices},
0,
newindices==={1,indices},
object,
riskyders==={},
Reconstruct[sym,newindices],
True,
Message[ToCanonical::cmods,First/@riskyders];
ToCanonicalDers[UxSort[Reconstruct[sym]],options]
];
If[verb,Print["ToCanonical:: result: ",result]];

(* Final result, sowing the list of indices *)
Sow[newindices,"NewIndices"];
result
];
ToCanonical::cmods="Detected metric-incompatible derivatives `1`.";


Reconstruct[Symmetry[n_,smt_,slotrules_,_]]:=smt/.slotrules;
Reconstruct[Symmetry[n_,smt_,slotrules_,_],{sign_,perm_IndexList}]:=sign smt/.Inner[Rule,slot/@IndexList@@Range[n],perm,List];


DefInertHead[TMPChristoffel,LinearQ->True,ProtectNewSymbol->False,DefInfo->False];
DefInertHead[TMPCovD,ProtectNewSymbol->False,DefInfo->False,ContractThrough->{delta}];
TMPCovD[0,_]:=0;
(* Define the weight *)
WeightOf[TMPChristoffel[expr_,z___]]^:=WeightOf[expr];
WeightOf[TMPCovD[expr_,z___]]^:=WeightOf[expr];


(* One argument to three or four arguments. Recursion on expr. Param ders not working *)
addTMPCovD[expr_]:=expr/.{
covd_?CovDQ[ind_][expr1_]:>addTMPCovD[addTMPCovD[expr1],covd[ind],CovD,VBundleOfIndex[ind]],
LieD[v_][expr1_]:>addTMPCovD[addTMPCovD[expr1],LieD[v],LieD,VBundleOfIndex[Dir[v]]],
OverDot[expr1_]:>addTMPCovD[addTMPCovD[expr1],OverDot,OverDot],
ParamD[pars__][expr1_]:>addTMPCovD[addTMPCovD[expr1],ParamD[pars],ParamD]
}


(* Four to five arguments. Add vbundle metric  *)
addTMPCovD[expr_,der_,type_,vbundle_]:=addTMPCovD[expr,der,type,vbundle,FirstMetricOfVBundle[vbundle,False]];


(* If there is no metric in the vbundle do not change derivative *)
addTMPCovD[expr_,der_,_,_,Null]:=der[expr];
(* Covariant derivatives. Use changeCovD adding the TMPCovD head. Do not mark Christoffels *)
addTMPCovD[expr_,covd_[ind_],CovD,vbundle_,metric_]:=changeCovD[covd[ind][expr],covd,CovDOfMetric[metric],TMPCovD,Identity];
(* Lie derivatives. Add the TMPCovD head. Do not mark derivatives of vector v *)
addTMPCovD[expr_,LieD[v_],LieD,vbundle_,metric_]:=
ExpandLieD[v,CovDOfMetric[metric],True,TMPCovD][expr];


removeTMPCovD[expr_]:=changeTMPCovD[expr]//.TMPCovD[expr1_,__]->expr1;
changeTMPCovD[expr_]:=expr/.TMPCovD[covd_?CovDQ[ind_][expr1_],oldcovd_]:>TMPCovD[changeTMPCovD[covd[ind][changeTMPCovD@expr1],oldcovd],oldcovd];


changeTMPCovD[covd_?CovDQ[ind_][expr_],oldcovd_?CovDQ]:=changeCovD[covd[ind][expr],covd,oldcovd,Identity1,TMPChristoffel];


changeTMPCovD[covd_?CovDQ[dir_][expr_],LieD]:=CovDToLieD[covd[dir][expr],TMPChristoffel];


SecondToCanonical[expr_]:=secondcan[Expand[expr]];
secondcan[expr_Plus]:=secondcan/@expr;
secondcan[expr_]:=If[MemberQ[expr,TMPChristoffel,{0,Infinity},Heads->True],ToCanonical[expr/.TMPChristoffel->Identity],
expr];


ToCanonicalDersChangeCovD[expr_,method_,options:OptionsPattern[]]:=Module[{verb,eC,result},
verb=OptionValue[ToCanonical,{options},Verbose];
eC="ExpandChristoffel"/.method/.Rest[Method/.Options[ToCanonical]];

result=Tee[SameDummies,verb]@Tee[SecondToCanonical,verb]@Tee[removeTMPCovD,verb]@Tee[ToCanonical,verb]@Tee[addTMPCovD,verb][expr];

If[eC,ToCanonical[result//ChristoffelToGradMetric//ContractMetric,UseMetricOnVBundle->None],result]
];


ToCanonicalDersImplode[expr_,options:OptionsPattern[]]:=With[{verb=OptionValue[ToCanonical,{options},Verbose]},

ToCanonical[
Tee[Explode,verb]@ToCanonical[
Tee[Implode,verb][expr],
options],
UseMetricOnVBundle->None,options]

];


Tee[command_,verb_][expr_]:=Module[{result=command[expr]},
If[verb,Print[command,": ",result]];result];


ToCanonicalDers[expr_,options:OptionsPattern[]]:=With[{method=OptionValue[ToCanonical,{options},Method]},
Switch[method,
ChangeCovD|{ChangeCovD,___},ToCanonicalDersChangeCovD[expr,Rest@Flatten[{method}],options],
Implode,ToCanonicalDersImplode[expr,options],
_,Throw@Message[ToCanonical::unknown,"method",method]
]
];


posits[list_List]:=Apply[Join,posits/@list];
posits[DummySet[_,{pairs___},_]]:=Join[pairs];
posits[RepeatedSet[list_]]:=list;


MakeDummySet[dummies_,fm_,vbundle_]:=DummySet[vbundle,List@@Select[dummies,(VBundleOfIndex[#]===vbundle)&],If[fm&&MetricEndowedQ[vbundle],SymmetryOfMetric@FirstMetricOfVBundle[vbundle],0]];


$RepeatedSingletonsQ=True;


ToCanonicalOne[indices_IndexList,dummies_IndexList,syms_,options:OptionsPattern[]]:=Module[{order=Length[indices],sortedindices,perm,dummysets,frees,verb,not,mms,fm,repes,newsyms},

{verb,mms}=OptionValue[ToCanonical,{options},{Verbose,UseMetricOnVBundle}];

(* Actual configuration *)
If[verb,Print["ToCanonicalOne:: Actual configuration: ",indices]];
(* Standard configuration *)
sortedindices=IndexSort[indices];
If[verb,Print["ToCanonicalOne:: Standard configuration: ",sortedindices]];
(* Look for repeated indices *)
repes=If[$RepeatedSingletonsQ,Union@TakeBlocked[indices],TakeRepeated[indices]];
If[verb,Print["ToCanonicalOne:: Repeated indices: ",repes]];
repes=List@@Map[RepeatedSet,Flatten[IndexPosition[sortedindices,#]]&/@repes];
If[verb,Print["ToCanonicalOne:: Repeated indices: ",repes]];

(* Permutation to be canonicalized *)
perm=TranslatePerm[PermutationFromTo[List@@sortedindices,List@@indices],Images];
If[verb,Print["ToCanonicalOne:: Permutation to be canonicalized: ",perm]];

(* Arrange metric use on the vbundles *)
Switch[mms,
All,fm[_]:=True,
None,fm[_]:=False,
_List,(fm[#]:=True)&/@mms;fm[_]:=False
];

(* Separate dummies according to the existing vbundles *)
If[verb,Print["dummies: ",dummies]];
dummysets=MakeDummySet[dummies,fm[#],#]&/@$VBundles;
If[verb,Print["ToCanonicalOne:: dummysets_tmp: ",dummysets]];
(* Positions of dummies. No spinorial case considered yet *)
dummysets=dummysets/.DummySet[man_,dums_,metricQ_]:>DummySet[man,{IndexPosition[sortedindices,#][[1,1]],IndexPosition[sortedindices,ChangeIndex@#][[1,1]]}&/@dums,metricQ];
If[verb,Print["ToCanonicalOne:: dummysets: ",dummysets]];
(* Positions of free (here all non-dummies or non-repeated) indices *)
frees=Complement[Range@order,posits[dummysets],posits[repes]];
If[verb,Print["ToCanonicalOne:: Free indices: ",frees]];

(* Add symmetries coming from 1D vbundles *)
newsyms=Symmetry1D[syms,Inner[Rule,slot/@Range@Length@indices,List@@indices,List]];

(* !!!!!!!!!!!!!!!! Invert to meet Renato's notation !!!!!!!!!!!!!!!! *)
perm=InversePerm[perm];

(* Canonicalization *)
If[verb,Print["ToCanonicalOne:: calling: ","CanonicalPerm[",perm,",",order,",",newsyms,",",frees,",",Join[dummysets,repes],",",options,"]"]];
perm=CanonicalPerm[perm,order,newsyms,frees,Join[dummysets,repes],FilterRules[{options},Options[CanonicalPerm]]];
If[verb,Print["ToCanonicalOne:: Canonical permutation: ",perm]];

(* !!!!!!!!!!!!!!!! Invert back to our notation !!!!!!!!!!!!!!!! *)
perm=If[perm===0,0,InversePerm[perm]];

(* Final indices: {sign, indexlist} *)
If[perm===0,{0,indices},{If[Head[perm]===Times,-1,1],PermuteList[sortedindices,perm]}]

];


Simplification[expr_,options___]:=Simplify@ToCanonical[expr,options];
SetNumberOfArguments[Simplification,{1,Infinity}];


ImposeSymmetry[expr_,{inds___},SGS_,f_:Identity]:=ImposeSymmetry[expr,IndexList[inds],SGS,f];
ImposeSymmetry[expr_,inds_IndexList,sym:(_Symmetric|_Antisymmetric),f_:Identity]:=ImposeSymmetry[expr,inds,SGSofsym[sym],f];
ImposeSymmetry[expr_,inds_IndexList,SGS_StrongGenSet,f_:Identity]:=ImposeSymmetry[expr,inds,SGS[[2]],f];
ImposeSymmetry[expr_,inds_IndexList,GS_GenSet,f_:Identity]:=ImposeSymmetry[expr,inds,Dimino[GS],f];
ImposeSymmetry[expr_,inds_IndexList,group_Group,Identity]:=Plus@@(PermuteIndices[expr,inds,#]&/@group)/Length[group];
ImposeSymmetry[expr_,inds_IndexList,group_Group,f_]:=Module[{tmp=0,elem},Do[tmp+=f[PermuteIndices[expr,inds,group[[elem]]]],{elem,Length[group]}];tmp]/Length[group];
ImposeSymmetry[expr_,inds_,group_,f_:Identity]:=Throw@Message[ImposeSymmetry::error,"Cannot impose symmetry."];
SetNumberOfArguments[ImposeSymmetry,{3,4}];
Protect[ImposeSymmetry];


xAct`xTensor`Symmetrize[expr_]:=xAct`xTensor`Symmetrize[expr,FindFreeIndices[expr],Identity];
xAct`xTensor`Symmetrize[expr_,{},f_:Identity]:=expr;
xAct`xTensor`Symmetrize[expr_,inds:{__Integer},f_:Identity]:=xAct`xTensor`Symmetrize[expr,FindIndices[expr][[inds]],f];
xAct`xTensor`Symmetrize[expr_,inds_,f_:Identity]:=ImposeSymmetry[expr,inds,Symmetric@Range@Length[inds],f];
SetNumberOfArguments[xAct`xTensor`Symmetrize,{1,3}];
Protect[xAct`xTensor`Symmetrize];


Antisymmetrize[expr_]:=Antisymmetrize[expr,FindFreeIndices[expr],Identity];
Antisymmetrize[expr_,{},f_:Identity]:=expr;
Antisymmetrize[expr_,inds:{__Integer},f_:Identity]:=Antisymmetrize[expr,FindIndices[expr][[inds]],f];
Antisymmetrize[expr_,inds_,f_:Identity]:=ImposeSymmetry[expr,inds,Antisymmetric@Range@Length[inds],f];
SetNumberOfArguments[Antisymmetrize,{1,3}];
Protect[Antisymmetrize];


PairSymmetrize[expr_,{},f_:Identity]:=expr;
PairSymmetrize[expr_,inds:{{_?GIndexQ,_?GIndexQ}..},f_:Identity]:=ImposeSymmetry[expr,Flatten[inds],GenSet@@(xAct`xPerm`Cycles@@#&/@Transpose/@Partition[Partition[Range[2Length[inds]],2],2,1]),f];
SetNumberOfArguments[PairSymmetrize,{2,3}];
Protect[PairSymmetrize];


PairAntisymmetrize[expr_,{},f_:Identity]:=expr;
PairAntisymmetrize[expr_,inds:{{_?GIndexQ,_?GIndexQ}..},f_:Identity]:=ImposeSymmetry[expr,Flatten[inds],GenSet@@(-xAct`xPerm`Cycles@@#&/@Transpose/@Partition[Partition[Range[2Length[inds]],2],2,1]),f];
SetNumberOfArguments[PairAntisymmetrize,{2,3}];
Protect[PairAntisymmetrize];


Cyclize[expr_,inds_List,f_:Identity]:=Plus@@Map[f,ReplaceAll[expr,Map[Inner[Rule,inds,#,List]&,NestList[RotateRight,inds,Length[inds]-1]]]]/Length[inds];
SetNumberOfArguments[Cyclize,{2,3}];
Protect[Cyclize];


(* Plus expression: throw an error *)
ObjectsWithIndices[expr_Plus,_]:=Throw[Message[MonomialsOfTerm::error,"Expression with head Plus cannot be separated in monomials."];ERROR[expr]];
(* List of indices *)
ObjectsWithIndices[term_,list_IndexList]:=Union@Flatten[ObjectsWithIndices[term,#]&/@List@@list];
(* Single index. IsIndexOf used without third argument *)
ObjectsWithIndices[term_,index_]:=Select[ListOfFactors[term],IsIndexOf[#,index]&,1];
Protect[ObjectsWithIndices];


InertHeadQ[Monomial]^=True;
PrintAs[Monomial]^="Monomial";


(* Two arguments: Find free indices of term and set up Monomial. Convert to four arguments. Argument inds can be an index or an IndexList *)
FindMonomial[term_,inds_]:=FindMonomial[term,inds,FindFreeIndices[term],Monomial[1,IndexList[]]];
(* Four arguments. No more indices to look for: stop monomial construction *)
FindMonomial[_,IndexList[],_IndexList,monomial_]:=monomial;
(* Four arguments. More indices to look for *)
FindMonomial[term_,inds_,frees_IndexList,Monomial[monomial_,minds_IndexList]]:=FindMonomial[term,ChangeIndex/@Complement[#[[2]],frees],frees,#]&@(Monomial[#,FindFreeIndices[#]]&@(Times@@ObjectsWithIndices[term/monomial,inds]monomial))
Protect[FindMonomial];


(* Find traceable indices of term and setup list of monomials *)
MonomialsOfTerm[term_]:=MonomialsOfTerm[term,TakeEIndices@FindIndices[term],{}];
(* Select monomial of first index of term *)
MonomialsOfTerm[term_,IndexList[i_,is___],monomials_List]:=MonomialsOfTerm[term,IndexList[is],monomials,FindMonomial[term,i]];
(* New monomial: append to list and eliminate from term *)
MonomialsOfTerm[term_,IndexList[is___],monomials_List,Monomial[monomial_,minds_]]:=MonomialsOfTerm[term/monomial,Complement[IndexList[is],FindIndices[monomial]],Append[monomials,Monomial[monomial,minds]]]
(* Non-indexed monomial. Prepend to list with head ScalarMonomial *)
MonomialsOfTerm[term_,IndexList[],monomials_List]:=Prepend[monomials,ScalarMonomial@@ListOfFactors[term]]


BreakInMonomials[expr_Plus]:=BreakInMonomials/@expr;
BreakInMonomials[expr_]:=Apply[Times,MonomialsOfTerm[expr]/.ScalarMonomial->Times/.Monomial[mono_,inds_]->Monomial[mono]];
SetNumberOfArguments[BreakInMonomials,1];
Protect[BreakInMonomials];


BreakScalars[expr_]:=expr/.Scalar[s_]:>Apply[Times,MonomialsOfTerm[s]//.{ScalarMonomial->Times,Monomial[monomial_,IndexList[]]->Scalar[monomial]}]


makeScalar[ScalarMonomial[expr__]]:=Times[expr]
makeScalar[expr_Scalar]:=expr
makeScalar[Monomial[expr_,IndexList[]]]:=Scalar[expr]
makeScalar[Monomial[expr_,IndexList[__]]]:=expr


PutScalar[expr_]:=Apply[Plus,Apply[Times,makeScalar/@MonomialsOfTerm[#]]&/@ListOfTerms[expr]/.{ScalarMonomial->Times}]


(* Thread over Plus and Times *)
NoScalar[expr_Plus]:=NoScalar/@expr;
NoScalar[expr_Times]:=NoScalar/@expr;
(* Remove Scalar head from positive powers *)
NoScalar[Power[expr_,n_Integer?Positive]]:=Apply[Times,Table[NoScalar[expr],{n}]];
NoScalar[Scalar[expr_]]:=ReplaceDummies[expr];
(* Skip inert heads *)
NoScalar[ih_?InertHeadQ[expr_,z___]]:=ih[NoScalar[expr],z];
NoScalar[expr:_?ProductQ[___]]:=NoScalar/@expr;
(* Derivatives *)
NoScalar[der_?FirstDerQ[expr_]]:=der[NoScalar[expr]];
(* Do nothing on other cases *)
NoScalar[expr_]:=expr;
SetNumberOfArguments[NoScalar,1];
Protect[NoScalar];


asymcoeff[n_,l_,d_]:=Gamma[l-n+d/2-1]Gamma[l+1]/((-4)^n Gamma[n+1]Gamma[l+d/2-1]Gamma[l+1-2n])


STFPart::wrongsym="Indices with different height cannot be symmetrized: `1`.";
STFPart[expr_,metric_,indices_:All]:=Module[{n,l,inds,symexpr,pairs,dummypairs,dummy,dim},
(* Indices *)
inds=Select[FindFreeIndices[expr],AIndexQ];
If[indices=!=All,inds=Intersection[inds,IndexList@@indices]];
l=Length[inds];
pairs=Partition[inds,2];
dummypairs=Map[DummyAs,pairs,{2}];
(* Dimension *)
dummy=DummyIn[VBundleOfMetric[metric]];
dim=metric[dummy,-dummy];
(* Symmetrize. Sent message, but do not throw it *)
If[Not[SameQ@@(UpIndexQ/@inds)],Message[STFPart::wrongsym,inds]];
symexpr=xAct`xTensor`Symmetrize[expr,List@@inds];
(* Remove traces *)
symexpr+Sum[asymcoeff[n,l,dim]xAct`xTensor`Symmetrize[symexpr=metric@@(pairs[[n]])metric@@(ChangeIndex/@dummypairs[[n]])ReplaceIndex[Evaluate[symexpr],Inner[Rule,pairs[[n]],dummypairs[[n]],List]] ,List@@inds],{n,1,Floor[l/2]}]
];
SetNumberOfArguments[STFPart,{2,3}];
Protect[STFPart];


(* Definition of Sym as an inert-head *)
InertHeadQ[Sym]^=True;
DefInfo[Sym]^={"inert head",""};
LinearQ[Sym]^=True;
PrintAs[Sym]^="Sym";
(* Linearity *)
Sym[x_?ConstantQ y_,other__]:=x Sym[y,other];
Sym[expr_Plus,other__]:=Sym[#,other]&/@expr;
Sym[0,other__]:=0;
(* Shortcuts *)
Sym[expr_]:=Sym[expr,FindFreeIndices[expr]];
Sym[expr_,inds_]:=Sym[expr,inds,Symmetric[Range@Length@inds]];
Sym[expr_,inds_,GS_GenSet]:=Sym[expr,inds,SchreierSims[{},GS]];


(* Temporary functions to handle spinorial metrics *)
symsign[list_List]:=Times@@(symsign1/@list);
symsign1[-x_Symbol]:=SymmetryOfIndex[x];
symsign1[x_Symbol]:=1;
(* Note that we place Sdelta after the expression. This is important for the symsign convention before *)
Sym/:ToCanonical[Sym[expr_,inds_,sym_],options___]:=With[{exprinds=Cases[FindIndices[expr],Alternatives@@inds]},
Module[{newinds=DummyAs/@exprinds,Sexpr},
Sexpr=symsign[newinds]ReplaceIndex[expr,Thread[exprinds->newinds]]Sdelta[sym][Sequence@@(ChangeIndex/@newinds),Sequence@@exprinds];
Sym[ContractMetric[RemoveSdelta@ToCanonical[Sexpr,options]],inds,sym]]];


SetNumberOfArguments[Sym,{1,3}];
Protect[Sym];


ToStripped[expr_]:=Strip[SameDummies@MathInputExpand[ToTMPTensorDerivative[expr]]];
SetNumberOfArguments[ToStripped,1];
Protect[ToStripped];


ToTMPTensorDerivative[expr_]:=expr/.dexpr:(_?CovDQ[__][_]|LieD[_][_]|ParamD[__][_]|OverDot):>(ToTensorDerivative[dexpr]/.TensorDerivative->TMPTensorDerivative);
xTensorQ[TMPTensorDerivative[exprs__]]^:=xTensorQ[TensorDerivative[exprs]];


(* Hide Plus, to avoid unexpected early canonicalization *)
Strip[expr_Plus]:=Strip/@(StrippedPlus@@expr);
(* Hide Times *)
Strip[expr_Times]:=StripProduct[Times,Strip/@List@@expr];
(* Tensors *)
Strip[tensor_?xTensorQ[inds___]]:=Stripped[Tensor,tensor,Length[{inds}]][inds];
(* Scalars *)
Strip[c_?ConstantQ]:=Stripped[Constant,c,0][];
Strip[p_Symbol?ParameterQ]:=Stripped[Parameter,p,0][];
Strip[expr_Scalar]:=Stripped[Scalar,ToCanonicalScalar[expr],0][];
Strip[sf_?ScalarFuncionQ[args__]]:=Stripped[ScalarFunction,ToCanonicalScalar/@sf[args],0][];
(* Inert heads *)
Strip[ih_?InertHeadQ[expr_,z___]]:=StripInertHead[ih,Strip[expr],z];
(* Hide other products *)
Strip[prod_?ProductQ[factors___]]:=StripProduct[prod,Strip/@{factors}];
(* Default *)
Strip[expr_]:=expr;


(* Convert products of stripped tensors into a single stripped object *)
StripProduct[prod_,factors_]:=With[{heads=Head/@factors,inds=IndexList@@@factors},
If[Union[Head/@heads]=!={Stripped},Throw[Validate::error,"Invalid expression found."]];
With[{joininds=Join@@inds},
Stripped[Product,{prod,heads},Length[joininds]]@@joininds
]
];


(* Convert an inert head expression into a tensor. We need to store the initial indices to compare heights at the end *)
StripInertHead[ih_,stripped_Stripped[inds___],z___]:=Stripped[InertHead,{ih,stripped,{inds},z},Length[{inds}]][inds];


FromStripped[sexpr_StrippedPlus]:=Plus@@(FromStripped/@sexpr);
FromStripped[Stripped[Tensor,TMPTensorDerivative[exprs__],n_][inds___]]:=FromTensorDerivative[TensorDerivative[exprs][inds]];
FromStripped[Stripped[Tensor,tensor_,n_][inds___]]:=tensor[inds];
FromStripped[Stripped[Constant,c_,0][]]:=c;
FromStripped[Stripped[Parameter,p_,0][]]:=p;
FromStripped[Stripped[Scalar,scalar_,0][]]:=scalar;
FromStripped[Stripped[ScalarFunction,scalar_,0][]]:=scalar;
FromStripped[Stripped[Product,{prod_,strippeds_List},n_][inds___]]:=
prod@@(FromStripped/@Apply@@@Transpose[{strippeds,Internal`PartitionRagged[{inds},strippeds[[All,3]]]}]);
(* Inert heads may be problematic if deltas or metrics cannot be contracted through them *)
FromStripped[Stripped[InertHead,{ih_,stripped_,initinds_List,z___},n_][inds___]]:=With[{needed=NeededChanges[{inds},DummyAs[#,Automatic]&/@initinds]},
If[And@@(ContractThroughQ[ih,#]&/@(Head/@needed)),
ih[FromStripped[stripped[inds]],z],
ContractMetric[xAct`xCoba`ContractBasis[ih[FromStripped[stripped@@initinds],z]Times@@needed]]
]
];
(* Default *)
FromStripped[expr_]:=expr;
SetNumberOfArguments[FromStripped,1];
Protect[FromStripped];


(* We are assuming (do not check) that both indices belong to the same space. Put dummies in the second index of deltas and metrics, so that their contraction is tried first *)
NeededChanges[inds_List,dummies_List]:=MapThread[NeededChange,{inds,dummies}];
NeededChange[ind1_,ind2_]:=Switch[{UpIndexQ[ind1],UpIndexQ[ind2]},
{True,True},delta[ind1,DownIndex@ind2],
{False,False},delta[ind1,UpIndex@ind2],
{True,False},FirstMetricOfVBundle[VBundleOfIndex[ind2],True][ind1,UpIndex@ind2],
{False,True},Inv[FirstMetricOfVBundle[VBundleOfIndex[ind2],True]][ind1,DownIndex@ind2],
_,Throw@Message[FromStripped::error1,"Found invalid indices:",{ind1,ind2}]
];


(*************************** 16.Computations **************************)


If[$ReadingVerbose,Print["Reading section 16: Computations."],Null,Null]


SetAttributes[Validate,HoldFirst]
Validate[expr_]:=Catch[UncatchedValidate[expr]];
SetNumberOfArguments[Validate,1];
Protect[Validate];


SetAttributes[UncatchedValidate,HoldFirst]
(* Out expressions *)
UncatchedValidate[x_Out]:=UncatchedValidate[Evaluate[x]];
(* Held expressions *)
UncatchedValidate[(Hold|HoldForm|HoldComplete|HoldPattern|xHold)[expr_]]:=UncatchedValidate[expr];
(* Times and Plus expressions. Check indices *)
UncatchedValidate[expr:(_Times|_Plus|_?ProductQ[___])]:=(FindIndices[expr];UncatchedValidate/@Unevaluated[expr]);
(* xTensor expressions. Convert to ValidateType *)
UncatchedValidate[t:_?xTensorQ[___]]:=ValidateTensor[t];
UncatchedValidate[covd:_?CovDQ[_][_]]:=ValidateCovD[covd];
UncatchedValidate[lied:LieD[_][_]]:=ValidateLieD[lied];
UncatchedValidate[br:Bracket[_,_][_]]:=ValidateBracket[br];
(* xTensor expressions. Recursion *)
UncatchedValidate[ParamD[ps__?ParameterQ][expr_]]:=ParamD[ps][UncatchedValidate[expr]];
UncatchedValidate[OverDot[expr_]]:=OverDot[UncatchedValidate[expr]];
UncatchedValidate[ih_?InertHeadQ[expr_,z___]]:=ih[UncatchedValidate[expr],z];
UncatchedValidate[Scalar[expr_]]:=Scalar[UncatchedValidate[expr]];
UncatchedValidate[sf_?ScalarFunctionQ[sc_,x___]]:=sf[UncatchedValidate[sc],x];
UncatchedValidate[Derivative[ders__Integer][f_][args__]]:=MapAt[Derivative[ders],UncatchedValidate[f[args]],0];
UncatchedValidate[c_Symbol?ConstantSymbolQ]:=c;
(* Atoms *)
UncatchedValidate[number:(_Integer|_Rational|_Real|_Complex)]:=number;
UncatchedValidate[string_String]:=string;
UncatchedValidate[x_Symbol]:=x;
(* Mathematica constructions *)
UncatchedValidate[expr:(_Slot|_Blank|_Pattern)]:=expr;
UncatchedValidate[Function[expr_]]:=Function[UncatchedValidate[expr]];
(* Other cases. Note that if h IS known the els ARE NOT validated *)
UncatchedValidate[x:h_[els___]]:=If[FreeQ[Join[$SystemNames,$xActNames],ToString[h]],
Throw[Message[Validate::unknown,"head",h]],x];
SetNumberOfArguments[UncatchedValidate,1];
Protect[UncatchedValidate];


Simplification[(action:Equal|Unequal)[x_,y_],options___]:=Module[{
fx=List@@FindFreeIndices[x],
fy=List@@FindFreeIndices[y],
sx,sy},
sx=Complement[$AbstractIndices,UpIndex/@fx];
sy=Complement[$AbstractIndices,UpIndex/@fy];
sx=ReplaceDummies[Simplification@x,IndexList@@sx];
sy=ReplaceDummies[Simplification@y,IndexList@@sy];
Which[
Sort@fx=!=Sort@fy && sx=!=0&&sy=!=0,action/.{Equal->False,Unequal->True},
True,action[sx,sy]
]
]


Protect[Simplification];


IndexSolve::free="Cannot solve for tensor with unknown free indices: `1`.";
IndexSolve[False,object_,options___]:={}
IndexSolve[True,object_,options___]:={{}}
IndexSolve[lhs_==rhs_,object_,options___]:=Module[{seq=Simplification[Expand[lhs-rhs]],frees,inds,scalar,righths,verb=OptionValue[MakeRule,{options},Verbose]},
frees=FindFreeIndices[Evaluate[seq]];
inds=Select[FindIndices[object],ABIndexQ];
If[verb,Print["frees: ",frees,", inds: ",inds]];
If[Sort@frees=!=Sort@inds,Throw[Message[IndexSolve::free,inds]]];
scalar=ToCanonical@Scalar@Coefficient[seq,ToCanonical@object];
If[verb,Print["scalar: ",scalar]];
If[!ScalarQ[scalar],Throw[Message[IndexSolve::error,""]]];
righths=Simplification[NoScalar[seq-scalar object]];
If[verb,Print["righths: ",righths]];
MakeRule[{Evaluate[object],Evaluate[-righths/scalar]},PatternIndices->List@@frees,options]//Flatten
]
SetNumberOfArguments[IndexSolve,{2,Infinity}];
Protect[IndexSolve];


(* Shortcut: With nonindexed form change to Coefficient *)
IndexCoefficient[expr_,form_?NonIndexedScalarQ]:=Coefficient[expr,form];
(* Recursion *)
IndexCoefficient[expr_,form1_ form2_]:=IndexCoefficient[Expand@IndexCoefficient[expr,form1],form2];
(* General case: Expand symmetry and average *)
IndexCoefficient[expr_,form_]:=average@SymmetryIndexCoefficient[ReplaceDummies[expr],SymmetryEquivalentsOf[form]];
SetNumberOfArguments[IndexCoefficient,2];
Protect[IndexCoefficient];

(* Secondary functions *)
average[{0...}]:=0;
average[list_List]:=With[{nonzero=DeleteCases[list,0]},Plus@@nonzero/Length[nonzero]];
(* Thread over symmetry equivalents *)
SymmetryIndexCoefficient[expr_,list_List]:=IndexCoefficient1[expr,#]&/@list;
(* Thread over Plus *)
IndexCoefficient1[expr_Plus,form_]:=IndexCoefficient1[#,form]&/@expr;
(* Products *)
IndexCoefficient1[expr_Times,form_]:=average@Map[IndexCoefficient2[expr/#,#,form]&,List@@expr];
IndexCoefficient1[expr_,form_]:=IndexCoefficient2[1,expr,form];
(* Compare expr and form *)
IndexCoefficient2[rest_,expr_,-form_]:=-IndexCoefficient2[rest,expr,form];
IndexCoefficient2[rest_,expr_,form_]:=rest IndexCoefficient3[FindIndices[expr],FindIndices[form]]/;SameExpressionsQ[expr,form];
IndexCoefficient2[rest_,expr_,form_]:=0;
(* Compute coefficient. Assume lists have same length. inds1 are the indices of expr and inds2 are the indices of form *)
IndexCoefficient3[inds1_,inds1_]:=1;
IndexCoefficient3[inds1_,inds2_]:=IndexCoefficient3[inds1,inds2,TakeEPairs[inds1],TakeEPairs[inds2]];
(* 1. There are no dummies: form pairs, taking care of repeated free indices.
   EXTENDED TO THE GENERAL CASE TO SEE WHAT HAPPENS *)
IndexCoefficient3[inds1_,inds2_,_,_]:=IndexCoefficient4[Thread[{inds1,inds2},IndexList],Intersection[inds1,inds2]];
todelta[{___,_LI,___}]:=Throw@Message[IndexCoefficient::error,"Cannot collect label indices."];
(* QUESTION: Is this OK with spinors? *)
todelta[{a_,b_?DownIndexQ}]:=delta[a,ChangeIndex[b]];
todelta[{a_,b_?UpIndexQ}]:=delta[ChangeIndex[b],a];
IndexCoefficient4[pairs_,IndexList[]]:=Apply[Times,todelta/@pairs];
IndexCoefficient4[IndexList[pairs1___,{a_,a_},pairs2___],IndexList[a_,other___]]:=IndexCoefficient4[IndexList[pairs1,pairs2],IndexList[other]];
IndexCoefficient4[_,_]:=0;
(* 2. There are dummies. ??? *)


SameExpressionsQ[tensor_[inds1___],tensor_?xTensorQ[inds2___]]:=Length[{inds1}]===Length[{inds2}];
SameExpressionsQ[covd_[_][expr1_],covd_?CovDQ[_][expr2_]]:=SameExpressionsQ[expr1,expr2];
SameExpressionsQ[ParamD[ps1__][expr1_],ParamD[ps2__][expr2_]]:={ps1}==={ps2}&&SameExpressionsQ[expr1,expr2];
SameExpressionsQ[LieD[v1_][expr1_],LieD[v2_][expr2_]]:=SameExpressionsQ[v1,v2]&&SameExpressionsQ[expr1,expr2];
SameExpressionsQ[Bracket[v1a_,v1b_][_],Bracket[v2a_,v2b_][_]]:=SameExpressionsQ[v1a,v2a]&&SameExpressionsQ[v1b,v2b];


$IndexCollectRest=Composition[ToCanonical,ContractMetric];
IndexCollect[expr_,forms_]:=IndexCollect[expr,forms,Identity];
IndexCollect[expr_,{},f_]:=f[expr];
IndexCollect[expr_,{form1_,forms___},f_]:=With[{coeff=ReplaceDummies@IndexCoefficient[expr,form1]},form1 IndexCollect[coeff,{forms},f]+f[$IndexCollectRest[expr- coeff form1]]];
IndexCollect[expr_,x_,f_]:=IndexCollect[expr,{x},f];
SetNumberOfArguments[IndexCollect,{2,3}];
Protect[IndexCollect];


TraceProductDummy[expr_]:=TraceProductDummy[expr,IndexList@@$SumVBundles];
TraceProductDummy[expr_,list_IndexList]:=Fold[TraceProductDummy,expr,list];
TraceProductDummy[expr_,vbundle_?VBundleQ]:=With[{vbundleQ=VBundleIndexQ[vbundle]},TraceDummy[expr,_?vbundleQ:>SubdummiesIn[vbundle]]];
TraceProductDummy[expr_,index_Symbol?AIndexQ]:=TraceDummy[expr,index:>SubdummiesIn[VBundleOfIndex[index]]];
SetNumberOfArguments[TraceProductDummy,{1,2}];
Protect[TraceProductDummy];


DefProductMetric::nonsum="Non-sum vbundle. Structure: `1`.";
DefProductMetric::need="Needed vbundles are: `1`.";
DefProductMetric::nometric="VBundle `1` does not have a metric.";


Options[DefProductMetric]={
PrintAs->Identity,
ProtectNewSymbol:>$ProtectNewSymbols
};
DefProductMetric[_,{},_,___]:=Message[DefProductMetric::empty,2,"pairs"];
DefProductMetric[_[-_,-_],{{_,_}},_,___]:=Message[DefProductMetric::error,"At least two pairs are needed."];
DefProductMetric[metric_,scalars_,covd_,covdsymbol:{_String,_String},options___]:=DefProductMetric[metric,scalars,covd,SymbolOfCovD->covdsymbol,options];
DefProductMetric[metric_[-ind1_,-ind2_],scalars:{{_,_}..},covd_,options:OptionsPattern[]]:=
Catch@With[{
vbundle=VBundleOfIndex[ind1],
christoffel=GiveSymbol[Christoffel,covd],
riemann=GiveSymbol[Riemann,covd],
ricci=GiveSymbol[Ricci,covd],
ricciscalar=GiveSymbol[RicciScalar,covd],
einstein=GiveSymbol[Einstein,covd],
covdsymbol=OptionValue[DefCovD,DeleteCases[{options},rule_[PrintAs,_]],SymbolOfCovD]},
Module[{subvbundles,signdet,invD,prot},

(* Generic checks *)
subvbundles=SplittingsOfVBundle[vbundle];
If [subvbundles==={} ,Throw[Message[DefProductMetric::nonsum,subvbundles]]];
subvbundles=List@@Last[subvbundles];

(* Checks on vbundles and scalars *)
If[Sort[Transpose[scalars][[1]]]=!=Sort[subvbundles],Throw@Message[DefProductMetric::need,subvbundles]];
Map[If[!MetricEndowedQ[#],Throw@Message[DefProductMetric::nometric,#]]&,subvbundles];
Map[If[!ScalarQ[#],Throw@Message[DefProductMetric::unknown,"scalar",#]]&,Transpose[scalars][[2]]];

(* Define metric *)
AppendToUnevaluated[$ProductMetrics,metric];
signdet=Times@@Map[SignDetOfMetric[FirstMetricOfVBundle[#,True]]&,subvbundles];
DefMetric[signdet,metric[-ind1,-ind2],covd,covdsymbol,options,CurvatureRelations->False];

(* Add rules *)
prot=Unprotect[metric];
With[{vb=First[#],scal=Last[#]},TagSet[metric,MetricScalar[metric,vb],scal]]&/@scalars;
Protect[Evaluate[prot]];

(* Construct rules *)
ProductMetricRules[metric,delta]={
delta[a_?AIndexQ,b_?AIndexQ]:>With[{aV=VBundleOfIndex[a],bV=VBundleOfIndex[b]},
Which[
aV===bV,delta[a,b],
aV=!=bV,0
]]};
ProductMetricRules[metric,Metric]={
metric[a_?AIndexQ,b_?AIndexQ]:>With[{aV=VBundleOfIndex[a],bV=VBundleOfIndex[b]},
With[{aVmet=FirstMetricOfVBundle[aV,True]},
Which[
aV===bV && DownIndexQ[a]&&DownIndexQ[b],MetricScalar[metric,aV]^2aVmet[a,b],
aV===bV && UpIndexQ[a]&&UpIndexQ[b],1/MetricScalar[metric,aV]^2aVmet[a,b],
aV===bV,aVmet[a,b],
aV=!=bV,0
]
]]};
ProductMetricRules[metric,Christoffel]={
christoffel[a_,b_,c_]:>With[{aV=VBundleOfIndex[a],bV=VBundleOfIndex[b],cV=VBundleOfIndex[c]},
With[{aVmet=FirstMetricOfVBundle[aV,True],bVmet=FirstMetricOfVBundle[bV,True],cVmet=FirstMetricOfVBundle[cV,True]},
Module[{chra,da,db,dc,sc=Function[MetricScalar[metric,#]]},
chra=Christoffel[CovDOfMetric@aVmet];
da=DummyIn[aV];
db=DummyIn[bV];
dc=DummyIn[cV];
Which[
aV===bV &&bV===cV,metric[a,-da]metric[b,db]metric[c,dc]chra[da,-db,-dc],
aV=!=bV&&bV===cV,-metric[b,c]metric[a,da]CovDOfMetric[aVmet][-da][sc[bV]]/sc[bV],
aV===bV&&aV=!=cV,metric[a,b]metric[c,dc]CovDOfMetric[cVmet][-dc][sc[aV]]/sc[aV],
aV===cV&&aV=!=bV,metric[a,c]metric[b,db]CovDOfMetric[bVmet][-db][sc[aV]]/sc[aV],
aV=!=bV&&bV=!=cV&&aV=!=cV,0
]
]]]/;FreeQ[VBundleOfIndex/@{a,b,c},vbundle]};
ProductMetricRules[metric,CovD]={
covd[a_][expr1_]:>Module[{aV=VBundleOfIndex[a],aVQ,cda,chra,da,dda,dummy=DummyIn[vbundle],inds=List@@FindFreeIndices[expr1]},
aVQ=VBundleIndexQ[aV];
cda=CovDOfMetric[FirstMetricOfVBundle[aV,True]];
chra=Christoffel[cda];
da=DummyIn[aV];
dda=DummyIn[aV];
metric[a,dda](cda[-dda][expr1]+
(TraceProductDummy[
Plus@@Map[christoffel[#,-dda,-dummy]ReplaceIndex[expr1,#->dummy]&,Select[inds,UpIndexQ]]]-Plus@@Map[chra[#,-dda,-da]ReplaceIndex[expr1,#->da]&,Select[inds,aVQ]])-(TraceProductDummy[Plus@@Map[christoffel[dummy,-dda,-#]ReplaceIndex[expr1,-#->-dummy]&,Select[-inds,UpIndexQ]]]-Plus@@Map[chra[da,-dda,-#]ReplaceIndex[expr1,-#->-da]&,Select[-inds,aVQ]])
-WeightOf[expr1,AIndex] expr1 (TraceProductDummy[christoffel[dummy,-dda,-dummy]-chra[da,-dda,-da]]))]/;FreeQ[{VBundleOfIndex[a]},vbundle]};
ProductMetricRules[metric,Riemann]={
riemann[a_,b_,c_,d_]:>Module[{aV=VBundleOfIndex[a],bV=VBundleOfIndex[b],cV=VBundleOfIndex[c],dV=VBundleOfIndex[d],da, db,dc,dd,dalpha,dbeta,sc=Function[MetricScalar[metric,#]],cda,cdb,cdc,cdd,riema},
da=DummyIn[aV];
db=DummyIn[bV];
dc=DummyIn[cV];
dd=DummyIn[dV];
dalpha=DummyIn[vbundle];
dbeta=DummyIn[vbundle];
cda=CovDOfMetric@FirstMetricOfVBundle[aV,True];
cdb=CovDOfMetric@FirstMetricOfVBundle[bV,True];
cdc=CovDOfMetric@FirstMetricOfVBundle[cV,True];
cdd=CovDOfMetric@FirstMetricOfVBundle[dV,True];
riema=Riemann[cda];
Which[

(* Riemann[a,b,c,d] *)
aV===bV ===cV ===dV,
metric[a,-da]metric[b,db]metric[c,dc]metric[d,dd]riema[da,-db,-dc,-dd]-(metric[a,c]metric[b,d]-metric[a,d]metric[b,c])$RiemannSign TraceProductDummy[metric[dalpha,dbeta]covd[-dalpha][sc[aV]]covd[-dbeta][sc[aV]]]/sc[aV]^2,

(* Riemann[a,b,C,D] *)
aV===bV&&cV===dV, 0,

(* Riemann[a,B,C,D] *)
bV===cV===dV,
-metric[a,da]cda[-da][sc[bV]]/sc[bV]$RiemannSign (metric[c,dc]cdc[-dc][sc[aV]]metric[b,d]-metric[d,dd]cdd[-dd][sc[aV]]metric[b,c])/sc[aV],
aV===cV===dV,-riemann[b,a,c,d],
aV===bV===dV,riemann[c,d,a,b],
aV===bV===cV,-riemann[d,c,a,b],

(* Riemann[a,B,c,D] *)
aV===cV&&bV===dV,$RiemannSign(
-metric[b,d]metric[a,da]metric[c,dc]cdc[-dc][cda[-da][sc[bV]]]/sc[bV]-metric[a,c]metric[b,db]metric[d,dd]cdb[-db][cdd[-dd][sc[aV]]]/sc[aV]-metric[a,c]metric[b,d]TraceProductDummy[metric[dalpha,dbeta]covd[-dalpha][sc[aV]]covd[-dbeta][sc[bV]]]/sc[aV]/sc[bV]),
aV===dV&&bV===cV,-riemann[a,b,d,c],

(* Riemann[a,B,c,delta] *)
aV===cV&&aV=!=bV&&aV=!=dV&&bV=!=dV,metric[a,c]metric[b,db]metric[d,dd]$RiemannSign(-cdb[-db][cdd[-dd][sc[aV]]]/sc[aV]+cdb[-db][sc[aV]]/sc[aV] cdd[-dd][sc[bV]]/sc[bV]+cdd[-dd][sc[aV]]/sc[aV]cdb[-db][sc[dV]]/sc[dV]-cdd[-dd][sc[aV]]/sc[aV] cdb[-db][sc[aV]]/sc[aV]),
aV===dV&&aV=!=bV&&aV=!=cV&&bV=!=cV,-riemann[a,b,d,c],
bV===cV&&bV=!=aV&&bV=!=dV&&aV=!=dV,-riemann[b,a,c,d],
bV===dV&&bV=!=aV&&bV=!=cV&&aV=!=cV,riemann[b,a,d,c],

(* Other cases *)
True,0]
]/;FreeQ[VBundleOfIndex/@{a,b,c,d},vbundle],

ricci[a_,b_]:>Module[{dummy=DummyIn[vbundle]},$RicciSign TraceProductDummy[riemann[dummy,a,-dummy,b]]],

ricciscalar[]:>Module[{dummy=DummyIn[vbundle]},TraceProductDummy[ricci[dummy,-dummy]]],

einstein[a_,b_]:>ricci[a,b]-ricciscalar[]/2metric[a,b]

};
]
];
DefProductMetric[metric_,{{_,_},{_,_}..},_,_,___]:=Message[DefProductMetric::invalid,metric,"metric"];
DefProductMetric[_[-_,-_],scalars_,_,_,___]:=Message[DefProductMetric::invalid,scalars,"list of pairs"];
SetNumberOfArguments[DefProductMetric,{4,Infinity}];
Attributes[ProductMetricRules]={ReadProtected};
Protect[DefProductMetric];
SetNumberOfArguments[MetricScalar,2];
Protect[MetricScalar];


ProductMetricRules[metric_]:=Union[Flatten[ProductMetricRules[metric,#]&/@{delta,Metric,Christoffel,CovD,Riemann}]];
ExpandProductMetric1[rules_][expr_]:=expr/.rules;
ExpandProductMetric[expr_,metric_Symbol?MetricQ,object___]:=CheckZeroDerivative@FixedPoint[ExpandProductMetric1[ProductMetricRules[metric,object]],expr];
ExpandProductMetric[expr_,list_List]:=Fold[ExpandProductMetric[#1,#2]&,expr,list];
ExpandProductMetric[expr_]:=ExpandProductMetric[expr,$ProductMetrics];
SetNumberOfArguments[ExpandProductMetric,{1,Infinity}];
Protect[ExpandProductMetric];
Attributes[ProductMetricRules]={ReadProtected};


ChangeFreeIndices[0,newfrees_List]:=0;
ChangeFreeIndices[expr_,newfrees_List]:=Module[{frees,dummies,newexpr},
{frees,dummies}=FindFreeAndDummyIndices[expr];
checkChangeFreeIndices[List@@frees,newfrees];
newexpr=arrangedummies[expr,List@@dummies,newfrees];
changeFreeIndices[newexpr,List@@IndexSort[frees],newfrees]
];
SetNumberOfArguments[ChangeFreeIndices,2];
Protect[ChangeFreeIndices];


metricendQ[{vbundle_,tf_}]:=If[MetricEndowedQ[vbundle],{vbundle,Null},{vbundle,tf}];


checkChangeFreeIndices[{},{}]:=Null;
checkChangeFreeIndices[frees_List,newfrees_List]:=Module[{manichars,newmanichars,ups},
(* Check1: Equal number of free indices *)
If[Length[frees]=!=Length[newfrees],Throw@Message[ChangeFreeIndices::error,"Inconsistent number of free indices."]];
(* Check2: All abstract indices *)
If[Not[AIndexQ[#]],Throw[Message[ChangeFreeIndices::invalid,#,"abstract index"]]]&/@Join[frees,newfrees];
(* Check3: Consistent vbundles and characters of free indices *)
manichars=metricendQ/@(VBundleUpQ/@frees);
newmanichars=metricendQ/@(VBundleUpQ/@newfrees);
If[Sort[manichars]=!=Sort[newmanichars],Throw@Message[ChangeFreeIndices::error,"Inconsistent vbundles or characters of free indices."]];
]


arrangedummies[expr_,dummies_List,newfrees_List]:=Module[{nodummies,newdummies},
nodummies=Intersection[dummies,UpIndex/@newfrees];
newdummies=DummyAs/@nodummies;
ReplaceIndex[expr,DuplicateRule/@Inner[Rule,nodummies,newdummies,List]]
];


changeFreeIndices[expr_,sortedfrees_List,newfrees_List]:=ReplaceIndex[expr,Inner[Rule,sortedfrees,newfrees,List]]


manicharpermQ[sortedfrees2_,frees1_]:=(metricendQ/@VBundleUpQ/@sortedfrees2)===(metricendQ/@VBundleUpQ/@frees1)


EqualExpressionsQ[expr1_,expr2_]:=Module[{frees1,frees2,dummies2,sortedfrees2,perms1,arrexpr2,newexpr2,count,result=False,perm,xxx,equation,solution},
(* Indices of expr1 *)
frees1=List@@FindFreeIndices[expr1];
(* Indices of expr2 *)
{frees2,dummies2}=FindFreeAndDummyIndices[expr2];
sortedfrees2=List@@IndexSort[frees2];
dummies2=List@@dummies2;
checkChangeFreeIndices[frees1,sortedfrees2];
(* All permutations of the free indices of expr1 *)
perms1=Permutations[List@@frees1];
(* Remove collisions between frees1 and dummies2 *)
arrexpr2=arrangedummies[expr2,dummies2,frees1];
DefConstantSymbol[xxx,DefInfo->False];
Do[
newexpr2=changeFreeIndices[arrexpr2,sortedfrees2,perms1[[count]]];
equation=ToCanonical[expr1-xxx newexpr2];
solution=First[xxx/.Solve[equation==0,xxx]];
If[ConstantQ[solution],result=True;perm=Inner[Rule,perms1[[count]],sortedfrees2,List];Break[]],
{count,Length[perms1]}];
If[result,Print["LEFT  == ",Switch[solution,1,"",-1,"-",_,solution]," RIGHT   with rules: ",perm]];
result
];
SetNumberOfArguments[EqualExpressionsQ,2];
Protect[EqualExpressionsQ];


ListableQ[f:(_Symbol|_String)]:=MemberQ[Attributes[f],Listable];
ListableQ[_]:=False;
Unprotect[Equal];
Equal/:Plus[Equal[a_,b_],Equal[c_,d_]]:=Equal[Plus[a,c],Plus[b,d]];
Equal/:Times[Equal[a_,b_],Equal[c_,d_]]:=Equal[Times[a,c],Times[b,d]];
Equal/:Power[Equal[a_,b_],Equal[c_,d_]]:=Equal[Power[a,c],Power[b,d]];
Equal/:f_?ListableQ[l___,eq_Equal,r___]:=f[l,#,r]&/@eq;
Protect[Equal];


(**************************** 17.Evaluation ***************************)


If[$ReadingVerbose,Print["Reading section 17: ExpressionManipulation."],Null,Null]


ColorPositionsOfPattern[pattern_,options___][expr_]:=xAct`ExpressionManipulation`ColorPositions[{Position[expr,pattern]},options][expr];
ColorTerms[expr_]:=xAct`ExpressionManipulation`ColorPositions[Partition[Range@Length@expr,1]][expr];
SetNumberOfArguments[ColorTerms,1];
Protect[ColorTerms,ColorPositionsOfPattern];


End[]


EndPackage[]
