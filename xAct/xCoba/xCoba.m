(************************ 0. Info and copyright ***********************)


xAct`xCoba`$Version={"0.8.4",{2018,2,28}}


xAct`xCoba`$xTensorVersionExpected={"1.1.3",{2018,2,28}}


(* xCoba, a free package for tensor component computations in Mathematica *)

(* Copyright (C) 2005-2018 David Yllanes and Jose M. Martin-Garcia *)

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


(* :Title: xCoba *)

(* :Author: David Yllanes and Jose M. Martin-Garcia *)

(* :Summary: Free package for tensor component computations *)

(* :Brief Discussion:
     - Component computations based on the abstract tools provided by xTensor.
     - Basis indices are denoted by {a, basis} or {-a, -basis}.
     - Component indices are denoted by {1, basis} or {1, -basis}.
     - The link between the abstract and the component world is given by
       Basis[a, {-b, -basis}], the basis vectors.
     - There are functions to manipulate Basis objects and the associated
       component scalars.
     - There are functions to store values for components.
     - There is limited support to work with charts.
*)
  
(* :Context: xAct`xCoba` *)

(* :Package Version: 0.8.4 *)

(* :Copyright: David Yllanes and Jose M. Martin-Garcia (2005-2018) *)

(* :History: see xCoba.History file *)

(* :Keywords: *)

(* :Source: xCoba.nb *)

(* :Warning: Still experimental! *)

(* :Mathematica Version: 6.0 and later *)

(* :Limitations: Not few *)


(************************ 1. Begin package ***********************)


With[{xAct`xCoba`Private`xCobaSymbols=DeleteCases[Join[Names["xAct`xCoba`*"],Names["xAct`xCoba`Private`*"]],"$Version"|"xAct`xCoba`$Version"|"$xTensorVersionExpected"|"xAct`xCoba`$xTensorVersionExpected"]},
Unprotect/@xAct`xCoba`Private`xCobaSymbols;
Clear/@xAct`xCoba`Private`xCobaSymbols;
]


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`xCoba`"];


BeginPackage["xAct`xCoba`",{"xAct`xTensor`","xAct`xPerm`","xAct`xCore`","xAct`ExpressionManipulation`"}]


If[Not@OrderedQ@Map[Last,{$xTensorVersionExpected,xAct`xTensor`$Version}],Message[General::versions,"xTensor",xAct`xTensor`$Version,$xTensorVersionExpected];
Abort[]];


Print[xAct`xCore`Private`bars];
Print["Package xAct`xCoba`  version ",xAct`xCoba`$Version[[1]],", ",xAct`xCoba`$Version[[2]]];
Print["CopyRight (C) 2005-2018, David Yllanes and Jose M. Martin-Garcia, under the General Public License."];


Off[General::shdw];
xAct`xCoba`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."];
On[General::shdw];


If[xAct`xCore`Private`$LastPackage==="xAct`xCoba`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]];


(* Definition *)
DefBasis::usage="DefBasis[basis, vbundle, {c1, ..., cn}] defines a basis of vector fields with name basis on the given vbundle. Its dual -basis is also implicitly defined. The list of cnumbers {c1, ..., cn} denotes the integers identifying the element of the basis, and hence must have length equal to the dimension of the vbundle.";
UndefBasis::usage="UndefBasis[basis] undefines the basis.";
$PDPrefixSymbol::usage="$PDPrefixSymbol is a global variable giving the character used to represent the parallel derivatives in StandardForm in Prefix notation. The default value is \[ScriptCapitalD] (the capital script D).";
$PDPostfixSymbol::usage="$PDPostfixSymbol is a global variable giving the character used to represent the parallel derivatives in StandardForm in Postfix notation. The default value is \[Cedilla] (a cedilla).";
BasisColor::usage="BasisColor[basis] gives the color with which objects and indices associated to the given basis will be represented in StandardForm.\n\nBasisColor is also an option for DefBasis giving the color associated to the defined basis. The default value is red (RGBColor[1, 0, 0]).";
FormatBasis::usage="FormatBasis[{i, -basis}, string] formats the basis vector Basis[{i, -basis}, a] as string[a], where i is one of the cnumbers compatible with basis. FormatBasis[{i, basis}, string] formats the basis covector Basis[-a, {i, basis}] as string[-a]. FormatBasis[CD[{i, -basis}], string] formats the given directional derivative as string. FormatBasis acting on only the first arguments of those cases removes the corresponding formatings.\n\nFormatBasis is also an option for DefBasis and DefChart specifying two lists of strings to format respectively the vectors and the covectors of the basis. For DefChart it is also possible to use \"Partials\" for vectors and/or \"Differentials\" for the covectors. The default value is Automatic.";
epsilonOrientationOfMetric::usage="epsilonOrientationOfMetric is an option of DefBasis specifying a pair {metric, o} where o is the factor between the epsilon of the given metric and the eta tensors of the basis being defined. By default it takes the value {Null, 1}.";
etaOrientation::usage="etaOrientation[basis] gives the orientation sign of the eta tensors of basis. It is computed as the Signature of the list of cnumbers of the basis at definition time.";
BasisChange::usage="BasisChange is an option of DefBasis specifying a 2-CTensor change from a previously defined basis. The CTensor object must use both bases, one covariant and the other contravariant. The default value is Null, meaning that the basis is defined without reference to any other basis. It is also possible to give a pair of CTensor objects, representing the direct and inverse basis changes.";
SetBasisChange::usage="SetBasisChange[CTensor[matrix, {-downbasis, upbasis}]] stores, both using a CTensor and TensorValues, the change between the bases. Input can also be given as SetBasisChange[CTensor[Transpose[matrix], {upbasis, -downbasis}]]. SetBasisChange[{direct, inverse}] provides a pair of CTensor objects describing the direct and inverse basis changes.";


(* Registering *)
VBundleOfBasis::usage="VBundleOfBasis[basis] gives the vbundle on which basis lives.";
BasesOfVBundle::usage="BasesOfVBundle[vb] gives the lists of bases registered on the vbunle vb.";
PDOfBasis::usage="PDOfBasis[basis] gives the parallel derivative associated to the given basis. If the basis is coordinated then (and only then) the derivative has no torsion and it can be called a partial derivative (ordinary derivative in Wald's book).";
BasisOfCovD::usage="BasisOfCovD[covd] for a basis-parallel derivative covd returns the basis to which covd is associated. If covd is not registered as parallel to a basis then it returns Null. ";
DependenciesOfBasis::usage="DependenciesOf[basis] gives the list of dependencies (manifolds and/or parameters) of basis.";
CNumbersOf::usage="CNumbersOf[basis, svb] returns the sublist of integer cnumbers associated to basis on the subvbundle svb. CNumbersOf[basis] is converted into CNumbersOf[basis, VBundleOfBasis[basis]].";
DaggerCIndex::usage="DaggerCIndex[basis, cindex] returns the conjugated index to the cindex (assumed to belong to basis or -basis), in general a Dir expression. This function should be handled through upvalues for basis.";
SetDaggerMatrix::usage="SetDaggerMatrix[basis, matrix] defines upvalues for DaggerCIndex of all c-indices of basis, its dual and their conjugates, based on the given conjugation matrix. This matrix gives the dagger of the basis up-vectors in terms of the vectors of its conjugate basis. When the basis is real the matrix must be unitary.";


(* Densities and Jacobians *)
etaUp::usage="etaUp is a reserved word in xCoba, used to denote the antisymmetric part of the product of vectors of a basis. It is a tensor density of weight +1 in basis.";
etaDown::usage="etaDown is a reserved word in xCoba, used to construct the antisymmetric part of the product of covectors of a basis. It is a tensor density of weight -1 in basis.";
etaUpToepsilon::usage="etaUpToepsilon[basis, metric] gives a rule transforming the etaUp tensor of basis into the epsilon tensor of metric.";
etaDownToepsilon::usage="etaDownToepsilon[basis, metric] gives a rule transforming the etaDown tensor of basis into the epsilon tensor of metric.";
epsilonToetaUp::usage="epsilonToetaUp[metric, basis] gives a rule transforming the epsilon tensor of metric into the etaUp tensor of basis.";
epsilonToetaDown::usage="epsilonToetaDown[metric, basis] gives a rule transforming the epsilon tensor of metric into the etaDown tensor of basis.";
(* Overload Jacobian *)
Jacobian::usage=Jacobian::usage<>"\n\nJacobian[b1, b2][] returns the Jacobian of the transformation between bases b1 and b2. If b1 and b2 are not sorted lexicographically, then it returns 1/Jacobian[b2, b1][].";


(* Values for the metric in a basis *)
MetricInBasis::usage="MetricInBasis[metric, -basis, matrix] stores the values in matrix for the covariant components of metric in the given basis. MetricInBasis[metric, -basis, diagonal] stores the given values for the diagonal list of components, and zero everywhere else. MetricInBasis[metric, -basis, \"Orthonormal\"] takes diagonal values from SignatureOfMetric[metric] and zero everywhere else. MetricInBasis[metric, -basis, \"Orthogonal\"] stores zero off-diagonal values, but does not define values on the diagonal. MetricInBasis[metric, basis, values] stores values for the contravariant components of the inverse metric, where values is any of the previous possibilities. Finally, MetricInBasis[metric, basis, expr] computes the components from the abstract expression expr, using the given basis. In this case we check that the character of basis coincides with that of the two indices of expr.\n\nMetricInBasis is also an option for DefBasis and DefChart specifying that information at definition time, that is MetricInBasis->{metric, -basis, matrix}. Its default value is {}.";


(* Contraction and separation of bases *)
ContractBasis::usage="ContractBasis[expr, indices] removes Basis objects from expr. The second argument identifies which Basis objects must be removed, and which indices must be eliminated. The possibilities are: IndicesOf[selectors], a list of g-indices with head IndexList or a single g-index. It is also possible to use the name of a vbundle or a basis as shortcuts for IndicesOf[vbundle] or IndicesOf[basis], respectively. See the documentation for IndicesOf for a complete list of possible selectors. See also the option OverDerivatives.";
SeparateBasis::usage="SeparateBasis[basis][expr, indices] expands indices of objects in expr into products of Basis objects and the original objects, using contractions of indices specified by basis. When basis is AIndex or is not given, abstract indices are used. The list of indices can be supplied in different ways, using IndicesOf[selectors], a list of g-indices with head IndexList or a single g-index. See the documentation for IndicesOf for a complete list of possible selectors.";
OverDerivatives::usage="OverDerivatives is a Boolean option for ContractBasis specifying whether Basis objects must be contracted or not with objects inside derivatives. Its default value is False.";
FreeToBasis::usage="FreeToBasis[basis][expr, finds] converts the given free indices finds (either abstract- or basis-) into free indices of the given basis. finds can be an IndicesOf[selectors] expression, or a list with head IndexList or a single g-index.";
DummyToBasis::usage="DummyToBasis[basis][expr, dinds] converts the given dummy indices dinds (either abstract- or basis-) into dummy indices of the given basis. dinds can be an IndicesOf[selectors] expression, or a list with head IndexList or a single g-index.";
ToBasis::usage="ToBasis[basis][expr, inds] converts the given indices inds into the given basis. It first uses the function DummyToBasis with the same arguments and then FreeToBasis on the result. inds can be an IndicesOf[selectors] expression, or a list with head IndexList or a single g-index.";
AutomaticBasisContractionStart::usage="AutomaticBasisContractionStart[] turns on the automatic conversion of contracted products of two Basis objects into a single one, both if the contracted index is of abstract or basis type.";
AutomaticBasisContractionStop::usage="AutomaticBasisContractionStop[] turns off the automatic conversion of contracted products of two Basis objects into a single one.";


(* Ricci rotation coefficients *)
RicciRotation::usage="RicciRotation[covd, basis] gives the Christoffel tensor of covd from the parallel derivative of basis.";


SplitBasis::usage="SplitBasis[basis, {subv1->cnumbers1, subvb2->cnumbers2, ...}] declares how the given basis of a vbundle must be splitted among the subvbundles of that vbundle.";


(* Bases and mappings *)
PrecomposeBasis::usage="Precompose[basis, phi] represents the precomposition with the mapping phi of the (vectors of the) given basis.";


(* BasisArray and BasisCollect *)
BasisArray::usage="BasisArray[basis1, basis2, ...][i1, i2, ...] generates the array of Basis objects with a-indices i1, i2, ... and the c-indices corresponding to the respective bases. The depth of the array is the number of a-indices. If all bases coincide then it is possible to write a single basis.";
BasisCollect::usage="BasisCollect[expr] collects all terms corresponding to the same component multiplying the same free basis vectors. BasisCollect[expr, f] maps the function f to those coefficients.";


(* Tracing dummies *)
TraceBasisDummy::usage="TraceBasisDummy[expr, inds] expands the basis-dummies inds in expr to the corresponding ranges of c-numbers. inds can be a single b-index, an IndexList list of indices or an IndicesOf specfification of selectors. The name of a basis or a vbundle are also accepted. TraceBasisDummy[expr] expands all basis-dummies. TraceBasisDummy[expr, inds1, ..., inds2, ...] sequentially expands the indices of the different specifications.";
BasisExpand::usage="BasisExpand[expr, basis] converts the abstract-indexed expression expr into a linear combination of components multiplied by basis elements of the given basis.";

(* Component array generation *)
ComponentArray::usage="ComponentArray[expr, bcinds] returns an array of components of expr obtained by expansion of the free b-indices in the list bcinds present in expr. bcinds can be an expression with head IndicesOf or a list of indices with head IndexList or a g-index. ComponentArray[expr] is equivalent to ComponentArray[expr, IndicesOf[Free,BIndex]].";
TableOfComponents::usage="TableOfComponents[expr, basis] returns a multidimensional table of all components (corresponding to the free-indices) of expr. TableOfComponents[expr, {A, basis}] gives the list of components of expr corresponding to the abstract index A expanded in basis or to the already present b-index {A, basis}. TableOfComponents[expr, a1, a2, ...] applies the options ai (either a basis or {A, basis}) sequentially.";

(* Threading over arrays *)
ThreadComponentArray::usage="ThreadComponentArray[arrayL, arrayR, f] generates an array of expressions f[compL, compR] where compL and compR are corresponding elements of the arrays arrayL and arrayL of same dimensions and depth. The default value of f is Rule. If arrayL or arrayR are not arrays then ComponentArray is mapped on them first.";


(* Explicit representation of values *)
CTensor::usage="CTensor[A, {B1, B2, ...}, w] represents an abstract tensor constructed from the array A of components in the bases Bi and with added density weight w.";
ColorBasisForm::usage="ColorBasisForm[array, colors] represents the array as MatrixForm would do, but coloring the lines around the array as given by the colors list.";
$CTensorRepresentation::usage="$ColorCTensor is a global variable that determines how the CTensor objects are represented. The default value is \"ColorBasisForm\", that typesets the component arrays using colored frames with the colors of the respective bases. The alternative is \"MatrixForm\", which uses MatrixForm to typeset arrays.";
$LargeComponentSize::usage="$LargeComponentSize is a global variable containing the maximum ByteCount size of an expression that can be typeset as a component of a CTensor object. For larger expressions the string $LargeComponentString is shown. The default value is 1000.";
$LargeComponentString::usage="$LargeComponentString is a global variable containing the string to be displayed in a CTensor object instead of the large components (those whose ByteCount is beyond $LargeComponentSize). The default value is a grey circle \[GrayCircle]. A tooltip shows its actual contents.";
ToCTensor::usage="ToCTensor[T, bases] returns a CTensor expression equivalent to the tensor T whose first argument is an array of components of T in the given bases. This can be used to perform changes of bases in a CTensor expression. It also performs contractions with the first-metric if the up/down character of the slots is different. ToCTensor[T, bases, w] adds w to the weight of the resulting CTensor object.";
ToCTensorOneSlot::usage="ToCTensorOneSlot[T, {n, change}] returns the result of changing the n-th slot of the CTensor object T multiplying with the second slot of the CTensor change.";
ToCCanonical::usage="ToCCanonical[expr] sorts the indices of the CTensor objects, transposing their arrays of components and permuting their lists of bases such that the tensor remains the same.";
$AutomaticCTensorMetricChange::usage="$AutomaticCTensorMetricChange is a global variable stating whether CTensor objects will automatically get contracted with the first-metric when their indices are raised and lowered.";
ToBasisExpand::usage="ToBasisExpand[expr] converts the CTensor objects in the expression expr into linear combinations of basis objects.";
FromBasisExpand::usage="FromBasisExpand[expr] converts Basis objects into CTensor objects.";

(* Explicit representation of covariant derivatives *)
CCovD::usage="CCovD[cd, chr] represents an abstract covariant derivative obtained by changing the covariant derivative cd as given by the Christoffel tensor chr. CCovD[cd, chr, metric] represents a connection deriving from the metric. When the covariant derivative is not associated to a metric field the third argument is automatically chosen to be Null.";
ToCCovD::usage="ToCCovD[ccovd, pd] changes the CCovD covariante derivative ccovd to use the derivative pd in its first argument, modifying its Christoffel tensor as required. ToCCovD[ccovd, pd, bases] also changes the bases of the new Christoffel Tensor. ToCCovD[ccovd, pd, chrbases, metbases] changes the bases of both the Christoffel tensor and the associated metric tensor. ";

(* Explicit representation of bases *)
CBasis::usage="CBasis[B, M] represents the new basis obtained by transforming the basis B using the matrix M. This is an experimental feature, only partially implemented.";


(* Duality *)
Dual::usage="Dual[cmatrix] computes the dual (inverse-transpose) of the CTensor matrix cmatrix.";


(* xCoba cache *)
xCobaCache::usage="xCobaCache[lhs, rhs] checks whether there is a value in the xCobaCacheTable assigned to lhs, and returns it if there is. Otherwise the value rhs is stored in the cache, assigned to lhs, and returned. xCobaCache[lhs, rhs, name] stores the pair {lhs, rhs} with the given name, with default being the head of the expresion lhs. xCoba[lhs, rhs, name, simp] uses the simplification function simp on the rhs expression before storing it in the cache table.";
xCobaCacheTable::usage="xCobaCacheTable[lhs, name] returns the expression associated to the key lhs and the given name, if it has been cached. Otherwise it stays unevaluated.";
$xCobaCache::usage="$xCobaCache is a boolean global variable specifying whether CTensor computations in xCoba should be cached. The default value is True.";
$xCobaCacheVerbose::usage="$xCobaCacheVerbose is a boolean global variable specifying whether caching and use of cache should be reported. The default value is False.";
ClearxCobaCache::usage="ClearxCobaCache[All] clears the cache table for CTensor computations. ClearxCobaCache[{pos}] or ClearxCobaCache[{{pos1}, {pos2}, ...}] removes the values at positions posi (corresponding to the list of downvalues of xCobaCacheTable. ClearxCobaCache accepts input with Delete conventions.";
PrintxCobaCache::usage="PrintxCobaCache[] gives a tabular summary of the information stored in the xCobaCache table. The column on the left corresponds to the positions in the list of downvalues of xCobaCacheTable, and can be used to eliminate entries from the table using ClearxCobaCache[{pos}].";


(* Handling list of values *)
TensorValues::usage="TensorValues[tensor, ders, {bases1, bases2, ...}] returns a FoldedRule structure with all components of tensor (or the ders derivatives of tensor) which have been computed until now with the configuration of indices specified by the symmetry-equivalent configurations bases1, bases2, ...";
ValID::usage="ValID[tensor, {{basis1, basis2, ...}}] identifies a set of values for the tensor with c-indices respectively in basis1, basis2, ... (with sign indicating character as usual; LI and -LI are accepted). For a tensor with symmetry, ValID[tensor, {bases1, bases2, ...}] identifies a set of values related by that symmetry, where each of the basesi is a list of possible reorderings of the bases of the c-indices. ValID[tensor, der1, der2, ..., bases] identifies a set of values for the given derivatives of the tensor in Postfix order (in Prefix notation it would mean ...[der2[der1[tensor]]), where bases is a list of lists as before.";
TensorValIDs::usage="TensorValIDs[tensor] gives the list ValID expressions of the sets of values for which at least one component has been already computed.";
BasisValues::usage="BasisValues[basis1, -basis2] and BasisValues[-basis2, basis1] give the list of component values for the matrix of change between those two bases. There is no automatic computation of the inverse matrix.";
DateOfValID::usage="DateOfValID[valID] gives the date (format of Date[]) of the last modification of the values of the given valID.";
DeleteTensorValues::usage="DeleteTensorValues[tensor, ders, bases] removes the stored rules TensorValues[tensor, ders, bases]. DeleteTensorValues[tensor, ders] removes all such rules for tensor, for all bases configurations in TensorValIDs[tensor, ders].";


(* Storing values *)
ComponentValue::usage="ComponentValue[expr], for a component tensor expression (a tensor or a derivative of a tensor), returns a value rule of the form expr -> canon where canon is the canonical form of expr under ToCanonical. This value rule is automatically stored in the TensorValues list of that tensor so that it can be used from that list without having to recompute it. ComponentValue[expr, value] returns a value rule expr -> value, and stores internally both the dependent rule expr -> canon and the independent rule canon -> value.";
HeldComponentValue::usage="HeldComponentValue[expr] and HeldComponentValue[expr, value] perform the same tasks as ComponentValue, but not letting expr evaluate.";
AllComponentValues::usage="AllComponentValues[expr] returns an array of all value-rules of all possible symmetry-equivalent components of ComponentArray[expr].";
$CVVerbose::usage="$CVVerbose is a Boolean global variable turning on/off verbose messages from ComponentValue. Its default value is True.";
$CVReplace::usage="$CVReplace is a Boolean global variable specifying whether stored values must be replaced by new different values or not (a message would be sent then). Its default value is True.";
$CheckZeroValue::usage="$CheckZeroValue is a Boolean global variable. If True, ComponentValue will check values for those components which are zero by symmetry, and will complain if they are not zero. The default is False.";
$CVSimplify::usage="$CVSimplify is a global variable specifying a simplification function to be used by many xCoba functions. The default is Simplify.";
RuleToSet::usage="RuleToSet[valID] converts the stored rules of valID into static definitions, effectively replacing Rule by Set. RuleToSet[T, d1, ..., dn, blist] applies RuleToSet on the known valIDs for the given tensor T, first-derivatives d1, ..., dn and list of bases blist. It is also possible to use the syntax RuleToSet[T, d1, ..., dn] or even RuleToSet[T], which applies RuleToSet on all known valIDs for the given input.";
SetToRule::usage="SetToRule[valID] transforms back the static definitions of valID into rules, effectively replacing Set by Rule. SetToRule[T, d1, ..., dn, blist] applies SetToRule on the known valIDs for the given tensor T, first-derivatives d1, ..., dn and list of bases blist. It is also possible to use the syntax SetToRule[T, d1, ..., dn] or even SetToRule[T], which applies SetToRule on all known valIDs for the given input.";
ToTensorRules::usage="ToTensorRules[tensor, ctensor] constructs and stores the rules associated to the assigment of the ctensor to the tensor. This is basically a call to ComponentValue having the ComponentArray of tensor in the first argument and ctensor in the second argument.";


(* Changing components *)
ChangeComponents::usage="ChangeComponents[expr1, expr2] computes and stores values for the components of expr1 starting from expr2. Those expressions are assumed to be related by metric and Basis factors only.";
$TUseValues::usage="$TUseValues is a global variable specifying which rules must be used internally by ComponentValue for the tensor whose components are being changed. Possible values are None, All and ToCanonical. The latter means that only dependent (symmetry) rules will be used, and is the default.";
$BMUseValues::usage="$BMUseValues is a global variable specifying which rules for the metric or the Basis objects must be used internally by ComponentValue. Possible values are None, All and ToCanonical. The latter means that only dependent (symmetry) rules will be used, and is the default.";
$CCSimplify::usage="$CCSimplify is a global variable storing a function to be used after each step of ChangeComponents. The default is Identity.";


(* Using values *)
ToValues::usage=
"ToValues[expr] returns expr after replacing all known tensor-values for the tensors in the expression. ToValues[expr, tensor] replaces only values for the given tensor. ToValues[expr, list] replaces values for the tensors in the list. ToValues[expr, list, f] applies the function f after replacement of each of the tensors in the list.";
WithSetValues::usage="WithSetValues[expr, tensors] computes expr temporarily transforming to Set all Rule values for the given list of tensors. WithSetValues[expr] transforms to Set state temporarily all tensors present in expr.";


(* The Reals[dim] manifold *)
TangentReals::usage="TangentReals[dim] represents the tangent vbundle to the manifold Reals[dim].";


(* Charts *)
DefChart::usage="DefChart[chart, manifold, cnumbers, scalars] defines chart as a coordinate system on manifold, with coordinate fields given by the list scalars and the integer cnumbers identifying those coordinates and the associated vectors/covectors fields. ";
UndefChart::usage="UndefChart[chart] undefines the chart, making it unusable.";
ChartColor::usage="ChartColor[chart] returns the color used to represent the given chart in StandardForm. It equals BasisColor[chart].\n\nChartColor is also an option for DefChart specifying that color at definition time.";
ExtendedCoordinateDerivatives::usage="ExtendedCoordinateDerivatives is a Boolean option for DefChart specifying that, if set to True, all covariant derivatives acting on the coordinate scalars are converted automatically into Basis objects. If set to False then only the parallel derivative of the declared chart acting on coordinate scalars will be converted. Note that this is decided at declaration time, and cannot be altered afterwards.";
$IdentityScalarSymbol::usage="$IdentityScalarSymbol is a global variable containing the symbol used to denote the natural scalar fields of the IdentityMapping[Reals[dim]] chart. The default value is the double-struck x. Hence those scalars are denoted as \[DoubleStruckX][dim, i][], where i is a positive integer between 1 and dim.";
\[DoubleStruckX]::usage="\[DoubleStruckX] is the symbol used by default to denote the scalar fields of Reals[dim]. They are actually denoted as \[DoubleStruckX][dim, i][] with the integer i taking values from 1 to dim. The default symbol can be changed modifying the value of the variable $IdentityScalarSymbol.";
ChartOfScalar::usage="ChartOfScalar[x[]] returns the chart to which the coordinate scalar x[] belongs, or Null otherwise.";
ScalarsOfChart::usage="ScalarsOfChart[chart] gives the list of coordinate scalar fields of chart.";
ManifoldOfChart::usage="ManifoldOfChart[chart] gives the manifold on which the given chart has been defined.";
ChartsOfManifold::usage="ChartsOfManifold[manifold] gives the charts already defined on the manifold.";
Coordinate::usage="Coordinate[i, chart] returns the name of the coordinate field in the given chart identified by the cnumber i.";
InChart::usage="InChart[expr, chart][x1, x2, ...] is a scalar-function with arguments x1, x2, ... representing the tensorial expression expr in the given chart. That means that the arguments x1, x2, ... are taken to be values for the respective coordinate fields of the chart. InChart[_,_] is recognized as a scalar function by ScalarFunctionQ.";
ToInChart::usage="ToInChart[chart][expr] converts scalar fields in expr into their corresponding InChart scalar functions. ToInChart[chart][expr] is converted into InChart[expr, chart][coords], where coords are the coordinate fields of the chart.";
ChangeChart::usage="ChangeChart[expr, chart] changes the explicit InChart expressions in expr to the given chart, changing also their arguments so that the final function is equivalent to the initial one (passive change of coordinates).";
CMapping::usage="CMapping[domc[{x1,...}], imc[{y1,...}]] represents the mapping from a point in domain chart domc to a point in the image chart imc.";
Transition::usage="Transition[chart1, ichart2][{x1, x2, ...}] computes the list chart1[ichart2[{x1, x2, ...}]].";
CurveTangent::usage="CurveTangent[curve, t] gives the tangent field tangent to the curve, parametrized by t.";
VectorD::usage="VectorD[s, At[v, P]] computes the directional derivative of the scalar field in the direction of the vector v at the point P.";


(* Computations from a metric in a coordinated frame *)
CVSimplify::usage="CVSimplify is an option of various component functions which allows to insert a function in some key places. Standard choices are Together or Simplify.";
MetricCompute::usage="MetricCompute[g, ch, T] computes the components of the curvature tensor T associated to the metric g in the chart ch, where g and ch are symbols already known to xTensor and xCoba, respectively. The metric g is assummed to have been assigned values as explicit functions of the coordinate scalars. The notation for T is special and currently allows the 15 possibilities: \"Metric\"[-1, -1], \"Metric\"[1, 1], \"DetMetric\"[], \"DMetric\"[-1, -1, -1], \"DDMetric\"[-1, -1, -1, -1], \"Christoffel\"[-1, -1, -1], \"Christoffel\"[1, -1, -1], \"Riemann\"[-1, -1, -1, -1], \"Riemann\"[-1, -1, -1, 1], \"Riemann\"[-1, -1, 1, 1], \"Ricci\"[-1, -1], \"RicciScalar\"[], \"Weyl\"[-1, -1, -1, -1], \"Einstein\"[-1, -1], \"Kretschmann\"[], where -1 denotes a covariant component and 1 a contravariant component. This function computes in advance everything needed to know the required tensor T. It is possible to say All instead of a tensor T, and then those 14 tensors will be computed. There are options CVSimplify, to specify a function which is applied to each component after each tensor is computed (default is Together), and Verbose, to get info messages during the computation (default is True).";
SetCMetric::usage="SetCMetric[met, ch] sets met, a CTensor[matrix, {-basis, -basis}] object, as metric for the vector bundle of the given basis. It is assumed to be a field on the coordinate chart ch. Some auxiliary objects, like metric derivatives and Christoffel values are precomputed. SetCMetric[met, tor, ch] adds the torsion tensor tor to the covariant derivative associated to the metric met. An option SignatureOfMetric -> {np, nn, nz} explicitly specifies the signature of the metric.";
UnsetCMetric::usage="UnsetCMetric[met] unsets the metric met, a CTensor object.";


(* Older functionality *)
InverseMetric::usage="InverseMetric[g[-a, -b], B][c, d] returns the inverse of the metric g[-a, -b], expressed as an abstract tensor with upper indices c, d. The result is computed using the components of the metric g in the basis B.";
ChristoffelFromMetric::usage="ChristoffelFromMetric[g[-a, -b], B][c, -d, -e] returns the Christoffel tensor of the LeviCivita connection of the metric g[-a, -b] from the parallel derivative of the basis B, expressed as an abstract tensor with indices c, -d, -e.";
RiemannFromChristoffel::usage="RiemannFromChristoffel[chr[a, -b, -c], B][-d, -e, -f, h] returns the Riemann tensor of the LeviCivita connection connected by the Christtofel tensor chr[a, -b, -c] from the parallel derivative of the basis B, expressed as an abstract tensor with indices -d, -e, -f, h.";


Begin["`Private`"]


$xCobaNames=Names["xAct`xCoba`*"]


(************************* 2. Bases ************************)


$PDPrefixSymbol="\[ScriptCapitalD]";
$PDPostfixSymbol="\[Cedilla]";


Options[DefBasis]={
BasisColor->RGBColor[1,0,0],
FormatBasis->Automatic,
ProtectNewSymbol:>$ProtectNewSymbols,
Dagger->Real,
ExtendedFrom->Null,
MetricInBasis->{},
BasisChange->Null,
epsilonOrientationOfMetric->{Null,1},
DependenciesOfBasis->{},
DefInfo->{"basis",""},
Master->Null
};


(* Main *)
DefBasis[basisname_,vbundle_,cnumbers_,options:OptionsPattern[]]:=Module[{bc,fb,pns,dag,ef,mb,change,eoom,deps,mb3,dim,tanb,covdef,info,master,manifold=BaseOfVBundle[vbundle],tbundle,oldbasis,cbases},
{bc,fb,pns,dag,ef,mb,change,eoom,deps,info,master}=OptionValue[{BasisColor,FormatBasis,ProtectNewSymbol,Dagger,ExtendedFrom,MetricInBasis,BasisChange,epsilonOrientationOfMetric,DependenciesOfBasis,DefInfo,Master}];

(**** Checks ****)
(* Check names *)
If[!VBundleQ[vbundle],Throw@Message[DefBasis::unknown,"vbundle",vbundle]];
dim=DimOfVBundle[vbundle];
If[ChartQ[basisname],
If[BasisQ[basisname],
(* The basis already exists. This is a duplicated name *)
Throw@Message[ValidateSymbol::used,basisname,"as a basis"],
(* Coordinate basis being defined. Symbol validation already performed. Do nothing *)
Null
],
(* Noncoordinate basis. Perform symbol validation *)
ValidateSymbol[basisname];
ValidateSymbolInSession[basisname]
];
(* Check cnumbers: heads, length and integer type. Repeated cnumbers are not allowed *)
If[Or[
Head[cnumbers]=!=List,
Union[cnumbers]=!=Sort[cnumbers],
Not[And@@(IntegerQ/@cnumbers)],
Length[cnumbers]=!=dim],
Throw@Message[DefBasis::invalid,cnumbers,"list of cnumbers"]];
(* Check complex option *)
If[FreeQ[{Real,Complex,Conjugate},dag],Throw@Message[DefBasis::invalid,dag,"value for option Dagger"]];
If[DaggerQ[vbundle]&&dag===Real,Throw@Message[DefBasis::error,"Cannot define a real basis on a complex manifold."]];
(* Check dependencies *)
If[Not@Or[ManifoldQ[#],ParameterQ[#]],Throw@Message[DefBasis::invalid,#,"dependency"]]&/@deps;
If[FreeQ[deps,manifold],deps=Prepend[deps,manifold]];
(* Check extension from other basis *)
covdef=If[ef=!=Null,
If[!BasisQ[ef],Throw@Message[DefBasis::unknown,"basis",ef]];
tanb=VBundleOfBasis[ef];
If[TangentBundleOfManifold@BaseOfVBundle@tanb=!=tanb,Throw@Message[DefBasis::invalid,ef,"basis to extend"]];
If[BaseOfVBundle@tanb=!=manifold,Throw@Message[DefBasis::invalid,ef,"basis to extend"]];
PDOfBasis[ef],
Null];
(* Check mb *)
If[mb=!={},
If[MatchQ[mb,{_?MetricQ,basisname|-basisname,_}],
Switch[mb3=mb[[3]],
"Orthogonal"|"Orthonormal",Null,
_List,If[Dimensions[mb3]=!={dim}&&Dimensions[mb3]=!={dim,dim},Throw@Message[DefBasis::invalid,mb3,"array of metric values"]],
_,Throw@Message[DefBasis::invalid,mb3,"metric values specification"]
],
Throw@Message[DefBasis::invalid,mb,"value for option MetricInBasis"]
]
];
(* Check change and compute the other basis to which it relates *)
If[change=!=Null,
oldbasis=otherBasis[change,basisname]
];
(* Check eoom *)
If[Head[eoom]=!=List||Length[eoom]=!=2,
Throw@Message[DefBasis::invalid,eoom,"value for option epsilonOrientationOfMetric"];
];
If[eoom[[1]]=!=Null&&!MetricQ[eoom[[1]]],
Throw@Message[DefBasis::unknown,"metric",eoom[[1]]];
];

(**** Define basis ****)
xAct`xTensor`Private`MakeDefInfo[DefBasis,basisname,info];
MakexTensions[DefBasis,"Beginning",basisname,vbundle,cnumbers,options];
(* Register *)
AppendTo[$Bases,basisname];
BasisQ[basisname]^=True;
BasisColor[basisname]^=bc;
VBundleOfBasis[basisname]^=vbundle;
basisname/:CNumbersOf[basisname,vbundle]=Sort@cnumbers;
etaOrientation[basisname]^=Signature[cnumbers];
If[eoom[[1]]=!=Null,basisname/:epsilonOrientation[eoom[[1]],basisname]=eoom[[2]]];
DependenciesOfBasis[basisname]^=deps;
DefInfo[basisname]^=info;
xAct`xTensor`Private`SymbolRelations[basisname,master,{vbundle}];
(* Define associated parallel derivative, always real *)
tbundle=TangentBundleOfManifold[manifold];
With[
{indtng=First@GetIndicesOfVBundle[tbundle,1],
pdbasis=If[HasDaggerCharacterQ[basisname],PDOfBasis[Dagger[basisname]],GiveSymbol[PD,basisname]]},
PDOfBasis[basisname]^=pdbasis;
If[dag=!=Conjugate,
(* Quiet OptionValue::nodef because we are passing DefBasis/DefChart options *)
Quiet[
DefCovD[pdbasis[-indtng],vbundle,{ColorString[$PDPostfixSymbol,bc],ColorString[$PDPrefixSymbol,bc]},Torsion:>Not@ChartQ[basisname],Curvature->False,Master->basisname,ExtendedFrom->covdef,DefInfo->If[info===False,False,{"parallel derivative",""}],options],
OptionValue::nodef];
If[Not@ChartQ[basisname],With[{torsion=Torsion[basisname]},xAct`xTensor`Private`SetPrintAs[torsion,ColorString["T",bc]]]];
BasisOfCovD[pdbasis]^=basisname;
If[dag===Complex,Set[Evaluate[MakeDaggerSymbol[pdbasis]],pdbasis]] 
] 
];
(* Store positions of cnumbers *)
MapIndexed[Set[PNumber[#1,basisname],First[#2]]&,CNumbersOf[basisname,vbundle]];
MapIndexed[Set[PNumber[#1,-basisname],First[#2]]&,CNumbersOf[basisname,vbundle]];
(* Properties of basis under complex conjugation *)
Switch[dag,
Real,Dagger[basisname]^=basisname,
Conjugate,Null,
Complex,
xAct`xTensor`Private`SetDaggerPair[basisname,MakeDaggerSymbol[basisname]];
DefBasis[Dagger[basisname],Dagger[vbundle],cnumbers,Dagger->Conjugate,Master->basisname,options]
];
(* Register change of basis *)
If[change=!=Null,
If[ChartQ[oldbasis],
SetBasisChange[change,oldbasis],
SetBasisChange[change]
]
];
(* Store values of components of some metric in this basis *)
If[mb=!={},Apply[MetricInBasis,mb]];
(* Define associated eta tensors, even for inner vbundles *)
defeta[basisname,dag,info];
(* Format bases elements *)
If[fb=!=Automatic,
If[!MatchQ[fb,{_List,_List}],Throw@Message[DefBasis::invalid,fb,"value for option FormatBasis"]];
FormatUpVectors[fb[[1]],CIndicesOf[-basisname],scalars];
FormatDownVectors[fb[[2]],CIndicesOf[basisname],scalars];
];

MakexTensions[DefBasis,"End",basisname,vbundle,cnumbers,options];

If[pns,Protect[basisname]];
];


SetNumberOfArguments[DefBasis,{3,Infinity}];
Protect[DefBasis];


otherBasis[ctensor_CTensor,basis_]:=If[
CTensorQ[ctensor,2,0],
otherbasis[CTensorBases[ctensor],basis],
$Failed
];
otherBasis[{ctensor1_CTensor,ctensor2_CTensor},basis_]:=With[{
obasis1=otherBasis[ctensor1,basis],
obasis2=otherBasis[ctensor2,basis]
},
If[obasis1===obasis2&&obasis1=!=$Failed&&Reverse[CTensorBases[ctensor1]]===-CTensorBases[ctensor2],
obasis1,
$Failed
]
];
otherBasis[_,_]:=$Failed;


otherbasis[{-basis_,b_?BasisQ},basis_]:=b;
otherbasis[{basis_,-b_?BasisQ},basis_]:=b;
otherbasis[{b_?BasisQ,-basis_},basis_]:=b;
otherbasis[{-b_?BasisQ,basis_},basis_]:=b;
otherbasis[_,_]:=False;


BasisColor[_]:=RGBColor[1,0,0];
SetNumberOfArguments[BasisColor,1];
Protect[BasisColor];


PDOfBasis[-basis_]:=PDOfBasis[basis];
PDOfBasis[x_]:=Throw@Message[PDOfBasis::unknown,"basis",x];
SetNumberOfArguments[PDOfBasis,1];
Protect[PDOfBasis];


BasisOfCovD[x_]:=Null;
SetNumberOfArguments[BasisOfCovD,1];
Protect[BasisOfCovD];


Unprotect[Torsion];
Torsion[basis_?ChartQ]:=Zero;
Torsion[basis_?BasisQ]:=Torsion[PDOfBasis[basis]];
Torsion[x_]:=Throw@Message[Torsion::error,"Cannot construct torsion tensor associated to "<>ToString[x]];
SetNumberOfArguments[Torsion,1];
Protect[Torsion];


BasesOfVBundle[vb_]:=Select[$Bases,VBundleOfBasis[#]===vb&];
SetNumberOfArguments[BasesOfVBundle,1];
Protect[BasesOfVBundle];


UndefBasis[basis_]:=With[{servants=ServantsOf[basis]},
If[!BasisQ[basis],Throw@Message[UndefBasis::unknown,"basis",basis]];
xAct`xTensor`Private`CheckRemoveSymbol[basis];
MakexTensions[UndefBasis,"Beginning",basis];
xUpSet[ServantsOf[basis],{}];
xAct`xTensor`Private`DropFromHosts[basis];
$Bases=DeleteCases[$Bases,basis];
Undef/@servants;
(* For complex bases we need to remove this special definition. This is ugly *)
If[HasDaggerCharacterQ[basis],xAct`xTensor`Private`RemoveSymbol[StringJoin["PD",ToString@basis]]];
MakexTensions[UndefBasis,"End",basis];
xAct`xTensor`Private`MakeUndefInfo[UndefBasis,basis];
xAct`xTensor`Private`RemoveSymbol[basis];
];
SetNumberOfArguments[UndefBasis,1];
Protect[UndefBasis];


DependenciesOfBasis[-basis_]:=DependenciesOfBasis[basis];
DependenciesOfBasis[basis_]:=Throw@Message[DependenciesOfBasis::unknown,"basis",basis];
SetNumberOfArguments[DependenciesOfBasis,1];
Protect[DependenciesOfBasis];


(* Standard use *)
CNumbersOf[basis_]:=CNumbersOf[basis,VBundleOfBasis[basis]];
(* Extended use from 0.7.0 *)
CNumbersOf[-basis_,vbundle_]:=CNumbersOf[basis,vbundle];
CNumbersOf[basis_,-vbundle_]:=CNumbersOf[basis,vbundle];
CNumbersOf[basis_,vbundle_]:=Throw@Message[CNumbersOf::unknown,"cnumbers of basis",basis];
SetNumberOfArguments[CNumbersOf,{1,2}];
Protect[CNumbersOf];


PNumber[cnumber_,basis_]:=Throw[Message[CNumbersOf::invalid,cnumber,"cnumber of basis "<>PrintAs[basis]]];


(* Standard use *)
CIndicesOf[basis:(_?BasisQ|-_?BasisQ)]:=Thread[{CNumbersOf[basis],basis}]; (* BASIS1 *)
(* Extended use from 0.7.0 *)
CIndicesOf[basis:(_?BasisQ|-_?BasisQ),vbundle_]:=Thread[{CNumbersOf[basis,vbundle],basis}];(* BASIS1 *)
(* Sometimes this acts on label indices. Then we need this case *)
CIndicesOf[expr_]:={expr};
CIndicesOf[expr_,_]:={expr};


VBundleOfBasis[-basis_]:=VBundleOfBasis[basis];
SetNumberOfArguments[VBundleOfBasis,1];
Protect[VBundleOfBasis];


SignedVBundleOfBasis[-basis_]:=-VBundleOfBasis[basis];
SignedVBundleOfBasis[basis_]:=VBundleOfBasis[basis];


SplitBasis[basis_,vbrules_List]:=SplitBasis[basis,#]&/@vbrules;
SplitBasis[basis_,vbundle_->cnumbers_List]:=TagSet[basis,CNumbersOf[basis,vbundle],cnumbers];
SetNumberOfArguments[SplitBasis,2];
Protect[SplitBasis];


BasisQ[basis_CirclePlus]^:=And@@(BasisQ/@List@@basis);
VBundleOfBasis[basis_CirclePlus]^:=VBundleOfBasis/@basis;
DependenciesOfBasis[basis_CirclePlus]^:=Union@@(DependenciesOfBasis/@(List@@basis));
PDOfBasis[basis_CirclePlus]^:=Throw@Message[PDOfBasis::error1,"Define the PD of the basis",basis];
  (* TODO: do something better than just black *)
BasisColor[basis_CirclePlus]^:=GrayLevel[0];
(* TODO: this is problematic if cnumbers are repeated *)
CNumbersOf[basis_CirclePlus,vb_]^:=Join@@(CNumbersOf[#,vb]&/@List@@basis);  


Unprotect[Basis];


(* xTensorQ[Basis] is already defined in xTensor, but we repeat it here *)
xTensorQ[Basis]^=True;
Dagger[Basis]^=Basis;
PrintAs[Basis]^="e";
DependenciesOfTensor[Basis]^:={};
SlotsOfTensor[Basis]^:={-All,All};
MasterOf[Basis]^=Symbol;
(* The symmetry of Basis depends on the existence and properties of a metric *)
SymmetryGroupOfTensor[Basis[a_,b_]]^:=With[{metric=xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfIndex[a],False]},If[metric===Null,StrongGenSet[{},GenSet[]],SymmetryGroupOfTensor[metric]]];


(* BASIS1 and BASIS2 *)
Bracket[
Basis[i1:({_,-basis_}|-basis_[_]),a_Symbol],
Basis[i2:({_,-basis_}|-basis_?BasisQ[_]),b_Symbol]
][i_]:=-$TorsionSign Torsion[basis][i,i1,i2]; 


Basis[-a_Symbol,b_Symbol]:=delta[-a,b];


Basis[a_Symbol,b_?DownIndexQ]:=xAct`xTensor`Private`SymmetryOfIndex[a]Basis[b,a];
Basis[a:{_,_?BasisQ},b_?DownIndexQ]:=xAct`xTensor`Private`SymmetryOfIndex[a]Basis[b,a];(* BASIS1 *);
Basis[a:_?BasisQ[_],b_?DownIndexQ]:=xAct`xTensor`Private`SymmetryOfIndex[a]Basis[b,a];(* BASIS2 *);


(* BASIS1 *)
Basis[a:(_Symbol|{_,_?BasisQ}),b:(_Symbol|{_,_?BasisQ})]:=xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfIndex[a],True][a,b];
Basis[a:(-_Symbol|{_,-_?BasisQ}),b:(-_Symbol|{_,-_?BasisQ})]:=xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfIndex[a],True][a,b];


Basis[{i_Integer,-basis_},{j_Integer,basis_}]:=KroneckerDelta[i,j]; (* BASIS1 *);


Basis[{-a_Symbol,-basis_},{a_Symbol,basis_}]:=DimOfVBundle@VBundleOfIndex[a]; (* BASIS1 *)


Basis/:(f_Symbol/;f===Times)[expr1___,Basis[{-a_Symbol,-basis_},{b_Symbol,basis_}],expr2___]:=ReplaceIndex[expr1 expr2,-b->-a]/;IsIndexOf[expr1 expr2,{-b,-basis},Basis];
Basis/:(f_Symbol/;f===Times)[expr1___,Basis[{-a_Symbol,-basis_},{b_Symbol,basis_}],expr2___]:=ReplaceIndex[expr1 expr2,a->b]/;IsIndexOf[expr1 expr2,{a,basis},Basis];


Dir[Basis[-a_Symbol,i_List]]:=i;
Dir[Basis[i_List,a_Symbol]]:=i;
(* QUESTION: How do we include the symmetry of Basis here? *)
Basis[i_,Dir[expr_]]:=With[{ui=xAct`xTensor`Private`UltraindexOf[Dir[expr]]},delta[-ui,i];ReplaceIndex[expr,ui->i]];
Basis[Dir[expr_],i_]:=With[{ui=xAct`xTensor`Private`UltraindexOf[Dir[expr]]},delta[-ui,i];ReplaceIndex[expr,ui->i]];


Protect[Basis];


(* For tensors *)
FormatBasis[{i_Integer,basis_?BasisQ},name_String]:=SetDelayed[xAct`xTensor`Private`xTensorBox[Basis,{ind_,{i,basis}}],xAct`xTensor`Private`MakeTensorBoxes[$TensorBoxes][name,xAct`xTensor`Private`IndexArray[$TensorBoxes][{ind}]]];
FormatBasis[{i_Integer,basis_?BasisQ}]:=Unset[xAct`xTensor`Private`xTensorBox[Basis,{ind_,{i,basis}}]];
FormatBasis[{i_Integer,-basis_?BasisQ},name_String]:=SetDelayed[xAct`xTensor`Private`xTensorBox[Basis,{{i,-basis},ind_}],xAct`xTensor`Private`MakeTensorBoxes[$TensorBoxes][name,xAct`xTensor`Private`IndexArray[$TensorBoxes][{ind}]]];
FormatBasis[{i_Integer,-basis_?BasisQ}]:=Unset[xAct`xTensor`Private`xTensorBox[Basis,{{i,-basis},ind_}]];


(* For derivatives *)
FormatBasis[covd_Symbol?CovDQ[{i_Integer,-basis_?BasisQ}],name_String]:=With[{pattern={i,-basis}},
MakeBoxes[covd[pattern][expr_],StandardForm]:=Block[{$WarningFrom="CovD Formatting"},
xAct`xTensor`Private`interpretbox[covd[pattern][expr],xAct`xTensor`Private`FlattenRowBox@RowBox[{name,xAct`xTensor`Private`boxof@MakeBoxes[expr,StandardForm]}]]
]];
FormatBasis[covd_Symbol?CovD[{i_Integer,-basis_?BasisQ}]]:=With[{pattern={i,-basis}},Unset[MakeBoxes[covd[pattern][expr_],StandardForm]]];
SetNumberOfArguments[FormatBasis,{1,2}];
Protect[FormatBasis];


BasisArray[][]:=1;
BasisArray[][a_]:=1;
BasisArray[-basis_?BasisQ][inds___]:=BasisArray[basis][inds];
BasisArray[basis_?BasisQ][a_?UpIndexQ]:=Map[Basis[{#,-basis},a]&,CNumbersOf[basis,VBundleOfIndex[a]]];
BasisArray[basis_?BasisQ][a_?DownIndexQ]:=Map[Basis[a,{#,basis}]&,CNumbersOf[basis,VBundleOfIndex[a]]];
BasisArray[bases__][indices__]:=Which[
Length[{bases}]==Length[{indices}],
Outer[Times,Inner[BasisArray[#1][#2]&,IndexList[bases],IndexList[indices],Sequence]],
Length[{bases}]==1,
Apply[BasisArray,Table[bases,{Length[{indices}]}]][indices],
True,
Throw@Message[BasisArray::error,"Inconsistent number of bases and indices."]];
Protect[BasisArray];


BasisCollect[expr_]:=Collect[expr,{a1_Basis a2_Basis a3_Basis a4_Basis,a1_Basis a2_Basis a3_Basis,a1_Basis a2_Basis,a1_Basis}];
BasisCollect[expr_,f_]:=Collect[expr,{a1_Basis a2_Basis a3_Basis a4_Basis,a1_Basis a2_Basis a3_Basis,a1_Basis a2_Basis,a1_Basis},f];


(* General definition for b-indices *)
DaggerBIndex[_,{a_,basis_}]:={DaggerIndex[a],Dagger[basis]};(* BASIS1 *)
DaggerBIndex[_,basis_?BasisQ[a_]]:=Dagger[basis][DaggerIndex[a]];(* BASIS2 *)


(* Default definition for c-indices *)
DaggerCIndex[-basis_,cindex_]:=DaggerCIndex[basis,cindex];
DaggerCIndex[_,{a_Integer,basis_}]:={a,Dagger[basis]};(* BASIS1 *)
DaggerCIndex[_,basis_?BasisQ[a_Integer]]:=Dagger[basis][a];(* BASIS2 *)
SetNumberOfArguments[DaggerCIndex,2];
Protect[DaggerCIndex];


(* Consistent conjugation. Internal function *)
inner[basis_,sbasis_,mat_,dbasis_,duind_]:=Inner[TagSet[basis,##]&,Map[Unevaluated[DaggerCIndex[basis,{#,sbasis}]]&,IndexList@@CNumbersOf[sbasis]],IndexList@@Map[Dir,mat.BasisArray[dbasis][duind]],List];
(* With no matrix, remove DaggerCIndex upvalues *)
SetDaggerMatrix[basis_]:=
With[{dagbasis=Dagger[basis]},
With[{prot=Unprotect[basis,dagbasis]},
UpValues[basis]^=DeleteCases[UpValues[basis],_[_[DaggerCIndex[basis,_]],_]];
If[DaggerQ[basis],UpValues[dagbasis]^=DeleteCases[UpValues[dagbasis],_[_[DaggerCIndex[dagbasis,_]],_]]];
Protect[Evaluate[prot]];
]
];
(* With a matrix we need four calls to inner *)
SetDaggerMatrix[basis_,matrix_]:=With[{inversematrix=Inverse[matrix],uind=IndicesOfVBundle[VBundleOfBasis[basis]][[1,1]],dagbasis=Dagger[basis]},
With[{daguind=DaggerIndex[uind],prot=Unprotect[basis,dagbasis]},
If[DaggerQ[basis],
UpValues[dagbasis]^=DeleteCases[UpValues[dagbasis],_[_[DaggerCIndex[dagbasis,_]],_]];
inner[dagbasis,-dagbasis,Dagger[inversematrix],basis,uind];
inner[dagbasis,dagbasis,Dagger[Transpose[matrix]],basis,-uind],
(* Check unitarity of matrix *)
If[Simplify[matrix.Transpose[Dagger[matrix]]]=!=IdentityMatrix[Length[matrix]],Throw@Message[SetDaggerMatrix::error,"Complexification of a real basis requires a unitary matrix."]]
];
UpValues[basis]^=DeleteCases[UpValues[basis],_[_[DaggerCIndex[basis,_]],_]];
inner[basis,-basis,matrix,dagbasis,daguind];
inner[basis,basis,Transpose[inversematrix],dagbasis,-daguind];

Protect[Evaluate[prot]];
]
];
SetNumberOfArguments[SetDaggerMatrix,{1,2}];
Protect[SetDaggerMatrix];


etaUp[basis_]:=GiveSymbol[etaUp,basis];
etaDown[basis_]:=GiveSymbol[etaDown,basis];
GiveSymbol[etaUp,basis_]:=SymbolJoin["etaUp",basis];
GiveSymbol[etaDown,basis_]:=SymbolJoin["etaDown",basis];


GiveOutputString[etaUp,basis_]:="\[Eta]";
GiveOutputString[etaDown,basis_]:="\[Eta]";


defeta[basis_,dag_,info_]:=With[{
vbundle=VBundleOfBasis[basis],
etaU=GiveSymbol[etaUp,basis],
etaD=GiveSymbol[etaDown,basis],
pd=PDOfBasis[basis]},
Module[{dim=DimOfVBundle[vbundle],inds,i1,i2,integerdimQ,prot},
integerdimQ=IntegerQ[dim];
inds=GetIndicesOfVBundle[vbundle,If[integerdimQ,Max[dim,2],2]];
{i1,i2}=inds[[{1,2}]];

(*** Define etaUp and etaDown ***)
If[dag=!=Conjugate,
DefTensor[etaU@@inds,vbundle,Antisymmetric@Range@Length@inds,PrintAs:>GiveOutputString[etaUp,basis],Dagger->dag,WeightOfTensor->basis,Master->basis,DefInfo->If[info===False,False,{"antisymmetric +1 density",""}]];
DefTensor[etaD@@(-inds),vbundle,Antisymmetric@Range@Length@inds,PrintAs:>GiveOutputString[etaDown,basis],Dagger->dag,WeightOfTensor->-basis,Master->basis,DefInfo->If[info===False,False,{"antisymmetric -1 density",""}]]
];

(*** Properties of the etaUp and etaDown tensors ***)
prot=Unprotect[etaU,etaD];
(* If the dimension is a symbol then overwrite the symmetries *)
If[Not@integerdimQ,
TagUnset[etaU,SymmetryGroupOfTensor[etaU]];
TagUnset[etaD,SymmetryGroupOfTensor[etaD]];
TagSetDelayed[etaU,SymmetryGroupOfTensor[etaU[upinds__]],Antisymmetric[Range@Length@upinds]];
TagSetDelayed[etaD,SymmetryGroupOfTensor[etaD[downinds__]],Antisymmetric[Range@Length@downinds]]
];
(* Product of etas with same length *)
etaU/:etaU[upinds__]etaD[downinds__]:=Gdelta[downinds,upinds]/;Length@IndexList[upinds]===Length@IndexList[downinds];
(* First derivatives in the original index characters. Use SeparateMetric otherwise *)
etaU/:(der_?FirstDerQ)[etaU[inds:(_Symbol)..]]:=With[{dummy=DummyIn[vbundle],bdummy={DummyIn[vbundle],basis}},Basis[-dummy,bdummy]der[Basis[-bdummy,dummy]]etaU[inds]];
etaD/:(der_?FirstDerQ)[etaD[inds:(-_Symbol)..]]:=With[{dummy=DummyIn[vbundle],bdummy={DummyIn[vbundle],basis}},Basis[-bdummy,dummy]der[Basis[-dummy,bdummy]]etaD[inds]];
(* Numerical values *)
etaU[inds:({_Integer,basis}..)]:=etaOrientation[basis] Signature[First/@{inds}];
etaD[inds:({_Integer,-basis}..)]:=etaOrientation[basis] Signature[First/@{inds}];
(* Protection *)
Protect[Evaluate[prot]];

]
];
Protect[etaUp,etaDown];


GiveSymbol[Jacobian,basis_,AIndex]:=SymbolJoin[Jacobian,basis];
GiveSymbol[Jacobian,basis1_,basis2_]:=SymbolJoin[Jacobian,basis1,basis2];
GiveOutputString[Jacobian,upbasis_,downbasis_]:="J";


BasisOrderedQ[{AIndex,AIndex}]:=True;
BasisOrderedQ[{AIndex,basis2_}]:=False;
BasisOrderedQ[{basis1_,AIndex}]:=True;
BasisOrderedQ[{basis1_,basis2_}]:=OrderedQ[{basis1,basis2}];


Unprotect[Jacobian];
Jacobian::reordered="Dagger bases `1` and `2` are not sorted. Definining additional Jacobians.";
Jacobian[basis_,basis_][]:=1;
Jacobian[basis2_,basis1_][]:=1/Jacobian[basis1,basis2][]/;BasisOrderedQ[{basis1,basis2}];
Jacobian[basis1_,basis2_][]:=ConstructJacobian[GiveSymbol[Jacobian,basis1,basis2],basis1,basis2]/;BasisOrderedQ[{basis1,basis2}];
Protect[Jacobian];


(* Jacobian already exists *)
ConstructJacobian[jac_?xTensorQ,basis1_,basis2_]:=jac[];
(* It does not exist yet *)
ConstructJacobian[jac_Symbol,basis1_,basis2_]:=With[{vbundle=VBundleOfBasis[basis1],dagQ=Or[DaggerQ@basis1,DaggerQ@basis2]},
If[basis1=!=AIndex&&basis2=!=AIndex&&vbundle=!=VBundleOfBasis[basis2],Throw@Message[Jacobian::error,"Incompatible vbundles of bases."]];

(* Real Jacobians *)
ConstructJacobian1[jac,basis1,basis2,Null,True,vbundle,Real];

(* Complex Jacobians *)
If[dagQ,
UnSet[Dagger[jac]];
With[{b1dag=Dagger@basis1,b2dag=Dagger@basis2},
With[{jacdag=GiveSymbol[Jacobian,b1dag,b2dag],sortedQ=OrderedQ[{b1dag,b2dag}]},
(* Complex bases are still sorted *)
ConstructJacobian1[jacdag,b1dag,b2dag,jac,sortedQ,vbundle,Conjugate];
xAct`xTensor`Private`SetDaggerPair[jac,jacdag];
(* Complex bases are unsorted: define inverses as well *)
If[!sortedQ,
Message[Jacobian::reordered,b1dag,b2dag];
With[{invjacdag=GiveSymbol[Jacobian,b2dag,b1dag],invjac=GiveSymbol[Jacobian,basis2,basis1]},
ConstructJacobian1[invjacdag,b2dag,b1dag,jac,True,vbundle,Conjugate];
ConstructJacobian1[invjac,basis2,basis1,jac,False,vbundle,Real];
UnSet[Dagger[invjac]];
jacdag[]=1/invjacdag[];
invjac[]=1/jac[];
xAct`xTensor`Private`SetDaggerPair[invjac,invjacdag];
]
]
]
]
];

jac[]
];


ConstructJacobian1[jac_Symbol,basis1_,basis2_,master_,ders_,vbundle_,dag_]:=Module[{},
DefTensor[jac[],vbundle,DefInfo->{"",""},ProtectNewSymbol->False,PrintAs:>GiveOutputString[Jacobian,basis1,basis2],WeightOfTensor->basis1-basis2,Dagger->dag];
xAct`xTensor`Private`SymbolRelations[jac,master,{basis1,basis2}];
(* Derivatives of a Jacobian *)
If[ders,
jac/:covd_Symbol?CovDQ[a_][jac[]]:=With[{dummy=DummyIn[vbundle]},-Christoffel[PDOfBasis[basis1],PDOfBasis[basis2]][dummy,a,-dummy]jac[]];
jac/:der_?FirstDerQ[jac[]]:=With[{dummy=DummyIn[vbundle],bdummy1={DummyIn[vbundle],basis1},bdummy2={DummyIn[vbundle],basis2}},(Basis[-dummy,bdummy1]der[Basis[-bdummy1,dummy]]+Basis[-bdummy2,dummy]der[Basis[-dummy,bdummy2]])jac[]]]
];


Unprotect[Determinant];


Determinant[metric_,-basis_?BasisQ]:=Determinant[metric,basis];
Determinant[delta[-_Symbol,_Symbol],basis_?BasisQ]:=1;
Determinant[delta[_Symbol,-_Symbol],basis_?BasisQ]:=1;
Determinant[metric_?MetricQ[-_Symbol,-_Symbol],basis_?BasisQ]:=Determinant[metric,basis][];
Determinant[metric_?MetricQ[_Symbol,_Symbol],basis_?BasisQ]:=1/Determinant[metric,basis][];


Determinant[tensor_?xTensorQ[inds__?ABIndexQ],basis_?BasisQ]:=
With[{dim=DimOfVBundle@VBundleOfBasis[basis],n=Length[{inds}]},Module[{dummies=Table[DummyAs/@{inds},{dim}],eta},
eta[indices:{_?UpIndexQ,___}]:=etaUp[basis]@@indices;
eta[indices:{_?DownIndexQ,___}]:=etaDown[basis]@@indices;
1/dim!Apply[Times,Apply[tensor,dummies,{1}]]Apply[Times,Map[eta,Transpose[-dummies],{1}]]
]
];


Protect[Determinant];


epsilonToetaDown[metric_?MetricQ,basis_?BasisQ]:=epsilon[metric][inds__]:>epsilonOrientation[metric,basis] Sqrt[SignDetOfMetric[metric]Determinant[metric,basis][]]etaDown[basis][inds];
epsilonToetaUp[metric_?MetricQ,basis_?BasisQ]:=epsilon[metric][inds__]:>epsilonOrientation[metric,basis] SignDetOfMetric[metric]/Sqrt[SignDetOfMetric[metric]Determinant[metric,basis][]]etaUp[basis][inds];


etaUpToepsilon[basis_?BasisQ,metric_?MetricQ]:=etaUp[basis][inds__]:>1/epsilonOrientation[metric,basis] SignDetOfMetric[metric]Sqrt[SignDetOfMetric[metric]Determinant[metric,basis][]]epsilon[metric][inds];
etaDownToepsilon[basis_?BasisQ,metric_?MetricQ]:=etaDown[basis][inds__]:>1/epsilonOrientation[metric,basis]/Sqrt[SignDetOfMetric[metric]Determinant[metric,basis][]]epsilon[metric][inds];


SetNumberOfArguments[#,2]&/@{epsilonToetaDown,epsilonToetaUp,etaUpToepsilon,etaDownToepsilon};
Protect[epsilonToetaDown,epsilonToetaUp,etaUpToepsilon,etaDownToepsilon];


CommonDependencies[PD[a_],bases_List]:=Intersection[{BaseOfVBundle[VBundleOfIndex[a]]},Union@@(DependenciesOfBasis/@bases)];
CommonDependencies[covd_[a_],bases_List]:=Intersection[DependenciesOfCovD[covd],Union@@(DependenciesOfBasis/@bases)];


Unprotect[Basis];
(* Derivative of basis vector fields *)
Basis/:HoldPattern[covd_?CovDQ[a_][Basis[{c_,-basis_?BasisQ},b_Symbol]]]:=If[CommonDependencies[covd[a],{basis}]==={},0,Christoffel[covd,PDOfBasis@basis][b,a,{c,-basis}]];
(* Derivative of basis covector fields *)
Basis/:HoldPattern[covd_?CovDQ[a_][Basis[-b_Symbol,{c_,basis_?BasisQ}]]]:=If[CommonDependencies[covd[a],{basis}]==={},0,-Christoffel[covd,PDOfBasis@basis][{c,basis},a,-b]];
(* Derivative of a change of basis. Note that it is independent of covd *)
Basis/:HoldPattern[covd_?CovDQ[a_][Basis[c:{_,-basisc_?BasisQ},b:{_,basisb_?BasisQ}]]]:=If[CommonDependencies[covd[a],{basisc,basisb}]==={},0,Christoffel[PDOfBasis@basisb,PDOfBasis@basisc][b,a,c]];
Protect[Basis];


RicciRotation[covd_?CovDQ]:=Christoffel[covd];
RicciRotation[covd_?CovDQ,basis_?BasisQ]:=Christoffel[covd,PDOfBasis[basis]];
SetNumberOfArguments[RicciRotation,{1,2}];
Protect[RicciRotation];


Unprotect[LieD];
LieD[v_][Basis[{b_,-basis_?BasisQ},a_Symbol]]:=-PDOfBasis[basis][{b,-basis}][ReplaceIndex[v,xAct`xTensor`Private`UltraindexOf[v]->a]];
LieD[v_][Basis[-a_Symbol,{b_,basis_?BasisQ}]]:=PDOfBasis[basis][-a][ReplaceIndex[v,xAct`xTensor`Private`UltraindexOf[v]->{b,basis}]];
LieD[v_][Basis[{b_,-basis2_?BasisQ},{a_,basis1_?BasisQ}]]:=With[{dummy=DummyAs[{a,basis1}]},Christoffel[PDOfBasis[basis1],PDOfBasis[basis2]][{a,basis1},-dummy,{b,-basis2}]ReplaceIndex[v,xAct`xTensor`Private`UltraindexOf[v]->dummy]];
Protect[LieD];


PrecomposeBasis[basis_,_IdentityMapping]:=basis;
(* Precompose and SmallCircle are essentially the same function, so we keep the order *)
PrecomposeBasis[PrecomposeBasis[basis_,phi1_],phi2_]:=PrecomposeBasis[basis,SmallCircle[phi1,phi2]];


BasisQ[PrecomposeBasis[basis_?BasisQ,phi_?MappingQ]]^:=True;
BasisColor[PrecomposeBasis[basis_,phi_]]^:=BasisColor[basis];
CNumbersOf[PrecomposeBasis[basis_,phi_],___]^:=CNumbersOf[basis];
DependenciesOfBasis[PrecomposeBasis[basis_,phi_]]^:=xAct`xTensor`Private`PrecomposeDependencies[DependenciesOfBasis[basis],phi];
(* TODO: This is probably wrong *)
etaOrientation[PrecomposeBasis[basis_,phi_]]^:=etaOrientation[basis];
PDOfBasis[PrecomposeBasis[basis_,phi_]]^:=PullBackCovD[PDOfBasis[basis],phi];
VBundleOfBasis[PrecomposeBasis[basis_,phi_]]^:=PullBackVBundle[VBundleOfBasis[basis],phi];
Dagger[PrecomposeBasis[basis_,phi_]]^:=PrecomposeBasis[Dagger[basis],Dagger[phi]];


PrecomposeBasis[-basis_,phi_]:=-PrecomposeBasis[basis,phi];


PrintAs[PrecomposeBasis[basis_,phi_]]^:=xAct`xTensor`Private`PrecomposeString[PrintAs[basis],phi];
MakeBoxes[PrecomposeBasis[basis_,phi_],StandardForm]:=xAct`xTensor`Private`PrecomposeBox[MakeBoxes[basis,StandardForm],phi];


(************************* 3. Abstract expressions ************************)


Protect[OverDerivatives];


Quiet[ContractBasis[xAct`xTensor`Private`expr_]=.];


Options[ContractBasis]:={OverDerivatives->False};
(* Functions associated to the options *)
overds[options:OptionsPattern[]]:=OptionValue[ContractBasis,{options},OverDerivatives];
(* Shortcuts *)
ContractBasis[expr_,index_?GIndexQ,options___]:=ContractBasis[expr,IndexList[index],options];
ContractBasis[expr_,master:(_?VBundleQ|_?BasisQ|AIndex),options___]:=ContractBasis[expr,IndicesOf[Basis,master][expr],options];
ContractBasis[expr_,f_IndicesOf,options___]:=ContractBasis[expr,f[expr],options];
(* Main definition *)
ContractBasis[expr_,list_IndexList,options___]:=ContractBasis1[overds[options]][Expand[expr],list];
(* Default case: all indices *)
ContractBasis[expr_,options___]:=ContractBasis[expr,IndicesOf[Basis],options];
SetNumberOfArguments[ContractBasis,{1,Infinity}];
Protect[ContractBasis];


(* Plus: thread *)
ContractBasis1[TF_][expr_Plus,inds_]:=ContractBasis1[TF][#,inds]&/@expr;
(* List of indices in Basis objects: fold *)
ContractBasis1[TF_][expr_,list_IndexList]:=Fold[ContractBasis1[TF],expr,list];
(* Specify (AB) index of a Basis vector. Separate upper/lower indices. We need an additional rule for inert-heads *)
ContractBasis1[TF_][expr_,i_?ABIndexQ]:=ContractBasis1[TF][expr,i,UpIndexQ[i]];
(* Upper index *)
ContractBasis1[False][expr_,i_,True]:=Expand[
expr/.With[{ic=-i},{
Basis[a_,i]tensor_?xTensorQ[i1___,ic,i2___]:>tensor[i1,a,i2],
Basis[a_,i]covd_?CovDQ[ic][expr1_]:>covd[a][expr1],
Basis[a_,i]exprod:(prod_Symbol?ProductQ[exprs__]):>ContractBasisInProduct[Basis[a,i],exprod,ic]/;IsIndexOf[exprod,ic,delta],
Basis[ic,a_]tensor_?xTensorQ[i1___,i,i2___]:>tensor[i1,a,i2],
Basis[ic,a_]covd_?CovDQ[i][expr1_]:>covd[a][expr1],
Basis[ic,a_]exprod:(prod_Symbol?ProductQ[exprs__]):>ContractBasisInProduct[Basis[ic,a],exprod,i]/;IsIndexOf[exprod,i,delta]
}]
];
ContractBasis1[True][expr_,i_?ABIndexQ,True]:=Expand[
ContractBasis1[False][expr,i,True]/.With[{ic=-i},{
Basis[a_,i]der_?FirstDerQ[expr1_]:>ContractBasisOverDerivatives[der,Basis[a,i],expr1,i]/;IsIndexOf[expr1,ic,delta],
Basis[ic,a_]der_?FirstDerQ[expr1_]:>ContractBasisOverDerivatives[der,Basis[ic,a],expr1,ic]/;IsIndexOf[expr1,i,delta]
}]
];
(* Lower index *)
ContractBasis1[False][expr_,i_,False]:=Expand[expr/.With[{ic=-i},{
Basis[i,a_]tensor_?xTensorQ[i1___,ic,i2___]:>tensor[i1,a,i2],
Basis[i,a_]covd_Symbol?CovDQ[ic][expr1_]:>covd[a][expr1],
Basis[i,a_]exprod:(_Symbol?ProductQ[__]):>ContractBasisInProduct[Basis[i,a],exprod,ic]/;IsIndexOf[exprod,ic,delta],
Basis[a_,ic]tensor_?xTensorQ[i1___,i,i2___]:>tensor[i1,a,i2],
Basis[a_,ic]covd_Symbol?CovDQ[i][expr1_]:>covd[a][expr1],
Basis[a_,ic]exprod:(_Symbol?ProductQ[__]):>ContractBasisInProduct[Basis[a,ic],exprod,i]/;IsIndexOf[exprod,i,delta]
}]
];
ContractBasis1[True][expr_,i_?ABIndexQ,False]:=Expand[
ContractBasis1[False][expr,i,False]/.With[{ic=-i},{
Basis[i,a_]der_?FirstDerQ[expr1_]:>ContractBasisOverDerivatives[der,Basis[i,a],expr1,i]/;IsIndexOf[expr1,ic,delta],
Basis[a_,ic]der_?FirstDerQ[expr1_]:>ContractBasisOverDerivatives[der,Basis[a,ic],expr1,ic]/;IsIndexOf[expr1,i,delta]
}]
];
(* For other types of indices, do nothing *)
ContractBasis1[_][expr_,AnyIndices]:=expr;
ContractBasis1[_][expr_,_]:=expr;


(* Contraction through products. This is recomputing information that we should know already *)
ContractBasisInProduct[basis_,exprod_,i_]:=With[{pos=Position[exprod,_?(IsIndexOf[#,i,delta]&),{1}][[1,1]]},MapAt[ContractBasis[basis #]&,exprod,pos]];
(* Contraction over derivatives *)
ContractBasisOverDerivatives[der_,basis_,expr1_,i_]:=der[ContractBasis[basis expr1,IndexList[i],OverDerivatives->True]]-der[basis] expr1;


checkreplace[expr1_,expr2_,i_]:=If[expr1===expr2,Throw@Message[FreeToBasis::error,"Index "<>ToString[i]<>" not found in expression."],expr1];


(* basis1 is never AIndex. This function is very similar to CompatibleQ *)
indexas[a_,basis1_]:=indexas[a,basis1,VBundleOfIndex[a],VBundleOfBasis[basis1]];
indexas[a_Symbol,basis1_?BasisQ,vbundlei_,vbundleb_]:={a,basis1}/;SubvbundleQ[vbundleb,vbundlei];
indexas[-a_Symbol,basis1_?BasisQ,vbundlei_,vbundleb_]:={-a,-basis1}/;SubvbundleQ[vbundleb,vbundlei];
indexas[_,_,_,_]:=Null;
changebasis[_,AnyIndices]:=IndexList[Null,Null];
changebasis[AIndex,a_?AIndexQ]:=IndexList[Null,Null];
changebasis[AIndex,{a_?AIndexQ,AIndex}]:=IndexList[Null,Null];
changebasis[AIndex,{a_?AIndexQ,basis2_}]:=IndexList[a,DummyAs[a,basis2]];
changebasis[basis1_?BasisQ,a_?AIndexQ]:=IndexList[indexas[a,basis1],DummyAs[a]];
changebasis[basis1_?BasisQ,{a_?AIndexQ,AIndex}]:=IndexList[indexas[a,basis1],DummyAs[a]];
changebasis[basis1_?BasisQ,{a_?AIndexQ,basis2_}]:=IndexList[indexas[a,basis1],DummyAs[a,basis2]];
changebasis[_,_]:=IndexList[Null,Null];


(* Driver definitions *)
FreeToBasis[-basis_][args__]:=FreeToBasis[basis][args];
FreeToBasis[basis_][expr_]:=FreeToBasis[basis][expr,IndicesOf[Free][expr]];
FreeToBasis[basis_][expr_,f_IndicesOf]:=FreeToBasis[basis][expr,f[expr]];
FreeToBasis[basis_][expr_,inds_IndexList]:=Fold[FreeToBasis[basis],expr,inds];
(* Compute pair of indices *)
FreeToBasis[basis1_][expr_,a_]:=freetobasis[expr,a,changebasis[basis1,a]];
(* Actual change *)
freetobasis[expr_,a2_,IndexList[Null,_]]:=expr;
freetobasis[expr_,a2_,IndexList[a1_,d2_]]:=ContractBasis[If[UpIndexQ[a1],Basis[-d2,a1],Basis[a1,-d2]]checkreplace[ReplaceIndex[expr,a2->d2],expr,a2],d2];
Protect[FreeToBasis];


AutomaticBasisContractionStart[]:=Module[{prot=Unprotect[Basis]},
Basis/:Basis[a_,{b_Symbol,basis_?BasisQ}]Basis[{-b_Symbol,-basis_?BasisQ},c_]:=Basis[a,c];
Basis/:Basis[a:{_,_},b_Symbol]Basis[-b_Symbol,c:{_,_}]:=Basis[a,c];
Protect[Evaluate[prot]];];
AutomaticBasisContractionStop[]:=Module[{prot=Unprotect[Basis]},
Basis/:Basis[a_,{b_Symbol,basis_?BasisQ}]Basis[{-b_Symbol,-basis_?BasisQ},c_]=.;
Basis/:Basis[a:{_,_},b_Symbol]Basis[-b_Symbol,c:{_,_}]=.;
Protect[Evaluate[prot]];];


ContractBasisPairs[expr_,AIndex]:=expr/.Basis[a_,b_Symbol]Basis[-b_Symbol,c_]:>Basis[a,c];
ContractBasisPairs[expr_,basis_?BasisQ]:=expr/.{Basis[a_,{b_,basis}]Basis[{-b_,-basis},c_]:>Basis[a,c],Basis[a_,b_Symbol]Basis[-b_Symbol,c_]:>Basis[a,c]};


Quiet[SeparateBasis[][xAct`xTensor`Private`expr_]=.]


(* Default case *)
SeparateBasis[][args__]:=SeparateBasis[AIndex][args];
(* Change to the "contravariant" form of bases *)
SeparateBasis[-basis_][args__]:=SeparateBasis[basis][args];
(* Thread over sums to avoid Union of indices from FindIndices *)
SeparateBasis[basis_][expr_Plus,indices___]:=SeparateBasis[basis][#,indices]&/@expr;

(* IndicesOf *)
SeparateBasis[basis_][expr_,f_IndicesOf]:=SeparateBasis[basis][expr,f[expr]]
(* Expand list of indices *)
SeparateBasis[basis_][expr_,list_IndexList]:=Fold[SeparateBasis[basis],expr,DeleteCases[list,_LI|-_LI|AnyIndices]];
(* Expand all indices, except those of given basis *)
SeparateBasis[basis_][expr_]:=SeparateBasis[basis][expr,IndicesOf[]]

(* Labels *)
SeparateBasis[basis_][expr_,_LI|-_LI]:=expr;
(* Expand in a given basis *)
SeparateBasis[basis_][expr_,index_?GIndexQ]:=If[CompatibleQ[basis,index],
expr/.{
tensor_?xTensorQ[i1___,index,i2___]:>With[{ii=DummyAs[index,basis]},tensor[i1,ii,i2]If[UpIndexQ[index],Basis[-ii,index],Basis[index,-ii]]],
covd_Symbol?CovDQ[index][expr1_]:>With[{ii=DummyAs[index,basis]},covd[ii][expr1]If[UpIndexQ[index],Basis[-ii,index],Basis[index,-ii]]]
},
expr];
(* Other cases *)
SeparateBasis[_][expr_,_]:=expr;

(* Error messages *)
SeparateBasis[___][]:=Message[SeparateBasis::argt,SeparateBasis,0,1,2];
SeparateBasis[___][_,_,x__]:=Message[SeparateBasis::argt,SeparateBasis,2+Length[{x}],1,2];
Protect[SeparateBasis];


CompatibleQ[AIndex,index_?AIndexQ]:=False;
CompatibleQ[AIndex,index_]:=True;
CompatibleQ[basis_,{_?AIndexQ,basis_|-basis_}]:=False;
CompatibleQ[basis_,index_]:=SubvbundleQ[VBundleOfBasis[basis],VBundleOfIndex[index]];


DummyToBasis[basis_][expr_]:=DummyToBasis[basis][expr,IndicesOf[Dummy,Not[basis]]];
DummyToBasis[basis_][expr_,f_IndicesOf]:=DummyToBasis[basis][expr,f[expr]];
DummyToBasis[basis_][expr_,list_IndexList]:=Fold[DummyToBasis[basis],expr,list];
DummyToBasis[basis_][expr_,i_?ABIndexQ]:=ContractBasis[SeparateBasis[basis][expr,i],i,OverDerivatives->True];
DummyToBasis[basis_][expr_,_]:=expr;
Protect[DummyToBasis];


DetToBasis[AIndex][expr_]:=expr;
DetToBasis[basis_][expr_]:=DetToBasis[basis][expr,MetricsOfVBundle[VBundleOfBasis[basis]]];
DetToBasis[basis_][expr_,metrics_List]:=Fold[ReplaceAll[#1,Determinant[#2,AIndex][]->Determinant[#2,basis][]]&,expr,metrics];


ToBasis[basis_][expr_,inds___]:=DetToBasis[basis][FreeToBasis[basis][DummyToBasis[basis][expr,inds],inds]];
Protect[ToBasis];


FoldToBasis[expr_,finds_List,dbases_]:=
FoldFreeToBasis[FoldDummyToBasis[expr,dbases,FindDummyIndices[expr]],finds];
(* Deal with the dummy indices *)
FoldDummyToBasis[expr_,bases_,IndexList[]]:=expr;
FoldDummyToBasis[expr_,{},dummies_IndexList]:=expr;
FoldDummyToBasis[expr_,basis_?BasisQ,dummies_IndexList]:=DummyToBasis[basis][expr,dummies];
FoldDummyToBasis[expr_,bases_List,dummies_IndexList]:=Fold[With[{basis=#2},DummyToBasis[basis][#1,Select[dummies,xAct`xTensor`Private`IndexOnQ[#,VBundleOfBasis[basis]]&]]]&,expr,bases];
(* Deal with the free indices *)
FoldFreeToBasis[expr_,{}]:=expr;
FoldFreeToBasis[expr_,finds:{__List}]:=Fold[FreeToBasis[Last[#2]][#1,First[#2]]&,expr,finds];


dummyrule[index:{-a_Symbol,-basis_?BasisQ}]:=dummyrule[{a,basis}];
dummyrule[index:{a_Symbol,basis_?BasisQ}]:=index:>IndexList@@CIndicesOf[basis,VBundleOfIndex[a]];


(* In case we have more than one inds-arguments (some could have head IndexList), we fold the action *)
TraceBasisDummy[expr_,inds1_,inds2__]:=Fold[TraceBasisDummy,expr,{inds1,inds2}];
(* With no explicit inds we trace all basis dummies *)
TraceBasisDummy[expr_]:=TraceDummy[expr,{a_Symbol,basis_?BasisQ}:>IndexList@@CIndicesOf[basis,VBundleOfIndex[a]]];
(* Now we always have one "index" argument. It can be a basis: shortcut *)
TraceBasisDummy[expr_,basis_?BasisQ]:=TraceDummy[expr,{_Symbol?AIndexQ,basis}:>IndexList@@CIndicesOf[basis,VBundleOfBasis[basis]]];
(* Convert other cases to IndexList *)
TraceBasisDummy[expr_,{-a_Symbol?AIndexQ,-basis_?BasisQ}]:=TraceBasisDummy[expr,IndexList[{a,basis}]];
TraceBasisDummy[expr_,index:{_Symbol?AIndexQ,_?BasisQ}]:=TraceBasisDummy[expr,IndexList[index]];
TraceBasisDummy[expr_,f_IndicesOf]:=TraceBasisDummy[expr,f[expr]];
TraceBasisDummy[expr_,vbundle_?VBundleQ]:=TraceBasisDummy[expr,IndicesOf[Dummy,BIndex,vbundle]];
(* Error *)
TraceBasisDummy::notrace="Cannot trace `1`.";
TraceBasisDummy[expr_,index_?GIndexQ]:=(Message[TraceBasisDummy::notrace,index];expr);
(* And from now on inds always has head IndexList *)
TraceBasisDummy[expr_,IndexList[]]:=expr;
TraceBasisDummy[expr_,inds_IndexList]:=TraceDummy[expr,Apply[List,dummyrule/@inds]];
TraceBasisDummy[expr_,x_]:=Throw@Message[TraceBasisDummy::invalid,x,"index specification"];

SetNumberOfArguments[TraceBasisDummy,{1,Infinity}];
Protect[TraceBasisDummy];


BasisExpand[expr_,basis_?BasisQ]:=TraceBasisDummy[SeparateBasis[basis][DummyToBasis[basis][expr]]];
BasisExpand[expr_,bases_List]:=Fold[BasisExpand,expr,bases];
SetNumberOfArguments[BasisExpand,2];
Protect[BasisExpand];


splitrule[{_Integer,basis_}]:=Sequence[];
splitrule[{_Integer,basis_,range_}]:=Sequence[];
splitrule[{a_,basis_}]:=a->IndexList@@CNumbersOf[basis,VBundleOfIndex[{a,basis}]];
splitrule[a_]:=Throw@Message[ComponentArray::invalid,a,"basis-index"];


keeporder[list1_,list2_]:=IndexList@@Cases[list1,Alternatives@@Verbatim/@list2];


(* Special non-index notation *)
ComponentArray[tensor_?xTensorQ,bases_List]:=Outer[tensor,Apply[Sequence,CIndicesOf/@bases],1];
(* One argument: all free b-indices *)
ComponentArray[expr_]:=ComponentArray[expr,IndicesOf[Free,BIndex]];
(* Thread on lists *)
ComponentArray[list_List,inds_]:=Map[ComponentArray[#,inds]&,list];
(* Indices. Do nothing on c-indices*)
ComponentArray[expr_,{_Integer,basis_}]:=expr;
ComponentArray[expr_,b:{_?AIndexQ,_}]:=ComponentArray[expr,IndexList[b]];
(* Compute indices of expression. Indices are sorted! *)
ComponentArray[expr_,IndicesOf[selectors___]]:=ComponentArray1[expr,IndicesOf[Free,BIndex,selectors][expr]];
(* Exception: 0. Split all indices *)
ComponentArray[0,inds_IndexList]:=ComponentArray1[0,inds];
(* Main definition: check that all indices are really there *)
ComponentArray[expr_,inds_IndexList]:=ComponentArray1[expr,keeporder[inds,IndicesOf[Free,BIndex][expr]]];
(* Working function: convert to SplitIndex after constructing the splitrules *)
ComponentArray1[expr_,inds_IndexList]:=SplitIndex[expr,splitrule/@inds];
(* Error message *)
ComponentArray[expr_,x_]:=Throw@Message[ComponentArray::invalid,x,"index specification"];
SetNumberOfArguments[ComponentArray,{1,2}];
Protect[ComponentArray];


(* No additional arguments *)
TableOfComponents[expr_]:=expr;
(* Folding *)
TableOfComponents[expr_,ind_,inds__]:=TableOfComponents[TableOfComponents[expr,ind],inds];
(* Transfer execution flow to ComponentArray *)
TableOfComponents[expr_,inds_IndexList]:=ComponentArray[expr,inds];
TableOfComponents[expr_,f_IndicesOf]:=ComponentArray[expr,f[expr]];
(* Exceptional case with c-indices: return expr instead of {expr} *)
TableOfComponents[expr_,{i_Integer,basis_}]:=expr;
(* Explicit ab-index: ensure it is a b-index using ToBasis *)
TableOfComponents[expr_,{a_?AIndexQ,basis_,range___}]:=ComponentArray[ToBasis[basis][expr,Intersection[IndexList[a],FindFreeIndices[expr]]],IndexList[{a,basis,range}]];
(* A basis: first convert all free a-indices of the corresponding vbundle into that basis *)
TableOfComponents[expr_,basis_?BasisQ]:=ComponentArray[ToBasis[basis][expr,IndicesOf[AIndex,Free,VBundleOfBasis[basis]][expr]],IndicesOf[basis,Free]];
(* Other cases: throw error *)
TableOfComponents[expr_,x_]:=Throw@Message[TableOfComponents::invalid,x,"basis or basis-index specification"];
SetNumberOfArguments[TableOfComponents,{1,Infinity}];
Protect[TableOfComponents];


ThreadComponentArray[comps_List?ArrayQ,values_List?ArrayQ,rule_:Rule]:=If[
Dimensions[comps]===Dimensions[values],
Module[{localrule},ThreadArray[localrule[comps,values]]/.localrule->rule],Throw@Message[ThreadComponentArray::error,"Incompatible dimensions of arrays."]
];


ThreadComponentArray[comps_List,function_Function,rule_:Rule]:=Module[{localrule},Map[localrule[#,function]&,comps,{ArrayDepth[comps]}]/.localrule->rule];


ThreadComponentArray[comps_List,RHS_,rule_:Rule]:=ThreadComponentArray[comps,ComponentArray[RHS],rule];
ThreadComponentArray[LHS_,RHS_,rule_:Rule]:=ThreadComponentArray[ComponentArray[LHS],RHS,rule];


SetNumberOfArguments[ThreadComponentArray,{2,3}];
Protect[ThreadComponentArray];


(* All indices (frees and dummies) in the same basis *)
CTensorComponentArray[expr_,basis_?BasisQ]:=ComponentArray[TraceBasisDummy[ToBasis[basis][expr]]];
(* Different indices in different bases. No consistency check *)
CTensorComponentArray[expr_,finds_List,dbases_]:=ComponentArray[TraceBasisDummy[FoldToBasis[expr,finds,dbases]]];


Unprotect[IndexCoefficient];
IndexCoefficient[expr_,ba_BasisArray]:=Coefficient[expr,Apply[ba,IndicesOf[Free][expr]]];
IndexCoefficient[expr_,basis_?BasisQ]:=IndexCoefficient[expr,BasisArray[basis]];
Protect[IndexCoefficient];


(************************* 4. CTensor ************************)


$CVSimplify=Simplify;


$xCobaCache = True;
$xCobaCacheVerbose=False;


SetAttributes[xCobaCache,HoldAll];
SetAttributes[{ClearxCobaCache,xCobaCacheTable},HoldFirst];


xCobaCache[lhs:head_[___],rhs_]:=xCobaCache[lhs,rhs,head,$CVSimplify];
xCobaCache[lhs_,rhs_,name_]:=xCobaCache[lhs,rhs,name,$CVSimplify];
xCobaCache[lhs_,rhs_,name_,simp_]:=Module[{cache=xCobaCacheTable[lhs,name],compute,time,messages={},printQ=$xCobaCacheVerbose&&name=!=Null},
If[Head[cache]=!=xCobaCacheTable,
(* There is cached information. Return it *)
If[printQ,Print["    ",name,": Using cache"]];
cache,
(* There is no cached information. Compute rhs and store it *)
If[printQ,Print["    ",name,": Caching"]];
time=AbsoluteTime[];
compute=Catch[rhs];messages=$MessageList;
If[printQ&&FreeQ[compute,$Failed|$Aborted],Print["      Computed cache value in ",AbsoluteTime[]-time," seconds"]];
time=AbsoluteTime[];
If[simp=!=Identity,
compute=Catch[simp[compute]];messages=Join[messages,$MessageList];
If[printQ&&FreeQ[compute,$Failed|$Aborted],Print["      Applied ",simp," in ",AbsoluteTime[]-time," seconds"]]
];
(* Some sanity check. Don't cache if something went wrong *)
If[$xCobaCache&&FreeQ[compute,$Failed|$Aborted]&&messages==={},
xCobaCacheTable[lhs,name]=compute;
If[printQ,Print["      Cached ",ByteCount[compute]," bytes"]];
];
compute
]
];


ClearxCobaCache[lhs_,name_]:=Unset[xCobaCacheTable[lhs,name]];
ClearxCobaCache[del:(_Integer|_List)]:=(DownValues[xCobaCacheTable]=Delete[DownValues[xCobaCacheTable],del];);
ClearxCobaCache[All]:=Clear[xCobaCacheTable];


$xCobaCacheLargeComponentSize=200;
PrintxCobaCache[]:=TableForm[PrintxCobaCache1/@DownValues[xCobaCacheTable]/.ctensor_CTensor:>RawBoxes[CTensorBoxes[ctensor,$xCobaCacheLargeComponentSize]],TableHeadings->{Automatic,None}];


PrintxCobaCache1[_[xCobaCacheTable[expr:"ReportCompute"[object_,__],name_]]:>value_List]:={"ReportCompute"[object],{Skeleton[Length[value]]}};
PrintxCobaCache1[_[xCobaCacheTable[ChartsOf[array_],name_]]:>value_]:={"ChartsOf"[Shallow[array,1]],Short[HoldForm[value],1]}
PrintxCobaCache1[_[xCobaCacheTable[expr_,name_]]:>value_]:={Short[HoldForm[expr],1],Short[HoldForm[value],1]}


CTensorArray[CTensor[array_,bases_,addweight_]]:=array;
CTensorBases[CTensor[array_,bases_,addweight_]]:=bases;
CTensorRank[CTensor[array_,bases_List,addweight_]]:=Length[bases];


CTensorQ[CTensor[array_,bases_,addweight_]]:=ArrayQ[array]&&ListQ[bases]&&Length[bases]===ArrayDepth[array];
CTensorQ[ctensor_CTensor,rank_Integer]:=CTensorQ[ctensor]&&CTensorRank[ctensor]===rank;
CTensorQ[ctensor_CTensor,bases_]:=CTensorQ[ctensor]&&CTensorBases[ctensor]===bases;
CTensorQ[ctensor_CTensor,rb_,weight_]:=CTensorQ[ctensor,rb]&&WeightOfTensor[ctensor]===weight;
CTensorQ[__]:=False;


xTensorQ[CTensor[scalar_,{},addweight_]]^:=True;
xTensorQ[CTensor[array_?ArrayQ,bases_List,addweight_]]^:=True;


SlotsOfTensor[CTensor[array_,bases_List,addweight_]]^:=SignedVBundleOfBasis/@bases;


deleteDuplicatesNoZero[array_]:=With[{list=DeleteDuplicates[ArrayElements[array]]},
If[MemberQ[list,0]&&Length[list]>1,
DeleteCases[list,0],
list
]
]


ArrayElements[array_List]:=Flatten[array];
(* If the default value was 0 we could do array["NonzeroValues"] *)
ArrayElements[array_SparseArray]:=Flatten[array];
(* This is assuming SymmetrizedArray type *)
ArrayElements[array_StructuredArray]:=array["Data"][[2,All,2]];
ArrayElements[array_]:=Flatten[{array}];


ArrayWeight[array_]:=If[Length[#]===1,
First[#],
Throw[Message[WeightOfTensor::error,"Inhomogeneous weight found."]]]&@DeleteDuplicates[Expand/@WeightOf/@deleteDuplicatesNoZero[array]];


WeightOfTensor[CTensor[array_,bases_List,addweight_]]^:=ArrayWeight[array]+addweight;


ArrayGrade[array_,prod_]:=If[Length[#]===1,
First[#],
Throw[Message[Grade::error,"Inhomogeneous grade found."]]]&@DeleteDuplicates[Expand/@(Grade[#,prod]&/@deleteDuplicatesNoZero[array])];


GradeOfTensor[CTensor[array_,bases_List,addweight_],prod_]^:=ArrayGrade[array,prod];


CTensor[array_,bases_List]:=CTensor[array,bases,0];


SymmetryGroupOfTensor[CTensor[scalar_,{},addweight_]]^:=StrongGenSet[{},GenSet[]];
SymmetryGroupOfTensor[CTensor[array_?ArrayQ,bases_List,addweight_]]^:=If[System`$VersionNumber>8.5,
With[{sym=TensorSymmetry[array]},
If[GroupTheory`Symmetries`SymmetryCompatibleQ[bases,sym],
xAct`xPerm`Private`MathToxPermSym[sym],
StrongGenSet[{},GenSet[]]
]
],
StrongGenSet[{},GenSet[]]
];


Dagger[CTensor[array_,bases_List,addweight_]]^:=CTensor[Dagger[array],Dagger/@bases,Dagger[addweight]];


DependenciesOfTensor[CTensor[array_,bases_List,addweight_]]^:=xAct`xTensor`Private`SortDependencies[DeleteDuplicates@Join[Flatten[DependenciesOf/@DeleteDuplicates[Flatten[{array}]]],Flatten[DependenciesOfBasis/@bases],DependenciesOfWeight[addweight]]];


DependenciesOfWeight[0]:={};
DependenciesOfWeight[addweight_Plus]:=Join@@(DependenciesOfWeight/@List@@addweight);
DependenciesOfWeight[Optional[_Integer]basis_]:=DependenciesOfBasis[basis];


CTensor::nobd="Incompatible number of bases `1` and array depth `2`.";
CTensor[array_?ArrayQ,bases_List,addweight_]:=With[{nb=Length[bases],ad=ArrayDepth[array]},
$Failed/;(nb=!=ad&&Throw[Message[CTensor::nobd,nb,ad]])
];


Expand[CTensor[expr_,rest__]]^:=CTensor[Expand[expr],rest];
Together[CTensor[expr_,rest__]]^:=CTensor[Together[expr],rest];


CTensor[CTensor[scalar_,{},addweight1_][],{},addweight2_]:=CTensor[scalar,{},Expand[addweight1+addweight2]];


CTensor/: MultiplyHead[k_,CTensor[array_,bases_List,addweight_]]:=CTensor[k array,bases,addweight];
CTensor/:xAct`xTensor`Private`TensorPlus[left___,t1_CTensor,middle___,t2_CTensor,right___]:=xAct`xTensor`Private`TensorPlus[left,t1+t2,middle,right];
CTensor/:xAct`xTensor`Private`TensorPlus[t_CTensor]:=t;


CTensor[x_,{},0][]:=x;


CTensor/:Power[CTensor[scalar_,{},addweight_][],n_]:=CTensor[Power[scalar,n],{},n addweight][];
CTensor/:Sqrt[CTensor[scalar_,{},addweight_][]]:=CTensor[Sqrt[scalar],{},Expand[addweight/2]][];


CTensor::nobi="Incompatible number of bases `1` and indices `2`.";
CTensor[array_?ArrayQ,bases_List,addweight_][inds___?GIndexQ]:=With[{nb=Length[bases],ni=Length[{inds}]},
$Failed/;(nb=!=ni&&Throw[Message[CTensor::nobi,nb,ni]])
];


Expand[ctensor_CTensor[inds__]]^:=Expand[ctensor][inds];
Together[ctensor_CTensor[inds__]]^:=Together[ctensor][inds];


ExplicitZeroQ[0]:=True;
ExplicitZeroQ[0.]:=True;
ExplicitZeroQ[_]:=False;


ZeroArrayQ[array_]:=ArrayQ[array,_,ExplicitZeroQ];


CTensor[0,{},addweight_]:=Zero;
CTensor[_?ZeroArrayQ,bases_List,addweight_]:=Zero;


VBundleOfMetric::novbs="Incompatible vbundles for the bases of a metric.";
VBundleOfMetric[CTensor[array_?MatrixQ,bases_List,addweight_]]^:=If[Length[#]===1,First[#],Throw[Message[VBundleOfMetric::novbs,#]]]&@Union[VBundleOfBasis/@bases];


WeightBox[boxes_,weight_Plus]:=Fold[WeightBox,boxes,List@@weight];
WeightBox[boxes_,basis_?BasisQ]:=OverscriptBox[boxes,ColorString["~",BasisColor[basis]]];
WeightBox[boxes_,-basis_?BasisQ]:=UnderscriptBox[boxes,ColorString["~",BasisColor[basis]]];
WeightBox[boxes_,n_Integer?Positive basis_]:=Nest[WeightBox[#,basis]&,boxes,n];
WeightBox[boxes_,n_Integer?Negative basis_]:=Nest[WeightBox[#,-basis]&,boxes,-n];
WeightBox[boxes_,0]:=boxes;
WeightBox[boxes_,basis_]:=(Message[CTensor::unknown,"basis",basis];boxes);


$MaxNumberOfRowsInCTensorFormatting=4;
$MaxNumberOfColumnsInCTensorFormatting=4;
RuleColumns[rules_]:=With[{l=Length[rules],r=$MaxNumberOfRowsInCTensorFormatting,c=$MaxNumberOfColumnsInCTensorFormatting},
If[l<=r,
rules,
Transpose@Partition[If[l<=r c,rules,Append[Take[rules,r c-1],"More rules"]],r,r,1,""]
]
];


AddToolTip[array_List,size_]:=MapIndexed[Tooltip[compf[#1,size],Rule[#2,#1]]&,array,{ArrayDepth[array]}];
AddToolTip[array_SparseArray,size_]:=RuleColumns@ArrayRules[array]/.Rule[lhs_,rhs_]:>Rule[lhs,Tooltip[compf[rhs,size]]];
AddToolTip[array_StructuredArray/;array["Structure"]==="SymmetrizedArray",size_]:=RuleColumns@SymmetrizedArrayRules[array]/.Rule[lhs_,rhs_]:>Rule[lhs,Tooltip[compf[rhs,size]]]
AddToolTip[array_]:=array;


$LargeComponentSize=1000;
$LargeComponentString="\[GrayCircle]";
compf[expr_,size_]:=If[ByteCount[expr]<size,expr,$LargeComponentString];
PrintAs[CTensor[scalar_,{},addweight_]]^:=WeightBox[MakeBoxes[scalar,StandardForm],addweight];
PrintAs[ctensor_CTensor]^:=CTensorBoxes[ctensor,$LargeComponentSize];
$CTensorRepresentation="ColorBasisForm";
CTensorBoxes[CTensor[array_,bases_List,addweight_],size_]:=WeightBox[
Which[
(*ZeroArrayQ[Unevaluated[array]],
	StyleBox["0",Bold],*)
!ArrayQ[Unevaluated[array]],
	MakeBoxes[CTensor[array,bases,addweight],StandardForm],
True,
	With[{miarray=AddToolTip[array,size]},
	Switch[$CTensorRepresentation,
	"ColorBasisForm",
		StyleBox[ArrayBox[miarray,BasisColor/@xAct`xPerm`Private`nosign/@bases],Small],
	"MatrixForm",
		MakeBoxes[Style[MatrixForm[miarray],Small],StandardForm]
	]
	]
],
addweight
];


ArrayBox[list_?VectorQ,{color_}]:=GridBox[List/@(MakeBoxes[#,StandardForm]&/@list),AutoDelete->False,GridBoxDividers->{"ColumnsIndexed"->{1->color,2->color}}];
ArrayBoxRecurse[matrix_?MatrixQ,{color1_,color2_}]:=With[{dims=Dimensions[matrix]},GridBox[matrix,AutoDelete->False,GridBoxDividers->{"ColumnsIndexed"->{1->color1,dims[[2]]+1->color1},"RowsIndexed"->{1->color2,dims[[1]]+1->color2}}]];
ArrayBox[matrix_?MatrixQ,{color1_,color2_}]:=With[{dims=Dimensions[matrix]},GridBox[Map[MakeBoxes[#,StandardForm]&,Normal@matrix,{2}],AutoDelete->False,GridBoxDividers->{"ColumnsIndexed"->{1->color1,dims[[2]]+1->color1},"RowsIndexed"->{1->color2,dims[[1]]+1->color2}}]];
ArrayBox[array_?ArrayQ,colors_List]:=With[{colors2=Take[colors,2],colorsrest=Drop[colors,2]},
ArrayBoxRecurse[Map[ArrayBox[#,colorsrest]&,array,{2}],colors2]
];
(* TODO: This is not using the colors and it should. How? *)
ArrayBox[Rule[lhs_,rhs_],colors_]:=RowBox[{MakeBoxes[lhs,StandardForm],"\[Rule]",MakeBoxes[rhs,StandardForm]}];
ArrayBox[x_,colors_]:=x;
ColorBasisForm[array_?ArrayQ,colors_List]:=RawBoxes[ArrayBox[array,colors]]


moveto[n_,n_]:={1};
moveto[original_Integer,final_Integer]:=If[original<final,
Join[Range[original-1],{final},Range[original,final-1]],
Join[Range[final-1],Range[final+1,original],{final}]
];


CTensorTranspose[Zero,perm_]:=Zero;
CTensorTranspose[ctensor_CTensor,permlist_List]:=CTensorTranspose[ctensor,Images[permlist]];
CTensorTranspose[ctensor_CTensor,System`Cycles[cyclist_]]:=CTensorTranspose[ctensor,xAct`xPerm`Cycles@@cyclist];
CTensorTranspose[CTensor[array_?ArrayQ,bases_List,addweight_],perm_:(Images[{2,1}])]:=With[{images=TranslatePerm[perm,Images]},
CTensor[Transpose[array,First[images]],PermuteList[bases,images],addweight]
];


ToCCanonical[ctensor:CTensor[scalar_,{},addweight_][]]:=ctensor;
ToCCanonical[CTensor[array_?ArrayQ,bases_List,addweight_][inds__]]:=With[{sortinds=IndexSort[IndexList[inds]]},
With[{images=TranslatePerm[PermutationFromTo[IndexList[inds],sortinds],Images]},
If[sortinds===IndexList[inds],
CTensor[array,bases,addweight][inds],
CTensor[Transpose[array,First[images]],PermuteList[bases,images],addweight]@@sortinds
]
]
];
ToCCanonical[expr_]:=expr/.ctensor:(_CTensor[__]):>ToCCanonical[ctensor];


CTensor/:Times[CTensor[scalar_,{},addweight1_][],CTensor[array_,bases_List,addweight2_][inds___
]]:=CTensor[scalar array,bases,addweight1+addweight2][inds];


CTensor/:Times[x_,CTensor[array_, bases_List,addweight_][inds___]]:=CTensor[x array,bases,addweight][inds] /;( AtomQ[x]||ScalarQ[x]) &&Head[x]=!=CTensor;


TransposeAs[CTensor[array_?ArrayQ,bases_List,addweight_][inds1__],{inds2__}]:=With[{perm=Ordering[{inds2}][[Ordering@Ordering[{inds1}]]]},
CTensor[Transpose[array,perm],Part[bases,Ordering@perm],addweight][inds2]
]/;Sort[{inds1}]===Sort[{inds2}];


CTensor/:CTensor[array1_?ArrayQ,bases_List,addweight_][inds__]+CTensor[array2_?ArrayQ,bases_List,addweight_][inds__]:=CTensor[array1+array2,bases,addweight][inds];


CTensor/:CTensor[array1_?ArrayQ,bases1_List,addweight_][inds1__]+CTensor[array2_?ArrayQ,bases2_List,addweight_][inds2__]:=CTensor[array1,bases1,addweight][inds1]+TransposeAs[CTensor[array2,bases2,addweight][inds2],{inds1}]/;Sort[{inds1}]===Sort[{inds2}]&&Sort[bases1]===Sort[bases2]&&!OrderedQ[{{inds2},{inds1}}];


CTensor/:CTensor[scalar1_,{},addweight_]+CTensor[scalar2_,{},addweight_]:=CTensor[scalar1+scalar2,{},addweight];


CTensor/:Times[CTensor[scalar_,{},addweight1_],CTensor[array_,bases_List,addweight2_]]:=CTensor[scalar array,bases,addweight1+addweight2];
CTensor/: x_ CTensor[array_,bases_List,addweight_]:=CTensor[x array,bases,addweight]/; (AtomQ[x]||ScalarQ[x])&& Head[x]=!=CTensor;


CTensor/:CTensor[array1_?ArrayQ,bases_List,addweight_]+CTensor[array2_?ArrayQ,bases_List,addweight_]:=CTensor[array1+array2,bases,addweight];
CTensor/:t1_CTensor+t2_CTensor:=With[{res=t1+ToCTensor[t2,CTensorBases[t1]]},res/;Head[res]===CTensor];
CTensor/: CTensor[scalar1_,{},addweight_]+CTensor[scalar2_,{},addweight_]:=CTensor[scalar1+scalar2,{},addweight];


Unprotect[Zero];
Zero/:Plus[Zero,x__]:=Plus[x];
Zero/:Times[Zero,__]:=Zero;
Protect[Zero];


(computation:Determinant[CTensor[matrix_?SquareMatrixQ,{basis1_,basis2_},addweight_]])^:=xCobaCache[
computation,
CTensor[Det[matrix],{},Expand[-basis1-basis2+Length[matrix]addweight]]
];


CTensor/:Determinant[ctensor:CTensor[_?SquareMatrixQ,_List,_],basis_?BasisQ]:=Determinant[ctensor,{basis,basis}];


jac[newbasis_?BasisQ,-oldbasis_?BasisQ][]:=Jacobian[newbasis,oldbasis][];
jac[newbasis_?BasisQ,oldbasis_?BasisQ][]:=1/Jacobian[newbasis,oldbasis][];
CTensor/:(computation:Determinant[CTensor[matrix_?SquareMatrixQ,{basis1_,basis2_},addweight_],{newbasis1_?BasisQ,newbasis2_?BasisQ}]):=xCobaCache[
computation,
CTensor[Det[matrix]jac[newbasis1,basis1][]jac[newbasis2,basis2][],{},Expand[-basis1-basis2+Length[matrix]addweight]]
];


(computation:Inv[ctensor:CTensor[matrix_?MatrixQ,{basis1_,basis2_},addweight_]])^:=xCobaCache[
computation,
CTensor[Inverse[matrix],{-basis2,-basis1},-addweight]
];


Dual[ctensor:CTensor[_?MatrixQ,_,_]]:=Inv[CTensorTranspose[ctensor]];


SignDetOfMetric::num="Cannot determine sign of determinant of matrix `1`. Assuming +1.";
SignatureOfMetric::num="Cannot determine signature of matrix `1`. Assuming Euclidean signature.";


SignDetOfMetric[CTensor[matrix_?MatrixQ,{-basis_,-basis_},addweight_]]^:=With[{sign=Sign[Det[matrix]]},
If[NumericQ[sign],
sign,
Message[SignDetOfMetric::num,matrix];
1
]
];
SignatureOfMetric[CTensor[matrix_?MatrixQ,{-basis_,-basis_},addweight_]]^:=With[{evs=TimeConstrained[Eigenvalues[matrix],1]},
If[VectorQ[evs,NumericQ],
{Count[evs,_?Positive],Count[evs,_?Negative],Count[evs,_?PossibleZeroQ]},
Message[SignatureOfMetric::num,matrix];
{Length[matrix],0,0}
]
];


(computation:epsilon[metric:CTensor[matrix_?MatrixQ,{-basis_,-basis_},addweight_]])^:=xCobaCache[
computation,
With[{dim=Length[matrix]},
CTensor[Sqrt[SignDetOfMetric[metric]Det[matrix]]LeviCivitaTensor[dim],ConstantArray[-basis,{dim}],addweight]
]
];


(computation:Tetra[metric:CTensor[matrix_?MatrixQ,{-basis_,-basis_},addweight_]])^:=xCobaCache[
computation,
Module[{a,b,c,d,dim=Length[matrix]},
If[dim=!=4,Throw@Message[Tetra::invalid,"dimension for Tetra",dim]];
{a,b,c,d}=GetIndicesOfVBundle[VBundleOfBasis[basis],4];
HeadOfTensor[I/2 epsilonOrientation[metric,basis]epsilon[metric][a,b,c,d]+metric[a,d]metric[b,c]/2-metric[a,c]metric[b,d]/2+metric[a,b]metric[c,d]/2,{a,b,c,d}]
]
];


ChangeWeight[Zero,changeweight_]:=Zero;
ChangeWeight[CTensor[array_,bases_List,addweight_],changeweight_]:=CTensor[array,bases,addweight+changeweight];


(* Basic definition. It includes all cases changing scalar densities *)
ToCTensor[ctensor:CTensor[array_,bases_List,addweight_],bases_List,changeweight_:0]:=ChangeWeight[ctensor,changeweight];


(* Main definition *)
ToCTensor[ctensor:CTensor[array_?ArrayQ,bases_List,addweight_],newbases_List,changeweight_:0]:=ChangeWeight[
Fold[ToCTensorOneSlot,ctensor,NeededChanges[bases,newbases]],
changeweight
]/;(Length[bases]===Length[newbases]||Throw[Message[ToCTensor::error,"Inconsistent number of bases."]]);


(* Extension on sums *)
ToCTensor[sum_Plus,rest__]:=ToCTensor[#,rest]&/@sum;


(* Decide changes to perform *)
NeededChanges[bases_List,newbases_List]:=MapThread[{#1,CTensorChangeBasis[#2,#3]}&,{Range[Length[bases]],bases,newbases},1];


(* Give the actual change, as a CTensor matrix acting FROM THE LEFT. This can be used to provide explicit values by hand. Note the convention CTensorChangeBasis[oldbasis, newbasis] *)
(*    Do nothing *)
CTensorChangeBasis[basis_,basis_]:=IdentityCTensor;
(*    Change basis in a covariant slot *)
CTensorChangeBasis[-basis_?BasisQ,-newbasis_?BasisQ]:=ToCTensor[Basis,{-newbasis,basis}];
(*    Change basis in a contravariant slot *)
CTensorChangeBasis[basis_?BasisQ,newbasis_?BasisQ]:=CTensorTranspose[ToCTensor[Basis,{-basis,newbasis}]];
(*    Lower a basis *)
CTensorChangeBasis[basis1_?BasisQ,-basis2_?BasisQ]:=ToCTensor[xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfBasis[basis1],True],{-basis2,-basis1}];
(*    Raise a basis *)
CTensorChangeBasis[-basis1_?BasisQ,basis2_?BasisQ]:=ToCTensor[Inv[xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfBasis[basis1],True]],{basis2,basis1}];


ToCTensorOneSlot[ctensor_,{n_,IdentityCTensor}]:=ctensor;
(computation:ToCTensorOneSlot[CTensor[array_?ArrayQ,bases_List,addweight_],{n_,CTensor[matrix_,{newbasis_,dualbasis_},matrixaddweight_]}]):=xCobaCache[
computation,
CTensor[Transpose[matrix.Transpose[array,moveto[n,1]],moveto[1,n]],ReplacePart[bases,n:>newbasis],addweight+matrixaddweight],
ToCTensorOneSlot
];


ToCTensor[Basis,{-basis1_?BasisQ,basis2_?BasisQ},changeweight_:0]:=CTensor[Outer[Basis,CIndicesOf[-basis1],CIndicesOf[basis2],1,1],{-basis1,basis2},changeweight];
ToCTensor[Basis,{basis1_?BasisQ,-basis2_?BasisQ},changeweight_:0]:=CTensorTranspose[ToCTensor[Basis,{-basis2,basis1}]];


ToCTensor[MultiplyHead[k_,head_],bases_List,changeweight_:0]:=k ToCTensor[head,bases,changeweight];
ToCTensor[tensor_?xTensorQ,bases_List,changeweight_:0]:=CTensor[Outer[tensor,Sequence@@(CIndicesOf/@bases),Sequence@@(1&/@bases)],bases,WeightOfTensor[tensor]+changeweight];


ConsistentBasesQ[bases_List,inds_List]:=And@@MapThread[BCQ,{bases,inds},1];
BCQ[-basis_?BasisQ,-ind_Symbol]:=True;
BCQ[basis_?BasisQ,ind_Symbol]:=True;
BCQ[basis_,{_,basis_}]:=True;
BCQ[_,_]:=False;


$AutomaticCTensorMetricChange=True;


(ctensor:CTensor[array_?ArrayQ,bases:{__},addweight_])[inds__?GIndexQ]:=ToCTensor[ctensor,IndexList[inds],0][inds]/;$AutomaticCTensorMetricChange&&!ConsistentBasesQ[bases,{inds}];


ToCTensor[ctensor:CTensor[array_?ArrayQ,bases_List,addweight_],inds_IndexList ,changeweight_:0]:=ChangeWeight[
(*Print["Computing NeededChanges"];*)
Fold[ToCTensorOneSlot,ctensor,NeededChanges[bases,inds]],
changeweight
]/;(Length[bases]===Length[inds]||Throw[Message[ToCTensor::error,"Inconsistent number of bases and indices."]]);


(* Decide changes to perform *)
NeededChanges[bases_List,inds_IndexList]:=MapThread[{#1,CTensorChangeIndex[#2,#3]}&,{Range[Length[bases]],bases,List@@inds},1];


(* Give the actual change, as a CTensor matrix acting FROM THE LEFT. This can be used to provide explicit values by hand. Note the convention CTensorChangeBasis[oldbasis, newbasis] *)
(*    Do nothing *)
CTensorChangeIndex[-basis_?BasisQ,-ind_Symbol]:=IdentityCTensor;
CTensorChangeIndex[basis_?BasisQ,ind_Symbol]:=IdentityCTensor;
CTensorChangeIndex[basis_,{_,basis_}]:=IdentityCTensor;
(*    Change basis in a covariant slot *)
CTensorChangeIndex[-basis_?BasisQ,{_,-newbasis_?BasisQ}]:=ToCTensor[Basis,{-newbasis,basis}];
(*    Change basis in a contravariant slot *)
CTensorChangeIndex[basis_?BasisQ,{_,newbasis_?BasisQ}]:=CTensorTranspose[ToCTensor[Basis,{-basis,newbasis}]];
(*    Lower a basis, with newbasis information *)
CTensorChangeIndex[basis1_?BasisQ,{_,-basis2_?BasisQ}]:=ToCTensor[xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfBasis[basis1],True],{-basis2,-basis1}];
(*    Raise a basis, with newbasis information *)
CTensorChangeIndex[-basis1_?BasisQ,{_,basis2_?BasisQ}]:=ToCTensor[Inv[xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfBasis[basis1],True]],{basis2,basis1}];
(*    Lower a basis. New basis is decided by the metric *)
CTensorChangeIndex[basis1_?BasisQ,-ind_Symbol]:=Module[{metric=xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfBasis[basis1],True],basis2},
basis2=If[Head[metric]===CTensor,-CTensorBases[metric][[1]],basis1];
ToCTensor[metric,{-basis2,-basis1}]
];
(*    Raise a basis. New basis is decided by the metric *)
CTensorChangeIndex[-basis1_?BasisQ,ind_Symbol]:=Module[{invmetric=Inv[xAct`xTensor`Private`FirstMetricOfVBundle[VBundleOfBasis[basis1],True]],basis2},
basis2=If[Head[invmetric]===CTensor,CTensorBases[invmetric][[1]],basis1];
ToCTensor[invmetric,{basis2,basis1}]
];


Options[SetBasisChange]={CVSimplify:>$CVSimplify};
(* If no chart is specified, then we will not pre-compute Christoffel tensors *)
SetBasisChange[change_,opts:OptionsPattern[]]:=SetBasisChange[change,Null,opts];
(* Compute the inverse, if not provided *)
SetBasisChange[ctensor_CTensor,chart_,opts:OptionsPattern[]]:=Module[{cvsimplify,inverse},
If[!CTensorQ[ctensor,2,0],Return[$Failed]];
cvsimplify=OptionValue[CVSimplify];
inverse=Check[Inv[ctensor],$Failed];
If[inverse=!=$Failed,
(* Both the original ctensor and its inverse use the same chart *)
SetBasisChange[{ctensor,inverse},chart,opts],
$Failed
]
];
(* General case *)
SetBasisChange[{direct_CTensor,inverse_CTensor},chart_,opts:OptionsPattern[]]:=Module[{},
(* Check consistency. We do not try to check that the CTensor objects are inverse of each other because that could take much time *)
If[Reverse[CTensorBases[direct]]=!=-CTensorBases[inverse],
Throw[Message[SetBasisChange::error,"Incompatible changes of basis provided."]]
];
SetBasisChange1[inverse,chart,False,opts];
SetBasisChange1[direct,chart,True,opts];
];
(* Standard Jacobian convention: {upbasis, -downbasis}. Transpose to our convention *)
SetBasisChange1[HoldPattern[CTensor[matrix_?MatrixQ,{upbasis_?BasisQ,-downbasis_?BasisQ},0]],chart_,more_,opts:OptionsPattern[]]:=SetBasisChange1[CTensor[Transpose[matrix],{-downbasis,upbasis},0],chart,more,opts];
(* Our standard convention, following the fundamental delta[-a, b] *)
SetBasisChange1[HoldPattern[CTensor[matrix_?MatrixQ,{-downbasis_?BasisQ,upbasis_?BasisQ},0]],chart_,more_,opts:OptionsPattern[]]:=Module[{cvsimplify,prot,jacobian},

cvsimplify=OptionValue[SetBasisChange,{opts},CVSimplify];

(****** Algebra part ******)

prot=Unprotect[Basis];
(* Store CTensors *)
Basis/:ToCTensor[Basis,{-downbasis,upbasis},changeweight_:0]:=CTensor[matrix,{-downbasis,upbasis},changeweight];
(* Store TensorValues, in Rule mode *)
ThreadComponentArray[ComponentArray[Basis,{-downbasis,upbasis}],matrix,ComponentValue];
Protect[Evaluate[prot]];

(* Store the value of the Jacobian determinant *)
If[more,
jacobian=Check[cvsimplify[Det[matrix]],$Failed];
If[jacobian=!=$Failed,
jacobian=CTensor[jacobian,{},downbasis-upbasis][];
With[{jacob=Jacobian[downbasis,upbasis][]},
If[MatchQ[jacob,Power[_,-1]],
ComponentValue[1/jacob,1/jacobian],
ComponentValue[jacob,jacobian]
]
]
]
];

(****** Calculus part ******)

If[more&&ChartQ[chart],
SetBasisChristoffel[upbasis,chart,opts];
SetBasisChristoffel[downbasis,chart,opts];
If[upbasis=!=chart&&downbasis=!=chart,
storeChristoffel[upbasis,downbasis,VBundleOfBasis[upbasis]]
]
]

];


SetBasisChristoffel[chart_,chart_,opts:OptionsPattern[]]:=Null;
SetBasisChristoffel[basis_,chart_,opts:OptionsPattern[]]:=Module[{},
MetricCompute[CTensor[IdentityMatrix[Length[CNumbersOf[chart]]],{-basis,-basis},0],chart,"Christoffel"[1,-1,-1],opts];
storeChristoffel[PDOfBasis[basis],PDOfBasis[chart],VBundleOfBasis[basis]];
];


storeChristoffel[cd1_,cd2_,vb_]:=Module[{i1,i2,i3,chr,sign},
{i1,i2,i3}=GetIndicesOfVBundle[vb,3];
chr=HeadOfTensor[Christoffel[cd1,cd2][i1,-i2,-i3],{i1,-i2,-i3}];
If[Head[chr]===MultiplyHead,
sign=First[chr];
chr=Last[chr],
sign=1
];
With[{
echr=chr,
etor=Torsion[cd1],
cchr=sign xCobaCacheTable[Christoffel[cd1,cd2],Christoffel],
ctor=xCobaCacheTable[Torsion[cd1],Torsion]
},
xTagSetDelayed[{echr,ToCTensor[echr,bases_List,addweight_:0]},ToCTensor[cchr,bases,addweight]];
xTagSetDelayed[{etor,ToCTensor[etor,bases_List,addweight_:0]},ToCTensor[ctor,bases,addweight]];
]
];


CTensorContract[Zero,_]:=Zero;
CTensorContract[CTensor[array_,bases_,addweight_],{n1_Integer,n2_Integer}]:=CTensor[ArrayContract[array,{n1,n2},bases[[n1]],-bases[[n2]]],Delete[bases,{{n1},{n2}}],addweight];


(* Same basis *)
ArrayContract[array_,{n1_,n2_},basis_,basis_]:=arrayContract[array,{n1,n2}];
(* Different bases. Introduce explicit change of basis *)
ArrayContract[array_,{n1_,n2_},basis1_?BasisQ,mbasis2_?BasisQ]:=arrayContract[array,{n1,n2},CTensorArray@ToCTensor[Basis,{mbasis2,-basis1}]];
ArrayContract[array_,{n1_,n2_},-basis1_?BasisQ,-mbasis2_?BasisQ]:=arrayContract[array,{n1,n2},CTensorArray@ToCTensor[Basis,{-mbasis2,basis1}]];
(* Other cases *)
ArrayContract[__]:=$Failed;
(* Actual contraction *)
arrayContract[array_,{n1_,n2_}]:=Tr[Transpose[Transpose[array,moveto[n1,1]],moveto[n2,2]],Plus,2];
arrayContract[array_,{n1_,n2_},change_]:=Tr[Transpose[change.Transpose[array,moveto[n1,1]],moveto[n2,2]],Plus,2];


CTensorContract[Zero,_,_,_]:=Zero;
CTensorContract[_,Zero,_,_]:=Zero;
CTensorContract[CTensor[array1_,bases1_,addweight1_],CTensor[array2_,bases2_,addweight2_],{n1_Integer,n2_Integer},prod_]:=CTensor[TwoArrayContract[array1,array2,{n1,Length[bases1],n2},prod,bases1[[n1]],-bases2[[n2]]],Join[Delete[bases1,{n1}],Delete[bases2,{n2}]],Expand[addweight1+addweight2]];


(* Same basis *)
TwoArrayContract[array1_,array2_,{n1_,r1_,n2_},prod_,basis_,basis_]:=twoarrayContract[array1,array2,{n1,r1,n2},prod];(* Different bases. Introduce explicit change of basis *)
TwoArrayContract[array1_,array2_,{n1_,r1_,n2_},prod_,basis1_?BasisQ,mbasis2_?BasisQ]:=
twoarrayContract[array1,array2,{n1,r1,n2},prod,CTensorArray@ToCTensor[Basis,{-basis1,mbasis2}]]/;VBundleOfBasis[basis1]===VBundleOfBasis[mbasis2];
TwoArrayContract[array1_,array2_,{n1_,r1_,n2_},prod_,-basis1_?BasisQ,-mbasis2_?BasisQ]:=
twoarrayContract[array1,array2,{n1,r1,n2},prod,Transpose[CTensorArray@ToCTensor[Basis,{-mbasis2,basis1}]]]/;VBundleOfBasis[basis1]===VBundleOfBasis[mbasis2];
(* Other cases *)
TwoArrayContract[__]:=$Failed;
(* Actual contraction. Times product *)
twoarrayContract[array1_,array2_,{n1_,r1_,n2_},Times]:=Transpose[array1,moveto[n1,r1]].Transpose[array2,moveto[n2,1]];
twoarrayContract[array1_,array2_,{n1_,r1_,n2_},Times,change_]:=Transpose[array1,moveto[n1,r1]].change.Transpose[array2,moveto[n2,1]];
(* Actual contraction. Other products *)
twoarrayContract[array1_,array2_,{n1_,r1_,n2_},prod_]:=Inner[prod,Transpose[array1,moveto[n1,r1]],Transpose[array2,moveto[n2,1]],Plus];
twoarrayContract[array1_,array2_,{n1_,r1_,n2_},prod_,change_]:=Inner[prod,Transpose[array1,moveto[n1,r1]].change,Transpose[array2,moveto[n2,1]],Plus];


CTensorContractMatrix[ctensor_CTensor,{cmatrix_CTensor,n_Integer},prod_]:=CTensorTranspose[CTensorContract[ctensor,cmatrix,{n,1},prod],moveto[CTensorRank[ctensor],n]];
CTensorContractMatrices[ctensor_CTensor,cmatrices:{___CTensor},ns_List,prod_]:=Fold[CTensorContractMatrix[##,prod]&,ctensor,Transpose[{cmatrices,ns}]];


CTensor/:ctensor_CTensor[left___,a_,center___,-a_,right___]:=Module[{res},
res=CTensorContract[ctensor,{Length[{left,a}],Length[{left,a,center,-a}]}];
res[left,center,right]/;FreeQ[res,$Failed]
];


CTensor/:ctensor_CTensor[left___,-a_,center___,a_,right___]:=Module[{res},
res=CTensorContract[ctensor,{Length[{left,-a}],Length[{left,-a,center,a}]}];
res[left,center,right]/;FreeQ[res,$Failed]
];


CTensor/:ctensor1_CTensor[left1___,a_,right1___]ctensor2_CTensor[left2___,-a_,right2___]:=Module[{n1=Length[{left1,a}],n2=Length[{left2,-a}],res},
res=CTensorContract[ctensor1,ctensor2,{n1,n2},Times];
res[left1,right1,left2,right2]/;FreeQ[res,$Failed]
];


ChristoffelContract[Zero,_,_]:=Zero;
ChristoffelContract[_,Zero,_]:=Zero;
ChristoffelContract[ctensor:CTensor[_,bases_,_],christoffel:CTensor[_,{chrbasis_,dbasis_,_},_],{n_}]:=
With[{basis=bases[[n]],rank=Length[bases]},
If[VBundleOfBasis[basis]=!=VBundleOfBasis[chrbasis],
Zero,
(* Ensure we do not change the bases of the differentiated tensor *)
ToCTensor[If[UpIndexQ[basis],
CTensorTranspose[CTensorContract[ctensor,CTensorTranspose[christoffel,{2,3,1}],{n,1},Times],moveto[rank,n]],
-CTensorTranspose[CTensorContract[ctensor,CTensorTranspose[christoffel,{1,3,2}],{n,1},Times],moveto[rank,n]]
],
Append[bases,dbasis]
]
]
];


LieDContract[Zero,_,_]:=Zero;
LieDContract[_,Zero,_]:=Zero;
LieDContract[ctensor:CTensor[_,bases_,_],vder:CTensor[_,{vbasis_,_},_],{n_}]:=
With[{basis=bases[[n]],rank=Length[bases]},
If[VBundleOfBasis[basis]=!=VBundleOfBasis[vbasis],
Zero,
(* Ensure we do not change the bases of the differentiated tensor *)
ToCTensor[
If[UpIndexQ[basis],
-CTensorTranspose[CTensorContract[ctensor,vder,{n,2},Times],moveto[rank,n]],
CTensorTranspose[CTensorContract[ctensor,vder,{n,1},Times],moveto[rank,n]]
],
bases,
0
]
]
];


tensorproduct[prod_][left___,scalar_/;!ArrayQ[scalar],right___]:=scalar tensorproduct[prod][left,right];
tensorproduct[prod_][arrays__?ArrayQ]:=Outer[prod,arrays];


CTensorProduct[]:=1;
CTensorProduct[ctensors__CTensor]:=CTensor[tensorproduct[Times]@@#1,Join@@#2,Plus@@#3]&@@Transpose[List@@@{ctensors}];
CTensorProduct[___,Zero,___]:=Zero;


CTensor/:ctensor1_CTensor[inds1__]ctensor2_CTensor[inds2__]:=CTensorProduct[ctensor1,ctensor2][inds1,inds2]/;xAct`xTensor`Private`TakePairs[{inds1,inds2}]==={}


(ctensor:CTensor[array_?ArrayQ,bases_List,addweight_])[left___,{c_Integer,basis_}?CIndexQ,right___]:=With[{n=Length[{left,c}],p=PNumber[c,basis]},
If[bases[[n]]===basis,
(* Basis is correct already *)
CTensorPart[ctensor,n,p][left,right],
(* Perform change of basis *)
CTensorPart[ToCTensor[ctensor,ReplacePart[bases,n:>basis]],n,p][left,right]
]
];


CTensorPart[CTensor[array_,bases_List,addweight_],n_,p_]:=CTensor[Transpose[array,moveto[n,1]][[p]],Delete[bases,{n}],addweight];


ToBasisExpand[expr_]:=expr/.CTensor[array_?ArrayQ,bases_List,0][inds__]:>Total[array (BasisArray@@bases)[inds],Infinity];


FromBasisExpand[expr_,bases_List]:=With[{frees=FindFreeIndices[expr]},CTensor[Coefficient[expr,(BasisArray@@bases)@@frees],bases,0]@@frees];


CTensor[array_?ArrayQ,bases_List,addweight_][left___,{a_,basis_}?BIndexQ,right___]:=With[{n=Length[{left,a}]},Dot[CTensor[#,Delete[bases,{n}],addweight][left,right]&/@Transpose[array,moveto[n,1]],BasisArray[bases[[n]]][{a,basis}]]
];


CTensor/:der_?FirstDerQ[ctensor_CTensor[inds___]]:=xAct`xTensor`Private`ToTensorDerivative1[der,ctensor[inds]];


BasisQ[CBasis[basis_,matrix_]]^:=BasisQ[basis]&&MatrixQ[matrix];


CBasis[-basis_,matrix_]:=-CBasis[basis,Transpose[Inverse[matrix]]];
CBasis[CBasis[basis_,matrix1_],matrix2_]:=CBasis[basis,Simplify[matrix2.matrix1]];


Unprotect[Basis];
Basis/:ToCTensor[Basis,{-CBasis[basis_,matrix_],basis_},0]:=CTensor[Inverse[matrix],{-CBasis[basis,matrix],basis},0];
Basis/:ToCTensor[Basis,{-basis_,CBasis[basis_,matrix_]},0]:=CTensor[matrix,{-basis,CBasis[basis,matrix]},0];
Basis/:ToCTensor[Basis,{-CBasis[basis_,matrix1_],CBasis[basis_,matrix2_]},0]:=CTensor[Inverse[matrix1].matrix2,{-CBasis[basis,matrix1],CBasis[basis,matrix2]},0];
Protect[Basis];


(************************* 4.Values ************************)


Protect[ValID];


(* Basis of an index *)
basisof[{_,basis_}]:=basis;
basisof[expr:(_LI|-_LI)]:=expr;
basisof[_]:=Null;
(* List of basis-configurations equivalent under symmetry to a given basis-configuration. Separate types. Use With to bypass HoldFirst *)
SetAttributes[BasesEquivalents,HoldFirst];
BasesEquivalents[tensor_?xTensorQ[inds___]]:=With[{basis=basisof/@{inds},sym=SymmetryGroupOfTensor[Unevaluated[tensor[inds]]]},BasesEquivalents[basis,sym]];
(* Added for HoldPattern *)
BasesEquivalents[expr_HoldPattern]:=With[{basis=basisof/@First@Apply[List,expr,{1}],sym=SymmetryGroupOfTensor[expr[[1,0]]]},BasesEquivalents[basis,sym]];
BasesEquivalents[expr_]:=With[{symmetry=SymmetryOf[expr]},
With[{basis=basisof/@Last/@symmetry[[3]],sym=symmetry[[4]]},BasesEquivalents[basis,sym]]];
BasesEquivalents[bases_List,tensor_?xTensorQ]:=With[{sym=SymmetryGroupOfTensor[tensor]},BasesEquivalents[bases,sym]];
(* Main definition *)
BasesEquivalents[bases_List,symmetry_StrongGenSet]:=BasesEquivalents[bases,symmetry]=Union@Map[PermuteList[bases,#]&,List@@Dimino[GenSet[symmetry]]];


(* Special notation for covariant derivatives *)
derof[covd_?CovDQ[{_,basis_}]]:=covd[basis];
derof[lied_LieD]:=lied;
derof[paramD_ParamD]:=paramD;
derof[der_]:=Throw@Message[TensorValues::invalid,der,"derivative for TensorValues"];
(* First part of a valID, as a list *)
SetAttributes[TensorDers,HoldFirst];
TensorDers[tensor_?xTensorQ[inds___]]:={tensor};
TensorDers[der_?FirstDerQ[expr_]]:=Append[TensorDers[expr],derof[der]];
TensorDers[expr_]:=Throw@Message[TensorValues::invalid,expr,"expression for TensorValues"];


(* Default definition for initialization *)
TensorValIDs[tensor_?xTensorQ]:=xUpSet[TensorValIDs[tensor],{}];
(* Expression argument: construct valID *)
TensorValIDs[expr_]:={ValID[Sequence@@TensorDers[expr],BasesEquivalents[expr]]};
(* Restrict by giving some derivatives *)
TensorValIDs[tensor_,ders__?FirstDerQ]:=Cases[TensorValIDs[tensor],ValID[tensor,ders,__]];
(* Restrict even more giving a list of bases (not a list of lists of bases) *)
TensorValIDs[tensor_,ders___?FirstDerQ,bases_List]:=Cases[TensorValIDs[tensor],ValID[tensor,ders,{___,bases,___}]];
(* Other *)
SetNumberOfArguments[TensorValIDs,{1,Infinity}];
Protect[TensorValIDs];


DateOfValID[valid_ValID]:=DateOfValID@@valid;
DateOfValID[x__]:=Throw@Message[DateOfValID::unknown,"valID",ValID[x]];


PrependTensorValID[tensor_,valid_ValID]:=xUpSet[TensorValIDs[tensor],prependTVI[TensorValIDs[tensor],valid]];
prependTVI[{valid1___,valid_,valid2___},valid_]:={valid,valid1,valid2};
prependTVI[valids_List,valid_]:=Prepend[valids,valid];


UpdateTensorValIDs[valid_ValID]:=UpdateTensorValIDs@@valid;
UpdateTensorValIDs[tensor_,ders___?FirstDerQ,baseslist:{___List}]:=(PrependTensorValID[tensor,ValID[tensor,ders,baseslist]];Set[DateOfValID[tensor,ders,baseslist],Date[]]);
UpdateTensorValIDs[tensor_,ders___?FirstDerQ,bases_List]:=UpdateTensorValIDs[tensor,ders,BasesEquivalents[bases,tensor]];


(* TensorValues on a ValID: convert to normal notation *)
TensorValues[valid_ValID]:=TensorValues@@valid;
(* Special treatment of Basis *)
TensorValues[Basis[{_,-basis1_?BasisQ},{_,basis2_?BasisQ}]]:=BasisValues[-basis1,basis2];
TensorValues[Basis,{{basis1_,basis2_}}]:=BasisValues[basis1,basis2];
TensorValues[Basis,{{basis1_,basis2_},{basis2_,basis1_}}]:=BasisValues[basis1,basis2];
TensorValues[Basis,ders__,bases_List]:=Throw@Message[TensorValues::error,"TensorValues on derivatives of Basis cannot be defined."];
(* Expression argument *)
TensorValues[tensor_?xTensorQ[___],ders___?FirstDerQ]:=TensorValues[tensor,ders];
TensorValues[der_?FirstDerQ[expr_],ders___?FirstDerQ]:=TensorValues[expr,der,ders];
(* Added for HoldPattern *)
TensorValues[expr_HoldPattern,ders___]:=TensorValues[expr[[1,0]],ders];
(* All known values for a tensor or its derivatives *)
TensorValues[tensor_?xTensorQ,ders___?FirstDerQ]:=JoinFoldedRule@@Apply[TensorValues,TensorValIDs[tensor,ders],1];
(* Initialization case *)
TensorValues[tensor_?xTensorQ,ders___?FirstDerQ,baseslist:{___List}]:=xTagSet[{tensor,TensorValues[tensor,ders,baseslist]},FoldedRule[{},{}]];
(* Single bases list. Derivatives and number of indices not taken into account in symmetries *)
TensorValues[tensor_?xTensorQ,ders___?FirstDerQ,bases_List]:=TensorValues[tensor,ders,BasesEquivalents[bases,tensor]];
(* General expressions. No restriction allowed yet *)
TensorValues[expr_]:=TensorValues[Sequence@@TensorDers[expr],BasesEquivalents[expr]];
(* Other *)
TensorValues[x___]:=Throw@Message[TensorValues::unknown,"input",{x}];
Protect[TensorValues];


JoinFoldedRule[]:=FoldedRule[{},{}];
JoinFoldedRule[frules__FoldedRule]:=Join[frules];


(* Initialization case: upvalue for the second basis *)
BasisValues[-basis1_?BasisQ,basis2_?BasisQ]:=xTagSet[{basis2,BasisValues[-basis1,basis2]},FoldedRule[{},{}]];
(* Fake orderless *)
BasisValues[basis1_?BasisQ,-basis2_?BasisQ]:=BasisValues[-basis2,basis1];
(* Other cases *)
BasisValues[basis1_?BasisQ,basis2_?BasisQ]:=Throw@Message[BasisValues::error,"Cannot change between two up-bases."];
BasisValues[-basis1_?BasisQ,-basis2_?BasisQ]:=Throw@Message[BasisValues::error,"Cannot change between two down-bases."];
SetNumberOfArguments[BasisValues,2];
Protect[BasisValues];


DeleteTensorValues::notfound="TensorValues for bases `1` not found.";
DeleteTensorValues::toomany="Found several TensorValues with bases `1`.";
checkvbasesof[{},bases_List]:=Throw@Message[DeleteTensorValues::notfound,bases];
checkvbasesof[{ValID[_,_,bases_]},_List]:=bases;
checkvbasesof[{__ValID},bases_List]:=Throw@Message[DeleteTensorValues::toomany,bases];


(* Delete all values for a tensor (or its derivatives). Return Null instead of a list of Null *)
DeleteTensorValues[tensor_?xTensorQ,ders___?FirstDerQ]:=(Apply[DeleteTensorValues,TensorValIDs[tensor,ders],1];Null);
(* Specify a full valID *)
DeleteTensorValues[valid_ValID]:=DeleteTensorValues@@valid;
(* Particular case of Basis *)
DeleteTensorValues[Basis,{___,{-basis1_,basis2_},___}]:=DeleteTensorValues[Basis,{-basis1,basis2}];
DeleteTensorValues[Basis,{-basis1_,basis2_}]:=With[{valid=ValID[Basis,Sort[{{-basis1,basis2},{basis2,-basis1}}]]},
If[StateOfValID[valid]===Set,Print["Changing state"];SetToRule[valid]];
xUpDeleteCasesTo[TensorValIDs[Basis],valid];
TagUnset[basis2,BasisValues[-basis1,basis2]];
Print["Deleted values for Basis and bases {",-basis1,",",basis2,"}"];
];
(* General case *)
DeleteTensorValues[tensor_?xTensorQ,ders___?FirstDerQ,baseslist:{___List}]:=With[{valid=ValID[tensor,ders,baseslist]},
If[StateOfValID[valid]===Set,SetToRule[valid]];
xUpDeleteCasesTo[TensorValIDs[tensor],valid];
TagUnset[tensor,TensorValues[tensor,ders,baseslist]];
Print["Deleted values for tensor ",tensor,", derivatives ",{ders}," and bases ",baseslist,"."];];
(* Specify a single bases list *)
DeleteTensorValues[tensor_?xTensorQ,ders___?FirstDerQ,bases_List]:=DeleteTensorValues[tensor,ders,checkvbasesof[TensorValIDs[tensor,ders,bases],bases]];
(* Other *)
DeleteTensorValues[x__]:=Throw@Message[DeleteTensorValues::unknown,"input",{x}];
SetNumberOfArguments[DeleteTensorValues,{1,Infinity}];
Protect[DeleteTensorValues];


(* One argument *)
ComponentValue[list_List]:=Map[ComponentValue,list];
ComponentValue[Rule[comp_,value_]]:=ComponentValue[comp,value];
ComponentValue[foldedrule:FoldedRule[___List,{_[expr_,_],___},___List]]:=With[{bders=TensorDers[expr]},
With[{valid=Sequence@@{Sequence@@bders,BasesEquivalents[expr]},tensor=First[bders]},
xTagSet[{tensor,TensorValues[valid]},DeleteCases[Join[foldedrule,TensorValues[valid]],{}]];
UpdateTensorValIDs[valid]
]];
(* Two arguments *)
ComponentValue[comps_List,ctensor_CTensor]:=ComponentValue[comps,First[ctensor]];
ComponentValue[comps_List,values_]:=ThreadComponentArray[comps,values,ComponentValue];
SetNumberOfArguments[ComponentValue,{1,2}];


checkrules[-expr_]:=-checkrules[expr];
checkrules[expr_]:=With[{rules=TensorValues[expr]},If[Position[rules,HoldPattern[Rule[expr,_]],{2}]=!={},expr/.rules,Null]];


SetAttributes[DoRule,HoldFirst];
DoRule[lhs_,rhs_]:=lhs->rhs;


SetAttributes[{HeldComponentValue,DCV},HoldFirst];
(* Step 0: start by checking what we know about the expr, evaluating it *)
ComponentValue[expr_]:=DCV[expr,expr,1];
HeldComponentValue[expr_]:=DCV[expr,expr,1];
(* Step 1F: it does not evaluate: check for stored values *)
DCV[expr_,expr_,1]:=DCV[expr,checkrules[expr],2];
(* Step 1T: (HeldCV case) it evaluates: no further processing, return new expression *)
DCV[expr_,value_,1]:=DoRule[expr,value];
(* Step 2F: expr does not have a stored value: canonicalize *)
DCV[expr_,Null,2]:=DCV[expr,ToCanonical[expr],3];
(* Step 2T: expr has already a stored value: return that value, no computation has been done *)
DCV[expr_,value_,2]:=DoRule[expr,value];
(* Step 3F: expr is canonical: new independent rule, store and return it *)
DCV[expr_,expr_,3]:=With[{rule=DoRule[expr,expr]},StoreRule[expr,rule,Last];rule];
(* Step 3T: expr changes under canonicalization: new dependent rule, store it and continue *)
DCV[expr_,canon_,3]:=With[{rule=DoRule[expr,canon]},StoreRule[expr,rule,First];DCV[expr,canon,4]];
(* Step 4T: canon is 0: return it *)
DCV[expr_,0,4]:=DoRule[expr,0];
(* Step 4F: canon is not 0: check again for stored values *)
DCV[expr_,canon_,4]:=DCV[expr,canon,checkrules[canon],5];
(* Step 5F: nothing known about canon: return it *)
DCV[expr_,canon_,Null|-Null,5]:=DoRule[expr,canon];
(* Step 5T: known value for canon: return it *)
DCV[expr_,canon_,value_,5]:=DoRule[expr,value];


$CVVerbose=True;
$CVReplace=True;


SetAttributes[StoreRule,HoldFirst];
(* General function. Argument action can be First or Last *)
StoreRule[expr_,rule_,action_]:=With[{bders=TensorDers[expr]},
With[{valid=Sequence@@{Sequence@@bders,BasesEquivalents[expr]},tensor=First[bders]},
xTagSet[{tensor,TensorValues[valid]},
ReplacePart[TensorValues[valid],storeact[action[TensorValues[valid]],rule,tensor,action],action[{1,2}]]];
UpdateTensorValIDs[valid]
]];


(* Particular case on Basis objects *)
StoreRule[Basis[{_,-basis1_},{_,basis2_}],rule_,action_]:=(
xTagSet[{basis2,BasisValues[-basis1,basis2]},
ReplacePart[BasisValues[-basis1,basis2],storeact[action[BasisValues[-basis1,basis2]],rule,Basis,action],action[{1,2}]]];
UpdateTensorValIDs[Basis,Sort[{{-basis1,basis2},{basis2,-basis1}}]]);


(* Driver to the elementary actions *)
storeact[flist_,rule_,tensor_,First]:=(If[$CVVerbose,Print["Added dependent rule ",rule," for tensor ",tensor]];Append[flist,rule]);
storeact[flist_,rule:Rule[_,Null|-Null],tensor_,Last]:=droprule[flist,rule,tensor];
storeact[flist_,rule_,tensor_,Last]:=addrule[flist,rule,tensor];


(* Drop independent rule *)
droprule[{rules1___,rule:_[expr_,_],rules2___},_[expr_,_],tensor_]:=(
If[$CVVerbose,Print["Dropped independent rule ",rule," for tensor ",tensor]];
{rules1,rules2});
droprule[ruleslist_List,rule_,tensor_]:=(
Print["Independent rule ",rule," not found."];
ruleslist);
(* Add independent rule *)
addrule[ruleslist:{___,rule_,___},rule_,tensor_]:=(
If[$CVVerbose,Print["Found again independent rule ",rule," for tensor ",tensor]];
ruleslist);
addrule[{rules1___,_[x1_,x1_],rules2___},rule:_[x1_,_],tensor_]:=(
If[$CVVerbose,Print["Added independent value ",rule," for tensor ",tensor]];
{rules1,rule,rules2});
addrule[{rules1___,old:_[x1_,_],rules2___},rule:_[x1_,x1_],tensor_]:=
(If[$CVVerbose,Print["Kept old independent rule ",old," vs. new ",rule," for tensor ",tensor]];
{rules1,old,rules2}
);
addrule[{rules1___,old:_[x1_,_],rules2___},rule:_[x1_,_],tensor_]:=
If[$CVReplace,If[$CVVerbose,Print["Replaced independent rule ",old," by ",rule," for tensor ",tensor]];
{rules1,rule,rules2},
If[$CVVerbose,Print["Kept old independent rule ",old," vs. new ",rule," for tensor ",tensor]];
{rules1,old,rules2}
];
addrule[{rules___},rule_,tensor_]:=(
If[$CVVerbose,Print["Added independent rule ",rule," for tensor ",tensor]];
{rules,rule});


(* Negative sign always on the RHS of rules *)
RightSign[head_[-exprL_,exprR_]]:=head[exprL,-exprR];
RightSign[rule_]:=rule;


(* Check for dependent rules and all rules for expr *)
checkrules2[-expr_]:=-checkrules2[expr];
checkrules2[expr_]:=With[{rules=TensorValues[expr]},If[Position[rules,HoldPattern[Rule[expr,_]],{2}]=!={},
{expr/.Take[rules,{1,-1,2}],expr/.rules},
Null]
];


(* If we try to associate a value\[NotEqual]0 to a zero component by symmetry then send (but not throw) a message *)
$CheckZeroValue=False;
checkzero[_Function]:=Null;
checkzero[value_]:=If[$CVSimplify[value]=!=0,Print["Value ",value," expected to be 0 by symmetry."]];


FunctionValue[expr_,function_Function]:=function[expr];
FunctionValue[expr_,value_]:=value;


SetAttributes[ICV,HoldFirst];
(* Step 0: evaluation of expr *)
ComponentValue[expr_,value_]:=ICV[expr,expr,$CVSimplify[value],1];
HeldComponentValue[expr_,value_]:=ICV[expr,expr,value,1];
(* Step 1F: expr does not evaluate: check for stored rules *)
ICV[expr_,expr_,value_,1]:=ICV[expr,checkrules2[expr],value,2];
(* Step 1T: expr does evaluate *)
ICV[expr_,expr2_,value_,1]:=ICV[expr,checkrules2[HoldPattern[expr]],value,2];
(* Step 2F: expr does not have a stored value: canonicalize *)
ICV[expr_,Null,value_,2]:=ICV[expr,ToCanonical[expr],value,3];
(* Step 2 T-0: stored canon is zero: return known dependent rule. A canonical 0 cannot be removed. Check zero value *)
ICV[expr_,{0,0},Null,2]:=DoRule[0,Null];
ICV[expr_,{0,0},value_,2]:=(If[$CheckZeroValue,checkzero[value]];DoRule[expr,0]);
(* Step 2T-func: if indep value known use it; otherwise compute value from canon *)
ICV[expr_,{canon_,canon_},function_Function,2]:=ICV[expr,{canon,canon},function[canon],2];
ICV[expr_,{canon_,value_},_Function,2]:=ICV[expr,{canon,value},value,2];
(* Step 2T-indep: values coincide: everything is known, return rule *)
ICV[expr_,{_,value_},value_,2]:=DoRule[expr,value];
(* Step 2T-dep: only canon is known: store value as independent rule *)
ICV[expr_,{canon_,_},value_,2]:=(StoreRule[expr,RightSign@DoRule[canon,value],Last];DoRule[expr,value]);
(* Step 3F: expr is canonical: new independent rule, store and return it *)
ICV[expr_,expr_,function_Function,3]:=ICV[expr,expr,function[expr],3];
ICV[expr_,expr_,value_,3]:=(StoreRule[expr,DoRule[expr,value],Last];DoRule[expr,value]);
(* Step 3 T-0: canon is zero: new dependent rule, store and return it. Check zero value *)
ICV[expr_,0,Null,3]:=(StoreRule[expr,DoRule[expr,0],First];DoRule[0,Null]);
ICV[expr_,0,value_,3]:=(StoreRule[expr,DoRule[expr,0],First];If[$CheckZeroValue,checkzero[value]];DoRule[expr,0]);
(* Step 3T: expr is not canonical: new dependent rule, store it and check for stored independent rules *)
ICV[expr_,canon_,value_,3]:=(StoreRule[expr,DoRule[expr,canon],First];ICV[expr,canon,checkrules2[canon],value,4]);
(* Step 4T-func: if indep value known use it; otherwise compute value from canon *)
ICV[expr_,canon_,{canon_,canon_}|Null|-Null,function_Function,4]:=ICV[expr,canon,Null,function[canon],4];
ICV[expr_,canon_,{canon_,value_},_Function,4]:=ICV[expr,canon,{canon,value},value,4];
(* Step 4T: value for canon is already stored: return it *)
ICV[expr_,canon_,{canon_,value_},value_,4]:=DoRule[expr,value];
(* Step 4F: value is different or nothing is known: new independent rule, store it and return final rule *)
ICV[expr_,canon_,{_,_}|Null|-Null,value_,4]:=(StoreRule[expr,RightSign@DoRule[canon,value],Last];DoRule[expr,value]);


Protect[ComponentValue,HeldComponentValue];


AllComponentValues[expr:tensor_?xTensorQ[___]]:=With[{bases=BasesEquivalents[expr]},ComponentValue/@(ComponentArray[tensor,#]&/@bases);TensorValues[tensor,bases]];
AllComponentValues[expr:tensor_?xTensorQ[___],values_]:=(ThreadComponentArray[expr,values,ComponentValue];AllComponentValues[expr]);
SetNumberOfArguments[AllComponentValues,{1,2}];
Protect[AllComponentValues];


General::state="ValID `1` is already in `2` state.";


AddHoldPattern[list_List]:=AddHoldPattern/@list;
AddHoldPattern[Rule[a_,b_]]:=Rule[HoldPattern[a],b];


RuleToSet[tensor_?xTensorQ]:=(RuleToSet/@TensorValIDs[tensor];Null);
RuleToSet[tensor_?xTensorQ,ders___?FirstDerQ]:=(RuleToSet/@TensorValIDs[tensor,ders];Null);
RuleToSet[tensor_?xTensorQ,ders___?FirstDerQ,baseslist:{__List}]:=RuleToSet[ValID[tensor,ders,baseslist]];
RuleToSet[tensor_?xTensorQ,ders___?FirstDerQ,bases_List]:=RuleToSet[ValID[tensor,ders,BasesEquivalents[bases]]];
RuleToSet[valID:ValID[tensor_?xTensorQ,ders___?FirstDerQ,baseslist:{__List}]]:=
If[StateOfValID[valID]===Set,
Message[RuleToSet::state,valID,"Set"],
With[{rules=TensorValues[tensor,ders,baseslist]},
If[tensor===Basis,
Off[UpSet::write];
BasisValues[baseslist[[1,1]],baseslist[[1,2]]]^=AddHoldPattern/@rules;
On[UpSet::write];
Unprotect[Basis];
Reverse[rules]/.HoldPattern[Rule[x_,y_]]:>xTagSet[{Basis,x},y];
Protect[Basis],
xTagSet[{tensor,TensorValues[tensor,ders,baseslist]},AddHoldPattern/@rules];
Reverse[rules]/.HoldPattern[Rule[x_,y_]]:>xTagSet[{tensor,x},y]
]
];
StateOfValID[valID]=Set;
];
SetNumberOfArguments[RuleToSet,{1,Infinity}];
Protect[RuleToSet];


unset[tensor_][list_List]:=unset[tensor]/@list;
unset[tensor_][Rule[a_HoldPattern,b_]]:=With[{tensorhead=SubHead[tensor]},TagUnset[tensorhead,a];First[a]->b];
unset[tensor_][rule_Rule]:=rule;


SetToRule[tensor_?xTensorQ]:=(SetToRule/@TensorValIDs[tensor];Null);
SetToRule[tensor_?xTensorQ,ders___?FirstDerQ]:=(SetToRule/@TensorValIDs[tensor,ders];Null);
SetToRule[tensor_?xTensorQ,ders___?FirstDerQ,baseslist:{__List}]:=SetToRule[ValID[tensor,ders,baseslist]];
SetToRule[tensor_?xTensorQ,ders___?FirstDerQ,bases_List]:=SetToRule[ValID[tensor,ders,BasesEquivalents[bases]]];
SetToRule[valID:ValID[tensor_?xTensorQ,ders___?FirstDerQ,baseslist:{__List}]]:=
If[StateOfValID[valID]===Rule,
Message[SetToRule::state,valID,"Rule"],
With[{rules=TensorValues[tensor,ders,baseslist]},
If[tensor===Basis,
Unprotect[Basis];
Off[UpSet::write];
BasisValues[baseslist[[1,1]],baseslist[[1,2]]]^=unset[Basis]/@rules;
On[UpSet::write];
Protect[Basis],
xTagSet[{tensor,TensorValues[tensor,ders,baseslist]},unset[tensor]/@rules]
]
];
StateOfValID[valID]=Rule;
];
SetNumberOfArguments[SetToRule,{1,Infinity}];
Protect[SetToRule];


StateOfValID[valID_]:=Rule


(* Scoring function *)
score[basis_,basis_]:=0;
score[basis_,-basis_]:=1;
score[-basis_,basis_]:=1;
score[basis1_?BasisQ,basis2_?BasisQ]:=1;
score[-basis1_?BasisQ,-basis2_?BasisQ]:=1;
score[basis1_,basis2_]:=2;
(* Mark init structures *)
CompareStructures[fstruct_List,istructs:{__List}]:=Map[CompareStructures[fstruct,#]&,istructs];
CompareStructures[fstruct_List,istruct_List]:=Inner[score,fstruct,istruct,Plus];
(* Select best init structure among a list *)
BestStructure1[fstruct_,basesequivalents_]:=Last@First@Sort@Transpose[{CompareStructures[fstruct,basesequivalents],basesequivalents}];
BestStructure[tensor_[finds__],tensor_[iinds__]]:=BestStructure1[basisof/@{finds},BasesEquivalents[tensor[iinds]]];
(* Alternative syntaxes *)
BestStructure[tensor_[finds__],istructs:{__List}]:=BestStructure1[basisof/@{finds},istructs];
BestStructure[tensor_[finds__]]:=BestStructure[tensor[finds],Last/@TensorValIDs[tensor]];


(* Construct route *)
Route[finds_,fstruct_,istruct_]:=DeleteCases[Apply[Route1,Transpose[{finds,fstruct,istruct}],1],Identity];


(* Actions on particular indices *)
Route1[ind_,basis_,basis_]:=Identity;
Route1[ind_,basis_,-basis_]:=Function[SeparateMetric[Automatic,basis][#,ind]];
Route1[ind_,-basis_,basis_]:=Function[SeparateMetric[Automatic,basis][#,ind]];
Route1[ind_,basis1_?BasisQ,basis2_?BasisQ]:=Function[SeparateBasis[basis2][#,ind]];
Route1[ind_,-basis1_?BasisQ,-basis2_?BasisQ]:=Function[SeparateBasis[basis2][#,ind]];
Route1[ind_,basis1_?BasisQ,-basis2_?BasisQ]:=Sequence[Function[SeparateMetric[Automatic,basis2][#,ind]],Function[SeparateBasis[basis2][#,ind]]];
Route1[ind_,-basis1_?BasisQ,basis2_?BasisQ]:=Sequence[Function[SeparateMetric[Automatic,basis2][#,ind]],Function[SeparateBasis[basis2][#,ind]]];


LastStep[tensor_[finds__],bestructure_]:=Last@ComposeList[Route[{finds},basisof/@{finds},bestructure],tensor[finds]]


RouteSteps[basis_Basis expr_]:=Append[RouteSteps[expr],basis];
RouteSteps[(metric:_?MetricQ[__])expr_]:=Append[RouteSteps[expr],metric];
RouteSteps[tensor:_?xTensorQ[__]]:={tensor};
RouteSteps[_]:=Throw[Message[ChangeComponents::error,"Non-metric tensor not found in expression"]];


$CCSimplify=Identity;
$TUseValues=ToCanonical;
$BMUseValues=ToCanonical;
takerules[rules_,All]:=rules;
takerules[rules_,ToCanonical]:=Take[rules,{1,-1,2}];
takerules[rules_,None]:={};


SetAttributes[{ComputeSteps,ComputeStep},HoldFirst];
ComputeSteps[rules_,{tensor_,factor1_,factors___}]:=ComputeSteps[rules,{ComputeStep[rules,tensor,factor1],factors}];
ComputeSteps[rules_,{tensor_}]:=tensor;
ComputeStep[rules_,tensor_,factor_]:=Module[
{time=AbsoluteTime[],
contracted=ContractMetric@ContractBasis[tensor factor],
expanded=$CCSimplify[ComponentArray@TraceBasisDummy[tensor factor]/.takerules[TensorValues[tensor],$TUseValues]/.takerules[TensorValues[Head[factor]],$BMUseValues]]},
PrependTo[rules,AllComponentValues[contracted,expanded]];
Print["Computed ",ScreenDollarIndices[contracted->tensor factor]," in ",AbsoluteTime[]-time," Seconds"];
contracted
]


ChangeComponents[final_,init_]:=Module[{rules={}},
ComponentValue[ComponentArray[init]];
ComputeSteps[rules,RouteSteps@LastStep[final,BestStructure[final,init]]];
Join@@rules];
ChangeComponents[final_,init_,function_]:=MapRuleR[ChangeComponents[final,init],function];
SetNumberOfArguments[ChangeComponents,{2,3}];
Protect[ChangeComponents];


ToValues[expr_]:=ToValues[expr,All];
ToValues[expr_,All,f_:Identity]:=ToValues[expr,Union@Map[Head,FindAllOfType[expr,Tensor]],f];
ToValues[expr_,tensor_?xTensorQ,f_:Identity]:=ToValues[expr,{tensor},f];
ToValues[expr_,list_List,f_:Identity]:=Fold[f[ReplaceAll[#1,#2]]&,expr,TensorValues/@list];
SetNumberOfArguments[ToValues,{1,3}];
Protect[ToValues];


toset[{tensor_,Rule}]:=RuleToSet[tensor];
toset[{tensor_,Set}]:=Null;
toinit[{tensor_,Rule}]:=SetToRule[tensor];
toinit[{tensor_,Set}]:=Null;


SetAttributes[WithSetValues,HoldFirst];
WithSetValues[expr_]:=WithSetValues[expr,Head/@FindAllOfType[expr,Tensor]];
WithSetValues[expr_,tensor_Symbol]:=WithSetValues[expr,{tensor}];
WithSetValues[expr_,tensors_List]:=Module[{tstates=Flatten[MapThread[Thread[{##}]&,{tensors,Map[StateOfValID,TensorValIDs/@tensors,{2}]}],1],result},
toset/@tstates;
result=expr;
toinit/@tstates;
result
];
SetNumberOfArguments[WithSetValues,{1,2}];
Protect[WithSetValues];


ToTensorRules[tensor_?xTensorQ,ctensor_CTensor]:=ComponentValue[ComponentArray[tensor,CTensorBases[ctensor]],ctensor]


expandsignature[{plus_,minus_,zero_}]:=Join[Table[1,{plus}],Table[-1,{minus}],Table[0,{zero}]];


Options[MetricInBasis]={Inverse->False,CVSimplify:>$CVSimplify};
(* Shortcuts *)
MetricInBasis[metric_Symbol?MetricQ,basis_,"Orthonormal",options___]:=MetricInBasis[metric,basis,expandsignature[SignatureOfMetric[metric]],options];
MetricInBasis[metric_Symbol,basis_,"Orthogonal",options___]:=MetricInBasis[metric,basis,Null IdentityMatrix[DimOfVbundle@VBundleOfBasis[basis]],options];
(* A tensor expression. We check consistency of aindices of expr with basis *)
MetricInBasis[metric_Symbol?MetricQ,basis_,expr_,options___]:=With[{inds=IndicesOf[Free,AIndex][expr]},MetricInBasis[metric,basis,ComponentArray[ToBasis[basis][expr]],options]/;(Length[inds]===2&&SameQ@@Map[UpIndexQ,Append[inds,basis]])];
(* Only the diagonal *)
MetricInBasis[metric_Symbol?MetricQ,basis_,diagonal_List/;ArrayDepth[diagonal]===1,options___]:=MetricInBasis[metric,basis,DiagonalMatrix[diagonal],options];
(* Give a CTensor *)
MetricInBasis[metric_Symbol?MetricQ,basis_,CTensor[matrix_,{basis_,basis_}],options___]:=MetricInBasis[metric,basis,matrix,options];
(* Main definition: basis could be up or down *)
MetricInBasis[metric_Symbol?MetricQ,basis_,matrix_List/;ArrayDepth[matrix]===2,options:OptionsPattern[]]:=Module[{inverse,cvsimplify},
If[!MetricQ[metric],Throw@Message[MetricInBasis::unknown,"metric",metric]];
If[!BasisQ[basis]&&!BasisQ[-basis],Throw@Message[MetricInBasis::unknown,"basis",basis]];
{inverse,cvsimplify}=OptionValue[{Inverse,CVSimplify}];
If[inverse,ComponentValue[ComponentArray[metric,{-basis,-basis}],cvsimplify@Inverse[matrix]]];ComponentValue[ComponentArray[metric,{basis,basis}],matrix]
];
(* Error *)
MetricInBasis[_,_,matrix_,___]:=Throw@Message[MetricInBasis::invalid,matrix,"metric-values specification"];
SetNumberOfArguments[MetricInBasis,{3,Infinity}];
Protect[MetricInBasis];


(************************* 5.Charts ************************)


Unprotect[Reals];


(* Dimension cannot be explicitly negative or explicitly noninteger *)
ValidDimensionQ[dim_]:=ConstantQ[dim]&&If[dim<0||(NumericQ[dim]&&!IntegerQ[dim]),False,True,True];
ManifoldQ[Reals[dim_]]^:=ValidDimensionQ[dim];


(* Properties *)
DimOfManifold[Reals[dim_]]^:=dim;
SplittingsOfManifold[Reals[dim_]]^:={};
Tangent[Reals[dim_]]^:=TangentReals[dim];


(* Standard formatting *)
PrintAs[Reals[dim_]]^:="\!\(\[DoubleStruckCapitalR]\^"<>ToString[dim]<>"\)";
MakeBoxes[Reals[dim_],StandardForm]:=xAct`xTensor`Private`interpretbox[Reals[dim],SuperscriptBox["\[DoubleStruckCapitalR]",MakeBoxes[dim,StandardForm]]];


(* Note we do not analyze the elements of the list, not even whether they are real *)
Unprotect[ManifoldQ,DimOfManifold,SplittingsOfManifold];
ManifoldQ[point_List]:=True;
DimOfManifold[point_List]:=0; 
SplittingsOfManifold[point_List]:={};
Protect[ManifoldQ,DimOfManifold,SplittingsOfManifold];
Reals/:SubmanifoldQ[Reals[dim_],point_List]:=Length[point]===dim;


Protect[Reals];


(manifold:CartesianProduct[Reals[_]..]):=Plus@@(First/@List@@Unevaluated[manifold]);


CartesianProduct[points__List]:=Join[points];


CartesianProduct[Reals[dim1_],Reals[dim2_]]:=Reals[dim1+dim2];


CartesianProduct::rdim="Incompatible dimensions of mapping domain and point `1`.";


phi_CartesianProduct[point_List]:=With[{dims=DimOfManifold/@MappingDomain/@List@@phi},
xAct`xTensor`Private`ThreadCartesianProduct[Compose,{phi,Internal`PartitionRagged[point,dims]}]/;(Total[dims]===Length[point]||Throw@Message[CartesianProduct::rdim,{Total[dims],Length[point]}])
];


VBundleQ[TangentReals[dim_]]^:=ValidDimensionQ[dim];


Dagger[TangentReals[dim_]]^:=TangentReals[dim];
BaseOfVBundle[TangentReals[dim_]]^:=Reals[dim];
DimOfVBundle[TangentReals[dim_]]^:=dim;
MetricsOfVBundle[TangentReals[dim_]]^:={EuclideanMetric[IdentityMapping[Reals[dim]]]};
SplittingsOfVBundle[TangentReals[dim_]]^:={};


IndicesOfVBundle[TangentReals[dim_]]^:=Throw@Message[IndicesOfVBundle::error2,"Please, specify a list of indices for",TangentReals[dim],"using RegisterIndices."];


$TangentRealsTemporaryIndices=Symbol/@CharacterRange["\[FormalA]","\[FormalZ]"];


PrintAs[TangentReals[dim_]]^:=StringJoin["\[DoubleStruckCapitalT]\[DoubleStruckCapitalR]\^",ToString[dim]];
MakeBoxes[TangentReals[dim_],StandardForm]:=xAct`xTensor`Private`interpretbox[TangentReals[dim],SuperscriptBox["\[DoubleStruckCapitalT]\[DoubleStruckCapitalR]",MakeBoxes[dim,StandardForm]]];


TangentReals/:CirclePlus[TangentReals[m_],TangentReals[n_]]:=TangentReals[m+n];


Options[DefChart]={
ChartColor->RGBColor[1,0,0],
FormatBasis->Automatic,
ProtectNewSymbol:>$ProtectNewSymbols,
Dagger->Real,
MetricInBasis->{},
epsilonOrientationOfMetric->{Null,1},
ExtendedCoordinateDerivatives->True,
DefInfo->{"chart",""}
};
DefChart[chartname_,manifold_,cnumbers_,scalars_List,options:OptionsPattern[]]:=
With[{pd=GiveSymbol[PD,chartname],heads=Head/@scalars,index=IndicesOfVBundle[TangentBundleOfManifold[manifold]][[1,1]]},
Module[{cc,fb,pns,dag,eoom,ecd,info,prot,dheads,pheads},
{cc,fb,pns,dag,eoom,ecd,info}=OptionValue[{ChartColor,FormatBasis,ProtectNewSymbol,Dagger,epsilonOrientationOfMetric,ExtendedCoordinateDerivatives,DefInfo}];
(* dheads=Map[SymbolJoin["d",#]&,heads];
pheads=Map[SymbolJoin["p",#]&,heads]; *)

(* Checks *)
ValidateSymbol[chartname];
ValidateSymbolInSession[chartname];
If[!ManifoldQ[manifold],Throw@Message[DefChart::unknown,"manifold",manifold]];
If[!IntegerQ[DimOfManifold[manifold]],Throw@Message[DefChart::error,"Chart cannot be defined on a manifold with symbolic dimension."]];
If[Not@And@@Join[IntegerQ/@cnumbers],Throw@Message[DefChart::error,"The cnumbers must be integers."]];
If[DimOfManifold[manifold]!=Length[cnumbers],Throw@Message[DefChart::error,"Incorrect number of cnumbers supplied."]];
If[DimOfManifold[manifold]!=Length[scalars],Throw@Message[DefChart::error,"Incorrect number of scalars supplied."]];
(* Check eoom *)
If[Head[eoom]=!=List||Length[eoom]=!=2,
Throw@Message[DefChart::invalid,eoom,"value for option epsilonOrientationOfMetric"];
];
If[!xTensorQ[#],ValidateSymbol[#]&&ValidateSymbolInSession[#]]&/@heads;
(* ValidateSymbolInSession/@dheads;
ValidateSymbolInSession/@pheads; *)

MakexTensions[DefChart,"Beginning",chartname,manifold,cnumbers,scalars,options];

(* Register *)
xAct`xTensor`Private`MakeDefInfo[DefChart,chartname,info];
ChartQ[chartname]^=True;
ManifoldOfChart[chartname]^=manifold;
ChartColor[chartname]^=cc;
AppendTo[$Charts,chartname];
(* Changes of coordinates *)
xAct`xTensor`Private`SymbolRelations[chartname,Null,{manifold}];

(* Define coordinate scalars if they do not exist yet *)
prot=Unprotect/@heads;
ServantsOf[chartname]^={};
If[!xTensorQ[Head[#]],DefTensor[#,manifold,Master->chartname,DefInfo->If[info===False,False,{"coordinate scalar",""}]]]&/@scalars;
MapIndexed[setinchart[chartname],scalars];
ScalarsOfChart[chartname]^=scalars;
(* ChartRules[chartname]^=Thread@Rule[manifold/@cnumbers,scalars];*)
(* Store coordinate names *)
Inner[Set,Coordinate[#,chartname]&/@cnumbers,heads,List];

(* Define associated mapping to Reals[dim] *)
DefMapping[chartname, manifold-> Reals[DimOfManifold[manifold]],ImmersionQ->True,SubmersionQ->True,InverseMapping->Automatic];
With[{inv=InverseMapping[chartname]},ChartIMapQ[inv]^=True];

(* Define associated basis, which in turn defines the parallel derivative. Turn off formating. Quiet OptionValue::nodef because we are passing DefChart options *)
Quiet[DefBasis[chartname,TangentBundleOfManifold[manifold],cnumbers,FormatBasis->Automatic,DefInfo->{"basis","Coordinated basis."},options,BasisColor->cc],OptionValue::nodef];
(* Define shortcuts for basis elements
Inner[Hold,Through[dheads[index_]],basis[{#,chartname},index]&/@cnumbers,List]/.basis\[Rule]Basis/.Hold\[Rule]SetDelayed;
Inner[Hold,Through[pheads[index_]],basis[{#,-chartname},index]&/@cnumbers,List]/.basis\[Rule]Basis/.Hold\[Rule]SetDelayed;*)
(* Define position vector *)
(* Format bases elements *)
If[fb=!=Automatic,
If[!MatchQ[fb,{_,_}],Throw@Message[DefChart::invalid,fb,"value for option FormatBasis"]];
FormatUpVectors[fb[[1]],CIndicesOf[-chartname],scalars];
FormatDownVectors[fb[[2]],CIndicesOf[chartname],scalars];
];
(* If MetricInBasis is specified, should we automatically compute Christoffels? *)

(* Any derivative of a coordinate scalar field gives a Basis vector *)
Inner[coordinateder[chartname,ecd],heads,cnumbers,List];

MakexTensions[DefChart,"End",chartname,manifold,cnumbers,scalars,options];

Protect[Evaluate[prot]];
]
];
SetNumberOfArguments[DefChart,{4,Infinity}];
Protect[DefChart];


partial[scalar_[]]:="\!\(\*FractionBox[\(\[PartialD]\),\(\[PartialD]\*"<>PrintAs[scalar]<>"\)]\)";
differential[scalar_[]]:="d"<>PrintAs[scalar];
FormatUpVectors["Partials",cindices_List,scalars_List]:=FormatUpVectors[partial/@scalars,cindices,scalars];
FormatUpVectors[list_List,cindices_List,scalars_List]:=Module[{},
If[Length[list]=!=Length[cindices],Throw@Message[FormatBasis::invalid,list,"list of format values for up-vectors"]];
xTensorFormStop[Tensor];
MapThread[FormatBasis,{cindices,list}];
xTensorFormStart[Tensor];
];
FormatDownVectors["Differentials",cindices_List,scalars_List]:=FormatDownVectors[differential/@scalars,cindices,scalars];
FormatDownVectors[list_List,cindices_,scalars_]:=Module[{},
If[Length[list]=!=Length[cindices],Throw@Message[FormatBasis::invalid,list,"list of format values for down-vectors"]];
xTensorFormStop[Tensor];
MapThread[FormatBasis,{cindices,list}];
xTensorFormStart[Tensor];
];
FormatUpVectors[x_,_,_]:=Throw@Message[FormatBasis::unknown,"formatting value for up-vectors",x];
FormatDownVectors[x_,_,_]:=Throw@Message[FormatBasis::unknown,"formatting value for down-vectors",x];


coordinateder[chart_Symbol,True][head_Symbol,number_Integer]:=Apply[TagSetDelayed,{head,_?CovDQ[a_][head[]],Unevaluated[Basis[a,{number,chart}]]}]


coordinateder[chart_Symbol,False][head_Symbol,number_Integer]:=Apply[TagSetDelayed,{head,PDOfBasis[chart][a_][head[]],Unevaluated[Basis[a,{number,chart}]]}]


setinchart[chart_][scalar_[],{n_}]:=(
xUpSet[ChartOfScalar[scalar[]],chart];
xTagSet[{scalar,InChart[scalar[],chart]},Function[Slot[n]]];
);


ChartOfScalar[expr_]:=Null;
SetNumberOfArguments[ChartOfScalar,1];
Protect[ChartOfScalar];


UndefChart[chart_]:=Module[{servants=ServantsOf[chart]},
If[!ChartQ[chart],Throw[Message[UndefChart::unknown,"chart",chart]]];
xUpSet[ServantsOf[chart],{}];
xAct`xTensor`Private`CheckRemoveSymbol[chart];
MakexTensions[UndefChart,"Beginning",chart];
xAct`xTensor`Private`DropFromHosts[chart];
UndefAbstractIndex/@Flatten@IndicesOfVBundle[PullBackVBundle[Tangent[MappingImage[chart]],chart]];
$Charts=DeleteCases[$Charts,chart];
$Bases=DeleteCases[$Bases,chart];
$Mappings=DeleteCases[$Mappings,chart];
Undef/@servants;
MakexTensions[UndefChart,"End",chart];
xAct`xTensor`Private`RemoveSymbol[chart];
];
SetNumberOfArguments[UndefChart,1];
Protect[UndefChart];


$IdentityScalarSymbol=\[DoubleStruckX];
With[{symbol=$IdentityScalarSymbol},
xTensorQ[symbol[dim_,i_Integer]]^:=True;
DependenciesOfTensor[symbol[dim_,_Integer]]^:={Reals[dim]};
SlotsOfTensor[symbol[dim_,_Integer]]^:={};
SymmetryGroupOfTensor[symbol[dim_,_Integer]]^:=StrongGenSet[{},GenSet[]];
WeightOfTensor[symbol[dim_,_Integer]]^:=0;
Dagger[symbol[dim_,i_Integer]]^:=symbol[dim,i];
ChartOfScalar[symbol[dim_,i_Integer]]^:=IdentityMapping[Reals[dim]];
PrintAs[symbol[dim_,i_Integer]]^:="\!\(\[DoubleStruckX]\^"<>ToString[i]<>"\)";
];


ChartQ[IdentityMapping[Reals[_]]]^=True;
ScalarsOfChart[IdentityMapping[Reals[dim_]]]^:=Table[$IdentityScalarSymbol[dim,i][],{i,1,dim}];
ManifoldOfChart[IdentityMapping[Reals[dim_]]]^:=Reals[dim];
ChartColor[IdentityMapping[Reals[dim_]]]^=GrayLevel[0];
IdentityMapping/:CNumbersOf[IdentityMapping[Reals[dim_]],Tangent[Reals[dim_]]]:=Range[dim];


ChartsOfManifold[manifold_]:=Select[VisitorsOf[manifold],ChartQ];
SetNumberOfArguments[ChartsOfManifold,1];
Protect[ChartsOfManifold];


ScalarsOfChart[chart_]:=Throw@Message[ScalarsOfChart::unknown,"chart",chart];
SetNumberOfArguments[ScalarsOfChart,1];
Protect[ScalarsOfChart];


ManifoldOfChart[chart_]:=Throw@Message[ManifoldOfChart::unknown,"chart",chart];
SetNumberOfArguments[ManifoldOfChart,1];
Protect[ManifoldOfChart];


ChartColor[chart_]:=Throw@Message[ChartColor::unknown,"chart",chart];
SetNumberOfArguments[ChartColor,1];
Protect[ChartColor];


ChartQ[chart_CartesianProduct]^:=And@@(ChartQ/@List@@chart);


ManifoldOfChart[chart_CartesianProduct]^:=ManifoldOfChart/@chart;
ScalarsOfChart[chart_CartesianProduct]^:=Join@@(ScalarsOfChart/@List@@chart);
ChartColor[chart_CartesianProduct]^:=GrayLevel[0];  (* TODO: do something better than black *)


If[$VersionNumber<8.5,
Unprotect[ArcTan];
Sin[ArcTan[x_,y_]]^=y/Sqrt[x^2+y^2];
Cos[ArcTan[x_,y_]]^=x/Sqrt[x^2+y^2];
Tan[ArcTan[x_,y_]]^=y/x;
Csc[ArcTan[x_,y_]]^=Sqrt[x^2+y^2]/y;
Sec[ArcTan[x_,y_]]^=Sqrt[x^2+y^2]/x;
Cot[ArcTan[x_,y_]]^=x/y;
Protect[ArcTan];
]


(* Main property *)
InChart/:ScalarFunctionQ[InChart[expr_,chart_]]:=True;


(* Trivial definitions *)
InChart[expr_Plus,chart_][args__]:=InChart[#,chart][args]&/@expr;
InChart[expr_Times,chart_][args__]:=InChart[#,chart][args]&/@expr;
InChart[expr_List,chart_][args__]:=InChart[#,chart][args]&/@expr;
InChart[k_?ConstantQ,_][__]:=k;
InChart[sf_?ScalarFunctionQ[sfargs__],chart_][args__]:=Apply[sf,InChart[#,chart][args]&/@{sfargs}];


(* Covariant derivative: equal charts *)
InChart[covd_Symbol?CovDQ[{a_Integer,-chart_?BasisQ}][expr_],chart_?BasisQ][args__]:=InChart[D[ToInChart[chart][expr],Coordinate[a,chart][]],chart][args];
(* Covariant derivative: different charts *)
InChart[covd_Symbol?CovDQ[{a_Integer,chart1_}][expr_],chart2_?BasisQ][args__]:=With[{dummy=DummyIn[VBundleOfBasis[chart1]]},InChart[TraceBasisDummy[Basis[{a,chart1},{dummy,chart2}]covd[{-dummy,-chart2}][expr],{dummy,chart2}],chart2][args]];


(* Basis *)
InChart[Basis[{j_Integer,-chart2_?ChartQ},{i_Integer,chart1_?ChartQ}],chart2_][args__]:=InChart[D[InChart[chart2][Coordinate[i,chart1][]],Coordinate[j,chart2][]],chart2][args];


(* For backwards compatibility, because xTras uses this form of InChart *)
InChart[chart_]:=ToInChart[chart];


Protect[InChart];


(* On CTensor objects *)
ToInChart[chart_][ctensor_CTensor[inds___]]:=ToInChart[chart][ctensor][inds];
ToInChart[chart_][CTensor[array_,bases_,weight_]]:=CTensor[ToInChart[chart][array],bases,weight];


(* On lists *)
ToInChart[chart_][list_List]:=ToInChart[chart]/@list;


(* Shortcut *)
ToInChart[chart_][expr_]:=InChart[expr,chart]@@ScalarsOfChart[chart];


MakeBoxes[InChart[expr_,chart_],StandardForm]:=xAct`xTensor`Private`interpretbox[InChart[expr,chart],UnderscriptBox[xAct`xTensor`Private`boxof@MakeBoxes[expr,StandardForm],StringJoin["\*StyleBox[\"_\",FontColor->",ToString[BasisColor[chart]],"]"]]];


(* Change InChart *)
ChangeChart[InChart[expr_,chart1_][args__],chart2_]:=InChart[ToInChart[chart2][expr],chart1][args];
(* Change derivative of InChart *)
ChangeChart[Derivative[ders__][InChart[expr_,chart1_]][args__],chart2_]:=InChart[D[ChangeChart[InChart[chart1][expr],chart2],Sequence@@variables[chart1,{ders}]],chart1][args];
(* Trivial cases for recursion *)
ChangeChart[expr_Plus,chart_]:=ChangeChart[#,chart]&/@expr;
ChangeChart[expr_Times,chart_]:=ChangeChart[#,chart]&/@expr;
ChangeChart[expr_List,chart_]:=ChangeChart[#,chart]&/@expr;
ChangeChart[k_?ConstantQ,_]:=k;
ChangeChart[sf_?ScalarFunctionQ[sfargs__],chart_]:=Apply[sf,ChangeChart[#,chart]&/@{sfargs}];
ChangeChart[expr_,chart_]:=expr;


SetNumberOfArguments[ChangeChart,2];
Protect[ChangeChart];


variables[chart_?BasisQ,ders_List]:=Inner[Table[#1,{#2}]&,ScalarsOfChart[chart],ders,Join];


Unprotect[ManifoldQ,DimOfManifold,SubmanifoldQ];
ManifoldQ[_List]:=True;
ManifoldQ[_?ChartIMapQ[_List]]:=True;
DimOfManifold[_List]:=0;
DimOfManifold[_?ChartIMapQ[_List]]:=0;
SubmanifoldQ[manifold_,ichart_?ChartIMapQ[_List]]:=SubmanifoldQ[manifold,ManifoldOfChart[InverseMapping[ichart]]];
Protect[ManifoldQ,DimOfManifold,SubmanifoldQ];


MappingQ[Transition[chart_?ChartQ,ichart_?ChartIMapQ]]^=True;
MappingDomain[Transition[chart_?ChartQ,ichart_?ChartIMapQ]]^:=MappingDomain[ichart];
MappingImage[Transition[chart_?ChartQ,ichart_?ChartIMapQ]]^:=MappingImage[chart];
ImmersionQ[Transition[chart_?ChartQ,ichart_?ChartIMapQ]]^=True;
SubmersionQ[Transition[chart_?ChartQ,ichart_?ChartIMapQ]]^=True;


Unprotect[At];
At[(sf:InChart[_,chart_])[args___],ichart_?ChartIMapQ[list_List]]:=sf@@({args}/.Thread[ScalarsOfChart[chart]->list])/;InverseMapping[chart]===ichart;
At[(sf:_Derivative[InChart[_,chart_]])[args___],ichart_?ChartIMapQ[list_List]]:=sf@@({args}/.Thread[ScalarsOfChart[chart]->list])/;InverseMapping[chart]===ichart;
Protect[At];


CurveTangent[ichart_?ChartIMapQ[list_List],param_][a_]:=If[UpIndexQ[a]&&VBundleOfIndex[a]===Tangent[MappingImage[ichart]],
At[CTensor[D[list,param],{InverseMapping[ichart]}][a],ichart[list]],
Throw@Message[CurveTangent::ind,"Index `1` cannot be used for the tangent vector of curve `2`."]
];


VectorD[scalar_?ScalarQ,At[vector_,P:ichart_?ChartIMapQ[_]]]:=With[{chart=InverseMapping[ichart]},
At[VectorD[InChart[scalar,chart]@@ScalarsOfChart[chart],vector],P]
];
VectorD[scalar_?ScalarQ,At[vector_,P_]]:=At[VectorD[scalar,vector],P];
VectorD[scalar_?ScalarQ,vector_]:=With[{ui=xAct`xTensor`Private`UltraindexOf[vector]},
ContractBasis[vector PD[-ui][scalar]]
];


arrayelements[array_List]:=DeleteDuplicates[Flatten[array]];
arrayelements[SparseArray[Automatic,_,_,{_,_,list_}]]:=DeleteDuplicates[list];
arrayelements[HoldPattern[StructuredArray[SymmetrizedArray,_,_[SymmetrizedArray,rules_,_]]]]:=DeleteDuplicates[Last/@rules];


(* Objects with no dependencies *)
FieldsOf[_Integer|_Rational|_Real|_Complex]:={};
FieldsOf[_Symbol|_String|_?ConstantQ]:={};
(* Objects that can contain coordinate scalars *)
FieldsOf[scalar_?xTensorQ[]]:={scalar[]};
FieldsOf[array:(_List|_SparseArray|_StructuredArray)]:=Union@Flatten[FieldsOf/@arrayelements[array]];
FieldsOf[expr:(_Plus|_Times|_?ScalarFunctionQ[__])]:=FieldsOf[List@@expr];
FieldsOf[_?InertHeadQ[expr_,z___]]:=FieldsOf[expr];
(* Objects that cannot contain undiferentiated coordinate scalars *)
FieldsOf[tensor_?xTensorQ[inds__]]:={tensor[inds]};
FieldsOf[der_?FirstDerQ[expr_]]:={der[expr]};
(* Unrecognized objects. Complain *)
FieldsOf[expr_]:=(Message[CTensor::unknown,"expression",expr];{expr});


(computation:ChartsOf[array_]):=xCobaCache[
computation,
Union[ChartOfScalar/@FieldsOf[array]],
Null,
Identity
];


General::nonchart="Found noncoordinate field.";
General::mixcharts="Found coordinate scalars of different charts `1` for manifold `2`.";


TensorDerivative[ctensor:CTensor[array_,bases_List,addweight_],covd_Symbol?CovDQ]:=Module[{vb,bs,basis,res,Dctensor,chrterms},
(* Decide the basis to use for the new CTensor slot *)
vb=TangentBundleOfManifold[ManifoldOfCovD[covd]];
bs=DeleteDuplicates[xAct`xPerm`Private`nosign/@bases];
bs=Select[bs,VBundleOfBasis[#]===vb&];
If[Length[bs]=!=1,
If[BasisOfCovD[covd]=!=Null,
bs={BasisOfCovD[covd]}
];
If[bs==={},
bs=BasesOfVBundle[vb]
];
If[bs==={},
Throw[Message[TensorDerivative::nobas]];
]
];
basis=First[bs];

(* Compute deep gradients *)
res=Map[ScalarCovDGradient[#,covd,basis]&,array,{If[ArrayQ[array],ArrayDepth[array],0]}];
Dctensor=CTensor[res,Append[bases,-basis],addweight];

(* Add Christoffels to take covd into consideration. Added slot has -basis *)
chrterms=Plus@@MapIndexed[TDChristoffelTerm[ctensor,basis,covd,#1,#2]&,bases];
res=Dctensor+chrterms;

(* Add density term. This is independent of the expansion basis *)
If[addweight=!=0,
res=res+Plus@@Map[TDChristoffelDensityTerm[ctensor,covd,#]&,xAct`xTensor`Private`ListOfTerms[addweight]]
];

res /; res=!=$Failed
   ];


ScalarCovDGradient[scalar_,covd_,basis_]:=covd[{#,-basis}][scalar]&/@CNumbersOf[basis];


TensorDerivative[ctensor_CTensor,der1_,ders__]:=With[{ctensor1=TensorDerivative[ctensor,der1]},
TensorDerivative[ctensor1,ders]/;Head[ctensor1]=!=TensorDerivative
];


TDChristoffelTerm[ctensor_,chart_,covd_Symbol,basis_,{n_}]:=ChristoffelContract[ctensor,RicciRotationCTensor[covd,basis,chart],{n}];


(* If we do not have a chart determined by the input, use the same basis in the output *)
RicciRotationCTensor[covd_,basis_,Null]:=RicciRotationCTensor[covd,basis,xAct`xPerm`Private`nosign@basis];
RicciRotationCTensor[covd_,basis_,basis2_]:=Module[{pdbasis=PDOfBasis[basis],vbundle,i1,i2,i3,christoffel},
If[covd===pdbasis,Return[Zero]];
vbundle=VBundleOfBasis[basis];
If[First@VBundlesOfCovD[covd]=!=vbundle,Return[Zero]];
{i1,i2,i3}=GetIndicesOfVBundle[vbundle,3];
christoffel=Christoffel[covd,pdbasis][i1,-i2,-i3];
christoffel=HeadOfTensor[christoffel,{i1,-i2,-i3}];
ToCTensor[christoffel,{UpIndex[basis],-basis2,DownIndex[basis]}]
];


TDChristoffelDensityTerm[ctensor_,covd_,n_. basis_?BasisQ]:=Module[{a,b,c,christoffel},
{a,b,c}=GetIndicesOfVBundle[VBundleOfBasis[basis],3];
christoffel=HeadOfTensor[Christoffel[covd,PDOfBasis[basis]][a,-b,-c],{a,-b,-c}];
christoffel=ToCTensor[christoffel,{basis,-basis,-basis},0];
ChristoffelDensityContract[ctensor,christoffel,{n,basis}]
];


ChristoffelDensityContract[ctensor_,christoffel_,{n_,basis_}]:=-n CTensorProduct[ctensor,CTensorContract[christoffel,{1,3}]];
(* Alternative notation for the CTensor case *)
ChristoffelDensityContract[ctensor_,christoffel_,n_. basis_?BasisQ]:=ChristoffelDensityContract[ctensor,christoffel,{n,basis}];


CTensor/:TensorDerivative[t:CTensor[array_,bases_List,taddweight_],LieD[(v:CTensor[vector_,{basis_},vaddweight_])[ui_]]]:=Module[
{vbundle,manifold,charts,chart,pdchart,vder},
vbundle=VBundleOfBasis[basis];
manifold=BaseOfVBundle[vbundle];
charts=Select[Union[ChartsOf[vector],ChartsOf[array]],(ManifoldOfChart[#]===manifold)&];
If[MemberQ[charts,Null],Throw@Message[LieD::nonchart]];
If[Length[charts]>1,Throw@Message[LieD::mixcharts,charts,manifold]];
If[charts==={},Return[Zero]];
chart=First[charts];
pdchart=PDOfBasis[chart];
vder=TensorDerivative[v,pdchart];
CTensorContract[TensorDerivative[t,pdchart],v,{Length[bases]+1,1},Times]+Plus@@MapIndexed[LieDContract[t,vder,#2]&,bases]
];


CTensor/:TensorDerivative[CTensor[array_,bases_List,addweight_],ParamD[ps__]]:=CTensor[ParamD[ps][array],bases,addweight];
CTensor/:TensorDerivative[CTensor[array_,bases_List,addweight_],OverDot]:=CTensor[OverDot[array],bases,addweight];


CTensor/:HeadOfTensor[tensor:CTensor[_?ArrayQ,_List,_][__],inds_]:=Head@TransposeAs[tensor,inds];


CCovD[covd_?CovDQ,christoffel_?xTensorQ]:=CCovD[covd,christoffel,Null];


CovDQ[CCovD[pd_?CovDQ,christoffel_?xTensorQ,metric_]]^:=True;
VBundlesOfCovD[CCovD[pd_?CovDQ,christoffel_?xTensorQ,metric_]]^:=DeleteDuplicates@(VBundleOfBasis/@christoffel[[2,{2,1,3}]]);
ManifoldOfCovD[ccovd:CCovD[pd_?CovDQ,christoffel_?xTensorQ,metric_]]^:=BaseOfVBundle[First[VBundlesOfCovD[ccovd]]];
DependenciesOfCovD[CCovD[pd_?CovDQ,christoffel_?xTensorQ,metric_]]^:=xAct`xTensor`Private`SortDependencies@Join[DependenciesOfCovD[pd],DependenciesOfTensor[christoffel]];


CCovD/:Christoffel[CCovD[pd_,christoffel_,metric_],pd_]:=christoffel;
CCovD/:Christoffel[pd_,CCovD[pd_,christoffel_,metric_]]:=MultiplyHead[-1,christoffel];
CCovD/:Christoffel[CCovD[pd_,christoffel1_,metric_],CCovD[pd_,christoffel2_,metric_]]:=xAct`xTensor`Private`TensorPlus[christoffel1,MultiplyHead[-1,christoffel2]];
CCovD/:Christoffel[CCovD[pd_,christoffel_,metric_],covd_]:=xAct`xTensor`Private`TensorPlus[Christoffel[pd,covd],christoffel];
CCovD/:Christoffel[covd_,CCovD[pd_,christoffel_,metric_]]:=xAct`xTensor`Private`TensorPlus[Christoffel[covd,pd],MultiplyHead[-1,christoffel]];


xAct`xTensor`Private`DaggerCovD[CCovD[pd_,christoffel_,metric_]]:=CCovD[xAct`xTensor`Private`DaggerCovD[pd],Dagger[christoffel],Dagger[metric]];
SymmetryGroupOfCovD[covd_CCovD]:=StrongGenSet[{},GenSet[]];
DependenciesOfCovD[CCovD[pd_,christoffel_,metric_]]^:=Join[xAct`xTensor`DependenciesOfCovD[pd],DependenciesOfTensor[christoffel],DependenciesOfTensor[metric]];
MetricOfCovD[CCovD[pd_,christoffel_,metric_]]^:=metric;


CCovD[CCovD[pd_,christoffel1_CTensor,_],christoffel2_,metric_]:=With[{bases=CTensorBases[christoffel1]},
CCovD[pd,christoffel1+ToCTensor[christoffel2,bases],If[metric===Null,Null,ToCTensor[metric,{-bases[[1]],-bases[[1]]}]]]
];


CChristoffel[pd1_,pd2_,bases_List]:=With[{inds=DummyIn/@SignedVBundleOfBasis/@bases},
ToCTensor[HeadOfTensor[Apply[Christoffel[pd1,pd2],inds],inds],bases]
];


(* A shortcut for a frequent case *)
ToCCovD[covd_?CovDQ,basis_?BasisQ]:=ToCCovD[covd,PDOfBasis[basis],{basis,-basis,-basis},{-basis,-basis}];
(* Recursive definitions *)
ToCCovD[covd_?CovDQ,pd_?CovDQ]:=ToCCovD[covd,pd,Automatic,Automatic];
ToCCovD[covd_?CovDQ,pd_?CovDQ,chrbases_]:=ToCCovD[covd,pd,chrbases,Automatic];
ToCCovD[covd_?CovDQ,pd_?CovDQ,basis_?BasisQ,metbases_]:=ToCCovD[covd,pd,{basis,-basis,-basis},metbases];
ToCCovD[covd_?CovDQ,pd_?CovDQ,chrbases_,basis_?BasisQ]:=ToCCovD[covd,pd,chrbases,{-basis,-basis}];
(* Special cases for CCovD objects *)
ToCCovD[ccovd:CCovD[pd_,christoffel_,metric_],pd_?CovDQ,Automatic,Automatic]:=ccovd;
ToCCovD[ccovd:CCovD[pd1_,christoffel_,metric_],pd2_?CovDQ,Automatic,metbases_]:=ToCCovD[ccovd,pd2,CTensorBases[christoffel],metbases];
(* General case for a CCovD object *)
ToCCovD[CCovD[pd1_,christoffel_,metric_],pd2_?CovDQ,chrbases_List,metbases_]:=CCovD[pd2,ToCTensor[christoffel,chrbases]+CChristoffel[pd1,pd2,chrbases],Which[metric===Null,Null,metbases===Automatic,metric,True,ToCTensor[metric,metbases]]];


CCovD/:TorsionQ[covd_CCovD]:=!SameQ[Torsion[covd],Zero];
CCovD/:CurvatureQ[covd_CCovD,vbundle_]:=If[xAct`xTensor`Private`TangentBundleOfCovD[covd]===vbundle,Riemann[covd]===Zero,FRiemann[covd]===Zero];


xAct`xTensor`Private`MakeLinearDerivative[{ccovd_CCovD[a_],ccovd[a]},True];
xAct`xTensor`Private`MakeTensorialDerivative[{ccovd_CCovD[a_],ccovd[a]}];


basisOfCCovD[CCovD[pd_,christoffel_,metric_]]:=Module[{basis},
basis=BasisOfCovD[pd];
If[!BasisQ[basis]&&MatchQ[christoffel,_CTensor],
basis=xAct`xPerm`nosign[CTensorBases[christoffel][[2]]]
];
If[!BasisQ[basis]&&MatchQ[metric,_CTensor],
basis=-CTensorBases[metric][[1]]
];
basis
]


(covd:CCovD[pd_,christoffel_,metric_])[ind_][scalar_?ScalarQ]:=With[{basis=basisOfCCovD[covd]},
If[basis=!=Null,
CTensor[pd[{#,-basis}][scalar]&/@CNumbersOf[basis],{-basis}][ind],
pd[ind][scalar]
]
];


Which[
System`$VersionNumber>8.5,
CCovD/:SymbolOfCovD[covd_CCovD]:={";",TooltipBox[FromCharacterCode[{62422}],CCovDBoxes[covd]]},
System`$VersionNumber>7.5,
CCovD/:SymbolOfCovD[covd_CCovD]:={";",TooltipBox[FromCharacterCode[{8711}],CCovDBoxes[covd]]},
True,
CCovD/:SymbolOfCovD[covd_CCovD]:={";",TooltipBox["\[Del]",CCovDBoxes[covd]]}
];


(computation:TensorDerivative[ctensor:CTensor[array_,bases_,addweight_],CCovD[pd_,christoffel_,metric_]])^:=xCobaCache[
computation,
TensorDerivative[ctensor,pd]+Plus@@MapIndexed[ChristoffelContract[ctensor,christoffel,#2]&,bases]+If[addweight===0,0,Plus@@Map[ChristoffelDensityContract[ctensor,christoffel,#]&,xAct`xTensor`Private`ListOfTerms[addweight]]]
];


CCovDBoxes[CCovD[der_,christoffel_CTensor,Null]]:=RowBox[{"CCovD","[",MakeBoxes[der,StandardForm],",",CTensorBoxes[christoffel,$LargeComponentSize],",","Null","]"}];
CCovDBoxes[CCovD[der_,christoffel_CTensor,metric_CTensor]]:=RowBox[{"CCovD","[",MakeBoxes[der,StandardForm],",",CTensorBoxes[christoffel,$LargeComponentSize],",",CTensorBoxes[metric,$LargeComponentSize],"]"}];
CCovDBoxes[ccovd_CCovD]:=MakeBoxes[ccovd,StandardForm];


MakeBoxes[ccovd_CCovD[inds__][expr_],StandardForm]:=Block[{$WarningFrom="CovD Formatting"},xAct`xTensor`Private`interpretbox[ccovd[inds][expr],xAct`xTensor`Private`MakeBoxesCovD[Unevaluated[ccovd][inds][xAct`xTensor`Private`boxof@xAct`xTensor`Private`BracketizedBoxesIfTimes[expr,StandardForm]],$CovDFormat]]];


(* Points of Reals[n] *)
ValidPointQ[tuple_List]:=True;
ChartOfPoint[tuple_List]:=IdentityMapping[Reals[Length@tuple]];
ChartIMapOfPoint[tuple_List]:=IdentityMapping[Reals[Length@tuple]];
ManifoldOfPoint[tuple_List]:=Reals[Length@tuple]; (* Our manifolds are real *)
ValuesOfPoint[tuple_List]:=tuple;
(* (i)Charted point in a general manifold *)
ValidPointQ[ichart_?ChartIMapQ[tuple_List]]:=True;
ChartOfPoint[ichart_?ChartIMapQ[tuple_List]]:=InverseMapping[ichart];
ChartIMapOfPoint[ichart_?ChartIMapQ[tuple_List]]:=ichart;
ManifoldOfPoint[ichart_?ChartIMapQ[tuple_List]]:=ManifoldOfChart[InverseMapping[ichart]];
ValuesOfPoint[chart_?ChartIMapQ[tuple_List]]:=tuple;
(* Error otherwise *)
ValidPointQ[point_]:=False;
ChartOfPoint[point_]:=Throw@Message[CMapping::invalid,point,"point"];
ChartIMapOfPoint[point_]:=Throw@Message[CMapping::invalid,point,"point"];
ManifoldOfPoint[point_]:=Throw@Message[CMapping::invalid,point,"point"];
ValuesOfPoint[point_]:=Throw@Message[CMapping::invalid,point,"point"];


(* Only one argument: deduce the chart from the list of values *)
CMapping[imP_?ValidPointQ]:=Module[{domcharts=ChartsOf[ValuesOfPoint[imP]],domchart,iimchart=ChartOfPoint[imP]},
If[MemberQ[domcharts,Null],Throw@@Message[CMapping::nonchart]];
If[Length[domcharts]>1,Throw@Message[CMapping::mixcharts,domcharts,ManifoldOfChart[First[domcharts]]]];
If[domcharts==={},Throw@Message[CMapping::error1,"Cannot infer chart from point",imP]];
domchart=First[domcharts];
CMapping[InverseMapping[domchart][ScalarsOfChart[domchart]],imP]
];
(* Two arguments, but the first one is only a chart or its inverse map *)
CMapping[domchart_?ChartQ,imP_]:=CMapping[InverseMapping[domchart][ScalarsOfChart[domchart]],imP];
CMapping[idomchart_?ChartIMapQ,imP_]:=CMapping[idomchart[ScalarsOfChart[InverseMapping[idomchart]]],imP];
(* Two arguments, but the first one contains symbols instead of scalar fields *)
CMapping[domP:{__Symbol},imP_]:=
With[{scalars=ScalarsOfChart[ChartOfPoint[domP],ManifoldOfPoint[domP]]},
CMapping[scalars,imP/.ThreadPoints[Rule,domP,scalars]]
];
CMapping[idomchart_?ChartIMapQ[domtuple:{__Symbol}],imP_]:=
With[{scalars=ScalarsOfChart[InverseMapping[idomchart]]},
CMapping[idomchart[scalars],imP/.ThreadPoints[Rule,domtuple,scalars]]
];


CMapping::dim="Dimensions of points `1` and `2` are incompatible.";


ThreadPoints[head_,point1_,point2_]:=With[{tuple1=ValuesOfPoint[point1],tuple2=ValuesOfPoint[point2]},
If[Length[tuple1]===Length[tuple2],
Thread[head[tuple1,tuple2],List],
Throw@Message[CMapping::dim,point1,point2]
]
];


MappingQ[CMapping[domP_?ValidPointQ,imP_?ValidPointQ]]^:=True;


MappingDomain[CMapping[domP_?ValidPointQ,imP_?ValidPointQ]]^:=ManifoldOfPoint[domP];
MappingImage[CMapping[domP_?ValidPointQ,imP_?ValidPointQ]]^:=ManifoldOfPoint[imP];


InverseMapping::noninv="Mapping cannot be inverted explicitly.";
InverseMapping::non1="Mapping inverse is not unique. Selecting last solution obtained by Solve.";


InverseMapping[CMapping[domP_?ValidPointQ,imP_?ValidPointQ]]^:=Module[{x,tmp,domchart,imchart,domtuple,imtuple,idomchart,iimchart,imscalars,sols},
domchart=ChartOfPoint[domP];
imchart=ChartOfPoint[imP];
domtuple=ValuesOfPoint[domP];
imtuple=ValuesOfPoint[imP];
idomchart=ChartIMapOfPoint[domP];
iimchart=ChartIMapOfPoint[imP];
imscalars=ScalarsOfChart[imchart];
tmp=Table[Unique[x],{Length[domtuple]}];
sols=Solve[ThreadPoints[Equal,imtuple/.Thread[domtuple->tmp],imscalars],tmp];
If[sols==={},Throw@Message[InverseMapping::noninv]];
If[Union[Length/@sols]=!={Length[domtuple]},Throw@Message[InverseMapping::noninv]];
If[Length[sols]>1,Message[InverseMapping::non1]];
CMapping[iimchart[imscalars],idomchart[tmp/.Last[sols]/.Thread[tmp->domtuple]]]
];


ImmersionQ[mapping_CMapping]^:=Module[{dmapping=TangentTensor[mapping],dims,matrix},
matrix=First[dmapping];
dims=Dimensions[matrix];
If[Greater@@dims,Return[False]];
If[Less@@dims,Return[True]];
Quiet[Check[Inverse[matrix],Return[False]]]; (* I'm not sure of this *)
True
];
SubmersionQ[mapping_CMapping]^:=Module[{dmapping=TangentTensor[mapping],dims,matrix},
matrix=First[dmapping];
dims=Dimensions[matrix];
If[Greater@@dims,Return[True]];
If[Less@@dims,Return[False]];
Quiet[Check[Inverse[matrix],Return[False]]]; (* I'm not sure of this *)
True
];


CMapping/:SmallCircle[left___,map2_CMapping,CMapping[domP1_,imP1_],right___]:=SmallCircle[left,CMapping[domP1,map2[imP1]],right];


CMapping[domP_?ValidPointQ,imP_?ValidPointQ][domQ_?ValidPointQ]:=imP/.ThreadPoints[Rule,ValuesOfPoint[domP],ChartOfPoint[domP][domQ]];


CMapping[P_,P_?ValidPointQ]:=IdentityMapping[ManifoldOfPoint[P]];


PrintAs[phi_CMapping]^:=MakeBoxes[Tooltip["\[CapitalPhi]",phi],StandardForm];
MakeBoxes[CMapping[imP_,domP_],StandardForm]:=xAct`xTensor`Private`interpretbox[CMapping[imP,domP],RowBox[{MakeBoxes[imP,StandardForm],"\[Function]",MakeBoxes[domP,StandardForm]}]];


TangentTensor[phi:CMapping[domP_,imP_]]^:=CTensor[Transpose@D[ValuesOfPoint[imP],{ValuesOfPoint[domP]}],{-ChartOfPoint[domP],PrecomposeBasis[ChartOfPoint[imP],phi]}];


CTensor/:Precompose[CTensor[array_,bases_List,addweight_],phi:CMapping[domP_?ValidPointQ,imP_?ValidPointQ]]:=CTensor[array/.ThreadPoints[Rule,ScalarsOfChart[ChartOfPoint[imP]],ValuesOfPoint[imP]],PrecomposeBasis[#,phi]&/@bases,xAct`xTensor`Private`PrecomposeWeight[addweight,phi]];


CTensor/:LinearPush[ctensor_CTensor,dphi_CTensor]:=Module[{bases,mbases,vb,vbs,ups,downs,idphitr},
bases=CTensorBases[ctensor];
mbases=CTensorBases[dphi];
vb=SignedVBundleOfBasis[-mbases[[1]]];
vbs=SignedVBundleOfBasis/@bases;
ups=First/@Position[vbs,vb,1];
downs=First/@Position[vbs,-vb,1];
If[downs=!={},idphitr=CTensorTranspose[Inv[dphi],{2,1}]];
CTensorContractMatrices[CTensorContractMatrices[ctensor,dphi&/@ups,ups,Times],idphitr&/@downs,downs,Times]
];


CTensor/:PullBackTensor[ctensor_CTensor,phi_CMapping]:=Module[{dphi=TangentTensor[phi]},
LinearPush[Precompose[ctensor,phi],CTensorTranspose[dphi,{2,1}]]
];


CTensor/:PushForwardTensor[ctensor_CTensor,phi_CMapping]:=Module[{dphi=TangentTensor[phi],iphi=InverseMapping[phi]},
Precompose[LinearPush[ctensor,dphi],iphi]
];


(************************* 5.Curvature components ************************)


SetCMetric::old="There are already metrics `1` in vbundle `2`.";


Options[SetCMetric]={SignatureOfMetric->Automatic};


SetCMetric[metric_,chart_?ChartQ,rest___]:=SetCMetric[metric,Zero,chart,rest];
SetCMetric[metric:CTensor[matrix_,{-basis_,-basis_}],torsion_,chart_?ChartQ,options___?OptionQ]:=Module[{vb,signature},

(* Precompute various objects *)
MetricCompute[metric,torsion,chart,{"Christoffel"[1,-1,-1],"Torsion"[1,-1,-1]},FilterRules[{options},Options[MetricCompute]]];

(* Register structure *)
AppendToUnevaluated[$Metrics,metric];
vb=VBundleOfBasis[basis];
If[MetricEndowedQ[vb],Message[SetCMetric::old,MetricsOfVBundle[vb],vb]];
With[{vbundle=vb},
xUpAppendTo[MetricsOfVBundle[vbundle],metric]
];

(* Register signature and determinant sign *)
signature=SignatureOfMetric/.Flatten[{options}]/.Options[SetCMetric];
If[signature===Automatic,signature=SignatureOfMetric[metric]];
If[VectorQ[signature,IntegerQ],
SignatureOfMetric[metric]^=signature;
SignDetOfMetric[metric]^=If[signature[[3]]>0,0,(-1)^signature[[2]]]
];

Null
];


CTensor/:DefMetric[_,_CTensor[_,_],___]:=Throw@Message[DefMetric::error,"Cannot declare a CTensor metric. Use SetCTensor for that."];


(* Extend properties *)
CTensor/:MetricQ[CTensor[matrix_,{-basis_,-basis_},_]]:=BasisQ[basis]&&SymmetricMatrixQ[matrix];
CTensor/:VBundleOfMetric[metric_CTensor?MetricQ]:=VBundleOfBasis[First[CTensorBases[metric]]];
CTensor/:FlatMetricQ[metric_CTensor?MetricQ]:=Not@CurvatureQ[CovDOfMetric[metric]];


UnsetCMetric[metric_CTensor]:=Module[{},
If[FreeQ[$Metrics,metric,1],Throw@Message[UnsetMetric::unknown,"metric",metric]];
With[{vb=VBundleOfBasis[First[CTensorBases[metric]]]},
xUpDeleteCasesTo[MetricsOfVBundle[vb],metric]
];
$Metrics=DeleteCases[$Metrics,metric];
];


CTensor/:UndefMetric[_CTensor]:=Throw@Message[UndefMetric::error,"Cannot undeclare a CTensor metric. Use UnsetCMetric for that."];


MetricCompute::mat="First argument of CTensor `1` must be a symmetric matrix.";
MetricCompute::notan="Metrics can only be defined on a tangent bundle.";
MetricCompute::chart1="Metric field components have mixed charts `1` on manifold `2`.";
MetricCompute::change="Cannot perform differentations in chart `1` because metric field is given with chart `2`.";


CMetricChartCheck[metric:CTensor[matrix_,{-basis_,-basis_},addweight_],torsion_,chart_]:=Module[{vb,dim,base,mcharts,res},

(* Checks of metric *)
If[!SymmetricMatrixQ[matrix],Throw@Message[MetricCompute::mat,matrix]];
If[!BasisQ[basis],Throw@Message[MetricCompute::unknown,"basis",basis]];
vb=VBundleOfBasis[basis];
dim=DimOfVBundle[vb];
If[Dimensions[matrix]=!={dim,dim},Throw@Message[MetricCompute::invalid,"dimensions",Dimensions[matrix]]];
If[TangentBundleOfManifold[BaseOfVBundle@vb]=!=vb,Throw@Message[MetricCompute::notan]];
(* Predict value of the chart argument *)
base=BaseOfVBundle[vb];
mcharts=Select[ChartsOf[matrix],(ManifoldOfChart[#]===base)&];
If[Length[mcharts]>1,Throw@Message[MetricCompute::chart1,mcharts,base]];

(* Checks of torsion *)
If[torsion=!=Zero&&SlotsOfTensor[torsion]=!={vb,-vb,-vb},Throw@Message[MetricCompute::invalid,"torsion tensor",torsion]];

(* Checks of chart *)
If[chart=!=Automatic,
If[!ChartQ[chart],Throw@Message[MetricCompute::invalid,"chart",chart]];
If[mcharts=!={}&&mcharts=!={chart},Throw@Message[MetricCompute::change,chart,First@mcharts]];
];

];
CMetricChartCheck[___]:=$Failed


Options[MetricCompute]={CVSimplify:>$CVSimplify,Verbose->False,Parallelize->False};
(* Backwards compatibility *)
MetricCompute[metric_,chart_?ChartQ,what_,options___]:=MetricCompute[metric,Zero,chart,what,options];
(* General case *)
MetricCompute[metric_,torsion_,chart_?ChartQ,what_,options___?OptionQ]:=
Module[{
case,symbolicQ,PDchart,basis,PDbasis,coords,dim,inds,deps,tensor,covd,data,matrix,array,time,inverse,
(* 24 pointers *)
BasisPointer,InverseBasisPointer,BasisChristoffel2Pointer,BasisTorsion1Pointer,BasisTorsion2Pointer,MetricPointer,Torsion1Pointer,Torsion2Pointer,DetMetricPointer,InvMetricPointer,DMetricPointer,DDMetricPointer,Christoffel1Pointer,Christoffel2Pointer,ADChristoffel1Pointer,RiemannDDDDPointer,RiemannDDDUPointer,RiemannDDUUPointer,RicciPointer,RicciScalarPointer,KretschmannPointer,EinsteinPointer,WeylPointer,CDRiemannDDDDPointer
},

(* Type of metric provided. Do not worry about the torsion *)
symbolicQ=Head[metric]=!=CTensor;

If[!symbolicQ,CMetricChartCheck[metric,torsion,chart]];

(* Decide basis for the components of the results *)
basis=If[symbolicQ,
chart,
First[-CTensorBases[metric]]
];

(* Decide case {basisQ, torsionQ} *)
case={basis=!=chart,torsion=!=Zero};

(* Setup *)
PDbasis=PDOfBasis[basis];
PDchart=PDOfBasis[chart];
coords=ScalarsOfChart[chart];
dim=Length[coords];
deps=Depends[what,case];
inds=GetIndicesOfVBundle[VBundleOfBasis[basis],5];

(* Register potential change of basis (convention is {-basis, chart})*)
tensor="Basis"[-1,1];
If[MemberQ[deps,tensor],
matrix=CTensorArray@ToCTensor[Basis,{-basis,chart}];
ReportComputeSet[tensor,matrix,BasisPointer,case,dim];
];

(* Register metric components *)
tensor="Metric"[-1,-1];
If[MemberQ[deps,tensor],
matrix=If[symbolicQ,
ComponentArray[metric,{-basis,-basis}]//ToValues,
CTensorArray[metric]
];
ReportComputeSet[tensor,matrix,MetricPointer,case,dim];
];

(* Register torsion components *)
tensor="Torsion"[1,-1,-1];
If[MemberQ[deps,tensor],
array=If[Head[torsion]===CTensor,
CTensorArray[torsion],
ComponentArray[torsion,{basis,-basis,-basis}]//ToValues
];
ReportComputeSet[tensor,array,Torsion2Pointer,case,dim];
];

(* Computations without knowledge of the full connection or the metric *)
data={{Automatic,Null},{basis,PDbasis},{chart,PDchart}};

(* Basis-associated objects *)
tensor="InverseBasis"[-1,1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,InverseBasisPointer,case,{BasisPointer},data,options]
];

tensor="BasisChristoffel"[1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,BasisChristoffel2Pointer,case,{BasisPointer,InverseBasisPointer},data,options]
];

tensor="BasisTorsion"[1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,BasisTorsion2Pointer,case,{BasisChristoffel2Pointer},data,options]
];

(* Computations without knowledge of the full connection *)
data={{metric,Null},{basis,PDbasis},{chart,PDchart}};

tensor="BasisTorsion"[-1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,BasisTorsion1Pointer,case,{BasisTorsion2Pointer,MetricPointer},data,options]
];

(* Metric-associated objects *)
tensor="DetMetric"[];
If[MemberQ[deps,tensor],
If[symbolicQ,Determinant[metric,chart][]];
ReportCompute[tensor,DetMetricPointer,case,{InverseBasisPointer,MetricPointer},data,options]
];

tensor="Metric"[1,1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,InvMetricPointer,case,{MetricPointer},data,options]
];

tensor="DMetric"[-1,-1,-1];
If[MemberQ[deps,tensor],
If[symbolicQ,Implode[PDchart[-inds[[3]]][metric[-inds[[1]],-inds[[2]]]]]];
ReportCompute[tensor,DMetricPointer,case,{BasisPointer,MetricPointer},data,options]
];

tensor="DDMetric"[-1,-1,-1,-1];
If[MemberQ[deps,tensor],
If[symbolicQ,Implode[PDchart[-inds[[4]]]@PDchart[-inds[[3]]][metric[-inds[[1]],-inds[[2]]]]]];
ReportCompute[tensor,DDMetricPointer,case,{DMetricPointer},data,options]
];

If[symbolicQ,
covd=CovDOfMetric[metric];
data={{metric,covd},{basis,PDbasis},{chart,PDchart}}
];

(* Lower first index of the torsion tensor *)
tensor="Torsion"[-1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,Torsion1Pointer,case,{Torsion2Pointer,MetricPointer},data,options]
];

tensor="Christoffel"[-1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,Christoffel1Pointer,case,{DMetricPointer,BasisTorsion1Pointer,Torsion1Pointer},data,options]
];

tensor="ADChristoffel"[-1,-1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,ADChristoffel1Pointer,case,{BasisPointer,Christoffel1Pointer},data,options]
];

tensor="Christoffel"[1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,Christoffel2Pointer,case,{InvMetricPointer,Christoffel1Pointer},data,options];
If[!symbolicQ ,
covd=CCovD[PDbasis,CTensor[Array[Christoffel2Pointer,{dim,dim,dim}],{basis,-basis,-basis},0],metric];
data={{metric,covd},{basis,PDbasis},{chart,PDchart}};
ReportCompute["CovDOfMetric",Christoffel2Pointer,case,{},data,options]
]
];

(* Computations needing the knowledge of the constructed CCovD *)
tensor="Riemann"[-1,-1,-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,RiemannDDDDPointer,case,{DDMetricPointer,ADChristoffel1Pointer,Christoffel1Pointer,Christoffel2Pointer,BasisTorsion2Pointer},data,options]
];

(* Algebraic computations *)
tensor="Riemann"[-1,-1,-1,1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,RiemannDDDUPointer,case,{InvMetricPointer,RiemannDDDDPointer},data,options]
];

tensor="Ricci"[-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,RicciPointer,case,{RiemannDDDUPointer},data,options]
];

tensor="RicciScalar"[];
If[MemberQ[deps,tensor],
ReportCompute[tensor,RicciScalarPointer,case,{InvMetricPointer,RicciPointer},data,options]
];

tensor="Einstein"[-1,-1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,EinsteinPointer,case,{RicciPointer,RicciScalarPointer,MetricPointer},data,options]
];

tensor="Weyl"[-1,-1,-1,-1];
If[!case[[2]]&&MemberQ[deps,tensor],
ReportCompute[tensor,WeylPointer,case,{RiemannDDDDPointer,RicciPointer,RicciScalarPointer,MetricPointer},data,options]
];

tensor="Riemann"[-1,-1,1,1];
If[MemberQ[deps,tensor],
ReportCompute[tensor,RiemannDDUUPointer,case,{InvMetricPointer,RiemannDDDUPointer},data,options]
];

tensor="Kretschmann"[];
If[MemberQ[deps,tensor],
ReportCompute[tensor,KretschmannPointer,case,{RiemannDDUUPointer},data,options]
];

(* Higher derivatives *)
tensor="CDRiemann"[-1,-1,-1,-1,-1];
If[MemberQ[deps,tensor],
If[symbolicQ,Implode[covd[-inds[[5]]][Riemann[covd][-inds[[1]],-inds[[2]],-inds[[3]],-inds[[4]]]]]];
ReportCompute[tensor,CDRiemannDDDDPointer,case,{BasisPointer,RiemannDDDDPointer,Christoffel2Pointer},data,options]
];

(* Pointers leave the module, and hence do not die automatically *)
Remove[BasisPointer,InverseBasisPointer,BasisChristoffel2Pointer,BasisTorsion1Pointer,BasisTorsion2Pointer,MetricPointer,Torsion1Pointer,Torsion2Pointer,DetMetricPointer,InvMetricPointer,DMetricPointer,DDMetricPointer,Christoffel1Pointer,Christoffel2Pointer,ADChristoffel1Pointer,RiemannDDDDPointer,RiemannDDDUPointer,RiemannDDUUPointer,RicciPointer,RicciScalarPointer,KretschmannPointer,EinsteinPointer,WeylPointer,CDRiemannDDDDPointer];
];
MetricCompute[_,_,_,___]:=Throw[MetricCompute::error,"Unspecified chart for field differentiation."];
SetNumberOfArguments[MetricCompute,{3,Infinity}];
Protect[MetricCompute];


(* Individual component *)
setSym["Symmetric"][head_[inds__]]:=Set[head[inds],Sort[head[inds]]];
setSym["Antisymmetric"][head_[inds__]]:=Set[head[inds],Signature[{inds}]Sort[head[inds]]];
setSym["Symmetric12"][head_[a_,b_,c__]]:=Set[head[a,b,c],head[Sequence@@Sort[{a,b}],c]];
setSym["Antisymmetric12"][head_[a_,b_,c__]]:=Set[head[a,b,c],Order[a,b]head[Sequence@@Sort[{a,b}],c]];
setSym["Symmetric23"][head_[a_,b_,c_,d___]]:=Set[head[a,b,c,d],head[a,Sequence@@Sort[{b,c}],d]];
setSym["Antisymmetric23"][head_[a_,b_,c_,d___]]:=Set[head[a,b,c,d],Order[b,c]head[a,Sequence@@Sort[{b,c}],d]];
setSym["DoubleSymmetric1234"][head_[a_,b_,c_,d_,e___]]:=Set[head[a,b,c,d,e],head@@(Flatten@{Sort[{a,b}],Sort[{c,d}],{e}})];
setSym["DoubleAntisymmetric1234"][head_[a_,b_,c_,d_,e___]]:=Set[head[a,b,c,d,e],Order[a,b]Order[c,d]head@@(Flatten@{Sort[{a,b}],Sort[{c,d}],{e}})];
setSym["Riemann1234"][head_[a_,b_,c_,d_,e___]]:=Set[head[a,b,c,d,e],Order[a,b]Order[c,d]head@@(Flatten@{Sort[{Sort[{a,b}],Sort[{c,d}]}],{e}})];
(* Whole object *)
setSymmetry[{rank_,""},_,_]:=Null;
setSymmetry[{rank_,symmetry_},head_,dim_Integer]:=setSymmetry[{rank,symmetry},head,ConstantArray[dim,rank]];
setSymmetry[{rank_,symmetry_},head_,dims_List]:=Scan[
setSym[symmetry],
If[Head[head]===Symbol,Clear[head]];
Array[head,dims],
{rank}
]


(* Note that the symmetry depends on the index configuration, contrary to standard xAct philosophy *)
RankSymmetry["Basis"[-1,1],case_]:={2,""};
RankSymmetry["InverseBasis"[-1,1],case_]:={2,""};
RankSymmetry["BasisChristoffel"[1,-1,-1],case_]:={3,""};
RankSymmetry["BasisTorsion"[1,-1,-1],case_]:={3,"Antisymmetric23"};
RankSymmetry["BasisTorsion"[-1,-1,-1],case_]:={3,"Antisymmetric23"};
RankSymmetry["Torsion"[1,-1,-1],case_]:={3,"Antisymmetric23"};
RankSymmetry["Torsion"[-1,-1,-1],case_]:={3,"Antisymmetric23"};
RankSymmetry["Metric"[-1, -1],case_] := {2, "Symmetric"};
RankSymmetry["Metric"[1, 1],case_] := {2, "Symmetric"};
RankSymmetry["DetMetric"[],case_] := {0, ""};
RankSymmetry["DMetric"[-1, -1, -1],case_] := {3, "Symmetric12"};
RankSymmetry["DDMetric"[-1, -1, -1, -1],{False,False}] := {4, "DoubleSymmetric1234"};
RankSymmetry["DDMetric"[-1, -1, -1, -1],case_]:=Throw[MetricCompute::error,"DDMetric can only be computed in the coordinated+torsionless case."];
RankSymmetry["Christoffel"[-1, -1, -1],{_,False}] := {3, "Symmetric23"};
RankSymmetry["Christoffel"[-1, -1, -1],{_,True}] := {3, ""};
RankSymmetry["Christoffel"[1, -1, -1],{_,False}] := {3, "Symmetric23"};
RankSymmetry["Christoffel"[1, -1, -1],{_,True}] := {3, ""};
RankSymmetry["ADChristoffel"[-1,-1,-1,-1],case_]:={4,"Antisymmetric12"};
RankSymmetry["Riemann"[-1, -1, -1, -1],{_,True}] := {4, "DoubleAntisymmetric1234"};
RankSymmetry["Riemann"[-1, -1, -1, -1],{_,False}] := {4, "Riemann1234"};
RankSymmetry["Riemann"[-1, -1, -1, 1],case_] := {4, "Antisymmetric12"};
RankSymmetry["Riemann"[-1, -1, 1, 1],case_] := {4, "DoubleAntisymmetric1234"};
RankSymmetry["Ricci"[-1, -1],{_,True}] := {2, ""};
RankSymmetry["Ricci"[-1, -1],{_,False}] := {2, "Symmetric"};
RankSymmetry["RicciScalar"[],case_] := {0, ""};
RankSymmetry["Einstein"[-1, -1],{_,True}] := {2, ""};
RankSymmetry["Einstein"[-1, -1],{_,False}] := {2, "Symmetric"};
RankSymmetry["Weyl"[-1,-1,-1,-1],{_,True}]:=Throw[MetricCompute::error,"Weyl tensor not defined when there is torsion."];
RankSymmetry["Weyl"[-1, -1, -1, -1],case_] := {4, "Riemann1234"};
RankSymmetry["Kretschmann"[],case_] := {0, ""};
RankSymmetry["CDRiemann"[-1, -1, -1, -1, -1],{_,True}] := {5, "DoubleAntisymmetric1234"};
RankSymmetry["CDRiemann"[-1, -1, -1, -1, -1],{_,False}] := {5, "Riemann1234"};



(* For symbolic metrics we do store results as TensorValues of corresponding objects *)
CacheKeySymbolic["DetMetric"[],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=GiveSymbol[Determinant,metric,chart];
CacheKeySymbolic["Metric"[-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=metric;
CacheKeySymbolic["Metric"[1,1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=metric;
CacheKeySymbolic["DMetric"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=SymbolJoin[pdb,metric];
CacheKeySymbolic["DDMetric"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=SymbolJoin[pd,pd,metric];
CacheKeySymbolic["Christoffel"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=
HeadOfTensor[Christoffel[covd,pdb]@@#,#]&@GetIndicesOfVBundle[VBundleOfMetric[metric],3];
CacheKeySymbolic["Torsion"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Torsion[covd];
CacheKeySymbolic["Riemann"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Riemann[covd];
CacheKeySymbolic["Ricci"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Ricci[covd];
CacheKeySymbolic["RicciScalar"[],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=RicciScalar[covd];
CacheKeySymbolic["Einstein"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Einstein[covd];
CacheKeySymbolic["Weyl"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Weyl[covd];
CacheKeySymbolic["Kretschmann"[],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Kretschmann[covd];
CacheKeySymbolic["CDRiemann"[__],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=SymbolJoin[covd,Riemann[covd]];
CacheKeySymbolic[_,{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Null;


CacheKeyCTensor["BasisChristoffel"[1,-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Christoffel[pdb,pd]];
CacheKeyCTensor["BasisTorsion"[1,-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Torsion[pdb]];
CacheKeyCTensor["BasisTorsion"[-1,-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=With[{torsion=Torsion[pdb]},Hold[ToCTensorOneSlot[torsion,{1,metric}]]];
CacheKeyCTensor["DetMetric"[],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Determinant[metric,chart]];
CacheKeyCTensor["Metric"[1,1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Inv[metric]];
CacheKeyCTensor["Riemann"[-1,-1,-1,1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Riemann[covd]];
CacheKeyCTensor["Riemann"[-1,-1,-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[RiemannDown[covd]];
CacheKeyCTensor["Riemann"[-1,-1,1,1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=With[{riem=Riemann[covd],invmetric=Inv[metric]},Hold[ToCTensorOneSlot[riem,{3,invmetric}]]];
CacheKeyCTensor["Ricci"[-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Ricci[covd]];
CacheKeyCTensor["RicciScalar"[],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[RicciScalar[covd]];
CacheKeyCTensor["Einstein"[-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Einstein[covd]];
CacheKeyCTensor["Weyl"[-1,-1,-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Weyl[covd]];
CacheKeyCTensor["Kretschmann"[],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=Hold[Kretschmann[covd]];
CacheKeyCTensor["CDRiemann"[-1,-1,-1,-1,-1],{{metric_,covd_},{basis_,pdb_},{chart_,pd_}}]:=With[{riemdown=RiemannDown[covd]},Hold[TensorDerivative[riemdown,covd]]];
CacheKeyCTensor[_,{{metric_CTensor,covd_},{basis_,pdb_},{chart_,pd_}}]:=Null;


ReportComputeSet[object_,array_,pointer_,case_,dim_]:=Module[{ranksym=RankSymmetry[object,case]},
setSymmetry[ranksym,pointer,dim];
ComputeSet[object,array,pointer,case,dim];
MCPrint[object,": ",Array[pointer,ConstantArray[dim,First[ranksym]]]];
];


SetValueFromArray[pointer_[pos___],array_]:=Set[pointer[pos],Part[array,pos]];
SetPointerFromCTensor[symdownvalues_,pointer_,CTensor[array_,bases_,addweight_]]:=Module[{deps},
deps=DeleteDuplicates[Cases[DeleteDuplicates[Last/@symdownvalues],_pointer,Infinity]];
SetValueFromArray[#,array]&/@deps
];


(* Special case *)
ReportCompute["CovDOfMetric",pointer_,case_,pointers_List,data:{{metric_,covd_},{basis_,PDbasis_},{chart_,_}},options:OptionsPattern[]]:=
Module[{verbose,tmp},

verbose=OptionValue[MetricCompute,{options},Verbose];

If[verbose,Print["** ReportCompute: ","CovDOfMetric"]];

tmp=xCobaCacheTable[CovDOfMetric[metric],CovDOfMetric];
If[Head[tmp]===xCobaCacheTable,
(* First time caching *)
xCobaCache[Christoffel[covd,PDbasis],covd[[2]],Christoffel,Identity];
xCobaCache[CovDOfMetric[metric],covd,CovDOfMetric,Identity],
If[tmp===covd,
(* Information has not changed *)
If[verbose,Print["    Already cached"]],
(* Information has changed *)
ClearxCobaCache[CovDOfMetric[metric],CovDOfMetric];
xCobaCache[CovDOfMetric[metric],covd,CovDOfMetric,Identity];
If[verbose,Print["    Replaced cached value"]]
]
]

];


(* Arrange signs *)
signedbases[tensor_[upQs___],basis_]:=basis {upQs};


(* This function resets component values after applying the function cvsimplify.
Parallelization code contributed by Thomas *)
applycvsimplify[pointer_,cvsimplify_,indeps_,parallelize_]:=If[parallelize,
Module[{presimplify,postsimplify,i},
presimplify=pointer@@@indeps;
postsimplify=ParallelMap[cvsimplify,presimplify,DistributedContexts->None];
For[i=1,i<=Length@indeps,i++,
Set[pointer[##],postsimplify[[i]]]&@@indeps[[i]]
]
],
Map[applycvsimplify1[pointer,cvsimplify],indeps];
];
applycvsimplify1[head_,cvsimplify_][{comp___}]:=Set[head[comp],cvsimplify[head[comp]]];


(* Main computation *)
ReportCompute[object_,pointer_,case_,pointers_List,data:{{metric_,_},{basis_,_},{chart_,_}},options:OptionsPattern[]]:=
Module[
{dim,time,verbose,cvsimplify,parallelize,indepdownvalues,depdownvalues,changerule,result,indeps,cache,cachekey,
ranksym=RankSymmetry[object,case],
coords=ScalarsOfChart[chart],
bases=signedbases[object,basis],
ctensorQ=Head[metric]===CTensor||metric===Automatic
},

{verbose,cvsimplify,parallelize}=OptionValue[MetricCompute,{options},{Verbose,CVSimplify,Parallelize}];

If[verbose,Print["** ReportCompute: ",object]];

dim=Length[coords];

(* Set symmetry relations *)
setSymmetry[ranksym,pointer,dim];
depdownvalues=DownValues[pointer];

(* Check cache. If data is present, use it and return *)
cache=xCobaCacheTable["ReportCompute"[object,case,data,metric],object];
If[Head[cache]=!=xCobaCacheTable,
cache/.RuleDelayed[Verbatim[HoldPattern][Coba[pos___]],value_]:>Set[pointer[pos],value];
If[verbose,Print["    Already cached"]];
Return[]
];

(* Do computation and time it *)
time=First@AbsoluteTiming[
indeps=Compute[object,pointer,case,pointers,coords,dim];
];
If[verbose,Print["    Constructed in ",time," seconds"]];

(* Apply simplification function, and change sign if needed *)
time=First@AbsoluteTiming[
applycvsimplify[pointer,cvsimplify,indeps,parallelize];
];
If[verbose,Print["    Applied ",cvsimplify," in ",time," seconds"]];

(* Cache result, so that it is not recomputed. Do not Simplify again *)
indepdownvalues=Complement[DownValues[pointer],depdownvalues];
xCobaCache["ReportCompute"[object,case,data,metric],indepdownvalues/.pointer->Coba,object,Identity];

(* For debugging *)
MCPrint[object,": ",Array[pointer,ConstantArray[dim,First[ranksym]]]];

(* Cache/store result in associated functions *)
cachekey=If[ctensorQ,
CacheKeyCTensor[object,data],
CacheKeySymbolic[object,data]
];

If[cachekey=!=Null,

time=First@AbsoluteTiming[

If[ctensorQ,

(* Use xCobaCache *)
MakexCobaCache[cachekey,pointer,bases,dim];
result=Last/@DownValues[pointer],

(* Use TensorValues *)
With[
{key=If[Head[cachekey]===MultiplyHead,Last@cachekey,cachekey],
sign=If[Head[cachekey]===MultiplyHead,First@cachekey,1]},

Declarechangerule[changerule,pointer,key,sign,bases,CNumbersOf[basis]];
result=xTagSet[
{key,TensorValues[key,{bases}]},
FoldedRule[
Map[changerule,depdownvalues],
Map[changerule,indepdownvalues]
]
];
xAct`xCoba`Private`UpdateTensorValIDs[ValID[key,{bases}]]
]

]

];

If[verbose,Print["    Stored in ",time," seconds and ",ByteCount[result]," bytes"]]

]

];

(* Store result of computation in xCobaCacheTable *)
MakexCobaCache[Hold[computation:head_[___]],pointer_,bases_,dim_]:=With[{array=Array[pointer,dim&/@bases],weight=If[head===Determinant,2Hold[computation][[1,2]],0]},
xCobaCache[
computation,
CTensor[array,bases,weight],
head,
Identity
]
];
MakexCobaCache[Null,__]:=Null;

(* Manipulation of downvalues to convert them into rules for our FoldedRule *)
Declarechangerule[changerule_Symbol,pointer_Symbol,key_Symbol,sign_,bases_List,cnumbers_List]:=(
(* Independent rule with no value: remove from list *)
changerule[rule:(ruledelayed_[_[pointer[inds___]],pointer[inds___]])]:=Sequence[]/;ruledelayed===RuleDelayed;
(* Dependent rule, possibly containing a sign *)
changerule[rule:(_[pointer[inds1___]]:>s_. pointer[inds2___])]:=Inner[List,cnumbers[[{inds1}]],bases,key]->s Inner[List,cnumbers[[{inds2}]],bases,key];
(* Independent rule with a value *)
changerule[rule:(_[pointer[inds___]]:>expr_)]:=Inner[List,cnumbers[[{inds}]],bases,key]->sign expr;
);


(* Input metric *)
ComputeSet["Metric"[-1,-1],matrix_,MetricPointer_,case_,dim_]:=
Reap[
Table[
Sow[{i,j}];
MetricPointer[i,j]=matrix[[i,j]],
{i,1,dim},{j,i,dim}
][[-1,1]]
];


(* Change of basis *)
ComputeSet["Basis"[-1,1],change_,BasisPointer_,case_,dim_]:=
Reap[
Table[
Sow[{i,j}];
BasisPointer[i,j]=change[[i,j]],
{i,1,dim},{j,1,dim}
][[-1,1]]
];


(* Torsion of the covd *)
ComputeSet["Torsion"[1,-1,-1],array_,Torsion2Pointer_,case_,dim_]:=
Reap[
Table[
Sow[{i,j,k}];
Torsion2Pointer[i,j,k]=array[[i,j,k]],
{i,1,dim},{j,1,dim},{k,j,dim}
][[-1,1]]
];


(* Lower first slot of the torsion tensor *)
Compute["Torsion"[-1,-1,-1],Torsion1Pointer_,{_,True},{Torsion2Pointer_,MetricPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{k,i,j}];
Torsion1Pointer[k,i,j]=Sum[MetricPointer[k,l]Torsion2Pointer[l,i,j],{l,1,dim}],
{i,1,dim},{j,i,dim},{k,1,dim}]
][[-1,1]];


(* Inverse change of basis with second argument {-chart, basis} *)
Compute["InverseBasis"[-1,1],InverseBasisPointer_,{True,_},{BasisPointer_},coords_,dim_]:=Module[{inverse},
inverse=Inverse@Array[BasisPointer,{dim,dim}];
Reap[
Table[
Sow[{i,j}];
InverseBasisPointer[i,j]=inverse[[i,j]],
{i,1,dim},{j,1,dim}]
][[-1,1]]
];

(* Christoffel[PDbasis, PDchart] *)
Compute["BasisChristoffel"[1,-1,-1],BasisChristoffel2Pointer_,{True,_},{BasisPointer_,InverseBasisPointer_},coords_,dim_]:=Module[{basis,bchr},
basis=Array[BasisPointer,{dim,dim}];
bchr=Transpose[basis.D[Array[InverseBasisPointer,{dim,dim}],{coords}].Transpose[basis],{3,1,2}];
Reap[
Table[
Sow[{i,j,k}];
BasisChristoffel2Pointer[i,j,k]=bchr[[i,j,k]],
{i,1,dim},{j,1,dim},{k,1,dim}]
][[-1,1]]
];

(* Torsion of the PDbasis *)
Compute["BasisTorsion"[1,-1,-1],BasisTorsion2Pointer_,{True,_},{BasisChristoffel2Pointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j,k}];
BasisTorsion2Pointer[i,j,k]=$TorsionSign(BasisChristoffel2Pointer[i,j,k]-BasisChristoffel2Pointer[i,k,j]),
{i,1,dim},{j,1,dim},{k,j,dim}]
][[-1,1]];
Compute["BasisTorsion"[-1,-1,-1],BasisTorsion1Pointer_,{True,_},{BasisTorsion2Pointer_,MetricPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j,k}];
BasisTorsion1Pointer[i,j,k]=Sum[MetricPointer[i,l]BasisTorsion2Pointer[l,j,k],{l,1,dim}],
{i,1,dim},{j,1,dim},{k,j,dim}]
][[-1,1]];


(* Determinant of the metric components, always in the coordinated basis *)
Compute["DetMetric"[],DetMetricPointer_,{False,_},{_,MetricPointer_},coords_,dim_]:=Module[{},
DetMetricPointer[]=Det[Array[MetricPointer,{dim,dim}]];
{{}}
];
Compute["DetMetric"[],DetMetricPointer_,{True,_},{InverseBasisPointer_,MetricPointer_},coords_,dim_]:=Module[{},
DetMetricPointer[]=Det[Array[MetricPointer,{dim,dim}]]Det[Array[InverseBasisPointer,{dim,dim}]]^2;
{{}}
];

(* Inverse of the metric *)
Compute["Metric"[1,1],InvMetricPointer_,{_,_},{MetricPointer_},coords_,dim_]:=Module[{inverse},
inverse=Inverse[Array[MetricPointer,{dim,dim}]];
Reap[
Table[
Sow[{i,j}];
InvMetricPointer[i,j]=inverse[[i,j]],
{i,1,dim},{j,i,dim}]
][[-1,1]]
];

(* First derivative of the basis-components of the metric, with new slot also in the basis *)
Compute["DMetric"[-1,-1,-1],DMetricPointer_,{False,_},{_,MetricPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j,k}];
DMetricPointer[i,j,k]=D[MetricPointer[i,j],coords[[k]]],
{i,1,dim},{j,i,dim},{k,1,dim}]
][[-1,1]];
Compute["DMetric"[-1,-1,-1],DMetricPointer_,{True,_},{BasisPointer_,MetricPointer_},coords_,dim_]:=Module[{dmetric},
dmetric=D[Array[MetricPointer,{dim,dim}],{coords}].Transpose[Array[BasisPointer,{dim,dim}]];
Reap[
Table[
Sow[{i,j,k}];
DMetricPointer[i,j,k]=dmetric[[i,j,k]],
{i,1,dim},{j,i,dim},{k,1,dim}]
][[-1,1]]
];

(* Christoffel of the first kind, between the main derivative and the PDbasis, all in basis components *)
Compute["Christoffel"[-1,-1,-1],Christoffel1Pointer_,{basisQ_,torsionQ_},{DMetricPointer_,BasisTorsion1Pointer_,Torsion1Pointer_},coords_,dim_]:=
Reap[
Table[
Sow[{k,i,j}];
Christoffel1Pointer[k,i,j]=1/2(DMetricPointer[k,i,j]+DMetricPointer[k,j,i]-DMetricPointer[i,j,k])+If[basisQ,$TorsionSign/2(BasisTorsion1Pointer[i,j,k]+BasisTorsion1Pointer[j,i,k]-BasisTorsion1Pointer[k,i,j]),0]+If[torsionQ,-$TorsionSign/2(Torsion1Pointer[i,j,k]+Torsion1Pointer[j,i,k]-Torsion1Pointer[k,i,j]),0],
{i,1,dim},{j,1,dim},{k,1,dim}]
][[-1,1]];

(* Christoffel of the second kind. Just an algebraic operation from the first kind, all in basis components *)
Compute["Christoffel"[1,-1,-1],Christoffel2Pointer_,{False,False},{InvMetricPointer_,Christoffel1Pointer_},coords_,dim_]:=
Reap[
Table[
Sow[{k,i,j}];
Christoffel2Pointer[k,i,j]=Sum[InvMetricPointer[k,l]Christoffel1Pointer[l,i,j],{l,1,dim}],
{i,1,dim},{j,i,dim},{k,1,dim}]
][[-1,1]];
Compute["Christoffel"[1,-1,-1],Christoffel2Pointer_,case_,{InvMetricPointer_,Christoffel1Pointer_},coords_,dim_]:=
Reap[
Table[
Sow[{k,i,j}];
Christoffel2Pointer[k,i,j]=Sum[InvMetricPointer[k,l]Christoffel1Pointer[l,i,j],{l,1,dim}],
{i,1,dim},{j,1,dim},{k,1,dim}]
][[-1,1]];


(* Second derivatives of the metric in the coordinated frame, for faster computation of curvature in this case *)
Compute["DDMetric"[-1,-1,-1,-1],DDMetricPointer_,{False,False},{DMetricPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j,k,l}];
DDMetricPointer[i,j,k,l]=D[DMetricPointer[i,j,k],coords[[l]]],
{i,1,dim},{j,i,dim},{k,1,dim},{l,k,dim}]
][[-1,1]];

(* Antisymmetrization of the first derivative of the Christtofel of the first kind *)
Compute["ADChristoffel"[-1,-1,-1,-1],ADChristoffel1Pointer_,{basisQ_,torsionQ_},{BasisPointer_,Christoffel1Pointer_},coords_,dim_]:=Module[{dchristoffel},
dchristoffel=If[basisQ,
D[Array[Christoffel1Pointer,{dim,dim,dim}],{coords}].Transpose[Array[BasisPointer,{dim,dim}]],
D[Array[Christoffel1Pointer,{dim,dim,dim}],{coords}]
];
Reap[
Table[
Sow[{i,j,k,l}];
ADChristoffel1Pointer[i,j,k,l]=dchristoffel[[l,i,k,j]]-dchristoffel[[l,j,k,i]],
{i,1,dim},{j,i,dim},{k,1,dim},{l,1,dim}]
][[-1,1]]
];

(* Compute curvature, with all indices down and with basis components *)
Compute["Riemann"[-1,-1,-1,-1],RiemannDDDDPointer_,{False,False},{DDMetricPointer_,_,Christoffel1Pointer_,Christoffel2Pointer_,_},coords_,dim_]:=
Reap[
Block[{i,j,k,l},
For[i=1,i<dim,i++,
For[j=i+1,j<=dim,j++,
For[k=i,k<dim,k++,
For[l=If[k==i,j,k+1],l<=dim,l++,
Sow[{i,j,k,l}];
RiemannDDDDPointer[i,j,k,l]=$RiemannSign(-1/2DDMetricPointer[j,l,i,k]+1/2DDMetricPointer[i,l,j,k]+1/2DDMetricPointer[j,k,i,l]-1/2DDMetricPointer[i,k,j,l]+Sum[Christoffel1Pointer[m,j,k]Christoffel2Pointer[m,i,l]-Christoffel1Pointer[m,j,l]Christoffel2Pointer[m,i,k],{m,1,dim}])
]
]
]
]
]
][[-1,1]];
Compute["Riemann"[-1,-1,-1,-1],RiemannDDDDPointer_,{basisQ_,_},{_,ADChristoffel1Pointer_,Christoffel1Pointer_,Christoffel2Pointer_,BasisTorsion2Pointer_},coords_,dim_]:=
Reap[
Block[{i,j,k,l},
For[i=1,i<dim,i++,
For[j=i+1,j<=dim,j++,
For[k=i,k<dim,k++,
For[l=If[k==i,j,k+1],l<=dim,l++,
Sow[{i,j,k,l}];
RiemannDDDDPointer[i,j,k,l]=$RiemannSign(ADChristoffel1Pointer[i,j,k,l]+Sum[Christoffel1Pointer[m,j,k]Christoffel2Pointer[m,i,l]-Christoffel1Pointer[m,j,l]Christoffel2Pointer[m,i,k]-If[basisQ,$TorsionSign BasisTorsion2Pointer[m,i,j]Christoffel1Pointer[l,m,k],0],{m,1,dim}])
]
]
]
]
]
][[-1,1]];

(* Raise last index of Riemann. Symmetry is always just "Antisymmetric12" *)
Compute["Riemann"[-1,-1,-1,1],RiemannDDDUPointer_,case_,{InvMetricPointer_,RiemannDDDDPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j,k,l}];
RiemannDDDUPointer[i,j,k,l]=Sum[InvMetricPointer[l,m]RiemannDDDDPointer[i,j,k,m],{m,1,dim}],
{i,1,dim},{j,i,dim},{k,1,dim},{l,1,dim}]
][[-1,1]];

(* With torsion the Ricci tensor is not symmetric *)
Compute["Ricci"[-1,-1],RicciPointer_,{_,True},{RiemannDDDUPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j}];
RicciPointer[i,j]=$RicciSign Sum[RiemannDDDUPointer[i,k,j,k],{k,1,dim}],
{i,1,dim},{j,1,dim}]
][[-1,1]];
Compute["Ricci"[-1,-1],RicciPointer_,{_,False},{RiemannDDDUPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j}];
RicciPointer[i,j]=$RicciSign Sum[RiemannDDDUPointer[i,k,j,k],{k,1,dim}],
{i,1,dim},{j,i,dim}]
][[-1,1]];

(* Ricci scalar *)
Compute["RicciScalar"[],RicciScalarPointer_,case_,{InvMetricPointer_,RicciPointer_},coords_,dim_]:=(
RicciScalarPointer[]=Sum[InvMetricPointer[i,j]RicciPointer[i,j],{i,1,dim},{j,1,dim}];
{{}}
);

(* DDUU Riemann. Symmetry is always "DoubleAntisymmetric1234" *)
Compute["Riemann"[-1,-1,1,1],RiemannDDUUPointer_,case_,{InvMetricPointer_,RiemannDDDUPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j,k,l}];
RiemannDDUUPointer[i,j,k,l]=Sum[InvMetricPointer[k,m]RiemannDDDUPointer[i,j,m,l],{m,1,dim}],
{i,1,dim},{j,i,dim},{k,1,dim},{l,k,dim}]
][[-1,1]];

(* Kretschmann scalar *)
Compute["Kretschmann"[],KretschmannPointer_,case_,{RiemannDDUUPointer_},coords_,dim_]:=Module[{},
KretschmannPointer[]=Sum[RiemannDDUUPointer[i,j,k,l]RiemannDDUUPointer[k,l,i,j],{i,1,dim},{j,1,dim},{k,1,dim},{l,1,dim}];
{{}}
];

(* Again, with torsion the Einstein tensor is not symmetric *)
Compute["Einstein"[-1,-1],EinsteinPointer_,{_,True},{RicciPointer_,RicciScalarPointer_,MetricPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j}];
EinsteinPointer[i,j]=If[dim<3,0,RicciPointer[i,j]-RicciScalarPointer[]MetricPointer[i,j]/2],{i,1,dim},{j,1,dim}]
][[-1,1]];
Compute["Einstein"[-1,-1],EinsteinPointer_,{_,False},{RicciPointer_,RicciScalarPointer_,MetricPointer_},coords_,dim_]:=
Reap[
Table[
Sow[{i,j}];
EinsteinPointer[i,j]=If[dim<3,0,RicciPointer[i,j]-RicciScalarPointer[]MetricPointer[i,j]/2],{i,1,dim},{j,i,dim}]
][[-1,1]];

(* Weyl tensor. Here we should never have torsion  *)
Compute["Weyl"[-1,-1,-1,-1],WeylPointer_,{_,False},{RiemannDDDDPointer_,RicciPointer_,RicciScalarPointer_,MetricPointer_},coords_,dim_]:=
Reap[
Block[{i,j,k,l},
For[i=1,i<dim,i++,
For[j=i+1,j<=dim,j++,
For[k=i,k<dim,k++,
For[l=If[k==i,j,k+1],l<=dim,l++,
Sow[{i,j,k,l}];
WeylPointer[i,j,k,l]=If[dim<4,0,RiemannDDDDPointer[i,j,k,l]-$RicciSign/(dim-2)(MetricPointer[i,k]RicciPointer[j,l]-MetricPointer[i,l]RicciPointer[j,k]-MetricPointer[j,k]RicciPointer[i,l]+MetricPointer[j,l]RicciPointer[i,k])+$RicciSign/(dim-1)/(dim-2)RicciScalarPointer[](MetricPointer[i,k]MetricPointer[j,l]-MetricPointer[i,l]MetricPointer[j,k])]
]
]
]
]
]
][[-1,1]];


(* First derivative of Riemann *)
Compute["CDRiemann"[-1,-1,-1,-1,-1],CDRiemannDDDDPointer_,{basisQ_,torsionQ_},{BasisPointer_,RiemannDDDDPointer_,Christoffel2Pointer_},coords_,dim_]:=Module[{DRiemannDDDDPointer,result},
Block[{i,j,k,l,m},
(* Coordinated derivatives *)
For[i=1,i<dim,i++,
For[j=i+1,j<=dim,j++,
For[k=If[torsionQ,1,i],k<dim,k++,
For[l=If[torsionQ,k+1,If[k==i,j,k+1]],l<=dim,l++,
For[m=1,m<=dim,m++,
DRiemannDDDDPointer[i,j,k,l,m]=D[RiemannDDDDPointer[i,j,k,l],coords[[m]]]
]
]
]
]
];
(* Complete covariant derivative *)
result=Reap[
For[i=1,i<dim,i++,
For[j=i+1,j<=dim,j++,
For[k=If[torsionQ,1,i],k<dim,k++,
For[l=If[torsionQ,k+1,If[k==i,j,k+1]],l<=dim,l++,
For[m=1,m<=dim,m++,
Sow[{i,j,k,l,m}];
CDRiemannDDDDPointer[i,j,k,l,m]=If[basisQ,
Sum[DRiemannDDDDPointer[i,j,k,l,n]BasisPointer[m,n],{n,1,dim}],
DRiemannDDDDPointer[i,j,k,l,m]
]-Sum[
Christoffel2Pointer[n,m,i]RiemannDDDDPointer[n,j,k,l]
+Christoffel2Pointer[n,m,j]RiemannDDDDPointer[i,n,k,l]
+Christoffel2Pointer[n,m,k]RiemannDDDDPointer[i,j,n,l]
+Christoffel2Pointer[n,m,l]RiemannDDDDPointer[i,j,k,n],{n,1,dim}]
]
]
]
]
]
][[-1,1]]
];
Remove[DRiemannDDDDPointer];
result
];


Depends[x_,case_]:=Union@Flatten@FixedPointList[DeleteCases[depends[#,case],Null]&,x];
depends[list_List,case_]:=Union@Flatten[depends[#,case]&/@list];
depends["Metric"[-1,-1],{_,_}]={};
depends["Basis"[-1,1],{_,_}]={};
depends["Torsion"[1,-1,-1],{_,_}]={};
depends["Torsion"[-1,-1,-1],{_,True}]={"Torsion"[1,-1,-1],"Metric"[-1,-1]};
depends["InverseBasis"[-1,1],{True,_}]={"Basis"[-1,1]};
depends["BasisChristoffel"[1,-1,-1],{True,_}]={"Basis"[-1,1],"InverseBasis"[-1,1]};
depends["BasisTorsion"[1,-1,-1],{True,_}]={"BasisChristoffel"[1,-1,-1]};
depends["BasisTorsion"[-1,-1,-1],{True,_}]={"BasisTorsion"[1,-1,-1],"Metric"[-1,-1]};
depends["DetMetric"[],{basisQ_,_}]:={If[basisQ,"InverseBasis"[-1,1]],"Metric"[-1,-1]};
depends["Metric"[1,1],{_,_}]={"Metric"[-1,-1]};
depends["DMetric"[-1,-1,-1],{basisQ_,_}]:={If[basisQ,"Basis"[-1,1]],"Metric"[-1,-1]};
depends["DDMetric"[-1,-1,-1,-1],{False,False}]={"DMetric"[-1,-1,-1]};
depends["ADChristoffel"[-1,-1,-1,-1],case_]={"Basis"[-1,1],"Christoffel"[-1,-1,-1]};
depends["Christoffel"[-1,-1,-1],{basisQ_,torsionQ_}]:={"DMetric"[-1,-1,-1],If[basisQ,"BasisTorsion"[-1,-1,-1]],If[torsionQ,"Torsion"[-1,-1,-1]]};
depends["Christoffel"[1,-1,-1],{_,_}]={"Metric"[1,1],"Christoffel"[-1,-1,-1]};
depends["Riemann"[-1,-1,-1,-1],{basisQ_,torsionQ_}]:={If[basisQ||torsionQ,"ADChristoffel"[-1,-1,-1,-1],"DDMetric"[-1,-1,-1,-1]],"Christoffel"[1,-1,-1],"Christoffel"[-1,-1,-1],If[basisQ,"BasisTorsion"[1,-1,-1]]};
depends["Riemann"[-1,-1,-1,1],{_,_}]={"Metric"[1,1],"Riemann"[-1,-1,-1,-1]};
depends["Riemann"[-1,-1,1,1],{_,_}]={"Metric"[1,1],"Riemann"[-1,-1,-1,1]};
depends["Ricci"[-1,-1],{_,_}]={"Riemann"[-1,-1,-1,1]};
depends["RicciScalar"[],{_,_}]={"Metric"[1,1],"Ricci"[-1,-1]};
depends["Kretschmann"[],{_,_}]={"Riemann"[-1,-1,1,1]};
depends["Einstein"[-1,-1],{_,_}]={"Ricci"[-1,-1],"RicciScalar"[],"Metric"[-1,-1]};
depends["Weyl"[-1,-1,-1,-1],{_,False}]={"Riemann"[-1,-1,-1,-1],"Ricci"[-1,-1],"RicciScalar"[],"Metric"[-1,-1]};
depends["CDRiemann"[-1,-1,-1,-1,-1],{basisQ_,_}]:={If[basisQ,"Basis"[-1,1]],"Riemann"[-1,-1,-1,-1],"Christoffel"[1,-1,-1]};
Depends[All,{basisQ_,torsionQ_}]:=Join[
If[basisQ,{"Basis"[-1,1],"InverseBasis"[-1,1],"BasisChristoffel"[1,-1,-1],"BasisTorsion"[1,-1,-1],"BasisTorsion"[-1,-1,-1]},{}],
{"Torsion"[1,-1,-1],"Metric"[-1,-1],"Metric"[1,1],"DetMetric"[],"DMetric"[-1,-1,-1],"Christoffel"[-1,-1,-1],"Christoffel"[1,-1,-1],"Riemann"[-1,-1,-1,-1],"Riemann"[-1,-1,-1,1],"Riemann"[-1,-1,1,1],"Ricci"[-1,-1],"Einstein"[-1,-1],"RicciScalar"[],"Kretschmann"[],"CDRiemann"[-1,-1,-1,-1,-1]},
If[basisQ||torsionQ,{"ADChristoffel"[-1,-1,-1,-1]},{"DDMetric"[-1,-1,-1,-1]}],
If[torsionQ,{"Torsion"[-1,-1,-1]},{"Weyl"[-1,-1,-1,-1]}]
];
depends[x_,case_]:=(Message[MetricCompute::error,"Unknown dependencies of "<>ToString[x]];{});


MetricChristoffel[metric:CTensor[_,{-basis_,-basis_},0],pd_]:=Module[{a,b,c,d,torsion,mtorsion,invmetric},
{a,b,c,d}=GetIndicesOfVBundle[VBundleOfMetric[metric],4];
torsion=ToCTensor[Torsion[pd],{basis,-basis,-basis}];
mtorsion=CTensorContract[metric,torsion,{2,1},Times];
invmetric=Inv[metric];
HeadOfTensor[
1/2invmetric[a,d](pd[-b][metric[-c,-d]]+pd[-c][metric[-b,-d]]-pd[-d][metric[-b,-c]])+$TorsionSign/2invmetric[a,d](mtorsion[-b,-c,-d]+mtorsion[-c,-b,-d]-mtorsion[-d,-b,-c]),
{a,-b,-c}]
];


(computation:LC[metric:CTensor[_?MatrixQ,{-basis_,-basis_},0]])^:=xCobaCache[
computation,
With[{pd=PDOfBasis[basis]},CCovD[pd,MetricChristoffel[metric,pd],metric]]
];


AddTorsionToCCovD[CCovD[pd_,christoffel_,metric_],torsion_]:=Module[{a,b,c,d,invmetric,mtorsion},
{a,b,c,d}=GetIndicesOfVBundle[VBundleOfMetric[metric],4];
mtorsion=CTensorContract[metric,torsion,{2,1},Times];
invmetric=Inv[metric];
CCovD[pd,
HeadOfTensor[
christoffel[a,-b,-c]-$TorsionSign/2invmetric[a,d](mtorsion[-b,-c,-d]+mtorsion[-c,-b,-d]-mtorsion[-d,-b,-c]),
{a,-b,-c}],
metric]
];


(computation:CovDOfMetric[metric:CTensor[_?MatrixQ,{-basis_,-basis_},0],torsion_])^:=xCobaCache[
computation,
AddTorsionToCCovD[LC[metric],torsion]
];


(computation:CovDOfMetric[metric_CTensor])^:=xCobaCache[
computation,
LC[metric]
];


PDCTorsion[pd_,christoffel_]:=Module[{torsion=Torsion[pd]},
If[torsion[Null,Null,Null]===0,
Zero,
ToCTensor[torsion,CTensorBases[christoffel]]
]
];


(computation:Torsion[CCovD[pd_,christoffel_CTensor,metric_]])^:=xCobaCache[
computation,
PDCTorsion[pd,christoffel]+$TorsionSign(christoffel-CTensorTranspose[christoffel,Images[{1,3,2}]])
];


(computation:Riemann[CCovD[pd_,christoffel_,metric_]])^:=xCobaCache[
computation,
Module[{a,b,c,d,e},
{a,b,c,d,e}=GetIndicesOfVBundle[First@VBundlesOfCovD[pd],5];
HeadOfTensor[
pd[-b][christoffel[d,-a,-c]]-pd[-a][christoffel[d,-b,-c]]+christoffel[d,-b,-e]christoffel[e,-a,-c]-christoffel[d,-a,-e]christoffel[e,-b,-c]-$TorsionSign PDCTorsion[pd,christoffel][e,-a,-b]christoffel[d,-e,-c],
{-a,-b,-c,d}]
]
];


(computation:RiemannDown[ccovd:CCovD[pd_,christoffel_,metric_]])^:=xCobaCache[
computation,
CTensorContract[Riemann[ccovd],metric,{4,1},Times]
];


(computation:Ricci[ccovd_CCovD])^:=xCobaCache[
computation,
CTensorContract[Riemann[ccovd],{2,4}]
];


(computation:TFRicci[ccovd:CCovD[_,_,metric_]])^:=xCobaCache[
computation,
Ricci[ccovd]-metric RicciScalar[ccovd]/Length[CTensorArray[metric]]
]/;metric=!=Null||Throw[Message[Einstein::metcon,"TFRicci"]];


General::metcon="`1` can only be computed for a metric connection.";


(computation:RicciScalar[ccovd:CCovD[_,_,metric_]])^:=xCobaCache[
computation,
CTensorContract[CTensorContract[Ricci[ccovd],Inv[metric],{2,1},Times],{1,2}]
]/;metric=!=Null||Throw[Message[RicciScalar::metcon,"RicciScalar"]];


(computation:Einstein[ccovd:CCovD[_,_,metric_]])^:=xCobaCache[
computation,
Ricci[ccovd]-metric RicciScalar[ccovd]/2
]/;metric=!=Null||Throw[Message[Einstein::metcon,"Einstein"]];


(computation:Weyl[ccovd:CCovD[pd_,_,metric_]])^:=xCobaCache[
computation,
Module[{vbundle,a,b,c,d,dim,ricci,ricciscalar},
vbundle=First@VBundlesOfCovD[pd];
{a,b,c,d}=GetIndicesOfVBundle[vbundle,4];
dim=DimOfVBundle[vbundle];
ricci=Ricci[ccovd];
ricciscalar=RicciScalar[ccovd];
HeadOfTensor[
RiemannDown[ccovd][-a,-b,-c,-d]+(metric[-c,-b]ricci[-a,-d]+metric[-d,-a]ricci[-b,-c]-metric[-d,-b]ricci[-a,-c]-metric[-c,-a]ricci[-b,-d])/(dim-2)+(metric[-a,-c]metric[-b,-d]-metric[-a,-d]metric[-b,-c])ricciscalar[]/((dim-1)(dim-2)),
{-a,-b,-c,-d}
]	
]
]/;metric=!=Null||Throw[Message[Weyl::metcon,"Weyl"]];


(computation:Kretschmann[ccovd:CCovD[pd_,_,metric_]])^:=xCobaCache[
computation,
Module[{a,b,c,d},
{a,b,c,d}=GetIndicesOfVBundle[First@VBundlesOfCovD[pd],4];
CTensor[Riemann[ccovd][-a,-b,c,d]Riemann[ccovd][-c,-d,a,b],{},0]
]
]/;metric=!=Null||Throw[Message[Kretschmann::metcon,"Kretschmann"]];


collect[expr_,basis_,inds_,f_]:=Collect[expr,Flatten[BasisArray[basis]@@inds],f];


Options[InverseMetric]={CVSimplify:>$CVSimplify};
InverseMetric[metric_,basis_,options:OptionsPattern[]][a_?UpIndexQ,b_?UpIndexQ]:=With[{matrix=ComponentArray[ToBasis[basis][metric]],cvsimplify=OptionValue[InverseMetric,{options},CVSimplify]},collect[Plus@@(Plus@@(Inverse[cvsimplify@matrix]BasisArray[basis,basis][a,b])),basis,IndexList[a,b],cvsimplify]
];


Options[ChristoffelFromMetric]={CVSimplify:>$CVSimplify};
ChristoffelFromMetric[metric_,basis_,options:OptionsPattern[]][a_,b_,c_]:=With[{vbundle=VBundleOfIndex[a]},With[{PDbasis=PDOfBasis[basis],
VBmetric=xAct`xTensor`Private`FirstMetricOfVBundle[vbundle],
dummy1=DummyIn[vbundle,basis],
dummy2=DummyIn[vbundle,basis],
dummy3=DummyIn[vbundle,basis],
dummy4=DummyIn[vbundle,basis],
frees=List@@FindFreeIndices[metric],cvsimplify=OptionValue[ChristoffelFromMetric,{options},CVSimplify]},
collect[ToValues@ContractBasis@With[{newmetric=ReplaceIndex[metric,Thread[frees->{-dummy3,-dummy4}]]},
TraceBasisDummy[ToValues[
VBmetric[a,-dummy1]VBmetric[b,dummy2]VBmetric[c,dummy3]1/2InverseMetric[metric,basis,options][dummy1,dummy4](PDbasis[-dummy2][newmetric]
+PDbasis[-dummy3][newmetric/.-dummy3->-dummy2]
-PDbasis[-dummy4][newmetric/.-dummy4->-dummy2])],
IndexList[dummy1,dummy2,dummy3,dummy4]]
],
basis,IndexList[a,b,c],cvsimplify]
]
];


Options[RiemannFromChristoffel]={CVSimplify:>$CVSimplify};
RiemannFromChristoffel[christoffel_,basis_,options:OptionsPattern[]][a_,b_,c_,d_]:=With[{vbundle=VBundleOfIndex[a],inds=List@@FindFreeIndices[christoffel]},
With[{PDbasis=PDOfBasis[basis],
VBmetric=xAct`xTensor`Private`FirstMetricOfVBundle[vbundle],
dummy1=DummyIn[vbundle,basis],
dummy2=DummyIn[vbundle,basis],
dummy3=DummyIn[vbundle,basis],
dummy4=DummyIn[vbundle,basis],
dummy5=DummyIn[vbundle,basis],
cvsimplify=OptionValue[RiemannFromChristoffel,{options},CVSimplify]},
With[{newchristoffel=ReplaceIndex[christoffel,Thread[inds->{dummy4,-dummy1,-dummy3}]]},collect[ToValues@ContractBasis@TraceBasisDummy[ToValues[
VBmetric[a,dummy1]VBmetric[b,dummy2]VBmetric[c,dummy3]VBmetric[d,-dummy4](PDbasis[-dummy2][newchristoffel]
-PDbasis[-dummy1][newchristoffel/.-dummy1->-dummy2]
+(newchristoffel/.{-dummy1->-dummy2,-dummy3->-dummy5})(newchristoffel/.dummy4->dummy5)
-(newchristoffel/.-dummy3->-dummy5)(newchristoffel/.{dummy4->dummy5,-dummy1->-dummy2}))],IndexList[dummy1,dummy2,dummy3,dummy4,dummy5]],
basis,IndexList[a,b,c,d],cvsimplify]
]
]
];


End[]


EndPackage[]
