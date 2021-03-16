xAct`AVF`$Version={"1.0.0",{2011,12,18}};


(* :Title: AVF *)

(* :Author: Hugo D. Wahlquist *)

(* :Summary: Exterior calculus with algebra valued forms *)

(* :Brief Discussion:
   - Handles products of scalars, forms and algebra elements.
   - Works with general algebras, defined by giving the table of products.
   - Cartan-Kahler theory.
   - Dedicated code for Clifford algebras. Precomputed results for many of them.
   - Riemannian geometry with orthonormal (moving) frames.
   - Cartan characters.
*)
  
(* :Context: xAct`AVF` *)

(* :Package Version: 1.0.0 *)

(* :Copyright: Hugo D. Wahlquist (2003-2015). Since 2011, mantained by Frank B. Estabrook and Jose M. Martin-Garcia.
*)

(* :History: see AVF.History file *)

(* :Keywords: *)

(* :Source: AVF.nb *)

(* :Warning: This package does not talk to the other xAct packages *)

(* :Mathematica Version: 7.0 and later *)

(* :Limitations: *)


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`AVF`"];


BeginPackage["xAct`AVF`",{"xAct`xCore`"}];


Print[xAct`xCore`Private`bars];Print["Package xAct`AVF`  version 1.0, {2011, 12, 18}"];Print["CopyRight (C) 2003-2015, Hugo D. Wahlquist, under the General Public License."]


If[
xAct`xCore`Private`$LastPackage==="xAct`AVF`",Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]
];


Off[General::shdw]
xAct`AVF`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


adjmat::usage="adjmat[elems] returns the matrix adjmat.adjlist = argument_list.";
adL::usage="adL[elem, elem1] returns elem ** elem1. adL[elem, {elem1, elem2, ...}] returns {elem**elem1, elem**elem2, ...}.";
adR::usage="adR[elem1, elem] returns elem1 ** elem. adR[{elem1, elem2, ...}, elem] returns {elem1**elem, elem2**elem, ...}.";
applydrules::usage="applydrules[avf] implements expand[avf /. drules].";
associators::usage="associators[n] returns up to n associators for algebra.";
coeffs::usage="coeffs[avf] returns a list of the non-zero coefficients of elements in avf.";
collect::usage="collect[avf] collects the factors of elements. It displays the identity element.";
contract::usage="contract[v, avf] contracts the vector v on avf. It has infix form given by SmallCircle.";
contractedidentity::usage="contractedidentity[] returns the differential identity for the Einstein 3-form \!\(\*OverscriptBox[OverscriptBox[\(G\), \(^\)], \(\\\ \)]\).";
curvatureidentity::usage="curvatureidentity[] returns the Bianchi identity. It is a general identity for the CAV field \!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\).";
d::usage="d[avf] returns the exterior derivative of the argument.";
da::usage="da[avf] is a shortcut for applydrules[d[avf]].";
\[Delta]::usage="\[Delta][avf] is the coderivative for arguments expressed in frame. It equals hs[da[hs[avf]]].";
dot::usage="dot[pform1, pform2] gives the scalar product of arguments expressed in frame.";
embedsub::usage="";
expand::usage="expand[avf] replaces infix multiplication operator ** by \[Wedge].";
factor::usage="factor[basisform, avf] returns the righthand factor of basisform in avf.";
generic::usage="generic[] constructs a general algebra for elements using skew and symm.";
grade::usage="grade[integer, CAV] returns the specified grade of a CAV form.";
grades::usage="grades[CAV] returns a list of all grades of a CAV form.";
hs::usage="hs[avf] returns the Hodge dual of avf expressed in frame.";
in::usage="in[avf] converts avf to an equivalent List expression for AVF operations.";
involute::usage="involute[CAV] changes sign of all odd grades of a CAV form.";
lie::usage="lie[v, avf] returns the Lie derivative of avf with respect to the vector v.";
liealgebra::usage="liealgebra[] returns the Lie algebra of the dual vectors to non-exact forms.";
makealgebra::usage="makealgebra[productmatrix] establishes the multiplication table for elements.";
makeall::usage="makeall[] in CAV does makethetahat, makeomegahat, maketorsion and makecurvature.";
makeclifford::usage="makeclifford[signaturelist] makes Clifford algebra with specified signature.";
makecoords::usage="makecoords[symbols] defines a set of independent scalars and partials.";
makecurvature::usage="makecurvature[] calculates the CAV 2-form \!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\) from the connection 1-form \!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\).";
makedrules::usage="makedrules[replacementlist] establishes drules for dforms, plus optional Rules.";
makeeinstein::usage="makeeinstein[] makes a CAV 3-form \!\(\*OverscriptBox[\(G\), \(^\)]\)=(\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\) **\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)+\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)**\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\) )/2 (foudimensional Einstein tensor).";
makeelements::usage="makeelements[symbols] establishes the list of generators for an algebra.";
makeforms::usage="makeforms[symbols] establishes the list of fundamental non-exact 1-forms.";
makeframe::usage="makeframe[symbols] defines a set of orthonormal basis forms. They are stored in the global variable frame.";
makematrices::usage="makematrices[] constructs a matrix representation of the chosen Clifford algebra.";
makeomegahat::usage="makeomegahat[] makes the generalized CAV connection 1-form \!\(\*OverscriptBox[OverscriptBox[\(\[Omega]\), \(^\)], \(\\\ \)]\).";
makeorthonormalcurvature::usage="";
makescalars::usage="makescalars[symbols] establishes the list of fundamental scalars (0-forms).";
makethetahat::usage="makethetahat[] makes the generalized CAV co-frame 1-form \!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\).";
maketorsion::usage="maketorsion[] calculates the CAV 2-form \!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\) from the CAV 1-forms \!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\) and \!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\).";
makevectors::usage="makevectors[] makes a set of four lists named {vec1, vec2, vec3, vec4}.";
matmult::usage="matmult[m1, m2] performs multiplication of AVF matrices. It can be given in infix form using CircleTimes.";
mtx::usage="mtx[array] outputs the argument in MatrixForm. By default such argument is the global variable algebra.";
out::usage="out[listavf] converts back the given avf in list form to a product of scalar, form and element. It is the inverse function of in.";
prune::usage="prune[list, exp] keeps only those members of list that appear in exp.";
pullback::usage="pullback[avf] pulls back avf into the subspace sub.";
reset::usage="reset[] sets timer to zero.";
reverse::usage="reverse[CAV] returns the reversion of CAV; it is equivalent to reversing all \!\(\*SubscriptBox[\(\[Gamma]\), \(i\)]\) products.";
setzero::usage="setzero[list] turns each argument of list into an equation to 0. ";
subspace::usage="subspace[basislist] makes replacement list tosub for pullback.";
timer::usage="timer[] returns elapsed time from most recent reset.";
torsionidentity::usage="torsionidentity[] gives the general identity for CAV field \!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\). Also known as Cartan identity.";
zerocurvature::usage="zerocurvature[] constructs a list of drules giving vanishing CAV curvature \!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\).";
zerotorsion::usage="zerotorsion[] constructs a list of drules giving vanishing CAV torsion \!\(\*OverscriptBox[OverscriptBox[\(\[CapitalTheta]\), \(^\)], \(\\\ \)]\).";


adjlist::usage="adjlist is a global variable containing an arbitrary permutation of the list elements.";
algebra::usage="algebra is a global variable containing the multiplication table for the elements of an algebra.";
brackets::usage="brackets is a global variable containing the Lie brackets for non-exact dual vectors.";
cadim::usage="cadim is a global variable containing the number of generators in the orthonormal basis of a clifford algebra.";
clifflist::usage="clifflist is a global variable containing the list of the filenames in the CliffordAlgebra folder.";
coords::usage="coords is a global variable containing the list of scalars designated as independent variables.";
coordsdim::usage="coordsdim is a global variable containing the number of independent variables. It equals the length of the list coords.";
cosub::usage="cosub is a global variable containing the list of basis 1-forms to be annulled in a subspace.";
depend::usage="depend is a global variable containing the list coords as a Sequence. It is used for function arguments.";
dforms::usage="dforms is a global variable containing the list of symbols for the exterior derivatives of the members of forms.";
drules::usage="drules is a global variable containing the list of replacement rules for the exterior derivatives of forms.";
dscalars::usage="dscalars is a global variable containing the list of symbols for the exterior derivatives of the list scalars.";
eldim::usage="eldim is a global variable containing the number of algebra elements. It equals the length of the list elements.";
elements::usage="elements is a global variable containing the list of generators of the algebra specified by algebra.";
fderiv::usage="fderiv is a global variable containing the basic rules to give exterior derivatives of forms and dforms.";
fmdim::usage="fmdim is a global variable containing the number of independent basis 1-forms. It equals the length of the list forms.";
forms::usage="forms is a global variable containing the list of basis 1-forms generating the exterior algebra. It includes dscalars.";
frame::usage="frame is a global variable containing the list of orthonomal basis forms.";
frdim::usage="frdim is a global variable containing the number of orthonormal basis 1-forms. It equals the length of the list frame.";
gammas::usage="gammas is a global variable containing all Clifford elements, retrieved from a file in the CliffordAlgebras folder.";
identity::usage="identity represents the identity element of the initial trivial algebra.";
names::usage="names is a global variable containing the list of all AVF and CAV variables and functions.";
natural::usage="natural is a global variable containing the drules specifying closed basis 1-forms. This is the default value of drules.";
omegas::usage="omegas is a global variable containing the first list of graded symbols for a complete set of CAV 1-forms.";
scalars::usage="scalars is a global variable containing the list of 0-forms which will have exterior derivatives given by dscalars.";
scdim::usage="scdim is a global variable containing the number of scalars. It equals the length of the list scalars.";
sderiv::usage="sderiv is a global variable containing the basic rules to give exterior derivatives of scalars.";
sig::usage="sig[i] gives the ith eigenvalue, \!\(\*SubscriptBox[\(g\), \(ii\)]\),of the diagonal Clifford metric. It must be either +1 or -1.";
signature::usage="signature is a global variable containing the list of all sig[i]. It is the argument for makeclifford[signature].";
skew::usage="skew is a global variable containing the list of skew-symmetric structure constants: \!\(\*SubscriptBox[\(sk\), \(i, j, k\)]\)+\!\(\*SubscriptBox[\(sk\), \(i, k, j\)]\)=0.";
sk::usage="";
sub::usage="sub is a global variable containing the list of basis 1-forms spanning a subspace. It equals Complement[forms, cosub].";
symm::usage="symm is a global variable containing the list of symmetric structure constants: \!\(\*SubscriptBox[\(sy\), \(i, j, k\)]\)-\!\(\*SubscriptBox[\(sy\), \(i, k, j\)]\)=0.";
sy::usage="";
thetas::usage="thetas is a global variable containing the second list of graded symbols for a complete set of CAV 1-forms.";
topartials::usage="topartials is a global variable containing the list of rules to express exterior derivatives by partials.";
tosub::usage="tosub is a global variable containing the list of rules to annul cosub and d[cosub] in a subspace.";
trivial::usage="trivial represents the initial default value of algebra, having identity in most positions. ";
vec1::usage="vec1 is the first of four independent vectors created by makevectors.";
vec2::usage="vec2 is the second of four independent vectors created by makevectors.";
vec3::usage="vec3 is the third of four independent vectors created by makevectors.";
vec4::usage="vec4 is the fourth of four independent vectors created by makevectors.";
v1::usage="\!\(\*SubscriptBox[\(v1\), \(i\)]\) is the ith component of the vector vec1 in the dual basis to forms.";
v2::usage="\!\(\*SubscriptBox[\(v2\), \(i\)]\) is the ith component of the vector vec2 in the dual basis to forms.";
v3::usage="\!\(\*SubscriptBox[\(v3\), \(i\)]\) is the ith component of the vector vec3 in the dual basis to forms.";
v4::usage="\!\(\*SubscriptBox[\(v4\), \(i\)]\) is the ith component of the vector vec4 in the dual basis to forms.";
\[Omega]::usage="";
d\[Omega]::usage="";
\[CapitalOmega]::usage="";
d\[CapitalOmega]::usage="";
\[Theta]::usage="";
d\[Theta]::usage="";
\[CapitalTheta]::usage="";
d\[CapitalTheta]::usage="";
\[CapitalGamma]::usage="";
L::usage="";
V::usage="";


R;
ONFvalue;


characters;
maxrandom;
maxnum;
pforms
embeddingforms;
fieldforms;
subR\[Theta];
solutionvectors;
involutoryvectors;
allones;
check;
genvec;
invset;
idealforms;
absentforms;
cauchys;
checkinvolution;
chars;
charv;


AVFstate:=Thread[{"adjlist","algebra","brackets","cadim","clifflist","cliffname","coords","coordsdim","cosub","depend","dforms","drules","dscalars","eldim","elements","fderiv","fmdim","forms","frame","frdim","gammas","identity","natural","omegas","scalars","scdim","sderiv","sig","signature","skew","sub","symm","thetas","topartials","tosub","trivial","vec1","vec2","vec3","vec4","v1","v2","v3","v4","\[Omega]","d\[Omega]","\!\(\*TagBox[OverscriptBox[\(\[Omega]\), \(^\)],
HoldForm]\)","\[CapitalOmega]","d\[CapitalOmega]","\[Theta]","d\[Theta]","\!\(\*TagBox[OverscriptBox[\(\[Theta]\), \(^\)],
HoldForm]\)","\[CapitalTheta]","d\[CapitalTheta]","\[CapitalGamma]","L","V","av$34","av$35","av$44","av$49","av$55","av$63","av$92","av$93","av$94","av$fg","av$cs","av$c","av$star","av$fm","av$0","av$h","av$x","av$null","spintable","omdim","R","maxrandom","embeddingforms","fieldforms","solutionvectors","involutoryvectors","allones","genvec","invset","idealforms","absentforms","cauchys","checkinvolution"}->HoldForm/@{adjlist,algebra,brackets,cadim,clifflist,xAct`AVF`Private`cliffname,coords,coordsdim,cosub,Hold[depend],dforms,drules,dscalars,eldim,elements,fderiv,fmdim,forms,frame,frdim,gammas,identity,natural,omegas,scalars,scdim,sderiv,sig,signature,skew,sub,symm,thetas,topartials,tosub,trivial,vec1,vec2,vec3,vec4,v1,v2,v3,v4,\[Omega],d\[Omega],OverHat[\[Omega]],\[CapitalOmega],d\[CapitalOmega],\[Theta],d\[Theta],OverHat[\[Theta]],\[CapitalTheta],d\[CapitalTheta],\[CapitalGamma],L,V,xAct`AVF`Private`av$34,xAct`AVF`Private`av$35,xAct`AVF`Private`av$44,xAct`AVF`Private`av$49,xAct`AVF`Private`av$55,xAct`AVF`Private`av$63,xAct`AVF`Private`av$92,xAct`AVF`Private`av$93,xAct`AVF`Private`av$94,xAct`AVF`Private`av$fg,xAct`AVF`Private`av$cs,xAct`AVF`Private`av$c,xAct`AVF`Private`av$star,xAct`AVF`Private`av$fm,xAct`AVF`Private`av$0,xAct`AVF`Private`av$h,xAct`AVF`Private`av$x,xAct`AVF`Private`av$null,xAct`AVF`Private`spintable,xAct`AVF`Private`omdim,R,maxrandom,embeddingforms,fieldforms,solutionvectors,involutoryvectors,allones,genvec,invset,idealforms,absentforms,cauchys,checkinvolution}];


ModifiedGlobalVariables:=Delete[First/@AVFstate,Position[Thread[AVFinitialstate==AVFstate],True,{1}]/.{}->List/@Range[Length@AVFstate]]


makeclear[]:=(
makeelements[{identity}];
makealgebra[{{identity}}];
makeclifford[{}];
makeframe[{}];
makeforms[{}];
makecoords[{}];
makescalars[{}];
)


Begin["`Private`"]


$AVFDir=$xActDirectory<>"/AVF";


timer[]:=(Print["Elapsed time = ",Round[AbsoluteTime[]-av$bt]," sec."]);reset[]:=(av$bt=AbsoluteTime[];);


names:=Names["xAct`AVF`*"]


makeelements[elems_List]:=(
validateelements[elems];
Unprotect[eldim,elements];
elements=elems;
eldim=Length[elements];
Protect[eldim,elements];
adjlist=elements
);


makealgebra[ptab_,id_:True]:=Block[{av$1},
Unprotect[algebra];
av$1=Length[ptab[[1]]];
If[id,
algebra=ptab;
Do[algebra[[1,i$1]]=algebra[[i$1,1]]=elements[[i$1]],{i$1,av$1}];
Protect[algebra];
Return[]
];
makeelements[Prepend[elements,id]];
av$1=av$1+1;
algebra=IdentityMatrix[av$1];
Do[algebra[[1,i$1]]=algebra[[i$1,1]]=elements[[i$1]],{i$1,av$1}];
Do[
algebra[[i$3+1,i$2+1]]=ptab[[i$3,i$2]];  (* Repeated !? *)algebra[[i$2+1,i$3+1]]=ptab[[i$2,i$3]],
{i$3,av$1-1},
{i$2,av$1-1}
];
Protect[algebra];
];


mtx[ptab_:algebra]:=MatrixForm[ptab];


generic[]:=(
symm=Flatten[Table[Subscript[sy, j$1,j$2,j$3],{j$1,0,eldim-1},{j$2,eldim-1},{j$3,j$2,eldim-1}]];
skew=Flatten[Table[Subscript[sk, j$1,j$2,j$3],{j$1,0,eldim-1},{j$2,eldim-2},{j$3,j$2+1,eldim-1}]];
Table[
Which[
j$2*j$3==0,elements[[j$2+1]],
j$2==j$3,Sum[Subscript[sy, j$1,j$2,j$3]elements[[j$1+1]],{j$1,0,eldim-1}],
j$2<j$3,Sum[(Subscript[sy, j$1,j$2,j$3]+ Subscript[sk, j$1,j$2,j$3])elements[[j$1+1]],{j$1,0,eldim-1}],
j$2>j$3,Sum[(Subscript[sy, j$1,j$3,j$2]- Subscript[sk, j$1,j$3,j$2])elements[[j$1+1]],{j$1,0,eldim-1}]
],
{j$2,0,eldim-1},
{j$3,0,eldim-1}
]
);


SetAttributes[Wedge,{Flat,OneIdentity}]


makeforms[elems_List]:=Block[{nodscs},
validateelements[elems];
Unprotect[fmdim,forms,dforms,fderiv,sderiv,natural,av$c,av$bs,av$cs];
nodscs=DeleteCases[elems,dsc_/;MemberQ[dscalars,dsc]];
forms=Join[nodscs,dscalars];
fmdim=Length[forms];
dforms=Table[prependinhead[d,forms[[i$1]]],{i$1,fmdim-scdim}];
av$bs=Join[{av$fm},forms,dforms];	
fderiv=Flatten[Join[{av$fm->0},Table[{dforms[[i$2]]->0,forms[[i$2]]->dforms[[i$2]]},{i$2,fmdim-scdim}],Table[forms[[fmdim-scdim+i$1]]->0,{i$1,scdim}]]];
sderiv=Table[Dt[scalars[[i$1]]]->dscalars[[i$1]],{i$1,scdim}];
If[av$fg,
natural=Table[dforms[[i$1]]->0,{i$1,fmdim-scdim}];
makedrules[natural]
];
av$fg=True;
av$cs=Table[Subscript[av$c, i$1],{i$1,fmdim}];
makevectors[];
Protect[fmdim,forms,dforms,fderiv,sderiv,natural,av$c,av$bs,av$cs];
forms
];


makevectors[]:=(
Do[
Subscript[v1, i$1]=Subscript[v2, i$1]=Subscript[v3, i$1]=Subscript[v4, i$1]=0;
Subscript[v1, i$1]=.;
Subscript[v2, i$1]=.;
Subscript[v3, i$1]=.;
Subscript[v4, i$1]=.,
{i$1,fmdim}
];
vec1=av$cs/.av$c->v1;
vec2=av$cs/.av$c->v2;
vec3=av$cs/.av$c->v3;
vec4=av$cs/.av$c->v4;
);


makescalars[elems_List]:=Block[{av$55},
validateelements[elems];
Unprotect[scdim,scalars,dscalars];
av$55=Take[forms,fmdim-scdim];
scalars=elems;
scdim=Length[scalars];
dscalars=Table[prependinhead[d,scalars[[i$1]]],{i$1,scdim}];
av$fg=False;
makeforms[av$55];
Protect[scdim,scalars,dscalars];
scalars
];


makecoords[elems_List]:=Block[{},
Unprotect[coords,coordsdim,depend,topartials];
Do[Format[D[av$z_[depend],coords[[j$3]]]]=.,{j$3,coordsdim}];
Do[Format[D[D[av$z_[depend],coords[[j$3]]],coords[[j$4]]]]=.,{j$3,coordsdim},{j$4,j$3,coordsdim}];
makescalars[Join[elems,Complement[scalars,elems]]];
coords=elems;
coordsdim=Length[coords];
depend=coords/.List->Sequence;
(* Jose: These two are a dangerous definitions, because they cannot use SetDelayed. They need to do the loop right now *)
Do[Format[D[av$z_[depend],coords[[j$3]]]]=Subscript[av$z,coords[[j$3]]],{j$3,coordsdim}];
Do[Format[D[D[av$z_[depend],coords[[j$3]]],coords[[j$4]]]]=Subscript[av$z,ToString[coords[[j$3]]]<>ToString[ coords[[j$4]]]],{j$3,coordsdim},{j$4,j$3,coordsdim}];
topartials=Table[dscalars[[j$1]]->Sum[D[scalars[[j$1]][depend],coords[[j$2]]] dscalars[[j$2]],{j$2,coordsdim}],{j$1,1+coordsdim,scdim}];
av$id:=IdentityMatrix[fmdim-scdim+coordsdim];
Subscript[D, av$gu_][av$gx_]:=Block[{av$dr,av$ret},
av$dr=drules;
makedrules[topartials];
If[Head[av$gu]===Integer,
av$ret=av$id[[fmdim-scdim+av$gu]]\[SmallCircle]d[av$gx],av$ret=Flatten[Extract[av$id,fmdim-scdim+Position[coords,av$gu]]]\[SmallCircle]d[av$gx]
];
makedrules[av$dr];
av$ret
];
makedrules[Join[drules,topartials]];
Protect[coords,coordsdim,depend,topartials];
coords
];
(* Jose: added initial value for coords and topartials *)
coords = {};
topartials={};


makeframe[{}]:=Block[{},
Unprotect[frame,frdim];
If[ValueQ[frame],frame=.];
If[ValueQ[frdim],frdim=.];
If[ValueQ[signature],signature=.];
];


makeframe[elems_List]:=Block[{av$31,av$32,av$33},
Unprotect[frame,frdim];
frdim=Length[elems];
signature=av$31=av$32={};
Do[
av$33=elems[[i$7]];
If[Head[av$33]===Times,
AppendTo[av$31,-av$33];
AppendTo[av$32,-1],
AppendTo[av$31,av$33];
AppendTo[av$32,1]
],
{i$7,frdim}
];
frame=Sort[av$31];
Do[
sig[i$7]=Extract[av$32,Position[av$31,frame[[i$7]]]][[1]];
AppendTo[signature,sig[i$7]],
{i$7,frdim}
];
av$fg=False;
makeforms[Join[frame,Complement[forms,frame]]];
av$s82[];
Protect[frame,frdim];
frame
];


av$s82[]:=(
av$star[{av$fm}]={1,frame};
av$star[frame]={Product[sig[i$3],{i$3,frdim}],{av$fm}};
If[frdim==1,Return[]];
Do[av$star[{frame[[i$1]]}]={(-1)^(i$1-1)sig[i$1] ,Delete[frame,{i$1}]},{i$1,frdim}];
If[frdim==2,Return[]];
Do[av$star[{frame[[i$1]],frame[[i$2]]}]={(-1)^(i$1+i$2-1)sig[i$1] sig[i$2],Delete[frame,{{i$1},{i$2}}]},{i$1,frdim-1},{i$2,i$1+1,frdim}];
If[frdim==3,Return[]];
Do[av$star[{frame[[i$1]],frame[[i$2]],frame[[i$3]]}]={(-1)^(i$1+i$2+i$3)sig[i$1] sig[i$2]sig[i$3],Delete[frame,{{i$1},{i$2},{i$3}}]},{i$1,frdim-2},{i$2,i$1+1,frdim-1},{i$3,i$2+1,frdim}];
If[frdim==4,Return[]];
Do[av$star[{frame[[i$1]],frame[[i$2]],frame[[i$3]],frame[[i$4]]}]={(-1)^(i$1+i$2+i$3+i$4)sig[i$1] sig[i$2]sig[i$3]sig[i$4],Delete[frame,{{i$1},{i$2},{i$3},{i$4}}]},{i$1,frdim-3},{i$2,i$1+1,frdim-2},{i$3,i$2+1,frdim-1},{i$4,i$3+1,frdim}];
If[frdim==5,Return[]];Do[av$star[{frame[[i$1]],frame[[i$2]],frame[[i$3]],frame[[i$4]],frame[[i$5]]}]={(-1)^(i$1+i$2+i$3+i$4+i$5-1)sig[i$1] sig[i$2]sig[i$3]sig[i$4]sig[i$5],Delete[frame,{{i$1},{i$2},{i$3},{i$4},{i$5}}]},{i$1,frdim-4},{i$2,i$1+1,frdim-3},{i$3,i$2+1,frdim-2},{i$4,i$3+1,frdim-1},{i$5,i$4+1,frdim}];
If[frdim==6,Return[]];
);


in[expr_]:=Block[{av$1,av$2},
If[Head[expr]===List,Return[expr]];
av$1=Expand[expr];
If[av$1==0,Return[{{av$0,{av$fm},elements[[1]]}}]];If[Head[av$1]=!=Plus,av$1=av$1+av$0*av$fm *elements[[1]]];av$2=Apply[List,av$1];
Map[av$s1,av$2]
];


av$s1[term_]:=Block[{av$3,av$4,av$5,av$6,av$7},
If[term===1,
Return[{1,{av$fm},elements[[1]]}],
av$4=Apply[List,av$3  term]
];av$6=Cases[av$4,av$12_/;(Head[av$12]===Wedge||MemberQ[av$bs,av$12])];av$99=1;
If[av$6=={},
av$6={av$fm};
av$5=av$4,
av$6=av$6[[1]];
av$5=DeleteCases[av$4,av$6];
av$6=av$6/.Wedge->List;
av$6=av$s10[av$6]
];
av$7=Cases[av$5,av$12_/;MemberQ[elements,av$12]];
If[av$7=={},
av$7=elements[[1]],
av$7=av$7[[1]];
av$5=DeleteCases[av$5,av$7]
];
If[av$5=={av$3},
av$5=av$99,
av$5=Apply[Times,Join[{av$99},av$5/.av$3->1]]];
{av$5,av$6,av$7}
];


av$s10[av$g_]:=Block[{av$8,av$9},
If[
Head[av$g]=!=List,
Return[{av$g}],
av$8=Cases[av$g,av$12_/;MemberQ[forms,av$12]];
av$9=Cases[av$g,av$13_/;MemberQ[dforms,av$13]];
av$99=Signature[av$8];
Join[Sort[av$8],Sort[av$9]]
]
];


out[av$g_]:=Block[{av$1,av$2,av$s2},
If[Head[av$g]=!=List,Return[av$g]];
av$s2[i$8_]:=(
av$2=av$g[[i$8,2]];
If[Length[av$2]==1,av$2=av$2[[1]],av$2=Apply[Wedge,av$2]]
);
av$1=Sum[av$g[[i$2,1]]av$s2[i$2]av$g[[i$2,3]],{i$2,Length[av$g]}];
av$1/.{av$fm->1,elements[[1]]->1,av$0->0}
];


mult[av$g1_,av$g2_]:=Block[{av$1,av$2,av$s4},
If[av$g1==0||av$g2==0,Return[0]];
If[NumberQ[av$g1],Return[av$g1*out[in[av$g2]]]];
If[NumberQ[av$g2],Return[av$g2*out[in[av$g1]]]];
av$1=Flatten[Outer[List,in[av$g1],in[av$g2],1],1];
av$s4[av$g3_]:=Thread[av$g3];
av$2=Map[av$s4,av$1];
out[Map[av$s3,av$2]]
];


av$s3[av$g_]:=Block[{av$4,av$5,av$6},av$5=DeleteCases[Flatten[av$g[[2]]],av$fm];
av$99=1;
av$5=Switch[Length[av$5],
0,{av$fm},
1,av$5,
_,av$s10[av$5]
];
av$4=av$99*Apply[Times,av$g[[1]]];
av$6=algebra[[
Position[elements,av$g[[3,1]]][[1,1]],Position[elements,av$g[[3,2]]][[1,1]]
]];
{av$4,av$5,av$6}
];


Unprotect[NonCommutativeMultiply];NonCommutativeMultiply[av$g1_,av$g2_]:=mult[av$g1,av$g2];Protect[NonCommutativeMultiply];


matmult[av$g1_,av$g2_]:=(
Table[
Sum[
av$g1[[i$1,i$3]]**av$g2[[i$3,i$2]],
{i$3,Dimensions[av$g1][[2]]}
],
{i$1,Dimensions[av$g1][[1]]},
{i$2,Dimensions[av$g2][[2]]}]
);


Unprotect[CircleTimes];
SetAttributes[CircleTimes,{Flat,OneIdentity}];
Unprotect[CircleTimes];
CircleTimes[av$g11_,av$g12_]:=matmult[av$g11,av$g12];
Protect[CircleTimes];


factor[av$g1_,av$g2_]:=Block[{av$2,av$3,av$4,av$s6},
If[FreeQ[av$g2,av$g1],Return[0]];
av$2={{av$0,{av$fm},elements[[1]]}};
av$3=1;
av$s6[av$g_]:=(
If[FreeQ[av$g[[2]],av$g1],Return[]];
If[Length[av$g[[2]]]==1,AppendTo[av$2,{av$g[[1]],{av$fm},av$g[[3]]}],av$4=Position[av$g[[2]],av$g1];
If[MemberQ[forms,av$g1],av$3=(-1)^(av$4[[1,1]]-1)];AppendTo[av$2,{av$3*av$g[[1]],Delete[av$g[[2]],av$4[[1]]],av$g[[3]]}]
]
);
Map[av$s6,in[av$g2]];
out[av$2]
];


contract[av$g3_,av$g4_]:=Block[{av$s14},
av$s14[ av$g5_] :=If[Head[av$g5] =!= List,
 Return[av$s12[av$g3, av$g5]],
 av$s14/@ av$g5
];
av$s14[av$g4]
];


av$s12[av$g6_,av$g7_]:=Block[{av$1,av$2,av$s16},
av$2=applydrules[av$g7];
av$s16[av$g_]:=ReplacePart[av$g,av$g6[[i$8]]av$g[[1]],1];
av$1={};Do[If[FreeQ[av$2,forms[[i$8]]],Continue[],AppendTo[av$1,Map[av$s16,av$s15[forms[[i$8]],av$2]]]],{i$8,fmdim}];
out[Flatten[av$1,1]]
];


av$s15[av$g1_,av$g2_]:=Block[{av$3,av$4,av$s17},
av$4={};
av$s17[av$gs_]:=(If[FreeQ[av$gs[[2]],av$g1],Return[]];If[Length[av$gs[[2]]]==1,AppendTo[av$4,{av$gs[[1]],{av$fm},av$gs[[3]]}],av$3=Position[av$gs[[2]],av$g1];
AppendTo[av$4,{(-1)^(av$3[[1,1]]-1)*av$gs[[1]],Delete[av$gs[[2]],av$3[[1]]],av$gs[[3]]}]]
);
Map[av$s17,in[av$g2]];
av$4
];


Unprotect[SmallCircle];
SetAttributes[SmallCircle,{Flat,OneIdentity}]; SmallCircle[av$g1_,av$g2_]:=contract[av$g1,av$g2];Protect[SmallCircle];


expand[expr_]:=Expand[expr/.Wedge->NonCommutativeMultiply];


collect[expr_]:=Block[{av$1,av$2},
av$1=Collect[Expand[expr]/.elements[[1]]->1,elements];av$2=av$1/.{av$13_/;MemberQ[elements,av$13]->0};av$1-av$2+av$2 elements[[1]]
];


coeffs[expr_]:=DeleteCases[Coefficient[collect[expr],elements],0];


SetAttributes[av$0,Constant];
Protect[av$0];


drules=natural;
Protect[drules];


makedrules[av$g_]:=Block[{av$4,av$98},
av$4={};
Do[If[Head[av$g[[i$1]]]===Rule,AppendTo[av$4,av$g[[i$1]]]],{i$1,Length[av$g]}];
av$98=Complement[av$g,av$4];
If[av$98=!={},
Print["Invalid input:   ",av$98];
Abort[],
Unprotect[drules];
If[av$g=={},drules=natural,drules=av$g];
Protect[drules];
drules
]
];


applydrules[av$g2_]:=expand[av$g2/.drules];


d[av$g8_]:=
If[Head[av$g8]=!=List,
Return[av$s21[av$g8]],
Map[d,av$g8]
];


av$s21[av$g_]:=Block[{av$1,av$2,av$3,av$4,av$5,av$6,av$7,av$s9,av$s11},av$1=in[av$g];
av$2=av$s7[av$1];
av$s9[av$g1_]:=(
i$3++;
av$7=av$g1/.fderiv;
If[av$7===0,
Return[],
av$6=av$5;
av$99=1;av$4=ReplacePart[av$3,av$s10[Delete[AppendTo[av$6,av$7],i$3]],2];
av$4[[1]]=(-1)^(i$3-1)*av$99*av$3[[1]];
AppendTo[av$2,av$4]
]
);
av$s11[av$g2_]:=(
av$3=av$g2;
av$5=av$g2[[2]];
i$3=0;
Map[av$s9,av$5]
);
Map[av$s11,av$1];
out[av$2]
];


av$s7[av$g4_]:=Block[{av$8,av$9,av$11,av$s8},
av$11=0;
av$s8[av$g_]:=(
av$8=Dt[av$g[[1]]]/.sderiv;av$8=av$8-(av$8/.av$12_/;MemberQ[dscalars,av$12]->0);If[av$8===0,Return[],av$9={{av$0,{av$fm},elements[[1]]},{1,av$g[[2]],av$g[[3]]}};av$11=av$11+mult[av$8,av$9]]);
Map[av$s8,av$g4];
in[av$11]
];


da[expr_]:=applydrules[d[expr]];


lie[av$g1_,av$g2_]:=av$g1\[SmallCircle]d[av$g2]+da[av$g1\[SmallCircle]av$g2];


liealgebra[]:=Block[{av$82,av$83,av$84},
av$84=fmdim-scdim;
av$82={};Do[If[FreeQ[drules,dforms[[j$3]]],AppendTo[av$82,dforms[[j$3]]]],{j$3,av$84}];
If[av$82=!={},Print["Need drules for: ",av$82];Abort[]];
av$83=Sort[Take[forms,av$84]];
V=.;
Print["order = ",av$83];
brackets=Flatten[Table[L[Overscript[V,av$83[[j$1]]],Overscript[V,av$83[[j$2]]]]->Sum[-Coefficient[da[av$83[[j$3]]],av$83[[j$1]]\[Wedge]av$83[[j$2]]] Overscript[V,av$83[[j$3]]],{j$3,av$84}],{j$1,av$84-1},{j$2,j$1+1,av$84}]
];
av$82=brackets;Do[av$82=ReplacePart[av$82,j$1,Position[av$82,av$83[[j$1]]]],{j$1,av$84}];
av$82
];


validateelements[elems_List]:=With[{av$98=Cases[elems,av$5_/;MemberQ[{Plus,Times,Power,Wedge,List},Head[av$5]]||NumericQ[av$5]]},
If[av$98=!={},Print["Invalid input:   ",av$98];Abort[]]
];


prependinhead[prefix_,expr_]:=Block[{subhead,newhead},
subhead=expr;
While[Head[subhead]=!=Symbol,subhead=subhead[[1]]];newhead=ToExpression[ToString[prefix]<>ToString[subhead]];ReplacePart[expr,newhead,Position[expr, subhead]]
];


prune[av$g1_,av$g2_]:=DeleteCases[av$g1,av$1_/;FreeQ[av$g2,av$1]];


setzero[av$g2_]:=Block[{av$s41},
av$s41[av$g1_]:=av$g1==0;
DeleteCases[Map[av$s41,av$g2],True]
];


adL[av$41_,av$42_]:=Block[{av$s71},
av$s71[av$43_]:=collect[av$41**av$43];
Map[av$s71,av$42]
];


adR[av$42_,av$41_]:=Block[{av$s72},
av$s72[av$43_]:=collect[av$43**av$41];
Map[av$s72,av$42]
];


av$44:=Table[Subscript[av$49, j$8,j$9],{j$8,Length[adjlist]},{j$9,Length[adjlist]}];


adjmat[av$46_]:=Block[{av$45,av$47},
av$47=av$44.adjlist-av$46;
av$47= setzero[Flatten[Table[coeffs[av$47[[i$1]]],{i$1,Length[av$47]}]]];
av$45=Flatten[Solve[av$47,Flatten[av$44]]];
av$44/.av$45
];


hs[av$g17_] := (
If[Head[av$g17] =!= List, av$s83[applydrules[av$g17]], hs /@ av$g17]
);


av$s83[av$g15_]:=Block[{av$2,av$s81},
av$s81[av$g1_]:=(
av$2=av$star[av$g1[[2]]];
ReplacePart[ReplacePart[av$g1,av$2[[1]] av$g1[[1]],{1}],av$2[[2]] ,{2}]
);
out[Map[av$s81,in[av$g15]]]
];


\[Delta][av$g16_]:=hs[hs[1]]hs[da[hs[av$g16]]];


dot[av$g18_, av$g19_] := hs[hs[1]]*hs[av$g18**hs[av$g19]];


subspace[av$g_]:=(
cosub=av$g;
sub=Complement[forms,cosub];
tosub=Flatten[
Table[{cosub[[i$1]]->0,d[cosub[[i$1]]]->0},{i$1,Length[cosub]}]
]
);


pullback[av$g_]:=expand[av$g/.tosub];


associators[i$7_]:=Block[{av$1,av$2,av$3},
av$2=Length[algebra[[1]]];
av$3=0;Do[
av$1=(elements[[i$4]]**elements[[i$5]])**elements[[i$6]]-elements[[i$4]]**(elements[[i$5]]**elements[[i$6]]);If[av$1=!=0,Print["(",elements[[i$4]],"",elements[[i$5]],")",elements[[i$6]]," - ",elements[[i$4]],"(",elements[[i$5]],"",elements[[i$6]],") = ",av$1];av$3=av$3+1];
If[av$3+1>i$7,Return[]],{i$4,2,av$2},{i$5,2,av$2},{i$6,2,av$2}];
Print["Done. Found:",av$3]
];


clifflist=Get[$AVFDir<>"/CliffordAlgebras/Cltypes"];


makeclifford[{},av$cg1_:12]:=(
av$s50[{}];
makescalars[{}];
makelements[{identity}];
makealgebra[{{identity}}];
omdim=.;
gammas=.;
omegas=.;
thetas=.;
Clear[producttable];
If[ValueQ[OverHat[\[Theta]]],OverHat[\[Theta]]=.];
If[ValueQ[OverHat[\[Omega]]],OverHat[\[Omega]]=.];
);


makeclifford[av$cg_,av$cg1_:12]:=(
av$34=av$cg1;
av$s50[av$cg];
If[signature==False,Print["Invalid signature!"];Return[]];
(* Jose: why is makescalars[{}] needed here? *)
makescalars[{}];
If[MemberQ[clifflist,cliffname],
{gammas,omegas,thetas,producttable}=Get[$AVFDir<>"/CliffordAlgebras/"<>cliffname],
If[av$cg=!={},av$s51[]]
];
makeelements[gammas];
makealgebra[producttable];
Clear[producttable];
omdim=Length[omegas];
makeforms[Join[thetas,omegas]];
makethetahat[];
makeomegahat[];
);


Unprotect[fmdim,scdim,coordsdim,dscalars];
fmdim=scdim=coordsdim=0;
dscalars={};
av$fg=True;


makeelements[{identity}];
makealgebra[{{identity}}];
makeforms[{}];
makescalars[{}];


Protect[av$fm,av$s1,av$s3,av$s7,av$s10,av$s12,av$s15,prependinhead,av$s21,validateelements,av$s82,av$s83,adL,adR,adjmat,makeelements,makealgebra,makeclifford,makeforms,makeframe,makescalars,makevectors,makecoords,makedrules,associators,generic,in,out,d,da,\[Delta],contract,lie,mult,matmult,coeffs,collect,dot,expand,timer,reset,setzero,hs,applydrules,factor,subspace,pullback,prune,mtx];


reset[];


av$s50[{}]:=(
cadim=.;
signature=.;
cliffname=.;
av$34=.;
av$92=.;
);


av$s50[av$g1_]:=Block[{av$2},
cadim=Length[av$g1];
signature=av$g1;
cliffname="Cl(";
clifflist=Get[$AVFDir<>"/CliffordAlgebras/Cltypes"];
av$2=Switch[Mod[cadim,4],
0,1,
1,1,
2,-1,
3,-1
];
av$92=Table[i$1,{i$1,cadim}];
sig[0]=1;
Do[sig[i$2]=signature[[i$2]];Switch[sig[i$2],1,cliffname=cliffname<>"+",-1,cliffname=cliffname<>"-",_,signature=False;Return[]];,{i$2,cadim}];
If[signature==False,Return[]];
If[av$34>cadim-1,cliffname=cliffname<>")",cliffname=cliffname<>","<>ToString[av$34]<>")"];sig[cadim+1]=av$2 \!\(
\*UnderoverscriptBox[\(\[Product]\), \(i$3 = 1\), \(cadim\)]\(sig[i$3]\)\)
];


av$s59[]:=Block[{av$1,av$2,av$3,av$4},
i$9=cadim;
av$1={{}};
av$3={{i$9+1}};
av$2={
\!\(\*OverscriptBox[\(\[Omega]\), \(0\)]\)};
av$4={
\!\(\*OverscriptBox[\(\[Omega]\), \(i$9\)]\)};
If[i$9==0,av$3=av$4={}];
If[i$9==1,AppendTo[av$1,{1}];AppendTo[av$2,
\!\(\*OverscriptBox[\(\[Omega]\), \(1\)]\)];av$3=av$4={}];
If[i$9>1,Do[AppendTo[av$1,{i$1}];AppendTo[av$2,
\!\(\*OverscriptBox[\(\[Omega]\), \(1\)]\)],{i$1,i$9}]];
If[i$9>2,Do[PrependTo[av$3,{i$9+1-i$1,i$9+1}];PrependTo[av$4,
\!\(\*OverscriptBox[\(\[Omega]\), \(i$9 - 1\)]\)],{i$1,i$9}]];
If[i$9>3,Do[AppendTo[av$1,{i$1,i$2}];AppendTo[av$2,
\!\(\*OverscriptBox[\(\[Omega]\), \(2\)]\)],{i$1,i$9-1},{i$2,i$1+1,i$9}]];
If[i$9>4,Do[PrependTo[av$3,{i$9-i$1,i$9-i$2,i$9+1}];PrependTo[av$4,
\!\(\*OverscriptBox[\(\[Omega]\), \(i$9 - 2\)]\)],{i$1,i$9-1},{i$2,0,i$1-1}]];
If[i$9>5,Do[AppendTo[av$1,{i$1,i$2,i$3}];AppendTo[av$2,
\!\(\*OverscriptBox[\(\[Omega]\), \(3\)]\)],{i$1,i$9-2},{i$2,i$1+1,i$9-1},{i$3,i$2+1,i$9}]];
If[i$9>6,Do[PrependTo[av$3,{i$9-i$1,i$9-i$2,i$9-i$3,i$9+1}];PrependTo[av$4,
\!\(\*OverscriptBox[\(\[Omega]\), \(i$9 - 3\)]\)],{i$1,2,i$9-1},{i$2,1,i$1-1},{i$3,0,i$2-1}]];
If[i$9>7,Do[AppendTo[av$1,{i$1,i$2,i$3,i$4}];AppendTo[av$2,
\!\(\*OverscriptBox[\(\[Omega]\), \(4\)]\)],{i$1,i$9-3},{i$2,i$1+1,i$9-2},{i$3,i$2+1,i$9-1},{i$4,i$3+1,i$9}]];
If[i$9>8,Do[PrependTo[av$3,{i$9-i$1,i$9-i$2,i$9-i$3,i$9-i$4,i$9+1}];PrependTo[av$4,
\!\(\*OverscriptBox[\(\[Omega]\), \(i$9 - 4\)]\)],{i$1,3,i$9-1},{i$2,2,i$1-1},{i$3,1,i$2-1},{i$4,0,i$3-1}]];
If[i$9>9,Do[AppendTo[av$1,{i$1,i$2,i$3,i$4,i$5}];AppendTo[av$2,
\!\(\*OverscriptBox[\(\[Omega]\), \(i$9 - 5\)]\)],{i$1,i$9-4},{i$2,i$1+1,i$9-3},{i$3,i$2+1,i$9-2},{i$4,i$3+1,i$9-1},{i$5,i$4+1,i$9}]];
If[i$9>10,Do[PrependTo[av$3,{i$9-i$1,i$9-i$2,i$9-i$3,i$9-i$4,i$9-i$5,i$9+1}];PrependTo[av$4,
\!\(\*OverscriptBox[\(\[Omega]\), \(i$9 - 5\)]\)],{i$1,4,i$9-1},{i$2,3,i$1-1},{i$3,2,i$2-1},{i$4,1,i$3-1},{i$5,0,i$4-1}]];
Unprotect[eldim];
makeforms[{}];
av$93=Join[av$2,av$4];
av$94=Join[av$1,av$3];
eldim=Length[av$94];
If[av$34>cadim-1,av$35=eldim,av$35=Sum[Binomial[cadim,i$6],{i$6,0, av$34}];av$93=Take[av$93,av$35]];
Protect[eldim];
];


av$s54[av$g_]:=Block[{av$4},
av$4="";
Do[av$4=av$4<>ToString[av$g[[i$1]]],{i$1,Length[av$g]}];
ToExpression[av$4]
];


av$s55[av$g_]:=Block[{av$4,av$5},
av$4="";
av$5=av$g;
If[av$5=!={}&&Last[av$5]==cadim+1,av$5=Drop[av$5,-1]];
Do[av$4=av$4<>ToString[av$5[[i$1]]],{i$1,Length[av$5]}];
ToExpression[av$4]
];


av$s56[]:=Block[{k$1,av$2,av$3},
k$1=20;
Do[If[Count[av$2,k$11]==2,av$2=ReplacePart[ReplacePart[av$2,k$1++,Position[av$2,k$11][[1]]],k$1++,Position[av$2,k$11][[2]]];av$3=av$3 sig[k$11]],{k$11,1,cadim}];
av$3=av$3  Signature[av$2];
av$2=Drop[Sort[av$2],20-k$1]
];


av$s58[av$g1_]:=Block[{av$2},
av$2=av$94[[av$g1]];
If[av$2=!={}&&Last[av$2]==1+cadim,av$2=Join[Drop[av$2,-1],av$92]];
av$s56[]
];


av$s51[]:=Block[{av$2},
av$s59[];
gammas=Join[{Subscript[\[Gamma], 0]},Table[Subscript[\[Gamma], av$s54[av$94[[i$1]]]],{i$1,2,eldim}]];
omegas=Join[{av$93[[1]]},Table[Subscript[(av$93[[j$1]]), av$s55[av$94[[j$1]]]],{j$1,2,av$35-1}]];
If[av$35==eldim,AppendTo[omegas,av$93[[eldim]]],AppendTo[omegas,Subscript[(av$93[[av$35]]), av$s55[av$94[[av$35]]]]]];
Clear[av$93];
thetas=omegas/.\[Omega]->\[Theta];
producttable=Table[av$3=1;av$2=Join[av$s58[i$1],av$s58[i$2]];av$s56[];If[Length[av$2]>cadim/2,av$2=Join[av$2,av$92,{cadim+1}];av$3=av$3 sig[cadim+1];av$s56[]];If[av$2=={},av$2={0}];av$3 Subscript[\[Gamma], av$s54[av$2]],{i$1,1,av$35},{i$2,1,av$35}];
Clear[av$94];
clifflist=Sort[AppendTo[clifflist,cliffname]];
Put[clifflist,"xAct/CliffordAlgebras/Cltypes"];
gammas=prune[gammas,producttable];
Put[{gammas,omegas,thetas,producttable},  cliffname]
];


makethetahat[]:=(
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)=Sum[thetas[[i$1]] elements[[i$1]],{i$1,omdim}]);
makeomegahat[]:=(
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)=Sum[omegas[[i$2]] elements[[i$2]],{i$2,omdim}]);
maketorsion[]:=(
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)=collect[ d[
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)]+(
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)/2)**
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)+
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)**(
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)/2)]);
makecurvature[]:=(
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)=collect[ d[
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)]+( 
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)/2)** 
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)]); 
makeeinstein[]:=(
\!\(\*OverscriptBox[\(G\), \(^\)]\)=collect[
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)**(
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)/2)+(
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)/2)**
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)]);


makeall[]:=(
makeforms[Join[thetas,omegas]];

\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)=
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)=
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)=
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)=0;

\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)=.;

\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)=.;

\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)=.;

\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)=.;
makethetahat[];
makeomegahat[];
makecurvature[];
maketorsion[];
);


grade[i$4_,av$g_]:=Block[{av$1,av$s5},
av$1=collect[av$g];
av$s5[-1]=-1;
av$s5[i$2_]:=av$s5[i$2-1]+Binomial[cadim,i$2];
Table[Sum[Coefficient[av$1,gammas[[i$1]]] gammas[[i$1]],{i$1,av$s5[i$4-1]+2,av$s5[i$4]+1}]]
];


grades[av$g1_]:=(Table[grade[i$5,av$g1],{i$5,0,cadim}]);


reverse[av$g_]:=Sum[Switch[Mod[i$6,4],0,1,1,1,2,-1,3,-1]grade[i$6,av$g],{i$6,0,cadim}];


involute[av$g_]:=Sum[(-1)^i$7 grade[i$7,av$g],{i$7,0,cadim}];


zerotorsion[]:=Flatten[Solve[setzero[coeffs[
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)]],prune[dforms,
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)]]];
zerocurvature[]:=Flatten[Solve[setzero[coeffs[
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)]],prune[dforms,
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)]]];
curvatureidentity[] :=( d[
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)]+(
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)/2)**
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)-
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)**(
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)/2)) ;
torsionidentity[] :=( d[
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)]+(
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)/2)**
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)-
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)**(
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)/2)+
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\)**(
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)/2)-(
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)/2)**
\!\(\*OverscriptBox[\(\[Theta]\), \(^\)]\) );
contractedidentity[]:=(d[
\!\(\*OverscriptBox[\(G\), \(^\)]\)]+
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)**(
\!\(\*OverscriptBox[\(G\), \(^\)]\)/2)+(
\!\(\*OverscriptBox[\(G\), \(^\)]\)/2)**
\!\(\*OverscriptBox[\(\[Omega]\), \(^\)]\)-(
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)/2)**
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)-
\!\(\*OverscriptBox[\(\[CapitalOmega]\), \(^\)]\)**(
\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(^\)]\)/2));


makematrices[av$85_:True]:=(
av$s85[av$85];
);


av$s85[av$g9_:True]:=Block[{ind,av$n,av$mat},
av$n=IntegerPart[cadim/2];
Do[
Do[
ind=Sort[{i$1,i$2}];
Subscript[\[Xi], i$1,i$2]=Signature[{i$1,i$2}]Subscript[\[Xi], ind[[1]],ind[[2]]];
Do[
ind=Sort[{i$1,i$2,i$3}];
Subscript[\[Xi], i$1,i$2,i$3]=Signature[{i$1,i$2,i$3}]Subscript[\[Xi], ind[[1]],ind[[2]],ind[[3]]];
Do[
ind=Sort[{i$1,i$2,i$3,i$4}];
Subscript[\[Xi], i$1,i$2,i$3,i$4]=Signature[{i$1,i$2,i$3,i$4}]Subscript[\[Xi], ind[[1]],ind[[2]],ind[[3]],ind[[4]]];
Do[
ind=Sort[{i$1,i$2,i$3,i$4,i$5}];
Subscript[\[Xi], i$1,i$2,i$3,i$4,i$5]=Signature[{i$1,i$2,i$3,i$4,i$5}]Subscript[\[Xi], ind[[1]],ind[[2]],ind[[3]],ind[[4]],ind[[5]]],
{i$5,av$n}],
{i$4,av$n}],
{i$3,av$n}],
{i$2,av$n}],
{i$1,av$n}];
spins={Subscript[\[Xi], 0]};
Do[AppendTo[spins,Subscript[\[Xi], i$1]],{i$1,av$n}];Do[AppendTo[spins,Subscript[\[Xi], i$1,i$2]],{i$1,av$n-1},{i$2,i$1+1,av$n}];
Do[AppendTo[spins,Subscript[\[Xi], i$1,i$2,i$3]],{i$1,av$n-2},{i$2,i$1+1,av$n-1},{i$3,i$2+1,av$n}];
Do[AppendTo[spins,Subscript[\[Xi], i$1,i$2,i$3,i$4]],{i$1,av$n-3},{i$2,i$1+1,av$n-2},{i$3,i$2+1,av$n-1},{i$4,i$3+1,av$n}];
If[av$n>4,
AppendTo[spins,Subscript[\[Xi], 1,2,3,4,5]]];
spdim=Length[spins];
Subscript[av$h, 0]=Sum[Subscript[\[Xi], i$1] Subscript[av$x, i$1],{i$1,0,av$n}];
Do[Subscript[av$h, i$1]=Subscript[\[Xi], 0] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$1]-Subscript[\[Xi], i$1] Subscript[av$x, 0]+Sum[Subscript[\[Xi], i$1,i$2] Subscript[av$x, i$2],{i$2,av$n}],{i$1,av$n}];
Do[Subscript[av$h, i$1,i$2]=Subscript[\[Xi], i$1] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$2]-Subscript[\[Xi], i$2] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$1]+Subscript[\[Xi], i$1,i$2] Subscript[av$x, 0]+Sum[Subscript[\[Xi], i$1,i$2,i$3] Subscript[av$x, i$3],{i$3,av$n}],{i$1,av$n-1},{i$2,i$1+1,av$n}];
Do[Subscript[av$h, i$1,i$2,i$3]=Subscript[\[Xi], i$1,i$2] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$3]+Subscript[\[Xi], i$2,i$3] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$1]+Subscript[\[Xi], i$3,i$1] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$2]-Subscript[\[Xi], i$1,i$2,i$3] Subscript[av$x, 0]+Sum[Subscript[\[Xi], i$1,i$2,i$3,i$4] Subscript[av$x, i$4],{i$4,av$n}],{i$1,av$n-2},{i$2,i$1+1,av$n-1},{i$3,i$2+1,av$n}];
Do[Subscript[av$h, i$1,i$2,i$3,i$4]=Subscript[\[Xi], i$1,i$2,i$3] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$4]-Subscript[\[Xi], i$2,i$3,i$4] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$1]+Subscript[\[Xi], i$3,i$4,i$1] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$2]-Subscript[\[Xi], i$4,i$1,i$2] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$3]+Subscript[\[Xi], i$1,i$2,i$3,i$4] Subscript[av$x, 0]+Sum[Subscript[\[Xi], i$1,i$2,i$3,i$4,i$5] Subscript[av$x, i$5],{i$5,av$n}],{i$1,av$n-3},{i$2,i$1+1,av$n-2},{i$3,i$2+1,av$n-1},{i$4,i$3+1,av$n}];
Do[Subscript[av$h, i$1,i$2,i$3,i$4,i$5]=Subscript[\[Xi], i$1,i$2,i$3,i$4] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$5]+Subscript[\[Xi], i$2,i$3,i$4,i$5] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$1]+Subscript[\[Xi], i$3,i$4,i$5,i$1] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$2]+Subscript[\[Xi], i$4,i$5,i$1,i$2] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$3]+Subscript[\[Xi], i$5,i$1,i$2,i$3] Subscript[
\!\(\*OverscriptBox[\(av$x\), \(_\)]\), i$4]-Subscript[\[Xi], i$1,i$2,i$3,i$4,i$5] Subscript[av$x, 0],{i$1,av$n-4},{i$2,i$1+1,av$n-3},{i$3,i$2+1,av$n-2},{i$4,i$3+1,av$n-1},{i$5,i$4+1,av$n}];
av$mat={Table[Coefficient[Subscript[av$h, 0],spins[[q]]],{q,spdim}]};
Do[AppendTo[av$mat,Table[Coefficient[Subscript[av$h, i$1],spins[[q]]],{q,spdim}]],{i$1,av$n}];
Do[AppendTo[av$mat,Table[Coefficient[Subscript[av$h, i$1,i$2],spins[[q]]],{q,spdim}]],{i$1,av$n-1},{i$2,i$1+1,av$n}];
Do[AppendTo[av$mat,Table[Coefficient[Subscript[av$h, i$1,i$2,i$3],spins[[q]]],{q,spdim}]],{i$1,av$n-2},{i$2,i$1+1,av$n-1},{i$3,i$2+1,av$n}];
Do[AppendTo[av$mat,Table[Coefficient[Subscript[av$h, i$1,i$2,i$3,i$4],spins[[q]]],{q,spdim}]],{i$1,av$n-3},{i$2,i$1+1,av$n-2},{i$3,i$2+1,av$n-1},{i$4,i$3+1,av$n}];
Do[AppendTo[av$mat,Table[Coefficient[Subscript[av$h, i$1,i$2,i$3,i$4,i$5],spins[[q]]],{q,spdim}]],{i$1,av$n-4},{i$2,i$1+1,av$n-3},{i$3,i$2+1,av$n-2},{i$4,i$3+1,av$n-1},{i$5,i$4+1,av$n}];
Do[Subscript[av$null, i$1]=(av$mat/.{Subscript[av$x, i$1]->1+Subscript[av$x, i$1]})-av$mat;
\!\(\*OverscriptBox[
SubscriptBox[\(av$null\), \(i$1\)], \(t\)]\)=Transpose[Subscript[av$null, i$1]],{i$1,0,av$n}];
matrep[av$g_]:=Block[{av$64,av$65},
av$65=ToString[av$g[[2]]];
av$64=Subscript[\[CapitalGamma], 0];
Do[av$64=av$64.Subscript[\[CapitalGamma], ToExpression[StringTake[av$65,{i$1}]]],{i$1,StringLength[av$65]}];
Subscript[\[CapitalGamma], av$g[[2]]]=av$64
];
Subscript[\[CapitalGamma], 0]=IdentityMatrix[spdim];
spincomps=spins;
zero=Subscript[\[CapitalGamma], 0 ]-Subscript[\[CapitalGamma], 0];
Do[spins[[i]]=Subscript[\[Nu], i];Subscript[\[CapitalNu], i]=Subscript[\[CapitalGamma], 0][[i]],{i,spdim}];
Subscript[\[CapitalGamma], cadim]=(sig[cadim])^(1/2) Subscript[av$null, 0];
Do[Subscript[\[CapitalGamma], i$1]=(sig[i$1])^(1/2)(Subscript[av$null, i$1]+
\!\(\*OverscriptBox[
SubscriptBox[\(av$null\), \(i$1\)], \(t\)]\));Subscript[\[CapitalGamma], i$1+av$n]=I (sig[i$1+av$n])^(1/2)(Subscript[av$null, i$1]-
\!\(\*OverscriptBox[
SubscriptBox[\(av$null\), \(i$1\)], \(t\)]\)),{i$1,av$n}];
Subscript[\[CapitalGamma], cadim+1]=Subscript[\[CapitalGamma], 1];
Do[Subscript[\[CapitalGamma], cadim+1]=Subscript[\[CapitalGamma], cadim+1].Subscript[\[CapitalGamma], i$1],{i$1,2,cadim}];

\!\(\*OverscriptBox[\(C\), \(~\)]\)=Subscript[\[CapitalGamma], 0];
Do[
\!\(\*OverscriptBox[\(C\), \(~\)]\)=
\!\(\*OverscriptBox[\(C\), \(~\)]\).(Subscript[av$null, i$1]-
\!\(\*OverscriptBox[
SubscriptBox[\(av$null\), \(i$1\)], \(t\)]\)),{i$1,av$n}];
If[av$g9=!=True,
av$g8=Inverse[av$g9];

\!\(\*OverscriptBox[\(C\), \(~\)]\)=av$g9.
\!\(\*OverscriptBox[\(C\), \(~\)]\).av$g8;
Do[Subscript[\[CapitalGamma], i$2]=av$g9.Subscript[\[CapitalGamma], i$2].av$g8,{i$2,cadim+1}]
];
Do[matrep[elements[[i]]],{i,1+cadim,eldim-1}];
];


Protect[makematrices,makethetahat,makeomegahat,makeeinstein,makecurvature,maketorsion,makeall,grades,grade,zerotorsion,zerocurvature,torsionidentity,curvatureidentity,contractedidentity,av$s50,av$s51,av$s54,av$s55,av$s56,av$s58,av$s59];


makeorthonormalcurvature[signature_,tocoords_,output_]:=Block[{dim,Rms,Rcs,torsions,riccis,riemann2,riemanns,b2s,toB2s,tob2s,fx$,fc$,id$,i$1,i$2,i$3,i$4,i$5,i$6,x\[Omega]},
dim=Length[signature];
Do[sig[i$1]=signature[[i$1]],{i$1,dim}];
makeforms[Table[Subscript[\[Theta], i$1],{i$1,dim}]];b2s=Flatten[Table[b2[i$1,i$2]=Subscript[\[Theta], i]\[Wedge]Subscript[\[Theta], j],{i$1,dim-1},{i$2,i$1+1,dim}]];toB2s=Flatten[Table[Subscript[\[Theta], i$1]\[Wedge]Subscript[\[Theta], i$2]->B2[i$1,i$2],{i$1,dim-1},{i$2,i$1+1,dim}]];tob2s=Flatten[Table[B2[i$1,i$2]->b2[i$1,i$2],{i$1,dim-1},{i$2,i$1+1,dim}]];riccis=Flatten[Table[Subscript[\[CapitalGamma], i$1,i$2,i$3],{i$1,dim},{i$2,dim-1},{i$3,i$2+1,dim}]];
Do[
Subscript[\[CapitalGamma], i$1,i$2,i$3]=-Subscript[\[CapitalGamma], i$1,i$3,i$2],
{i$1,dim},{i$3,dim-1},{i$2,i$3+1,dim}];
Do[
Subscript[\[CapitalGamma], i$1,i$3,i$3]=0,
{i$1,dim},{i$3,dim}];
Do[
Subscript[x\[Omega], i$1,i$2]=-Sum[sig[i$3]Subscript[\[CapitalGamma], i$3,i$1,i$2] Subscript[\[Theta], i$3],{i$3,dim}];
Subscript[x\[Omega], i$2,i$1]=-Subscript[x\[Omega], i$1,i$2],
{i$1,dim-1},{i$2,i$1+1,dim}];
Do[
Subscript[x\[Omega], i$1,i$1]=0,
{i$1,dim}];
Rms=Flatten[Table[Subscript[R, i$1,i$2,i$3,i$4],{i$1,dim-1},{i$2,i$1+1,dim},{i$3,dim-1},{i$4,i$3+1,dim}]];
Rcs=Flatten[Table[Subscript[R, i$1,i$2],{i$1,dim},{i$2,i$1,dim}]];
ONFvalue["stress"]={"\[Rho]"->(Subscript[R, 1,1]+Subscript[R, 2,2]+Subscript[R, 3,3]+Subscript[R, 4,4])/4,"p"->Subscript[R, 4,4]/4-(Subscript[R, 1,1]+Subscript[R, 2,2]+Subscript[R, 3,3])/12,"\[Mu]"->Subscript[R, 4,4],Subscript["t", 1]->Subscript[R, 1,4]/2,Subscript["t", 2]->Subscript[R, 2,4]/2,Subscript["t", 3]->Subscript[R, 3,4]/2,Subscript["M", 1,1]->-Subscript[R, 1,1]/3+(Subscript[R, 2,2]+Subscript[R, 3,3])/6,Subscript["M", 2,2]->-Subscript[R, 2,2]/3+(Subscript[R, 1,1]+Subscript[R, 3,3])/6,Subscript["M", 3,3]->-Subscript[R, 3,3]/3+(Subscript[R, 2,2]+Subscript[R, 1,1])/6,Subscript["M", 1,2]->-Subscript[R, 1,2]/2,Subscript["M", 1,3]->-Subscript[R, 1,3]/2,Subscript["M", 2,3]->-Subscript[R, 2,3]/2};
ONFvalue["AB"]={Subscript["A", 1,1]->Subscript[C, 1,4,1,4],Subscript["A", 2,2]->Subscript[C, 2,4,2,4],Subscript["A", 3,3]->Subscript[C, 3,4,3,4],Subscript["A", 1,2]->Subscript[C, 1,4,2,4],Subscript["A", 1,3]->Subscript[C, 1,4,3,4],Subscript["A", 2,3]->Subscript[C, 2,4,3,4],Subscript["B", 1,1]->Subscript[C, 1,4,2,3],Subscript["B", 2,2]->-Subscript[C, 1,3,2,4],Subscript["B", 3,3]->Subscript[C, 1,2,3,4],Subscript["B", 1,2]->-Subscript[C, 1,3,1,4],Subscript["B", 1,3]->Subscript[C, 1,2,1,4],Subscript["B", 2,3]->Subscript[C, 1,2,2,4]};
ONFvalue["dyadics"]={Subscript["a", 1]->-Subscript[\[CapitalGamma], 4,1,4],Subscript["a", 2]->-Subscript[\[CapitalGamma], 4,2,4],Subscript["a", 3]->-Subscript[\[CapitalGamma], 4,3,4],Subscript["\[CapitalOmega]", 1]->(Subscript[\[CapitalGamma], 2,3,4]-Subscript[\[CapitalGamma], 3,2,4])/2,Subscript["\[CapitalOmega]", 2]-> (-Subscript[\[CapitalGamma], 1,3,4]+Subscript[\[CapitalGamma], 3,1,4])/2,Subscript["\[CapitalOmega]", 3]->(Subscript[\[CapitalGamma], 1,2,4]-Subscript[\[CapitalGamma], 2,1,4])/2,Subscript["S", 1,1]->Subscript[\[CapitalGamma], 1,1,4],Subscript["S", 2,2]->Subscript[\[CapitalGamma], 2,2,4],Subscript["S", 3,3]->Subscript[\[CapitalGamma], 3,3,4],Subscript["S", 1,2]-> (Subscript[\[CapitalGamma], 1,2,4]+Subscript[\[CapitalGamma], 2,1,4])/2,Subscript["S", 1,3]-> (Subscript[\[CapitalGamma], 1,3,4]+Subscript[\[CapitalGamma], 3,1,4])/2,Subscript["S", 2,3]->(Subscript[\[CapitalGamma], 2,3,4]+Subscript[\[CapitalGamma], 3,2,4])/2,"TrS"->Subscript[\[CapitalGamma], 1,1,4]+Subscript[\[CapitalGamma], 2,2,4]+Subscript[\[CapitalGamma], 3,3,4]};
ONFvalue["Q"]=Table[If[i$2>i$1-1,Subscript["A", i$1,i$2]+I Subscript["B", i$1,i$2],Subscript["A", i$2,i$1]+I Subscript["B", i$2,i$1]],{i$1,3},{i$2,3}];

Print["The inverse transformation back to orthoframe:'toortho'"];
ONFvalue["toortho"]=Simplify[Flatten[Solve[tocoords/.Rule->Equal,prune[dscalars,tocoords]]]];
If[output,Print["toortho = ",ONFvalue["toortho"]]];

Print["Exterior derivatives of orthoframe:'dortho'"];
ONFvalue["dortho"]=Table[Subscript[d\[Theta], i$1]->Simplify[expand[d[Subscript[\[Theta], i$1]/.tocoords]/.ONFvalue["toortho"]]],{i$1,dim}];
If[output,Print["dortho = ",ONFvalue["dortho"]]];

Print["The general matrix of connection 1-forms:'generalomegas'"];
ONFvalue["generalomegas"]=Flatten[Table[Subscript[\[Omega], i$1,i$2]->Subscript[x\[Omega], i$1,i$2],{i$1,dim-1},{i$2,i$1+1,dim}]];
If[output,Print["generalomegas = ",ONFvalue["generalomegas"]]];

Print["Ricci rotation coefficients for orthoframe:'rotationcomps'"];
ONFvalue["torsion"]=Table[(dforms[[i$1]]/.ONFvalue["dortho"])+Sum[sig[i$3]Subscript[x\[Omega], i$1,i$3]**Subscript[\[Theta], i$3],{i$3,dim}],{i$1,dim}]/.toB2s;
torsions=Flatten[Table[Coefficient[ONFvalue["torsion"][[i$1]],B2[i$2,i$3]],{i$1,dim},{i$2,dim-1},{i$3,i$2+1,dim}]];
ONFvalue["rotationcomps"]=Flatten[Solve[setzero[torsions],riccis]];
ONFvalue["rotationcomps"]=Simplify[ONFvalue["rotationcomps"]];
If[output,Print["rotationcomps = ",ONFvalue["rotationcomps"]]];

Print["The solved matrix of connection 1-forms:'omegas'"];
ONFvalue["omegas"]=Simplify[ONFvalue["generalomegas"]/.ONFvalue["rotationcomps"]];
If[output,Print["omegas = ",ONFvalue["omegas"]]];

Print["Orthonormal components of the Riemann tensor:'riemanncomps'"] ;
Do[Subscript[x\[Omega], i$1,i$2]=(Subscript[x\[Omega], i$1,i$2]/.ONFvalue["rotationcomps"]),{i$1,dim},{i$2,dim}];
riemann2=Flatten[
Table[
Sum[sig[i$3] sig[i$4]Subscript[R, i$1,i$2,i$3,i$4]B2[i$3,i$4],{i$3,dim-1},{i$4,i$3+1,dim}]-
expand[d[Subscript[x\[Omega], i$1,i$2]]/.ONFvalue["dortho"]/.ONFvalue["toortho"]]-
Sum[sig[i$5]Subscript[x\[Omega], i$1,i$5]**Subscript[x\[Omega], i$5,i$2],{i$5,dim}],
{i$1,dim-1},{i$2,i$1+1,dim}
]
]/.toB2s;
riemanns=Flatten[Table[Coefficient[riemann2[[i$1]],B2[i$2,i$3]],{i$1,Length[riemann2]},{i$2,dim-1},{i$3,i$2+1,dim}]];
ONFvalue["riemanncomps"]=Flatten[Solve[setzero[riemanns],Rms]];
ONFvalue["riemanncomps"]=Simplify[ONFvalue["riemanncomps"]/.{Cos[am$_]^2->1-Sin[am$]^2,Cot[an$_]^2->Csc[an$]^2-1},Trig->False];
If[output,Print["riemanncomps = ",ONFvalue["riemanncomps"]]];

Print["Orthonormal components of the Ricci tensor:'riccicomps'"];
fx$:=(
fc$=Signature[{i$3,i$1}]Signature[{i$3,i$2}];
id$=Flatten[Join[Sort[{i$3,i$1}],Sort[{i$3,i$2}]]]
);
ONFvalue["riccicomps"]:=Flatten[Table[Subscript[R, i$1,i$2]->Sum[fx$;fc$ sig[i$3] Subscript[R, id$[[1]],id$[[2]],id$[[3]],id$[[4]]],{i$3,dim}],{i$1,dim},{i$2,i$1,dim}]];
ONFvalue["riccicomps"]=Simplify[ONFvalue["riccicomps"]/.ONFvalue["riemanncomps"],Trig->False];
Do[Subscript[R, i$2,i$1]=Subscript[R, i$1,i$2],{i$1,dim},{i$2,i$1,dim}];
If[output,Print["riccicomps = ",ONFvalue["riccicomps"]]];

Print["The Ricci scalar:'ricciscalar'"];
ONFvalue["ricciscalar"]=Simplify[{
\!\(\*SubscriptBox[\(R\), \("\<s\>"\)]\)->Sum[sig[i$3] Subscript[R, i$3,i$3],{i$3,dim}]}/.ONFvalue["riccicomps"],Trig->False];
If[output,Print["ricciscalar = ",ONFvalue["ricciscalar"]]];

If[dim>2,
Print["Orthonormal components of the Weyl tensor:'weylcomps'"];gx$s[i$5_,i$6_]:=If[i$5==i$6,sig[i$5],0];ONFvalue["weylcomps"]:=Flatten[Table[Subscript[C, i$1,i$2,i$3,i$4]->Subscript[R, i$1,i$2,i$3,i$4]-(gx$s[i$2,i$4] Subscript[R, i$1,i$3])/(dim-2)+(gx$s[i$1,i$4] Subscript[R, i$2,i$3])/(dim-2)-(gx$s[i$1,i$3] Subscript[R, i$2,i$4])/(dim-2)+(gx$s[i$2,i$3] Subscript[R, i$1,i$4])/(dim-2)+(gx$s[i$1,i$3]gx$s[i$2,i$4]-gx$s[i$1,i$4]gx$s[i$2,i$3])
\!\(\*SubscriptBox[\(R\), \("\<s\>"\)]\)/(dim-1)/(dim-2),{i$1,dim-1},{i$2,i$1+1,dim},{i$3,i$1,dim-1},{i$4,i$3+1,dim}]];
ONFvalue["weylcomps"]=Simplify[ONFvalue["weylcomps"]/.ONFvalue["riemanncomps"]/.ONFvalue["riccicomps"]/.ONFvalue["ricciscalar"],Trig->False];
If[output,Print["weylcomps = ",ONFvalue["weylcomps"]]]
]

];


embedsub[av$g1_,av$g2_]:=(
subdim=av$g1;sigs=av$g2;dim=Length[sigs];
Do[sig[i]=sigs[[i]],{i,dim}];
allthetas=Table[Subscript[\[Theta], i],{i,dim}];
allomegas=Flatten[Table[Subscript[\[Omega], j,i],{i,2,dim},{j,i-1}]];
Do[Subscript[\[Omega], i,j]=-Subscript[\[Omega], j,i],{i,2,dim},{j,i-1}];
Do[Subscript[\[Omega], i,i]=0,{i,dim}];
makeforms[Join[allthetas,allomegas]];
subthetas=Take[allthetas,subdim];
cothetas=Take[allthetas,subdim-dim];
subomegas=Take[allomegas,subdim (subdim-1)/2];
cosubomegas=Flatten[Table[Subscript[\[Omega], i,k],{i,subdim},{k,subdim+1,dim}]];
cocoomegas=Flatten[Table[Subscript[\[Omega], i,k],{i,subdim+1,dim-1},{k,i+1,dim}]];
subspace[cothetas];
pforms[1]=cothetas;
embeddingforms=Table[Sum[sig[j]Subscript[\[Omega], j,i]**Subscript[\[Theta], j],{j,subdim}],{i,subdim+1,dim}];
torsionforms=Table[Sum[sig[j]Subscript[\[Omega], j,i]**Subscript[\[Theta], j],{j,subdim+1,dim}],{i,subdim}];
);	


alldrules:=Join[thetadrules,omegadrules];
thetadrules:=Table[Subscript[d\[Theta], i]->Sum[-sig[j]Subscript[\[Omega], i,j]**Subscript[\[Theta], j],{j,dim}],{i,dim}];
omegadrules:=Flatten[Table[Subscript[d\[Omega], i,j]->Sum[-sig[k](Subscript[\[Omega], i,k])**Subscript[\[Omega], k,j],{k,dim}],{i,dim-1},{j,i+1,dim}]];


subR:=(
Rlist=Table[Sum[sig[k]Subscript[\[Omega], i,k]**Subscript[\[Omega], j,k],{k,subdim+1,dim}],{i,subdim-1},{j,i+1,subdim}];
Do[subRcomps[i,j]=Which[j-i==0,0,j-i>0,Rlist[[i,j-i]],j-i<0,-Rlist[[j,i-j]]],{i,subdim},{j,subdim}];
Flatten[Rlist]
);


subR\[Theta]:=(
If[subdim>5,
Print["Not included."];
Return[],
subR;
Switch[subdim,
3,Flatten[Table[Sum[Signature[{i,j,k}]subRcomps[i,j]**Subscript[\[Theta], k],{i,subdim-1},{j,i+1,subdim},{k,subdim}]]],
4,
Flatten[Table[Sum[Signature[{i,j,k,l}]subRcomps[i,j]**Subscript[\[Theta], k],{i,subdim-1},{j,i+1,subdim},{k,subdim}],{l,subdim}]],
5,
Flatten[Table[Sum[Signature[{i,j,k,l,m}]subRcomps[i,j]**Subscript[\[Theta], k],{i,subdim-1},{j,i+1,subdim},{k,subdim}],{l,subdim-1},{m,l+1,subdim}]],
_,
0
]
]
);


subR\[Theta]\[Theta]:=(
If[subdim>5,
Print["Not included."];
Return[],
subR;
Switch[subdim,
4,
Flatten[Table[Sum[Signature[{i,j,k,l}]subRcomps[i,j]**Subscript[\[Theta], k]**Subscript[\[Theta], l],{i,subdim-1},{j,i+1,subdim},{k,subdim-1},{l,k+1,subdim}]]],
5,
Flatten[Table[Sum[Signature[{i,j,k,l,m}]subRcomps[i,j]**Subscript[\[Theta], k]**Subscript[\[Theta], l],{i,subdim-1},{j,i+1,subdim},{k,subdim-1},{l,k+1,subdim}],{m,subdim}]],
_,
0
]
]
);


subR\[Theta]\[Theta]\[Theta]:=(
If[subdim>5,
Print["Not included."];
Return[],
subR;
Switch[subdim,
5,
Flatten[Table[Sum[Signature[{i,j,k,l,m}]subRcomps[i,j]**Subscript[\[Theta], k]**Subscript[\[Theta], l]**Subscript[\[Theta], m],{i,subdim-1},{j,i+1,subdim},{k,subdim-2},{l,k+1,subdim-1},{m,l+1,subdim}]]],
_,
0
]
]
);


subRR\[Theta]:=(
If[subdim>5,
Print["Not included."];
Return[],
subR;
Switch[subdim,
5,
Flatten[Table[Sum[Signature[{i,j,k,l,m}]subRcomps[i,j]**subRcomps[k,l]**(Subscript[\[Theta], m]/2),{i,subdim-1},{j,i+1,subdim},{k,subdim-1},{l,k+1,subdim},{m,subdim}]]],
_,
0
]
]
);


fieldforms[av$g3_]:=(
makedrules[alldrules];
Table[pullback[factor[cothetas[[i]],da[av$g3]]],{i,Length[cothetas]}]
);


characters:=(
getideal;
null=Table[0,{i,fmdim}];
vs=null;
newmat={null};
characs={};
solutionvectors={};
allones={};
sum=0;
If[Head[maxrandom]=!=Integer||maxrandom<3,max=3,max=maxrandom];
Do[
character;
If[Length[vs]<=num+1,
Print["There are no more independent vectors."];
Return[],
If[num===maxnum,Print["There may be more independent vectors, so the actual genus may be smaller than the printed value."];
Return[]
]
],
{num,0,maxnum}
];
Print["dimension = ",fmdim,"    genus = ",fmdim-sum];
);


getideal:=(
Do[If[Head[pforms[num]]=!=List,pforms[num]={}],{num,1,maxnum}];
ideal =Join@@ Table[pforms[num],{num,1,maxnum}];
savedrules=drules;
saveforms=forms;
makeforms[prune[forms,ideal]];
idealforms=forms;
absentforms=Complement[saveforms,forms];
);


makeones[0,charv_,pforms_]:=pforms[1];
makeones[num_,charv_,pforms_]:=Join@@Apply[SmallCircle,Append[#,pforms[Length[#]+1]]&/@Map[charv,Prepend[#,num]&/@Reverse/@Subsets[Range[num-1]],{2}],{1}];


getvector:=(
iv=0;
While[iv<num+1,
charv[num+1]=newvector;	involutoryvectors=DeleteCases[RowReduce[Join[solutionvectors,{charv[num+1]}]],null];
iv=Length[involutoryvectors]
];
AppendTo[solutionvectors ,charv[num+1]
]
);


newvector:=If[sum==0,Return[Table[(-1)^Random[Integer] Random[Integer,{1,max}],{j,fmdim}]],Return[Simplify[Sum[(-1)^Random[Integer] Random[Integer,{1,max}] vs[[i]],{i,Length[vs]}]]]
];


character:=(
If[Length[vs]>num,
newones=makeones[num,charv,pforms],
Return[]
];
If[Length[newones]==0,
chars[num]=0;
AppendTo[characs,chars[num]];
Print[characs];
If[Length[vs]>num,getvector];
Return[],
allones=Join[allones,newones]
];
mat=Table[Coefficient[newones[[i]],forms[[j]]],{i,Length[newones]},{j,fmdim}];
newmat=Join[newmat,mat];
newmat=DeleteCases[Simplify[RowReduce[newmat]],null];
chars[num]=First[Dimensions[newmat]]-sum;
sum=sum+chars[num];
AppendTo[characs,chars[num]];
Print[characs];
vs=Simplify[NullSpace[newmat]];
If[Length[vs]>num,getvector,Return[]]
);


check:= (
invset=Table[Subscript[V, Position[involutoryvectors[[i]],1][[1,1]]],{i,Length[involutoryvectors]}];genvec=Sum[invset[[i]]involutoryvectors[[i]],{i,Length[involutoryvectors]}];
Simplify[genvec\[SmallCircle]allones]
);


completevectors:=(
makeforms[saveforms];
j=1;
Do[
If[FreeQ[absentforms,forms[[k]]],
vec1[[k]]=involutoryvectors[[1,j]];
vec2[[k]]=involutoryvectors[[2,j]];vec3[[k]]=involutoryvectors[[3,j]];
vec4[[k]]=involutoryvectors[[4,j]];
j=j+1
],
{k,fmdim}
]
);


cauchys:=(
ccs[each_]:=Coefficient[each,forms];
vars=invset;
If[Length[pforms[1]]>0,ks=Table[Subscript[k, i,j],{i,Length[pforms[2]]},{j,Length[pforms[1]]}];vars=Join[invset,Flatten[ks]],
ks={}
];
cauchyeqns=setzero[Flatten[Map[ccs,genvec\[SmallCircle]pforms[2]-ks.pforms[1]]]];
cauchysoln=Flatten[Solve[cauchyeqns,vars]];
cauchyvec=genvec/.cauchysoln
);


End[]


AVFinitialstate=AVFstate;


EndPackage[]
