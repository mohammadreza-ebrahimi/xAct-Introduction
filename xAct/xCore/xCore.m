(************************ 0. Info and copyright ***********************)


xAct`xCore`$Version={"0.6.10",{2018,2,28}}


(* xCore, basic functional additions to Mathematica *)

(* Copyright (C) 2007-2018 Jose M. Martin-Garcia *)

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


(* :Title: xCore *)

(* :Author: Jose M. Martin-Garcia *)

(* :Summary: Programming tools for xAct *)

(* :Brief Discussion:
*)
  
(* :Context: xAct`xCore` *)

(* :Package Version: 0.6.9 *)

(* :Copyright: Jose M. Martin-Garcia 2007-2018 *)

(* :History: see xCore.History file *)

(* :Keywords: *)

(* :Source: xCore.nb *)

(* :Warning: Still experimental! *)

(* :Mathematica Version: 6.0 and later *)

(* :Limitations: unlimited limitations *)


(************************ 1. Begin package ***********************)


With[{xAct`xCore`Private`xCoreSymbols=DeleteCases[Join[Names["xAct`xCore`*"],Names["xAct`xCore`Private`*"]],"$Version"|"xAct`xCore`$Version"|"xAct`xCore`Private`$LastPackage"]},
Unprotect/@xAct`xCore`Private`xCoreSymbols;
Clear/@xAct`xCore`Private`xCoreSymbols;
]


If[System`$VersionNumber<6.5,
(* Old definitions *)
xAct`xCore`$xActDirectory=First@FileNames["xAct",{$UserBaseDirectory,$BaseDirectory,$InstallationDirectory},Infinity];
xAct`xCore`$xActDocDirectory=StringJoin[xAct`xCore`$xActDirectory,"/Documentation/English"],
(* FindFile will find the package in the same place that Get found it. Thanks Teake! *)
xAct`xCore`$xActDirectory=DirectoryName[FindFile["xAct`xPerm`"],3];
xAct`xCore`$xActDocDirectory=StringJoin[xAct`xCore`$xActDirectory,"Documentation/English"]
];


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`xCore`"];


BeginPackage["xAct`xCore`"]


xAct`xCore`Private`$barslength=60;
xAct`xCore`Private`bars=StringJoin[Table["-",{xAct`xCore`Private`$barslength}]];


If[xAct`xCore`Private`$LastPackage==="xAct`xCore`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["Package xAct`xCore`  version ",xAct`xCore`$Version[[1]],", ",xAct`xCore`$Version[[2]]];
Print["CopyRight (C) 2007-2018, Jose M. Martin-Garcia, under the General Public License."];
Print[xAct`xCore`Private`bars];
Print["This package comes with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]
];


Off[General::shdw]
xAct`xCore`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


Off[General::spell]
Off[General::spell1]


(* Directories *)
$xActDirectory::usage="$xActDirectory is a global variable containing the directory where the xAct directory has been placed. The two recommended options are $UserBaseDirectory/Applications/xAct for a user-based installation or $BaseDirectory/Applications/xAct for a system-wide installation. The third possible place is $InstallationDirectory/AddOns/Applications/xAct, also for a system-wide installation.";
$xActDocDirectory::usage="$xActDocDirectory is a global variable containing the directory with the Documentation of xAct. It is the subdirectory Documentation/English of $xActDirectory.";


(* Warnings *)
$WarningFrom::usage="$WarningFrom is a global variable which is set to different values in different parts of the code. It is used to get an idea of where the warning messages are coming from.";


(* Folded rules *)
FoldedRule::usage="FoldedRule[rules1, rules2, ...] contains a number of lists of rules which are applied sequentially (first rules1, then rules2, etc.) when called by ReplaceAll and ReplaceRepeated.";
CollapseFoldedRule::usage="CollapseFoldedRule[frule, {n, m}] converts the elements n to m of the foldedrule frule into a single list of replacements in terms of the independent objects at level m. The positional argument follows the notation of Take: both n, m or just m can be negative, counting from the end; a single positive integer m represents {1, m}; a single negative integer -n represents {-n, -1}. A third argument can be given specifying a function to be mapped on all rules after collapsing a new level. CollapseFoldedRule[frule] or CollapseFoldedRule[frule, All] are interpreted as collapse of all elements of frule.";


(* Dependent and independent rules *)
IndependentRules::usage="IndependentRules[rules] returns a list with the subset of rules of the form x->x. On a FoldedRule expression it returns a list with the last rules.";
DependentRules::usage="DependentRules[rules] return a list with the subset of rules not of the form x->x. On a FoldedRule expression ir returns the first list of rules.";

(* AppendToUnevaluated *)
AppendToUnevaluated::usage="AppendToUnevaluated[symbol, value] works like AppendTo[symbol, value] but none of the elements of the expression associated to symbol is evaluated.";


(* Number of arguments *)
xAct`xCore`Private`setargs::usage="xAct`xCore`Private`setargs[head, found, expected] sets a definition for head[args] such that if the number of arguments found is not compatible with the number of arguments expected then an error message is sent (not thrown). Number found must be an integer or Infinity, whereas expected can be an integer or Infinity or a range indicated by a list of two integers {m, n} or an integer and Infinity {m, Infinity}.";
SetNumberOfArguments::usage="SetNumberOfArguments[function, expected] sets several definitions for head[args] using the (private) function setargs for those cases which are incompatible with the expected number or range of arguments. The format for expected is explained in setargs.";


(* Small functions for lists *)
JustOne::usage="JustOne[{expr}] gives expr, or an error otherwise.";
MapIfPlus::usage="MapIfPlus[f, expr] maps f on the elements of expr if expr has head Plus, or returns f[expr] otherwise.";
If[System`$VersionNumber<6.5,DeleteDuplicates::usage="DeleteDuplicates[list] gives the union of the elements of list, keeping the original order. The elements of list are evaluated before the union is taken. The expression list may have head other than List."
];
If[System`$VersionNumber<9.0,
DuplicateFreeQ::usage="DuplicateFreeQ[list] returns True if there are no identical elements in the list, and False otherwise."
];


(* Combination of names *)
SymbolJoin::usage="SymbolJoin[s1, s2, ...] generates a new symbol joining together the symbols, numbers or strings si, after evaluating them.";


(* Convert named patterns into their names *)
NoPattern::usage="NoPattern[expr] converts the named patterns in expr into their names.";


(* String modification *)
Underline::usage="Underline[x] returns a string with x underlined. The string form of x must not contain blanks. Underline[x, color] prints colored underlines (but x is not colored).";
Overline::usage="Overline[x] returns a string with x overlined. The string form of x must not contain blanks. overline[x, color] prints colored overlines (but x is not colored).";
TildeString::usage="TildeString[x, n] returns a string with n tildes over x if n>0 and n tildes below x if n<0. When n is not an integer, n is replaced by 1. TildeString[x, n, color] prints colored tildes (but x is not colored).";
ColorString::usage="ColorString[atom, color] returns a string with atom printed in color.";
HatString::usage="HatString[x, n] returns a string with n hats over x if n>0 and n hats below x if n<0. When n is not an integer, n is replaced by 1. HatString[x, n, color] prints colored hats (but x is not colored).";


(* Options *)
CheckOptions::usage="CheckOptions[options] checks that options have the right rule structure and that the tensor rank of {options} is either 1 or 2. If True, it returns Flatten[{options}, 1]. If False, it throws an error.";
TrueOrFalse::usage="TrueOrFalse[True] and TrueOrFalse[False] give True. Any other argument gives False.";


(* Array manipulation *)
ThreadArray::usage="ThreadArray[head[array1, array2]] threads head over pairs of elements of the two arrays.";


(* xUpvalues *)
xUpSet::usage="xUpSet[f[x], rhs] sets an upvalue for x using UpSet[f[x], rhs] but first unprotects x and finally protects it if it was initially protected.";
xUpSetDelayed::usage="xUpSetDelayed[f[x], rhs] sets a delayed upvalue for x using UpSetDelayed[f[x], rhs] but first unprotects x and finally protects it if it was initially protected.";
xUpAppendTo::usage="xUpAppendTo[f[x], e] assumes that f[x] is a list of elements associated to x as an upvalue and appends the element e to it, resetting the association. If f[x] does not have a value, it is initialized in the first place to {}.";
xUpDeleteCasesTo::usage="xUpDeleteCasesTo[f[x], cases] assumes that f[x] is a list of elements associated to x as an upvalue and removes cases from the list, resetting the association.";
xTagSet::usage="xTagSet[{f, lhs}, rhs] assigns rhs to be the value of lhs, and associates the assignment with the symbol f. There are two differences with TagSet: 1) The symbol f is unprotected and later protected again if required; 2) xTagSet evaluates rhs (consistently with all other Set functions), but TagSet does not evaluate it!";
xTagSetDelayed::usage="xTagSetDelayed[{f, lhs}, rhs] assigns rhs to be the delayed value of lhs, and associates the assignment with the symbol f. The only difference with TagSetDelayed is that the symbol f is unprotected and later protected again if required.";
SubHead::usage="SubHead[expr] returns the atom obtained after nesting Head as many times as needed.";


(* MapRule *)
MapRuleR::usage="MapRuleR[f, expr] maps f onto the second argument of all Rule or RuleDelayed expressions of expr.";
MapRuleL::usage="MapRuleL[f, expr] maps f onto the first argument of all Rule or RuleDelayed expressions of expr.";


(* Linking symbols *)
$LinkCharacter::usage="$LinkCharacter is a global variable containing a single character to be used in linking symbols to define a new symbol. By default we use the symbol \"\[UnderBracket]\" (under-bracket), imitating the package Notations.";
LinkSymbols::usage="LinkSymbols[{s1, s2, ...}] returns a single symbol formed by joining the symbols s1, s2, ... inserting the charcter $LinkCharacter in between any two of them.";
UnlinkSymbol::usage="UnlinkSymbol[symbol] returns a list of symbols obtained by breaking the given symbol at those points having the character $LinkCharacter.";


(* Evaluation functions *)
xEvaluateAt::usage="xEvaluateAt[expr, pos] forces evaluation of expr at the position indicated by pos. xEvaluateAt[expr, {pos1, pos2, ...}] forces evaluation at several positions.";
xAct`xCore`Private`HoldMap::usage="xAct`xCore`Private`HoldMap[f, expr] maps and evaluates f on the elements of expr without evaluating the whole expression, which is returned wrapped in xHold. xAct`xCore`Private`HoldMap[f, expr, g] returns the expression wrapped with the function g.";
xHold::usage="xHold reproduces the behaviour of HoldForm, but with much lower precedence. It is introduced to avoid conflicts with the use of HoldForm by the user.";


(* DefType command xtension *)
xTension::usage="xTension[package, defcommand, moment]:=func declares func[args] to be executed at some moment during the execution of a defcommand (DefType[args] or UndefType[args]) in the xAct packages. This allows extensions of commands like DefMetric without actually modifying the code of xTensor. defcommand must be a symbol, like DefMetric or UndefMetric. package must be a string with the name of the package making the extension, and this is used to separate the extensions of different packages. Currently moment must be either \"Beginning\" or \"End\", representing whether the code is executed at the beginning or the end of the defcommand. The code is provided as a function func taking the same arguments args as defcommand[args].";
MakexTensions::usage="MakexTensions[defcommand, moment, symbol] collects the xTension expressions defined for defcommand at the given moment (\"Beginning\" or \"End\") and applies them to the symbol being defined.";


(* Change configuration variables *)
ReportSet::usage="ReportSet[var, val] sets assigns the value val to the variable var, reporting it if the variable had before a different value.";
ReportSetOption::usage="ReportSetOption[s, opt->val] sets the value val for the option opt of the symbol s, reporting it if the option had before a different value.";


(* Functions for previous versions of Mathematica *)
If[$VersionNumber<4.1,Ordering::usage="Ordering[{e1, e2, ...}] gives the permutation-list that brings the ei to the canonical order defined by OrderedQ."];


(* Tests *)
xTest::usage="xTest[s, expr, res, messQ] executes the expression expr and compares the result with the expected expression res, using SameQ. The argument messQ says whether we expect messages will be produced during the execution of expr. The checks yield a True/False answer that is stored as TestResult[s, c] where c is an integer that increases for each test performed for the symbol s.";
TestCounter::usage="TestCounter[s] stores the number of tests performed so far for the symbol s.";
TestResult::usage="TestResult[s, c] stores a True/False value reporting whether the test number c for the symbol s was successfull or not.";
AllTests::usage="AllTests[] return the list of all True/False values stored in TesteResult. AllTests[s] returns only those values associated to the tests for the symbol s.";
CheckTests::usage="CheckTests[] report whether all tests performed passed or failed. CheckTests[s] reports only on the tests for the symbol s.";


(* Types of names *)
$EMNames::usage="$EMNames gives the list of all names of symbols defined by the package ExpressionManipulation.";
$xPermNames::usage="$xPermNames gives the list of all names of symbols defined by the package xPerm.";
$xTensorNames::usage="$xTensorNames gives the list of names of all symbols defined by the package xTensor.";
$xCoreNames::usage="";
$xTableauNames::usage="";
$xCobaNames::usage="";
$InvarNames::usage="";
$xPertNames::usage="";
$HarmonicsNames::usage="";
$SpinorsNames::usage="";
$xActNames::usage="";
$SystemNames::usage="$SystemNames gives the list of names of all built-in symbols in Mathematica.";
$SpecialOutputNames::usage="$SpecialOutputNames gives the list of names of Mathematica symbols in $SystemNames whose output differs from them.";
FindSymbols::usage="FindSymbols[expr] gives the list of all symbols in expr, without evaluating expr. FindSymbols[expr, f] gives the list of all symbols in expr, each wrapped with the function f; if the function f has a Hold attribute then none of the symbols of expr is evaluated in the process.";
ValidateSymbol::usage="ValidateSymbol[symbol] throws an error if symbol: 1) has a value or numeric character, or is not atomic, 2) its name is locked, protected or readprotected, or 3) it is already used by Mathematica or any of the xAct packages already loaded. Otherwise it gives Null. (Read)Protection is not checked for the capitals C, D, K, N, O, used by Mathematica. Symbols E and I have numeric value.";


(* Dagger character *)
$DaggerCharacter::usage="$DaggerCharacter gives the character string to be appended to a daggered symbol. By default it is the dagger symbol \[Dagger].";
HasDaggerCharacterQ::usage="HasDaggerCharacterQ on symbol or -symbol returns True if symbol contains $DaggerCharacter as one of its characters, and False otherwise. On any other type of expression it throws an error.";
MakeDaggerSymbol::usage="MakeDaggerSymbol[symbol] returns a symbol with an added $DaggerCharacter if it does not already have it as last chracter, or it removes the $DaggerCharacter if symbol already has it. If the symbol contains a dollar, then the character is placed just before the first dollar. On nonsymbol expressions this function throws an error.";


Begin["`Private`"]


(******************* 2. Functions *******************)


If[System`$VersionNumber<4.1,Ordering[list_List]:=Last/@Sort[Transpose[{list,Range@Length@list}]]];


If[System`$VersionNumber<7.,
DeleteDuplicates[list_]:=Module[{f,x},f[x_]:=(f[Verbatim[x]]=Sequence[];x);f/@list]
];


If[6.5<System`$VersionNumber<8.5,
Unprotect[DeleteDuplicates];
DeleteDuplicates[head_[elems___]/;head=!=List]:=head@@DeleteDuplicates[{elems}];
Protect[DeleteDuplicates];
];


If[System`$VersionNumber<6.5,DeleteDuplicates[list_]:=Module[{f,x,r},f[x_]:=(f[Verbatim[x]]=Sequence[];x);r=f/@list;Remove[f];r];
SetNumberOfArguments[DeleteDuplicates,1];
Protect[DeleteDuplicates];
]


If[System`$VersionNumber<9.0,DuplicateFreeQ[expr_]:=SameQ[DeleteDuplicates[expr],expr]];


$WarningFrom=General;
General::warning=Switch[System`$VersionNumber,
_?(#>=5.&),"\!\(\*StyleBox[`1`,FontColor->RGBColor[0,0,1]]\)",
_?(#<5.&),"\!\(\*StyleBox[`1`,FontColor->RGBColor[1,0,0]]\)"]<>" : From `2`."


Off[General::warning]


(* General messages *)
General::versions="Loaded `1` version `2` but expected version `3` at least.";
General::unknown="Unknown `1` `2`.";
General::invalid="`1` is not a valid `2`.";
General::noundef="`1` `2` cannot be undefined because `3`.";
General::nouse="Attempting to apply `1` on `2`.";
General::empty="Argument number `1` cannot be an empty list of `2`.";
General::notyet="Sorry. Cannot work with `1` yet.";
General::missing="There is no `1` in `2`.";
General::undef="Undefined `1` of `2`.";
General::error="`1`";
General::error1="`1` `2`";
General::error2="`1` `2` `3`";
General::error3="`1` `2` `3` `4`";


General::nostdvar="Nonstandard configuration: variable `1` now has value `2`.";
General::nostdopt="Nonstandard configuration: option `1` of `2` now has value `3`.";


$ReportToJose="Uhh. This was not expected (though there is nothing wrong). Please, report to jose@xact.es."


CheckOptions[]:={};
CheckOptions[opts__]:=If[OptionQ[{opts}]&&MemberQ[{1,2},ArrayDepth[{opts}]],
Flatten[{opts},1],
Throw[Message[CheckOptions::invalid,opts,"option or list of options"]]];
Protect[CheckOptions];


TrueOrFalse[True|False]:=True;
TrueOrFalse[_]:=False;
SetNumberOfArguments[TrueOrFalse,1];
Protect[TrueOrFalse];


SetAttributes[setargs,HoldFirst];

setargs[f_,n_,n_]:=Null;
setargs[f_,Infinity,1]:=SetDelayed[f[_,x__],Message[f::argx,f,1+Length[{x}]]];
setargs[f_,Infinity,n_]:=SetDelayed[f[##],Message[f::argrx,f,n+Length[{x}],n]]&@@Table[Blank[],{n}]~Join~{Pattern[x,BlankSequence[]]};
setargs[f_,1,n_/;n=!=1]:=SetDelayed[f[_],Message[f::argr,f,n]];
setargs[f_,i_/;i=!=1,1]:=SetDelayed[f[##],Message[f::argx,f,i]]&@@Table[Blank[],{i}];
setargs[f_,i_,n_]:=SetDelayed[f[##],Message[f::argrx,f,i,n]]&@@Table[Blank[],{i}];

setargs[f_,n_,{n_,Infinity}]:=Null;
setargs[f_,Infinity,{n_,Infinity}]:=SetDelayed[f[##],Message[f::argm,f,n+Length[{x}],n]]&@@Table[Blank[],{n}]~Join~{Pattern[x,BlankSequence[]]};
setargs[f_,1,{n_/;n=!=1,Infinity}]:=SetDelayed[f[_],Message[f::argmu,f,n]];
setargs[f_,i_,{n_,Infinity}]:=SetDelayed[f[##],Message[f::argm,f,i,n]]&@@Table[Blank[],{i}];

setargs[f_,Infinity,{0,0}]:=setargs[f,Infinity,0];
setargs[f_,Infinity,{0,n_}]:=SetDelayed[f[##],Message[f::argf,f,n+Length[{x}],n]]&@@Table[Blank[],{n}]~Join~{Pattern[x,BlankSequence[]]};
setargs[f_,0,{1,2}]:=SetDelayed[f[],Message[f::argt,f,0,1,2]];
setargs[f_,Infinity,{1,2}]:=SetDelayed[f[_,_,x__],Message[f::argt,f,2+Length[{x}],1,2]];
setargs[f_,0,{2,3}]:=SetDelayed[f[],Message[f::argt,f,0,2,3]];
setargs[f_,1,{2,3}]:=SetDelayed[f[_],Message[f::argt,f,1,2,3]];
setargs[f_,Infinity,{2,3}]:=SetDelayed[f[_,_,_,x__],Message[f::argt,f,3+Length[{x}],2,3]];


SetAttributes[SetNumberOfArguments,HoldFirst];

(* Function with exactly n arguments *)
SetNumberOfArguments[f_,n_Integer]:=(
Do[setargs[f,i,n],{i,0,n-1}];
setargs[f,Infinity,n];
);

(* Function with n or more arguments *)
SetNumberOfArguments[f_,{n_Integer?Positive,Infinity}]:=
Do[setargs[f,i,{n,Infinity}],{i,0,n-1}];

(* Function with n or fewer arguments *)
SetNumberOfArguments[f_,{0,n_Integer?Positive}]:=setargs[f,Infinity,{0,n}];

(* Function with 1 or 2 arguments *)
SetNumberOfArguments[f_,{1,2}]:=(
setargs[f,0,{1,2}];
setargs[f,Infinity,{1,2}];
);

(* Function with 2 or 3 arguments *)
SetNumberOfArguments[f_,{2,3}]:=(
setargs[f,0,{2,3}];
setargs[f,1,{2,3}];
setargs[f,Infinity,{2,3}];
);


JustOne::notone="Expecting a list with one element, but got `1`.";
JustOne[{expr_}]:=expr;
JustOne[]:=Throw@Message[JustOne::notone,"nothing"];
JustOne[expr_]:=Throw@Message[JustOne::notone,expr];
SetNumberOfArguments[JustOne,1];
Protect[JustOne];


MapIfPlus[f_,expr_Plus]:=Map[f,expr];
MapIfPlus[f_,expr_]:=f[expr];
SetNumberOfArguments[MapIfPlus,2];
Protect[MapIfPlus];


ThreadArray[head_[arrayL_List,arrayR_List]]:=MapThread[head,{arrayL,arrayR},ArrayDepth[arrayL]];
SetNumberOfArguments[ThreadArray,1];
Protect[ThreadArray];


xEvaluateAt[expr_,positions_]:=Fold[ReplacePart[#1,Part[#1,Sequence@@#2],#2]&,Unevaluated[expr],positions];
SetNumberOfArguments[xEvaluateAt,2];
Protect[xEvaluateAt];


SetAttributes[xHold,HoldAll];
Format[xHold[expr_]]:=DisplayForm@Parenthesize[expr, StandardForm, 15, NonAssociative];
Protect[xHold];


Format::blank="Empty string cannot be `1`.";
Underline[""]:=Throw@Message[Format::blank,"underlined"];
Underline[x_String]:=StringJoin["\!\(",x,"\+_\)"];
Underline[x_]:=Underline[ToString[x]];


Underline["",color_]:=Throw@Message[Format::blank,"underlined"];
Underline[x_String,color_]:=StringJoin["\!\(",x,"\+\*StyleBox[\(_\),FontColor->",ToString[color],"]\)"];
Underline[x_,color_]:=Underline[ToString[x],color];
SetNumberOfArguments[Underline,{1,2}];
Protect[Underline];


Format::ntilde="Number of tildes `1` should be an integer. Replaced by 1.";


TildeString[x_]:=TildeString[x,1];


TildeString["",n_]:=Throw[Message[Format::blank,"tilded"]];
TildeString[x_String,0]:=x;
TildeString[x_String,n_Integer?Positive]:=StringJoin["\!\(",x,Table["\&~",{n}],"\)"];
TildeString[x_String,n_Integer?Negative]:=StringJoin["\!\(",x,Table["\+~",{-n}],"\)"];
TildeString[x_String,n_]:=(Message[Format::ntilde,n];TildeString[x,1]);
TildeString[x_,n_]:=TildeString[ToString[x],n];


TildeString["",n_,color_]:=Throw[Message[Format::blank,"tilded"]];
TildeString[x_String,0,color_]:=x;
TildeString[x_String,n_Integer?Positive,color_]:=With[{colortilde=StringJoin["\&\*StyleBox[\"~\",FontColor->",ToString[color],"]"]},StringJoin["\!\(",x,Table[colortilde,{n}],"\)"]];
TildeString[x_String,n_Integer?Negative,color_]:=With[{colortilde=StringJoin["\+\*StyleBox[\"~\",FontColor->",ToString[color],"]"]},StringJoin["\!\(",x,Table[colortilde,{-n}],"\)"]];
TildeString[x_String,n_,color_]:=(Message[Format::ntilde,n];TildeString[x,1,color]);
TildeString[x_,n_,color_]:=TildeString[ToString[x],n,color];
SetNumberOfArguments[TildeString,{1,3}];
Protect[TildeString];


(*ColorString[x_String,color_]:="\!\(\*StyleBox[\""<>x<>"\",FontColor->"<>ToString[N/@color]<>"]\)";*)
(* Replaced by the following line, as suggested by Thomas, to be able to change the color of an already colored string *)
ColorString[x_String,color_]:="\!\(\*StyleBox[\""<>StringReplace[x,StringExpression["\!\(\*StyleBox[\"",y___,"\",FontColor->",col__,"]\)"]:>y]<>"\",FontColor->"<>ToString[N/@color]<>"]\)";
ColorString[x_Symbol,color_]:=ColorString[ToString[x],color];
ColorString[x_Integer,color_]:=ColorString[ToString[x],color];
ColorString[x_,_]:=(Message[Format::nouse,"xTensor`Private`ColorString",x];x);
SetNumberOfArguments[ColorString,2];
Protect[ColorString];


Format::blank="Empty string cannot be `1`.";
Overline[""]:=Throw@Message[Format::blank,"overlined"];
Overline[x_String]:=StringJoin["\!\(",x,"\&_\)"];
Overline[x_]:=Overline[ToString[x]];


Overline["",color_]:=Throw@Message[Format::blank,"overlined"];
Overline[x_String,color_]:=StringJoin["\!\(",x,"\&\*StyleBox[\(_\),FontColor->",ToString[color],"]\)"];
Overline[x_,color_]:=Overline[ToString[x],color];
SetNumberOfArguments[Overline,{1,2}];
Unprotect[Overline];


Format::nhat="Number of hats `1` should be an integer. Replaced by 1.";


HatString[x_]:=HatString[x,1];


HatString["",n_]:=Throw[Message[Format::blank,"hatted"]];
HatString[x_String,0]:=x;
HatString[x_String,n_Integer?Positive]:=StringJoin["\!\(",x,Table["\&^",{n}],"\)"];
HatString[x_String,n_Integer?Negative]:=StringJoin["\!\(",x,Table["\+^",{-n}],"\)"];
HatString[x_String,n_]:=(Message[Format::nhat,n];HatString[x,1]);
HatString[x_,n_]:=HatString[ToString[x],n];


HatString["",n_,color_]:=Throw[Message[Format::blank,"hatted"]];
HatString[x_String,0,color_]:=x;
HatString[x_String,n_Integer?Positive,color_]:=With[{colorhat=StringJoin["\&\*StyleBox[\"^\",FontColor->",ToString[color],"]"]},StringJoin["\!\(",x,Table[colorhat,{n}],"\)"]];
HatString[x_String,n_Integer?Negative,color_]:=With[{colorhat=StringJoin["\+\*StyleBox[\"^\",FontColor->",ToString[color],"]"]},StringJoin["\!\(",x,Table[colorhat,{-n}],"\)"]];
HatString[x_String,n_,color_]:=(Message[Format::nhat,n];TildeString[x,1,color]);
HatString[x_,n_,color_]:=HatString[ToString[x],n,color];
SetNumberOfArguments[HatString,{1,3}];
Protect[HatString];


SetAttributes[xUpSet,{HoldFirst,SequenceHold}];
xUpSet[head_[symbol_],rhs_]:=With[{protected=Unprotect@@{SubHead[symbol]}},
head[symbol]^=rhs;
Protect[protected];
head[symbol]];
xUpSet[head_[symbol_]]:=With[{subhead=SubHead[symbol],protected=Unprotect@@{SubHead[symbol]}},
TagUnset[subhead,head[symbol]];
Protect[protected];
];
SetNumberOfArguments[xUpSet,{1,2}];
Protect[xUpSet];


SetAttributes[xUpSetDelayed,{HoldAll,SequenceHold}];
xUpSetDelayed[head_[symbol_],rhs_]:=With[{protected=Unprotect@@{SubHead[symbol]}},
head[symbol]^:=rhs;
Protect[protected];
head[symbol]];
xUpSetDelayed[head_[symbol_]]:=With[{subhead=SubHead[symbol],protected=Unprotect@@{SubHead[symbol]}},
TagUnset[subhead,head[symbol]];
Protect[protected];
];
SetNumberOfArguments[xUpSetDelayed,{1,2}];
Protect[xUpSetDelayed];


SetAttributes[xUpAppendTo,HoldFirst];
xUpAppendTo[head_[symbol_],element_]:=xUpSet[head[symbol],Append[If[ValueQ[head[symbol]],head[symbol],{}],element]];
SetNumberOfArguments[xUpAppendTo,2];
Protect[xUpAppendTo];


SetAttributes[xUpDeleteCasesTo,HoldFirst];
xUpDeleteCasesTo[head_[symbol_],cases_]:=xUpSet[head[symbol],DeleteCases[head[symbol],cases]];
SetNumberOfArguments[xUpDeleteCasesTo,2];
Protect[xUpDeleteCasesTo];


SubHead[symbol_Symbol]:=symbol;
SubHead[expr_]:=NestWhile[Head,expr,Function[!AtomQ[#]]];


SetAttributes[xTagSet,{HoldFirst,SequenceHold}];
xTagSet[{expr_,lhs_},rhs_]:=With[{symbol=SubHead[expr]},
With[{protected=Unprotect[symbol]},
TagSet[symbol,lhs,rhs];
Protect[protected];
rhs]
];
SetNumberOfArguments[xTagSet,2];
Protect[xTagSet];


SetAttributes[xTagSetDelayed,{HoldAll,SequenceHold}];
xTagSetDelayed[{expr_,lhs_},rhs_]:=With[{symbol=SubHead[expr]},
With[{protected=Unprotect[symbol]},
TagSetDelayed[symbol,lhs,rhs];
Protect[protected];
]
];
SetNumberOfArguments[xTagSetDelayed,2];
Protect[xTagSetDelayed];


MapRuleR[f_,list_List]:=Map[MapRuleR[f,#]&,list];
MapRuleR[f_,Rule[x_,y_]]:=Rule[x,f[y]];
MapRuleR[f_,RuleDelayed[x_,y_]]:=RuleDelayed[x,f[y]];
MapRuleR[f_,FoldedRule[rules__,last_]]:=FoldedRule[rules,MapRuleR[f,last]];
MapRuleR[f_,expr_]:=expr;
SetNumberOfArguments[MapRuleR,2];
Protect[MapRuleR];


MapRuleL[f_,list_List]:=Map[MapRuleL[f,#]&,list];
MapRuleL[f_,Rule[x_,y_]]:=Rule[f[x],y];
MapRuleL[f_,RuleDelayed[x_,y_]]:=RuleDelayed[f[x],y];
MapRuleL[f_,FoldedRule[rules__,last_]]:=FoldedRule[rules,MapRuleL[f,last]];
MapRuleL[f_,expr_]:=expr;
SetNumberOfArguments[MapRuleL,2];
Protect[MapRuleL];


Attributes[FoldedRule]={Flat,OneIdentity};
FoldedRule/:ReplaceAll[expr_,FoldedRule[rules___]]:=Fold[ReplaceAll,expr,{rules}];
FoldedRule/:ReplaceRepeated[expr_,FoldedRule[rules___]]:=Fold[ReplaceRepeated,expr,{rules}];
Protect[FoldedRule];


maprule[f_][head_[lhs_,rhs_]]:=head[lhs,f[rhs]];


collapse1[FoldedRuleTMP[rules_],f_]:=Flatten[{rules}];
collapse1[FoldedRuleTMP[rules___,rulesm2_,rulesm1_],f_]:=FoldedRuleTMP[rules,Flatten[{Map[maprule[f],Flatten[{rulesm2/.rulesm1}],{1}],rulesm1}]];
collapse[FoldedRule[rules__],f_]:=Nest[collapse1[#,f]&,FoldedRuleTMP[rules],Length[{rules}]];


CollapseFoldedRule::pos="Cannot collapse positions `1`.";
CollapseFoldedRule[frule_]:=CollapseFoldedRule[frule,{1,Length[frule]},Identity];
CollapseFoldedRule[frule_,All,f_:Identity]:=CollapseFoldedRule[frule,{1,Length[frule]},f];
CollapseFoldedRule[frule_,0|{0,0},f_:Identity]:=frule;
CollapseFoldedRule[frule_,n_Integer?Positive,f_:Identity]:=CollapseFoldedRule[frule,{1,n},f];
CollapseFoldedRule[frule_,n_Integer?Negative,f_:Identity]:=CollapseFoldedRule[frule,{n+1,0}+Length[frule],f];
CollapseFoldedRule[frule_,{first_Integer?Negative,last_Integer?Negative},f_:Identity]:=CollapseFoldedRule[frule,{first,last}+Length[frule]+1,f];
CollapseFoldedRule[frule_,{first_Integer?Positive,last_Integer?Negative},f_:Identity]:=CollapseFoldedRule[frule,{first,last+1+Length[frule]},f];
CollapseFoldedRule[frule_,{first_Integer?Positive,last_Integer?Positive}/;last>=first,f_:Identity]:=FoldedRule[Take[frule,{1,first-1}],collapse[Take[frule,{first,last}],f],Take[frule,{last+1,Length[frule]}]];
CollapseFoldedRule[frule_,positions_,f_:Identity]:=Throw[Message[CollapseFoldedRule::pos,positions]];
SetNumberOfArguments[CollapseFoldedRule,{1,3}];
Protect[CollapseFoldedRule];


DependentRules[list_]:=List@@DeleteCases[list,HoldPattern[x_->x_]];
DependentRules[frule_FoldedRule]:=Flatten[Join@@Map[List,Drop[frule,-1]]];
IndependentRules[list_]:=Cases[list,HoldPattern[x_->x_]];
IndependentRules[frule_FoldedRule]:=Flatten[{Last[frule]}];
SetNumberOfArguments[DependentRules,1];
SetNumberOfArguments[IndependentRules,1];
Protect[DependentRules,IndependentRules];


SetAttributes[AppendToUnevaluated,HoldFirst];
append[Hold[{values___}],value_]:=Hold[{values,value}];
AppendToUnevaluated[symbol_Symbol,value_]:=Set[OwnValues[symbol],ruledelayed[HoldPattern[symbol],append[Extract[OwnValues[symbol],{1,2},Hold],value]]];
ruledelayed[LHS_,Hold[RHS_]]:=RuleDelayed[LHS,RHS];
Protect[AppendToUnevaluated];


MakexTensions[defcommand_Symbol,moment_String,args__]:=ReleaseHold[#][args]&/@Cases[DownValues[xTension],rule_[holdpattern_[xTension[package_,defcommand,moment,___]],action_]:>Hold[action]];


SetAttributes[ReportSet,HoldFirst];
ReportSet[var_,value_]:=If[var=!=value,
If[ValueQ[var],
Print["** Variable ",Unevaluated[var]," changed from ",var," to ",value],
Print["** Variable ",Unevaluated[var]," assigned value ",value]
];
Set[var,value]
];
Protect[SetConfigurationVariable];


ReportSetOption[symbol_,option_->value_]:=If[(option/.Options[symbol])=!=value,
Print["** Option ",option," of ",symbol," changed from ",option/.Options[symbol]," to ",value];
SetOptions[symbol,option->value]
];
Protect[ReportSetOption];


Clear[xTest,TestResult,TestCounter,AllTests];

SetAttributes[xTest,HoldAll];
SetAttributes[{TestResult,TestCounter,AllTests},HoldFirst];

TestCounter[]:=(TestCounter[]=0);
TestCounter[symbol_]:=(TestCounter[symbol]=0);

xTest[symbol_,expr_,expected_,messageQ_:False,postprocess_:Null]:=Module[{eval,result},
TestCounter[]++;
TestCounter[symbol]++;
eval=(
TestResult[symbol,TestCounter[symbol]]=
Check[SameQ[result=With[{res=Catch[expr]},Hold[res]],Hold[expected]],messageQ]
);
If[!eval,Print["Obtained instead: ",result]];
postprocess;
{HoldForm[symbol],TestCounter[symbol],eval}
];


AllTests[]:=Last/@DownValues[TestResult];
AllTests[symbol_]:=
Cases[DownValues[TestResult],_[_[TestResult[symbol,_]],result_]:>result]


CheckTests[symbol___]:=With[{alltests=AllTests[symbol]},
If[And@@alltests &&Length@alltests===TestCounter[],

Print["All ",Length[alltests]," tests passed"],

Print["These tests failed: "];Print[(First/@DownValues[TestResult][[Flatten@Position[alltests,False]]])/.{HoldPattern->HoldForm,TestResult->List}//Column]

]
];


NoPattern[x_PatternTest]:=NoPattern[First[x]];
NoPattern[x_Pattern]:=NoPattern[First[x]];
NoPattern[x_Blank]:=NoPattern[First[x]];
NoPattern[f_[args___]]:=NoPattern[f]@@(NoPattern/@{args});
NoPattern[x_]:=x;
Protect[NoPattern];


(******************* 3. Symbols, names and messages *******************)


If[$ReadingVerbose,Print["Reading section 1: Symbols, names and messages."],Null,Null]


$EMNames=Names["xAct`ExpressionManipulation`*"];
$xCoreNames=Names["xAct`xCore`*"];
$SystemNames=DeleteCases[Names["System`*"],"$DefaultImagingDevice"|"$ImagingDevices"];


$xPermNames:={};
$xTableauNames:={};
$xTensorNames:={};
$xCobaNames:={};
$InvarNames:={};
$HarmonicsNames:={};
$xPertNames:={};
$SpinorsNames:={};


$xActNames:=Join[$EMNames,$xCoreNames,$xPermNames,$xTableauNames,$xTensorNames,$xCobaNames,$InvarNames,$HarmonicsNames,$xPertNames,$SpinorNames];


$SpecialOutputNames={};


LockedQ[symbol_]:=MemberQ[Attributes[symbol],Locked]
$LockedNames=Select[Complement[$SystemNames,$SpecialOutputNames],LockedQ]


ProtectedQ[symbol_]:=MemberQ[Attributes[symbol],Protected]
$ProtectedNames=Select[Complement[$SystemNames,$SpecialOutputNames,$LockedNames],ProtectedQ];


ReadProtectedQ[symbol_]:=MemberQ[Attributes[symbol],ReadProtected]
$ReadProtectedNames=Select[Complement[$SystemNames,$SpecialOutputNames],ReadProtectedQ];


$LengthOneNames=Select[$SystemNames,StringLength[#]==1&]


SetAttributes[FindSymbols,HoldAllComplete];
FindSymbols[expr_,f_:Identity]:=Cases[Level[Unevaluated[expr],{-1},Hold,Heads->True],x_Symbol:>f[x]];
FindSymbols[]:={};
SetNumberOfArguments[FindSymbols,{0,2}];
Protect[FindSymbols];


SystemCapitalQ[symbol_]:=And[MemberQ[$LengthOneNames,ToString[symbol]],Context[symbol]==="System`"];


ValidateSymbol::invalid="Symbol `1` is invalid because it `2`.";
ValidateSymbol::protected="Symbol `1` is `2`.";
ValidateSymbol::used="Symbol `1` is already used `2`.";


SetAttributes[ValidateSymbol,HoldFirst]
ValidateSymbol[symbol_]:=With[{name=ToString[Unevaluated[symbol]]},
Which[

(* Errors: generic problems *)
NumericQ[symbol],
Throw@Message[ValidateSymbol::invalid,name,"has numeric value"],
ValueQ[symbol],
Throw@Message[ValidateSymbol::invalid,name,"has an ownvalue"],
!AtomQ[symbol],
Throw@Message[ValidateSymbol::invalid,name,"is not atomic"],
LockedQ[Symbol[name]],
Throw@Message[ValidateSymbol::protected,name,"is Locked"],

(* Errors: xAct problems *)
MemberQ[$EMNames,name],
Throw@Message[ValidateSymbol::used,name,"by ExpressionManipulation"],
MemberQ[$xCoreNames,name],
Throw@Message[ValidateSymbol::used,name,"by xCore"],
MemberQ[$xPermNames,name],
Throw@Message[ValidateSymbol::used,name,"by xPerm"],
MemberQ[$xTableauNames,name],
Throw@Message[ValidateSymbol::used,name,"by xTableau"],
MemberQ[$xTensorNames,name],
Throw@Message[ValidateSymbol::used,name,"by xTensor"],
MemberQ[$xCobaNames,name],
Throw@Message[ValidateSymbol::used,name,"by xCoba"],
MemberQ[$InvarNames,name],
Throw@Message[ValidateSymbol::used,name,"by Invar"],
MemberQ[$HarmonicsNames,name],
Throw@Message[ValidateSymbol::used,name,"by Harmonics"],
MemberQ[$xPertNames,name],
Throw@Message[ValidateSymbol::used,name,"by xPert"],
MemberQ[$SpinorsNames,name],
Throw@Message[ValidateSymbol::used,name,"by Spinors"],

SystemCapitalQ[symbol],
Return[],

(* Errors: Mathematica problems *)
ProtectedQ[Symbol[name]],
Throw@Message[ValidateSymbol::protected,name,"Protected"],
ReadProtectedQ[Symbol[name]],
Throw@Message[ValidateSymbol::protected,name,"ReadProtected"],
MemberQ[$SystemNames,name]&&Context[name]=="System`",
Throw@Message[ValidateSymbol::used,name,"by Mathematica"]

]
];
SetNumberOfArguments[ValidateSymbol,1];
Protect[ValidateSymbol];


SymbolJoin[symbols__]:=Symbol@StringJoin[ToString/@{symbols}];
SymbolJoin[]:=Null;
Protect[SymbolJoin];


$LinkCharacter="\[UnderBracket]";


LinkSymbols[symbols_List]:=SymbolJoin@@Insert[symbols,$LinkCharacter,Partition[Range[2,Length[symbols]],1]];
SetNumberOfArguments[LinkSymbols,1];
Protect[LinkSymbols];


UnlinkSymbol[symbol_Symbol]:=UnlinkSymbol2[ToString[symbol]];
UnlinkSymbol2[string_String]:=UnlinkSymbol3[string,First/@StringPosition[string,$LinkCharacter]];
UnlinkSymbol3[string_,{}]:={Symbol[string]};
UnlinkSymbol3[string_,positions_List]:=Map[Symbol,Map[StringTake[string,#]&,Transpose[{Prepend[positions+1,1],Append[positions-1,Length[Characters[string]]]}]]];
SetNumberOfArguments[UnlinkSymbol,1];
Protect[UnlinkSymbol];


$DaggerCharacter="\[Dagger]";


HasDaggerCharacterQ[-symbol_Symbol]:=HasDaggerCharacterQ[SymbolName[symbol]];
HasDaggerCharacterQ[symbol_Symbol]:=HasDaggerCharacterQ[SymbolName[symbol]];
HasDaggerCharacterQ[string_String]:=StringPosition[string,$DaggerCharacter]=!={};
HasDaggerCharacterQ[x_]:=Throw@Message[Validate::nouse,"HasDaggerCharacterQ",x];
SetNumberOfArguments[HasDaggerCharacterQ,1];
Protect[HasDaggerCharacterQ];


MakeDaggerSymbol[symbol_Symbol]:=ToExpression[StringJoin[Characters[ToString[symbol]]/.With[{dg=$DaggerCharacter},{{i__,dg,j___}->{i,j},{i__,"$",j___}->{i,dg,"$",j},{i__}->{i,dg}}]]];
MakeDaggerSymbol[x_]:=Throw@Message[Validate::nouse,"MakeDaggerSymbol",x];
SetNumberOfArguments[MakeDaggerSymbol,1];
Protect[MakeDaggerSymbol];


End[]


EndPackage[]
