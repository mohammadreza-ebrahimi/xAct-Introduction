(* :Title: ExpressionManipulation *)
(* :Context: Algebra`ExpressionManipulation` *)
(* :Author: David J.M. Park Jr., Ted Ersek *)
(* :Summary: 
    ExpressionManipulation contains routines useful in detailed 
    manipulation of algebraic expressions. 
    It also creates a palette for finding the positions of
    subexpressions in larger algebraic expressions. *)
(* :Copyright: \[Copyright] 1999, 2000, 2001
    by David J.M. Park Jr. and Ted Ersek *)
(* :Package Version: 1.0 *)
(* :Mathematica Version: 4.0 *)
(* :Keywords: evaluation, find, position, palette *)

(* Modifications by JMM 
   - March 2005: Symbols EMIndex, EMPosition, x are put in the
     xAct`ExpressionManipulation` context
   - June 2007: Overloading of built-ins changed to new functions
     NewMapAt, NewExtract, NewReplacePart. Every MapAt changed to
     NewMapAt and then created a fall back definition. Usage message of
     ExpressionManipulation modified accordingly.
*)



BeginPackage["xAct`ExpressionManipulation`"];

ExpressionManipulation::usage=
    "ExpressionManipulation contains routines to aid in the detailed \
manipulation of algebraic expressions.\n1) PositionsPalette creates a palette \
which can be used to find positions of highlighted portions of non-Held \
expressions (Deactivated in xAct).\n2) ColorPositions will color and label \
parts of an expression \
according to a position list.\n3) ExtendedPosition will find positions of \
specific patterns which correspond to a level subset of parts of a \
subexpression.\n4) Extract, MapAt and ReplacePart have been extended to \
new functions NewExtract, NewMapAt and NewReplacePart which \
accept extended positions.\n5) EvaluateAt, EvaluateAtPattern and \
EvaluateAtLevel are used for detailed evaluation of parts of Held \
expressions.\n6) CompleteTheSquare, RootsTogether and NDTimesExpand handle \
some common algebraic manipulations.\n7) ErsekComplexity is an alternative \
ComplexityFunction for Simplify.";

ColorPositions::usage=
    "ColorPositions[positions][expr] will color the expression at the \
specified positions. positions may be a single position, a list of positions, \
or a list of lists of positions. The positions within a single list of \
positions will have various colors. If positions consist of multiple lists, \
then positions within each group will have the same color but the different \
groups will have various colors. The option EMPositionTag determines how the \
positions will be tagged. Extended positions may be used. The option \
EMPositionFontSize can be used to control the font size in the position \
tags.";

(* JMM 2005: added two lines *)
EMIndex::usage="";
EMPosition::usage="";

EMPositionTag::usage=
    "EMPositionTag is an option for ColorPositions which tells how the \
colored expressions will be tagged. When set to EMPosition each colored \
expression will be tagged with its position. This is the default. When set to \
EMIndex each colored expression is tagged with the index of the position in \
the flattened list of positions. When set to None there are no tags on the \
colored expressions.";

EMPositionFontSize::usage=
    "EMPositionFontSize is an option for ColorPositions which gives the \
FontSize to use when colored positions are tagged with their actual position. \
Its default value is 10.";

CompleteTheSquare::usage=
    "CompleteTheSquare[expr, var:x] returns expr as a perfect square plus a \
constant. If the variable is not x, it must be supplied as the second \
argument.";
CompleteTheSquare::notquad="`1` is not a quadratic expression in `2`.";

eP::usage=
    "eP is a wrapper for extended positions. An extended position is a subset \
of level parts in a base position corresponding to a subexpression which is \
Flat and Orderless. eP[base position, list of level parts]. For example \
eP[{1},{2,4}] is an extended position and specifies the position of \n  a+c \
in f[1+a+b+c].";

ErsekComplexity::usage=
    "ErsekComplexity is a complexity function for Simplify and FullSimplify \
which sometimes results in simpler expressions but may take longer to run. \
Usage is Simplify[expr, ComplexityFunction \[Rule] ErsekComplexity] and \
likewise for FullSimplify.";

EvaluateAt::"usage"=
    "EvaluateAt[pos][expr] forces evaluation of expr at the position \
indicated by pos. EvaluateAt[{pos1, pos2, ...}][expr] forces evaluation at \
several positions. EvaluateAt[pos, f][expr] and EvaluateAt[{pos1, pos2, ...}, \
f][expr] evaluates function f on specified positions.";

EvaluateAtPattern::usage=
    "EvaluateAtPattern[pattern, levelspec:\[Infinity], \
functionname:Identity][expr] will evaluate held expressions at the positions \
within the level specification which match the pattern. A pure function given \
by functionname may be applied to the results of the evaluations.";

ExtendedPattern::usage=
    "ExtendedPattern[expr, pattern, levelspec:{0,\[Infinity]}, n:\[Infinity]] \
will find all the extended positions for which the subparts match the \
pattern. Only extended positions which contain two or more subparts will be \
returned. The optional arguments can be used to restrict the search levels \
and the number of extended positions returned.";

ExtendedPosition::usage=
    "ExtendedPosition[expr, pattern] gives a list of the positions at which \
objects matching pattern appear in expr. ExtendedPosition[expr, pattern, \
levspec] finds only objects whose base positions appear on levels specified \
by levspec. ExtendedPosition[expr, pattern, levspec, n] gives the positions \
of the first n objects found. ExtendedPosition is used to find extended \
positions which correspond to a subset of level parts of a Flat and Orderless \
operation . For example \n   ExtendedPosition[f[1+a+b+c,a+c],a+c]\
\[LongRightArrow]{eP[{1},{2,4}]}.";

(* JMM 2007: Commented out this line *)
(* $NewMessage[NewExtract,"usage"]; *)
NewExtract::usage=
  StringReplace[Extract::usage,"Extract"->"NewExtract"]<>
    " The positions in NewExtract can be extended positions such as \n \
eP[{1},{2,4}] which is the position of a+c in f[1+a+b+c]."

FlattenHold::usage=
    "FlattenHold[][expr] will Flatten any subexpression whose head has the \
attribute Flat. Flatten[f][expr] will flatten only expressions whose head \
matches the pattern f, regardless of their attributes. This is sometimes \
desirable after a replacement rule has introduced a nonflat expression which \
would normally be flattened in a non-Held expression.";

(* JMM 2007: Commented out this line *)
(* $NewMessage[NewMapAt,"usage"]; *)
NewMapAt::usage=
  StringReplace[MapAt::usage,"MapAt"->"NewMapAt"]<>
    " The positions in NewMapAt can be extended positions such as \n \
eP[{1},{2,4}] which is the position of a+c in f[1+a+b+c]."

NDTimesExpand::usage=
    "NDTimesExpand[expression, factor] multiplies the numerator and \
denominator of expr by factor and expands each of them.";

PositionQ::usage=
    "PositionQ[position][expr] will determine if position is a valid position \
in expr. Both regular and extended positions may be tested.";

(* JMM 2005: Commented out PossitionsPalette *)
(* PositionsPalette::usage=
    "GeneratePositionsPalette creates a PositionsPalette. The palette can be \
used to find the position of a subexpression in a larger expression which \
constitutes the contents of a cell. To capture an expression,highlight it and \
use the Capture[\[SelectionPlaceholder]] button. Select the subexpression and \
use the Positions[\[SelectionPlaceholder]] button. The position of the \
subexpression will be printed along with an indication whether that \
subexpression occurs at a single position in the expression or at multiple \
places in the expression. The subexpression will be highlighted in the \
expression. If the subexpression occurs at multiple positions, the first \
occurrance is highlighted and printed. The NextPosition button will cycle \
through the other positions of the subexpression.\n\n The cell contents can \
be reformatted using Explicit Format. The Restore button will then display \
the expression in the new format. Explicit Format will keep rational numbers \
together, and will format negative powers in explicit form. These items can \
then be selected as subexpressions. The cursor must always be returned to the \
cell before using any of the buttons. The palette is intended for use with \
unheld StandardForm expressions."; *)

(* JMM 2007: Commented out this line *)
(* $NewMessage[NewReplacePart,"usage"]; *)
NewReplacePart::usage=
  StringReplace[ReplacePart::usage,"MapAt"->"NewMapAt"]<>
    " The positions in NewReplacePart can be extended positions such as \n \
eP[{1},{2,4}] which is the position of a+c in f[1+a+b+c]."

RootsTogether::usage=
    "RootsTogether[expr] will put factors involving the same kind of root \
together under one root sign. Like PowerExpand it is not always a permissible \
operation. Mathematica automatically removes numeric factors from root \
expressions.";

Begin["`Private`"];

(* JMM 2007: Commented out this line *)
(* protected=Unprotect[Extract,MapAt,ReplacePart]; *)



(* JMM 2005: Options[ColorPositions]={EMPositionTag\[Rule]Global`EMPosition, *)
Options[ColorPositions]={EMPositionTag\[Rule]EMPosition,
      EMPositionFontSize\[Rule]10};

ColorPositions[positionlist_,opts___?OptionQ][expr_]:=
  Module[{indicator,pfontsize,posdisplay,color,hue,i,j,index,plist2,plist3,
      expr2,pos,replacement},
    
    indicator=EMPositionTag/.{opts}/.Options[ColorPositions];
    pfontsize=EMPositionFontSize/.{opts}/.Options[ColorPositions];
    expr2=HoldForm[expr];
    plist2=Switch[Depth[positionlist/.eP[__]\[Rule]{}],
        2,Partition[{positionlist},1],
        3,Partition[positionlist,1],
        4,positionlist];
    plist2=
      Map[If[Head[#]===eP,
            eP[Join[{1},#\[LeftDoubleBracket]1\[RightDoubleBracket]],#\
\[LeftDoubleBracket]2\[RightDoubleBracket]],Join[{1},#]]&,plist2,{2}];
    
    index=0;
    Do[
      plist3=plist2\[LeftDoubleBracket]i\[RightDoubleBracket];
      hue=If[Length[plist2]>1,0.7(i-1)/(Length[plist2]-1),0.5];
      color=Hue[hue,0.4,0.95];
      Do[
        pos=plist3\[LeftDoubleBracket]j\[RightDoubleBracket];
        posdisplay=
          Switch[indicator,
            None,None,
(* JMM 2005: Global`EMIndex,DisplayForm[FrameBox[++index]],Global`EMPosition, *)
            EMIndex,DisplayForm[FrameBox[++index]],EMPosition,
            DisplayForm[
              FrameBox[
                StyleForm[
                  plist2\[LeftDoubleBracket]i,
                      j\[RightDoubleBracket]/.{a_List\[RuleDelayed]Drop[a,1],
                      eP[a_,b_]\[RuleDelayed]eP[Drop[a,1],b]},
                  FontSize\[Rule]pfontsize]]]];
        
        If[PositionQ[pos][expr2],
          replacement=
            
            If[posdisplay===None,
              DisplayForm[
                StyleForm[FrameBox[NewExtract[expr2,pos,HoldForm]],
                  Background\[Rule]color]],
              StyleForm[
                SequenceForm[posdisplay," ",NewExtract[expr2,pos,HoldForm]],
                Background\[Rule]color]];
          expr2=
            Switch[pos,
                  _List,NewReplacePart[expr2,replacement,pos],
                  _eP,
                  eReplacePart[expr2, replacement&, pos]]/.Sequence[]\[Rule]
                  eP[]/.Sequence[a_]\[Rule]a
          ],
        {j,1,Length[plist3]}],
      {i,1,Length[plist2]}];
    expr2//.f_[a___,eP[],b___]\[Rule]f[a,b]]



eReplacePart[expr_,func_,pos_]:=
  Module[{expr2=Hold[expr],pos2,eRP,i,r,d},
    
    eRP[expr1_,func1_,pos1_]:=
      Module[{subexpr=First[NewExtract[expr1,{pos1}]],new2},
        new2=func1[subexpr];
        Switch[pos1,
          _List,NewReplacePart[expr1,r[new2],pos1],
          _eP,
          Module[{base=pos1\[LeftDoubleBracket]1\[RightDoubleBracket],
              parts=pos1\[LeftDoubleBracket]2\[RightDoubleBracket],partfirst,
              partrest,e2},
            partfirst=Join[base,{First[parts]}];
            partrest=Join[base,{#}]&/@Rest[parts];
            e2=NewReplacePart[expr1,r[new2],partfirst];
            NewReplacePart[e2,d[],partrest]]
          ]
        ];
    
    pos2=Switch[pos,
        _Integer,{{pos}},
        _eP,{pos},
        {_Integer..},{pos},
        _,pos];
    pos2=Switch[#,
            _List,Join[{1},#],
            _eP,
            eP[Join[{1},#\[LeftDoubleBracket]1\[RightDoubleBracket]],#\
\[LeftDoubleBracket]2\[RightDoubleBracket]]]&/@pos2;
    Do[expr2=
        eRP[expr2,func,pos2\[LeftDoubleBracket]i\[RightDoubleBracket]],{i,1,
        Length[pos2]}];
    expr2=expr2//.{r\[Rule]Sequence,d[]\[Rule]Sequence[]};
    First[expr2]
    ]



(* JMM 2005: \!\(CompleteTheSquare[expr_, var_:  Global`x] := \n\t *)
\!\(CompleteTheSquare[expr_, var_: x] := \n\t
    Module[{a, b, c}, \n\t
      If[\ Exponent[expr, var] \[NotEqual] 2, 
        Message[CompleteTheSquare::notquad, expr, var]; 
        Return[]]; \n\t{c, b, a} = CoefficientList[expr, var]; \n\t
      a \((var + b\/\(2  a\))\)\^2 + c - b\^2\/\(4  a\)]\)



ErsekComplexity:=
  Module[{DigitsLength,Digits1,Digits2,VariableCount},
    DigitsLength[0|1]=1/10;
    DigitsLength[-1]=1/8;
    DigitsLength[n_]:=Length[IntegerDigits[n]];
    Digits1[expr_]:=2Plus@@(DigitsLength[#]&/@Cases[expr,_Integer,-1]);
    Digits2[expr_]:=
      2Plus @@Flatten[
            Map[DigitsLength[#]&,({Numerator[#],Denominator[#]}&/@
                  Cases[expr,_Rational,{-1}]),{-1}]];
    VariableCount[expr_]:=Count[expr,_Symbol?(Not[NumericQ[#]]&),{-1}];
    LeafCount[#]+Digits1[#]+Digits2[#]+3VariableCount[#]&
    ]

DigitsLength[0|1]=0.1;
DigitsLength[-1]=0.125;
DigitsLength[n_]:=Length[IntegerDigits[n]];
Digits1[expr_]:=Plus@@(DigitsLength[#]&/@Cases[expr,_Integer,-1]);
Digits2[expr_]:=(
      e2=Cases[expr,p_Rational\[RuleDelayed]{Numerator[p],Denominator[p]}];
      Plus@@DigitsLength/@Flatten[e2]
      );
Digits3[expr_]:= (
      e3=Cases[expr,z_Complex\[RuleDelayed]{Re[z],Im[z]},{-1}];
      e3=Flatten[e3/.p_Rational\[RuleDelayed]{Numerator[p],Denominator[p]}];
      Plus@@DigitsLength/@e3
      );
VariableCount[expr_]:=Count[expr,_Symbol?(Not[NumericQ[#]]&),{-1}];
ErsekComplexity=
    LeafCount[#]+2.0(Digits1[#]+Digits2[#]+Digits3[#])+3.0*VariableCount[#]&;



ComparePosn[p1_List,p2_List]:=OrderedQ[{p1,p2}]

ComparePosn[p1_eP,p2_List]:=OrderedQ[{First@p1,p2}]

ComparePosn[p1_List,p2_eP]:=OrderedQ[{p1,First@p2}]

ComparePosn[p1_eP,p2_eP]:=OrderedQ[{First@p1,First@p2}]

EvaluateAt[posn:(_Integer|{__Integer}),f_:Identity][expr_]:=
  NewReplacePart[expr,NewExtract[expr,posn,f],posn]

EvaluateAt[posn:{{__Integer}..},f_:Identity][expr_]:=
  Fold[NewReplacePart[#1,NewExtract[#1,#2,f],#2]&,expr,Reverse[Sort[posn]]]

EvaluateAt[posn_eP,f_:Identity][expr_]:=
  NewReplacePart[expr,NewExtract[expr,posn,f],posn]

EvaluateAt[posn:{({__Integer}|_eP)..},f_:Identity][expr_]:=
  Fold[NewReplacePart[#1,NewExtract[#1,#2,f],#2]&,expr,
    Reverse[Sort[posn,ComparePosn]]]



Attributes[EvaluateAtPattern]={HoldFirst};
EvaluateAtPattern[ pattern_,levelspec_:\[Infinity], f_:Identity] [expr_]:=
    Module[{pos1},
        pos1 =  Position[expr,Unevaluated[pattern],levelspec];
    Fold[NewReplacePart[#1, NewExtract[#1, #2, f], #2] &, expr,
            Reverse[Sort[pos1]]]
        ]



ExtendedPattern[expr_,pattern_,levelspec_:{0,\[Infinity]},n_:\[Infinity]]:=
  Module[{poslist},
    poslist=Sort[Position[expr,pattern,levelspec]];
    poslist=Split[poslist,Drop[#1,-1]===Drop[#2,-1]&];
    poslist=
      eP[Drop[#\[LeftDoubleBracket]1\[RightDoubleBracket],-1],
            Last[Transpose[#]]]&/@poslist;
    poslist=Select[poslist,Length[Last[#]]>1&];
    Take[poslist,Min[n,Length[poslist]]]
    ]



extractpositions[elist_,plist_]:=
  Module[{flist=elist,qlist=plist,dummy,i,p},
    Do[
      p=Position[flist, qlist\[LeftDoubleBracket]i\[RightDoubleBracket],{1},
          1];
      If[p=!={},qlist\[LeftDoubleBracket]i\[RightDoubleBracket]=p;
        flist\[LeftDoubleBracket]Sequence@@First[p]\[RightDoubleBracket]=
          dummy,qlist\[LeftDoubleBracket]i\[RightDoubleBracket]=dummy],
      {i,1,Length[plist]}];
    qlist=Flatten[qlist];
    If[FreeQ[qlist,dummy],qlist,$Failed]
    ]

ExtendedPosition[expr_,pattern_,levelspec_:{0,\[Infinity]},n_:\[Infinity]]:=
  Module[{headpositions,patternhead,headcases,drop,pparts,eparts,holdpattern,
      flat,subposn},
    patternhead=
      If[Head[pattern]===HoldPattern,holdpattern=True;
        pattern\[LeftDoubleBracket]1,0\[RightDoubleBracket],holdpattern=False;
        Head[pattern]];
    flat=And@@(MemberQ[Attributes[Evaluate[patternhead]],#]&/@{Flat,
              Orderless});
    pparts=
      If[holdpattern,ReleaseHold[First[List@@@Map[HoldPattern,pattern,{2}]]],
        List@@pattern];
    headpositions=Position[expr,_patternhead,levelspec];
    headcases=NewExtract[expr,headpositions,Hold];
    Do[
      eparts=
        ReleaseHold[
          List@@@headcases\[LeftDoubleBracket]i\[RightDoubleBracket]];
      subposn=extractpositions[eparts,pparts];
      If[subposn===$Failed,
        headpositions\[LeftDoubleBracket]i\[RightDoubleBracket]=drop,
        If[Length[subposn]<Length[eparts],
          headpositions\[LeftDoubleBracket]i\[RightDoubleBracket]=
            eP[headpositions\[LeftDoubleBracket]i\[RightDoubleBracket],
              subposn]]],
      {i,1,Length[headpositions]}];
    parts=
      Select[headpositions,\[Not](#===
                  drop\[Or](Head[#]\[Equal]eP\[And]\[Not]flat))&];
    If[Length[parts]>n,Take[parts,n],parts]
    ]







NewExtract[expr_,part_eP]:=NewExtract[expr,part,Identity]

NewExtract[expr_,pos_eP,h_]:=Module[{a1,hd,TempHead},
    a1=First[NewExtract[expr,{First@pos},Hold]];
    hd=Part[a1,1,0];
    a1=NewExtract[a1,{1,#},Hold]&  /@Last[pos];
    h@@(Thread[TempHead@@a1,Hold]/.TempHead\[Rule]hd)
    ]

NewExtract[expr_,part_List?(MemberQ[#,_eP,{1}]&)]:=NewExtract[expr,#]&  /@part

NewExtract[expr_,part_List?(MemberQ[#,_eP,{1}]&),h_]:=NewExtract[expr,#,h]& /@part

(* JMM 2007: Fall back *)
NewExtract[expr_,list_]:=Extract[expr,list];
NewExtract[expr_,list_,h_]:=Extract[expr,list,h];





FlattenHold[][expr_]:=
  expr//.f_[a___,f_[b___],c___]\[RuleDelayed]
      f[a,b,c]/;MemberQ[Attributes[Evaluate[f]],Flat]

FlattenHold[g_][expr_]:=
  expr//.f_[a___,f_[b___],c___]\[RuleDelayed]f[a,b,c]/;MatchQ[f, g]



IndividualParts[p_eP]:=Append[First[p],#]& /@Last[p]

IndividualParts[lst_List]:=lst

ReplacementList[prt_eP,new_,temp_]:=Module[{lst},
    lst=Last[prt]/._Integer\[Rule]temp;
    Part[lst,1]=new;
    lst
    ]

ReplacementList[prt_List,new_,temp_]:={new}

NewMapAt[func_,expr_,posn_?(MemberQ[#,_eP,{1}]&)]:=
  Module[{temp,TempHold,PosnList,NewParts,Int,result,hd=Head[expr]},
    (Attributes[TempHold]={HoldAll};
      PosnList={IndividualParts/@posn//.{elems__List}\[RuleDelayed]
              Sequence[elems]};
      PosnList=Join[{1},#]& /@PosnList;
      NewParts=Flatten[
          ReplacementList[#,func/@NewExtract[expr,#,TempHold],temp]&/@posn];
      Int=List/@Range[Length@PosnList];
      result=NewReplacePart[Hold@@{expr},NewParts,PosnList,
            Int]/.TempHold[ex_]\[RuleDelayed]Unevaluated[ex];
      hd@@DeleteCases[result,temp,\[Infinity]]   )
    ]

NewMapAt[func_,expr_,posn_eP]:=NewMapAt[func,expr,{posn}]

(* JMM 2007: Fall back *)
NewMapAt[f_,expr_,n_]:=MapAt[f,expr,n];



\!\(NDTimesExpand[expr_, factor_] := 
    Expand[factor\ Numerator[expr]]\/Expand[factor\ Denominator[expr]]\)



PositionQ[pos_List][expr_]:=
  Module[{i,returnval=True,e1},
    If[pos\[Equal]{},Return[True]];
    If[Length[expr]<pos\[LeftDoubleBracket]1\[RightDoubleBracket],
      Return[False]];
    Do[e1=
        Map[Hold,
            NewExtract[expr,Take[pos,i],
              Hold],{2}]\[LeftDoubleBracket]1\[RightDoubleBracket];
      If[Length[e1]<pos\[LeftDoubleBracket]i+1\[RightDoubleBracket]\[Or]
          Depth[e1]<3,returnval=False;Break[]],{i,1,Length[pos]-1}];
    returnval
    ]
PositionQ[pos_eP][expr_]:=
  PositionQ[pos\[LeftDoubleBracket]1\[RightDoubleBracket]][expr]\[And]
    Length[expr\[LeftDoubleBracket]
          Sequence@@
            pos\[LeftDoubleBracket]1\[RightDoubleBracket]\[RightDoubleBracket]\
]\[GreaterEqual]Max[pos\[LeftDoubleBracket]2\[RightDoubleBracket]]


(* JMM 2005: Commented out PositionsPalette *)
(*
\!\(PositionsPalette := 
    Module[{palettenb, pcolor}, \[IndentingNewLine]pcolor = 
        RGBColor[0.796887, \ 0.812512, \ 
          0.906264]; \[IndentingNewLine]\[IndentingNewLine]capturefunction \
:= \[IndentingNewLine]Module[{}, \[IndentingNewLine]If[
            SelectedNotebook[] === ButtonNotebook[], 
            Print["\<Unselect the PositionsPalette\>"]; 
            Return[]]; \[IndentingNewLine]expression0 = 
            ToExpression[NotebookRead[evalnb]]; \[IndentingNewLine]If[
            expression0 === {}, captured = False; 
            Return[]]; \[IndentingNewLine]SelectionMove[evalnb, All, 
            CellContents]; \[IndentingNewLine]NotebookWrite[evalnb, \ 
            ToBoxes[expression0]]; \[IndentingNewLine]\(captured = True; 
          positionfound = 
            False;\)]; \[IndentingNewLine]\[IndentingNewLine]positionfunction \
:= \[IndentingNewLine]Module[{selection, npositions, dispexpr, 
            a}, \[IndentingNewLine]If[
            SelectedNotebook[] === ButtonNotebook[], 
            Print["\<Unselect the PositionsPalette\>"]; 
            Return[]]; \[IndentingNewLine]If[\[Not] captured, 
            Return[]]; \[IndentingNewLine]selection = 
            ToExpression[
                NotebookRead[evalnb] /. 
                  FormBox[expr_, _] \[RuleDelayed] expr] /. 
              StyleForm[expr_, 
                  Background \[Rule] 
                    RGBColor[0.691417, \ 0.98439, \ 0.992203]] \[Rule] 
                expr; \[IndentingNewLine]If[selection === {}, 
            Return[]]; \[IndentingNewLine]positions = 
            Position[expression0, selection]; \[IndentingNewLine]npositions = 
            Length[positions]; \[IndentingNewLine]positionmesg = 
            If[npositions == 1, "\<Single Position\>", 
              StringForm["\< `` Positions\>", 
                npositions]]; \[IndentingNewLine]dispexpr = 
            HoldForm[a] /. a \[Rule] expression0; \[IndentingNewLine]If[
            npositions > 0 \[And] 
              positions\[LeftDoubleBracket]1\[RightDoubleBracket] =!= {}, \
\[IndentingNewLine]expression = 
              NewReplacePart[dispexpr, \ 
                StyleForm[selection, 
                  Background \[Rule] 
                    RGBColor[0.691417, \ 0.98439, \ 0.992203]], {1, \ 
                    positions\[LeftDoubleBracket]1\[RightDoubleBracket]} // 
                  Flatten]; \[IndentingNewLine]SelectionMove[evalnb, All, 
              CellContents]; \[IndentingNewLine]NotebookWrite[evalnb, \ 
              ToBoxes[expression]]; \[IndentingNewLine]positionfound = 
              True; \[IndentingNewLine]Print[{positions\[LeftDoubleBracket]1\
\[RightDoubleBracket], positionmesg}], 
            Print["\<Selection does not have a position.\>"]]\
\[IndentingNewLine]]; \[IndentingNewLine]\[IndentingNewLine]nextfunction \
:= \[IndentingNewLine]Module[{newselection, dispexpr, a, 
            celltest}, \[IndentingNewLine]If[
            SelectedNotebook[] === ButtonNotebook[], 
            Print["\<Unselect the PositionsPalette\>"]; 
            Return[]]; \[IndentingNewLine]SelectionMove[evalnb, All, 
            Cell]; \[IndentingNewLine]celltest = 
            NotebookRead[evalnb]; \[IndentingNewLine]If[celltest === {}, 
            Return[]]; \[IndentingNewLine]If[\[Not] positionfound, 
            SelectionMove[evalnb, After, CellContents]; 
            Print["\<Find a position first.\>"]; 
            Return[]]; \[IndentingNewLine]positions = 
            RotateLeft[positions, \ 1]; \[IndentingNewLine]expression = 
            expression /. 
              StyleForm[expr_, 
                  Background \[Rule] 
                    RGBColor[0.691417, \ 0.98439, \ 0.992203]] \[RuleDelayed] 
                expr; \[IndentingNewLine]newselection = 
            expression0[\([Sequence @@ 
                  positions\_\(\(\[LeftDoubleBracket]\)\(1\)\(\
\[RightDoubleBracket]\)\)]\)]; \[IndentingNewLine]dispexpr = 
            HoldForm[a] /. 
              a \[Rule] expression0; \[IndentingNewLine]expression = 
            NewReplacePart[dispexpr, \ 
              StyleForm[newselection, 
                Background \[Rule] 
                  RGBColor[0.691417, \ 0.98439, \ 0.992203]], {1, \ 
                  positions\[LeftDoubleBracket]1\[RightDoubleBracket]} // 
                Flatten]; \[IndentingNewLine]SelectionMove[evalnb, \ All, 
            CellContents]; \[IndentingNewLine]NotebookWrite[evalnb, \ 
            ToBoxes[expression]]; \[IndentingNewLine]Print[{positions\
\[LeftDoubleBracket]1\[RightDoubleBracket], 
              positionmesg}]]; \
\[IndentingNewLine]\[IndentingNewLine]restorefunction \
:= \[IndentingNewLine]Module[{}, \[IndentingNewLine]If[
            SelectedNotebook[] === ButtonNotebook[], 
            Print["\<Unselect the PositionsPalette\>"]; 
            Return[]]; \[IndentingNewLine]If[\[Not] captured, 
            Return[]]; \[IndentingNewLine]SelectionMove[evalnb, All, 
            Cell]; \[IndentingNewLine]celltest = 
            NotebookRead[evalnb]; \[IndentingNewLine]If[celltest === {}, 
            Return[]]; \[IndentingNewLine]SelectionMove[evalnb, All, 
            CellContents]; \[IndentingNewLine]NotebookWrite[evalnb, 
            ToBoxes[expression0]]]; \
\[IndentingNewLine]\[IndentingNewLine]makeboxesdefinitions := \((MakeBoxes[
              Times[a_StyleForm, b_Plus], form_] := 
            RowBox[{RowBox[{"\<(\>", MakeBoxes[b, form], "\<)\>"}], 
                RowBox[{"\<(\>", 
                    MakeBoxes[a, 
                      form], "\<)\>"}]}]; \[IndentingNewLine]\
\[IndentingNewLine]MakeBoxes[Times[a_StyleForm, b_], form_] := 
            RowBox[{MakeBoxes[b, form], 
                RowBox[{"\<(\>", 
                    MakeBoxes[a, 
                      form], "\<)\>"}]}]; \[IndentingNewLine]\
\[IndentingNewLine]MakeBoxes[Times[a_Rational, b_Plus], 
              form_] := \[IndentingNewLine]RowBox[{MakeBoxes[a, form], 
                RowBox[{"\<(\>", 
                    MakeBoxes[b, 
                      form], "\<)\>"}]}]; \[IndentingNewLine]\
\[IndentingNewLine]MakeBoxes[Times[a_Rational, b_], 
              form_] := \[IndentingNewLine]RowBox[{MakeBoxes[a, 
                  form], "\< \>", 
                MakeBoxes[b, 
                  form]}]; \[IndentingNewLine]\[IndentingNewLine]MakeBoxes[
              Times[a_Rational, b__], 
              form_] := \[IndentingNewLine]RowBox[{MakeBoxes[a, form], 
                MakeBoxes[Times[b], 
                  form]}]; \[IndentingNewLine]\[IndentingNewLine]MakeBoxes[
              c_Plus\ Power[a_, b_?Negative], 
              form_] := \[IndentingNewLine]RowBox[{SuperscriptBox[
                  MakeBoxes[a, form], ToBoxes[b, form]], 
                RowBox[{"\<(\>", 
                    ToBoxes[c, 
                      form], "\<)\>"}]}]; \[IndentingNewLine]\
\[IndentingNewLine]MakeBoxes[c_\ Power[a_, b_?Negative], 
              form_] := \[IndentingNewLine]RowBox[{SuperscriptBox[
                  MakeBoxes[a, form], ToBoxes[b, form]], 
                ToBoxes[c, 
                  form]}]; \[IndentingNewLine]\[IndentingNewLine]MakeBoxes[
              Power[a_, b_?Negative], 
              form_] := \[IndentingNewLine]SuperscriptBox[MakeBoxes[a, form], 
              ToBoxes[b, 
                form]]; \[IndentingNewLine]\[IndentingNewLine]MakeBoxes[
              Power[a_, Rational[1, 2]], form_] := 
            SuperscriptBox[MakeBoxes[a, form], 
              ToBoxes[Rational[1, 
                  2]]])\); \
\[IndentingNewLine]\[IndentingNewLine]clearboxesdefinitions := \((Off[
            Unset::norep]; \[IndentingNewLine]MakeBoxes[
              Times[a_StyleForm, b_Plus], 
              form_] =. ; \[IndentingNewLine]MakeBoxes[
              Times[a_StyleForm, b_], 
              form_] =. ; \[IndentingNewLine]MakeBoxes[
              Times[a_Rational, b_Plus], 
              form_] =. ; \[IndentingNewLine]MakeBoxes[Times[a_Rational, b_], 
              form_] =. ; \[IndentingNewLine]MakeBoxes[
              Times[a_Rational, b__], 
              form_] =. ; \[IndentingNewLine]MakeBoxes[
              c_\ Power[a_, b_?Negative], form_] =. ; 
          MakeBoxes[c_Plus\ Power[a_, b_?Negative], 
              form_] =. ; \[IndentingNewLine]MakeBoxes[\ 
              Power[a_, b_?Negative], 
              form_] =. ; \[IndentingNewLine]MakeBoxes[
              Power[a_, Rational[1, 2]], form_] =. ; \[IndentingNewLine]On[
            Unset::norep])\); \[IndentingNewLine]\[IndentingNewLine]evalnb = 
        EvaluationNotebook[]; \[IndentingNewLine]palettenb = 
        NotebookPut[
          Notebook[{\ \[IndentingNewLine]Cell[
                BoxData[
                  StyleBox[
                    GridBox[{\[IndentingNewLine]{ButtonBox["\<Capture[\
\[SelectionPlaceholder]]\>", 
                            ButtonFunction \[RuleDelayed] \((capturefunction)\
\), ButtonEvaluator \[Rule] Automatic, Active \[Rule] True, 
                            ButtonExpandable \[Rule] True, 
                            ButtonStyle \[Rule] None, 
                            ButtonSource \[Rule] CellContents, 
                            Background \[Rule] 
                              pcolor]}, \
\[IndentingNewLine]\[IndentingNewLine]{ButtonBox["\<Positions[\
\[SelectionPlaceholder]]\>", 
                            ButtonFunction \[RuleDelayed] \
\((positionfunction)\), ButtonEvaluator \[Rule] Automatic, 
                            Active \[Rule] True, 
                            ButtonExpandable \[Rule] True, 
                            ButtonStyle \[Rule] None, 
                            ButtonSource \[Rule] CellContents, 
                            Background \[Rule] 
                              pcolor]}, \
\[IndentingNewLine]\[IndentingNewLine]{ButtonBox["\<NextPosition\>", 
                            ButtonFunction \[RuleDelayed] \((nextfunction)\), 
                            ButtonEvaluator \[Rule] Automatic, 
                            Active \[Rule] True, 
                            ButtonExpandable \[Rule] True, 
                            ButtonStyle \[Rule] None, 
                            ButtonSource \[Rule] CellContents, 
                            Background \[Rule] 
                              pcolor]}, \
\[IndentingNewLine]\[IndentingNewLine]{ButtonBox["\<Restore\>", 
                            ButtonFunction \[RuleDelayed] \((restorefunction)\
\), ButtonEvaluator \[Rule] Automatic, Active \[Rule] True, 
                            ButtonExpandable \[Rule] True, 
                            ButtonStyle \[Rule] None, 
                            ButtonSource \[Rule] CellContents, 
                            Background \[Rule] 
                              pcolor]}, \
\[IndentingNewLine]\[IndentingNewLine]{GridBox[{{ToBoxes["\< Explicit\n \
Format\>"], \
\[IndentingNewLine]GridBox[{\[IndentingNewLine]{ButtonBox["\<On\>", 
                                        ButtonFunction \[RuleDelayed] \
\((Module[{}, makeboxesdefinitions] &)\), ButtonEvaluator \[Rule] Automatic, 
                                        Active \[Rule] True, 
                                        ButtonExpandable \[Rule] True, 
                                        ButtonStyle \[Rule] None, 
                                        ButtonSource \[Rule] CellContents, 
                                        Background \[Rule] 
                                        pcolor]}, \
\[IndentingNewLine]{ButtonBox["\<Off\>", 
                                        ButtonFunction \[RuleDelayed] \
\((Module[{}, clearboxesdefinitions] &)\), ButtonEvaluator \[Rule] Automatic, 
                                        Active \[Rule] True, 
                                        ButtonExpandable \[Rule] True, 
                                        ButtonStyle \[Rule] None, 
                                        ButtonSource \[Rule] CellContents, 
                                        Background \[Rule] pcolor]}}, 
                                  RowSpacings \[Rule] 0]}}, 
                            RowAlignments \[Rule] Center]}}, 
                      RowSpacings \[Rule] 
                        0], \[IndentingNewLine]Background \[Rule] 
                      RGBColor[0.832044, \ 0.941421, \ 0.988296], 
                    FontWeight \[Rule] "\<Bold\>", 
                    FontFamily \[Rule] "\<Helvetica\>", 
                    FontColor \[Rule] 
                      RGBColor[0.210941, \ 0.460945, \ 
                        0.218753]]], "\<Output\>", 
                ShowCellBracket \[Rule] False, 
                CellMargins \[Rule] {{\(-2\), 0}, {0, 0}}]}, 
            WindowSize \[Rule] {80, 107}, 
            WindowMargins \[Rule] {{Automatic, 5}, {145, Automatic}}, 
            WindowFrame \[Rule] "\<Palette\>", WindowTitle \[Rule] "\< \>", 
            WindowFloating \[Rule] False, WindowElements \[Rule] {}, 
            WindowFrameElements \[Rule] {"\<CloseBox\>"}, 
            WindowClickSelect \[Rule] 
              False]]; \[IndentingNewLine]SetSelectedNotebook[
        evalnb]; \[IndentingNewLine]captured = False; 
      positionfound = False;\[IndentingNewLine]]\)
*)



NewReplacePart[expr_,new_,posn_?(MemberQ[#,_eP,{1}]&)]:=
  Module[{PosnList,NewParts,temp=Unique[],Int},
    PosnList={IndividualParts/@posn//.{elems__List}\[RuleDelayed]
            Sequence[elems]};
    PosnList=Join[{1},#]& /@PosnList;
    NewParts=Flatten[ReplacementList[#,new,temp]&  /@posn];
    Int=List/@Range[Length@PosnList];
    NewParts=NewReplacePart[Hold@@{expr},NewParts,PosnList,Int];
    First@DeleteCases[NewParts,temp,\[Infinity]]
    ]

NewReplacePart[expr_,new_,posn_eP]:=NewReplacePart[expr,new,{posn}]

(* JMM 2007: Fall back *)
NewReplacePart[expr_,new_,n_]:=ReplacePart[expr,new,n];
NewReplacePart[expr_,new_,n_,m_]:=ReplacePart[expr,new,n,m];



\!\(RootsTogether[
      expr_] := \[IndentingNewLine]Module[{numbertest, radicalexpand, 
        togetherrules}, \[IndentingNewLine]numbertest = \((FreeQ[#, 
                Complex] \[And] \ \ \[Not] NumericQ[#] &)\); \
\[IndentingNewLine]radicalexpand = \ 
        Power[Power[a_, p_Integer], q_Rational] \[RuleDelayed] 
          Power[a, 
            p\ q]; \[IndentingNewLine]togetherrules = {\[IndentingNewLine]n_. \
\ Power[a_?numbertest, q_Rational] 
              Power[b_?numbertest, q_Rational] \[RuleDelayed] 
            n\ Power[a\ b, 
                q], \[IndentingNewLine]\[IndentingNewLine]n_. \ Power[
                  a_?numbertest, p_Rational]\ Power[b_?numbertest, 
                  q_Rational] /; \((p + q == 0\  \[And] \ 
                  p > q)\) \[RuleDelayed] \[IndentingNewLine]n\ Power[a/b, 
                p], \[IndentingNewLine]\[IndentingNewLine]n_. \ Power[
                a_?numbertest, Rational[na_, r_]]\ Power[b_?numbertest, 
                Rational[nb_, 
                  r_]] \[RuleDelayed] \[IndentingNewLine]n\ Power[\(a\^na\) 
                  b\^nb, Rational[1, r]]}; \[IndentingNewLine]\((expr //. 
            radicalexpand)\) //. togetherrules]\)



(* JMM 2007: Commented out this line *)
(* Protect[Evaluate[protected]]; *)

End[];

Protect[Evaluate[$Context<>"*"]];

EndPackage[]

(* PositionsPalette; *)
