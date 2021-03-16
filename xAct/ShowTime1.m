(* :Title: ShowTime *)

(* :Author: Roman E. Maeder *)

(* :Summary:
This package provides a utility to print timing information for each
evaluation.
*)

(* :Context: Utilities`ShowTime` *)

(* :Copyright: Copyright 1991-2003, Wolfram Research, Inc.*)

(* :Source: Roman E. Maeder: Programming in Mathematica,
        Third Edition, Addison-Wesley, 1996.
*)

(* :History: 
Original version by Roman Maeder, 1991
Changes to permit showing the timing of Sequence, Mark E. Fisher, Sept. 1993
V2.0 -- Update for Version 3.0, R. Maeder, 1995
V2.1 by Robby Villegas, October 1998 -- no change in functionality, but
   revises code to eliminate problems with bad values being assigned to
   Out[] when Null is the correct value.
V2.2 by JMM, September 2005. Added variable $ShowTimeThreshold with
     default 1 Second.
*)

(* :Package Version: 2.2 *)

(* :Mathematica Version: 3.0 *)

BeginPackage["Utilities`ShowTime`"]

Unprotect[ShowTime]

ShowTime::usage = "On[ShowTime] turns timing information on. Off[ShowTime]
	turns it off again.  The time taken for each command is printed
	before the result (if any)."
$ShowTimeThreshold::usage = "$ShowTimeThreshold is a global variable
        setting a minimum time below which ShowTime does not print
        timings."

ShowTime::twice = "ShowTime is already on."
ShowTime::off = "ShowTime is not in effect."

Begin["`Private`"]

`oldPre          (* saved user's value of $Pre *)
ison = False     (* whether ShowTime is currently turned on *)

SetAttributes[ ShowTime, {HoldAll, SequenceHold} ]

setOldPre := If[ HasValue[$Pre], oldPre = $Pre, Clear[oldPre] ]
setPre := If[ HasValue[oldPre], $Pre = oldPre, Clear[$Pre] ]


(*
villegas, 28 Sep 1998:

When In[n] evaluates to Null, Out[n] is not assigned Null, but instead is
assigned the last non-Null value, called 'LastValue', returned by any
statement in any CompoundExpression executed during the evaluation of
In[n].  Of course, if there were no CompoundExpression's or if every
statement in every CompoundExpression returned Null, then Out[n] will be
assigned Null, but that's the only way Out[n] will be Null.

This means that any processor of the input or output, such as $Pre =
ShowTime, can inadvertently change what gets stored in Out[n], *even
though it tries to be a transparent processor*!  This will make % and %n
give wrong and unexpected results in the user's session. Most commonly,
this will happen to inputs that suppress output with a terminating
semicolon:

  In[n] := a; b; c;

The processor can avoid the unintended side-effect of overwriting
LastValue if it can ensure that every CompoundExpression executed in the
course of processing produces only Null results from its statements.  This
is possible, with a certain amount of inconvenience, if the processor
calls only top-level code that it has control over, i.e. only the code in
the implementation of the processor itself, which its author is free to
alter.  Actually, it might be possible for the processor to ensure the
Null results for arbitrary code that it executes, using a top-level trap
on CompoundExpression that is put into effect except during the evaluation
of the user's input, but I'm not certain of that.

I chose to do this by wrapping all statements in all CompoundExpression's
in this code with a dummy function DiscardResult[expr] which does nothing
but evaluate expr and return Null.  And making sure I don't call any
top-level code that ShowTime.m doesn't control; hence the substitution of
HasValue for ValueQ.
*)

DiscardResult[_] = Null


Attributes[HasValue] = HoldFirst

HasValue[s_Symbol] := UnsameQ[OwnValues[s], {}]

$ShowTimeThreshold = 1;

ShowTime[ expr_ ] :=
	Module[{timing, result},
		Block[{$Pre},
			DiscardResult @ If[ HasValue[oldPre],
			    DiscardResult[ $Pre = oldPre ];
			    DiscardResult[ timing = AbsoluteTiming[$Pre[expr]] ]
			  , (* else *)
			    timing = AbsoluteTiming[expr]
			];
			DiscardResult[ If[(timing[[1]]/.Second->1)>$ShowTimeThreshold,Print[ timing[[1]] ] ]];
			DiscardResult[ setOldPre ];
			DiscardResult[ If[!HasValue[$Pre], $Pre = Identity] ]; (* this is subtle *)  (* <=== Thanks for the informative comment *)
			result = If[ Length[timing] == 2, timing[[2]],
		                     Sequence @@ Drop[timing, 1] ]; (* restore sequences *)
		];
		DiscardResult[ If[!ison, setPre] ];         (* turn it off, restore $Pre *)
		result
	]

ShowTime/: On[ShowTime] := (
	If[ ison, Message[ShowTime::twice],
	    setOldPre; $Pre = ShowTime; ison = True ]; )

ShowTime/: Off[ShowTime] := (
	If[ ison, ison = False,
	    Message[ShowTime::off] ]; )

End[]
Protect[ ShowTime ]
EndPackage[]

On[ShowTime]
