'From Cuis 5.0 of 7 November 2016 [latest update: #3204] on 1 November 2017 at 3:31:08 pm'!

!Character methodsFor: 'accessing' stamp: 'jmv 11/1/2017 15:30:19'!
nonImmediateNumericValue
	"Answer the numeric value of the receiver, if instances happen to be regular (i.e. not in Spur)"

"	^self instVarAt: 1"
"There is currently (Nov 2017) a bug in the InterpreterVM that makes #instVarAt: always faile. For instance:
3@4 instVarAt: 1
Code below is an UndeclaredVariableReference in Spur, but works ok in V3 VMs (InterpreterVM, StackVM, CogVM)
"
	^value! !

