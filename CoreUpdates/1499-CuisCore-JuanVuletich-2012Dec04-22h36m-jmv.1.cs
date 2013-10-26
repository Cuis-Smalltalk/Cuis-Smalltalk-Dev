'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 10:42:08 pm'!

!Decompiler methodsFor: 'private' stamp: 'eem 11/9/2012 14:17'!
convertToDoLoop
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| initStmt toDoStmt limitStmt |
	(stack notEmpty
	 and: [stack last isAssignmentNode])
		ifTrue:
			[initStmt := stack last.
			 (toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			 stack removeLast; addLast: toDoStmt.
			 statements removeLast]
		ifFalse:
			[statements size < 2 ifTrue:
				[^self].
			initStmt := statements at: statements size-1.
			(toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			statements removeLast; removeLast; addLast: toDoStmt].
	initStmt variable scope: -1.  "Flag arg as block temp"

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...].
	The complication here is that limitVar := limitExpr's value may be used, in which case it'll
	be statements last, or may not be used, in which case it'll be statements nextToLast."
	statements size < 2 ifTrue: [^ self].
	limitStmt := statements last.
	((limitStmt isMemberOf: AssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
			[limitStmt := statements at: statements size-1.
			((limitStmt isMemberOf: AssignmentNode)
				and: [limitStmt variable isTemp
				and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
					[^self]].

	(self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset) ifFalse:
		[^self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements last == limitStmt
		ifTrue: [statements removeLast]
		ifFalse: [statements removeLast; removeLast; addLast: toDoStmt]! !

