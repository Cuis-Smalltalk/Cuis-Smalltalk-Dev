'From Cuis 5.0 of 7 November 2016 [latest update: #3761] on 23 May 2019 at 8:48:15 am'!

!Parser methodsFor: 'expression types' stamp: 'HAW 5/23/2019 08:42:06'!
braceExpression
	" { elements } => BraceNode."

	| elements locations loc more sourceRangeStart sourceRangeEnd |

	sourceRangeStart _ hereMark.
	elements := OrderedCollection new.
	locations := OrderedCollection new.
	self advance.
	more := hereType ~~ #rightBrace.
	[more]
		whileTrue: 
			[loc := hereMark + requestorOffset.
			self expression
				ifTrue: 
					[elements addLast: parseNode.
					locations addLast: loc]
				ifFalse:
					[^self expected: 'Variable or expression or right brace'].
			(self match: #period)
				ifTrue: [more := hereType ~~ #rightBrace]
				ifFalse: [more := false]].
	parseNode := BraceNode new elements: elements sourceLocations: locations.
	sourceRangeEnd _ hereEnd.

	(self match: #rightBrace) ifFalse: [^self expected: 'Period or right brace'].
	encoder noteSourceRange: (sourceRangeStart to: sourceRangeEnd) forNode: parseNode.

	^true! !

