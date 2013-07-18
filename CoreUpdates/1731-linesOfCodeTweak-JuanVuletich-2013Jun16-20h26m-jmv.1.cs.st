'From Cuis 4.1 of 12 December 2012 [latest update: #1728] on 16 June 2013 at 8:32:12.520346 pm'!

!ClassDescription methodsFor: 'private' stamp: 'jmv 6/16/2013 20:31'!
linesOfCode

"
Object linesOfCode 
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."

	| lines |
	lines _ 0.
	self selectorsDo: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) linesOfCode ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class linesOfCode]
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) detectSum: [:c |
	(SystemOrganization superclassOrderIn: c) detectSum: [:cl | cl linesOfCode]]
 23232
"! !

