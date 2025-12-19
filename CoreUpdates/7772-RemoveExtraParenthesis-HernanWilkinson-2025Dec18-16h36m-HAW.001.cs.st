'From Cuis7.5 [latest update: #7771] on 18 December 2025 at 4:37:21 pm'!

!CompiledMethod methodsFor: 'time stamp' stamp: 'HAW 18/Dec/2025 16:36:57'!
authorAndStampAsSaved
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	| preamble tokens |
	preamble := self getPreamble.
	(preamble includesSubString: 'methodsFor:') ifFalse: [ ^''].
	tokens := Scanner new scanTokens: preamble.
	^CompiledMethod stampFrom: tokens! !

