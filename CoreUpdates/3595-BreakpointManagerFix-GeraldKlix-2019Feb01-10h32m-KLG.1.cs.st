'From Cuis 5.0 of 7 November 2016 [latest update: #3593] on 1 February 2019 at 10:33:43 am'!

!BreakpointManager class methodsFor: 'private' stamp: 'KLG 1/31/2019 17:21:07'!
compilePrototype: aSymbol in: aClass 
	"Compile and return a new method containing a break statement"

	| source node trailer |
	trailer _ (aClass compiledMethodAt: aSymbol) trailer.
	source := self breakpointMethodSourceFor: aSymbol in: aClass.
	node := aClass compilerClass new
		compile: source
		in: aClass 
		notifying: nil 
		ifFail: [self error: '[breakpoint] unable to install breakpoint'].
	^node ifNotNil: [ node generate: trailer ]! !

