'From Cuis 5.0 [latest update: #4194] on 29 May 2020 at 4:32:09 pm'!

!Workspace methodsFor: 'testing' stamp: 'jmv 5/29/2020 16:30:47'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	This is useful in Workspaces, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^true! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 5/29/2020 16:31:26'!
formatAndStyleIfNeededWith: anSHTextStyler
	| separator fragmentStart fragmentEnd done|
	(anSHTextStyler notNil and: [self shouldStyle: self actualContents with: anSHTextStyler])
		ifFalse: [^ self].

	self styleByParagraphs ifFalse: [
		^super formatAndStyleIfNeededWith: anSHTextStyler ].

	actualContents _ actualContents optimizedForMutationSpeed.
	anSHTextStyler formatWorkspace: actualContents.
	separator _ String newLineString, String newLineString.
	fragmentStart _ 1.
	done _ false.
	[done] whileFalse: [
		fragmentEnd _ actualContents
			indexOfSubCollection: separator
			startingAt: fragmentStart
			ifAbsent: [done _ true. actualContents size].
		anSHTextStyler styleWorkspaceFrom: fragmentStart to: fragmentEnd.
		fragmentStart _ fragmentEnd+separator size ].
	self changed: #shoutStyled! !

