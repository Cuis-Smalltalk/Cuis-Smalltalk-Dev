'From Cuis 4.0 of 21 April 2012 [latest update: #1304] on 12 June 2012 at 4:58:18 pm'!

!SHParserST80 methodsFor: 'accessing' stamp: 'jmv 6/12/2012 16:53'!
last3Ranges
	| r s lastI |
	s _ ranges size.
	r _ ranges last.
	lastI _ r type = #excessCode
		ifTrue: [ s - 1 ]
		ifFalse: [ s].
	^{ 
		(lastI > 2 ifTrue: [ranges at: lastI-2]).
		(lastI > 1 ifTrue: [ranges at: lastI-1]).
		(ranges at: lastI)
	}! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 6/12/2012 16:55'!
computeEntries

	| allSource contextClass id p last3Ranges range prevRange receiverClass prevPrevRange |
	allSource _ model actualContents string.
	p _ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ].
	contextClass _ (p respondsTo: #selectedClassOrMetaClass) ifTrue: [
		p selectedClassOrMetaClass ].

	"Instead of creating a new string, maybe we could pass the last position to parse to Shout..."
	parser _ SHParserST80 new.
	parser
		workspace: ((model isMemberOf: Workspace) ifTrue: [ model ]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position).
	parser parse.
	last3Ranges _ parser last3Ranges.
	range _ last3Ranges third.
	range ifNil: [ ^entries _ #() ].

	"If parsing breaks before position, then we don't know what to suggest, therefore don't open Completion"
	range end = position ifFalse: [ ^entries _ #() ].

	prefix _ allSource copyFrom: range start to: range end.
	
	(parser isMessage: range type) ifTrue: [
		"If previous range is a constant or a well known identifier, we might filter messages"
		prevRange _ last3Ranges second.
		prevPrevRange _ last3Ranges first.
		receiverClass _ nil.
		"3 if -> ifNil: but not ifTrue:
		3=4 -> ifNil: or ifTrue:"
		(prevRange notNil and: [ prevPrevRange isNil or: [ (#(binary keyword) includes: prevPrevRange type) not]]) ifTrue: [
			id _ (allSource copyFrom: prevRange start to: prevRange end).
			receiverClass _ prevRange type caseOf: {
				[ #globalVar ] -> [ (Smalltalk at: id asSymbol) class ].
				[ #self ] -> [ contextClass ].
				[ #super ] -> [ contextClass superclass ].
				[ #true ] -> [ True ].
				[ #false ] -> [ False ].
				[ #nil ] -> [ UndefinedObject ].
				[ #character ] -> [ id first class ].
				[ #number ] -> [ (Compiler evaluate: id) class ].
				[ #string ] -> [ (Compiler evaluate: id) class ].
				[ #symbol ] -> [ (Compiler evaluate: id) class ].
				[ #stringSymbol ] -> [ (Compiler evaluate: id) class ].
				"thisContext could mean ContextPart or BlockClosure..."
				"[ #thisContext ] -> [ ContextPart ]"
			} otherwise: [ nil ]
		].
		^self computeMessageEntries: receiverClass ].

	(parser isPartialOrFullIdentifier: range type) ifTrue: [
		^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #()! !

!methodRemoval: SHParserST80 #penultimateRange!
SHParserST80 removeSelector: #penultimateRange!
