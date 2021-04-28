'From Cuis 5.0 [latest update: #4571] on 26 April 2021 at 7:33:07 pm'!
!SmalltalkCompleter methodsFor: 'accessing' stamp: 'jmv 4/26/2021 19:28:15'!
allSource
	^model actualContents string! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 4/26/2021 19:31:49'!
computeEntries

	| allSource source contextClass specificModel range
	separator fragmentStart fragmentEnd done |

	allSource _ self allSource.
	separator _ String newLineString, String newLineString.
	fragmentStart _ 1.
	done _ false.
	[done] whileFalse: [
		fragmentEnd _ allSource
			indexOfSubCollection: separator
			startingAt: fragmentStart
			ifAbsent: [allSource size].
		fragmentEnd >= position ifTrue: [
			source _ allSource copyFrom: fragmentStart to: position.
			done _ true ].
		fragmentStart _ fragmentEnd+separator size ].

	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel. 
	allRanges _ self parse: source in: contextClass and: specificModel.
	range _ allRanges lastIfEmpty: [ ^entries _ #() ].
	possibleInvalidSelectors _ #().

	range end = source size
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: source at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: source at: range in: contextClass and: specificModel ].

	! !

!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'jmv 4/26/2021 19:32:47'!
parse: source in: contextClass and: specificModel
	
	| isMethod |
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: source.

	isMethod := (specificModel is: #Browser)
		ifTrue: [ specificModel isEditingClass not ]
		ifFalse: [ specificModel is: #CodeProvider ].
	parser parse: isMethod.

	^ parser rangesWithoutExcessCode.! !

!methodRemoval: SHParserST80 #allSource stamp: 'jmv 4/26/2021 19:29:22'!
SHParserST80 removeSelector: #allSource!
!methodRemoval: SHParserST80 #allSource: stamp: 'jmv 4/26/2021 19:29:24'!
SHParserST80 removeSelector: #allSource:!
!methodRemoval: SmalltalkCompleter #changePositionTo: stamp: 'jmv 4/26/2021 19:30:42'!
SmalltalkCompleter removeSelector: #changePositionTo:!
!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges blockDepths blockDepthsStartIndexes braceDepth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
