'From Cuis 5.0 of 7 November 2016 [latest update: #3688] on 18 April 2019 at 5:55:07 pm'!

!MessageNode methodsFor: 'testing' stamp: 'HAW 4/18/2019 17:52:07'!
isCascade

	^receiver isNil ! !


!MethodNode methodsFor: 'testing' stamp: 'HAW 3/29/2019 23:50:04'!
isMultipleRanges: aRangeOrRanges

	^aRangeOrRanges isKindOf: OrderedCollection ! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/29/2019 23:42:52'!
inspectSelectionOrLine

	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ morph flash ]
		profiled: false! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/29/2019 23:58:09'!
selectForInspection: aNodeUnderCursor in: aMethodNode 
	
	(aNodeUnderCursor isLiteralNode or: [ aNodeUnderCursor isVariableNode ]) ifTrue: [ ^self selectNodeRange: aNodeUnderCursor in: aMethodNode ].
	aNodeUnderCursor isMessageNode ifTrue: [ ^self selectMessageNode: aNodeUnderCursor in: aMethodNode ].! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/30/2019 00:29:14'!
selectMessageNode: aMessageNodeUnderCursor in: aMethodNode
	
	| messageRange |
	
	self 
		withReceiverRangeOf: aMessageNodeUnderCursor 
		in: aMethodNode 
		selectorPosition: self startIndex 
		do: [ :receiverRange |
			messageRange := aMethodNode sourceRangeFor: aMessageNodeUnderCursor. 
			self selectFrom: receiverRange first to: messageRange last ]
		
	! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/29/2019 23:57:59'!
selectNodeRange: aNodeUnderCursor in: aMethodNode 
	
	| range ranges |
	
	ranges := aMethodNode sourceRangeFor: aNodeUnderCursor.
	range := (aMethodNode isMultipleRanges: ranges) 
		ifTrue: [ ranges detect: [ :aRange | aRange includes: self startIndex ] ifNone: [ ^self ]]
		ifFalse: [ ranges ].
		
	self selectFrom: range first to: range last
! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/29/2019 23:57:08'!
selectNodeUnderCursorForInspectionIn: aMethodNode 
	
	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self selectForInspection: nodeUnderCursor in: aMethodNode ]
		ifAbsent: []


! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 4/18/2019 17:54:45'!
withReceiverRangeOf: aMessageNode in: aMethodNode selectorPosition: aSelectorPosition do: aBlock   
	
	| receiverRange receiverRangeOrRanges messageNodeReceiver |
	
	"If aMessageNode receiver isNil it means that it is a cascade receiver so this imposes the question on how to inspect
	a cascade message send. We could inspect the result of sending all the messages up to the cursor but the problem is
	that when looking for the cascade receiver range it does not find it because it is a different node that the used in the source
	ranges... we could do the trick of looking for printString in the sourceRanges keys, but that is too much - Hernan"
	aMessageNode isCascade ifFalse: [ 
		messageNodeReceiver := aMessageNode receiver.
		messageNodeReceiver isMessageNode ifTrue: [ 
			^self withReceiverRangeOf: messageNodeReceiver in: aMethodNode selectorPosition: (messageNodeReceiver keywordPositionAt: 1) first do: aBlock ].
		
		receiverRangeOrRanges := aMethodNode sourceRangeFor: messageNodeReceiver. 
		
		receiverRange := (aMethodNode isMultipleRanges: receiverRangeOrRanges)
			ifTrue: [ | closestRange |
				closestRange := receiverRangeOrRanges first.
				receiverRangeOrRanges do: [ :aRange |  (aRange last < aSelectorPosition and: [ aRange last > closestRange last ]) ifTrue: [ closestRange := aRange ]].
				closestRange ]
			ifFalse: [ receiverRangeOrRanges ].

		aBlock value: receiverRange ]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 3/30/2019 00:45:22'!
inspectIt
	
	self hasSelection ifFalse: [
		self
			withMethodNodeAndClassDo: [ :methodNode :selectedClass | self selectNodeUnderCursorForInspectionIn: methodNode ]
			ifErrorsParsing: [ :anError | ]].
			
	self inspectSelectionOrLine

	! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 4/18/2019 17:34:21'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	"I have to do this because some codeProviders do no answer selectedClassOrMetaClass like the Workspace - Hernan"
	selectedClass := [ self codeProvider selectedClassOrMetaClass ] on: Error do: [ :anError | anError return: UndefinedObject ].
	[ methodNode := selectedClass methodNodeFor: model actualContents ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	aBlock value: methodNode value: selectedClass.! !


!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'HAW 3/29/2019 23:50:31'!
abstractSourceMap
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."

	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |

	abstractSourceRanges ifNotNil: [ ^abstractSourceRanges].

	methodNode encoder hasGeneratedMethod
		ifTrue: [
			rawSourceRanges := methodNode encoder rawSourceRanges.
		 	theMethodToScan := self method ]
		ifFalse: [
			"If the methodNode hasn't had a method generated it doesn't have pcs set in its
			 nodes so we must generate a new method and might as well use it for scanning."
			 [methodNode rawSourceRangesAndMethodDo: [ :ranges :method |
				rawSourceRanges := ranges.
			 	theMethodToScan := method ]]
			on: UndeclaredVariableWarning
			do: [ :ex | ex resume ]].

	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo: [ :node :range |
		node pc ~= 0 ifTrue: [ | realRange |
			realRange := (methodNode isMultipleRanges: range) ifTrue: [ range last ] ifFalse: [ range ].
			concreteSourceRanges at: node pc put: realRange ]].

	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := InstructionClient new.
	[
		(concreteSourceRanges includesKey: scanner pc) ifTrue: [
			abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
		 abstractPC := abstractPC + 1.
		 scanner interpretNextInstructionFor: client.
		 scanner atEnd ] whileFalse.
	
	^abstractSourceRanges! !


!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: selectionDoItSourceCodeIn: withReceiverRangeOf:in:selectorPosition:do:)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifRenameCanBeAppliedDo: rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
!

