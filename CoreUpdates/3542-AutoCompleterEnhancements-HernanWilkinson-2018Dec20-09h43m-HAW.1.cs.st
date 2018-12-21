'From Cuis 5.0 of 7 November 2016 [latest update: #3526] on 20 December 2018 at 3:12:35 pm'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser '
	classVariableNames: 'AccessLock Selectors EntriesLimit '
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!Object methodsFor: 'private' stamp: 'HAW 12/20/2018 14:18:53'!
errorDescriptionForSubcriptBounds: index

	^'subscript is out of bounds: ' , index printString! !


!SequenceableCollection methodsFor: 'assertions' stamp: 'HAW 12/20/2018 14:40:40'!
assertIsInBounds: anIndex

	(self isInBounds: anIndex) ifFalse: [ self errorSubscriptBounds: anIndex ]
	! !


!ArrayedCollection methodsFor: 'inserting' stamp: 'HAW 12/20/2018 14:54:55'!
insert: anObject shiftingRightAt: anInsertionIndex

	"Inserts anObject at anInsertionIndex, moving right object between anInsertionIndex and self size, 
	loosing last object. Example: 
	#(0 1 3 4 5) insert: 2 shiftingRightAt: 3
	returns:  #(0 1 2 3 4) 
	" 
	| currentIndex |
	
	self assertIsInBounds: anInsertionIndex.
	currentIndex _ self size .
	
	[currentIndex > anInsertionIndex] whileTrue: [
		self at: currentIndex put: (self at: currentIndex-1).
		currentIndex _ currentIndex - 1].
	
	self at: anInsertionIndex put: anObject
! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 14:43:28'!
add: selector and: lastUsedTime to: selectorsToShow

	^ selectorsToShow size < EntriesLimit
		ifTrue: [ self add: selector and: lastUsedTime whenNotFullTo: selectorsToShow ]
		ifFalse: [ self add: selector and: lastUsedTime whenFullTo: selectorsToShow ]	! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 14:44:54'!
add: selector and: lastUsedTime whenFullTo: selectorsToShow

	selectorsToShow
		findBinaryIndex: [ :selectorAndTime | selectorAndTime second < lastUsedTime ifTrue: [ -1 ] ifFalse: [ 1 ]]
		do: [ :found | ]
		ifNone: [ :leftBound :rightBound | self insert: selector and: lastUsedTime at: rightBound to: selectorsToShow ].
		
	^selectorsToShow 
! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 14:49:34'!
add: selector and: lastUsedTime whenNotFullTo: selectorsToShow
			
	selectorsToShow add: { selector . lastUsedTime }.
	
	^selectorsToShow size = EntriesLimit 
		ifTrue: [ self sortByLastUsedTime: selectorsToShow ]
		ifFalse: [ selectorsToShow ]
	! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 14:45:44'!
insert: selector and: lastUsedTime at: insertionIndex to: selectorsToShow

	insertionIndex <= EntriesLimit ifTrue: [ selectorsToShow insert: { selector . lastUsedTime } shiftingRightAt: insertionIndex ].
	
	^selectorsToShow ! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 14:44:13'!
sortByLastUsedTime: selectorsToShow

	^selectorsToShow asArray sort: [ :leftSelectorAndTime :rightSelectorAndTime | leftSelectorAndTime second > rightSelectorAndTime second ]! !


!Object methodsFor: 'private' stamp: 'HAW 12/20/2018 14:17:22'!
errorSubscriptBounds: index 
	"Create an error notification that an improper integer was used as an index."

	self error: (self errorDescriptionForSubcriptBounds: index)! !


!TestCase methodsFor: 'Accessing' stamp: 'HAW 12/20/2018 14:19:58'!
should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: anExpectedErrorMessageCreator

	self 
		should: aBlockToFail 
		raise: anExceptionHandlingCondition 
		withExceptionDo: [ :anException | self assert: anExpectedErrorMessageCreator value equals: anException messageText ]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 14:47:00'!
computeMessageEntries: receiverClass

	| selectorsToShow notUnderstoodSelectors |
	
	selectorsToShow _ OrderedCollection new.
	notUnderstoodSelectors _ OrderedCollection new.
	
	self class protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :selector :lastUsedTime |
			(receiverClass isNil or: [ receiverClass canUnderstand: selector ]) 
				ifTrue: [ selectorsToShow := self add: selector and: lastUsedTime to: selectorsToShow ]
				ifFalse: [ notUnderstoodSelectors := self add: selector and: lastUsedTime to: notUnderstoodSelectors ]]].
				
	selectorsToShow isEmpty ifTrue: [ selectorsToShow _ notUnderstoodSelectors ]. 
	selectorsToShow size < EntriesLimit ifTrue: [ selectorsToShow _  self sortByLastUsedTime: selectorsToShow ].
	
	" To see the timestamps in the menu (need to tweak #insertCompletion: to activate. Right now, just for debugging)
	entries _ selectorsToShow collect: [ :ary | ary first, '(', ((DateAndTime fromString: '01/01/1996 00:00') + ary second minutes) printString,')' ]
	"
	entries _ selectorsToShow collect: [ :selectorAndTime | selectorAndTime first ]
	
! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 11:32:31'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id receiverClassOrEntries |

	id _ allSource copyFrom: range start to: range end.
	receiverClassOrEntries _ range rangeType 
		caseOf: {
			[ #globalVar ] 		-> [ (Smalltalk at: id asSymbol) class ].
			[ #self ] 				-> [ contextClass ].
			[ #super ] 				-> [ contextClass superclass ].
			[ #true ] 				-> [ True ].
			[ #false ] 				-> [ False ].
			[ #nil ] 				-> [ UndefinedObject ].
			[ #character ] 		-> [ id first class ].
			[ #number ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #string ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #symbol ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #stringSymbol ]  	-> [ self classOfLiteral: id in: contextClass ].
			[ #instVar ] 			-> [ specificModel classOfInstVarNamed: id ].
			[ #methodArg ] 		-> [ specificModel classOfTempVarNamed: id ].
			[ #tempVar ] 			-> [ specificModel classOfTempVarNamed: id ].
			[ #workspaceVar ]		-> [ specificModel classOfWorkspaceVarNamed: id ].
			[ #blockArg ]			-> [ specificModel classOfBlockArgNamed: id ].
			[ #blockTempVar ]		-> [ specificModel classOfBlockTempVarNamed: id ].
			[ #thisContext ] 		-> [ specificModel classOfThisContext ]. 
			[ #classVar ] 			-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #poolConstant ] 	-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #blockEnd ]			-> [ BlockClosure ].
			[ #arrayEnd ]			-> [ Array ].
			[ #rightBrace ]		-> [ Array ].
			[ #unary ]				-> [ self returnTypeOfUnaryMessage: id at: range ].
			[ #rightParenthesis ]  -> [ self returnTypeOfEnclosedExpressionAt: range ]. }
		otherwise: [ nil ].
	
	receiverClassOrEntries isCollection 
		ifTrue: [ self computeMessageEntriesForSelectors: receiverClassOrEntries ]
		ifFalse: [ self computeMessageEntries: receiverClassOrEntries ]! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'HAW 12/20/2018 11:51:22'!
initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue allImplemented |
	
	EntriesLimit _ 400.
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.
	
	self protected: [
		allImplemented _ Smalltalk allImplementedMessages.
		Selectors _ Trie new.
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				Selectors at: sel ifAbsentPut: [ 0 ].
				method messages do: [ :sentMsg |
					Selectors at: sentMsg put: (Selectors at: sentMsg ifAbsent: [ 0 ]) + 1 ]]].
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				self addSelector: sel method: method allImplemented: allImplemented]].
		""
	
		"The following might not be found in #messages. Give them maximum priority."
		maxSortValue _ SmallInteger maxVal.
		"From MessageNode>>#initialize"
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:) do: [ :sel |
				Selectors at: sel put: maxSortValue ].
	
		maxSortValue _ SmallInteger maxVal-1.
		"From SystemDictionary >>#recreateSpecialObjectsArray"
		(1 to: Smalltalk specialSelectorSize) do:  [ :i |
				Selectors at: (Smalltalk specialSelectorAt: i) put: maxSortValue ]]! !


!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents rawBasicAt: rawBasicAt:put: size yourself)
('as yet unclassified' revisar)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= closeTo: hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed changed: removeDependent: update:)
('error handling' assert: assert:description: caseError confirm: confirm:orCancel: doesNotUnderstand: error: halt halt: handles: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isFloat isFraction isInteger isInterval isMethodProperties isNumber isPoint isPseudoContext isSequenceableCollection isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy explore hasContentsInExplorer inform: notYetImplemented notify:)
('private' errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('*Aconcagua-Extensions' equalsCompoundMeasure: equalsSimpleMeasure: isNothing)
('*Chalten-Time Extensions' isGregorianDate)
('*DenotativeObject' isEqualTo: isIdenticalTo: isNotEqualTo: isNotIdenticalTo:)
!


!ArrayedCollection reorganize!
('accessing' size)
('adding' add:)
('printing' storeOn:)
('private' defaultElement storeElementsFrom:to:on:)
('sorting' isSorted isSortedBy: mergeFirst:middle:last:into:by: mergeSortFrom:to:by: mergeSortFrom:to:src:dst:by: quickSortFrom:to:by: sort sort:)
('objects from disk' byteSize bytesPerBasicElement bytesPerElement restoreEndianness writeOn:)
('inserting' insert:shiftingRightAt:)
('*Compression' writeOnGZIPByteStream:)
!


!TestCase reorganize!
('Dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('Accessing' assert: assert:description: assert:description:resumable: assert:equals: defaultFailDescription deny: deny:description: deny:description:resumable: errored: fail failWith: failureString raisedError resources selector should: should:description: should:raise: should:raise:description: should:raise:withExceptionDo: should:raise:withExceptionDo:description: should:raise:withMessageText: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description: signalFailure:)
('Running' debug debugAsFailure debugAsFailureIfCanNot: failureLog isLogging logFailure: run run: runCase setUp setUpResources tearDown tearDownResources)
('Private' canNotDebugMethodErrorDescription comparingStringBetween:and: ifCanNotDebugDo: methodForTest openDebuggerOnFailingTestMethod performTest runCaseAsFailure setTestSelector: signalCanNotDebugMethod)
('Printing' printOn:)
('Testing' is: isSameAs:)
!

!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser'
	classVariableNames: 'AccessLock EntriesLimit Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntry)
('entries - private ' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: canComputeMessageEntriesFor:and: classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: computeIdentifierEntries computeMessageEntries: computeMessageEntriesFor:at:in:and: computeMessageEntriesForSelectors: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and: insert:and:at:to: parse:in:and: returnTypeOfEnclosedExpressionAt: returnTypeOfUnaryMessage:at: selectedClassOrMetaClassIn: sortByLastUsedTime: textProviderOrModel withParserSourceMethodNodeDo:ifError:)
('testing' opensWithTab)
('*LiveTypeInformation' returnTypeOf:in: returnTypeOfEnclosedExpresionIn:at: returnTypeOfNodeUnderCursorIn:at:)
!

SmalltalkCompleter initialize!
