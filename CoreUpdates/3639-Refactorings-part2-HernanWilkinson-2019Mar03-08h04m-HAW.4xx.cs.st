'From Cuis 5.0 of 7 November 2016 [latest update: #3639] on 4 March 2019 at 4:23:49 pm'!

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:56:13'!
withParseNodeIncluding: aPosition do: aBlock ifAbsent: anAbsentBlock

	| nodeAndPosition |

	nodeAndPosition :=self parseNodeIncluding: aPosition ifAbsent: [ ^ anAbsentBlock value ].
	^aBlock value: nodeAndPosition key.! !


!CodeProvider methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:52:13'!
isEditingMethod

	^false! !


!Browser methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:02:28'!
insertSuperclass

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(InsertSuperclassApplier on: self for: aBehavior theNonMetaClass) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:02:36'!
removeClass

	self safelyRemoveClass ! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:02:38'!
renameClass

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(RenameClassApplier on: self for: aBehavior theNonMetaClass) value ].! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:02:41'!
safelyRemoveClass

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(SafelyRemoveClassApplier on: self of: aBehavior theNonMetaClass) value ].! !

!Browser methodsFor: 'testing' stamp: 'HAW 3/4/2019 16:02:31'!
isEditingMethod

	^editSelection = #editMessage or: [ editSelection = #newMessage ]
! !


!MessageSet methodsFor: 'message list' stamp: 'HAW 3/4/2019 16:11:56'!
addMethodReference: aMethodReferenceToAdd ifIncluded: aBlockClosure

	(messageList includes: aMethodReferenceToAdd)
		ifTrue: aBlockClosure
		ifFalse: [ self addMethodReference: aMethodReferenceToAdd ]! !

!MessageSet methodsFor: 'message list' stamp: 'HAW 3/4/2019 16:11:51'!
removeMessageFromBrowser
	"Remove the selected message from the browser."

	self removeMessageFromBrowserKeepingLabel.
	self changed: #relabel! !

!MessageSet methodsFor: 'message list' stamp: 'HAW 3/4/2019 16:11:48'!
removeMessageFromBrowserKeepingLabel

	selectedMessage ifNil: [ ^nil ].
	self deleteFromMessageList: self selection.
	self reformulateList.
! !


!Debugger methodsFor: 'testing' stamp: 'HAW 3/4/2019 16:05:01'!
isEditingMethod

	^true! !


!UndefinedObject methodsFor: 'accessing instances and variables' stamp: 'HAW 3/4/2019 15:18:06'!
whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock

	^aNoneBlock value! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 3/4/2019 15:19:49'!
methodsSelect: aCondition

	^ self methodDict valuesSelect: aCondition! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 3/4/2019 15:20:07'!
methodsWithArgumentOrTemporaryNamed: instVarName

	^self methodsSelect: [:aMethod | aMethod hasArgumentOrTemporary: instVarName ]! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 3/4/2019 15:20:45'!
withSuperclassThatIncludesSelector: aSelector do: aFoundClosure ifNone: aNoneClosure

	^superclass
		ifNil: aNoneClosure
		ifNotNil: [
			(superclass whichClassIncludesSelector: aSelector)
				ifNil: aNoneClosure
				ifNotNil: aFoundClosure ]! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 3/4/2019 15:18:50'!
definesClassVariableNamedInHierarchy: aClassVariableName

	^self allClassVarNames includes: aClassVariableName ! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 3/4/2019 15:19:06'!
definesInstanceVariableNamed: anInstanceVariableName

	^self instVarNames includes: anInstanceVariableName! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 3/4/2019 15:19:30'!
indexOfInstanceVariable: aName

	^self allInstVarNames indexOf: aName! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 3/4/2019 15:20:24'!
whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock

	(self definesInstanceVariableNamed: aVariable) ifTrue: [ ^self ].

	^superclass whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock! !


!ClassDescription methodsFor: 'instance variables' stamp: 'HAW 3/4/2019 15:21:47'!
chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock

	^self
		chooseDefiningInstanceVariableAlphabeticallyWith: aCaption
		thenDo: aBlock
		ifNone: [ ^ self inform: 'There are no instance variables defined in ', self name ] ! !

!ClassDescription methodsFor: 'instance variables' stamp: 'HAW 3/4/2019 15:21:57'!
chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock ifNone: noneBlock

	| vars index |
	"Put up a menu of the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter."

	vars _ self instVarNames sorted.
	vars isEmpty ifTrue: [^ noneBlock value ].

	index _ (PopUpMenu labelArray: vars lines: #()) startUpWithCaption: aCaption.
	index = 0 ifTrue: [^ self].
	aBlock value: (vars at: index)! !

!ClassDescription methodsFor: 'instance variables' stamp: 'HAW 3/4/2019 15:26:00'!
withClassesThatDefineInHierarchyInstanceVariable: aName do: foundBlock ifNone: noneBlock

	^(self classThatDefinesInstanceVariable: aName)
		ifNil: [ self withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock ]
		ifNotNil: [ :definingClass | foundBlock value: (Array with: definingClass) ]! !

!ClassDescription methodsFor: 'instance variables' stamp: 'HAW 3/4/2019 15:26:16'!
withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock

	| definingSubclasses |

	definingSubclasses := self allSubclasses select: [ :aSubclass | aSubclass definesInstanceVariableNamed: aName ].

	^definingSubclasses isEmpty
		ifTrue: noneBlock
		ifFalse: [ foundBlock value: definingSubclasses ]! !


!Class methodsFor: 'class name' stamp: 'HAW 3/4/2019 15:27:49'!
rename: aString
	"The new name of the receiver is the argument, aString."

	| newName |
	(newName _ aString asSymbol) ~= self name
		ifFalse: [^ self].
	(Smalltalk includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists'].
	(Undeclared includesKey: newName)
		ifTrue: [self inform: 'There are references to, ' , aString printString , '
from Undeclared. Check them after this change.'].

	self safeRenameTo: newName.! !

!Class methodsFor: 'class name' stamp: 'HAW 3/4/2019 15:27:53'!
safeRenameTo: newName

	Smalltalk renameClass: self as: newName.
	name _ newName! !


!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 3/4/2019 15:58:45'!
hasCategory: aCategory

	^self categories includes: aCategory ! !

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 3/4/2019 15:58:48'!
removeSystemCategories: categories

	(self superclassOrderInAll: categories) reverseDo: [ :class | class removeFromSystem].

	categories do: [ :aCategory | self removeCategory: aCategory].
! !

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 3/4/2019 15:58:52'!
superclassOrderInAll: categories

	| classes |

	classes := OrderedCollection new.
	categories do: [ :aCategory | classes addAll: (self classesAt: aCategory)].

	^Array streamContents: [ :stream | Smalltalk hierarchySorted: classes do: [ :aClass | stream nextPut: aClass ]].! !


!Number methodsFor: 'intervals' stamp: 'HAW 3/4/2019 15:29:17'!
toSelfPlus: aDelta

	^self to: self + aDelta! !


!Collection methodsFor: 'converting' stamp: 'HAW 3/4/2019 15:36:16'!
as: aPrintingBlock on: aStream delimiter: delimiter last: lastDelimiter

	| position selfSize |

	position := 1.
	selfSize := self size.

	self
		do: [:elem |
			position := position + 1.
			aPrintingBlock value: elem ]
		separatedBy: [
			aStream nextPutAll: (position = selfSize ifTrue: [lastDelimiter] ifFalse: [delimiter])]! !

!Collection methodsFor: 'converting' stamp: 'HAW 3/4/2019 15:36:31'!
asCommaSeparated: aPrintingBlock on: aStream

	^self as: aPrintingBlock on: aStream delimiter: ', ' last: ' and '! !

!Collection methodsFor: 'converting' stamp: 'HAW 3/4/2019 15:37:03'!
asCommaStringAnd

	^String streamContents: [:stream | self asStringOn: stream delimiter: ', ' last: ' and ']! !

!Collection methodsFor: 'converting' stamp: 'HAW 3/4/2019 15:37:18'!
asStringOn: aStream delimiter: delimString last: lastDelimString

	self as: [ :elem | aStream nextPutAll: elem asString ] on: aStream delimiter: delimString last: lastDelimString! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'HAW 3/4/2019 15:38:35'!
intervalOfSubCollection: aSubCollectionToFind

	| startingIndex |

	startingIndex := self indexOfSubCollection: aSubCollectionToFind startingAt: 1.

	^startingIndex toSelfPlus: aSubCollectionToFind size! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'HAW 3/4/2019 15:37:58'!
with: otherCollection do: twoArgBlock separatedBy: separatorBlock

	| beforeFirst |

	beforeFirst := true.
	self with: otherCollection do: [ :selfElement :otherCollectionElement |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		twoArgBlock value: selfElement value: otherCollectionElement ].


! !


!String methodsFor: 'copying' stamp: 'HAW 3/4/2019 15:30:11'!
copyReplacing: rangesToNewStrings

	^self class streamContents: [ :replacementStream | self copyReplacing: rangesToNewStrings into: replacementStream ].! !

!String methodsFor: 'copying' stamp: 'HAW 3/4/2019 15:30:14'!
copyReplacing: rangesToNewStrings into: replacementStream

	| lastFrom |

	lastFrom := rangesToNewStrings inject: 1 into: [ :from :aRangeToNewString |
		self
			newFromAfterAppending: aRangeToNewString value
			into: replacementStream
			keepingFrom: from
			skipping: aRangeToNewString key ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).
! !

!String methodsFor: 'copying' stamp: 'HAW 3/4/2019 15:30:18'!
copyReplacing: ranges with: newString

	"Ranges must be in order, with first ranges first. If not, result is unexpected - Hernan"

	^ self class streamContents: [ :replacementStream | self copyReplacing: ranges with: newString into: replacementStream ]
		! !

!String methodsFor: 'copying' stamp: 'HAW 3/4/2019 15:30:22'!
copyReplacing: ranges with: newString into: replacementStream

	| lastFrom |

	lastFrom := ranges
		inject: 1
		into: [ :from :aRange |
			self newFromAfterAppending: newString into: replacementStream keepingFrom: from skipping: aRange ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).! !

!String methodsFor: 'copying' stamp: 'HAW 3/4/2019 15:30:28'!
newFromAfterAppending: aNewString into: replacementStream keepingFrom: from skipping: aRange

	replacementStream
		nextPutAll: (self copyFrom: from to: aRange first - 1);
		nextPutAll: aNewString.

	^ aRange last + 1! !


!Symbol class methodsFor: 'instance creation' stamp: 'HAW 3/4/2019 15:31:05'!
fromCollectionOfStrings: aCollectionOfStrings

	^self newFrom: aCollectionOfStrings concatenation ! !


!CompiledMethod methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:32:05'!
hasArgumentOrTemporary: aVariable

	| methodNode |

	methodNode := self methodNode.

	^methodNode tempNames includes: aVariable ! !

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:33:16'!
readsInstanceVariable: aName

	^self readsField: (self methodClass indexOfInstanceVariable: aName) ! !

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:33:41'!
referencesParameterAt: parameterIndex

	| methodNode implementorParameterNodeToRemove parameterRanges |

	methodNode := self methodNode.
	implementorParameterNodeToRemove := methodNode arguments at: parameterIndex.
	parameterRanges := methodNode positionsForTemporaryVariable: implementorParameterNodeToRemove name ifAbsent: [#()].

	^parameterRanges size ~= 1! !

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:33:58'!
sendsOrRefersTo: aSelector

	^ (self hasLiteralThorough: aSelector) or: [ self sendsSelector: aSelector ]! !

!CompiledMethod methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:34:45'!
writesInstanceVariable: aName

	^self writesField: (self methodClass indexOfInstanceVariable: aName)! !

!CompiledMethod methodsFor: 'printing' stamp: 'HAW 3/4/2019 15:31:47'!
classAndSelector

	^String streamContents: [:stream | self printClassAndSelectorOn: stream ]! !

!CompiledMethod methodsFor: 'printing' stamp: 'HAW 3/4/2019 15:32:47'!
printClassAndSelectorOn: aStream

	aStream
		print: self methodClass;
		nextPutAll: '>>';
		nextPutAll: self selector storeString! !

!CompiledMethod methodsFor: 'printing' stamp: 'HAW 3/4/2019 15:32:51'!
printOn: aStream
	"Overrides method inherited from the byte arrayed collection."

	aStream nextPut: $(.
	self printClassAndSelectorOn: aStream.
	aStream space; nextPut: $".
	self printNameOn: aStream.
	aStream nextPut: $(; print: self identityHash; nextPut: $); nextPut: $"; nextPut: $)! !

!CompiledMethod methodsFor: 'source code management' stamp: 'HAW 3/4/2019 15:34:32'!
sourceCode

	"This method is implemented because getSource is not so intuitive - Hernan"
	^self getSource! !


!MethodDictionary methodsFor: 'enumeration' stamp: 'HAW 3/4/2019 15:35:17'!
valuesSelect: aCondition

	| selected |

	selected := OrderedCollection new.
	self valuesDo: [ :aValue | (aCondition value: aValue) ifTrue: [ selected add: aValue ]].

	^selected! !


!Parser class methodsFor: 'parsing' stamp: 'HAW 3/4/2019 15:41:47'!
parse: sourceCode class: aClass

	^self parse: sourceCode class: aClass noPattern: false! !

!Parser class methodsFor: 'parsing' stamp: 'HAW 3/4/2019 15:41:50'!
parse: sourceCode class: aClass noPattern: aBoolean

	^(self new
		encoderClass: EncoderForV3PlusClosures;
		parse: sourceCode class: aClass noPattern: aBoolean)
			sourceText: sourceCode;
			yourself! !


!ParseNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:43:27'!
isInstanceVariableNode

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:43:38'!
isMessageNamed: aSelector

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:43:50'!
isReturn

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:43:53'!
isTempOrArg

	^false! !


!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:39:58'!
messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aBlock

	| positions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordAndParameterPositionAt: anIndex encodedWith: self].

	^ positions isEmpty
		ifTrue: aBlock
		ifFalse: [ positions ]
	! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:02'!
messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock

	| positions sortedPositions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | (aMessageSendNode keywordPositionAt: anIndex) first ].

	positions isEmpty ifTrue: [ ^aBlock value ].
	sortedPositions := positions asSortedCollection.

	^sortedPositions ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:05'!
messageSendLastPositionsOf: aSelector ifAbsent: aBlock

	| positions sortedPositions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | (sourceRanges at: aMessageSendNode) last ].

	positions isEmpty ifTrue: [ ^aBlock value ].
	sortedPositions := positions asSortedCollection.

	^sortedPositions ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:08'!
messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock

	| ranges sortedRanges |

	ranges := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordRanges ].

	ranges isEmpty ifTrue: [ ^aBlock value ].
	sortedRanges := ranges asSortedCollection: [ :left :right | left first first < right first first ].

	^sortedRanges ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:12'!
parameterDefinitionPositionFor: aParameterNode

	^ (self sourceRangeFor: aParameterNode) first! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:15'!
positionsForInstanceVariable: aName ifAbsent: aBlock

	^ self
		rangesForVariable: aName
		checkingType: [ :variableNode | variableNode isInstanceVariableNode ]
		ifAbsent: aBlock ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:19'!
positionsForLiteralNode: aName ifAbsent: aBlock

	| variableNode |

	variableNode := litSet at: aName ifAbsent: [ ^aBlock value ].

	^sourceRanges at: variableNode ifAbsent: aBlock! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:23'!
positionsForLiteralVariableNode: aName ifAbsent: aBlock

	| variableNode |

	variableNode := litIndSet values detect: [ :aLiteralVariableNode | aLiteralVariableNode name = aName ] ifNone: [ ^aBlock value ].

	^sourceRanges at: variableNode ifAbsent: aBlock! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:25'!
positionsForTemporaryVariable: aName ifAbsent: aBlock

	^ self
		rangesForVariable: aName
		checkingType: [ :variableNode | variableNode isTemp ]
		ifAbsent: aBlock ! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:40:29'!
positionsOfLiteralArraysContaining: aSymbol

	| positions |

	positions := OrderedCollection new.
	litSet keysAndValuesDo: [ :aLiteral :aLiteralNode |
		(aLiteral isArray and: [ aLiteral hasLiteral: aSymbol ]) ifTrue: [ positions addAll: (sourceRanges at: aLiteralNode) ]].

	^positions ! !


!EncoderForV3PlusClosures methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:41:15'!
hasLocalNamed: aName

	^ scopeTable includesKey: aName ! !


!LeafNode methodsFor: 'accessing' stamp: 'HAW 3/4/2019 15:44:23'!
key: aKey

	key := aKey ! !


!VariableNode methodsFor: 'initialization' stamp: 'HAW 3/4/2019 15:51:10'!
nameAndKey: aName

	name := key := aName ! !


!InstanceVariableNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:51:26'!
isInstanceVariableNode

	^true! !


!TempVariableNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:51:43'!
isTempOrArg

	^self isTemp or: [ self isArg ]! !


!MessageNode methodsFor: 'source ranges' stamp: 'HAW 3/4/2019 15:45:16'!
keywordAndParameterPositionAt: anIndex encodedWith: anEncoder

	| keywordPosition parameterLastPosition |

	keywordPosition := keywordRanges at: anIndex.
	parameterLastPosition := anIndex = arguments size
		ifTrue: [ (anEncoder sourceRangeFor: self) last ]
		ifFalse: [ (keywordRanges at: anIndex + 1) first - 1].

	^keywordPosition first to: parameterLastPosition! !

!MessageNode methodsFor: 'source ranges' stamp: 'HAW 3/4/2019 15:45:19'!
keywordPositionAt: anIndex

	^keywordRanges at: anIndex ! !

!MessageNode methodsFor: 'source ranges' stamp: 'HAW 3/4/2019 15:45:22'!
keywordRanges

	^keywordRanges! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:44:44'!
isMessageNamed: aSelector

	^aSelector == self selectorSymbol ! !

!MessageNode methodsFor: 'accessing' stamp: 'HAW 3/4/2019 16:23:43'!
selectorSymbol

	^selector key! !


!MethodNode methodsFor: 'accessing' stamp: 'HAW 3/4/2019 15:46:33'!
argumentNames

	^arguments collect: [ :anArgumentNode | anArgumentNode name ]! !

!MethodNode methodsFor: 'accessing' stamp: 'HAW 3/4/2019 15:48:55'!
tempNodes

	^encoder tempNodes! !

!MethodNode methodsFor: 'printing' stamp: 'HAW 3/4/2019 15:47:01'!
classAndSelector

	^self methodClass name, '>>', self selector storeString! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:46:24'!
addPositionTo: symbolPositions of: symbolString inside: literalArrayPosition

	| insidePosition |

	insidePosition := literalArrayPosition first.
	[ insidePosition < literalArrayPosition last ] whileTrue: [
		insidePosition := self nextPositionAfterAddPositionTo: symbolPositions of: symbolString startingAt: insidePosition ].
	! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:09'!
messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aClosure

	^encoder messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aClosure! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:12'!
messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock

	^encoder messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock
	! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:16'!
messageSendLastPositionsOf: aSelector ifAbsent: aBlock

	^encoder messageSendLastPositionsOf: aSelector ifAbsent: aBlock! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:19'!
messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock

	^encoder messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:23'!
nextPositionAfterAddPositionTo: symbolPositions of: symbolString startingAt: insidePosition

	| symbolStartPosition nextPosition |

	symbolStartPosition := sourceText indexOfSubCollection: symbolString startingAt: insidePosition.

	symbolStartPosition = 0
		ifTrue: [ nextPosition := SmallInteger maxVal ]
		ifFalse: [
			nextPosition := symbolStartPosition + symbolString size.
			(sourceText at: nextPosition) tokenish ifFalse: [ symbolPositions add: (symbolStartPosition to: nextPosition - 1) ]].

	^nextPosition

	! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:27'!
parameterDefinitionPositionAt: anIndex

	^encoder parameterDefinitionPositionFor: (arguments at: anIndex)! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:30'!
positionsForInstanceVariable: aName ifAbsent: aBlock

	^encoder positionsForInstanceVariable: aName ifAbsent: aBlock
! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:33'!
positionsForLiteralNode: aName ifAbsent: aBlock

	^encoder positionsForLiteralNode: aName ifAbsent: aBlock
! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:36'!
positionsForLiteralVariableNode: aName ifAbsent: aBlock

	^encoder positionsForLiteralVariableNode: aName ifAbsent: aBlock! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:39'!
positionsForTemporaryVariable: aName ifAbsent: aBlock

	^encoder positionsForTemporaryVariable: aName ifAbsent: aBlock! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:42'!
positionsInLiteralArrayOf: aSymbol

	| literalArrayPositions |

	literalArrayPositions := encoder positionsOfLiteralArraysContaining: aSymbol.

	^self positionsOf: aSymbol printString containedIn: literalArrayPositions.

! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:45'!
positionsOf: symbolString containedIn: literalArrayPositions

	| symbolPositions |

	symbolPositions := OrderedCollection new.

	literalArrayPositions do: [ :literalArrayPosition | self addPositionTo: symbolPositions of: symbolString inside: literalArrayPosition ].

	^symbolPositions




! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:48'!
selectorKeywordPositionAt: anIndex

	^selectorKeywordsRanges at: anIndex! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 3/4/2019 15:48:52'!
selectorKeywordsPositions

	^selectorKeywordsRanges! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:47:33'!
hasArgumentOrTemporary: aVariable

	^self tempNames includes: aVariable! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 16:13:33'!
hasLocalNamed: aName

	^ encoder hasLocalNamed: aName ! !


!ReturnNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:50:43'!
isImplicitSelfReturnIn: aMethodNode

	^self isReturnSelf and: [ (aMethodNode encoder rawSourceRanges includesKey: expr) not ]! !

!ReturnNode methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:50:48'!
isReturn

	^true! !


!TextEditor methodsFor: 'accessing' stamp: 'HAW 3/4/2019 15:52:45'!
actualContents

	^model actualContents ! !

!TextEditor methodsFor: 'accessing' stamp: 'HAW 3/4/2019 15:52:48'!
actualContents: aString

	model actualContents: aString ! !

!TextEditor methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:53:18'!
hasUnacceptedEdits

	^morph hasUnacceptedEdits ! !

!TextEditor methodsFor: 'testing' stamp: 'HAW 3/4/2019 15:53:29'!
hasUnacceptedEdits: aBoolean

	^morph hasUnacceptedEdits: aBoolean ! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:20'!
contextualRename

	self
		withMethodNodeAndClassDo: [ :methodNode :classToRefactor | self contextualRenameOf: methodNode in: classToRefactor]
		ifErrorsParsing: [ :anError | morph flash ].

		! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:23'!
contextualRename: aKeyboardEvent

	self contextualRename.
	^true! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:26'!
contextualRenameOf: aMethodNode in: aClassToRefactor

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self rename: nodeUnderCursor in: aClassToRefactor ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifRenameCanBeAppliedDo: [ self renameSelectorFor: aMethodNode selector in: aClassToRefactor ]]
				ifFalse: [ morph flash ]]


! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:30'!
ifRenameCanBeAppliedDo: aBlock

	^(self hasUnacceptedEdits or: [morph hasEditingConflicts ])
		ifTrue: [ self inform: 'Rename can not be applied becuase there are unsaved changes' ]
		ifFalse: aBlock! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:35'!
rename: aNodeUnderCursor in: aClassToRefactor

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor name ].

	self ifRenameCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode ifTrue: [ ^ self renameSelectorFor: aNodeUnderCursor selector key in: aClassToRefactor ].
		aNodeUnderCursor isInstanceVariableNode ifTrue: [ ^ self renameInstanceVariableOn: self codeProvider for: aNodeUnderCursor name at: aClassToRefactor ].
		aNodeUnderCursor isLiteralVariableNode ifTrue: [ | posibleBehavior |
			posibleBehavior := aNodeUnderCursor key value.
			posibleBehavior isBehavior ifTrue: [ ^self renameClassOn: self codeProvider for: posibleBehavior theNonMetaClass ]].

		^morph flash ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:38'!
renameClassOn: aBrowser for: aClassToRefactor

	(RenameClassApplier on: aBrowser for: aClassToRefactor) value! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:41'!
renameInstanceVariableOn: aBrowser for: anInstanceVariableName at: aClassToRefactor

	(RenameInstanceVariableApplier on: aBrowser for: anInstanceVariableName at: aClassToRefactor) value! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:45'!
renameSelectorFor: aSelector in: aClassToRefactor

	RenameSelectorApplier createAndValueHandlingExceptions: [ RenameSelectorApplier on: model textProvider for: aSelector in: aClassToRefactor ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:49'!
renameTemporary: aTemporaryName

	self codeProvider isEditingMethod ifTrue: [ | applier |
		[ applier := RenameTemporaryApplier on: self for: aTemporaryName ]
			on: SyntaxErrorNotification
			do: [:anError | ^self inform: (RenameTemporaryApplier errorMessageForCanNotParseMethod: anError) ].
		applier value ].
! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 3/4/2019 15:57:52'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| class methodNode |

	class := self codeProvider selectedClassOrMetaClass.
	methodNode := [ class methodNodeFor: model actualContents ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: class.! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 15:59:35'!
addInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(AddInstanceVariableApplier on: model for: aClass) value ].! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 15:59:38'!
addParameter

	model selectedMessageName ifNotNil: [ :oldSelector |
		AddParameterApplier createAndValueHandlingExceptions: [ AddParameterApplier on: model for: oldSelector in: model selectedClassOrMetaClass ]]! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 15:59:41'!
classRefactoringMenu

	^DynamicMenuBuilder buildTitled: 'Refactorings' targeting: self collectingMenuOptionsWith: #classRefactoringMenuOptions.! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 15:59:47'!
messageRefactoringMenu

	^DynamicMenuBuilder buildTitled: 'Refactorings' targeting: self collectingMenuOptionsWith: #messsageRefactoringMenuOptions.! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 15:59:50'!
openClassRefactoringMenu

	^self classRefactoringMenu popUpInWorld! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 15:59:53'!
openMessageRefactoringMenu

	^self messageRefactoringMenu popUpInWorld! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:00:10'!
pushUpSelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushUpMethod for: model selectedClassOrMetaClass>>selectedSelector) apply.
		model messageListIndex: 0.
		model changed: #messageList.
		model setClassOrganizer ].! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:00:13'!
removeAllUnreferencedInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveAllUnreferencedInstanceVariablesApplier on: model for: aClass) value ].! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:00:16'!
removeInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveInstanceVariableApplier on: model for: aClass) value ].! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:00:23'!
removeParameter

	model selectedMessageName ifNotNil: [ :oldSelector |
		RemoveParameterApplier createAndValueHandlingExceptions: [
			RemoveParameterApplier on: model for: oldSelector in: model selectedClassOrMetaClass ]]! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:00:26'!
renameInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RenameInstanceVariableApplier on: model at: aClass) value ].! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 3/4/2019 16:00:30'!
renameSelector

	model selectedMessageName ifNotNil: [ :oldSelector |
		RenameSelectorApplier createAndValueHandlingExceptions: [ RenameSelectorApplier on: model for: oldSelector in: model selectedClassOrMetaClass ]].! !


!MessageSetWindow class methodsFor: 'opening' stamp: 'HAW 3/4/2019 16:01:36'!
openFullProtocolForClass: aClass
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openFullProtocolForClass: ProtocolBrowser."

	| aPBrowser label |

	aPBrowser _ ProtocolBrowser new on: aClass.
	label _ 'Entire protocol of: ', aClass name.

	^self open: aPBrowser label: label! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'HAW 3/4/2019 16:01:39'!
openMessageList: anArray label: aString
	"Create a standard system view for the message set on the list, anArray.
	The label of the view is aString."

	^self open: (MessageSet messageList: anArray) label: aString! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'HAW 3/4/2019 16:01:42'!
openMessageList: messageList label: labelString autoSelect: autoSelectString
	"Open a system view for a MessageSet on messageList.
	 1/24/96 sw: the there-are-no msg now supplied by my sender"

	| messageSet |

	messageSet _ MessageSet messageList: messageList.
	messageSet autoSelectString: autoSelectString.

	^self open: messageSet label: labelString ! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'HAW 3/4/2019 16:01:46'!
openSubProtocolForClass: aClass
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openSubProtocolForClass: ProtocolBrowser."

	| aPBrowser label |

	aPBrowser _ ProtocolBrowser new onSubProtocolOf: aClass.
	label _ 'Sub-protocol of: ', aClass name.

	^self open: aPBrowser label: label! !


!ChangeSelector methodsFor: 'create new implementors - private' stamp: 'HAW 3/4/2019 15:24:22'!
compileNewImplementorOf: anImplementor

	| implementorClassification newSourceCode |

	newSourceCode := self implementorNewSourceCodeOf: anImplementor.
	implementorClassification := anImplementor methodClass organization categoryOfElement: oldSelector.

	anImplementor methodClass
		compile: newSourceCode
		classified: implementorClassification.
! !


!PushUpMethod methodsFor: 'applying' stamp: 'HAW 3/4/2019 15:23:40'!
apply

	| methodCategory |

	methodCategory := method methodClass organization categoryOfElement: method selector.
	method methodClass superclass
		compile: method sourceCode
		classified: methodCategory.

	method methodClass removeSelector: method selector.
 ! !

!methodRemoval: MethodNode #selectorKeywordsRanges!
MethodNode removeSelector: #selectorKeywordsRanges!
!methodRemoval: ClassDescription #compile:classifyUnder:!
ClassDescription removeSelector: #compile:classifyUnder:!

!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForHierarchyFor: annotationForSelector:ofClass: annotationRequests annotationSeparator)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText commentContents contentsSymbol contentsSymbol: methodNodeOf:ifErrorsParsing:)
('commands' hierarchyBrowser removeClass)
('controls' sourceAndDiffsQuintsOnly)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showDiffs: showLineDiffs: showPrettyLineDiffs: showPrettyWordDiffs: showWordDiffs: showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleDiffing toggleLineDiffing togglePlainSource togglePrettyLineDiffing togglePrettyPrint togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' prettyPrintString showByteCodes: showComment showDecompile: showDocumentation: showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingDocumentation showingDocumentationString showingPlainSource showingPlainSourceString showingPrettyPrint showingSource toggleDecompile toggleShowDocumentation toggleShowingByteCodes)
('message list' decompiledSource selectedBytecodes selectedMessage sourceStringPrettifiedAndDiffed)
('message list menu' copySelector fileOutMessage inspectCompiledMethod revertToPreviousVersion timeStamp unusedMethods)
('message category functions' canShowMultipleMessageCategories)
('testing' is: isEditingMethod)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext)
('shout styling' shouldStyle:with:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('*LiveTyping' contentsSymbolQuints isModeStyleable showingTypedSource showingTypedSourceString textToCompile: toggleTypedSource typedSource)
!


!Browser reorganize!
('accessing' acceptedStringOrText contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' insertSuperclass removeClass renameClass safelyRemoveClass)
('*LiveTyping' classDefinitionText computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed: contents:notifying: selectedMessage)
('*DenotativeObject' compile:receiver:in: debug debug:receiver:in:withResultDo: debugAndInspect debugAndWithResultDo: debugDenotativeObjectMethodTest isTest: runDenotativeObjectMessageCategoryTests runDenotativeObjectMethodTest runDenotativeObjectSystemCategoryTests runDenotativeObjectTests send sendAndInspect sendAndWithResultDo: stringToEvalSending:to:onCancel:)
('testing' isEditingMethod)
!


!MessageSet reorganize!
('message list' addMethodReference: addMethodReference:ifIncluded: growable messageList messageListIndex: removeMessageFromBrowser removeMessageFromBrowserKeepingLabel selectedMessageName sortByDate)
('message functions' deleteFromMessageList: methodCategoryChanged reformulateList removeMessage)
('class list' metaClassIndicated selectedClass selectedClassOrMetaClass selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText)
('private' autoSelectString autoSelectString: initializeMessageList: selection setFilteredList:)
('filtering' filterFrom: filterToAnyChangeSet filterToCommentedMethods filterToCurrentAuthor filterToImplementorsOf filterToMessagesInChangesFile filterToMessagesInSourcesFile filterToMessagesWithPriorVersions filterToMessagesWithoutPriorVersions filterToNotAnyChangeSet filterToNotCurrentAuthor filterToNotImplementorsOf filterToNotSendersOf filterToSendersOf filterToUncommentedMethods filterToUnsentMessages)
('message category functions' canShowMultipleMessageCategories)
('accessing' labelString reformulateListNoting:)
('shout styling' shouldStyle:with:)
('*LiveTyping' contents:notifying: selectedMessage)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: fixReceiverInspector lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inCategory:fromClass:context: implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('testing' is: isEditingMethod)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('*Cuis-University-Model' askForSuperclassOf:toImplement:ifCancel:)
!


!SystemChangeNotifier reorganize!
('private' triggerEvent: triggerEvent:withArguments:)
('system triggers' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: evaluated:context: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('public' doSilently: isBroadcasting)
('initialization' initialize)
!


!UndefinedObject reorganize!
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('copying' shallowCopy)
('printing' printOn: storeOn:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isEmptyOrNil isLiteral isNil notNil)
('dependents access' addDependent: suspend)
('class hierarchy' addSubclass: allSuperclassesUpTo: literalScannedAs:notifying: removeSubclass: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclasses subclassesDo: subclassesDoGently: superclassNotValidErrorDescriptionFor: typeOfClass)
('bottom context' canHandleSignal: handleSignal:)
('accessing instances and variables' whichClassDefinesInstanceVariable:ifNone:)
!


!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesPreviousTo: allSuperclassesUpTo: subclasses superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesPreviousTo: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allUnreferencedInstanceVariables allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
('*LiveTyping' highestClassImplementing:ifNone: indexOfInstanceVariable:ifAbsent: initializeMethodsTypeInformation isSubclassesEmpty methodVariablesTypesOf: withAllSubclassesImplementing:)
!


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: addSelector:withMethod:notifying: allMethodCategoriesIntegratedThrough: induceMDFault methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: ultimateSourceCodeAt:ifAbsent:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: moveInstVarNamed:to:after: noteCompilationOf:meta: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('private' errorCategoryName instVarMappingFrom: linesOfCode logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing class hierarchy' classesThatImplementAllOf: printSubclassesOn:level: removeUninstantiatedSubclassesSilently subclasses subclassesDo:)
('method dictionary' allMethodsInCategory:)
('system-support' allUnreferencedClassVariables)
('initialization' doneCompiling superclass:methodDictionary:format: updateInstancesFrom:)
('release' obsolete)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('*LiveTyping-unload-private' allRawTypesUnload: instanceVariablesRawTypesUnload: methodsAllRawTypesUnload:)
('*LiveTyping' asTypeFor: commonSelectorsOfInstanceVariableNamed: commonSelectorsOfInstanceVariableNamed:previousTo: commonSelectorsOfInstanceVariableNamedPreviousToObject: commonSelectorsOfMethodVariableNamed:at: commonSelectorsOfMethodVariableNamed:at:previousTo: commonSelectorsOfMethodVariableNamedPreviousToObject:at: commonSupertypeOfInstanceVariableNamed: commonSupertypeOfInstanceVariableNamed:ifNoTypes: commonSupertypeOfMethodVariableNamed:at: commonSupertypeOfMethodVariableNamed:at:ifNoTypes: initializeInstanceVariablesRawTypes initializeInstanceVariablesRawTypesWithAllInstances instanceVariablesTypes methodVariablesTypesAt: returnCommonSelectorsAt: returnCommonSelectorsAt:previousTo: returnCommonSelectorsPreviousToObjectAt: returnCommonSupertypeAt: returnCommonSupertypeAt:ifNoTypes: returnTypeInfoAt: returnTypesAt: typeInfoOfInstanceVariableNamed: typeInfoOfInstanceVariableNamed:ifAbsent: typeInfoOfMethodVariableNamed:at: typesOfInstanceVariableAt: typesOfInstanceVariableNamed: typesOfInstanceVariableNamed:ifAbsent: typesOfMethodVariableNamed:at:)
('*LiveTyping-private' createHierarchyInstanceVariableTypeInfoOf:with: createInstanceVariableRawTypesFor: createInstanceVariablesRawTypes createLocalInstanceVariableTypeInfoOf:with: createMethodReturnRawTypesOf: createMethodReturnTypeInfoOf: createMethodVariableRawTypesFor:in: createMethodVariableTypeInfoOf:in:are: createRawTypesOfSize: defaultRawTypesSize initializeEachInstanceVariableRawTypes instanceVariableRawTypesSizeFor: instanceVariablesRawTypes methodReturnRawTypesSizeOf: methodVariableRawTypesSizeFor:in: printInstanceVariablesTypesOn: storeAllInstancesCurrentTypes typedDefinition)
!


!Class reorganize!
('accessing' classPool classPool: classPoolFrom: name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarNames ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' fileOut fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('private' setName: spaceUsed)
('initialization' declare: sharing: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('tdd' testCaseClass)
('*LiveTyping' isSubclassesEmpty unloadFromAllRawTypes unloadFromAllRawTypesOf:)
!


!SystemOrganizer reorganize!
('fileIn/Out' fileOutAllCategories fileOutCategory: fileOutCategory:on:initializing: fileOutCategoryNoPoolsNoInit:on: objectForDataStream: superclassOrderIn:)
('remove' categoriesMatching: removeCategoriesMatching: removeMissingClasses removeSystemCategory:)
('categories' hasCategory: removeSystemCategories: superclassOrderInAll:)
!


!Number reorganize!
('accessing' argument imaginary magnitude phase real)
('arithmetic' * + - / // \\ ^ abs arg div: mod: negated quo: reciprocal rem:)
('comparing' %)
('converting' @ adaptToCollection:andSend: adaptToComplex:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asComplex asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours i inMinusPiToPiRange milliSeconds minutes nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log2 log: nthRoot: raisedTo: raisedToFraction: raisedToInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even ifNotZero: isDivisibleBy: isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart roundDownTo: roundTo: roundUpTo: rounded truncateTo: truncated)
('*Aconcagua-Extensions' addCompoundMeasure: addInfinity: addMinusInfinity: addSimpleMeasure: amount baseUnit compareGreaterThan: composeWith: composeWithNumber: composeWithSimpleMeasure: convertAmountToBaseUnit convertTo: convertTo:ifSuccessful:ifFails: convertToBaseUnit denominator divideByUnit: divideCompoundMeasure: divideInfinity: divideMinusInfinity: divideSimpleMeasure: equalsCompoundMeasure: equalsSimpleMeasure: fromInfinity fromMinusInfinity greaterThanSimpleMeasure: isCompoundMeasure isFixed isNothing lessThanSimpleMeasure: moduloCompoundMeasure: moduloInfinity: moduloMinusInfinity: moduloSimpleMeasure: multiplyByUnit: multiplyCompoundMeasure: multiplyInfinity: multiplyMinusInfinity: multiplySimpleMeasure: numerator remainderCompoundMeasure: remainderInfinity: remainderMinusInfinity: remainderSimpleMeasure: sameDomainAs: subtractFrom: subtractInfinity: subtractMinusInfinity: toInfinity toMinusInfinity unit unitNameOf:)
!


!Collection reorganize!
('accessing' anyOne atRandom size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCharacterSet asCommaSeparated:on: asCommaStringAnd asDictionary asFloat64Array asFloatArray asIdentitySet asIntegerArray asNewArray asOrderedCollection asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: detectSum: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: customizeExplorerContents eighth fifth fillWith: first first: fourth from:to:put: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate quickIndexOf: replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: third)
('comparing' = hasEqualElements: hash)
('converting' @ asByteArray asColorArray asNewArray concatenation isSequenceable printStringWithNewline readStream reverse reversed writeStream)
('copying' , copyAfter: copyAfterLast: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withNextDo: withPreviousDo:)
('private' asDigitsAt:in:do: checkedAt: combinationsAt:in:after:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -= derivative incrementFraction integral)
('assertions' assertIsInBounds:)
('as yet unclassified' allButLastDo:)
!


!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('converting' aOrAnPrefix adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: afterBlanksEndsWith: asCamelCase asDate asFileName asHex asIdentifier: asInteger asLowercase asMonth asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector firstNoBlankIndex initialIntegerOrNil keywords lastNoBlankIndex prefixAndSuffix: prefixedWithAOrAn romanNumber squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withLineEndings: withNewLines withoutEnclosing: withoutLeadingBlanks withoutPrefix: withoutSuffix: withoutTrailingBlanks)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral print printOn: storeOn:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer isAlphaNumeric isRemote isString)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('user interface' edit editLabel:)
('formatting' format:)
('uCompletion' separateKeywords)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-actions' fileContents fileContents:)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('text conversion helpers' bold italic struck sub super under)
!


!Symbol class reorganize!
('access' allSymbols findInterned: selectorsContaining: selectorsMatching: thatStarts:skipping: thatStartsCaseSensitive:)
('class initialization' allSymbolTablesDo: allSymbolTablesDo:after: compactSymbolTable initialize)
('instance creation' fromCollectionOfStrings: intern: internCharacter: lookup: newFrom: readFrom:)
('private' rehash shutDown)
('services' hasInterned:and: hasInterned:ifTrue: hasInternedAndImplemented: hasInternedAndImplementedOrReferenced: possibleSelectorsFor:)
!


!CompiledMethod reorganize!
('accessing' bytecodeSetName clearFlag defaultSelector encoderClass endPC flag flushCache frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: trailer)
('comparing' = equivalentTo: hash)
('testing' hasArgumentOrTemporary: hasNewPropertyFormat hasReportableSlip is: isInstalled isQuick isReturnField isReturnSelf isReturnSpecial isTestMethod readsInstanceVariable: referencesParameterAt: sendsOrRefersTo: usesClosureBytecodes writesInstanceVariable:)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('literals' allLiterals hasLiteral: hasLiteralSuchThat: hasLiteralThorough: header headerDescription indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: refersToLiteral: sendsSelector: xtraBindings)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: scanLongLoad: scanLongStore: scanVeryLongLoad:offset: scanVeryLongStore:offset: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreambleFrom:at: getSource getSourceFor:in: getSourceFromFile linesOfCode putSource:fromParseNode:class:category:inFile:priorMethod: putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceClass sourceCode sourceFileStream sourcePointer)
('file in/out' objectForDataStream: readDataFrom:size: storeDataOn: zapSourcePointer)
('evaluating' valueWithReceiver:arguments:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile methodNode parserClass)
('breakpoints' hasBreakpoint)
('code analysis' scanForEqSmallConstant)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('private' penultimateLiteral penultimateLiteral:)
('accessing-pragmas & properties' pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo: withPropertiesDo:ifSelector:)
('closures' containsBlockClosures embeddedBlockClosures)
('tracing' outboundPointersDo:)
('converting' asString)
('time stamp' author dateAndTime dateMethodLastSubmitted dateSortingValue timeStamp)
('inspecting' explorerContents inspectorClass)
('organization' category)
('initialization' copyWithTrailerBytes: needsFrameSize:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('*LiveTyping' allRawTypesUnload: caretAutoCompleterDocumentation doesOnlySurelySend: initializeTypeInformation initializeTypeInformationUsing: isQuickReturnSelf liveTypingAutoCompleterDocumentation methodVariablesTypes parameterTypeAutoCompleterDocumentationOf:using: printTypeInfoOn: returnRawTypes returnTypeAutoCompleterDocumentation returnTypeInfo returnTypes typeInfoOfVariableNamed: typeInfoOfVariableNamed:ifAbsent: typedMessageSendersOf: typesOfInstanceVariableNamed: typesOfInstanceVariableNamed:ifAbsent: typesOfVariableNamed: typesOfVariableNamed:ifAbsent: variablesRawTypes)
('*DenotativeObject' printTheNonMetaClassAndSelectorOn:)
!


!MethodDictionary reorganize!
('accessing' add: associationAt:ifAbsent: at:ifAbsent: at:put: keyAtIdentityValue:ifAbsent: keyAtValue:ifAbsent:)
('removing' removeAll removeKey:ifAbsent:)
('enumeration' associationsDo: keysAndValuesDo: keysDo: valuesDo: valuesSelect:)
('private' fixCollisionsFrom: grow keyAt: methodArray postCopy rehash rehashWithoutBecome removeDangerouslyKey:ifAbsent: scanFor: swap:with:)
!


!Parser class reorganize!
('class initialization' initialize)
('accessing' chainExamplesDoubleColon primitivePragmaSelectors)
('services' methodHeaderLengthFrom: selectorFrom:)
('parsing' parse:class: parse:class:noPattern:)
!


!ParseNode reorganize!
('visiting' accept: nodesDo:)
('tiles' currentValueIn:)
('converting' asReturnNode)
('testing' assignmentCheck:at: canCascade ensureCanCascade: isArg isAssignmentNode isBlockNode isComplex isConstantNumber isInstanceVariableNode isJust: isLiteralNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelfPseudoVariable isSpecialConstant isTemp isTempOrArg isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine toDoIncrement:)
('comment' comment comment:)
('code generation' pc pc:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('encoding' encodeSelector:)
('private' ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('*LiveTyping' identifierPrintString isLiteralVariableNode nodeTypeAndIdentifierPrintString nodeTypePrintString typesIn:addingIncompleteTypeInfoTo:)
('*Cuis-University-Model' referencesSelf sendsMessageToSelf: sendsRecursivelyToSelf)
!


!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:in: bindTemp:range: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: globalSourceRanges messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap sourceRangeFor: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
('*LiveTyping' topParseNodeIncluding:ifAbsent: typedMessageSendSelectorKeywordPositionsOf:ifAbsent:)
!


!EncoderForV3PlusClosures reorganize!
('bytecode generation' genBranchPopFalse: genBranchPopTrue: genCallPrimitive: genDup genJump: genJumpLong: genPop genPushClosureCopyNumCopiedValues:numArgs:jumpSize: genPushConsArray: genPushInstVar: genPushInstVarLong: genPushLiteral: genPushLiteralVar: genPushNClosureTemps: genPushNewArray: genPushReceiver genPushRemoteTemp:inVectorAt: genPushSpecialLiteral: genPushTemp: genPushTempLong: genPushThisContext genReturnReceiver genReturnSpecialLiteral: genReturnTop genReturnTopToCaller genSend:numArgs: genSendSpecial:numArgs: genSendSuper:numArgs: genStoreInstVar: genStoreInstVarLong: genStoreLiteralVar: genStorePopInstVar: genStorePopInstVarLong: genStorePopLiteralVar: genStorePopRemoteTemp:inVectorAt: genStorePopTemp: genStoreRemoteTemp:inVectorAt: genStoreTemp: maxIndexableLiterals)
('testing' hasLocalNamed: supportsClosureOpcodes)
!


!LeafNode reorganize!
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation' code emitCodeForLoad:forValue:encoder: reserve: sizeCodeForLoad:forValue:)
('private' code:type:)
('code generation (new scheme)' emitCodeForEffect:encoder: sizeCodeForEffect: sizeCodeForValue:)
('accessing' key key:)
('initialization' key:code: key:index:type: name:key:code:)
!


!VariableNode reorganize!
('visiting' accept:)
('testing' assignmentCheck:at: index isSelfPseudoVariable isUndeclared isVariableNode isVariableReference varNodeType)
('code generation (closures)' beingAssignedToAnalyseTempsWithin:rootNode:assignmentPools:)
('tiles' currentValueIn: variableGetterBlockIn:)
('code generation (new scheme)' emitCodeForReturn:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('code generation' emitCodeForLoad:forValue:encoder: fieldOffset sizeCodeForReturn:)
('accessing' name)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' asStorableNode: name: name:index:type: name:key:code: name:key:index:type: nameAndKey:)
('*LiveTyping' isFalsePseudoVariable isNilPseudoVariable isSuperPseudoVariable isThisContextPseudoVariable isTruePseudoVariable nodeTypePrintString typesIn:addingIncompleteTypeInfoTo:)
('*Cuis-University-Model' referencesSelf)
!


!InstanceVariableNode reorganize!
('visiting' accept:)
('code generation (new scheme)' emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('initialization' name:index:)
('testing' isInstanceVariableNode)
('*LiveTyping' nodeTypePrintString typesIn:addingIncompleteTypeInfoTo:)
!


!TempVariableNode reorganize!
('visiting' accept:)
('code generation (closures)' addReadWithin:at: addWriteWithin:at: analyseClosure: analyseTempsWithin:rootNode:assignmentPools: beingAssignedToAnalyseTempsWithin:rootNode:assignmentPools: definingScope definingScope: index: isDefinedWithinBlockExtent: isIndirectTempVector isReferencedWithinBlockExtent: referenceScopesAndIndicesDo:)
('testing' assignmentCheck:at: beBlockArg beMethodArg isArg isBlockArg isDeclaredAtMethodLevel isMethodArg isRemote isTemp isTempOrArg isUndefTemp isUnusedTemp remoteNode scope)
('printing' printDefinitionForClosureAnalysisOn: printWithClosureAnalysisOn:indent:)
('decompiler' remoteNode:)
('code generation' emitCodeForLoad:forValue:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForLoad:forValue: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('initialization' name:index:type:scope: nowHasDef nowHasRef scope:)
('*LiveTyping' nodeTypePrintString typesIn:addingIncompleteTypeInfoTo:)
!


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' keywordAndParameterPositionAt:encodedWith: keywordPositionAt: keywordRanges)
('testing' canCascade ensureCanCascade: isComplex isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf toDoIncrement: toDoLimit:)
('cascading' cascadeReceiver receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: ifNilReceiver pvtCheckForPvtSelector:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transform: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
('*LiveTyping' identifierPrintString nodeTypePrintString receiverOrCascadeReceiver typesIn:addingIncompleteTypeInfoTo:)
('*Cuis-University-Model' sendsMessageToSelf:)
('accessing' selectorSymbol)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' addPositionTo:of:inside: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('*Cuis-University-Model' sendsRecursivelyToSelf)
('*LiveTyping' identifierPrintString nodeTypePrintString topBlockTempsMap topParseNodeIncluding:ifAbsent: typedMessageSendSelectorKeywordPositionsOf:ifAbsent: typesIn:addingIncompleteTypeInfoTo: withTopParseNodeIncluding:do:ifAbsent:)
('testing' hasArgumentOrTemporary: hasLocalNamed:)
!


!ReturnNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('converting' asReturnNode)
('code generation' code)
('code generation (new scheme)' emitCodeForReturn:encoder: emitCodeForValue:encoder: sizeCodeForReturn: sizeCodeForValue:)
('printing' expr printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' isImplicitSelfReturnIn: isReturn isReturnSelf isSpecialConstant isVariableReference)
('initialization' expr: expr:encoder:sourceRange:)
('*LiveTyping' identifierPrintString typesIn:addingIncompleteTypeInfoTo:)
!


!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: setSearch: text)
('accessing-selection' hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selection selectionAsStream selectionIntervalsDo: startBlock startIndex stopBlock stopIndex)
('as yet unclassified' characterBlockForIndex: totalTextHeight visibleHeight)
('attributes' changeEmphasisOrAlignment clearFont offerColorMenu offerDefaultFontMenu offerFontMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: tabKey: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model:)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: selectFrom:to: selectInterval: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('scrolling' scrollBy:)
('typing support' addString: backTo: dispatchOn: processKeyStroke: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete: selectAll)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: storeSelectionInComposition unapplyAttributes: zapMultiSelection)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
!


!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier selectWord)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('contextual rename' contextualRename contextualRename: contextualRenameOf:in: ifRenameCanBeAppliedDo: rename:in: renameClassOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameTemporary: withMethodNodeAndClassDo:ifErrorsParsing:)
('*LiveTyping' actualImplementorsOfIt actualImplementorsOfIt: actualSendersOfIt actualSendersOfIt: showSelectionTypeInfo showSelectionTypeInfo: showTypeInfoOf:in:definedAt:)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumn buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCodePane buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' systemCatListKey:from: systemCatSingletonKey:from:)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('refactorings' addInstVar addParameter classRefactoringMenu extractMethodToMethodObject messageRefactoringMenu openClassRefactoringMenu openMessageRefactoringMenu pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('*DenotativeObject-Tools' classNameRequester findClass findClassDialogTitle)
!


!MessageSetWindow class reorganize!
('instance creation')
('opening' openFullProtocolForClass: openMessageList:label: openMessageList:label:autoSelect: openSubProtocolForClass:)
!
