'From Cuis 5.0 [latest update: #4384] on 5 October 2020 at 12:24:21 pm'!
!classDefinition: #SHST80RangeType category: #'Tools-Syntax Highlighting'!
Object subclass: #SHST80RangeType
	instanceVariableNames: 'sourceCode messageRange receiverRange messageRangeType receiverRangeType classOrMetaClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
!classDefinition: 'SHST80RangeType class' category: #'Tools-Syntax Highlighting'!
SHST80RangeType class
	instanceVariableNames: ''!

!Class methodsFor: 'class variables' stamp: 'HAW 10/5/2020 08:09:51'!
classVarBindingOf: varNameSymbol

	^self classPool bindingOf: varNameSymbol! !

!Class methodsFor: 'class variables' stamp: 'HAW 10/5/2020 08:09:57'!
classVarValueOf: varNameSymbol

	^(self classPool bindingOf: varNameSymbol) ifNotNil: [:binding | binding value ]! !


!SHRange methodsFor: 'printing' stamp: 'HAW 10/4/2020 19:12:52'!
printOn: aStream

	aStream 
		nextPutAll: type;
		space;
		nextPut: $(;
		print: start;
		nextPutAll: ' to: ';
		print: end;
		nextPut: $)! !


!SHST80RangeType methodsFor: 'initialize' stamp: 'HAW 10/5/2020 09:19:59'!
initializeFor: aSourceCode in: aClassOrMetaclass

	sourceCode := aSourceCode.
	classOrMetaClass := aClassOrMetaclass ! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 08:36:53'!
doesReceiverRespondsToMessageIn: aReceiver 

	^aReceiver respondsTo: self messageName! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:22:07'!
messageName

	^ (self sourceCodeIn: messageRange) asSymbol! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 08:35:06'!
messageUndefinedType
	
	messageRangeType = #unary ifTrue: [ ^#undefinedUnary ].
	messageRangeType = #binary ifTrue: [ ^#undefinedBinary ].
	
	^#invalid
	! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:22:07'!
receiverAsNumber

	^ (self sourceCodeIn: receiverRange) asNumber! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:22:07'!
receiverAsSymbol

	^ (self sourceCodeIn: receiverRange) asSymbol! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:22:07'!
sourceCodeIn: aRange

	^ sourceCode copyFrom: aRange start to: aRange end! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:55:09'!
typeWhenSendTo: receiver

	^(self doesReceiverRespondsToMessageIn: receiver)
		ifTrue: [ messageRangeType ]
		ifFalse: [ self messageUndefinedType ]
		
	
! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:55:09'!
typeWhenSendToClassVar

	| classVarValue |
	
	^ classOrMetaClass 
		ifNil: [ messageRangeType ]
		ifNotNil: [ 
			classVarValue := classOrMetaClass theNonMetaClass classVarValueOf: (self sourceCodeIn: receiverRange).
			self typeWhenSendTo: classVarValue ]! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:55:09'!
typeWhenSendToGlobal

	^ Smalltalk 
		at: self receiverAsSymbol 
		ifPresent: [ :globalValue | self typeWhenSendTo: globalValue ]
		ifAbsent: [ messageRangeType ]! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:53:59'!
typeWhenSendToInstanceOf: aClass

	^ (aClass canUnderstand: self messageName)
		ifTrue: [ messageRangeType ]
		ifFalse: [ self messageUndefinedType ]! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:55:09'!
typeWhenSendToNumber

	^self typeWhenSendTo: self receiverAsNumber 
		! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:54:17'!
typeWhenSendToSelf

	^ classOrMetaClass 
		ifNil: [ messageRangeType ]
		ifNotNil: [ self typeWhenSendToInstanceOf: classOrMetaClass ]! !

!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 10/5/2020 09:54:28'!
typeWhenSendToSuper

	^ classOrMetaClass 
		ifNil: [ messageRangeType ]
		ifNotNil: [ 
			classOrMetaClass theNonMetaClass superclass isNil
				ifTrue: [ self messageUndefinedType ]
				ifFalse: [ self typeWhenSendToInstanceOf: classOrMetaClass superclass]]! !

!SHST80RangeType methodsFor: 'range type' stamp: 'HAW 10/5/2020 09:35:07'!
lastRange: aRange

	receiverRange := aRange ! !

!SHST80RangeType methodsFor: 'range type' stamp: 'HAW 10/5/2020 09:59:50'!
ofCurrentRangeOrMessageSendIn: aPotentialMessageRange 

	| potentialMessageRangeType |
	
	potentialMessageRangeType := aPotentialMessageRange rangeType.
	
	(#(unary binary) includes: potentialMessageRangeType) ifFalse: [ ^potentialMessageRangeType ].
	receiverRange ifNil: [ ^potentialMessageRangeType ].

	messageRange := aPotentialMessageRange.
	messageRangeType := potentialMessageRangeType.
	receiverRangeType := receiverRange rangeType.
		
	receiverRangeType = #number ifTrue: [ ^self typeWhenSendToNumber ].
	receiverRangeType = #string ifTrue: [ ^self typeWhenSendToInstanceOf: String ].
	receiverRangeType = #symbol ifTrue: [ ^self typeWhenSendToInstanceOf: Symbol ].
	receiverRangeType = #arrayEnd ifTrue: [ ^self typeWhenSendToInstanceOf: Array ].
	receiverRangeType = #rightBrace ifTrue: [ ^self typeWhenSendToInstanceOf: Array ].
	receiverRangeType = #blockEnd ifTrue: [ ^self typeWhenSendToInstanceOf: BlockClosure ].
	receiverRangeType = #character ifTrue: [ ^self typeWhenSendToInstanceOf: Character ].
	receiverRangeType = #nil ifTrue: [ ^self typeWhenSendToInstanceOf: nil class ].
	receiverRangeType = #true ifTrue: [ ^self typeWhenSendToInstanceOf: true class ].
	receiverRangeType = #false ifTrue: [ ^self typeWhenSendToInstanceOf: false class ].
	receiverRangeType = #self ifTrue: [^self typeWhenSendToSelf ].
	receiverRangeType = #super ifTrue: [^self typeWhenSendToSuper ].		
	receiverRangeType = #globalVar ifTrue: [^self typeWhenSendToGlobal ].
	receiverRangeType = #classVar ifTrue: [^self typeWhenSendToClassVar ].
	
	^messageRangeType ! !


!SHST80RangeType class methodsFor: 'instance creation' stamp: 'HAW 10/5/2020 09:20:06'!
for: aSourceCode in: aClassOrMetaclass

	^self new initializeFor: aSourceCode in: aClassOrMetaclass! !


!SHTextStylerST80 methodsFor: 'private - show subscript' stamp: 'HAW 10/4/2020 19:23:15'!
formatAsSubscript: range

	^ formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
		formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ]! !

!SHTextStylerST80 methodsFor: 'private - show subscript' stamp: 'HAW 10/4/2020 19:22:30'!
hasToShowSubscriptOf: range

	^ #(instVar classVar globalVar workspaceVar poolConstant
		patternArg methodArg patternTempVar tempVar		
		blockPatternArg blockArg blockPatternTempVar blockTempVar 
		incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType! !

!SHTextStylerST80 methodsFor: 'private - show subscript' stamp: 'HAW 10/4/2020 19:22:08'!
showAsSubscriptIfAppropriate: range

	classOrMetaClass ifNotNil: [
		classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript ifTrue: [
			(self hasToShowSubscriptOf: range ) ifTrue: [ self formatAsSubscript: range ]]]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'HAW 10/5/2020 10:00:44'!
applySmalltalkStylingIn: range using: st80RangeType
 
	| rangeType |
	
	rangeType := st80RangeType ofCurrentRangeOrMessageSendIn: range.
	(self attributesFor: rangeType) ifNotNil: [ :attributes |
		attributes do: [ :each | formattedText addAttribute: each from: range start to: range end ]].
! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'HAW 10/5/2020 09:17:13'!
optimizeForMutationSpeed: ranges

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel!! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ ranges size < 50 ]) ifFalse: [
			formattedText _ formattedText optimizedForMutationSpeed.
			textModel basicActualContents: formattedText ]].

! !


!Class methodsFor: 'compiling' stamp: 'HAW 10/5/2020 08:08:59'!
localBindingOf: varNameSymbol
	"Answer the binding of some variable resolved in the scope of the receiver."

	"First look in classVar dictionary."
	(self classVarBindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		(pool bindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].
	].

	"Finally look higher up the superclass chain and fail at the end."
	^superclass ifNotNil: [ superclass localBindingOf: varNameSymbol ]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'HAW 10/5/2020 09:59:08'!
setAttributesFromRanges: ranges

	| st80RangeType |
	
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	self optimizeForMutationSpeed: ranges.
	st80RangeType := SHST80RangeType for: formattedText string in: classOrMetaClass.
	
	ranges do: [ :currentRange |
		self 
			applySmalltalkStylingIn: currentRange using: st80RangeType;
			showAsSubscriptIfAppropriate: currentRange.			
		st80RangeType lastRange: currentRange ]! !


!Class reorganize!
('accessing' name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarBindingOf: classVarNames classVarValueOf: ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools classPool classPool: classPoolFrom: removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('private' becomeCompact becomeCompactSimplyAt: becomeUncompact setName: spaceUsed)
('initialization' declare: sharing: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('tdd' testCaseClass)
!


!SHRange reorganize!
('accessing' end end: length rangeType start start: start:end:type: type:)
('printing' printOn:)
!


!SHST80RangeType reorganize!
('initialize' initializeFor:in:)
('range type - private' doesReceiverRespondsToMessageIn: messageName messageUndefinedType receiverAsNumber receiverAsSymbol sourceCodeIn: typeWhenSendTo: typeWhenSendToClassVar typeWhenSendToGlobal typeWhenSendToInstanceOf: typeWhenSendToNumber typeWhenSendToSelf typeWhenSendToSuper)
('range type' lastRange: ofCurrentRangeOrMessageSendIn:)
!


!SHST80RangeType class reorganize!
('instance creation' for:in:)
!


!SHTextStylerST80 reorganize!
('styling' formatWorkspace: styleWorkspaceFrom:to:)
('private - show subscript' formatAsSubscript: hasToShowSubscriptOf: showAsSubscriptIfAppropriate:)
('private' applySmalltalkStylingIn:using: attributesFor: convertAssignmentsToAnsi convertAssignmentsToLeftArrow optimizeForMutationSpeed: parseSetWorkspace: privateFormatAndConvert privateStyle replaceStringForRangesWithType:with: setAttributesFromRanges:)
('accessing' classOrMetaClass: disableFormatAndConvert workspace:)
('initialization' initialize)
!

