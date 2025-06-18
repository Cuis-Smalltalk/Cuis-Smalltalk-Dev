'From Cuis7.5 [latest update: #7272] on 18 June 2025 at 5:36:01 pm'!

!Parser methodsFor: 'expression types' stamp: 'jmv 6/17/2025 18:01:15'!
variable
	
	^self advanceWithRangeDo: [ :variableName :range | | varName result rightRange |
		varName := variableName.
		
		"See ParserTest>>#testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat
		There is a very difficult bug to fix. It happends when the source code ends with a return of a variable of 
		one char an no other char after that, for example: '^a' 
		In that case the range includes the ^ that is incorrect and makes the rename temporary fail.
		I do this fix becuase changing how the range is calculated is almost imposible due to the coupling 
		and complexity of the parser. This change applies only to variables and therefore it assures no
		unexpected behavior. I'm not cheching for size = 1 because it is redundant - Hernan"
		rightRange := varName size = range size
			ifTrue: [ range ]
			ifFalse: [ range last - varName size + 1 to: range last ].
		
		[result := encoder encodeVariable: varName sourceRange: rightRange ifUnknown: nil.
		result ifNil: [
			result := (UndeclaredVariableReference new)
				parser: self;
				varName: varName;
				varStart: rightRange first;
				varEnd: rightRange last;
				signal ].
		result isString ] whileTrue: [ varName := result].
		encoder addMultiRange: rightRange for: result ].
	! !

!Parser methodsFor: 'error correction' stamp: 'jmv 6/17/2025 18:00:35'!
declarationRangesForTemps: someTempNodes
	
	^someTempNodes collect: [ :temporaryNode | 
		(encoder rangeForNode: temporaryNode ifAbsent: nil) first ].! !


!Encoder methodsFor: 'results' stamp: 'jmv 6/17/2025 18:02:03'!
associationForClass
	| assoc |
	assoc := Smalltalk associationAt: class name ifAbsent: nil.
	^assoc value == class
		ifTrue: [ assoc ]
		ifFalse: [ Association new value: class ]! !

!Encoder methodsFor: 'temps' stamp: 'eem 12/1/2008 12:07'!
fixTemp: name
	| node |
	node := scopeTable at: name ifAbsent: nil.
	node class ~~ TempVariableNode ifTrue:
		[self error: 'can only fix a floating temp var'].
	node index: nTemps.
	nTemps := nTemps + 1.
	^node! !

!Encoder methodsFor: 'temps' stamp: 'eem 7/13/2007 14:13'!
floatTemp: node
	(node ~~ (scopeTable at: node name ifAbsent: nil)
	or: [node class ~~ TempVariableNode
	or: [node code ~= (node code: nTemps - 1 type: LdTempType)]]) ifTrue:
		[self error: 'can only float the last allocated temp var'].
	nTemps := nTemps - 1! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 6/17/2025 18:51:01'!
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries properties |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	properties := method properties copy.
	(properties at: #onceCache ifAbsent: nil) ifNotNil:
		[:onceCache|
		 properties := properties copyWithout: (Association
													key: #onceCache
													value: onceCache)].
	blockNode := MethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (method encoderClass new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: properties.
	blockNode properties method: blockNode.
	^blockNode! !


!TemporaryVariableDeclarationRemover methodsFor: 'source text generation' stamp: 'EB 5/28/2019 21:22:07'!
sourceTextWithoutTemporaryDeclaration

	| temporaryVariablePositions variableDeclarationPosition | 

	temporaryVariablePositions := methodNode positionsForTemporaryVariable: temporaryVariable ifAbsent: nil.
	variableDeclarationPosition := {temporaryVariablePositions first}.
	^methodNode sourceText copyReplacing: variableDeclarationPosition with: ''! !


!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 6/17/2025 18:51:13'!
familyNamed: aString
	"
	FontFamily familyNamed: FontFamily defaultFamilyName
	"
	^AvailableFamilies at: aString ifAbsent: nil.! !


!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 6/17/2025 18:51:21'!
mutateInstances: oldClass to: newClass
	"Mutate the old class and subclasses into newClass and subclasses.
	Note: This method is slightly different from: #mutate:toSuper: since
	here we are at the root of reshaping and have two distinct roots."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do: [ :oldSubclass | | newSubclass |
		newSubclass := newClass subclasses detect: [ :c | c name = oldSubclass name ]. 
		self mutateInstances: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do: [ :oldSubclass | | newSubclass |
		oldSubclass ifNotNil: [
			newSubclass := newClass subclasses detect: [ :c | c name = oldSubclass name ] ifNone: nil.
			newSubclass ifNotNil: [
				self mutateInstances: oldSubclass to: newSubclass ].
		].
	].
	self update: oldClass to: newClass.
	^newClass! !


!CodeProvider methodsFor: 'categories' stamp: 'jmv 6/17/2025 18:03:13'!
categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	^(MethodCategoriesPrompter 
		staringFrom: aClass 
		rejectingFirst: false) prompt: aPrompt ifNone: nil! !


!Browser methodsFor: 'class list' stamp: 'jmv 6/17/2025 18:03:34'!
classListIndex: anInteger
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList prevSelectedClassName prevSelectedMessage |
	classList := self classList.
	canSelectClass := classList isInBounds: anInteger.
	prevSelectedClassName := selectedClassName.
	selectedClassName := canSelectClass ifTrue: [
		| newClassName |
		newClassName := classList at: anInteger ifAbsent: nil.
		newClassName := newClassName ifNotNil: [newClassName withoutLeadingBlanks asSymbol].
		newClassName ].
	self setClassOrganizer.
	prevSelectedMessage := selectedMessage.
	selectedMessage := nil.

	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent := self class recentClasses.
		className := self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].

	"Clear selectedMessageCategory if there is no match in the new list of categories"
	(self messageCategoryList indexOf: selectedMessageCategory) = 0 ifTrue: [
		selectedMessageCategory := nil].

	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged.
	prevSelectedClassName = selectedClassName ifFalse: [
		"If clicked on already selected class, deselect selector, so class definition is shown"
		self setSelector: prevSelectedMessage ].! !


!Debugger methodsFor: 'code pane' stamp: 'jmv 6/17/2025 18:07:09'!
contentsSelection

	"If we are just creating the method in the debugger, help the user by selecting the method body."
	| sendInterval |
	currentCompiledMethod ifNotNil: [
		((currentCompiledMethod hasLiteral: #shouldBeImplemented) and: [
			(currentCompiledMethod abstractPCForConcretePC: self selectedContext pc) = 1]) ifTrue: [
				sendInterval := (self selectedContext debuggerMap abstractSourceMap at: 2 ifAbsent: nil).
				sendInterval ifNotNil: [ ^ sendInterval first - 5 to: sendInterval last + 1 ]]].

	^self pcRange! !


!Color methodsFor: 'other' stamp: 'jmv 6/17/2025 18:07:16'!
colorName
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color.
	Return nil if named color support is not present"

	Color classPool 
		at: #ColorNamesDict
		ifPresent: [ :dict | ^dict keyAtValue: self ifAbsent: nil].
	^nil! !


!Behavior methodsFor: 'obsolete subclasses' stamp: 'jmv 6/17/2025 18:51:50'!
removeObsoleteSubclass: aClass
	"Remove aClass from the weakly remembered obsolete subclasses."

	ObsoleteSubclasses ifNil: [
		^ self ].
	ObsoleteSubclasses remove: aClass ifAbsent: nil.! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'jmv 6/17/2025 18:04:18'!
trimToMatchExpressionOn: aSourceCode

	| initialChar endingChar |
	
	initialChar := aSourceCode at: start ifAbsent: nil.
	endingChar := aSourceCode at: stop ifAbsent: nil.
	
	^(initialChar == $( and: [ endingChar == $) ])
		ifTrue: [ self trimParentesisOn: aSourceCode ]
		ifFalse: [ self trimNoParentesisStartingWith: initialChar endingWith: endingChar on: aSourceCode]! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'eem 8/13/2014 13:29'!
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) ifFalse:
		[self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header >= 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: nil) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

!CompiledMethod class methodsFor: 'class initialization' stamp: 'eem 8/13/2014 13:29'!
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) ifFalse:
		[self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header < 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: nil) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

!CompiledMethod class methodsFor: 'source code management aux' stamp: 'jmv 6/17/2025 18:09:13'!
overridenMethodReferenceFrom: tokens

	^ self field: #overrides: from: tokens ifAbsentOrNil: nil! !

!CompiledMethod class methodsFor: 'source code management aux' stamp: 'jmv 6/17/2025 18:09:18'!
priorReferenceFrom: tokens

	^ self field: #prior: from: tokens ifAbsentOrNil: nil! !

