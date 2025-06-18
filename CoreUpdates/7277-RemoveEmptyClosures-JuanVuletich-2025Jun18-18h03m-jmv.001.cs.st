'From Cuis7.5 [latest update: #7272] on 18 June 2025 at 6:17:39 pm'!

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 6/18/2025 18:16:54'!
withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock

	| definingSubclasses |

	definingSubclasses := self allSubclasses select: [ :aSubclass |
		aSubclass definesInstanceVariableNamed: aName ].

	^definingSubclasses isEmpty
		ifTrue: [ noneBlock value ]
		ifFalse: [ foundBlock value: definingSubclasses ]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 6/17/2025 18:58:06'!
debugIt

	self lineSelectAndEmptyCheck: [^self].
	self 
		afterCompiling: self selection string
		do: [ :compiler :method :receiver :context |
			(method notNil and: [method isQuick not]) ifTrue: [
				self debug: method receiver: receiver in: context ]]
		ifFail: nil.! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 6/17/2025 18:54:38'!
selectNodeUnderCursorForInspectionIn: aMethodNode 
	
	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self selectForInspection: nodeUnderCursor in: aMethodNode ]
		ifAbsent: nil! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/17/2025 18:56:27'!
findMethodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!!"
	| searchString selection scopeClass scopeClassName includeSubclasses includeSuperclasses labelList package |

	true ifTrue: [
		^FindInSourceDialog openOn: self ].

	searchString := StringRequestMorph
		request: 'Enter search text:'
		initialAnswer: self selectedString
		orCancel: [^self].
	
	scopeClass := self selectedClass.
	scopeClassName := scopeClass name asString.
	includeSubclasses := false.
	includeSuperclasses := false.

	labelList := {
		'This method'. 
		'All methods in image'. } asOrderedCollection.
	(scopeClass notNil)
		ifTrue: [
			labelList  addAll: {
				'Class ', scopeClassName printString.
				'Class ', scopeClassName printString, ' and subclasses'.
				'Class ', scopeClassName printString, ', subclasses, and superclasses'.}.
			package := self codeProvider isEditingExistingMethod
				ifTrue: [CodePackage packageOfMethod: self codeProvider currentCompiledMethod asMethodReference ifNone: nil]
				ifFalse: [CodePackage packageOfClass: scopeClass ifNone: nil].
			package ifNotNil: [labelList add: 'In my package: ', package packageName]].
				
	selection := (SelectionMenu labelList: labelList selections: nil) startUpWithCaption: 'Search Scope'.

	(selection = 0) ifTrue:[^self].
	(selection = 1) "This method"
		ifTrue:[^self find: searchString].
	(selection = 2) "All methods in image"
		ifTrue:[scopeClass := nil].
	"(selection = 3) ifTrue:[]. ""Class only - nothing to do"
	(selection = 4) "Class and subclasses"
		ifTrue:[includeSubclasses := true].
	(selection = 5) "Class, subclasses, and superclasses"
		ifTrue:[
			includeSubclasses := true.
			includeSuperclasses := true].
	(selection = 6) "In my package if any "
		ifTrue: [^ Smalltalk browseMethodsWithSourceString: searchString in: package].
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses! !


!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'jmv 6/17/2025 18:54:45'!
initializeCategoriesWhenMeta: classCategories

	categories add: Categorizer instanceCreation.
	classCategories remove: Categorizer instanceCreation ifAbsent: nil.
	reject add: Categorizer instanceCreation! !


!TrieNode methodsFor: 'private' stamp: 'jmv 6/17/2025 18:56:33'!
removeChildFor: aString characterIndex: i
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil."
	| nextSegmentStart nextSegmentFirst |

	nextSegmentStart := i + segmentSize.
	nextSegmentFirst := nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	"Just one child, if adding, convert references to a collection"
	childrenFirstChars isCollection ifFalse: [
		childrenFirstChars = nextSegmentFirst
			ifTrue: [ childrenFirstChars := nil. children := nil ].
		^self ].
	
	"Already more than one child."
	childrenFirstChars
		findBinaryIndex: [ :each |
			each = nextSegmentFirst ifTrue: [ 0 ]
				ifFalse: [each > nextSegmentFirst ifTrue: [-1] ifFalse: [1]]]
		do: [ :found |
			childrenFirstChars size = 2
				ifTrue: [		"Two children: remove the arrays"
					childrenFirstChars := childrenFirstChars at: 3-found.
					children := children at: 3-found ]
				ifFalse: [	"More than two children. condense the arrays"
					childrenFirstChars := (childrenFirstChars copyFrom: 1 to: found-1), 
						(childrenFirstChars copyFrom: found+1 to: childrenFirstChars size).
					children := (children copyFrom: 1 to: found-1), (children copyFrom: found+1 to: children size) ]]
		ifNone: nil.! !


!MethodReference methodsFor: 'queries' stamp: 'jmv 6/17/2025 18:06:28'!
actualClass 

	^self actualClassIfAbsent: nil! !


!ChangeSet methodsFor: 'change logging' stamp: 'jmv 6/17/2025 18:07:22'!
noteRenameClass: class as: newName 
	"Include indication that a class has been renamed."

	| recorder |
	(recorder := self changeRecorderFor: class)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.
		
	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: class name.
	self noteClassStructure: class.

	recorder := changeRecords at: class class name ifAbsent: nil.
	recorder ifNotNil: [
		changeRecords at: (newName, ' class') put: recorder.
		changeRecords removeKey: class class name.
		recorder noteNewName: newName , ' class' ].

	self hasUnsavedChanges: true! !


!PopUpMenu methodsFor: 'accessing' stamp: 'jmv 6/17/2025 18:06:34'!
iconAt: aPosition

	^icons at: aPosition ifAbsent: nil! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'jmv 6/17/2025 18:56:40'!
positionToInsertNewTemporaryVariableAssignment

	| siblings |
	
	siblings := self siblingStatementsOfTemporaryAssignment.
	siblings do: [ :sibling |
		(completeSourceRanges at: sibling) 
			detect: [ :range | range includesAllOf: intervalToExtract ]
			ifFound: [ :range | ^range first ]
			ifNone: nil].
		
	self shouldNotHappenBecause: 'sibling should be part of completeSourceRanges'! !


!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'FB 5/2/2021 18:20:30'!
selectCompleteRangesOfAssignmentsToTemporaryFrom: aParseNode thatMatch: aConditionBlock

	| assignments |

	assignments := Set new.
	aParseNode nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode)
			ifTrue: [
				| nodeRanges satisfyingRanges |
				nodeRanges := methodNode completeSourceRanges at: aNode ifAbsent: nil.
				satisfyingRanges := (nodeRanges select: aConditionBlock)
					collect:[:range | range asSourceCodeInterval
						expandUntilStartOfNextStatementOn: methodNode sourceText].
				assignments addAll: (satisfyingRanges)]
	].

	^assignments






! !


!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/19/2025 09:11:50'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor

	| potentialImplementor |

	"Phil B. requested to avoid refactoring OMeta2 classes, so right now
	it avoids implementors and senders whose compilerClass is not register
	as allowed compiler - Hernan"
	(self canRefactor: aPotentialClassToRefactor) ifFalse: [ ^self ].

	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: anOldSelector ifAbsent: nil.
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].

	(aPotentialClassToRefactor whichSelectorsReferTo: anOldSelector) do: [ :aSelector |
		self addTo: senders referencesTo: anOldSelector from: aSelector in: aPotentialClassToRefactor ]! !


!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'jmv 6/17/2025 18:56:46'!
warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: implementorsCollection

	implementorsCollection do: [:anImplementor |
		anImplementor methodClass
			withSuperclassThatIncludesSelector: aNewSelector
			do: [ :aSuperclass | self warnImplementionOf: aNewSelector in: anImplementor methodClass willOverrideImplementationIn: aSuperclass ]
			ifNone: nil]! !


!AddParameter class methodsFor: 'pre-conditions' stamp: 'jmv 6/17/2025 18:50:11'!
assertNewParameterValueCompiles: trimmedNewParameterValue

	^ [ Parser new parse: trimmedNewParameterValue readStream class: self noPattern: true notifying: nil ifFail: nil ]
		on: SyntaxErrorNotification 
		do: [ :error | self signalNewParameterValueDoesNotCompile ].! !

!AddParameter class methodsFor: 'exceptions' stamp: 'jmv 6/17/2025 18:11:39'!
classesDefiningInstanceVariable: aName inAny: implementors
 
	^ implementors 
		inject: Set new
		into: [ :classesDefiningInstVar :implementor | 
			(implementor methodClass whichClassDefinesInstanceVariable: aName ifNone: nil) ifNotNil: [ :classDefiningInstVar |
				classesDefiningInstVar add: classDefiningInstVar ].
			classesDefiningInstVar ]
! !


!TransferMethod class methodsFor: 'warnings' stamp: 'jmv 6/17/2025 18:54:58'!
warnIfMethodIsImplementedInSuperClass: aMethodToTransfer

	aMethodToTransfer methodClass superclass
		compiledMethodAt: aMethodToTransfer selector
		ifPresent: [ :superclassMethod | (self shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superclassMethod)
			ifFalse: [ self refactoringWarning: self methodImplementedInSuperclassWarningMessage ]]
		ifAbsent: nil! !


!RefactoringApplier class methodsFor: 'appliers - private' stamp: 'jmv 6/17/2025 18:55:03'!
resetApplierAt: anApplierId

	Appliers removeKey: anApplierId ifAbsent: nil! !


!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'jmv 6/17/2025 18:58:12'!
assertIsNotAlreadyDefined
	
	^ classToAddInstVar 
		withClassesThatDefineInHierarchyInstanceVariable: instVarName 
		do: [ :definingClasses | self signalAlreadyDefinedInAll: definingClasses ]
		ifNone: nil.! !

