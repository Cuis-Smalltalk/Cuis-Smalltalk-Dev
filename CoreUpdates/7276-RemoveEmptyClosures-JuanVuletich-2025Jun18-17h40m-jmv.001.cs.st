'From Cuis7.5 [latest update: #7272] on 18 June 2025 at 6:03:40 pm'!

!EphemeronIdentityDictionary methodsFor: 'finalization' stamp: 'eem 12/18/2024 11:55:20'!
finalizeEphemeron: anEphemeron

	"Remove the ephemeron from myself.  Note that no finalization of the ephemeron's key is done.
	 Here we are using ephemerons to clean up property tables."

	mutex critical:
		[self removeKey: anEphemeron key ifAbsent: nil]! !


!FinalizationRegistry methodsFor: 'finalization' stamp: 'eem 1/20/2025 17:02:57'!
finalizeEphemeron: anEphemeron
	"Finalize the ephemeron's key and remove the ephemeron from myself.
	 N.B. Here we send #finalize since finalization is called for by placing
	 objects in a FinalizationRegistry. i.e. the response to the ephemeron
	 being triggered is a property of the container of the ephemeron, not
	 the ephemeron itself."

	self removeKey: anEphemeron key ifAbsent: nil.
	
	"Usually no executor is needed. The object that is about to be collected, the key needs to be finalized.
	 Some users may prefer to include an executor (an instance of ObjectFinalizer) as the value.
	 N.B. errors here will not stall the finalization process."
	
	(anEphemeron value ifNil: [anEphemeron key]) finalize! !


!UndeclaredVariable methodsFor: 'exception handling' stamp: 'jmv 6/17/2025 18:10:17'!
defaultAction
	
	| labels actions lines caption choice icons |
	
	labels := OrderedCollection new.
	actions := OrderedCollection new.
	lines := OrderedCollection new.
	icons := OrderedCollection new.
	
	self createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines.
	caption := 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice := (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	
	self resume: (actions at: choice ifAbsent: nil).! !


!UnknownSelector methodsFor: 'exception handling' stamp: 'jmv 6/17/2025 18:04:32'!
defaultAction 

	| alternatives labels lines caption choice icons |

	alternatives := Symbol possibleSelectorsFor: name.
	labels := Array streamContents: [:s | s nextPut: name; nextPutAll: alternatives; nextPut: 'cancel'].
	lines := {1. alternatives size + 1}.
	icons := Array new: labels size.
	icons at: 1 put: #acceptIcon.
	icons at: icons size put: #cancelIcon.
	caption := 'Unknown selector, please\confirm, correct, or cancel' withNewLines.
	
	choice := (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	choice = 1 ifTrue: [self resume: name asSymbol].
	choice = labels size ifTrue: [self resume: nil].
	self resume: (alternatives at: choice - 1 ifAbsent: nil)! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 6/17/2025 18:53:42'!
pushReceiverVariable: offset

	| var |
	(var := instVars at: offset + 1 ifAbsent: nil) == nil
		ifTrue:
			["Not set up yet"
			var := constructor codeInst: offset.
			instVars size < (offset + 1) ifTrue: [
				instVars := (Array new: offset + 1)
					replaceFrom: 1 to: instVars size with: instVars; yourself ].
			instVars at: offset + 1 put: var].
	stack addLast: var! !

!Decompiler methodsFor: 'private' stamp: 'eem 4/25/2017 16:07'!
markTemp: tempVarNode asOutOfScope: scopeFlag
	tempVarNode scope: scopeFlag.
	tempReadCounts removeKey: tempVarNode ifAbsent: nil! !


!Process methodsFor: 'changing process state' stamp: 'jar 8/15/2022 12:48:39'!
suspendAndUnblock
	"ar 12/7/2007 17:10:
	 Primitive. Stop the process that the receiver represents in such a way 
	 that it can be restarted at a later time (by sending the receiver the 
	 message resume). If the receiver represents the activeProcess, suspend it. 
	 Otherwise remove the receiver from the list of waiting processes.
	 The return value of this method is the list the receiver was previously on (if any)."

	<primitive: 88 error: ec>
	"ar 12/7/2007 17:10:
	 This is fallback code for VMs which only support the old primitiveSuspend which 
	 would not accept processes that are waiting to be run."
	 ^myList ifNotNil: "this allows us to use suspend multiple times"
		[:oldList|
		 myList := nil.
		 oldList remove: self ifAbsent: nil.
		 oldList]! !


!Morph methodsFor: 'halos and balloon help' stamp: 'KLG 9/19/2022 10:42:35'!
balloonText
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer.
	Allow for dynamic balloon text."

	^ (self valueOfProperty: #balloonText ifAbsent: nil) value.! !


!FindInSourceDialog methodsFor: 'scopes list' stamp: 'jmv 6/17/2025 18:56:03'!
scopesList
	| scopeClassName labelList |
	labelList := {
		'This method or text'.
		'All methods in image'. } asOrderedCollection.
	smalltalkEditor selectedClass ifNotNil:
		[ :scopeClass |
			scopeClassName := scopeClass name asString.
			labelList  addAll: {
				'Class ', scopeClassName printString.
				'Class ', scopeClassName printString, ' and subclasses'.
				'Class ', scopeClassName printString, ', subclasses, and superclasses'.}.
			package := smalltalkEditor selectedMethod
				ifNotNil: [ :scopeMethod |CodePackage packageOfMethod: scopeMethod ifNone: nil]
				ifNil: [CodePackage packageOfClass: scopeClass ifNone: nil].
			package ifNotNil: [labelList add: 'In my package: ', package packageName]].
	^labelList! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 6/17/2025 18:57:38'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self adjustExtentFor: nil.
	
	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	self layoutNeeded: false.! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 6/18/2025 17:49:26'!
request: queryString initialAnswer: defaultAnswer do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	
	^ self
		request: queryString
		centeredAt: self runningWorld activeHand morphPosition
		initialAnswer: defaultAnswer
		validationBlock: [:aString| true]
		acceptBlock: acceptBlock
		cancelBlock: nil! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 6/17/2025 18:57:50'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock
	"Modal. This method returns when an answer is provided."

	^self request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: nil! !


!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'len 6/12/2022 12:54:46'!
acceptDroppingMorph: aMorph event: dropEvent
	| localPosition row dropSelectorArgument dropActionSelector args |
	localPosition := self internalizeFromWorld: dropEvent eventPosition.
	row := self rowAtLocation: localPosition ifNone: nil.
	dropSelectorArgument := aMorph
		valueOfProperty: #dropSelectorArgument
		ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
	dropActionSelector := self valueOfProperty: #dropActionSelector.
	args := dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: dropSelectorArgument ].
	args = 2 ifTrue: [ ^model perform: dropActionSelector with: row with: dropSelectorArgument ].
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'! !

!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 6/17/2025 18:56:15'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	scroller highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: nil]).
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jpb 8/16/2019 17:34:29'!
deleteAction
	"Handles deleting action, which happens when the user presses backspace or delete key within me"
	| deleteActionBlock |
	deleteActionBlock := self valueOfProperty: #deleteAction ifAbsent: nil.
	deleteActionBlock isNil
		ifTrue: [ self flash ]
		ifFalse: [ deleteActionBlock value ].
	^self! !


!PluggableListMorphByItem methodsFor: 'model access' stamp: 'jmv 6/17/2025 18:06:07'!
setSelectionIndex: anInteger
	"Change the model's selected item to be the one at the given index."

	| item |
	setIndexSelector ifNotNil: [
		item := itemList at: anInteger ifAbsent: nil.
		model perform: setIndexSelector with: item.
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		^ true ].
	^false
	! !


!BrowserWindow class methodsFor: 'services' stamp: 'jmv 6/17/2025 18:11:29'!
findClassFrom: potentialClassNames ifFound: aBlock with: pattern

	| exactMatch foundClass index  classNames toMatch |
	pattern isEmpty ifTrue: [^self flash].

	"If the user entered an exact class name (including case), just use it."
	(potentialClassNames includes: pattern) ifTrue: [
		foundClass := Smalltalk at: pattern asSymbol.
		aBlock value: foundClass.
		^self ].

	toMatch := (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	classNames := self classNamesFrom: potentialClassNames with: pattern matching: toMatch.
	classNames isEmpty ifTrue: [^self flash].

	exactMatch := classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.
	index := self indexOfClassFrom: classNames exactMatch: exactMatch.
	index = 0 ifTrue: [^self flash].

	foundClass := Smalltalk at: (classNames at: index) asSymbol.
	aBlock value: foundClass.! !

!BrowserWindow class methodsFor: 'services' stamp: 'jmv 6/17/2025 18:54:10'!
indexOfClassFrom: classNames exactMatch: exactMatch

	| options |
	
	classNames size = 1 ifTrue: [ ^1 ].
	
	options := exactMatch
		ifNil: [ PopUpMenu labelArray: classNames lines: #()]
		ifNotNil: [
			classNames remove: exactMatch ifAbsent: nil.
			classNames addFirst: exactMatch.
			PopUpMenu labelArray: classNames lines: #(1)].
		
	^options startUpMenu! !


!WorldMorph methodsFor: 'stepping' stamp: 'jmv 6/17/2025 18:54:16'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods whose time has come. Purge any morphs that are no longer in this world."

	| timerBefore timerAfter stepMessage |
	timerAfter := nowTime.
	[ stepList notEmpty and: [ (stepMessage := stepList first) scheduledTime <= nowTime ]] 
		whileTrue: [
			(stepMessage receiver shouldGetStepsFrom: self)
				ifFalse: [
					stepList remove: stepMessage ifAbsent: nil]
				ifTrue: [
					timerBefore := timerAfter.
					stepMessage valueAtTime: nowTime.
					timerAfter := Time localMillisecondClock.
					"No step message should take more than 50% CPU. 
					Reschedule it only after its required time is elapsed again."
					stepMessage rescheduleAfter: timerAfter + (timerAfter - timerBefore).
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort ]].! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 6/18/2025 17:51:35'!
rowAtLocation: aPoint ifNone: aNoneBlock

	| potentialRowNumber |
	
	potentialRowNumber := aPoint y // font lineSpacing + 1.
	
	^(listItems isInBounds: potentialRowNumber) 
		ifTrue: [ potentialRowNumber ]
		ifFalse: [ aNoneBlock value ]! !


!HandMorph methodsFor: 'private' stamp: 'jmv 6/17/2025 18:58:02'!
forgetGrabMorphDataFor: aMorph

	grabMorphData 
		removeKey: aMorph 
		ifAbsent: nil "no error"! !


!DynamicMenuBuilder methodsFor: 'building - private' stamp: 'jmv 6/17/2025 18:54:21'!
addSubMenuTo: aMenuItem

	subMenuItemsDefinition 
		at: aMenuItem contents
		ifPresent: [ :subMenuDefinition | self addSubMenuTo: aMenuItem from: subMenuDefinition ]
		ifAbsent: nil! !


!FileIOAccessor methodsFor: 'actions' stamp: 'pb 5/25/2016 00:32'!
deleteFile: fullPathName
	^self deleteFile: fullPathName ifAbsent: nil! !


!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'pb 5/25/2016 00:31'!
removeKey: localFileName 
	self removeKey: localFileName ifAbsent: nil! !

