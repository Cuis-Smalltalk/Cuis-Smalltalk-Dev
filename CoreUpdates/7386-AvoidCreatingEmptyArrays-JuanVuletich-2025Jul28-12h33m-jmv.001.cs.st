'From Cuis7.5 [latest update: #7385] on 28 July 2025 at 12:33:49 pm'!

!MessageNode methodsFor: 'printing' stamp: 'eem 6/2/2008 11:47'!
printWithClosureAnalysisWhileOn: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	(arguments isEmpty not
	 and: [arguments first isJust: NodeNil]) ifTrue:
			[selector := SelectorNode new
							key:
									(selector key == #whileTrue:
										ifTrue: [#whileTrue]
										ifFalse: [#whileFalse])
							code: #macro.
			arguments := #()].
	self printWithClosureAnalysisKeywords: selector key arguments: arguments
		on: aStream indent: level! !


!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 1/24/2011 15:55'!
rectanglesAt: y
	(y+1) > self bottom ifTrue: [^ #()].
	^ Array with: (origin x @ y corner: corner x @ (y+1))! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'di 4/26/2000 20:20'!
systemCategorySingleton

	| cls |
	cls := self selectedClass.
	^ cls ifNil: [#()]
		ifNotNil: [Array with: cls category]! !


!WeakMessageSend methodsFor: 'accessing' stamp: 'nk 4/25/2002 09:54'!
arguments
	^arguments ifNil: [ #() ]
! !


!RunArray class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	^self runs: #() values: #()! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/5/2024 18:11:09'!
unimplemented
	"Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system."

	| all unimplemented entry |
	all := IdentitySet new.
	self allBehaviorsDo: [ :cl | cl selectorsDo: [ :aSelector | all add: aSelector]].

	unimplemented := IdentityDictionary new.
	self allBehaviorsDo: [:cl |
		 cl selectorsDo: [:sel |
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [
					entry := unimplemented at: m ifAbsent: [#()].
					entry := entry copyWith: (cl name, '>', sel).
					unimplemented at: m put: entry]]]].

	"remove some clutter from the result:"
	#(doPrimitive: primitiveFail success:) do: [ :sel |
		unimplemented removeKey: sel ifAbsent: nil].

	^ unimplemented! !


!ReferenceStream methodsFor: 'statistics' stamp: 'jmv 2/6/2024 09:32:24'!
statisticsOfRefs
	"Analyze the information in references, the objects being written out"

	| parents n kids nm ownerBags tallies owners objParent normalReferences |
	normalReferences := self references.	"Exclude unrealized weaks"
	parents := IdentityDictionary newWithRoomForMoreThan: normalReferences size * 2.
	n := 0.
	'Finding Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:
		[ :parent | barBlock value: (n := n+1).
		kids := parent class isFixed
			ifTrue: [(1 to: parent class instSize) collect: [:i | parent instVarAt: i]]
			ifFalse: [parent class isBits ifTrue: [#()]
					 ifFalse: [(1 to: parent basicSize) collect: [:i | parent basicAt: i]]].
		(kids select: [:x | normalReferences includesKey: x])
			do: [:child | parents at: child put: parent]]].
	ownerBags := Dictionary new.
	tallies := Bag new.
	n := 0.
	'Tallying Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:  "For each class of obj, tally a bag of owner classes"
		[ :obj | barBlock value: (n := n+1).
		nm := obj class name.
		tallies add: nm.
		owners := ownerBags at: nm ifAbsent: [ownerBags at: nm put: Bag new].
		(objParent := parents at: obj ifAbsent: nil) ifNotNil: [
			owners add: objParent class name]]].
	^ String streamContents: [ :strm | 
		tallies sortedCounts do: [ :assn |
			n := assn key.  nm := assn value.
			owners := ownerBags at: nm.
			strm newLine; nextPutAll: nm; space; print: n.
			owners size > 0 ifTrue: [
				strm newLine; tab; print: owners sortedCounts]]]! !


!ContextPart methodsFor: 'system simulation' stamp: 'sqr 6/26/2019 09:59:16'!
runSimulated: aBlock contextAtEachStep: block2
	"Simulate the execution of the argument, aBlock, until it ends. aBlock 
	MUST NOT contain an '^'. Evaluate block2 with the current context 
	prior to each instruction executed. Answer the simulated value of aBlock."
	| current |
	aBlock hasNonLocalReturn
		ifTrue: [self error: 'simulation of blocks with ^ can run loose'].
	current := aBlock asContext.
	current pushArgs: #() from: self.
	[current == self]
		whileFalse:
			[block2 value: current.
			current := current step].
	^self pop! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 9/25/2011 15:46'!
rectanglesAt: y
	(y+1) > extentForComposing y ifTrue: [^ #()].
	^ Array with: (0 @ y corner: extentForComposing x @ (y+1))! !


!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'jmv 12/14/2015 23:34'!
clearExternalObjects
	"Clear the array of objects that have been registered for use in non-Smalltalk code."

	self accessProtect critical: [Smalltalk specialObjectsArray at: 39 put: #()].
! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'hlsf 6/24/2022 09:42:07'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skipBack])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines := allLabels size.
	lineArray ifNil: [lineArray := #()].
	nLinesPer := segmentHeight // (Preferences at: #standardMenuFont) lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines := lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines := (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !

