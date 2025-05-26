'From Cuis7.3 [latest update: #7239] on 25 May 2025 at 11:36:11 am'!

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 5/24/2025 20:10:42'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor
	"A method with the given selector was added to aClass in protocol aCategoryName."

	self
		triggerEvent: #methodAddedInProtocolTimeStamp
		withArguments: { aMethod . aSymbol . aCategoryName . aClass . aTimeStampString. requestor }! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 5/24/2025 20:19:44'!
addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category methodTimeStamp: aTimeStampString notifying: requestor
	| priorMethodOrNil priorProtocolOrNil |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: nil.
	priorProtocolOrNil := self whichCategoryIncludesSelector: selector.
	self addSelectorSilently: selector withMethod: compiledMethod.
	SystemChangeNotifier uniqueInstance doSilently: [self organization classify: selector under: category].
	priorMethodOrNil
		ifNil: [
			SystemChangeNotifier uniqueInstance 
				methodAdded: compiledMethod
				selector: selector
				inProtocol: category
				class: self
				methodTimeStamp: aTimeStampString
				requestor: requestor ]
		ifNotNil: [
			SystemChangeNotifier uniqueInstance 
				methodChangedFrom: priorMethodOrNil 
				to: compiledMethod 
				selector: selector 
				inClass: self 
				requestor: requestor.
			category = priorProtocolOrNil ifFalse: [
				SystemChangeNotifier uniqueInstance
					selectorRecategorized: selector
					from: priorProtocolOrNil
					to: category
					inClass: self ]]! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 20:03:56'!
addSelector: aSymbol method: aCompiledMethod methodTimeStamp: aTimeStampString

	| thisMethodSortingValue sentMessages |
	thisMethodSortingValue := CompiledMethod timeStamp: aTimeStampString partsDo: [ :authorInitials :dateAndTime |
		dateAndTime
			ifNil: [ 0 ]
			ifNotNil: [ (dateAndTime - (DateAndTime fromString: '01/01/1996 00:00')) totalMinutes max: 0 ]].
	sentMessages := aCompiledMethod messages.
	self protected: [
		thisMethodSortingValue > (Selectors at: aSymbol ifAbsent: [0]) ifTrue: 
			[ Selectors at: aSymbol put: thisMethodSortingValue ].
		sentMessages do: [ :sentMsg |
			thisMethodSortingValue > (Selectors at: sentMsg ifAbsent: [0]) ifTrue: 
				[ Selectors at: sentMsg put: thisMethodSortingValue ]]].! !

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 5/24/2025 20:11:01'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor

	self addSelector: aSymbol method: aMethod methodTimeStamp: aTimeStampString.! !


!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 5/24/2025 20:05:18'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor

	self
		noteNewMethod: aMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: nil! !


!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 5/24/2025 20:11:37'!
methodAdded: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfMethod: aCompiledMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodAdded: aCompiledMethod
			selector: aSymbol
			inProtocol: aCategoryName
			class: aClass
			methodTimeStamp: aTimeStampString
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 5/24/2025 20:22:16'!
compile: code classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	
	| methodAndNode selector isExistingMethod updatedCode |	
	methodAndNode := self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector := methodAndNode selector.
	isExistingMethod := self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category methodTimeStamp: changeStamp notifying: requestor.
	^ methodAndNode selector! !


!CodeWindow methodsFor: 'initialization' stamp: 'jmv 5/24/2025 20:15:19'!
model: anObject
	"Set my model and make me me a dependent of the given object.
	Note: The implementors of the sent messages must check if they are still 'alive'. The usual
	way is asking `owner ifNotNil: [ do stuff ]`.
	The reason is that the receiver (i.e. the CodeWindow) may be already closed, and
	without a model, although not yet garbage collected, when the message gets dispatched"

	super model: anObject.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded: to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAddedInProtocol send: #updateListsAndCode to: self;
		when: #methodAddedInProtocolTimeStamp send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self.! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 20:15:26'!
initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue |
	
	EntriesLimit := 100.
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.
	
	self protected: [
		Selectors := Trie new.
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				Selectors at: sel ifAbsentPut: [ 0 ].
				method messages do: [ :sentMsg |
					Selectors at: sentMsg put: (Selectors at: sentMsg ifAbsent: [ 0 ]) + 1 ]]].
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				self addSelector: sel method: method ]].
		""
	
		"The following might not be found in #messages. Give them maximum priority."
		maxSortValue := SmallInteger maxVal.
		"From MessageNode>>#initialize"
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:) do: [ :sel |
				Selectors at: sel put: maxSortValue ].
	
		maxSortValue := SmallInteger maxVal-1.
		"From SystemDictionary >>#recreateSpecialObjectsArray"
		(1 to: Smalltalk specialSelectorSize) do:  [ :i | | sym |
			sym := Smalltalk specialSelectorAt: i.
			(Selectors includesKey: sym)
				ifTrue: [ Selectors at: sym put: maxSortValue ]]]! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 20:15:32'!
initialize
	"
	ChangeSet initialize
	"
	AllChangeSets := OrderedCollection new.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self.! !
