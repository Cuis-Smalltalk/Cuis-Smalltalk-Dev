'From Cuis7.3 [latest update: #7239] on 25 May 2025 at 11:36:23 am'!

!CodeWindow methodsFor: 'initialization' stamp: 'jmv 5/24/2025 20:16:20'!
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
		when: #methodAddedInProtocolTimeStamp send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self.! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 20:26:55'!
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
				self addSelector: sel method: method methodTimeStamp: method timeStamp ]].
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


!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 20:16:52'!
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
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self.! !

!methodRemoval: ChangeSet class #methodAdded:selector:inProtocol:class:requestor: stamp: 'jmv 5/25/2025 11:36:19'!
ChangeSet class removeSelector: #methodAdded:selector:inProtocol:class:requestor:!
!methodRemoval: ChangeSet #methodAdded:selector:inProtocol:class:requestor: stamp: 'jmv 5/25/2025 11:36:19'!
ChangeSet removeSelector: #methodAdded:selector:inProtocol:class:requestor:!
!methodRemoval: SmalltalkCompleter class #methodAdded:selector:inProtocol:class:requestor: stamp: 'jmv 5/25/2025 11:36:20'!
SmalltalkCompleter class removeSelector: #methodAdded:selector:inProtocol:class:requestor:!
!methodRemoval: SmalltalkCompleter class #addSelector:method: stamp: 'jmv 5/25/2025 11:36:20'!
SmalltalkCompleter class removeSelector: #addSelector:method:!
!methodRemoval: CompiledMethod #dateSortingValue stamp: 'jmv 5/25/2025 11:36:20'!
CompiledMethod removeSelector: #dateSortingValue!
!methodRemoval: ClassDescription #addAndClassifySelector:withMethod:inProtocol:notifying: stamp: 'jmv 5/25/2025 11:36:20'!
ClassDescription removeSelector: #addAndClassifySelector:withMethod:inProtocol:notifying:!
!methodRemoval: SystemChangeNotifier #methodAdded:selector:inProtocol:class:requestor: stamp: 'jmv 5/25/2025 11:36:20'!
SystemChangeNotifier removeSelector: #methodAdded:selector:inProtocol:class:requestor:!
SmalltalkCompleter initialize!
ChangeSet initialize!
