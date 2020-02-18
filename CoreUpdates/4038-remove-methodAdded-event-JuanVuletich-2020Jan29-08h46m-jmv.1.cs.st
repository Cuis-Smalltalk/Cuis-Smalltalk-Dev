'From Cuis 5.0 [latest update: #4034] on 29 January 2020 at 8:52:03 am'!

!Behavior methodsFor: 'compiling' stamp: 'jmv 1/29/2020 08:47:49'!
compile: code notifying: requestor 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| methodAndNode |
	methodAndNode _ self
		basicCompile: code "a Text"
		notifying: requestor
		trailer: self defaultMethodTrailer
		ifFail: [^nil].
	methodAndNode method putSource: code fromParseNode: methodAndNode node inFile: 2
			withPreamble: [:f | f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: methodAndNode selector withMethod: methodAndNode method.
	^ methodAndNode selector! !


!Decompiler class methodsFor: 'testing' stamp: 'jmv 1/29/2020 08:46:03'!
recompileAllTest
	"[Decompiler recompileAllTest]"
	"decompile every method and compile it back; if the decompiler is correct then the system should keep running.  :)"
	
	Smalltalk allBehaviorsDo: [ :behavior |
		Utilities informUser: (behavior printString) during: [
			behavior selectors do: [ :sel |
				| decompiled ast compiled |
				decompiled := Decompiler new decompile: sel in: behavior.
				ast := Compiler new compile: decompiled in: behavior notifying: nil ifFail: [ self error: 'failed' ].
				compiled := ast generate: (behavior compiledMethodAt: sel) trailer.
				behavior addSelectorSilently: sel withMethod: compiled. ] ] ]! !


!CodeWindow methodsFor: 'notification actions' stamp: 'jmv 1/29/2020 08:49:25'!
registerNotificationActions

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #updateListsAndCode to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAddedInProtocol send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self ! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 1/29/2020 08:49:18'!
registerNotificationActions

	"Only sent when model is not nil - Hernan"
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 1/29/2020 08:49:30'!
initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue allImplemented |
	
	EntriesLimit _ 100.
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
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
		(1 to: Smalltalk specialSelectorSize) do:  [ :i | | sym |
			sym _ Smalltalk specialSelectorAt: i.
			(Selectors includesKey: sym)
				ifTrue: [ Selectors at: sym put: maxSortValue ]]]! !

!methodRemoval: SmalltalkCompleter class #methodAdded:selector:inClass:requestor: stamp: 'jmv 1/29/2020 08:50:24'!
SmalltalkCompleter class removeSelector: #methodAdded:selector:inClass:requestor:!
!methodRemoval: ChangeSet class #methodAdded:selector:inClass:requestor: stamp: 'jmv 1/29/2020 08:50:00'!
ChangeSet class removeSelector: #methodAdded:selector:inClass:requestor:!
!methodRemoval: ChangeSet #methodAdded:selector:inClass:requestor: stamp: 'jmv 1/29/2020 08:50:24'!
ChangeSet removeSelector: #methodAdded:selector:inClass:requestor:!
!methodRemoval: ClassDescription #addSelector:withMethod:notifying: stamp: 'jmv 1/29/2020 08:48:29'!
ClassDescription removeSelector: #addSelector:withMethod:notifying:!
!methodRemoval: Behavior #addSelector:withMethod: stamp: 'jmv 1/29/2020 08:46:11'!
Behavior removeSelector: #addSelector:withMethod:!
!methodRemoval: Behavior #addSelector:withMethod:notifying: stamp: 'jmv 1/29/2020 08:48:31'!
Behavior removeSelector: #addSelector:withMethod:notifying:!
!methodRemoval: SystemChangeNotifier #methodAdded:selector:inClass:requestor: stamp: 'jmv 1/29/2020 08:50:25'!
SystemChangeNotifier removeSelector: #methodAdded:selector:inClass:requestor:!
SmalltalkCompleter initialize!
ChangeSet initialize!
