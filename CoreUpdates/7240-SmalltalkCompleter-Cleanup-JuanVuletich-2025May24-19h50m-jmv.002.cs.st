'From Cuis7.3 [latest update: #7236] on 24 May 2025 at 7:53:09 pm'!

!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 19:51:30'!
addSelector: aSymbol method: aCompiledMethod

	| thisMethodSortingValue sentMessages |
	thisMethodSortingValue := (ChangeSet notInstallOrTestRun or: [Smalltalk platformName ~= 'Win32'])
		ifTrue: [
			"Expensive and not worth doing in Windows with antivirus active, when installing large packages"
			aCompiledMethod dateSortingValue ]
		ifFalse: [ 0 ].
	sentMessages := aCompiledMethod messages.
	self protected: [
		thisMethodSortingValue > (Selectors at: aSymbol ifAbsent: [0]) ifTrue: 
			[ Selectors at: aSymbol put: thisMethodSortingValue ].
		sentMessages do: [ :sentMsg |
			thisMethodSortingValue > (Selectors at: sentMsg ifAbsent: [0]) ifTrue: 
				[ Selectors at: sentMsg put: thisMethodSortingValue ]]].! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 19:52:10'!
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

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 5/24/2025 19:51:36'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	self addSelector: aSymbol method: aMethod.! !

!methodRemoval: SmalltalkCompleter class #addSelector:method:allImplemented: stamp: 'jmv 5/24/2025 19:52:29'!
SmalltalkCompleter class removeSelector: #addSelector:method:allImplemented:!
