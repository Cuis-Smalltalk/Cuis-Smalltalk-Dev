'From Cuis7.3 [latest update: #7244] on 26 May 2025 at 11:09:18 am'!

!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/26/2025 11:07:49'!
addSelector: thisSelector method: aCompiledMethod methodTimeStamp: aTimeStampString updateSortingOfSentMessages: aBoolean

	| thisMethodSortingValue sentMessages existingSortingValue |
	thisMethodSortingValue := CompiledMethod timeStamp: aTimeStampString partsDo: [ :authorInitials :dateAndTime |
		dateAndTime
			ifNil: [ 0 ]
			ifNotNil: [ (dateAndTime - (DateAndTime fromString: '01/01/1996 00:00')) totalMinutes max: 0 ]].
	sentMessages := aCompiledMethod messages.
	self protected: [
		thisMethodSortingValue > (Selectors at: thisSelector ifAbsent: [0]) ifTrue:
			[
				Selectors at: thisSelector put: thisMethodSortingValue.
				aBoolean ifTrue:
					[
						sentMessages do: [ :sentMsg |
							existingSortingValue := Selectors at: sentMsg ifAbsent: [0].
							(existingSortingValue > 0 or:
								[ Smalltalk isThereAnImplementorOf: sentMsg ]) ifTrue:
									[ thisMethodSortingValue > (Selectors at: sentMsg ifAbsent: [0]) ifTrue:
										[ Selectors at: sentMsg put: thisMethodSortingValue ]]]]]].! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/26/2025 11:05:51'!
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
				self addSelector: sel method: method
					methodTimeStamp: method timeStamp updateSortingOfSentMessages: false ]].
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

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 5/26/2025 11:06:05'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor

	self addSelector: aSymbol method: aMethod
		methodTimeStamp: aTimeStampString updateSortingOfSentMessages: true.! !

!methodRemoval: SmalltalkCompleter class #addSelector:method:methodTimeStamp: stamp: 'jmv 5/26/2025 11:07:56'!
SmalltalkCompleter class removeSelector: #addSelector:method:methodTimeStamp:!
SmalltalkCompleter initialize!
