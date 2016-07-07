'From Cuis 4.2 of 25 July 2013 [latest update: #2836] on 5 July 2016 at 9:53:49.678342 am'!

!Bag methodsFor: 'testing' stamp: 'jmv 7/4/2016 21:56'!
is: aSymbol
	^ aSymbol == #Bag or: [ super is: aSymbol ]! !


!Set methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:16'!
is: aSymbol
	^aSymbol == #Set or: [ super is: aSymbol ]! !


!Dictionary methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:13'!
is: aSymbol
	^aSymbol == #Dictionary or: [ super is: aSymbol ]! !


!Bag methodsFor: 'adding' stamp: 'jmv 7/4/2016 21:57'!
addAll: aCollection
	"Refer to the comment in Collection|addAll:."

	(aCollection is: #Bag) ifFalse: [^ super addAll: aCollection].
	aCollection contents associationsDo: [ :each |
		self add: each key withOccurrences: each value].
	^ aCollection! !

!Bag methodsFor: 'comparing' stamp: 'jmv 7/5/2016 09:21'!
= aBag
	"Two bags are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same size.
	 (c) each element occurs the same number of times in both of them".

	self == aBag ifTrue: [ ^ true ].		"Any object is equal to itself"
	(aBag is: #Bag) ifFalse: [ ^ false ].
	self size = aBag size ifFalse: [ ^ false ].
	contents associationsDo: [ :assoc |
		(aBag occurrencesOf: assoc key) = assoc value
			ifFalse: [ ^ false ]].
	^true! !


!Set methodsFor: 'testing' stamp: 'jmv 7/5/2016 09:21'!
= aSet

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	(aSet is: #Set) ifFalse: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true! !


!Dictionary methodsFor: 'adding' stamp: 'jmv 7/4/2016 22:14'!
addAll: aCollection

	"It should hold Associations, then"
	(aCollection is: #Dictionary) ifFalse: [
		^super addAll: aCollection ].
	
	aCollection == self ifFalse: [
		aCollection keysAndValuesDo: [:key :value |
			self at: key put: value]].
	
	^aCollection! !

!Dictionary methodsFor: 'comparing' stamp: 'jmv 7/5/2016 09:20'!
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value".

	self == aDictionary ifTrue: [^ true].	"Any object is equal to itself"
	(aDictionary is: #Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true

! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/4/2016 22:40'!
poolUsers
	"Answer a dictionary of pool name -> classes that refer to it. Also includes any globally know dictionaries (such as Smalltalk, Undeclared etc) which although not strictly accurate is potentially useful information "
	"Smalltalk poolUsers"
	| poolUsers pool refs |
	poolUsers _ Dictionary new.
	Smalltalk keys
		do: [ :k |
			 (((pool _ Smalltalk at: k) is: #Dictionary)
					or: [pool isKindOf: SharedPool class])
				ifTrue: [refs _ Smalltalk allClasses
								select: [:c | c sharedPools identityIncludes: pool]
								thenCollect: [:c | c name].
					refs _ refs asOrderedCollection.
					refs
						add: (Smalltalk
								allCallsOn: (Smalltalk associationAt: k)).
					poolUsers at: k put: refs]].
	^ poolUsers! !


!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 7/4/2016 22:16'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu object |
	aMenu _ MenuMorph new defaultTarget: self.

	aMenu addList: #(
		('inspect (i)'							inspectSelection)
		('explore (I)'							exploreSelection)
		('basic inspect'						inspectBasic)
		('references finder'					openReferencesFinder)
		('weight explorer'					openWeightExplorer)).

	object _ model object.
	(object is: #Dictionary) ifTrue: [ aMenu addList: #(
		-
		('senders of this key'				sendersOfSelectedKey)
		('add key'								addEntry)
		('rename key'							renameEntry)
		('remove'								removeSelection			''		model)) ]
	
	ifFalse: [ (object is: #Set) ifTrue: [ aMenu addList: #(
		-
		('remove'								removeSelection			''		model))]].

	aMenu addList: #(
		-
		('browse full (b)'						browseMethodFull)
		('browse hierarchy (h)'				browseHierarchy)
		('browse protocol (p)'				browseFullProtocol)).
	^ aMenu! !

