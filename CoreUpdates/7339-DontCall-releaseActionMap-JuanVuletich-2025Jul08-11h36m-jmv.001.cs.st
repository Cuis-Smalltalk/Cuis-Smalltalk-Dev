'From Cuis7.5 [latest update: #7336] on 8 July 2025 at 11:38:56 am'!

!Object methodsFor: 'events-removing' stamp: 'jmv 7/8/2025 11:09:50'!
removeActionsForEvent: anEventSelector

	| map |
	map := self actionMap.
	map ifNotNil: [
		map removeKey: anEventSelector asSymbol ifAbsent: nil.
		"No need. ActionMaps is an EphemeronIdentityDictionary. Keys are automatically removed when collectable.
		Besides, this is dangerous for instance, if colled from an iteration of ActionMaps."
		"map isEmpty
			ifTrue: [ self releaseActionMap ]"
		]! !


!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/8/2025 11:23:30'!
clearDependencyAndEvents
"
Morph allSubInstancesDo: [ :m | m clearDependencyAndEvents ].
"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	self removeAllActions.! !


!ProgressiveTestRunner methodsFor: 'evaluating - private' stamp: 'jmv 7/8/2025 11:27:48'!
unregisterTestSuiteAction
	
	testSuite removeAllActions! !

