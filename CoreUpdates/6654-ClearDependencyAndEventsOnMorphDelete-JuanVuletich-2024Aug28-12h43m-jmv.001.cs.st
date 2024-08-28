'From Cuis7.1 [latest update: #6653] on 28 August 2024 at 1:00:22 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/28/2024 12:54:27'!
clearDependencyAndEvents
"
Morph allSubInstancesDo: [ :m | m clearDependencyAndEvents ].
"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	self releaseActionMap.! !


!PluggableMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/28/2024 12:52:06'!
clearDependencyAndEvents

	model removeActionsWithReceiver: self.
	super clearDependencyAndEvents.! !


!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/28/2024 12:53:59'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [ self runningWorld ].
	aWorld ifNotNil: [
		aWorld activeHand ifNotNil: [ :h | h
			releaseKeyboardFocus: self;
			releaseMouseFocus: self ]].
	owner ifNotNil: [ self privateDelete].
	self clearDependencyAndEvents.
	self allSubmorphsDo: [ :each |
		each clearDependencyAndEvents ].! !

!methodRemoval: FillInTheBlankMorph #delete stamp: 'jmv 8/28/2024 12:57:43'!
FillInTheBlankMorph removeSelector: #delete!

!PluggableMorph reorganize!
('access' balloonText menuNshortcutsKey menuNshortcutsKey: model)
('caching' releaseCachedState)
('classification' hasModel)
('initialization' defaultBorderWidth defaultColor initialize model:)
('updating' update:)
('submorphs-add/remove' clearDependencyAndEvents)
!

