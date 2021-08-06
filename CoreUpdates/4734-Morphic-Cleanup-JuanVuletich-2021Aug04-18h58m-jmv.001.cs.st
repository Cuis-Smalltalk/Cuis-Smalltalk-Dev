'From Cuis 5.0 [latest update: #4733] on 4 August 2021 at 7:01:09 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:31:09'!
externalizeBoundsToWorld: r

	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: r ]
		ifNil: [ r encompassingIntegerRectangle ]! !


!MovableMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/4/2021 18:31:20'!
externalizeBoundsToWorld: r

	| inOwners |
	inOwners _ location displayBoundsOfTransformOf: r.
	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: inOwners ]
		ifNil: [ inOwners ]! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:30:34'!
externalizeBoundsToWorld: r

	^ r! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 8/4/2021 19:00:12'!
invalidateLocalRect: localRectangle

	self invalidateDisplayRect: (self externalizeBoundsToWorld: localRectangle) for: self.! !


!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/4/2021 18:28:20'!
acceptDroppingMorph: aMorph atRow: row withEvent: dropEvent

	| args dropActionSelector |

	dropActionSelector _ self valueOfProperty: #dropActionSelector.
	args _ dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: row].
	args = 2 ifTrue: [ | dropSelectorArgument |
		dropSelectorArgument _ aMorph
			valueOfProperty: #dropSelectorArgument
			ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
		^model perform: dropActionSelector with: row with: dropSelectorArgument ].
	
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'
	
	! !

!methodRemoval: MorphicCanvas #externalizeDisplayBounds:from: stamp: 'jmv 8/4/2021 18:59:08'!
MorphicCanvas removeSelector: #externalizeDisplayBounds:from:!
!methodRemoval: PluggableListMorph #flashRow: stamp: 'jmv 8/4/2021 18:58:52'!
PluggableListMorph removeSelector: #flashRow:!
!methodRemoval: InnerListMorph #flashRow: stamp: 'jmv 8/4/2021 18:58:53'!
InnerListMorph removeSelector: #flashRow:!
!methodRemoval: WorldMorph #externalizeDisplayBounds: stamp: 'jmv 8/4/2021 19:00:23'!
WorldMorph removeSelector: #externalizeDisplayBounds:!
!methodRemoval: MovableMorph #externalizeDisplayBounds: stamp: 'jmv 8/4/2021 19:00:20'!
MovableMorph removeSelector: #externalizeDisplayBounds:!
!methodRemoval: Morph #externalizeDisplayBounds: stamp: 'jmv 8/4/2021 19:00:19'!
Morph removeSelector: #externalizeDisplayBounds:!
