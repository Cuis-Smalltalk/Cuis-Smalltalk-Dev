'From Cuis 6.0 [latest update: #5064] on 8 February 2022 at 9:29:26 am'!

!WorldMorph methodsFor: 'world menu' stamp: 'MM 2/8/2022 09:25:39'!
findAWindowSatisfying: qualifyingBlock orMakeOneUsing: makeBlock
	"Locate a window satisfying a block, open it, and bring it to the front.  Create one if necessary, by using the makeBlock"
	| aWindow |
	submorphs do: [ :aMorph |
		(((aWindow _ aMorph) is: #SystemWindow) and: [ qualifyingBlock value: aWindow ]) ifTrue: [
			aWindow isCollapsed ifTrue: [ aWindow expand ].
			aWindow activateAndForceLabelToShow.
			^ aWindow ]].
	"None found, so create one"
	^ makeBlock value.! !

