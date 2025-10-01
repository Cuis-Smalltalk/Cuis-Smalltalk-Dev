'From Cuis7.5 [latest update: #7634] on 1 October 2025 at 12:55:05 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 10/1/2025 12:54:28'!
addMorph: newMorph behind: aMorph
	"Add a morph to the list of submorphs behind the specified morph
		Play with this in a Workspace:
		
	r := ColoredBoxMorph new color: Color red. g := ColoredBoxMorph new color: Color green. b := ColoredBoxMorph new color: Color blue.
	y := ColoredBoxMorph new color: Color yellow. o := ColoredBoxMorph new color: Color orange. p := ColoredBoxMorph new color: Color pink.

	l := LinearLayoutMorph newColumn.
	l morphExtent: 200@200.
	l addMorph: r. l addMorph: g. l addMorph: b. l addMorph: y. l addMorph: o. l addMorph: p.
	l openInWorld.

	'Original positions'.
	l addMorph: b behind: g. l addMorph: b inFrontOf: g.

	'Try these once and repeated times, in random order. Repeated times should have no effect.'.
	l addMorph: b behind: r.
	l addMorph: b behind: g.
	l addMorph: b behind: y.
	l addMorph: b behind: o.
	l addMorph: b behind: p.
	"
	| aMorphIndex index |
	newMorph == aMorph ifTrue: [
		^ self error: 'both arguments are the same morph' ].
	aMorphIndex := submorphs indexOf: aMorph.
	index := (newMorph owner == self and: [
		(submorphs indexOf: newMorph) < aMorphIndex])
			ifTrue: [aMorphIndex]
			ifFalse: [aMorphIndex + 1].
	self privateAddMorph: newMorph atIndex: index! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 10/1/2025 12:54:34'!
addMorph: newMorph inFrontOf: aMorph
	"Add a morph to the list of submorphs in front of the specified morph
		Play with this in a Workspace:
		
	r := ColoredBoxMorph new color: Color red. g := ColoredBoxMorph new color: Color green. b := ColoredBoxMorph new color: Color blue.
	y := ColoredBoxMorph new color: Color yellow. o := ColoredBoxMorph new color: Color orange. p := ColoredBoxMorph new color: Color pink.

	l := LinearLayoutMorph newColumn.
	l morphExtent: 200@200.
	l addMorph: r. l addMorph: g. l addMorph: b. l addMorph: y. l addMorph: o. l addMorph: p.
	l openInWorld.

	'Original positions'.
	l addMorph: b behind: g. l addMorph: b inFrontOf: g.

	'Try these once and repeated times, in random order. Repeated times should have no effect.'.
	l addMorph: b inFrontOf: r.
	l addMorph: b inFrontOf: g.
	l addMorph: b inFrontOf: y.
	l addMorph: b inFrontOf: o.
	l addMorph: b inFrontOf: p.
	"
	| aMorphIndex index |
	newMorph == aMorph ifTrue: [
		^ self error: 'both arguments are the same morph' ].
	aMorphIndex := submorphs indexOf: aMorph.
	index := (newMorph owner == self and: [
		(submorphs indexOf: newMorph) > aMorphIndex])
			ifTrue: [aMorphIndex]
			ifFalse: [aMorphIndex - 1].
	self privateAddMorph: newMorph atIndex: index.! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 10/1/2025 12:32:04'!
addMorphBack: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	self privateAddMorph: aMorph
		atIndex: (aMorph owner == self
			ifTrue: [submorphs size]
			ifFalse: [submorphs size+1])! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 10/1/2025 12:32:27'!
addMorphBack: aMorph position: aPoint
	self
		privateAddMorph: aMorph
		atIndex: (aMorph owner == self
			ifTrue: [submorphs size]
			ifFalse: [submorphs size+1])
		position: aPoint! !


!LayoutMorph methodsFor: 'private' stamp: 'jmv 10/1/2025 12:32:17'!
privateAddMorph: aMorph atIndex: index position: aPoint

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	super privateAddMorph: aMorph atIndex: index position: aPoint.! !

