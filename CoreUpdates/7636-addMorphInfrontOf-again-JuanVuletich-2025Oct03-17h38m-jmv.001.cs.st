'From Cuis7.5 [latest update: #7635] on 3 October 2025 at 5:48:12 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 10/3/2025 17:38:56'!
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
		(submorphs indexOf: newMorph) < aMorphIndex])
			ifTrue: [aMorphIndex - 1]
			ifFalse: [aMorphIndex].
	self privateAddMorph: newMorph atIndex: index.! !

