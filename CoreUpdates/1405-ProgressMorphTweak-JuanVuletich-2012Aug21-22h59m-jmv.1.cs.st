'From Cuis 4.0 of 21 April 2012 [latest update: #1404] on 21 August 2012 at 11:01:12 pm'!

!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 23:00'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	aWorld addMorph: self.
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self morphBoundsInWorld: (Display boundingBox center - (w@h//2) extent: w@h).
	labelMorph fitContents.
	subLabelMorph fitContents.
	self layoutSubmorphs.
	aWorld startSteppingSubmorphsOf: self.! !

