'From Cuis7.5 [latest update: #7720] on 27 November 2025 at 6:01:26 pm'!

!LabelMorph methodsFor: 'accessing' stamp: 'FJG 27/Nov/2025 17:55:17'!
contents: newContents
	contents := (newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			color := newContents colorAt: 1.
			font := newContents fontAt: 1 default: font.
			newContents string ]
		ifFalse: [
			contents = newContents ifTrue: [ ^self ].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded! !

