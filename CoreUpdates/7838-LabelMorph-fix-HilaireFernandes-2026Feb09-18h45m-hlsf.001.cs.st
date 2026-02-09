'From Cuis7.7 [latest update: #7835] on 9 February 2026 at 6:46:15 pm'!

!LabelMorph methodsFor: 'accessing' stamp: 'hlsf 9/Feb/2026 18:45:44'!
contents: newContents
	contents := (newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			color := (newContents colorAt: 1) ifNil: [color].
			font := newContents fontAt: 1 default: font.
			newContents string ]
		ifFalse: [
			contents = newContents ifTrue: [ ^self ].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded! !

