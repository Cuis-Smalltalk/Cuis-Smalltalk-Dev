'From Cuis7.7 [latest update: #7836] on 9 February 2026 at 1:49:18 pm'!

!LabelMorph methodsFor: 'accessing' stamp: 'jmv 9/Feb/2026 13:48:47'!
contents: newContents
	contents := (newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			(newContents colorAt: 1) ifNotNil: [ :cc |
				color := cc ].
			font := newContents fontAt: 1 default: font.
			newContents string ]
		ifFalse: [
			contents = newContents ifTrue: [ ^self ].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded! !

