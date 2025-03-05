'From Cuis7.3 [latest update: #7062] on 5 March 2025 at 11:04:00 am'!

!AbstractFont methodsFor: 'measuring' stamp: 'hlsf 3/5/2025 11:00:45'!
averageCharWidth

	^ ((self widthOf: $A) + (self widthOf: $i) / 2) asIntegerOrFloat
	! !


!LabelMorph methodsFor: 'accessing' stamp: 'hlsf 3/5/2025 11:02:45'!
contents: newContents constrainedTo: aWidth
"
	Label may need to be displayed given a constrained width
"
	| f adjustedContents |
	f := self font.
	adjustedContents := (f widthOfString: newContents ) > aWidth 
		ifTrue: [" label to be truncated, adjsust the avalaible space for label considering the additional ellipsis "
			newContents truncateWithElipsisTo: (aWidth - (f widthOfString: '...') / f averageCharWidth) rounded]
		ifFalse: [newContents].
	self contents: adjustedContents 
! !

