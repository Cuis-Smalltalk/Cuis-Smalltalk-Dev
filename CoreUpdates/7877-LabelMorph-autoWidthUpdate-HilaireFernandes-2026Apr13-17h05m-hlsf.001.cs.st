'From Cuis7.7 [latest update: #7875] on 13 April 2026 at 7:49:57 pm'!

!LabelMorph methodsFor: 'accessing' stamp: 'hlsf 13/Apr/2026 17:22:19'!
contents: newContents constrainedTo: aWidth
"
	Label may need to be displayed given a constrained width
"
	| f adjustedContents |
	f := self font.
	(f widthOfString: newContents ) > aWidth
		ifTrue: [" label to be truncated, adjsust the avalaible space for label considering the additional ellipsis "
			| guessedLength |
			guessedLength  := (aWidth - (f widthOfString: '...') / f averageCharWidth) truncated.
			[
				adjustedContents := newContents truncateWithElipsisTo: guessedLength.
				(f widthOfString: adjustedContents ) > aWidth
			] whileTrue: [guessedLength := guessedLength - 1]
		]
		ifFalse: [adjustedContents := newContents].
	self contents: adjustedContents.
! !

