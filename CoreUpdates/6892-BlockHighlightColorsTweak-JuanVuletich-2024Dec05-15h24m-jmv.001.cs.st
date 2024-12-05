'From Cuis7.1 [latest update: #6891] on 5 December 2024 at 3:49:13 pm'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 12/5/2024 15:48:40'!
privateStyle

	| alpha end start count startIndexes c hue |
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	(Preferences at: #highlightBlockNesting) ifTrue: [
		startIndexes := parser blockDepthsStartIndexes.
		count := startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start := startIndexes at: idx.
			end := idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha := 0.
			depth > 0 ifTrue: [
				"alpha := depth+0.5 / 12.0 min: 0.80."
				alpha := depth sqrt * 0.2 - 0.1 min: 0.8 ].
			hue := depth * 75.
			c := Color h: hue s: 0.2 v: 0.5 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]].
! !

