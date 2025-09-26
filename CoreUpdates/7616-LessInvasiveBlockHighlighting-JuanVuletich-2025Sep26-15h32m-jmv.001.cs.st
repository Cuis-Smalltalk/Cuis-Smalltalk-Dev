'From Cuis7.5 [latest update: #7615] on 26 September 2025 at 3:33:46 pm'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 9/26/2025 14:40:36'!
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
				alpha := depth sqrt * 0.1 - 0.05 min: 0.5 ].
			hue := depth * 75. 
			c := Color h: hue s: 0.05 v: 0.4 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]].! !

