'From Cuis 6.0 [latest update: #5699] on 17 March 2023 at 6:20:05 pm'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/17/2023 12:22:34'!
privateStyle

	| alpha end start count startIndexes c hue |
	self parseSetWorkspace: true.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	(Preferences at: #highlightBlockNesting) ifTrue: [
		startIndexes := parser blockDepthsStartIndexes.
		count := startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start := startIndexes at: idx.
			end := idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha := depth / 15.0 min: 1.0.
			hue := depth * 60.
			c := Color h: hue s: 0.2 v: 0.5 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]]! !

