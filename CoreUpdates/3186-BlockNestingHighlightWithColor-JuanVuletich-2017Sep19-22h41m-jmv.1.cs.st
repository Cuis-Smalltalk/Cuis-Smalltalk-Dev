'From Cuis 5.0 of 7 November 2016 [latest update: #3185] on 19 September 2017 at 10:42:44 pm'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 9/19/2017 22:40:08'!
privateStyle

	| alpha end start count startIndexes c hue |
	self parseSetWorkspace: true.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	Preferences highlightBlockNesting ifTrue: [
		startIndexes _ parser blockDepthsStartIndexes.
		count _ startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start _ startIndexes at: idx.
			end _ idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha _ depth / 10.0 min: 1.0.
			hue _ depth * 60.
			c _ Color h: hue s: 0.2 v: 0.5 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor 
				color: c ) from: start to: end ]]! !

