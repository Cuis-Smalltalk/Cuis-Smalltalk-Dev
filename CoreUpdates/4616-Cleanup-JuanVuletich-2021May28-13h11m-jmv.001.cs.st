'From Cuis 5.0 [latest update: #4613] on 28 May 2021 at 1:11:50 pm'!

!AutoCompleterMorph methodsFor: 'testing' stamp: 'jmv 5/28/2021 11:16:16'!
isXOutOfScreen: aLocation with: anExtent
	
	^aLocation x + anExtent x > Display width! !

!AutoCompleterMorph methodsFor: 'testing' stamp: 'jmv 5/28/2021 11:16:23'!
isYOutOfScreen: aLocation with: anExtent
	
	^aLocation y + anExtent y > Display height! !

