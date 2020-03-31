'From Cuis 5.0 [latest update: #4074] on 29 March 2020 at 5:06:59 pm'!
"Change Set:		4075-CuisCore-AuthorName-2020Mar29-16h47m
Date:			29 March 2020
Author:			Nahuel Garbezza

Fix Extract Method error occurring on some optimized selector cases"!


!MessageNode methodsFor: 'testing' stamp: 'RNG 3/29/2020 17:02:11'!
hasEquivalentArgumentsWith: aMessageNode

	self arguments with: aMessageNode arguments do:
		[ :myArgument :otherParseNodeArgument | (myArgument equivalentTo: otherParseNodeArgument) ifFalse: [ ^ false ] ].
	^ true! !


!MessageNode methodsFor: 'testing' stamp: 'RNG 3/29/2020 17:06:17'!
equivalentTo: aParseNode

	^ ((aParseNode isMessageNode
		and: [ self receiver equivalentTo: aParseNode receiver ])
		and: [ self selector = aParseNode selector ])
		and: [ self hasEquivalentArgumentsWith: aParseNode ]! !

