'From Cuis 5.0 of 7 November 2016 [latest update: #3672] on 19 March 2019 at 2:19:53 am'!
"Change Set:		3673-CuisCore-AuthorName-2019Mar19-02h18m
Date:			19 March 2019
Author:			Nahuel Garbezza

add #isLiteralVariableNode which was needed when renaming contextually in the editor."!


!ParseNode methodsFor: 'testing' stamp: 'RNG 3/19/2019 02:18:54'!
isLiteralVariableNode

	^ false! !


!LiteralVariableNode methodsFor: 'testing' stamp: 'RNG 3/19/2019 02:18:42'!
isLiteralVariableNode

	^ true! !

