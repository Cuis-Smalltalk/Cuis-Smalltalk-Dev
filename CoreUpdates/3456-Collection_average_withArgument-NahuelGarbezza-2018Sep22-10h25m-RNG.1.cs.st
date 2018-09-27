'From Cuis 5.0 of 7 November 2016 [latest update: #3450] on 22 September 2018 at 10:28:10 am'!
"Change Set:		3451-CuisCore-AuthorName-2018Sep22-10h25m
Date:			22 September 2018
Author:			Nahuel Garbezza

Method #average: for Collection. Basically the composition of #collect: and #average"!


!Collection methodsFor: 'statistics' stamp: 'RNG 9/22/2018 10:26:52'!
average: aBlock

	^ (self sum: aBlock) / self size! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
!

