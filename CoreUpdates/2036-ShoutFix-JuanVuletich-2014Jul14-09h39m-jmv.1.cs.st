'From Cuis 4.2 of 25 July 2013 [latest update: #2033] on 14 July 2014 at 9:41:52.750092 am'!

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 7/10/2014 13:57'!
peekChar
	^source at: sourcePosition + 1 ifAbsent: [ $  ] "asking #value to a char gives ascii..."! !

