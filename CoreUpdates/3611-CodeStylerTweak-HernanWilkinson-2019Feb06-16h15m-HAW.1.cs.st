'From Cuis 5.0 of 7 November 2016 [latest update: #3598] on 10 February 2019 at 9:09:37 pm'!

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'HAW 2/10/2019 21:09:29'!
namesBeginningWith: aString do: aBlock
	"Evaluate aBlock for all available names that start with aString"

	self nonGlobalNamesDo: [ :name |
		(name beginsWith: aString) ifTrue: [ aBlock value: name ]].
	Smalltalk namesBeginningWith: aString do: aBlock
	
! !

