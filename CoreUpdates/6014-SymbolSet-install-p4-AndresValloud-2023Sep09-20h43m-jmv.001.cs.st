'From Cuis 6.0 [latest update: #6013] on 9 September 2023 at 8:46:08 pm'!

!Symbol class methodsFor: 'instance creation' stamp: 'sqr 12/30/2021 07:46:32'!
intern: aStringOrSymbol

	^Symbols intern: aStringOrSymbol! !

!Symbol class methodsFor: 'private' stamp: 'sqr 12/30/2021 07:46:23'!
rehash	
	"Rebuild the hash table"
	"Symbol rehash"

	Symbols rehash! !

!Symbol class methodsFor: 'private' stamp: 'jmv 9/9/2023 20:44:32'!
shutDown
	"remove this method"! !

