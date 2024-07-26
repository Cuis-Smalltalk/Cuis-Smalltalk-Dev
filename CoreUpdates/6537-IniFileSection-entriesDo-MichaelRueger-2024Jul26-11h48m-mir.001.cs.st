'From Cuis7.1 [latest update: #6536] on 26 July 2024 at 11:50:13 am'!

!IniFileSection methodsFor: 'enumerating' stamp: 'mir 7/18/2024 17:11:55'!
entriesDo: aBlock
	entries keysAndValuesDo: [:key :value | aBlock value: key value: value]! !

