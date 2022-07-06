'From Cuis 6.0 [latest update: #5352] on 6 July 2022 at 10:57:03 am'!

!Preference methodsFor: 'accessing' stamp: 'hlsf 7/6/2022 10:45:40'!
nameDetailed
"Provide a detailed name if possible, alternatively turn the symbolic name to a nicer aspect for the reader"
	^ description isEmptyOrNil 
		ifTrue: [name capitalized] ifFalse: [description]! !

