'From Cuis 5.0 of 7 November 2016 [latest update: #3083] on 28 May 2017 at 8:56:52 pm'!

!SetInspector methodsFor: 'accessing' stamp: 'HAW 5/28/2017 20:56:40'!
fieldList
	
	(object isNil or: [ object array isNil]) ifTrue: [^ Set new].
	
	^ self baseFieldList, (object array withIndexCollect: [:each :i | each ifNotNil: [i printString]]) select: [:each | each notNil]! !

