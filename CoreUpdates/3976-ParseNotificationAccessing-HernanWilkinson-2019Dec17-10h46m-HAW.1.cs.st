'From Cuis 5.0 [latest update: #3972] on 17 December 2019 at 11:16:36 am'!

!ParserNotification methodsFor: 'name' stamp: 'HAW 12/17/2019 10:47:30'!
name

	^name! !

!ParserNotification methodsFor: 'initialization' stamp: 'HAW 12/17/2019 10:46:22'!
initializeNamed: aName

	name _ aName! !


!ParserNotification class methodsFor: 'instance creation' stamp: 'HAW 12/17/2019 10:47:11'!
name: aName

	^(self new initializeNamed: aName) signal! !

!methodRemoval: ParserNotification #setName: stamp: 'HAW 12/17/2019 10:47:19'!
ParserNotification removeSelector: #setName:!

!ParserNotification reorganize!
('name' name)
('initialization' initializeNamed:)
!


!ParserNotification class reorganize!
('instance creation' name:)
!

