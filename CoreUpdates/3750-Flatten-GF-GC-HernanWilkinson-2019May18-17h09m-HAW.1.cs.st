'From Cuis 5.0 of 7 November 2016 [latest update: #3748] on 18 May 2019 at 5:13:28 pm'!

!Collection methodsFor: 'copying' stamp: 'HAW 5/18/2019 17:09:55'!
flatten

	^ self species streamContents: [ :flattenedStream | self flattenTo: flattenedStream ]! !

!Collection methodsFor: 'copying' stamp: 'HAW 5/18/2019 17:10:09'!
flattenTo: flattenedStream

	self do: [ :each | 
		each isCollection 
			ifTrue: [ each flattenTo: flattenedStream ] 
			ifFalse: [ flattenedStream nextPut: each ]].
		
	^ flattenedStream 
! !


!String methodsFor: 'copying' stamp: 'HAW 5/18/2019 17:10:29'!
flattenTo: flattenedStream

	flattenedStream nextPut: self! !

