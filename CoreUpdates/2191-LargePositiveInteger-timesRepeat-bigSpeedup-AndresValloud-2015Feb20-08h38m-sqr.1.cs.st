'From Cuis 4.2 of 25 July 2013 [latest update: #2190] on 20 February 2015 at 8:40:27.194236 am'!

!LargePositiveInteger methodsFor: 'enumerating' stamp: 'sqr 2/19/2015 18:53'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| toGo thisRound count |
	toGo := self.
	[toGo > 0] whileTrue:
		[
			thisRound := toGo min: SmallInteger maxVal.
			count := 1.
			[count <= thisRound] whileTrue:
				[
					aBlock value.
					count := count + 1
				].
			toGo := toGo - thisRound
		]! !

