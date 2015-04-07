'From Cuis 4.2 of 25 July 2013 [latest update: #2218] on 5 April 2015 at 9:01:43.938257 pm'!

!Color class methodsFor: 'class initialization' stamp: 'jmv 4/5/2015 20:58'!
initializeNames
	"Name some colors."

	self setColorNamesDict: self defaultColorNamesDictionary! !

!methodRemoval: Color class #named:put:!
Color class removeSelector: #named:put:!
