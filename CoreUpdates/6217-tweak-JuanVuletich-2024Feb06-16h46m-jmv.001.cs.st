'From Cuis6.3 [latest update: #6216] on 6 February 2024 at 4:47:33 pm'!

!Collection class methodsFor: 'instance creation' stamp: 'jmv 2/6/2024 16:06:44'!
new: size
	"Redefined in subclasses as appropriate."

	self error: 'Instances of ', self name, ' can not be created with the #new: message. Better call #new instead.'! !

