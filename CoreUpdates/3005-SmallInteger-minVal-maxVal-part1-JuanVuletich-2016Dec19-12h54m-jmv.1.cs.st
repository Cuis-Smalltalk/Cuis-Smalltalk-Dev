'From Cuis 5.0 of 7 November 2016 [latest update: #3002] on 19 December 2016 at 1:08:36 pm'!
!classDefinition: 'SmallInteger class' category: #'Kernel-Numbers'!
SmallInteger class
	instanceVariableNames: 'minVal maxVal '!

!SmallInteger class methodsFor: 'class initialization' stamp: 'jmv 12/19/2016 13:03:09'!
initMinValAndMaxVal
	| next val |
	val := -32768. "Assume at least 16 bits"
	[next := val + val.
	 next class == self] whileTrue:
		[val := next].
	minVal := val.
	maxVal := -1 - val! !


!SystemDictionary methodsFor: 'image' stamp: 'jmv 12/19/2016 13:04:12'!
wordSize
	"Answer the size in bytes of an object pointer or word in the object memory.
	The value does not change for a given image, but may be modified by a SystemTracer
	when converting the image to another format. The value is cached in WordSize to
	avoid the performance overhead of repeatedly consulting the VM."

	"Smalltalk wordSize"

	^ WordSize ifNil: [
		SmallInteger initMinValAndMaxVal.
		WordSize := [self vmParameterAt: 40] on: Error do: [4]]! !

!classDefinition: 'SmallInteger class' category: #'Kernel-Numbers'!
SmallInteger class
	instanceVariableNames: 'minVal maxVal'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SmallInteger initMinValAndMaxVal!

