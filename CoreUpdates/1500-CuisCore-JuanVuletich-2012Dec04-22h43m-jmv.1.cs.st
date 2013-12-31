'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 10:46:12 pm'!

!SystemDictionary methodsFor: 'image format' stamp: 'jmv 12/4/2012 22:45'!
imageFormatVersion
	"Answer an integer identifying the type of image in memory. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines."

	"
	Smalltalk imageFormatVersion
	"

	<primitive: 'primitiveImageFormatVersion'>

	"Cog provides a VM parameter"
	^[Smalltalk vm vmParameterAt: 41]
		on: Error
		do: [self notify: 'This virtual machine does not support the optional ',
				'primitive #primitiveImageFormatVersion'.
			nil]
! !
