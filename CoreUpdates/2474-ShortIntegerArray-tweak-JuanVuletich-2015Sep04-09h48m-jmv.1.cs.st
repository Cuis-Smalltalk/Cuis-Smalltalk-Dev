'From Cuis 4.2 of 25 July 2013 [latest update: #2463] on 4 September 2015 at 1:12:32.830201 pm'!

!ShortIntegerArray commentStamp: '<historical>' prior: 0!
ShortIntegerArray is an array for efficiently representing signed integers in the 16bit range, i.e. between -16r8000 and 16r7FFF.

Additionaly, using #at:putUnsigned: and #at:putSigned:, it supports unsigned 16 bits integers, between 0 and 16rFFFF. You can also alternatively ShortWordArray to store unsigned 16 bit integers with the usual #at: and #at:put: protocol.

Array size must be even, as two vales are stored in each word.!


!Form commentStamp: '<historical>' prior: 0!
A rectangular array of pixels, used for holding images.  All pictures, including character images are Forms.  The depth of a Form is how many bits are used to specify the color at each pixel.  The actual bits are held in a Bitmap, whose internal structure is different at each depth.  Class Color allows you to deal with colors without knowing how they are actually encoded inside a Bitmap.
	  The supported depths (in bits) are 1, 2, 4, 8, 16, and 32.  The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million.
	Forms are indexed starting at 0 instead of 1; thus, the top-left pixel of a Form has coordinates 0@0.
	Forms are combined using BitBlt.  See the comment in class BitBlt.

	colorAt: x@y						Returns the abstract Color at this location
	displayAt: x@y					shows this form on the screen
	displayOn: aMedium at: x@y	shows this form in a Window, a Form, or other DisplayMedium
	fillColor: aColor					Set all the pixels to the color.
	edit								launch an editor to change the bits of this form.
	pixelValueAt: x@y				The encoded color.  The encoding depends on the depth.
!


!ShortIntegerArray class methodsFor: 'instance creation' stamp: 'jmv 9/4/2015 13:09'!
new: n
	"Instances are always of even size. Only accept those."
	n even ifFalse: [
		self error: 'ShortIntegerArray>>#new: must be called with an even argument' ].
	^ super new: n // 2! !

