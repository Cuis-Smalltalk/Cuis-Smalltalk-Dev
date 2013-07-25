'From Cuis 4.1 of 12 December 2012 [latest update: #1762] on 24 July 2013 at 11:46:03.949076 am'!

!Form methodsFor: 'converting' stamp: 'jmv 7/24/2013 11:45'!
asGrayScale: component
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)
	
	If component = 1, take the red component
	If component = 2, take the green component
	If component = 3, take the blue component
	"
	| f32 srcForm result map bb grays |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayScale: component].
	self unhibernate.
	srcForm _ Form extent: (width * 4)@height depth: 8.
	srcForm bits: bits.
	result _ ColorForm extent: width@height depth: 8.
	map _ Bitmap new: 256.
	2 to: 256 do: [:i | map at: i put: i - 1].
	map at: 1 put: 1.  "map zero pixel values to near-black"
	bb _ (BitBlt toForm: result)
		sourceForm: srcForm;
		combinationRule: Form over;
		colorMap: map.
	0 to: width - 1 do: [ :dstX |
		bb  sourceRect: (((dstX * 4) + component)@0 extent: 1@height);
			destOrigin: dstX@0;
			copyBits].

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map _ Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: 0@0;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	
	grays _ (0 to: 255) collect: [:brightness | Color gray: brightness asFloat / 255.0].
	grays at: 1 put: Color transparent.
	result colors: grays.
	^ result! !


!Form methodsFor: 'converting' stamp: 'jmv 7/24/2013 11:45'!
asGrayScale
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)"

	"By default, take the green component"
	^self asGrayScale: 2! !

