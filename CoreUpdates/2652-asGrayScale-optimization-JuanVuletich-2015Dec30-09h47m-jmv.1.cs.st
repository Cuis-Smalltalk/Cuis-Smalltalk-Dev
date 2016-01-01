'From Cuis 4.2 of 25 July 2013 [latest update: #2651] on 30 December 2015 at 10:51:04.892064 am'!

!Form methodsFor: 'converting' stamp: 'jmv 12/30/2015 09:42'!
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
		^ f32 asGrayScale: component ].
	srcForm _ Form extent: (width * 4)@height depth: 8 bits: bits.
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

