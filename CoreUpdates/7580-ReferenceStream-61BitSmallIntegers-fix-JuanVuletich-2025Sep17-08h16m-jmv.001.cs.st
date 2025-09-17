'From Cuis7.5 [latest update: #7579] on 17 September 2025 at 8:31:18 am'!

!DataStream methodsFor: 'write and read' stamp: 'jmv 9/17/2025 08:23:27'!
writeInteger: anInteger
	"PRIVATE -- Write the contents of a SmallInteger.
	Always store it as if we were on a 64 bit system.
	This is a waste on 32 bit systems."

	byteStream nextInt32Put: anInteger >> 32 bigEndian: true.						"signed"
	byteStream nextUint32Put: (anInteger bitAnd: 16rFFFFFFFF) bigEndian: true.		"unsigned"! !

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/17/2025 08:24:51'!
readInteger
    "PRIVATE -- Read the contents of a SmallInteger.
	See #writeInteger:"

	| highBits lowBits |
	highBits := byteStream nextInt32BigEndian: true.	"signed!!!!!!"
	lowBits := byteStream nextUint32BigEndian: true.	"unsigned!!!!!!"
	^highBits << 32 + lowBits.! !


!ReferenceStream class methodsFor: 'as yet unclassified' stamp: 'jmv 9/17/2025 08:30:24'!
versionCode
    "Answer a number representing the 'version' of the ReferenceStream facility; this is stashed at the beginning of ReferenceStreams, as a secondary versioning mechanism (the primary one is the fileTypeCode).   At present, it serves for information only, and is not checked for compatibility at reload time, but could in future be used to branch to variant code. "

	" 1 = original version 1992"
	" 2 = HyperSqueak.  PathFromHome used for Objs outside the tree.  SqueakSupport SysLibrary for shared
		globals like Display and StrikeFonts.  File has version number, class structure, then an IncomingObjects
		manager.  8/16/96 tk.  
			Extended to SmartRefStream.  class structure also keeps superclasse chain.  Does analysis on
		structure to see when translation methods are needed.  Embedable in file-ins.  (factored out
		HyperSqueak support)  Feb-May 97 tk"
	" 3 = Reference objects are byte offsets relative to the start of the object portion of the file.  Rectangles with
		values -2048 to 2047 are encoded compactly."
	" 4 = If UniClasses (subclasses of Player) have class instance variables, append their values in the form
		(#Class43 (val1 val2 vla3)).  An array of those.  Can still read version 3."
	"Cuis Smalltalk. Can not really manage UniClasses, as there are no Etoys system in place. No class Player
		either. Anyway, keep answering 4 (instead of 3) as it is quite possible that some other subtletles have
		changed since version 3."
	" 5 = Store SmallIntegers as two 32 bit words. Not doing this was a serious bug in 64 bit systems."
	^ 5! !

