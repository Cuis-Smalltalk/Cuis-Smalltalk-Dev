'From Cuis 4.2 of 25 July 2013 [latest update: #1807] on 21 August 2013 at 5:48:07.71418 pm'!
'Description Please enter a description for this package'!
!provides: 'FFITests' 1 2!
!requires: 'FFI' 1 nil nil!
!classDefinition: #FFITestLibrary category: #FFITests!
ExternalLibrary subclass: #FFITestLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFITests'!
!classDefinition: 'FFITestLibrary class' category: #FFITests!
FFITestLibrary class
	instanceVariableNames: ''!

!classDefinition: #FFITestPoint2 category: #FFITests!
ExternalStructure subclass: #FFITestPoint2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFITests'!
!classDefinition: 'FFITestPoint2 class' category: #FFITests!
FFITestPoint2 class
	instanceVariableNames: ''!

!classDefinition: #FFITestPoint4 category: #FFITests!
ExternalStructure subclass: #FFITestPoint4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFITests'!
!classDefinition: 'FFITestPoint4 class' category: #FFITests!
FFITestPoint4 class
	instanceVariableNames: ''!

!classDefinition: #FFIPluginTests category: #FFITests!
TestCase subclass: #FFIPluginTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FFITests'!
!classDefinition: 'FFIPluginTests class' category: #FFITests!
FFIPluginTests class
	instanceVariableNames: ''!


!FFITestLibrary commentStamp: 'ar 8/14/2006 23:06' prior: 0!
ExternalLibrarty used in FFI tests!

!FFITestPoint2 commentStamp: 'ar 8/14/2006 23:06' prior: 0!
A class used for testing structures as arguments for the FFI.!

!FFITestPoint4 commentStamp: 'ar 8/14/2006 23:06' prior: 0!
A class used for testing structures as arguments for the FFI.!

!FFIPluginTests commentStamp: '<historical>' prior: 0!
SUnitized tests for the FFI (mostly the plugin side)!

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 21:59'!
ffiPrintString: aString
	"FFITestLibrary new ffiPrintString: 'Hello'"
	<cdecl: char* 'ffiPrintString' (char *)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 21:59'!
ffiTestChar: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestChar: $A with: 65 with: 65.0 with: true"
	<cdecl: char 'ffiTestChars' (char char char char)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestDoubles: f1 with: f2
	"FFITestLibrary new ffiTestDoubles: $A with: 65.0"
	<cdecl: double 'ffiTestDoubles' (double double)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestFloats: f1 with: f2
	"FFITestLibrary new ffiTestFloats: $A with: 65.0"
	<cdecl: float 'ffiTestFloats' (float float)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestInt: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestInt: $A with: 65 with: 65.0 with: $A"
	<cdecl: long 'ffiTestInts' (long long long long)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestShort: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestShort: $A with: 65 with: 65.0 with: $A"
	<cdecl: short 'ffiTestShorts' (short short short short)>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiPrintString: aString
	"FFITestLibrary ffiPrintString: 'Hello'"
	<cdecl: char* 'ffiPrintString' (char *) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiTestChar: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestChar: $A with: 65 with: 65.0 with: true"
	<cdecl: char 'ffiTestChars' (char char char char) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiTestDoubles: f1 with: f2
	"FFITestLibrary ffiTestDoubles: $A with: 65.0"
	<cdecl: double 'ffiTestDoubles' (double double) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiTestFloats: f1 with: f2
	"FFITestLibrary ffiTestFloats: $A with: 65.0"
	<cdecl: float 'ffiTestFloats' (float float) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:28'!
ffiTestInt: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestInt: $A with: 65 with: 65.0 with: true"
	<cdecl: long 'ffiTestInts' (long long long long) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:01'!
ffiTestLongLong: long1 with: long2
	"FFITestLibrary ffiTestLongLong: 3 with: 4"
	<cdecl: longlong 'ffiTestLongLong' (longlong longlong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 11/28/1999 19:51'!
ffiTestPoint2: pt1 with: pt2
	<cdecl: FFITestPoint2 'ffiTestStruct64' (FFITestPoint2 FFITestPoint2) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 11/28/1999 19:51'!
ffiTestPoint4: pt1 with: pt2
	<cdecl: FFITestPoint4 'ffiTestStructBig' (FFITestPoint4 FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 12/1/1999 16:39'!
ffiTestPointers: pt1 with: pt2
	<cdecl: FFITestPoint4* 'ffiTestPointers' (FFITestPoint4* FFITestPoint4*) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:28'!
ffiTestShort: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestShort: $A with: 65 with: 65.0 with:1"
	<cdecl: short 'ffiTestShorts' (short short short short) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 7/28/2009 18:14'!
ffiTestUlong: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestUlong: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339"
	<cdecl: long 'ffiTestInts' (ulong ulong ulong ulong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'accessing' stamp: 'ar 11/28/1999 19:51'!
moduleName
	"Use the fully qualified VM name so we ensure testing loading a library"
	^'SqueakFFIPrims'! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
x
	"This method was automatically generated"
	^handle signedLongAt: 1! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
x: anObject
	"This method was automatically generated"
	handle signedLongAt: 1 put: anObject! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
y
	"This method was automatically generated"
	^handle signedLongAt: 5! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
y: anObject
	"This method was automatically generated"
	handle signedLongAt: 5 put: anObject! !

!FFITestPoint2 class methodsFor: 'field definition' stamp: 'ar 12/1/1999 16:42'!
fields
	"FFITestPoint2 defineFields"
	^#(
		(x	'long')
		(y	'long')
	)! !

!FFITestPoint2 class methodsFor: 'class initialization' stamp: 'jmv 8/21/2013 17:39'!
initialize
	self compileFields! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
w
	"This method was automatically generated"
	^handle signedLongAt: 13! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
w: anObject
	"This method was automatically generated"
	handle signedLongAt: 13 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
x
	"This method was automatically generated"
	^handle signedLongAt: 1! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
x: anObject
	"This method was automatically generated"
	handle signedLongAt: 1 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
y
	"This method was automatically generated"
	^handle signedLongAt: 5! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
y: anObject
	"This method was automatically generated"
	handle signedLongAt: 5 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
z
	"This method was automatically generated"
	^handle signedLongAt: 9! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
z: anObject
	"This method was automatically generated"
	handle signedLongAt: 9 put: anObject! !

!FFITestPoint4 class methodsFor: 'field definition' stamp: 'ar 12/1/1999 16:41'!
fields
	"FFITestPoint4 defineFields"
	^#(
		(x	'long')
		(y	'long')
		(z	'long')
		(w	'long')
	)! !

!FFITestPoint4 class methodsFor: 'class initialization' stamp: 'jmv 8/21/2013 17:39'!
initialize
	self compileFields! !

!FFIPluginTests methodsFor: 'external form tests' stamp: 'jmv 8/21/2013 17:30'!
testBlitToAndFromExternalForm
	"Ensure that we can blit to/from all 4 permutatations of Form and ExternalForm."
	| source external1 external2 destination |
	"Avoid translucency."
	source := (Cursor wait asCursorForm asFormOfDepth: 1) asFormOfDepth: 32.
	destination := Form extent: source extent depth: 32.
	external1 := ExternalForm extent: source extent depth: 32.
	external1 allocateSpace.
	external2 := ExternalForm extent: source extent depth: 32.
	external2 allocateSpace.
	self shouldnt: [source bits = destination bits].
	source displayOn: external1.
	external1 displayOn: external2.
	external2 displayOn: destination.
	self should: [source bits = destination bits].! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'hjh 1/23/2013 13:56'!
testConstructedCharCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestChars' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType char
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType char]).
	result := meth invokeWith: $A with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result asciiValue = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:22'!
testConstructedDoubleCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestDoubles' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType double
		argumentTypes: ((1 to: 2) collect:[:i| ExternalType double]).
	result := meth invokeWithArguments: (Array with: 41 with: true).
	self assert: result = 42.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:21'!
testConstructedFloatCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestFloats' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType float
		argumentTypes: ((1 to: 2) collect:[:i| ExternalType float]).
	result := meth invokeWith: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:20'!
testConstructedIntCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestInts' module: FFITestLibrary moduleName
		callType: 0 returnType: ExternalType long
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType long]).
	result := meth invokeWith: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:22'!
testConstructedPrintString
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiPrintString' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType string
		argumentTypes: ((1 to: 1) collect:[:i| ExternalType string]).
	result := meth invokeWith:'Hello World!!'.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:20'!
testConstructedShortCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestShorts' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType short
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType short]).
	result := meth invokeWithArguments: (Array with: $A with: 65 with: 65.0 with: true).
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'hjh 1/23/2013 13:55'!
testGenericCharCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestChar: $A with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result asciiValue = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericDoubleCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestDoubles: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericFloatCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestFloats: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericIntCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestInt: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericPrintString
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiPrintString:'Hello World!!'.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:18'!
testGenericShortCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestShort: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'hjh 1/23/2013 13:53'!
testLibraryCharCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestChar: $A with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result asciiValue = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryDoubleCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestDoubles: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryFloatCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestFloats: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryIntCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestInt: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryPrintString
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiPrintString:'Hello World!!'.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:23'!
testLongLongs
	"Test passing and returning longlongs"
	| long1 long2 long3 |
	long1 := 16r123456789012.
	long2 := (-1 << 31).
	long3 := FFITestLibrary ffiTestLongLong: long1 with: long2.
	self assert: long3 = (long1 + long2)! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:24'!
testPoint2
	"Test passing and returning up of structures >32bit and <= 64 bit"
	| pt1 pt2 pt3 |
	pt1 := FFITestPoint2 new.
	pt1 x: 1. pt1 y: 2.
	pt2 := FFITestPoint2 new.
	pt2 x: 3. pt2 y: 4.
	pt3 := FFITestLibrary ffiTestPoint2: pt1 with: pt2.
	self assert: pt3 x = 4.
	self assert: pt3 y = 6.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:25'!
testPoint4
	"Test passing and returning up of structures > 64 bit"
	| pt1 pt2 pt3 |
	pt1 := FFITestPoint4 new.
	pt1 x: 1. pt1 y: 2. pt1 z: 3. pt1 w: 4.
	pt2 := FFITestPoint4 new.
	pt2 x: 5. pt2 y: 6. pt2 z: 7. pt2 w: 8.
	pt3 := FFITestLibrary ffiTestPoint4: pt1 with: pt2.
	self assert: pt3 x = 6.
	self assert: pt3 y = 8.
	self assert: pt3 z = 10.
	self assert: pt3 w = 12.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:25'!
testPointers
	"Test passing and returning of pointers to structs"
	| pt1 pt2 pt3 |
	pt1 := FFITestPoint4 new.
	pt1 x: 1. pt1 y: 2. pt1 z: 3. pt1 w: 4.
	pt2 := FFITestPoint4 new.
	pt2 x: 5. pt2 y: 6. pt2 z: 7. pt2 w: 8.
	pt3 := FFITestLibrary ffiTestPointers: pt1 with: pt2.
	self assert: pt3 x = 6.
	self assert: pt3 y = 8.
	self assert: pt3 z = 10.
	self assert: pt3 w = 12.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 7/28/2009 20:53'!
testUlongRange
	"Simple test for making sure the FFI can call certain numbers in the ulong range"
	| result |
	self shouldnt:[result := FFITestLibrary ffiTestUlong: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339] raise: Error.
	self should: result =  1912741382.! !
FFITestPoint2 initialize!
FFITestPoint4 initialize!
